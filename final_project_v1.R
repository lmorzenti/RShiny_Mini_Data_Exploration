# load in all libraries used here
library(shiny)
library(bslib)
library(ggplot2)
library(colourpicker)
library(dplyr)
library(tidyr)
library(ggbeeswarm)
library(DT)
library(pheatmap)

# increases the size of the files that I can update
options(shiny.maxRequestSize = 100 * 1024^2)  

# Define UI
ui <- fluidPage(
  titlePanel("Data Exploration of mRNA-Seq profiling of human post-mortem BA9 brain tissue for Huntington's Disease and neurologically normal individuals"),
  sidebarLayout(
    sidebarPanel(
      fileInput("metadata_file", "Upload metadata",  accept = c(".csv", ".tsv")),
      fileInput("counts_file", "Upload counts matrix",  accept = c(".csv", ".tsv")),
      fileInput("DiffExp_file", "Upload DESeq2 results",  accept = c(".csv", ".tsv")), 
      hr(),
      
      # sidebar only to show on side bar
      conditionalPanel(
        condition = "input.tabs == 'Metadata'",
        selectInput("col_to_plot", "Column to plot", choices = NULL),
        selectInput("sample_plot_type", "Plot type",
                    choices = c("density", "histogram", "violin")),
        actionButton("plot_samples_button", "Generate Plot")
      ),
      
      conditionalPanel(
        condition = "input.tabs == 'Counts'",
        sliderInput("variance_percentile", "Variance filter percentile",
                    min = 0, max = 100, value = 50),
        numericInput("min_nonzero", "Minimum nonzero samples", value = 3),
        hr(),
        numericInput("pc_x", "PC X axis", value = 1, min = 1, max = 10),
        numericInput("pc_y", "PC Y axis", value = 2, min = 1, max = 10),
        actionButton("run_pca_button", "Generate PCA Plot")
      ),
      
      conditionalPanel(
        condition = "input.tabs == 'Differential Expression'",
        sliderInput("padj_slider", "Adjusted p-value cutoff (10^x)",
                    min = -10, max = 0, value = -2)
      ),
      
      conditionalPanel(
        condition = "input.tabs == 'Individual Gene Expression'",
        selectizeInput("gene_name", "Select gene", choices = NULL),
        selectInput("categorical_var", "Group by", choices = NULL),
        selectInput("gene_plot_type", "Gene plot type",
                    choices = c("boxplot", "violinplot", "beeswarm", "barplot")),
        actionButton("plot_gene_button", "Plot Gene")
      )
    ),
      
      
    mainPanel(
      tabsetPanel(
        id = "tabs",
        
        tabPanel("Metadata", 
                 tabsetPanel(
                   tabPanel("Sample Summary", tableOutput("samples_table"), textOutput("samples_info")),
                   tabPanel("Sample Stats",  DT::dataTableOutput("stats_table")),
                   tabPanel("Plots", plotOutput("samples_plot"))
                 )),
        tabPanel("Counts", 
                 tabsetPanel(
                   tabPanel("Summary", tableOutput("counts_table")),
                   tabPanel("Diagnostic Scatter Plots",
                            plotOutput("diagnostic_var_plot"),
                            plotOutput("diagnostic_zero_plot")),
                   tabPanel("Clustered Heatmap", plotOutput("heatmap_plot")),
                   tabPanel("PCA Scatter Plot", plotOutput("pca_plot"))
                 )),
        tabPanel("Differential Expression", 
                 tabsetPanel(
                   tabPanel("Table", DT::dataTableOutput("differential_express_table")),
                   tabPanel("Volcano Plot", plotOutput("volcano_plot"))
                 )),
        tabPanel("Individual Gene Expression",
                 tabsetPanel(
                   tabPanel("Plot", plotOutput("individual_gene_plot"))
                 ))
      ) ) ) )


server <- function(input, output, session) {
  
  # load in and save data  - 
  
  metadata <- reactive({
    req(input$metadata_file)
    df <- read.csv(input$metadata_file$datapath, check.names=FALSE)
    return(df)
  })
  
  counts <- reactive({
    req(input$counts_file)
    df <- read.csv(input$counts_file$datapath, check.names=FALSE)
    return(df)
  })
  
  de_results <- reactive({
    req(input$DiffExp_file)
    df <- read.csv(input$DiffExp_file$datapath, check.names=FALSE)
    return(df)
  })
  
  data_meta <- reactive({
    req(input$metadata_file)
    
    ext <- tools::file_ext(input$metadata_file$name)
    
    validate(
      need(ext %in% c("csv", "tsv"), "Metadata must be .csv or .tsv file")
    )
    
    if (ext == "csv") {
      read.csv(input$metadata_file$datapath)
    } else {
      read.delim(input$metadata_file$datapath)
    }
  })
  
  data_counts <- reactive({
    req(input$counts_file)
    
    ext <- tools::file_ext(input$counts_file$name)
    
    validate(
      need(ext %in% c("csv", "tsv"), "Counts file must be .csv or .tsv")
    )
    
    if (ext == "csv") {
      read.csv(input$counts_file$datapath)
    } else {
      read.delim(input$counts_file$datapath)
    }
  })
  
  data_diffexp <- reactive({
    req(input$DiffExp_file)
    
    ext <- tools::file_ext(input$DiffExp_file$name)
    
    validate(
      need(ext %in% c("csv", "tsv"), "Counts file must be .csv or .tsv")
    )
    
    if (ext == "csv") {
      read.csv(input$DiffExp_file$datapath)
    } else {
      read.delim(input$DiffExp_file$datapath)
    }
  })

  # --- observes ---------------------------------------------- 
  
  observe({
    req(input$metadata_file)
    df <- metadata()
    numeric_cols <- names(df)[sapply(df, is.numeric)]
    cat_cols <- names(df)[!sapply(df, is.numeric)]
    
    updateSelectInput(session, "col_to_plot", choices = numeric_cols)
    updateSelectInput(session, "categorical_var", choices = cat_cols)
  })
  
  # --- Tab 1: Samples --------------------------------------------
  
    # build summary table
  make_summary_table <- function(df) {
    do.call(rbind, lapply(names(df), function(col) {
      x <- df[[col]]
      data.frame(
        `Column Name` = col,
        `Type` = class(x),
        `Mean(sd) or Distinct Values` = if (is.numeric(x))
          sprintf("%.1f (+/- %.1f)", mean(x, na.rm=TRUE), sd(x, na.rm=TRUE))
        else if (length(unique(x)) > 10)
          sprintf("%d unique values", length(unique(x)))  # too many to list
        else
          paste(unique(x), collapse = ", "),  # few enough to list
        check.names = FALSE
      )
    }))
  }
    
    # build interactive summary table 
    make_data_table <- function(df) {
      display <- DT::datatable(df)
      return(display)
    }
   
    # build the sample plot
    make_sample_plot <- function(df, col_name, plot_type) {
      
      if (plot_type == "violin") {
        
        p <- ggplot(df, aes(x = diagnosis,
                            y = .data[[col_name]],
                            fill = diagnosis)) +
          geom_violin()
        
      } else if (plot_type == "density") {
        
        p <- ggplot(df, aes(x = .data[[col_name]], fill = diagnosis)) +
          geom_density(alpha = 0.5)
        
      } else if (plot_type == "histogram") {
        
        p <- ggplot(df, aes(x = .data[[col_name]], fill = diagnosis)) +
          geom_histogram(position = "identity", alpha = 0.5, bins = 30)
        
      }
      
      p +
        theme_classic() +
        labs(
          title = paste(plot_type, "of", col_name, "by diagnosis"),
          x = col_name,
          y = "value",
          fill = "Diagnosis"
        )
    }
    
    # --- Tab 2: Counts --------------------------------------------
    
    counts_only <- reactive({
      req(input$counts_file)
      df <- counts()
      
      rownames(df) <- df[[1]]
      df <- df[, -1]
      
      df <- as.matrix(df)
      storage.mode(df) <- "numeric"
      
      df
    })
    
    # filter and Summarize effect of filtering
    filter_by_variance <- function(counts_only, percentile) {
      gene_variances <- apply(counts_only, 1, var)
      threshold <- quantile(gene_variances, percentile/100)
      keep <- gene_variances >= threshold
      filtered_variance <- counts_only[keep, ]
      return (filtered_variance)
    }
    
    filter_by_zeros <- function(counts_only, min_nonzero_count) {
      sample_counts <- apply(counts_only, 1, function(x) sum(x>0))
      keep <- sample_counts >= min_nonzero_count
      filtered_zeros <- counts_only[keep, ]
      return (filtered_zeros)
    }
    
    filter_counts <- function(counts_only, percentile, min_nonzero_count) {
      after_variance <- filter_by_variance(counts_only, percentile)
      after_both <- filter_by_zeros(after_variance, min_nonzero_count)
      return (after_both)
    }
    
    filtered_counts <- reactive({
      req(counts_only())
      filter_counts(
        counts_only(),
        input$variance_percentile,
        input$min_nonzero
      )
    })
    
    filtered_sum_table <- function(counts_only, filtered) {
      table <- data.frame(
        Metric = c(
          "Number of Samples",
          "Total Number of Genes", 
          "Number of genes passing filter",
          "Percent of genes passing filter",
          "Number of genes not passing filter",
          "Percent of genes not passing filter"
        ),
        Value = c(
          ncol(counts_only),
          nrow(counts_only),
          nrow(filtered),
          round((nrow(filtered)/nrow(counts_only))*100, 2),
          nrow(counts_only) - nrow(filtered),
          round(((nrow(counts_only) - nrow(filtered))/nrow(counts_only))*100, 2)
        )
      )
      return(table)
    }
    
    # scatter plot tab
    gene_stats <- reactive({
      mat <- counts_only()
      filt <- filtered_counts()
      
      data.frame(
        median = apply(mat, 1, median, na.rm = TRUE),
        variance = apply(mat, 1, var, na.rm = TRUE),
        num_zeros = apply(mat, 1, function(x) sum(x == 0, na.rm = TRUE)),
        passing_filter = rownames(mat) %in% rownames(filt)
      )
    })
    
    medcount_vs_var <- function(df) {
      ggplot(df, aes(x = log10(median + 1), y = log10(variance + 1), color = passing_filter)) +
        geom_point(alpha = 0.7) +
        scale_color_manual(
          values = c("TRUE" = "darkblue", "FALSE" = "lightpink"),
          labels = c("TRUE" = "Passed filter", "FALSE" = "Filtered out"),
          name = "Filter status"
        ) +
        labs(
          title = "Gene filtering diagnostics: Median vs Variance",
          x = "log10(Median + 1)",
          y = "log10(Variance + 1)"
        ) +
        theme_classic()
    }
    
    medcount_vs_numofzero <- function(df) {
      ggplot(df, aes(x = log10(median + 1), y = num_zeros, color = passing_filter)) +
        geom_point(alpha = 0.7) +
        scale_color_manual(
          values = c("TRUE" = "darkblue", "FALSE" = "lightpink"),
          labels = c("TRUE" = "Passed filter", "FALSE" = "Filtered out"),
          name = "Filter status"
        ) +
        labs(
          title = "Gene filtering diagnostics: Median vs Zero counts",
          x = "log10(Median + 1)",
          y = "Number of zero-expression samples"
        ) +
        theme_classic()
    }
    # heatmap tab
    make_heatmap <- function(mat, log_transform = TRUE) {
      
      vars <- apply(mat, 1, var, na.rm = TRUE)
      top_genes <- mat[order(vars, decreasing = TRUE)[1:min(150, nrow(mat))], ]
      
      if (log_transform) {
        top_genes <- log10(top_genes + 1)
      }
      
      p <- pheatmap::pheatmap(
        top_genes,
        show_rownames = FALSE,
        show_colnames = FALSE,
        cluster_rows = TRUE,
        cluster_cols = TRUE,
        silent = TRUE
      )
      
      grid::grid.newpage()
      grid::grid.draw(p$gtable)
    }
    
    # PCA plot tab
    make_pca_plot <- function(mat, meta, pc_x = 1, pc_y = 2) {
      
      pca <- prcomp(t(mat), scale. = TRUE)
      
      var_exp <- (pca$sdev^2) / sum(pca$sdev^2)
      
      df <- data.frame(pca$x)
      
      # safer matching
      if ("sample" %in% colnames(meta)) {
        df$sample <- rownames(df)
        df <- merge(df, meta, by = "sample", all.x = TRUE)
      } else {
        df$sample <- rownames(df)
        df <- merge(df, meta, by = "sample", all.x = TRUE)
      }
      
      ggplot(df, aes_string(
        x = paste0("PC", pc_x),
        y = paste0("PC", pc_y),
        color = "diagnosis"
      )) +
        geom_point(size = 2, alpha = 0.8) +
        theme_classic() +
        labs(
          x = paste0("PC", pc_x, " (", round(var_exp[pc_x] * 100, 1), "%)"),
          y = paste0("PC", pc_y, " (", round(var_exp[pc_y] * 100, 1), "%)"),
          color = "Diagnosis"
        )
    }
    
    # --- Tab 3: Differential Expression --------------------------------------------
    # Volcano plot 
    volcano_plot <- function(dataf, x_name, y_name, slider,
                             color1 = "darkblue", color2 = "lightpink") {
      
      padj_cutoff <- 10^slider
      
      dataf$neg_log10_p <- -log10(dataf[[y_name]])
      
      dataf$significance <- ifelse(
        dataf[[y_name]] < padj_cutoff,
        "Significant",
        "Not significant"
      )
      
      p <- ggplot(dataf, aes(x = .data[[x_name]], y = neg_log10_p)) +
        
        geom_point(aes(color = significance), alpha = 0.7, size = 2) +
        
        scale_color_manual(values = c(
          "Significant" = color1,
          "Not significant" = color2
        )) +
        
        geom_hline(yintercept = -log10(padj_cutoff), linetype = "dashed") +
        geom_vline(xintercept = c(-1, 1), linetype = "dashed") +
        
        # upreg label
        annotate(
          "text",
          x = 2,   # adjust if needed
          y = max(dataf$neg_log10_p, na.rm = TRUE),
          label = "Upregulated",
          color = "black",
          fontface = "bold"
        ) +
        
        # downreg label
        annotate(
          "text",
          x = -2,  # adjust if needed
          y = max(dataf$neg_log10_p, na.rm = TRUE),
          label = "Downregulated",
          color = "black",
          fontface = "bold"
        ) +
        
        labs(
          x = "log2 Fold Change",
          y = "-log10 adjusted p-value",
          title = paste(
            "Visulization of DESeq2 Differential Expression Analysis (padj < 10^",
            slider, ")", sep = ""
          ),
          color = "Significance"
        ) +
        
        theme_classic(base_size = 13) +
        theme(
          legend.position = "right"
        )
      
      return(p)
    }
    
    # sortable table
    draw_table <- function(dataf, slider) {
      filtered <-  dataf[dataf$padj < 10^slider, ]
      filtered <- na.omit(filtered)
      filtered$pvalue <- formatC(filtered$pvalue, format = "e", digits = 3)
      filtered$padj   <- formatC(filtered$padj,   format = "e", digits = 3)
      
       DT::datatable(
        filtered,
        options = list(
          pageLength = 10,
          autoWidth = TRUE,
          orderClasses = TRUE
        ),
        rownames = FALSE
      )
    }
    
    # --- Tab 4: Individual Gene Expression --------------------------------------------
    # prepare the genes to be plotted
    prepare_gene_data <- function(counts, clean_metadata, gene_name) {
      counts_long <- pivot_longer(
        counts,
        cols = -X,
        names_to="sample",
        values_to="expression"
      )
      
      counts_long <- left_join(
        counts_long,
        clean_metadata,
        by="sample"
      )
      
      gene_data <- counts_long[counts_long$X == gene_name, ]
      return(gene_data)
    }
    
    observe({
      req(input$DiffExp_file)
      de <- de_results()
      de <- de[!is.na(de$symbol), ]
      gene_choices <- setNames(
        de$X,
        de$symbol
      )
      updateSelectizeInput(session, "gene_name",
                           choices = gene_choices,
                           server = TRUE)
    })
    
    # plot the gene expression
    plot_individual_geneexpression <- function(gene_data, plot_type, categorical_var) {
      p <- ggplot(gene_data, aes(x = .data[[categorical_var]], 
                                 y = expression, 
                                 fill = .data[[categorical_var]],
                                 color = .data[[categorical_var]])) +
        theme_classic() +
        theme(legend.position = "none") +
        labs(
          title = paste("Expression of", gene_data$X[1] ),
          x = categorical_var,
          y = "Normalized Expression", 
          fill = categorical_var,
          color = categorical_var
        )
      
      if (plot_type == "boxplot") {
        p <- p +
          geom_boxplot(
            color = "black",
            outlier.shape = NA
          ) 
      }
      else if (plot_type == "violinplot") {
        p <- p +
          geom_violin(
            color = "black",
            alpha = 0.7
          ) +
          geom_beeswarm(
            shape = 21,
            color = "black",
            size = 2
          )
      }
    
      else if (plot_type == "barplot") {
          p <- p +
            stat_summary(fun = mean, geom = "col", color="black") +
            stat_summary(fun.data = mean_se,
                        geom = "errorbar",
                        color = "black",
                        width = 0.2)
        }
      else if (plot_type == "beeswarm") p <- p + geom_beeswarm()
      else stop("Invalid plot type")
      
      if (categorical_var == "diagnosis") {
        
        p <- p +
          theme(
            legend.position = "right"
          )
        
      } else {
        
        p <- p +
          theme(
            legend.position = "none",
            axis.text.x = element_text(
              angle = 60,
              hjust = 1
            )
          )
      }
      
      return(p)
    }
    
    
    # --- Connect Outputs to Server ---------------------------------------
    
    #' Sample table
    output$samples_table <- renderTable({
      req(input$metadata_file)
      make_summary_table(metadata())
    })
    
    #' A little more information about the data for the user
    output$samples_info <- renderText({
      req(input$metadata_file)
      paste("Rows:", nrow(metadata()), "| Columns:", ncol(metadata()))
    })
    
    #' Stats table
    output$stats_table <- DT::renderDT({
      req(input$metadata_file)
      make_data_table(metadata())
    })
    
    sample_plot_inputs <- eventReactive(input$plot_samples_button, {
      list(
        col = input$col_to_plot,
        type = input$sample_plot_type
      )
    })
    
    #' Sample plot
    output$samples_plot <- renderPlot({
      req(sample_plot_inputs())
      
      make_sample_plot(
        metadata(),
        sample_plot_inputs()$col,
        sample_plot_inputs()$type
      )
    })
    
    #' Counts table
    output$counts_table <- renderTable({
      req(counts_only(), filtered_counts())
      
      filtered_sum_table(
        counts_only(),
        filtered_counts()
      )
    })
    
    #' Counts Diagnostic scatter plots
    output$diagnostic_var_plot <- renderPlot({
      medcount_vs_var(gene_stats())
    })
    
    output$diagnostic_zero_plot <- renderPlot({
      medcount_vs_numofzero(gene_stats())
    })
    
    #' Counts heatmap
    output$heatmap_plot <- renderPlot({
      req(filtered_counts())
      make_heatmap(filtered_counts(), TRUE)
    })
    
    #' Counts PCA scatter plot
    
    pca_inputs <- eventReactive(input$run_pca_button, {
      
      mat <- filtered_counts()
      meta <- metadata()
      
      # hard safety checks (no silent failure)
      if (is.null(mat) || nrow(mat) < 2 || ncol(mat) < 2) {
        return(NULL)
      }
      
      mat <- as.matrix(mat)
      storage.mode(mat) <- "numeric"
      
      list(
        mat = mat,
        meta = meta,
        pc_x = input$pc_x,
        pc_y = input$pc_y
      )
    })
    
    output$pca_plot <- renderPlot({
      
      pca_obj <- pca_inputs()
      
      if (is.null(pca_obj)) {
        return(NULL)
      }
      
      make_pca_plot(
        pca_obj$mat,
        pca_obj$meta,
        pca_obj$pc_x,
        pca_obj$pc_y
      )
    })

    #' Diff. Expression table
    output$differential_express_table <- DT::renderDT({
      draw_table(de_results(), input$padj_slider)
    }) 

    #' Diff. Expression volcano plot
   output$volcano_plot <- renderPlot({
     volcano_plot(de_results(), "log2FoldChange", "padj", input$padj_slider, "darkblue", "lightpink")
   })
   
   #'  Individ. gene plot
   gene_plot_inputs <- eventReactive(input$plot_gene_button, {
     
     req(input$DiffExp_file,
         input$counts_file,
         input$metadata_file)
     
     req(input$gene_name != "")
     req(input$categorical_var != "")
     req(input$gene_plot_type != "")
     
     gene_df <- prepare_gene_data(
       counts(),
       metadata(),
       input$gene_name
     )
     list(
       gene_data = gene_df,
       plot_type = input$gene_plot_type,
       categorical_var = input$categorical_var
     )
   })
   
   output$individual_gene_plot <- renderPlot({
     req(gene_plot_inputs())
     plot_individual_geneexpression(
       gene_plot_inputs()$gene_data,
       gene_plot_inputs()$plot_type,
       gene_plot_inputs()$categorical_var
     )
   })
   
   outputOptions(output, "heatmap_plot", suspendWhenHidden = FALSE)
   outputOptions(output, "pca_plot", suspendWhenHidden = FALSE)
   outputOptions(output, "diagnostic_var_plot", suspendWhenHidden = FALSE)
   outputOptions(output, "diagnostic_zero_plot", suspendWhenHidden = FALSE)
   
} # This line is what will actually launch the app
shinyApp(ui = ui, server = server)
