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

# add this at the very top of your app, before ui
options(shiny.maxRequestSize = 100 * 1024^2)  # 100MB limit

# Define UI
ui <- fluidPage(
  titlePanel("Data Exploration of mRNA-Seq profiling of human post-mortem BA9 brain tissue for Huntington's Disease and neurologically normal individuals"),
  sidebarLayout(
    sidebarPanel(
      fileInput("metadata_file", "Upload metadata",  accept = c(".csv", ".tsv", ".txt")),
      fileInput("counts_file", "Upload counts matrix",  accept = c(".csv", ".tsv", ".txt")),
      fileInput("DiffExp_file", "Upload DESeq2 results",  accept = c(".csv", ".tsv", ".txt")), 
      hr(),
      
      # only show on Counts tab
      conditionalPanel(
        condition = "input.tabs == 'Counts'",
        sliderInput("variance_percentile", "Variance filter percentile",
                    min = 0, max = 100, value = 50),
        numericInput("min_nonzero", "Minimum nonzero samples", value = 3)
      ),
      
      conditionalPanel(
        condition = "input.tabs == 'Differential Expression'",
        sliderInput("padj_slider", "Adjusted p-value cutoff (10^x)",
                    min = -10, max = 0, value = -2)
      ),
      
      conditionalPanel(
        condition = "input.tabs == 'Individual Gene Expression'",
        selectizeInput("gene_name", "Select gene", choices = NULL),
        selectInput("plot_type", "Gene plot type",
                    choices = c("boxplot", "violinplot", "beeswarm", "barplot")),
        actionButton("plot_button", "Plot Gene")
      ),
      
      sliderInput("variance_percentile", "Variance filter percentile",
        min = 0, max = 100, value = 50),
      
      numericInput("min_nonzero", "Minimum nonzero samples",
        value = 3),
      
      sliderInput("padj_slider", "Adjusted p-value cutoff (10^x)",
        min = -10, max = 0, value = -2),
      
      selectizeInput("gene_name", "Select gene", choices = NULL),
      
      selectInput("plot_type", "Gene plot type",
        choices = c("boxplot", "violinplot", "beeswarm", "barplot"))
      ),
    mainPanel(
      tabsetPanel(
        id = "tabs",
        tabPanel("Samples", 
                 tabsetPanel(
                   tabPanel("Summary", tableOutput("samples_table")),
                   tabPanel("Stats",  DT::dataTableOutput("stats_table")),
                   tabPanel("Plots", plotOutput("samples_plot"))
                 )),
        tabPanel("Counts", 
                 tabsetPanel(
                   tabPanel("Table", tableOutput("counts_table")),
                   tabPanel("Diagnostic Scatter Plot", plotOutput("diagnostic_scatter_plot")),
                   tabPanel("Clustered Heatmap", plotOutput("heatmap_plot")),
                   tabPanel("PCA Scatter Plot", plotOutput("pca_plot"))
                 )),
        tabPanel("Differential Expression", 
                 tabsetPanel(
                   tabPanel("Table", tableOutput("differential_express_table")),
                   tabPanel("Volcano Plot", plotOutput("volcano_plot"))
                 )),
        tabPanel("Individual Gene Expression",
                 tabsetPanel(
                   tabPanel("Plot", tableOutput("individual_gene_plot"))
                 ))
      ),verbatimTextOutput("debug"))))


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
    make_sample_plot <- function(df, col_name, group_by) {
      p <- ggplot(df, aes(x = .data[[col_name]], fill = .data[[group_by]])) +
        theme_classic() + 
        geom_density(alpha = 0.5)
      return(p)
    }
    
    # --- Tab 2: Counts --------------------------------------------
    
    counts_only <- reactive({
      req(input$counts_file)
      counts()[, -1]
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
      filter_counts(counts_only(),input$variance_percentile,input$min_nonzero)
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
      data.frame(
        median = apply(counts_only(), 1, median),
        variance = apply(counts_only(), 1, var),
        num_zeros = apply(counts_only(), 1, function(x) sum(x>0)),
        passing_filter = rownames(counts_only()) %in% rownames(filtered_counts())
      )})
    
    medcount_vs_var <- function(gene_stats) {
      p <- ggplot(gene_stats, aes(x=median+1, y=variance+1, color=passing_filter)) + 
        geom_point() +
        scale_color_manual(values = c("TRUE" = "darkblue", "FALSE" = "lightpink")) +
        scale_x_log10() +
        scale_y_log10() 
      return(p)
    }
    
    medcount_vs_numofzero <- function(gene_stats) {
      m <- ggplot(gene_stats, aes(x=median, y=num_zeros, color=passing_filter)) + 
        geom_point() +
        scale_color_manual(values = c("TRUE" = "darkblue", "FALSE" = "lightpink")) +
        scale_x_log10() +
        scale_y_log10()  
      return(m)
    }
    
    # heatmap tab
    make_heatmap <- function(filtered, log_transform) {
  
      vars <- apply(filtered, 1, var)
      top_genes <- filtered[order(vars, decreasing=TRUE)[1:500], ]
      
      mat <- if (log_transform) log10(top_genes + 1) else top_genes
      
      pheatmap(mat,
               show_rownames = FALSE,
               show_colnames = FALSE,
               cluster_rows = TRUE,
               cluster_cols = TRUE
      )
    }
    
    # PCA plot tab
    make_pca_plot <- function(filtered, clean_metadata, pc_x=1, pc_y=2) {
      
      pca_result <- prcomp(t(filtered), scale. = TRUE)
      
      # second step 
      pca_coords <- as.data.frame(pca_result$x)
      pca_coords$diagnosis <- clean_metadata$diagnosis
      
      # plot it
      variance_explained <- summary(pca_result)$importance[2,]
      x_label <- paste0("PC", pc_x, " (", round(variance_explained[pc_x] * 100, 2), "%)")
      y_label <- paste0("PC", pc_y, " (", round(variance_explained[pc_y] * 100, 2), "%)")
      
      ggplot(pca_coords, aes(x = .data[[paste0("PC", pc_x)]], y = .data[[paste0("PC", pc_y)]], color = diagnosis)) +
        geom_point() +
        theme_classic() +
        labs(x = x_label, y = y_label)
      
    }
    
    # --- Tab 3: Differential Expression --------------------------------------------
    # Volcano plot 
    volcano_plot <-
      function(dataf, x_name, y_name, slider, color1, color2) {
        
        dataf$transformed_y <- -log10(dataf[[y_name]]) 
        dataf$significance <- ifelse(dataf[[y_name]] < 10^slider, "TRUE", "FALSE")
        
        p <- ggplot(dataf, aes(x = .data[[x_name]], y = transformed_y)) +
          geom_point(size=2, aes(color = significance)) +
          scale_color_manual(values = c("TRUE" = color1, "FALSE" = color2)) +
          labs(x = x_name, y = "-log10(padj)", title = "Volcano plot") +
          theme_classic()
        
        return(p)
      }
    
    draw_table <- function(dataf, slider) {
      filtered <-  dataf[dataf$padj < 10^slider, ]
      filtered <- na.omit(filtered)
      filtered$pvalue <- formatC(filtered$pvalue, format = "e", digits = 3)
      filtered$padj   <- formatC(filtered$padj,   format = "e", digits = 3)
      return(filtered)
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
    plot_individual_geneexpression <- function(gene_data, plot_type) {
      p <- ggplot(gene_data, aes(x = diagnosis, y = expression, fill = diagnosis)) +
        theme_classic() +
        labs(
          title=paste("Expression of", gene_data$X[1]),
          x = "Diagnosis",
          y = "Normalized Expression"
        ) +
        scale_fill_manual(
          values=c("Huntington's Disease" = "darkblue",
                   "Neurologically normal" = "lightblue")
        )
      
      if (plot_type == "boxplot") p<-p + geom_boxplot()
      else if (plot_type == "violinplot") p<-p + geom_violin()
      else if (plot_type == "barplot") p<-p + geom_col()
      else if (plot_type == "beeswarm") p<-p + geom_beeswarm()
      else stop("Invalid plot type")
      
      return(p)
    }
    
    
    # --- Connect Outputs to Server ---------------------------------------
    
    #' Sample table
    output$samples_table <- renderTable({
      req(input$metadata_file)
      make_summary_table(metadata())
    })
    
    #' Stats table
    output$stats_table <- DT::renderDT({
      req(input$metadata_file)
      make_data_table(metadata())
    })
    
    #' Sample plot
    output$samples_plot <- renderPlot({
      df <- metadata()
      numeric_cols <- names(df)[sapply(df, is.numeric)]
      req(length(numeric_cols)>0)
      make_sample_plot(df, numeric_cols[1], "diagnosis")
    })
    
    #' Counts table
    output$counts_table <- renderTable({
      req(input$counts_file)
      counts_only <- counts()[, -1] 
      filtered_sum_table(counts_only,filtered_counts())
    })
    
    #' Counts Diagnostic scatter plot
    output$diagnostic_scatter_plot <- renderPlot({
      medcount_vs_var(gene_stats())
    })
    
    #' Counts heatmap
    output$heatmap_plot <- renderPlot({
      make_heatmap(
        filtered_counts(),
        TRUE)
    })
    
    #' Counts PCA scatter plot
    output$pca_plot <- renderPlot({
      req(input$counts_file, input$metadata_file)
      make_pca_plot(
        filtered_counts(),
        metadata()
      )})

    #' Diff. Expression table
    output$differential_express_table <- renderTable({
      draw_table(de_results(), input$padj_slider)
    }) 

    #' Diff. Expression volcano plot
   output$volcano_plot <- renderPlot({
     volcano_plot(de_results(), "log2FoldChange", "padj", input$padj_slider, "red", "grey")
   })
   
   #'  Individ. gene plot
   output$individual_gene_plot <- renderPlot({
     req(input$DiffExp_file, input$counts_file, input$metadata_file)
     req(input$gene_name != "")
     gene_data <- prepare_gene_data(counts(), metadata(), input$gene_name)
     plot_individual_geneexpression(gene_data, input$plot_type)
   })
   
   output$debug <- renderPrint({
     req(input$metadata_file)
     cat("diagnosis values:", unique(metadata()$diagnosis), "\n")
   })
   
   
} # This line is what will actually launch the app
shinyApp(ui = ui, server = server)
