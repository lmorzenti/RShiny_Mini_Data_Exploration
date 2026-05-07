# All libraries
library(shiny)
library(bslib)
library(ggplot2)
library(colourpicker)
library(dplyr)
library(tidyr)
library(ggbeeswarm)
library(DT)
library(GEOquery)
library(pheatmap)

# Define UI
ui <- fluidPage(
  titlePanel("Data Exploration of Given Gene Dataset"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Load in results",  accept = c(".csv", ".tsv")),
      p("This application only accepts csv and tsv files") 
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Samples", 
                 tabsetPanel(
                   tabPanel("Table", tableOutput("samples_table")),
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
                   tabPanel("Plot", plotOutput("volcano_plot"))
                 )),
      ))))


server <- function(input, output, session) {

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
          else
            paste(unique(x), collapse = ", "),
          check.names = FALSE
        )}))}
    
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
    
    filtered_sum_table <- function(counts_only, filtered) {
      table <- data.frame(
        `Number of Samples` = ncol(counts_only),
        `Total Number of Genes` = nrow(counts_only),
        `Number of genes passing filter` = nrow(filtered),
        `Percent of genes passing filter` = (nrow(filtered)/nrow(counts_only))*100,
        `Number of genes not passing the filter` = nrow(counts_only) - nrow(filtered),
        `Percent of genes not passing the filter` = ((nrow(counts_only) - nrow(filtered))/nrow(counts_only))*100
      )
      return(table)
    }
    
    # scatter plot tab
    gene_stats <- data.frame(
      median = apply(counts_only, 1, median),
      variance = apply(counts_only, 1, var),
      num_zeros = apply(counts_only, 1, function(x) sum(x>0)),
      passing_filter = rownames(counts_only) %in% rownames(filtered)
    )
    
    medcount_vs_var <- function(gene_stats) {
      p <- ggplot(gene_stats, aes(x=median, y=variance, color=passing_filter)) + 
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
      if (log_transform) {
        mat <- log10(filtered + 1)
      } else {
        mat <- filtered
      }
      p<- pheatmap(mat,
                   show_rownames = FALSE,
                   show_colnames = TRUE,
                   cluster_rows = TRUE,
                   cluster_cols = TRUE,
                   fontsize_col = 5
      )
      return(p)
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
    
    
    #' This area will be where the above functions connect to the interfac
    #' Sample table
    #output$samples_table <- renderTable({
     # samples_table(load_data())
    #})
    
    #' Sample plot
    #output$samples_plot <- renderPlot({
   # })
    
    #' Counts table
   # output$counts_table <- renderTable({
    #})
    
    #' Counts Diagnostic scatter plot
   # output$diagnostic_scatter_plot <- renderPlot({
   # })
    
    #' Counts heatmap
    #output$heatmap_plot <- renderPlot({
    #})
    
    #' Counts PCA scatter plot
    #output$pca_plot <- renderPlot({
   # })

    #' Diff. Expression table
    #output$differential_express_table <- renderTable({
     # draw_table(load_data(), input$slider)
   # }) 

    #' Diff. Expression volcano plot
   
    
} # This line is what will actually launch the app
shinyApp(ui = ui, server = server)
