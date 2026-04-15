# All libraries
library(shiny)
library(bslib)
library(ggplot2)
library(colourpicker)

# Define UI
ui <- fluidPage(
  titlePanel("Mini Exploration of Given Gene Dataset"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Load in results",  accept = c(".csv", ".tsv")),
      p("This application accepts csv and tsv files that are organized in the following manner:
        a, b, c, d, e, f.") #Fix this, make sure that it will give a warning for crappy data input
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Samples", 
                 tabsetPanel(
                   tabPanel("Table", plotTable("samples_table")),
                   tabPanel("Plots", plotOutput("samples_plot"))
                 )),
        tabPanel("Counts", 
                 tabsetPanel(
                   tabPanel("Table", plotTable("counts_table")),
                   tabPanel("Diagnostic Scatter Plot", plotOutput("diagnostic_scatter_plot")),
                   tabPanel("Clustered Heatmap", plotOutput("heatmap_plot")),
                   tabPanel("PCA Scatter Plot", plotOutput("pca_plot"))
                 )),
        tabPanel("Differential Expression", 
                 tabsetPanel(
                   tabPanel("Table", plotTable("differential_express_table")),
                   tabPanel("Volcano Plot", plotOutput("volcano_plot"))
                 )),
      )
    )
  )
)


# This is where I should put all of my functions
server <- function(input, output, session) {
  
   #' This is to deal with loading in the data from the input box
   #' Please 
    load_data <- reactive({
      req(input$file) 
      df <- read.csv(input$file$datapath)
        return(df)
    })
    
    #' Volcano plot
    #'
    #' @param dataf The loaded data frame.
    #' @param x_name The column name to plot on the x-axis
    #' @param y_name The column name to plot on the y-axis
    #' @param slider A negative integer value representing the magnitude of
    #' p-adjusted values to color. Most of our data will be between -1 and -300.
    #' @param color1 One of the colors for the points.
    #' @param color2 The other colors for the points. Hexadecimal strings: "#CDC4B5"
    #'
    #' @return A ggplot object of a volcano plot
    #' @details I bet you're tired of these plots by now. Me too, don't worry.
    #' This is _just_ a normal function. No reactivity, no bells, no whistles. 
    #' Write a normal volcano plot using geom_point, and integrate all the above 
    #' values into it as shown in the example app. The testing script will treat 
    #' this as a normal function.
    #' 
    #' !!sym() may be required to access column names in ggplot aes().
    #'
    #' @examples volcano_plot(df, "log2fc", "padj", -100, "blue", "taupe")
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
    
    #' Draw and filter table
    #'
    #' @param dataf Data frame loaded by load_data()
    #' @param slider Negative number, typically from the slider input.
    #'
    #' @return Data frame filtered to p-adjusted values that are less than 
    #' 1 * 10^slider, columns for p-value and p-adjusted value have more digits 
    #' displayed.
    #' @details Same as above, this function is a standard R function. Tests will 
    #' evaluate it normally. Not only does this function filter the data frame to 
    #' rows that are above the slider magnitude, it should also change the format 
    #' of the p-value columns to display more digits. This is so that it looks 
    #' better when displayed on the web page. I would suggest the function 
    #' `formatC()`
    #'
    #' @examples draw_table(deseq_df, -210)
    #'    X  baseMean     log2FC     lfcSE      stat       pvalue         padj
    #'gene1 11690.780   9.852926 0.2644650  37.25607 8.45125e-304 1.54472e-299
    #'gene2  3550.435  -6.183714 0.1792708 -34.49369 9.97262e-261 9.11398e-257
    draw_table <- function(dataf, slider) {
      filtered <-  dataf[dataf$padj < 10^slider, ]
      filtered <- na.omit(filtered)
      filtered$pvalue <- formatC(filtered$pvalue, format = "e", digits = 3)
      filtered$padj   <- formatC(filtered$padj,   format = "e", digits = 3)
        return(filtered)
    }
    


    
    #' This area will be where you can connect your above functions to the interface to display them
    #' Sample table
    output$samples_table <- renderTable({
    })
    
    #' Sample plot
    output$samples_plot <- renderPlot({
    })
    
    #' Counts table
    output$counts_table <- renderTable({
    })
    
    #' Counts Diagnostic scatter plot
    output$diagnostic_scatter_plot <- renderPlot({
    })
    
    #' Counts heatmap
    output$heatmap_plot <- renderPlot({
    })
    
    #' Counts PCA scatter plot
    output$pca_plot <- renderPlot({
    })

    #' Diff. Expression table
    output$differential_express_table <- renderTable({
      draw_table(load_data(), input$slider)
    }) 

    #' Diff. Expression volcano plot
    output$volcano_plot <- renderPlot({
      volcano_plot(load_data(), input$x_axis, input$y_axis, input$slider, input$base, input$highlight)
    })
    
}

# This line is what will actually launch the app
shinyApp(ui = ui, server = server)
