#v1 of the final_project
  

# Welcome to R Shiny. All that glitters is not gold.
library(shiny)
library(bslib)
library(ggplot2)
library(colourpicker) # you might need to install this

# Define UI for application that draws a volcano plot
ui <- fluidPage(
  titlePanel("Assignment 7 - Volcano plot!"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Load in results",  accept = ".csv"),
      p("This website is for visualizing the expression results from the given dataset, deseq_res.csv"),
      radioButtons("x_axis", "Choose column for x-axis", choices = c("baseMean", "log2FoldChange",
                                                          "lfcSE", "stat", "pvalue", "padj"),
                                                         selected = "log2FoldChange"),
      radioButtons("y_axis", "Choose column for y-axis", choices = c("baseMean", "log2FoldChange",
                                                          "lfcSE", "stat", "pvalue", "padj"),
                                                         selected = "padj"),
      colourInput("base", "Base point color", value = "#FEE0E9"),
      colourInput("highlight", "Highlight point color", value = "#C7DD9D"),
      sliderInput("slider", "P-adj threshold", min = -300, max = -1, value = -100),
      submitButton("Run Analysis", icon = icon("star"))
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Plot", plotOutput("volcano")),
        tabPanel("Table", tableOutput("table"))
      )
    )
  )
)

# Define server logic required to draw a volcano plot
server <- function(input, output, session) {
    
    #' load_Data
    #'
    #' @details Okay this one is a little weird but bear with me here. This is 
    #' still a "function", but it will take no arguments. The `reactive({})` bit 
    #' says "if any of my inputs (as in, input$...) are changed, run me again". 
    #' This is useful when a user clicks a new button or loads a new file. In 
    #' our case, look for the uploaded file's datapath argument and load it with 
    #' read.csv. Return this data frame in the normal return() style.
    load_data <- reactive({
      # input$file is a data frame with columns: name, size, type, datapath
      # You need the 'datapath' column
      req(input$file) # "req" stops execution if no file uploaded yet
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
    
    #' These outputs aren't really functions, so they don't get a full skeleton, 
    #' but use the renderPlot() and renderTabel() functions to return() a plot 
    #' or table object, and those will be displayed in your application.
    output$volcano <- renderPlot({
      volcano_plot(load_data(), input$x_axis, input$y_axis, input$slider, input$base, input$highlight)
    })
    
    # Same here, just return the table as you want to see it in the web page
    output$table <- renderTable({
      draw_table(load_data(), input$slider)
    }) 
}

# Run the application
shinyApp(ui = ui, server = server)
