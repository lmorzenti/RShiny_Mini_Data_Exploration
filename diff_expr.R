library(shiny)
library(bslib)
library(ggplot2)
library(colourpicker)

ui <- fluidPage(
  titlePanel("diff expr for final proj - so far only from assignment 7"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Load in results",  accept = ".csv"),
      p("This website is for visualizing the diff expr result from given dataset"),
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

  #these are the functions to define what you are doing on the app
  load_data <- reactive({
    # input$file is a data frame with columns: name, size, type, datapath
    req(input$file)
    df <- read.csv(input$file$datapath)
    return(df)
  })
  
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
  
 
  # This will render the above functions to the app
  output$volcano <- renderPlot({
    volcano_plot(load_data(), input$x_axis, input$y_axis, input$slider, input$base, input$highlight)
  })
  
  output$table <- renderTable({
    draw_table(load_data(), input$slider)
  }) 
}

#this will run the app 
shinyApp(ui = ui, server = server)

