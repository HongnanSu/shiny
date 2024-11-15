library(shiny)
library(ggplot2)
library(shinythemes)

datasets <- c("economics", "faithfuld", "seals")


ui <- fluidPage(
  theme = shinythemes::shinytheme("darkly"),
  

  textInput("name", "What's your name?", placeholder = "Enter your name"),
  textOutput("greeting"),
  

  sliderInput("x", "If x is", min = 1, max = 50, value = 30),
  sliderInput("y", "and y is", min = 1, max = 50, value = 5),
  "then, (x * y) is", textOutput("product"),
  "and, (x * y) + 5 is", textOutput("product_plus5"),
  "and (x * y) + 10 is", textOutput("product_plus10"),
  
  selectInput("dataset", "Select Dataset", choices = datasets),
  verbatimTextOutput("summary"),
  plotOutput("plot"),

  sliderInput(
    "dates",
    "When should we deliver?",
    min = as.Date("2019-08-09"),
    max = as.Date("2019-08-16"),
    value = as.Date("2019-08-10")
  ),

  selectInput(
    "breed",
    "Select your favorite animal breed:",
    choices = list(
      `dogs` = list('German Shepherd', 'Bulldog', 'Labrador Retriever'),
      `cats` = list('Persian cat', 'Bengal cat', 'Siamese Cat')
    )
  ),
  

  sliderInput("number", "Select a number:", min = 0, max = 100, value = 0, step = 5, animate = TRUE),

  fluidRow(
    column(width = 6, plotOutput("plot1")),
    column(width = 6, plotOutput("plot2"))
  ),
  

  headerPanel("Central limit theorem"),
  sidebarLayout(
    position = "right",
    sidebarPanel(
      numericInput("m", "Number of samples:", 2, min = 1, max = 100)
    ),
    mainPanel(
      plotOutput("hist")
    )
  ),
  

  uiOutput("debug_ui"),
  

  dataTableOutput("table")
)

server <- function(input, output, session) {
  

  output$greeting <- renderText({
    paste0("Hello, ", input$name)
  })
  
  product <- reactive(input$x * input$y)
  output$product <- renderText(product())
  output$product_plus5 <- renderText(product() + 5)
  output$product_plus10 <- renderText(product() + 10)
  

  dataset <- reactive({
    get(input$dataset, "package:ggplot2")
  })
  output$summary <- renderPrint({
    summary(dataset())
  })
  output$plot <- renderPlot({
    plot(dataset())
  })
  

  output$dates_output <- renderText({
    paste("Delivery Date: ", input$dates)
  })
  

  output$breed_output <- renderText({
    paste("Selected Breed: ", input$breed)
  })
  

  output$number_output <- renderText({
    paste("Selected number: ", input$number)
  })
  

  output$plot1 <- renderPlot(plot(1:5))
  output$plot2 <- renderPlot(plot(1:5))
  

  output$hist <- renderPlot({
    means <- replicate(1e4, mean(runif(input$m)))
    hist(means, breaks = 20)
  })
  

  df <- mtcars
  col_var <- reactive({ df[[input$var]] })
  col_range <- reactive({ range(col_var(), na.rm = TRUE) })
  output$debug_ui <- renderUI({
    selectInput("var", "Choose a column from mtcars:", choices = colnames(df))
  })
  output$debug <- renderPrint({
    col_range()
  })
  

}

# Run the application
shinyApp(ui = ui, server = server)

