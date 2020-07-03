library(shiny)

#####################################################################################
## Shiny app 1                                                                     ##
#####################################################################################


ui1 <- fluidPage(
  selectInput("dataset", label = "Dataset", choices = ls("package:datasets")),
  verbatimTextOutput("summary"),
  tableOutput("table"),
)

server1 <- function(input, output, session) {
  dataset <- reactive({
    get(input$dataset, "package:datasets")
  })
  
  output$summary <- renderPrint({
    summary(dataset())
  })
  
  output$table <- renderTable({
    dataset()
  })
}

#####################################################################################
## Shiny app 2                                                                     ##
#####################################################################################

 ui2 <- fluidPage(
   textInput("name", label = "What's your name?"),
   verbatimTextOutput("name"),
 )

 server2 <- function(input, output, session) {

   output$name <- renderText({
     paste0("Hello ", input$name)
   })


 }
 
 #####################################################################################
 ## Shiny app 3                                                                     ##
 #####################################################################################
 
 ui3 <- fluidPage(
   sliderInput("x", label = "If x is", min = 1, max = 50, value = 30),
   sliderInput("y", label = "and y is", min = 1, max = 50, value = 30),
   "then x multiplied by y is",
   textOutput("product")
 )
 
 server3 <- function(input, output, session) {
   output$product <- renderText({ 
     input$x * input$y
   })
 }
 
 #####################################################################################
 ## Shiny app 4                                                                     ##
 #####################################################################################
 
 ui4 <- fluidPage(
   sliderInput("x", "If x is", min = 1, max = 50, value = 30),
   sliderInput("y", "and y is", min = 1, max = 50, value = 5),
   "then, (x * y) is", textOutput("product"),
   "and, (x * y) + 5 is", textOutput("product_plus5"),
   "and (x * y) + 10 is", textOutput("product_plus10")
 )
 
 server4 <- function(input, output, session) {
   
   product_r <- reactive({
     input$x * input$y
   })
   
   output$product <- renderText({
     product_r()
   })
   
   output$product_plus5 <- renderText({
     product_r() + 5
   })
   
   output$product_plus10 <- renderText({
     product_r() + 10
   })
   
 }
 
 #####################################################################################
 ## Shiny app 5                                                                     ##
 #####################################################################################
 
 library(ggplot2)
 datasets <- data(package = "ggplot2")$results[c(2, 4, 10), "Item"]
 
 ui5 <- fluidPage(
   selectInput("dataset", "Dataset", choices = datasets),
   verbatimTextOutput("summary"),
   plotOutput("plot")
 )
 
 server5 <- function(input, output, session) {
   dataset <- reactive({
     get(input$dataset, "package:ggplot2")
   })
   output$summary <- renderPrint({
     summary(dataset())
   })
   output$plot <- renderPlot({
     plot(dataset())
   }, res = 96)
 }

shinyApp(ui5, server5)

## fluidPage() is a layout function that sets up the basic visual structure of the page
## selectInput() is an input control that lets the user interact with the app by providing
## a value. In this case, it's a select box with the label "Dataset" and lets you choose
## one of the built-in datasets that come with R.
## verbatimTextOutput() and tableOutput() are output controls that tell Shiny where to put
## rendered output. verbatimTextOutput() displays code and tableOutput() displays tables.

