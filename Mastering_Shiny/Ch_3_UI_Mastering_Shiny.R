library(shiny)

ui1 <- fluidPage(
  textInput("name", "What's your name?"),
  passwordInput("password", "What's your password?"),
  textAreaInput("story", "Tell me about yourself!", rows = 3),
  
  #verbatimTextOutput("name"),
  #verbatimTextOutput("password"),
  #verbatimTextOutput("story"),
)

server1 <- function(input, output, session) {
  
  output$name <- renderText({
    paste0("Hello ", input$name)
  })
  output$password <- renderText({
    paste0("Haha, you will not get the password.")
  })
  output$story <- renderText({
    paste0(input$story)
  })
}

new_states <- list(first = state.name[1:15], second = state.name[16:30], third = state.name[30:50])

ui2 <- fluidPage(
  textInput("name", "", placeholder = "Your name"),
  sliderInput("date", "When should we deliver", 
              min = as.Date("2020-07-03"), 
              max = as.Date("2020-07-10"), 
              value = as.Date("2020-07-03")),
  selectInput("state", "What's your favourite state?", new_states,
              multiple = TRUE),
  sliderInput("number", "Choose a number.",
              min = 0, max = 100, value = 25, step = 5,
              animate = TRUE),
  numericInput("number", "Select a value", 
               value = 150, min = 0, max = 1000, step = 50)
)

server2 <- function(input, output, session) {
  
}

ui3 <- fluidPage(
  textOutput("text"),
  verbatimTextOutput("code")
)

server3 <- function(input, output, session) {
  output$text <- renderText("Hello friend!")
  output$code <- renderPrint(summary(1:10))
}

ui4 <- fluidPage(
  tableOutput("static"),
  dataTableOutput("dynamic"),
  tableOutput("dynamic2"),
  dataTableOutput("static2")
)

server4 <- function(input, output, session) {
  output$static <- renderTable(head(mtcars))
  output$dynamic <- renderDataTable(mtcars, options = list(pageLength = 5))
}

ui5 <- fluidPage(
  plotOutput("plot", width = "400px")
)

server5 <- function(input, output, session) {
  output$plot <- renderPlot(plot(1:5), res = 96)
}

ui6 <- fluidPage(
  splitLayout(plotOutput("plot1"),plotOutput("plot2")),
  dataTableOutput("table")
)

server6 <- function(input, output, session) {
  output$plot1 <- renderPlot(plot(1:5), res = 96)
  output$plot2 <- renderPlot(plot(rnorm(10), rnorm(10), type = "l"), res = 96)
  output$table <- renderDataTable(mtcars, options = list(pageLength = 5,
                                                         searching = F,
                                                         ordering = F,
                                                         searching = F))
}



ui7 <- fluidPage(
  titlePanel("Central limit theorem"),
  sidebarLayout(
    sidebarPanel(
      numericInput("m", "Number of samples", 2, min = 1, max = 100)
    ),
    mainPanel(
      plotOutput("hist")
      )
    )
  )

server7 <- function(input, output, session) {
  output$hist <- renderPlot({
    means <- replicate(1e4, mean(runif(input$m)))
    hist(means, breaks = 20, col = "#9dd7f8")
  }, res = 96)
}

theme_demo <- function(theme) {
  fluidPage(
    theme = shinythemes::shinytheme(theme),
    titlePanel("Central limit theorem"),
    sidebarLayout(
      sidebarPanel(
        numericInput("m", "Number of samples:", 2, min = 1, max = 100),
      ), 
      position = "right",
      mainPanel(
        h1("Histogram of the sampling distribution"),
        plotOutput("hist")
      )
    )
  )
}

shinyApp(theme_demo("united"), server7)
