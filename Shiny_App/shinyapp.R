
source("~/Documents/M_S/SA_Peter/Survey_Data_Analysis/1.Data_Handling.R")
source("~/Documents/M_S/SA_Peter/Survey_Data_Analysis/Shiny_App/functions.R")

ui <- fluidPage(
  theme = shinythemes::shinytheme("paper"),
  fluidRow(
    column(3,
           selectInput("state", "State", unique(data$state))
           ),
    column(3,
           selectInput("who", "Trump and/or Clinton", c("Trump", "Clinton", "Trump & Clinton"))
           ),
    column(4, 
           sliderInput("period", "Poll period", min = min(data$enddate), max(data$enddate), 
                       value = c(min(data$enddate), max(data$enddate)))
           )
  ),
  fluidRow(
    column(3,
           selectInput("xvar", "Independent variable", 
                       c("forecastdate", "startdate", "enddate",
                         "grade", "samplesize", "population"))
           )
  ),
  fluidRow(
    plotOutput("results")
  )
)

server <- function(input, output, session) {
  
  dataset <- reactive(data %>% 
                        filter(state == input$state) %>%
                        filter(enddate >= input$period[1] & enddate <= input$period[2]) %>%
                        select(xvar = input$xvar, state, rawpoll_clinton, rawpoll_trump, 
                               adjpoll_clinton, adjpoll_trump, pop_clinton, pop_trump,
                               raw_adj_dif_clinton, raw_adj_dif_trump))
  
  output$results <- renderPlot({
    dataset() %>% plot_results(., input$who)
  }, res = 96)
}

shinyApp(ui, server)
