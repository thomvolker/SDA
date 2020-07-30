
source("~/Documents/M_S/SA_Peter/Survey_Data_Analysis/1.Data_Handling.R")
source("~/Documents/M_S/SA_Peter/Survey_Data_Analysis/Shiny_App/functions.R")

library(shiny)

ui <- fluidPage(
  
  theme = shinythemes::shinytheme("spacelab"),
  
  headerPanel("U.S. 2016 Election Surveys"),

  sidebarPanel(
    selectInput("state", "State", unique(data$state)[c(1, order(unique(data$state)[-1]) + 1)]),
    selectInput("who", "Trump and/or Clinton", 
                c("Trump" = "trump", "Clinton" = "clinton", 
                  "Difference (Trump - Clinton)" = "dif")),
    selectInput("xvar", "Independent variable",
                c("Enddate of survey" = "enddate", "Grade" = "grade",
                  "Sample size" = "samplesize", "Population" = "population")),
    sliderInput("period", "Poll period", 
                min = min(data$enddate), max(data$enddate), 
                value = c(min(data$enddate), max(data$enddate)))
  ),
  
  mainPanel(plotOutput("results"))
  
)
 
  # 
  # fluidRow(
  #   column(3,
  #          selectInput("state", "State", unique(data$state)[c(1, order(unique(data$state)[-1]) + 1)])
  #          )),
  # fluidRow(
  #   column(3,
  #          selectInput("who", "Trump and/or Clinton", c("Trump" = "trump", "Clinton" = "clinton", "Difference (Trump - Clinton)" = "dif"))
  #          )),
  # fluidRow(
  #   column(4, 
  #          sliderInput("period", "Poll period", min = min(data$enddate), max(data$enddate), 
  #                      value = c(min(data$enddate), max(data$enddate)))
  #          )
  # ),
  # fluidRow(
  #   column(3,
  #          selectInput("xvar", "Independent variable", 
  #                      c("End of survey" = "enddate", "Grade" = "grade", 
  #                        "Sample size" = "samplesize", 
  #                        "Population" = "population"))
  #          )
  # ),
  # fluidRow(
  #   plotOutput("results")
  # )

server <- function(input, output, session) {
  
  dataset <- reactive(data %>% 
                        filter(state == input$state) %>%
                        filter(enddate >= input$period[1] & enddate <= input$period[2]) %>%
                        select(xvar = input$xvar, state, rawpoll_clinton, rawpoll_trump, 
                               adjpoll_clinton, adjpoll_trump, pop_clinton, pop_trump,
                               raw_adj_dif_clinton, raw_adj_dif_trump))
  
  output$results <- renderPlot({
    dataset() %>% plot_results(., input$who, input$xvar)
  }, res = 96)
}

shinyApp(ui, server)
