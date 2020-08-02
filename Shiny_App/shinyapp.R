
source("~/Documents/M_S/SA_Peter/Survey_Data_Analysis/1.Data_Handling.R")
source("~/Documents/M_S/SA_Peter/Survey_Data_Analysis/Shiny_App/functions.R")

library(shiny)

ui <- navbarPage("Survey Data Analysis",
  
  theme = shinythemes::shinytheme("spacelab"),
  
  tabPanel("By Thom Volker & Peter Lugtig",
           headerPanel("U.S. 2016 Election Surveys"),

  sidebarPanel(
    selectInput("state", "State", unique(data$state)[c(1, order(unique(data$state)[-1]) + 1)]),
    selectInput("who", "Trump and/or Clinton", 
                c("Trump" = "trump", "Clinton" = "clinton", 
                  "Difference (Trump - Clinton)" = "dif")),
    selectInput("xvar", "Independent variable",
                c("Enddate of survey" = "enddate", "Grade" = "grade",
                  "Sample size" = "samplesize", "Population" = "population")),
    selectInput("grade", "Grade of the survey",
                unique(data$grade), multiple = TRUE, selected = unique(data$grade)),
    sliderInput("period", "Poll period", 
                min = min(data$enddate), max(data$enddate), 
                value = c(min(data$enddate), max(data$enddate)))
  ),
  
  mainPanel(
    plotOutput("results", height = "600px")
    )
  )
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
                        filter(grade %in% input$grade) %>%
                        select(xvar = input$xvar, state, clinton, trump, raw_adj, months,
                               pop_clinton, pop_trump, raw_adj_dif_clinton, raw_adj_dif_trump) %>%
                        group_by(months, raw_adj) %>%
                        mutate(month_mean_clinton = mean(clinton, na.rm = T),
                               month_mean_trump = mean(trump, na.rm = T))) 
  
  output$results <- renderPlot({
    dataset() %>% plot_results(., input$who, input$xvar)
  }, res = 96)
  
}

shinyApp(ui, server)



