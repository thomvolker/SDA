
source("data_handling.R")
source("functions.R")

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
                value = c(min(data$enddate), max(data$enddate))),
    br(),
    checkboxInput("box_labs", "Display boxplot medians"),
    br(),
    uiOutput("data_source"),
    br(),
    uiOutput("shiny_code"),
    width = 3
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
    dataset() %>% plot_results(., input$who, input$xvar, input$state, input$box_labs)
  }, res = 96)
  
  output$data_source <- renderUI({
    tagList(paste0("The poll data is retrieved from "), 
            a("https://projects.fivethirtyeight.com/2016-election-forecast/?ex_cid=rrpromo#plus",
              href = "https://projects.fivethirtyeight.com/2016-election-forecast/?ex_cid=rrpromo#plus"))
  })
  
  output$shiny_code <- renderUI({
    tagList(paste0("The R-code used to make this Shiny application can be found on "),
            a("https://github.com/thomvolker/SDA",
              href = "https://github.com/thomvolker/SDA"))
  })
}

shinyApp(ui, server)


