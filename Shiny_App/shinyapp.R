
source("~/Documents/M_S/SA_Peter/Survey_Data_Analysis/1.Data_Handling.R")

ui <- fluidPage(
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
    plotOutput("results")
  )
)

plot_results <- function(data, who) {
  
  us_intercept <- 46.1
  
  if (who == "Trump") {
    
    state_intercept <- unique(data$pop_trump)
    
    data %>%
      ggplot() +
      geom_point(mapping = aes(x = enddate, y = rawpoll_trump, color = "Raw Score Trump"), size = .5) +
      geom_point(mapping = aes(x = enddate, y = adjpoll_trump, color = "Adjusted Score Trump"), size = .5) +
      geom_abline(intercept = us_intercept, slope = 0, show.legend = TRUE) +
      geom_abline(intercept = state_intercept, slope = 0, linetype = "dashed", show.legend = TRUE) +
      scale_color_brewer(palette = "Set1") +
      ylim(0, 100) +
      ylab("Proportion of votes") +
      ggtitle(paste0("Votes for Trump")) +
      theme_classic()
  }
}

server <- function(input, output, session) {
  
  dataset <- reactive(data %>% 
                        filter(state == input$state) %>%
                        filter(enddate >= input$period[1] & enddate <= input$period[2]))
  
  output$results <- renderPlot({
    dataset() %>% plot_results(., input$who)
  }, res = 96)
}

shinyApp(ui, server)
