get_data <- function(data, state, period, xvar) {
  
  data %>%
    filter(state == state) %>%
    filter(enddate >= input$period[1] & enddate <= input$period[2]) %>%
    select(xvar = xvar,
           rawpoll_clinton = rawpoll_clinton,
           rawpoll_trump = rawpoll_trump,
           adjpoll_clinton = adjpoll_clinton,
           adjpoll_trump = adjpoll_trump,
           )
}

plot_results <- function(data, who) {
  
  us_trump <- 46.1
  us_clinton <- 48.2
  
  base_plot <- data %>% 
    ggplot(aes(x = xvar)) + 
    ylim(0,100) +
    ylab("Proportion of votes") +
    scale_color_brewer(palette = "Set1") +
    theme_classic()
    
  
  if (who == "Trump") {
    
    state_intercept <- unique(data$pop_trump)
    
    base_plot + 
      geom_point(mapping = aes(y = rawpoll_trump, color = "Raw Score Trump"), size = .5) +
      geom_point(mapping = aes(y = adjpoll_trump, color = "Adjusted Score Trump"), size = .5) +
      geom_abline(intercept = us_trump, slope = 0, show.legend = TRUE) +
      geom_abline(intercept = state_intercept, slope = 0, linetype = "dashed", show.legend = TRUE)
  }
  
  else if (who == "Clinton") {
    
    state_intercept <- unique(data$pop_clinton)
    
    base_plot + 
      geom_point(mapping = aes(y = rawpoll_clinton, color = "Raw Score Clinton"), size = .5, shape = 2) +
      geom_point(mapping = aes(y = adjpoll_clinton, color = "Adjusted Score Clinton"), size = .5, shape = 2) +
      geom_abline(intercept = us_clinton, slope = 0, show.legend = TRUE) +
      geom_abline(intercept = state_intercept, slope = 0, linetype = "dashed", show.legend = TRUE)
  }
  
  else if (who == "Trump & Clinton") {
    
    state_intercept <- unique(data$pop_clinton)
    
    base_plot +
      geom_point(mapping = aes(y = rawpoll_trump, color = "Raw Score Trump"), size = .5) +
      geom_point(mapping = aes(y = adjpoll_trump, color = "Adjusted Score Trump"), size = .5) +
      geom_point(mapping = aes(y = rawpoll_clinton, color = "Raw Score Clinton"), size = .5, shape = 2) +
      geom_point(mapping = aes(y = adjpoll_clinton, color = "Adjusted Score Clinton"), size = .5, shape = 2) +
      geom_abline(intercept = us_clinton, slope = 0, show.legend = TRUE) +
      geom_abline(intercept = state_intercept, slope = 0, linetype = "dashed", show.legend = TRUE)
    
  }
}
