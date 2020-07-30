

plot_results <- function(data, who, xvar) {
  
  us_trump <- 46.1
  us_clinton <- 48.2
  
  base_plot <- data %>% 
    ggplot(aes(x = xvar)) + 
    ylim(0,100) +
    ylab("Proportion of votes") +
    scale_color_brewer(palette = "Set1") +
    theme_classic() +
    theme(legend.position = "bottom", legend.title = element_blank())
  
  if (xvar == "enddate") {
    
    if (who == "trump") {
    
    state_intercept <- unique(data$pop_trump)
    
    base_plot + 
      geom_point(mapping = aes(y = rawpoll_trump, color = "Raw Score Trump"), size = .5) +
      geom_point(mapping = aes(y = adjpoll_trump, color = "Adjusted Score Trump"), size = .5) +
      geom_abline(intercept = us_trump, slope = 0, show.legend = TRUE) +
      geom_abline(intercept = state_intercept, slope = 0, linetype = "dashed", show.legend = TRUE)
    }
  
  else if (who == "clinton") {
    
    state_intercept <- unique(data$pop_clinton)
    
    base_plot + 
      geom_point(mapping = aes(y = rawpoll_clinton, color = "Raw Score Clinton"), size = .5, shape = 2) +
      geom_point(mapping = aes(y = adjpoll_clinton, color = "Adjusted Score Clinton"), size = .5, shape = 2) +
      geom_abline(intercept = us_clinton, slope = 0, show.legend = TRUE) +
      geom_abline(intercept = state_intercept, slope = 0, linetype = "dashed", show.legend = TRUE)
    }
  
  else if (who == "dif") {
    
    state_intercept <- unique(data$pop_trump) - unique(data$pop_clinton)
    
    base_plot +
      geom_point(mapping = aes(y = rawpoll_trump - rawpoll_clinton, color = "Raw Difference (Trump - Clinton)"), size = .5) +
      geom_point(mapping = aes(y = adjpoll_trump - adjpoll_clinton, color = "Adjusted Difference (Trump - Clinton)"), size = .5) +
      geom_abline(intercept = us_trump - us_clinton, slope = 0, show.legend = TRUE) +
      geom_abline(intercept = state_intercept, slope = 0, linetype = "dashed", show.legend = TRUE) +
      ylim(-100, 100)
    }
  }
  
}
