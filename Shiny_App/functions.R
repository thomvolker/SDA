library(tidyverse)
library(scales)

Sys.setlocale("LC_TIME", "C")

plot_results <- function(data, who, xvar) {
  
  us_trump <- 46.1
  us_clinton <- 48.2
  
  state_trump <- unique(data$pop_trump)
  state_clinton <- unique(data$pop_clinton)
  
  base_plot <- data %>% 
    ggplot(aes(x = xvar, color = raw_adj)) + 
    ylab("Proportion of votes") +
    xlab(paste(names(xvar_choices)[xvar == xvar_choices])) +
    scale_color_brewer(palette = "Set1") +
    scale_fill_brewer(palette = "Set1") +
    theme_classic() +
    theme(legend.position = "bottom", legend.title = element_blank())
  
  if (xvar == "enddate") {
    
    base_plot <- base_plot +
      scale_x_date(date_breaks = "months", date_labels = "%b %y") +
      theme(axis.text.x = element_text(angle = 30, vjust = 1, hjust = 1))
    
    if (who == "trump") {
      
      base_plot +
        geom_point(aes(y = trump), size = .5) +
        geom_abline(intercept = us_trump, slope = 0) +
        geom_abline(intercept = state_trump, slope = 0, linetype = "dashed") +
        ylim(0, 100)
      
    }
    else if (who == "clinton") {
      
      base_plot +
        geom_point(aes(y = clinton), size = .5) +
        geom_abline(intercept = us_clinton, slope = 0) +
        geom_abline(intercept = state_clinton, slope = 0, linetype = "dashed") +
        ylim(0, 100)
    }
    else if (who == "dif") {
      
      base_plot +
        geom_point(aes(y = trump - clinton), size = .5) +
        geom_abline(intercept = us_trump - us_clinton, slope = 0) +
        geom_abline(intercept = state_trump - state_clinton, slope = 0, linetype = "dashed") +
        ylim(-100, 100)
    }
  }
  
  else if (xvar == "grade") {
    
    if (who == "trump") {
      base_plot +
        geom_abline(intercept = us_trump, slope = 0) +
        geom_abline(intercept = state_trump, slope = 0, linetype = "dashed") +
        geom_boxplot(mapping = aes(y = trump, fill = raw_adj, color = NULL),
                     outlier.alpha = .25)
    }
    else if (who == "clinton") {
      base_plot +
        geom_abline(intercept = us_clinton, slope = 0) +
        geom_abline(intercept = state_clinton, slope = 0, linetype = "dashed") +
        geom_boxplot(mapping = aes(y = clinton, fill = raw_adj, color = NULL),
                     outlier.alpha = .25)
    }
    else if (who == "dif") {
      base_plot +
        geom_abline(intercept = us_trump - us_clinton, slope = 0) +
        geom_abline(intercept = state_trump - state_clinton, slope = 0, linetype = "dashed") +
        geom_boxplot(mapping = aes(y = trump - clinton, fill = raw_adj, color = NULL),
                     outlier.alpha = .25)
    }
  }
}

xvar_choices <- c("Enddate of survey" = "enddate", "Grade" = "grade",
                  "Sample size" = "samplesize", "Population" = "population")
