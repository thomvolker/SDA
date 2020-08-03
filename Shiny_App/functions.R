library(tidyverse)
library(scales)

Sys.setlocale("LC_TIME", "C")

plot_results <- function(data, who, xvar, state) {
  
  if(nrow(data) == 0) {stop("There are no observations that meet the input requirements.")}
  
  us_trump <- 46.1
  us_clinton <- 48.2
  
  first_month <- min(data$months)
  
  state_trump <- unique(data$pop_trump)
  state_clinton <- unique(data$pop_clinton)
  
  base_plot <- data %>% 
    ggplot(aes(x = xvar, color = raw_adj)) + 
    ylab("Proportion of votes") +
    xlab(paste(names(xvar_choices)[xvar == xvar_choices])) +
    scale_color_brewer(palette = "Set1") +
    scale_fill_brewer(palette = "Set1") +
    theme_classic() +
    theme(legend.position = "bottom", legend.title = element_blank(),
          axis.title.x = element_text(vjust = -2))
  
  if (xvar == "enddate") {
    
    monthly_means <- data %>% # necessary because extracting text labels from the complete data
      summarise(month_mean_clinton = mean(month_mean_clinton), # is very very slow
                month_mean_trump = mean(month_mean_trump))
    
    base_plot <- base_plot +
      scale_x_date(date_breaks = "months", date_labels = "%b %y") +
      theme(axis.text.x = element_text(angle = 30, vjust = 1, hjust = 1))
    
    if (who == "trump") {
      
      base_plot +
        geom_point(aes(y = trump), size = .5) +
        geom_abline(aes(linetype = paste0("True population value in ", state), intercept = state_trump, slope = 0)) +
        scale_linetype_manual(values = "dashed") +
        annotate("label", x = first_month, y = max(data$trump, na.rm = T) + 5, color = "white",fill = "black",
                 label = paste0("Election ", state, ": ", round(state_trump, digits = 2), "%"), hjust = 0) +
        geom_label(data = monthly_means,
                   aes(x = months, y = rep(c(5,12.5), length(unique(months))),
                       label = paste0(round(month_mean_trump, digits = 1), "%")),
                   position = "identity", show.legend = FALSE, hjust = 0) +
         ylim(0, 100)
      
    }
    else if (who == "clinton") {
      
      base_plot +
        geom_point(aes(y = clinton), size = .5) +
        geom_abline(aes(linetype = paste0("True population value in ", state), intercept = state_clinton, slope = 0)) +
        scale_linetype_manual(values = "dashed") +
        annotate("label", x = first_month, y = max(data$clinton, na.rm = T) + 5, color = "white",fill = "black",
                 label = paste0("Election ", state, ": ", round(state_clinton, digits = 2), "%"), hjust = 0) +
        geom_label(data = monthly_means,
                   aes(x = months, y = rep(c(5,12.5), length(unique(months))),
                       label = paste0(round(month_mean_clinton, digits = 1), "%")),
                   position = "identity", show.legend = FALSE, hjust = 0) +
        ylim(0, 100)
      
    }
    else if (who == "dif") {
      
      base_plot +
        ylim(-100, 100) +
        geom_point(aes(y = trump - clinton), size = .5) +
        geom_abline(aes(linetype = paste0("True population value in ", state), 
                        intercept = state_trump - state_clinton, slope = 0)) +
        scale_linetype_manual(values = "dashed") +
        annotate("label", x = first_month, y = max(data$trump - data$clinton, na.rm = T) + 5, 
                 color = "white", fill = "black",
                 label = paste0("Election ", state, ": ", round(state_trump - state_clinton, digits = 2), "%"), hjust = 0) +
        geom_label(data = monthly_means,
                   aes(x = months, y = rep(c(-75, -90), length(unique(months))),
                       label = paste0(round(month_mean_trump - month_mean_clinton, digits = 1), "%")),
                   position = "identity", show.legend = FALSE, hjust = 0)
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
