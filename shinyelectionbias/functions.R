library(tidyverse)
library(scales)
library(plotly)

Sys.setlocale("LC_TIME", "C")

plot_results <- function(data, who, xvar, state, box_labs) {
  
  if(nrow(data) == 0) {stop("There are no observations that meet the input requirements.")}
  
  us_trump <- 46.1
  us_clinton <- 48.2
  
  
  state_trump <- unique(data$pop_trump)
  state_clinton <- unique(data$pop_clinton)
  
  base_plot <- data %>% 
    ggplot(aes(x = xvar, color = raw_adj)) + 
    ylab("Proportion of votes") +
    xlab(paste(names(xvar_choices)[xvar == xvar_choices])) +
    scale_color_brewer(palette = "Dark2") +
    scale_fill_brewer(palette = "Dark2") +
    theme_classic() +
    theme(legend.position = "bottom", 
          legend.title = element_blank(),
          axis.title.x = element_text(vjust = -2))
  
  mean_est <- data %>%
    group_by(raw_adj) %>%
    summarise(mean_trump = mean(trump, na.rm = TRUE),
              mean_clinton = mean(clinton, na.rm = TRUE),
              median_trump = median(trump, na.rm = TRUE),
              median_clinton = median(clinton, na.rm = TRUE),
              median_dif = median(trump - clinton, na.rm = TRUE))
    
  
  if (xvar == "enddate") {
    
    monthly_means <- data %>%
      group_by(months, raw_adj) %>%# necessary because extracting text labels from the complete data
      summarise(month_mean_clinton = mean(month_mean_clinton), # is very very slow
                month_mean_trump = mean(month_mean_trump))
    
    base_plot <- base_plot +
      scale_x_date(date_breaks = "months", 
                   date_labels = "%b %y") +
      theme(axis.text.x = element_text(angle = 30, 
                                       vjust = 1, 
                                       hjust = 1))
    
    if (who == "trump") {
      
      base_plot +
        geom_point(aes(y = trump), 
                   size = .5) +
        geom_abline(aes(linetype = paste0("True population value in ", state), 
                        intercept = state_trump, 
                        slope = 0)) +
        scale_linetype_manual(values = "dashed") +
        geom_abline(data = mean_est,
                    mapping = aes(color = raw_adj,
                                  intercept = mean_trump,
                                  slope = c(0,0)),
                    linetype = "dashed",
                    show.legend = FALSE) +
        annotate("label", 
                 x = min(data$months), 
                 y = max(c(data$trump, state_trump), na.rm = T) + (max(data$trump, na.rm = T) - min(data$trump, na.rm = T))/5, 
                 label = paste0("Election ", state, ": ", round(state_trump, digits = 2), "%"), 
                 hjust = 0) +
        geom_label(data = mean_est,
                   mapping = aes(x = rep(min(data$months), 2),
                                 y = c(min(data$trump, na.rm = T) - 2,
                                       min(data$trump, na.rm = T) - 
                                         (max(c(data$trump, state_trump), na.rm = T) - 
                                            min(c(data$trump, state_trump), na.rm = T))/10 - 2),
                                 label = paste0(raw_adj, " mean: ", round(mean_trump, 2), "%")),
                   position = "identity",
                   show.legend = FALSE,
                   hjust = 0)
      
    }
    else if (who == "clinton") {
      
      base_plot +
        geom_point(aes(y = clinton), 
                   size = .5) +
        geom_abline(aes(linetype = paste0("True population value in ", state), 
                        intercept = state_clinton, 
                        slope = 0)) +
        geom_abline(data = mean_est,
                    mapping = aes(color = raw_adj,
                                  intercept = mean_clinton,
                                  slope = c(0,0)),
                    linetype = "dashed",
                    show.legend = FALSE) +
        scale_linetype_manual(values = c("dashed", "dashed", "dashed")) +
        annotate("label", 
                 x = min(data$months), 
                 y = max(c(data$clinton, state_clinton), na.rm = T) + 
                       (max(data$clinton, na.rm = T) - min(data$clinton, na.rm = T))/5, 
                 label = paste0("Election ", state, ": ", round(state_clinton, digits = 2), "%"), 
                 hjust = 0) +
        geom_label(data = mean_est,
                   mapping = aes(x = rep(min(data$months), 2),
                                 y = c(min(data$clinton, na.rm = T) - 2,
                                       min(data$clinton, na.rm = T) - 
                                         (max(c(data$clinton, state_clinton), na.rm = T) - 
                                            min(c(data$clinton, state_clinton), na.rm = T))/10 - 2),
                                 label = paste0(raw_adj, " mean: ", round(mean_clinton, 2), "%")),
                   position = "identity",
                   show.legend = FALSE,
                   hjust = 0)
      
    }
    else if (who == "dif") {
      
      base_plot +
        geom_point(aes(y = trump - clinton), 
                   size = .5) +
        geom_abline(aes(linetype = paste0("True population value in ", state), 
                        intercept = state_trump - state_clinton, 
                        slope = 0)) +
        geom_abline(data = mean_est,
                    mapping = aes(color = raw_adj,
                                  intercept = mean_trump - mean_clinton,
                                  slope = c(0,0)),
                    linetype = "dashed",
                    show.legend = FALSE) +
        scale_linetype_manual(values = c("dashed", "dashed", "dashed")) +
        annotate("label", 
                 x = min(data$months), 
                 y = max(c(data$trump - data$clinton, state_trump - state_clinton), na.rm = T) + 
                       (max(data$trump - data$clinton, na.rm = T) - 
                          min(data$trump - data$clinton, na.rm = T))/5, 
                 label = paste0("Election ", state, ": ", round(state_trump - state_clinton, digits = 2), "%"), 
                 hjust = 0) +
        geom_label(data = mean_est,
                   mapping = aes(x = rep(min(data$months), 2),
                                 y = c(min(data$trump - data$clinton, na.rm = T) - 2,
                                       min(data$trump - data$clinton, na.rm = T) - 
                                         (max(c(data$trump - data$clinton, state_trump - state_clinton), na.rm = T) - 
                                            min(c(data$trump - data$clinton, state_trump - state_clinton), na.rm = T))/10 - 2),
                                 label = paste0(raw_adj, " mean: ", round(mean_trump - mean_clinton, 2), "%")),
                   position = "identity",
                   show.legend = FALSE,
                   hjust = 0)
    }
  }
  
  else if (xvar == "grade") {
    
    text_medians <- data %>%
      group_by(xvar, raw_adj) %>%
      summarise(trump_m = median(trump),
                clinton_m = median(clinton),
                dif_m = median(trump - clinton))
    
    if (who == "trump") {
      gg_box_plot <- base_plot +
        geom_abline(aes(linetype = paste0("True population value in ", state), 
                        intercept = state_trump, 
                        slope = 0),
                    linetype = "dashed") +
        geom_abline(data = mean_est,
                    mapping = aes(color = raw_adj,
                                  intercept = mean_trump,
                                  slope = c(0,0)),
                    linetype = "dashed",
                    show.legend = FALSE) +
        annotate("label", 
                 x = unique(sort(data$xvar))[1], 
                 y = max(data$trump, na.rm = T) + 2, 
                 label = paste0("Election ", state, ": ", round(state_trump, digits = 2), "%"), 
                 hjust = 0, 
                 vjust = 0) +
        geom_boxplot(mapping = aes(y = trump, 
                                   fill = raw_adj, 
                                   color = NULL),
                     outlier.alpha = .25,
                     position = "dodge2") +
        geom_label(data = mean_est,
                   mapping = aes(x = rep(unique(sort(data$xvar))[1], 2),
                                 y = c(min(data$trump, na.rm = T) - 2,
                                       min(data$trump, na.rm = T) - 
                                         (max(c(data$trump, state_trump), na.rm = T) - 
                                            min(c(data$trump, state_trump), na.rm = T))/10 - 2),
                                 label = paste0(raw_adj, " mean: ", round(mean_trump, 2), "%")),
                   position = "identity",
                   show.legend = FALSE,
                   hjust = 0)
      
      if(box_labs) {
        gg_box_plot + 
          geom_label(data = text_medians,
                     mapping = aes(y = trump_m,
                                   group = xvar,
                                   fill = raw_adj,
                                   label = paste0(round(trump_m, digits = 0)),
                                   vjust = 1.2),
                     position = position_dodge2(width = 0.75),
                     colour = "white",
                     fontface = "bold",
                     show.legend = FALSE)
      }
      else gg_box_plot
      
    }
    else if (who == "clinton") {
      gg_box_plot <- base_plot +
        geom_abline(aes(linetype = paste0("True population value in ", state), 
                        intercept = state_clinton, 
                        slope = 0),
                    linetype = "dashed") +
        geom_abline(data = mean_est,
                    mapping = aes(color = raw_adj,
                                  intercept = mean_clinton,
                                  slope = c(0,0)),
                    linetype = "dashed",
                    show.legend = FALSE) +
        annotate("label", 
                 x = unique(sort(data$xvar))[1], 
                 y = max(data$clinton, na.rm = T) + 2, 
                 label = paste0("Election ", state, ": ", round(state_clinton, digits = 2), "%"), 
                 hjust = 0, 
                 vjust = 0) +
        geom_boxplot(mapping = aes(y = clinton, 
                                   fill = raw_adj, 
                                   color = NULL),
                     outlier.alpha = .25,
                     position = "dodge2") +
        geom_label(data = mean_est,
                   mapping = aes(x = rep(unique(sort(data$xvar))[1], 2),
                                 y = c(min(data$clinton, na.rm = T) - 2,
                                       min(data$clinton, na.rm = T) - 
                                         (max(c(data$clinton, state_clinton), na.rm = T) - 
                                            min(data$clinton, na.rm = T))/10 - 2),
                                 label = paste0(raw_adj, " mean: ", round(mean_clinton, 2), "%")),
                   position = "identity",
                   show.legend = FALSE,
                   hjust = 0)
      
      if(box_labs) {
        gg_box_plot + 
          geom_label(data = text_medians,
                     mapping = aes(y = clinton_m,
                                   group = xvar,
                                   fill = raw_adj,
                                   label = paste0(round(clinton_m, digits = 0)),
                                   vjust = 1.2),
                     position = position_dodge2(width = 0.75),
                     colour = "white",
                     fontface = "bold",
                     show.legend = FALSE)
      }
      else gg_box_plot
    }
    else if (who == "dif") {
      gg_box_plot <- base_plot +
        geom_abline(aes(linetype = paste0("True population value in ", state), 
                        intercept = state_trump - state_clinton, 
                        slope = 0),
                    linetype = "dashed") +
        geom_abline(data = mean_est,
                    mapping = aes(color = raw_adj,
                                  intercept = mean_trump - mean_clinton,
                                  slope = c(0,0)),
                    linetype = "dashed",
                    show.legend = FALSE) +
        annotate("label", 
                 x = unique(sort(data$xvar))[1], 
                 y = max(data$trump - data$clinton, na.rm = T) + 2, 
                 label = paste0("Election ", state, ": ", round(state_trump - state_clinton, digits = 2), "%"), 
                 hjust = 0, 
                 vjust = 0) +
        geom_boxplot(mapping = aes(y = trump - clinton, 
                                   fill = raw_adj, 
                                   color = NULL),
                     outlier.alpha = .25,
                     position = "dodge2") +
        geom_label(data = mean_est,
                   mapping = aes(x = rep(unique(sort(data$xvar))[1], 2),
                                 y = c(min(data$trump - data$clinton, na.rm = T) - 2,
                                       min(data$trump - data$clinton, na.rm = T) - 
                                         (max(c(data$trump - data$clinton, state_trump - state_clinton), na.rm = T) - 
                                            min(c(data$trump - data$clinton, state_trump - state_clinton), na.rm = T))/10 - 2),
                                 label = paste0(raw_adj, " mean: ", round(mean_trump - mean_clinton, 2), "%")),
                   position = "identity",
                   show.legend = FALSE,
                   hjust = 0)
      
      if(box_labs) {
        gg_box_plot + 
          geom_label(data = text_medians,
                     mapping = aes(y = dif_m,
                                   group = xvar,
                                   fill = raw_adj,
                                   label = paste0(round(dif_m, digits = 0)),
                                   vjust = 1.2),
                     position = position_dodge2(width = 0.75),
                     colour = "white",
                     fontface = "bold",
                     show.legend = FALSE)
      }
      else gg_box_plot
    }
  }
  
  else if (xvar == "samplesize") {
    
    if (who == "trump") {
      base_plot +
        geom_point(aes(y = trump),
                   size = 0.5) +
        geom_abline(aes(linetype = paste0("True population value in ", state), 
                        intercept = state_trump, 
                        slope = 0)) +
        scale_linetype_manual(values = "dashed") +
        geom_abline(data = mean_est,
                    mapping = aes(color = raw_adj,
                                  intercept = mean_trump,
                                  slope = c(0,0)),
                    linetype = "dashed",
                    show.legend = FALSE) +
        annotate("label", 
                 x = min(data$xvar), 
                 y = max(c(data$trump, state_trump), na.rm = T) + (max(data$trump, na.rm = T) - min(data$trump, na.rm = T))/5, 
                 label = paste0("Election ", state, ": ", round(state_trump, digits = 2), "%"), 
                 hjust = 0,
                 vjust = 0) +
        geom_label(data = mean_est,
                   mapping = aes(x = rep(min(data$xvar), 2),
                                 y = c(min(data$trump, na.rm = T) - 2,
                                       min(data$trump, na.rm = T) - 
                                         (max(c(data$trump, state_trump), na.rm = T) - 
                                            min(c(data$trump, state_trump), na.rm = T))/10 - 2),
                                 label = paste0(raw_adj, " mean: ", round(mean_trump, 2), "%")),
                   position = "identity",
                   show.legend = FALSE,
                   hjust = 0) +
        scale_x_log10()
    }
    
    else if (who == "clinton") {
      base_plot +
        geom_point(aes(y = clinton),
                   size = 0.5) +
        geom_abline(aes(linetype = paste0("True population value in ", state), 
                        intercept = state_clinton, 
                        slope = 0)) +
        scale_linetype_manual(values = "dashed") +
        geom_abline(data = mean_est,
                    mapping = aes(color = raw_adj,
                                  intercept = mean_clinton,
                                  slope = c(0,0)),
                    linetype = "dashed",
                    show.legend = FALSE) +
        annotate("label", 
                 x = min(data$xvar), 
                 y = max(c(data$clinton, state_clinton), na.rm = T) + (max(data$clinton, na.rm = T) - min(data$clinton, na.rm = T))/5, 
                 label = paste0("Election ", state, ": ", round(state_clinton, digits = 2), "%"), 
                 hjust = 0,
                 vjust = 0) +
        geom_label(data = mean_est,
                   mapping = aes(x = rep(min(data$xvar), 2),
                                 y = c(min(data$clinton, na.rm = T) - 2,
                                       min(data$clinton, na.rm = T) - 
                                         (max(c(data$clinton, state_clinton), na.rm = T) - 
                                            min(c(data$clinton, state_clinton), na.rm = T))/10 - 2),
                                 label = paste0(raw_adj, " mean: ", round(mean_clinton, 2), "%")),
                   position = "identity",
                   show.legend = FALSE,
                   hjust = 0) +
        scale_x_log10()
    }
    
    else if (who == "dif") {
      base_plot +
        geom_point(aes(y = trump - clinton),
                   size = 0.5) +
        geom_abline(aes(linetype = paste0("True population value in ", state), 
                        intercept = state_trump - state_clinton, 
                        slope = 0)) +
        scale_linetype_manual(values = "dashed") +
        geom_abline(data = mean_est,
                    mapping = aes(color = raw_adj,
                                  intercept = mean_trump - mean_clinton,
                                  slope = c(0,0)),
                    linetype = "dashed",
                    show.legend = FALSE) +
        annotate("label", 
                 x = min(data$xvar), 
                 y = max(c(data$trump - data$clinton, state_trump - state_clinton), na.rm = T) + 
                       (max(data$trump - data$clinton, na.rm = T) - min(data$trump - data$clinton, na.rm = T))/5, 
                 label = paste0("Election ", state, ": ", round(state_trump - state_clinton, digits = 2), "%"), 
                 hjust = 0,
                 vjust = 0) +
        geom_label(data = mean_est,
                   mapping = aes(x = rep(min(data$xvar), 2),
                                 y = c(min(data$trump - data$clinton, na.rm = T) - 2,
                                       min(data$trump - data$clinton, na.rm = T) - 
                                         (max(c(data$trump - data$clinton, state_trump - state_clinton), na.rm = T) - 
                                            min(c(data$trump - data$clinton, state_trump - state_clinton), na.rm = T))/10 - 2),
                                 label = paste0(raw_adj, " mean: ", round(mean_trump - mean_clinton, 2), "%")),
                   position = "identity",
                   show.legend = FALSE,
                   hjust = 0) +
        scale_x_log10()
    }
  }
  
  else if (xvar == "population") {
    
    text_medians <- data %>%
      group_by(xvar, raw_adj) %>%
      summarise(trump_m = median(trump),
                clinton_m = median(clinton))
    
    if (who == "trump") {
      gg_box_plot <- base_plot +
        geom_abline(aes(linetype = paste0("True population value in ", state), 
                        intercept = state_trump, 
                        slope = 0)) +
        scale_linetype_manual(values = "dashed") +
        annotate("label", 
                 x = unique(sort(data$xvar))[1], 
                 y = max(data$trump, na.rm = T), 
                 label = paste0("Election ", state, ": ", round(state_trump, digits = 2), "%"), 
                 hjust = 0, 
                 vjust = 0) +
        geom_boxplot(mapping = aes(y = trump, 
                                   fill = raw_adj, 
                                   color = NULL),
                     outlier.alpha = .25,
                     position = "dodge2")
      
      if(box_labs) {
        gg_box_plot + 
          geom_label(data = text_medians,
                     mapping = aes(y = trump_m,
                                   group = xvar,
                                   fill = raw_adj,
                                   label = paste0(round(trump_m, digits = 0)),
                                   vjust = 1.2),
                     position = position_dodge2(width = 0.75),
                     colour = "white",
                     fontface = "bold",
                     show.legend = FALSE)
      }
      else gg_box_plot
    }
    
    else if (who == "clinton") {
      
      gg_box_plot <- base_plot +
        geom_abline(aes(linetype = paste0("True population value in ", state), 
                        intercept = state_clinton, 
                        slope = 0)) +
        scale_linetype_manual(values = "dashed") +
        annotate("label", 
                 x = unique(sort(data$xvar))[1], 
                 y = max(data$clinton, na.rm = T), 
                 label = paste0("Election ", state, ": ", round(state_clinton, digits = 2), "%"), 
                 hjust = 0, 
                 vjust = 0) +
        geom_boxplot(mapping = aes(y = clinton, 
                                   fill = raw_adj, 
                                   color = NULL),
                     outlier.alpha = .25,
                     position = "dodge2")
      
      if(box_labs) {
        gg_box_plot + 
          geom_label(data = text_medians,
                     mapping = aes(y = clinton_m,
                                   group = xvar,
                                   fill = raw_adj,
                                   label = paste0(round(clinton_m, digits = 0)),
                                   vjust = 1.2),
                     position = position_dodge2(width = 0.75),
                     colour = "white",
                     fontface = "bold",
                     show.legend = FALSE)
      }
      else gg_box_plot
      
    }
    
    else if (who == "dif") {
      
      gg_box_plot <- base_plot +
        geom_abline(aes(linetype = paste0("True population value in ", state), 
                        intercept = state_trump - state_clinton, 
                        slope = 0)) +
        scale_linetype_manual(values = "dashed") +
        annotate("label", 
                 x = unique(sort(data$xvar))[1], 
                 y = max(data$trump - data$clinton, na.rm = T), 
                 label = paste0("Election ", state, ": ", round(state_trump - state_clinton, digits = 2), "%"), 
                 hjust = 0, 
                 vjust = 0) +
        geom_boxplot(mapping = aes(y = trump - clinton, 
                                   fill = raw_adj, 
                                   color = NULL),
                     outlier.alpha = .25,
                     position = "dodge2")
      
      if(box_labs) {
        gg_box_plot + 
          geom_label(data = text_medians,
                     mapping = aes(y = trump_m - clinton_m,
                                   group = xvar,
                                   fill = raw_adj,
                                   label = paste0(round(trump_m - clinton_m, digits = 0)),
                                   vjust = 1.2),
                     position = position_dodge2(width = 0.75),
                     colour = "white",
                     fontface = "bold",
                     show.legend = FALSE)
      }
      else gg_box_plot
      
    }
    
  }
  
}

xvar_choices <- c("Enddate of survey" = "enddate", "Grade" = "grade",
                  "Sample size" = "samplesize", "Population" = "population")
