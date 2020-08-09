#########################################################################################
## File for all data handling                                                          ##
#########################################################################################

## Load tidyverse for the pipe, and the dplyr functions
library(tidyverse)

## Load in the data (due to a semicolon after each row, we get rows containing only a
## semicolon, which we exclude from the data, additionally, there are five columns of
## nonsense in the dataframe (probably due to how I read the data in), which are 
## excluded as well).
data <- read.csv("~/Documents/M_S/SA_Peter/Survey_Data_Analysis/president_general_polls_2016_ed.csv")[,-(27:31)] %>% filter(cycle != ";")

#########################################################################################
## Add actual presidential results of 2016 election                                    ##
#########################################################################################

## Look at all unique states
unique(data$state)

## Load in a dataset with the population election results in all states
pop_dat <- read.csv("~/Downloads/1976-2016-president.csv")

## In the population data, extract only the rows concerning 2016, remove the variable
## party, which is not of any interest, but will mess up the pivoting, keep only Trump
## and Clinton, extract all variables, but sum the votes for the candidates based on 
## the state (this happens because in New York, there are three rows with Clinton data,
## all indicating a different party, and we need to add these in a single value). 
## Then, using pivot_wider make separate columns for trump and clinton votes, instead
## of separate rows, and compute the proportion of votes for trump and clinton
pop_dat <- pop_dat %>% 
  filter(year == 2016) %>%
  select(-party) %>%
  filter(candidate == "Trump, Donald J." | candidate == "Clinton, Hillary") %>%
  mutate(candidate = recode(candidate, 
                            "Trump, Donald J." = "Trump", 
                            "Clinton, Hillary" = "Clinton")) %>%
  group_by(state, candidate) %>%
  summarise(year = unique(year),
            state = unique(state),
            state_po = unique(state_po),
            state_fips = unique(state_fips), 
            state_cen = unique(state_cen),
            state_ic = unique(state_ic),
            office = unique(office),
            candidate = unique(candidate),
            writein = unique(writein),
            candidatevotes = sum(candidatevotes),
            totalvotes = unique(totalvotes),
            version = unique(version),
            notes = unique(notes)) %>%
  pivot_wider(names_from = candidate, values_from = candidatevotes) %>%
  mutate(prop_clinton = 100 * (Clinton / totalvotes),
         prop_trump = 100 * (Trump / totalvotes))

## Now extract only the states and the votes
pop_votes <- pop_dat[,c("state", "prop_clinton", "prop_trump")]

## Add the district data manually, because it is not included in the data thus far, and
## add the data of the U.S. data as a whole
pop_votes <- bind_rows(pop_votes, 
                       data.frame(state = c("U.S.", "Maine CD-1", "Maine CD-2", 
                                            "Nebraska CD-1", "Nebraska CD-2", "Nebraska CD-3"),
                                  prop_clinton = c(48.2, 53.96, 40.97, 36, 45, 20),
                                  prop_trump = c(46.1, 39.15, 51.26, 56, 47, 74)))

## Now add variables to the total dataset for the population score of trump and clinton
## and loop over all states to add the population score in that state to the dataset
## containing the election info
data$pop_clinton <- data$pop_trump <- NA

for (i in 1:nrow(pop_votes)) {
  data$pop_clinton[data$state %in% pop_votes$state[i]] <- pop_votes$prop_clinton[i]
  data$pop_trump[data$state %in% pop_votes$state[i]] <- pop_votes$prop_trump[i]
}

#########################################################################################
## Difference between raw and adjusted proportions, and the correct structure of the   ##
## variables                                                                           ##
#########################################################################################

data <- data %>%
  mutate(forecastdate = as.Date(forecastdate, format = "%m/%d/%y"),
         startdate =  as.Date(startdate, format = "%m/%d/%Y"),
         enddate = as.Date(enddate, format = "%m/%d/%Y"),
         samplesize = as.numeric(samplesize),
         poll_wt = as.numeric(poll_wt),
         rawpoll_clinton = as.numeric(rawpoll_clinton),
         rawpoll_trump = as.numeric(rawpoll_trump),
         adjpoll_clinton = as.numeric(adjpoll_clinton),
         adjpoll_trump = as.numeric(adjpoll_trump),
         raw_adj_dif_clinton = adjpoll_clinton - rawpoll_clinton,
         raw_adj_dif_trump = adjpoll_trump - rawpoll_trump,
         createddate = as.Date(createddate, format = "%m/%d/%y"),
         months = enddate %>% format("%Y-%m") %>% paste0(., "-01") %>% as.Date)

data$rawpoll_clinton[data$rawpoll_clinton == 600] <- NA

data <- data %>%
  pivot_longer(cols = c("rawpoll_trump", "adjpoll_trump", "rawpoll_clinton", "adjpoll_clinton"), 
               names_to = c("raw_adj", ".value"),
               names_sep = "_") %>%
  mutate(raw_adj =  recode(raw_adj, "adjpoll" = "Adjusted", "rawpoll" = "Raw")) %>%
  group_by(months, raw_adj) %>%
  mutate(month_mean_clinton = mean(clinton, na.rm = T),
         month_mean_trump = mean(trump, na.rm = T))

#data$population[!data$population %in% c("lv", "rv")] <- NA
