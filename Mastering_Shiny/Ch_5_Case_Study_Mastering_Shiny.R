library(shiny)
library(vroom)
library(tidyverse)
library(neiss)

top_prod <- injuries %>%
  filter(trmt_date >= as.Date("2017-01-01"), trmt_date < as.Date("2018-01-01")) %>%
  count(prod1, sort = TRUE) %>%
  filter(n > 5 * 365)

injuries <- injuries %>%
  filter(trmt_date >= as.Date("2017-01-01"), trmt_date < as.Date("2018-01-01")) %>%
  semi_join(top_prod, by = "prod1") %>%
  mutate(age = floor(age), sex = tolower(sex), race = tolower(race)) %>%
  filter(sex != "unknown") %>%
  select(trmt_date, age, sex, race, body_part, diag, location, prod_code = prod1, weight, narrative) 

products <- products %>%
  semi_join(top_prod, by = c("code" = "prod1")) %>%
  rename(prod_code = code)

population <- population %>%
  filter(year == 2017) %>%
  select(-year) %>%
  rename(population = n)

selected <- injuries %>% filter(prod_code == 1842)

selected %>% count(diag, wt = weight, sort = TRUE)
selected %>% count(body_part, wt = weight, sort = TRUE)
selected %>% count(location, wt = weight, sort = TRUE)

summary <- selected %>% count(age, sex, wt = weight)

summary %>% ggplot(aes(age, n, colour = sex)) +
  geom_line() +
  labs(y = "Estimated number of injuries")

summary <- selected %>%
  count(age, sex, wt = weight) %>%
  left_join(population, by = c("age", "sex")) %>%
  mutate(rate = n/population * 1e4)

summary %>%
  ggplot(aes(age, rate, colour = sex)) +
  geom_line(na.rm = T) +
  labs(y = "Injuries per 10,000 people")

# When building a complex app, I strongly recommend starting as simple as possible, so 
# that you can confirm the basic mechanics work before you start doing something more 
# complicated. Here I'll start with one input (the product code), three tables, and one
# plot. 

ui <- fluidPage(
  fluidRow(
    column(6,
           selectInput("code", "Product", setNames(products$prod_code, products$title))
    )
  ),
  fluidRow(column(4, tableOutput("diag")),
           column(4, tableOutput("body_part")),
           column(4, tableOutput("location"))
  ),
  fluidRow(
    column(12, plotOutput("age_sex"))
  )
)

# The use of setNames() in the selectInput() choices shows the product name in the UI and
# returns the product code to the server. The server function is relatively
# straightforward. I first convert the selected and summary variables (defined above) to 
# reactive expressions. This is a reasonable general pattern: you create variables in 
# your data analysis to decompose the analysis into steps, and to avoid recomputing 
# things multiple times, and reactive expressions play the same role in Shiny apps.

server <- function(input, output, session) {
  selected <- reactive(injuries %>% filter(prod_code == input$code))
  
  output$diag <- renderTable(
    selected() %>% count(diag, wt = weight, sort = TRUE)
  )
  output$body_part <- renderTable(
    selected() %>% count(body_part, wt = weight, sort = T)
  )
  output$location <- renderTable(
    selected() %>% count(location, wt = weight, sort = T)
  )
  summary <- reactive({
    selected() %>%
      count(age, sex, wt = weight) %>%
      left_join(population, by = c("age", "sex")) %>%
      mutate(rate = n / population * 1e4)
  })
  output$age_sex <- renderPlot({
    summary() %>%
      ggplot(aes(age, n, colour = sex)) +
      geom_line() +
      labs(y = "Estimated number of injuries")
  }, res = 96)
}

# The app currently shows a lot of information in the tables, where we probably only want
# the highlights. To fix this, we need to truncate the tables (which is done by a
# combination of forcats functions: the variable is converted to a factor, ordered by the
# frequency of the levels, and then lumped together all levels after the top 5).

count_top <- function(df, var, n = 5) {
  df %>%
    mutate({{ var }} := fct_lump(fct_infreq({{ var }}), n = n)) 
}



shinyApp(ui, server)    

injuries$location

