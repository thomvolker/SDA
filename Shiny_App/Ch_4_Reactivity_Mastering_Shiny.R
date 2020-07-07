library(shiny)

ui <- fluidPage(
  textOutput("greeting")
)

server <- function(input, output, session) {
  output$greeting <- renderText("Hello human!")
}

# input objects are read-only. If you attempt to modify an input inside the server 
# function, you'll get an error. This error occurs because input reflects what's 
# happening in the browser, and the browser is Shiny's "single source of truth". If you
# could modify the value in R, you could introduce inconsistencies, where the input
# slider said one thing in the browser, and input$count said something different in R.
# One more important thing about input: it's selective about who is allowed to read it. 
# To read from an input, you must be in a reactive context created by a function like
# renderText() or reactive(). 

# Output is very similar to input: it's also a list-like object named according to the
# output ID. The main difference is that you use it for sending output instead of 
# receiving input. You always use the output object in concert with a render function.

# The render function does two things: 
# 1. It sets up a special reactive context that automatically tracks what inputs the
#    output uses.
# 2. It converts the output of your R code into HTML suitable for display on a web page.

########################################################################################
## REACTIVE PROGRAMMING                                                               ##
########################################################################################

# An app is going to be pretty boring if it only has inputs or only has outputs. The 
# real magic of Shiny happens when you have an app with both.

ui1 <- fluidPage(
  textInput("name", "What's your name?"),
  textOutput("greeting")
)

server1 <- function(input, output, session) {
  output$greeting <- renderText({
    paste0("Hello ", input$name, "!")
  })
}

# It's easy to read this code as "paste together 'hello' and the user's name, then send
# it to output$greeting". But this mental model is wrong in a subtle, but important, way.
# Think about it: with this model, you only issue the instruction once, but Shiny 
# performs the action every time we update input$name, so there must be something more 
# going on.
# This app works because the code doesn't tell Shiny to create the string and send it to
# the browser, but instead, it informs Shiny how it could create the string if it needs
# to. It's up to Shiny when (and even if!) the code should be run. Think of your app as
# providing Shiny with recipes, not giving it commands.


########################################################################################
## Imperative vs declarative programming                                              ##
########################################################################################

# The difference between commands and recipes is one of the key differences between two
# important styles of programming:
# Imperative programming: you issue a specific command and it's carried out immediately.
# This is the style of programming you're used to in your analysis scripts: you command
# R to load your data, transform it, visualise it, and save the results to disk.
# Declarative programming: you express higher-level goals or describe important 
# constraints, and rely on someone else to decide how and/or when to translate that into
# action. This is the style of programming you use in Shiny.
# With imperative code you say "make me a sandwich". With declarative code you say 
# "ensure there is a sandwich in the refrigerator whenever I look inside of it". 
# Imperative code is assertive; declarative code is passive-aggressive. 

# One of the strengths of declarative programming in Shiny is that it allows apps to be
# extremely lazy. A shiny app will only ever do the minimal amount of work needed to 
# update the output controls that you can currently see. Shiny's laziness has an 
# important property. In most R code, you can understand the order of execution by 
# reading the code from top to bottom. That doesn't work in Shiny, because code is only
# run when when needed. To understand the order of execution you need to instead look at 
# the reactive graph, which describes how inputs and outputs are connected.

ui2 <- fluidPage(
  textInput("name", "What's your name?"),
  textOutput("greeting")
)

server2 <- function(input, output, session) {
  text <- reactive(paste0("Hello ", input$name, "!"))
  output$greeting <- renderText(text())
}

# The code above makes use of a reactive expression. Reactive expressions can be seen
# as a tool that reduces duplication in your reactive code by introducing additional 
# nodes into the reactive graph. Reactive expressions take inputs and produce outputs so
# they have a shape that combines features of both inputs and outputs. It's important
# to understand that the order in which your code is run is determined solely by the 
# reactive graph. This is different from most R code where the execution order is 
# determined by the order of lines. For example, we could flip the order of the two 
# lines in our simple server function:

server3 <- function(input, output, session) {
  output$greeting <- renderText(text())
  text <- reactive(paste0("Hello ", input$name, "!"))
}

# Due to Shiny's laziness, the code is only run when the session starts, after `text`
# has been created. 

########################################################################################
## Reactive expressions                                                               ##
########################################################################################

# Reactive expressions are important for two reasons
# 1. They give Shiny more information so that it can do less recomputation when inputs
#    change, making apps more efficients.
# 2. They make it easier for humans to understand the app by simplifying the reactive
#    graph.
# Reactive expressions have a flavour of both inputs and outputs:
# Like inputs, you can use the results of a reactive expression in an output. And like
# outputs, reactive expressions depend on inputs and automatically know when they need
# updating. 
# Because of this duality, some functions work with eiter reactive inputs or expressions,
# and some functions work with either reactive expressions or reactive outputs. We'll 
# use producers to refer to either reactive inputs or expressions, and consumers to
# refer to either reactive expressions or outputs. 

library(ggplot2)

histogram <- function(x1, x2, binwidth = .1, xlim = c(-3,3)) {
  df <- data.frame(
    x = c(x1, x2),
    g = c(rep("x1", length(x1)), rep("x2", length(x2)))
  )
  
  ggplot(df, aes(x, fill = g)) +
    geom_histogram(binwidth = binwidth) +
    coord_cartesian(xlim = xlim) + 
    theme_classic()
}

t_test <- function(x1, x2) {
  test <- t.test(x1, x2)
  
  sprintf(
    "p value: %0.3f\n[%0.2f, %0.2f]",
    test$p.value, test$conf.int[1], test$conf.int[2]
  )
}

x1 <- rnorm(100, mean = 0, sd = 0.5)
x2 <- rnorm(200, mean = 0.15, sd = 0.9)

histogram(x1, x2)
cat(t_test(x1, x2))

# Extracting imperative code out into regular functions is an important technique for
# all Shiny apps: the more code you can extract out of your app, the easier it will be
# to understand. This is good software engineering because it helps isolate concerns:
# the functions outside of the app focus on the computation so that the code inside of
# the app can focus on responding to user actions. 

# If one wants to use these functions to quickly explore a bunch of simulations, a Shiny
# app is a great way to do this because it lets you avoid tediously modifying and 
# re-running R code. Below, the pieces are wrapped into a Shiny app where one can 
# interactively tweak the inputs. 

ui4 <- fluidPage(
  fluidRow(
    column(4,
           "Distribution 1",
           numericInput("n1", label = "n", value = 1000, min = 1),
           numericInput("mean1", label = "µ", value = 0, step = 0.1),
           numericInput("sd1", label = "σ", value = 0.5, min = 0.1, step = 0.1)
           ),
    column(4,
           "Distribution 2",
           numericInput("n2", label = "n", value = 1000, min = 1),
           numericInput("mean2", label = "µ", value = 0, step = 0.1),
           numericInput("sd2", label = "σ", value = 0.5, min = 0.1, step = 0.1)
           ),
    column(4,
           "Histogram",
           numericInput("binwidth", label = "Bin width", value = 0.1, step = 0.1),
           sliderInput("range", label = "range", value = c(-3,3), min = -5, max = 5)
           )
  ),
  fluidRow(
    column(9, plotOutput("hist")),
    column(3, verbatimTextOutput("ttest"))
  )
)

server4 <- function(input, output, session) {
  output$hist <- renderPlot({
    x1 <- rnorm(input$n1, input$mean1, input$sd1)
    x2 <- rnorm(input$n2, input$mean2, input$sd2)
    
    histogram(x1, x2, binwidth = input$binwidth, xlim = input$range)
  }, res = 96)
  
  output$ttest <- renderText({
    x1 <- rnorm(input$n1, input$mean1, input$sd1)
    x2 <- rnorm(input$n2, input$mean2, input$sd2)
    
    t_test(x1, x2)
  })
}

########################################################################################
## The reactive graph                                                                 ##
########################################################################################

# Shiny updats an output only when the inputs it refers to change, but it's not smart 
# enough to only selectively run pieces of code inside an output. In other words, 
# outputs are atomic: they're either executed or not as a whole. The fact that in the 
# code above almost everything is connected to eachother, results in a very dense 
# reactive graph, which poses two problens:
# 1. The app is hard to understand because there are so many connections. There are no
#    pieces of the app that you can pull out and analyse in isolation.
# 2. The app is inefficient because it does more work than necessary. For example, if
#    you change the breaks of the plot, the data is recalculated; if you change the 
#    value of n1, x2 is updated in two places.
# There is one major flaw in the app: the histogram and t-test use separate random draws.
# This is rather misleading, as you'd expect them to be working on the same underlying
# data. All these issues can be fixed by using reactive expressions.

# Simplifying the graph

server5 <- function(input, output, session) {
  x1 <- reactive(rnorm(input$n1, input$mean1, input$sd1))
  x2 <- reactive(rnorm(input$n2, input$mean2, input$sd2))
  
  output$hist <- renderPlot({
    histogram(x1(), x2(), binwidth = input$binwidth, xlim = input$range)
  }, res = 96)
  
  output$ttest <- renderText({
    t_test(x1(), x2())
  })
}

# When you first start working with reactive code, you might wonder why we need reactive
# expressions. Why can't you use your existing tools for reducing duplication in code?
# Unfortunately neither of these techniques work in a reactive environment. 

########################################################################################
## Controlling timing of evaluation                                                   ##
########################################################################################

# 






shinyApp(ui4, server5)
