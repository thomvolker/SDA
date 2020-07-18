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

ui6 <- fluidPage(
  fluidRow(
    column(3,
           numericInput("lambda1", label = "lambda1", value = 3),
           numericInput("lambda2", label = "lambda2", value = 3),
           numericInput("n", label = "n", value = 1000, min = 0)
           ),
    column(9, plotOutput("hist"))
  )
)

server6 <- function(input, output, session) {
  x1 <- reactive(rpois(input$n, input$lambda1))
  x2 <- reactive(rpois(input$n, input$lambda2))
  output$hist <- renderPlot({
    histogram(x1(), x2(), binwidth = 1, xlim = c(0,40))
  }, res = 96)
}

# Timed invalidation
# Imagine you wanted to reinforce the fact that this is for simulated data by constantly
# resimulating the data, so that you see an animation rather than a static plot. We can 
# increase the frequency of updates with a new function: reactiveTimer().
# reactiveTimer() is a reactive expression that has a dependency on a hidden input: the
# current time. You can use a reactiveTimer() when you want a reactive expression to
# invalidate itself more than it otherwise would. 

server7 <- function(input, output, session) {
  timer <- reactiveTimer(1000)
  
  x1 <- reactive({
    timer()
    rpois(input$n, input$lambda1)
  })
  x2 <- reactive({
    timer()
    rpois(input$n, input$lambda2)
  })
  output$hist <- renderPlot({
    histogram(x1(), x2(), binwidth = 1, xlim = c(0, 40))
  }, res = 96)
}

# Note how we use timer() in the reactive expressions that compute x1() and x2(): we call
# it, but don't use the value. This lets x1 and x2 take a dependency on timer, without
# worrying about exactly what value it returns.

# On click
# In the above scenario, think about what would happen if the simulation code took 1
# second to run. We perform the simulation every 0.5s, so Shiny would have more and more
# to do, ,and would never be able to catch up. The same problem can happen if someone is
# rapidly clicking buttons in your app and the computation is relatively expensive. It's
# possible to create a big backlog of work for Shiny, and while it's working on the 
# backlog, it can't respond to any new events. This leads to a poor user experience.

# If this situation arises in your app, you might want to require the user to opt-in to
# performing the expensive calculation by requiring them to click a button. This is a 
# great use case for an actionButton():

ui8 <- fluidPage(
  fluidRow(
    column(3,
           numericInput("lambda1", label = "lambda1", value = 3),
           numericInput("lambda2", label = "lambda2", value = 3),
           numericInput("n", label = "n", value = 1000, min = 0),
           actionButton("simulate", "Simulate!")
           ),
    column(9, plotOutput("hist"))
    )
)

# To use the action button, we need to learn a new tool. To see why, let's first tackle
# the problem using the same approach as above. As above, we refer to simulate without
# using its value to take a reactive dependency on it.

server8 <- function(input, output, session) {
  x1 <- reactive({
    input$simulate
    rpois(input$n, input$lambda1)
  })
  x2 <- reactive({
    input$simulate
    rpois(input$n, input$lambda2)
  })
  output$hist <- renderPlot({
    histogram(x1(), x2(), binwidth = 1, xlim = c(0, 40))
  }, res = 96)
}

# However, this approach doesn't achieve our goal, because it just introduces a new 
# dependency: x1() and x2() will update when we click the simulate button, but they'll 
# also continue to update when lambda1, lambda2 or n change. We want to replace the 
# existing dependencies, not add to them. To solve this problem, we need a new tool: a
# way to use input values without taking a reactive dependency on them. We need
# eventReactive(), which has two arguments: the first argument specifies what to take a
# dependency on, and the second argument specifies what to compute. That allows this app 
# to only compute x1() and x2() when simulate is clicked:

server9 <- function(input, output, session) {
  x1 <- eventReactive(input$simulate, {
    rpois(input$n, input$lambda1)
  })
  x2 <- eventReactive(input$simulate, {
    rpois(input$n, input$lambda2)
  })
  output$hist <- renderPlot({
    histogram(x1(), x2(), binwidth = 1, xlim = c(0, 40))
  }, res = 96)
}

# Observers
# So far, we've focused on what's happening inside the app. But sometimes you need to
# reach outside of the app and cause side-effects to happen elsewhere in the world. This
# might be saving a file to a shared network drive, sending data to a web API, updating 
# a database, or (most commonly) printing a debugging message to the console. These
# actions don't affect how your app looks, so you can't use an output and a render 
# function. Instead, you need to use an observer.
# observeEvent() is very similar to eventReactive(). It has two important arguments: 
# eventExpr and handlerExpr. The first argument is the input or expression to take a
# dependency on; the second argument is the code that will be run. For example, the 
# following modification to server() means that every time that name is updated, a 
# message will be sent to the console

ui10 <- fluidPage(
  textInput("name", "What's your name?"),
  textOutput("greeting")
)


server10 <- function(input, output, session) {
  text <- reactive(paste0("Hello ", input$name, "!"))
  
  output$greeting <- renderText(text())
  observeEvent(input$name, {
    message("Greeting performed")
  })
}

# There are two important differences between observeEvent() and eventReactive():
# 1. You don't assign the result of observeEvent() to a variable, so
# 2. you can't refer to it from other reactive consumers.
# Observers and outputs are closely related. You can think of outputs as having a special
# side effect: updating the HTML in the user's browser. To emphasise this closeness, 
# we'll draw them the same way in the reactive graph. 

shinyApp(ui10, server10)
