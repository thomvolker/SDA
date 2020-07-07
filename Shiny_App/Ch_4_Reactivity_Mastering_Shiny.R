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

shinyApp(ui2, server3)
