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
shinyApp(ui1, server1)
