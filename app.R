# This is the main file of the Cocktail Shiny App :)

library(shiny)
# UI
ui <- fluidPage(
  titlePanel("Hello boys! Your group seems to be awesome but I need your names!"),
  textInput("user.name.1", "The first name", ""),
  textInput("user.name.2", "The second name", ""),
  textInput("user.name.3", "The third name", ""),
  textInput("user.name.4", "The fourth name", ""),
  textOutput("salutation")
)
# Server
server <- function(input, output) {
  
}

#shinyApp()
shinyApp(ui = ui, server = server)