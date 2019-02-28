# This is the main file of the Cocktail Shiny App :)

library(shiny)
# UI
ui <- fluidPage( tabsetPanel(
  tabPanel("tab 1", "contents"),
  tabPanel("tab 2", "contents"),
  tabPanel("tab 3", "contents")))
# Server
server <- function(input, output) {

}

#shinyApp()
shinyApp(ui = ui, server = server)
