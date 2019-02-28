# This is the main file of the Cocktail Shiny App :)

library(shiny)
# UI
ui <- fluidPage(
  titlePanel("Hello boys! Your group seems to be awesome but I need your names!"),
  mainPanel(
    # testing code, remove later
    actionButton('showimage', 'Click me!', icon = NULL),
    img(src = 'A.png', height = 140, width = 300)),
  textInput("user.name.1", "The first name", ""),
  textInput("user.name.2", "The second name", ""),
  textInput("user.name.3", "The third name", ""),
  textInput("user.name.4", "The fourth name", ""),
  textOutput("salutation"),
  
  sidebarPanel()
  
)
# Server
server <- function(input, output) {
  output$salutation <- renderText({
    paste0("Hello ", input$user.name.1, ", ",
    "Hello ", input$user.name.2, ", ",
    "Hello ", input$user.name.3, ", ",
    "Hello ", input$user.name.4, "! ")
    })
  # testing code wjltn
  output$showimage <- renderImage({paste(img(src = 'B.png'))})
}

#shinyApp()
shinyApp(ui = ui, server = server)
