# This is the main file of the Cocktail Shiny App :)

library(shiny)
# UI
ui <- fluidPage(
  
)
print("Zanis is not so cool")
# Server
server <- function(input, output) {
  output$salutation <- renderText({
    paste0("Hello ", input$user.name.1, ", ",
    "Hello ", input$user.name.2, ", ",
    "Hello ", input$user.name.3, ", ",
    "Hello ", input$user.name.4, "! ")
    })
}

#shinyApp()
shinyApp(ui = ui, server = server)