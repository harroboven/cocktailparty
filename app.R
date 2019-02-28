# This is the main file of the Cocktail Shiny App :)

library(shiny)
library(markdown)
# UI
ui <- navbarPage(title = "Shiny Drinks",
  tabPanel("tab 1", "contents"),
  navbarMenu("Networking Exploration",
    tabPanel("Exploration by drinks", splitLayout(
      actionButton(inputId, label, icon),
      actionButton(inputId, label, icon)
    ),
    tabPanel("Exploration by ingredients", "contents"),
    tabPanel("Bipartite visualization", "contents"))
  ),
  tabPanel("tab 3", "contents")
)
# Server
server <- function(input, output) {
  output$showimage <- renderImage({img(src = 'B.png')}

}

#shinyApp()
shinyApp(ui = ui, server = server)
