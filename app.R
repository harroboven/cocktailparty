# This is the main file of the Cocktail Shiny App :)

library(shiny)
library(markdown)
# UI
ui <- navbarPage(title = "Shiny Drinks",
  tabPanel("tab 1", "contents"),
  navbarMenu("Networking Exploration",
    tabPanel("Exploration by drinks", 
             verticalLayout(
               titlePanel("Exploration by drinks"),
               (splitLayout(
                 verticalLayout(
                   titlePanel("Summary Statistics of the Network by DRINKS"), 
                   img(src = 'A.png', height = 300, width = 300)),
                 titlePanel("Table with summary statistics"))))),
    tabPanel("Exploration by ingredients", "contents"),
    tabPanel("Bipartite visualization", "contents")
  ),
  tabPanel("tab 3", "contents")
)
# Server
server <- function(input, output) {
  output$showimage <- renderImage({img(src = 'B.png')})

}

#shinyApp()
shinyApp(ui = ui, server = server)
