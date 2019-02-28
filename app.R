# This is the main file of the Cocktail Shiny App :)

library(shiny)
# UI
ui <- fluidPage(
  titlePanel("Hello boys! Your group seems to be awesome but I need your names!"),
  mainPanel(
    # testing code, remove later
    actionButton('showimage', 'Click me!', icon = NULL),
    img(src = 'A.png', height = 140, width = 300),
    
    # RadioButtons - distribution of obs.
    radioButtons('dist.obs', 'Drinks distributed by:', 
                 c('Alcoholic nature' = 'an',
                   'Drink type' = 'dt',
                   'Glass type' = 'gt',
                   'Complexity' = 'cc',
                   'Popularity' = 'pp',
                   'Price' = 'pp')),
    
    
    ),
  
  sidebarPanel()
)
# Server
server <- function(input, output) {

  d <- reactive({
    dist.obst <- switch (input$dist.obs,
      an = action
    )
  })
  # testing code 
  output$showimage <- renderImage({paste(img(src = 'B.png'))})

  
  }

#shinyApp()
shinyApp(ui = ui, server = server)

