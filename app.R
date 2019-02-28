# This is the main file of the Cocktail Shiny App :)

library(shiny)
# UI
# Define UI for cocktail app ----
ui <- fluidPage(
  titlePanel("Hello boys! Your group seems to be awesome but I need your names!"),
  mainPanel(
    
    # RadioButtons - distribution of obs.
    radioButtons('dist.obs', 'Drinks distributed by:', 
                 c('Alcoholic nature' = 'an',
                   'Drink type' = 'dt',
                   'Glass type' = 'gt',
                   'Complexity' = 'cc',
                   'Popularity' = 'pp',
                   'Price' = 'pp'))
    
    
    
    ),
  # SliderInput - Network of drinks
  sliderInput('weight.edges',
              label = 'Min. weight of edges:', 
              min = 1, max = 20, value = c(1,20), step = 1
  ),
  
  
  
  

  # Title and tabpanels with drop-downs
  navbarPage(title = "Shiny Drinks",
            # 1st Drop-down tabpanels
             navbarMenu("Data Desription", 
                            tabPanel("Summary of Data",   
                                     splitLayout( 
                                       # object 1
                                       verticalLayout( 
                                         # object 1
                                         titlePanel("Header Object 1"),
                                         # object 2,
                                         img(src ='A.png', height = 140, width =300)
                                         ),
                                       # object 2
                                       verticalLayout( 
                                         # object 1
                                         titlePanel("Header Object 2"),
                                         # object 2,
                                         img(src ='A.png', height = 140, width =300)
                                         )
                                       )
                                     ),
                        tabPanel("Data by Drinks", "content 2")
                        ),
            # 2nd tabpanel
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
                       tabPanel("Bipartite visualization", "contents")),
            # 2nd tabpanel
            tabPanel("tab 3", "contents")
  )

)


# Server
server <- function(input, output) {

  # RadioButtons - distr. of obs. 
  d <- reactive({
    dist.obst <- switch (input$dist.obs,
      an = action
    )
  })
  # SliderInput - Network of drinks 
  output$scatterplot <- renderPlot({})

  
  }


#shinyApp()
shinyApp(ui = ui, server = server)

