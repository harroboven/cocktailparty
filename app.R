# This is the main file of the Cocktail Shiny App :)
library(ggplot2)
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
############################################################# PAGE 1 PROPOSAL ############################################################# 
                        # 1st Drop-down item
                        tabPanel("Summary of Data", 
                                 verticalLayout(
                                   # header of whole page
                                   titlePanel("Summary of Data"),
                                   # 1st Object of page
                                   splitLayout( 
                                     # left object 
                                     verticalLayout(
                                       #Header of left object
                                       titlePanel("Explanation of Summary"),
                                       #content of left object
                                       p("INTROTEXT", 
                                         style = "font-family: 'times'; font-si16pt")
                                       ),
                                     # right object 
                                     verticalLayout( 
                                       # Header right object
                                       titlePanel("Header Object 2"),
                                       # content of right object
                                       p("Summary Statistics: Table with number of observations etc.", 
                                         style = "font-family: 'times'; font-si16pt")
                                       )
                                     ),
                                   # 2nd Object of page
                                   splitLayout( 
                                     # left object 
                                     verticalLayout(
                                       #Header of left object
                                       titlePanel("Distribution of Observations"),
                                       #content of left object
                                       p("INTROTEXT", 
                                         style = "font-family: 'times'; font-si16pt"),
                                       p("Drinks distributed by:", 
                                         style = "font-family: 'times'; font-si16pt"),
                                       flowLayout( 
                                         # Element 1
                                         img(src = 'A.png', height = 15, width = 15),
                                         # Element 2
                                         img(src = 'A.png', height = 15, width = 15),
                                         # Element 3
                                         img(src = 'A.png', height = 15, width = 15),
                                         # Element 4
                                         img(src = 'A.png', height = 15, width = 15),
                                         # Element 5
                                         img(src = 'A.png', height = 15, width = 15),
                                         # Element 6
                                         img(src = 'A.png', height = 15, width = 15)
                                       )
                                     ),
                                     # right object 
                                     verticalLayout( 
                                       # Header right object
                                       titlePanel("Header Object 2"),
                                       # content of right object
                                       # Alcoholic nature histogram
                                       plotOutput(outputId = "hist.alc.nat")
                                       )
                                     )
                                   )
                                   )
                        ),
############################################################# PAGE 2 PROPOSAL ############################################################# 
                        # 2nd Drop-down item
                        tabPanel("Data by Drinks", "content 2")
                        ),
            # 2nd tabpanel
            navbarMenu("Networking Exploration",
############################################################# PAGE 3 PROPOSAL #############################################################
                       # 1st Drop-down item
                       tabPanel("Exploration by drinks", 
                                verticalLayout(
                                  # header of whole page
                                  titlePanel("Exploration by drinks"),
                                  # rest of page
                                  splitLayout(
                                    # left object 
                                    verticalLayout(
                                      #title of left object
                                      titlePanel("Summary Statistics of the Network by DRINKS"),
                                      #content of left object
                                      img(src = 'A.png', height = 300, width = 300)),
                                    # right object
                                    verticalLayout(
                                      #title of right object
                                      titlePanel("Table with summary statistics"),
                                      #content of right object
                                      img(src = 'A.png', height = 300, width = 300)
                                      )
                                    )
                                  )
                                ),
############################################################# PAGE 4 PROPOSAL #############################################################
                       # 2nd Drop-down item
                       tabPanel("Exploration by ingredients", 
                                "contents"
                                ),
############################################################# PAGE 5 PROPOSAL #############################################################
                       # 3rd Drop-down item
                       tabPanel("Bipartite visualization", 
                                "contents"
                                )
                       ),
            # 2nd tabpanel
            tabPanel("tab 3", 
                     "contents"
                     )
            )
  )

# Server
server <- function(input, output) {
  
  # histogram alcoholic nature
  output$hist.alc.nat <- renderPlot({
    ggplot(drinks[unique(id), ], aes(x = is_alcoholic)) + geom_bar()
  })
}

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

