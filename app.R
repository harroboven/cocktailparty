# This is the main file of the Cocktail Shiny App :)
library(ggplot2)
library(shiny)
# UI
# Define UI for cocktail app ----
ui <- fluidPage(
  # Title and tabpanels with drop-downs
  navbarPage(title = "Shiny Drinks",
             #Start Page
             fluidRow(
               #left column
               column(4,
                      img(src = 'cocktail-glass.png', height = 300, width = 300)
                      ),
               #right column
               column(8,
                      verticalLayout(
                        # header of right column
                        titlePanel("Let's have some drinks!"),
                        # content of right column
                        p("***Relevance Text***", 
                          style = "font-family: 'times'; font-si16pt"
                        )
                      )
                      )
               ),
            # 1st Drop-down tabpanels
             navbarMenu("Data Desription",
############################################################# PAGE 1 PROPOSAL ############################################################# 
                        # 1st Drop-down item
                        tabPanel("Summary of Data", 
                                 verticalLayout(
                                   # header of whole page
                                   titlePanel("Summary of Data"),
                                   # 1st block of page
                                   fluidRow(
                                     # left column
                                     column(6,
                                            verticalLayout(
                                              #Header of left column
                                              titlePanel("Explanation of Summary"),
                                              #content of left column
                                              p("INTROTEXT", 
                                                style = "font-family: 'times'; font-si16pt"
                                                )
                                              )
                                            ),
                                     # right column
                                     column(6,
                                            verticalLayout( 
                                              # Header right column
                                              titlePanel("Header Object 2"),
                                              # content of right column
                                              p("Summary Statistics: Table with number of observations etc.", 
                                                style = "font-family: 'times'; font-si16pt"
                                                )
                                              )
                                            )
                                   ),
                                   # 2nd block of page
                                   fluidRow(
                                     # left column
                                     column(6,
                                            verticalLayout(
                                              #Header of left column
                                              titlePanel("Distribution of Observations"),
                                              #content of left column
                                              p("INTROTEXT", 
                                                style = "font-family: 'times'; font-si16pt"
                                                ),
                                              # RadioButtons - distribution of obs.
                                              radioButtons('dist.obs', 'Drinks distributed by:', 
                                                           c('Alcoholic nature' = 'an',
                                                             'Drink type' = 'dt',
                                                             'Glass type' = 'gt',
                                                             'Complexity' = 'cc',
                                                             'Popularity' = 'pp',
                                                             'Price' = 'pp'
                                                             )
                                              )
                                              )
                                     ),
                                     # right column
                                     column(6, 
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
                        tabPanel("Data by Drinks", 
                                 verticalLayout(
                                   # header of whole page
                                   titlePanel("Data by Drinks"),
                                   # 1st block of page
                                   fluidRow(
                                     # left column
                                     column(6,
                                            verticalLayout(
                                              #Header of left object
                                              titlePanel("Data by Drinks"),
                                              #content of left object
                                              p("INTROTEXT", 
                                                style = "font-family: 'times'; font-si16pt"
                                                )
                                              )
                                            ),
                                     # right column  
                                     column(6,
                                            verticalLayout(
                                              # Header right object
                                              titlePanel("Header1?"),
                                              # content of right object
                                              p("TOP TABLE: Shows top results based on selection", 
                                                style = "font-family: 'times'; font-si16pt"
                                                )
                                              )
                                            )
                                   ),
                                   # 2nd block of page
                                   fluidRow(
                                     # left column
                                     column(6,
                                            verticalLayout(
                                              #Header of left column
                                              titlePanel("Header2?"),
                                              #content of left column
                                              # RadioButtons - drinks ordered
                                              radioButtons('drinks.ordered', 'Drinks ordered by:', 
                                                           c('Complexity' = 'cp',
                                                             'Popularity' = 'pp',
                                                             'Price' = 'pr'
                                                             )
                                                           )
                                              )
                                     ),
                                     #right column
                                     column(6,
                                            verticalLayout(
                                              # Header right column
                                              titlePanel("Header3?"),
                                              # content of right object
                                              # Alcoholic nature histogram
                                              img(src = 'cocktail-glass.png', height = 50, width = 50)
                                              )
                                            )
                                     )
                                   )
                        )
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
                                      # SliderInput - Network of drinks
                                      sliderInput('weight.edges',
                                        label = 'Min. weight of edges:', 
                                        min = 1, max = 20, value = c(1,20), step = 1
                                      )),
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
############################################################# PAGE XX PROPOSAL #############################################################
            # 2nd tabpanel
            tabPanel("Network Analysis", 
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

  # RadioButtons - distr. of obs. - Proposal Page 1
  d <- reactive({
    dist.obst <- switch (input$dist.obs,
      an = action
    )
  })
  # RadioButtons - Proposal Page 2
  g <- reactive({
    drinks.ordered <- switch (input$drinks.ordered,
                              an = action
                              )
    })

#shinyApp()
shinyApp(ui = ui, server = server)

