# This is the main file of the Cocktail Shiny App :)
library(ggplot2)
library(shiny)
# UI
# Define UI for cocktail app ----
ui <- fluidPage(
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
                                         # RadioButtons - distribution of obs.
                                         radioButtons('dist.obs', 'Drinks distributed by:', 
                                                      c('Alcoholic nature' = 'an',
                                                      'Drink type' = 'dt',
                                                      'Glass type' = 'gt',
                                                      'Complexity' = 'cc',
                                                      'Popularity' = 'pp',
                                                      'Price' = 'pp'))
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
                                  # 1st block of page
                                  fluidRow(
                                    # left column 
                                    column(6,
                                      #title of left object
                                      titlePanel("Summary Statistics of the Network by DRINKS"),
                                      #content of left object
                                      p("INTROTEXT", style = "font-family: 'times'; font-si16pt")),
                                    # right column
                                    column(6,
                                      #title of right object
                                      titlePanel("Table with summary statistics"),
                                      #content of right object
                                      img(src = 'A.png', height = 300, width = 300)
                                      )),
                                  # 2nd block of page
                                  fluidRow(
                                    # left column
                                    column(6,
                                      #title of left object
                                      titlePanel("Centrality easures by DRINKS"),
                                      p("Introtext", style = "font-family: 'times'; font-si16pt"),
                                      # RadioButtons - distribution of obs.
                                      radioButtons('dist.obs', 'Drinks distributed by:', 
                                                   c('Degree' = 'dg',
                                                     'Closeness' = 'cl',
                                                     'Betweenness' = 'bt',
                                                     'Eigenvector' = 'ev'))),
                                    # right column
                                    column(6,
                                      titlePanel("Table with centrality measures")
                                      )),
                                  # 3rd block of page
                                  fluidRow(
                                    # left column
                                    column(6,
                                      # title of left column
                                      titlePanel("Network of Drinks"),
                                      fluidRow(
                                        # left sub-column
                                        column(6,
                                        # left sub-column
                                          p("Introtext", style = "font-family: 'times'; font-si16pt"),
                                          p("Minimum weight of edges:", style = "font-family: 'times'; font-si16pt"),
                                          # SliderInput - Network of drinks
                                          sliderInput('weight.edges',
                                                      label = 'Min. weight of edges:', 
                                                      min = 1, max = 20, value = c(1,20), step = 1)
                                        ),
                                        # right sub-column
                                        column(6,
                                               # right sub-column
                                               p("Placeholder Crazy Network Graph")))),
                                    # right object
                                    column(6,
                                      # title of right column
                                      titlePanel("Network of one Drinks"),
                                      fluidRow(
                                        # left sub-column
                                        column(6,
                                               # Introtext
                                               p("Introtext", style = "font-family: 'times'; font-si16pt"),
                                               # Drink Choice
                                               p("Choose drink", style = "font-family: 'times'; font-si16pt"),
                                               p("PLACEHOLDER DROP DOWN")
                                               ),
                                        # right sub-column
                                        column(6,
                                               p("Placeholder Crazy Network Graph")
                                               )
                                        )
                                      )
                                    )
                                  )
                                ),
############################################################# PAGE 4 PROPOSAL #############################################################
                       # 2nd Drop-down item
                       tabPanel("Exploration by ingredients",
                                verticalLayout(
                                  # header of whole page
                                  titlePanel("Exploration by ingredients"),
                                  # 1st block of page
                                  fluidRow(
                                    # left column
                                    column(6,
                                           # title of left object
                                           titlePanel("Summary Statistics of the Network by INGREDIENTS"),
                                           # introtext
                                           p("Introtext", style = "font-family: 'times'; font-si16pt")),
                                    # right column
                                    column(6,
                                           # title of right object
                                           titlePanel("Table with summary statistics"),
                                           # content of right object
                                           img(src = 'A.png', height = 300, width = 300)
                                           )
                                    ),
                                  # 2nd block of page
                                  fluidRow(
                                    # left column
                                    column(6,
                                           #title of left object
                                           titlePanel("Summary Statistics of the Network by INGREDIENTS"),
                                           p("Introtext", style = "font-family: 'times'; font-si16pt"),
                                           # RadioButtons - distribution of obs.
                                           radioButtons('dist.obs', 'Drinks distributed by:', 
                                                        c('Degree' = 'dg',
                                                          'Closeness' = 'cl',
                                                          'Betweenness' = 'bt',
                                                          'Eigenvector' = 'ev'))),
                                    # right column
                                    column(6,
                                             titlePanel("Table with centrality measures")
                                           )),
                                  # 3rd block of page
                                  fluidRow(
                                    # left column
                                    column(6,
                                           # title of left column
                                           titlePanel("Network of Ingredients"),
                                           # 1st sub-block
                                           fluidRow(
                                             column(6,
                                                    # left sub-column
                                                    p("Introtext", style = "font-family: 'times'; font-si16pt"),
                                                    p("Minimum weight of edges:", style = "font-family: 'times'; font-si16pt"),
                                                    # SliderInput - Network of drinks
                                                    sliderInput('weight.edges',
                                                                label = 'Min. weight of edges:', 
                                                                min = 1, max = 20, value = c(1,20), step = 1)
                                             ),
                                             column(6,
                                                    # right sub-column
                                                    p("Placeholder Crazy Network Graph")))),
                                    # right object
                                    column(6,
                                           # title of right column
                                           titlePanel("Network of one ingredients"),
                                           fluidRow(
                                             # left sub-column
                                             column(6,
                                                    # Introtext
                                                    p("Introtext", style = "font-family: 'times'; font-si16pt"),
                                                    # Drink Choice
                                                    p("Choose ingredients", style = "font-family: 'times'; font-si16pt"),
                                                    p("PLACEHOLDER DROP DOWN")
                                             ),
                                             # right sub-column
                                             column(6,
                                                    p("Placeholder Crazy Network Graph")
                                                    )
                                             )
                                           )
                                    )
                                  )
                                ),

############################################################# PAGE 5 PROPOSAL #############################################################
                       # 3rd Drop-down item
tabPanel("Bipartite visualization",
         verticalLayout(
           # header of whole page
           titlePanel("Bipartite visualization"),
           # 1st block of page
           fluidRow(
             # left column
             column(6,
                    # title of left object
                    titlePanel("Bipartite overview"),
                    # introtext
                    p("Introtext", style = "font-family: 'times'; font-si16pt"),
                    p("Please choose your drink filters", style = "font-family: 'times'; font-si16pt"),
                    column(4,
                           p("Alcoholic nature", style = "font-family: 'times'; font-si16pt"),
                           p("Placeholder dropdown", style = "font-family: 'times'; font-si16pt"),
                           p("Max cost per drink", style = "font-family: 'times'; font-si16pt"),
                           # SliderInput - Network of drinks
                           sliderInput('Max.costs',
                                       label = 'Max cost per drink',
                                       min = 1, max = 20, value = c(1,20), step = 1)),
                    column(4,
                           p("Drink type", style = "font-family: 'times'; font-si16pt"),
                           p("Placeholder dropdown", style = "font-family: 'times'; font-si16pt"),
                           p("Preparation complexity", style = "font-family: 'times'; font-si16pt"),
                           # SliderInput - Network of drinks
                           sliderInput('Preparation.complexit',
                                       label = 'Preparation complexity',
                                       min = 1, max = 20, value = c(1,20), step = 1)),
                    column(4,
                           p("Glass type", style = "font-family: 'times'; font-si16pt"),
                           p("Placeholder dropdown", style = "font-family: 'times'; font-si16pt"),
                           p("Popularity", style = "font-family: 'times'; font-si16pt"),
                           # SliderInput - Network of drinks
                           sliderInput('Popularity',
                                       label = 'Popularity',
                                       min = 1, max = 20, value = c(1,20), step = 1)),
                    p("Please choose your ingredient filters", style = "font-family: 'times'; font-si16pt"),
                    column(4,
                           p("Glass type", style = "font-family: 'times'; font-si16pt"),
                           p("Placeholder dropdown", style = "font-family: 'times'; font-si16pt"),
                           p("Popularity", style = "font-family: 'times'; font-si16pt"),
                           # SliderInput - Network of drinks
                           sliderInput('Popularity',
                                       label = 'Popularity',
                                       min = 1, max = 20, value = c(1,20), step = 1)
                           )
             ),
             # right column
             column(6,
                    # title of right object
                    titlePanel("Bipartite overview"),
                    # content of right object
                    img(src = 'A.png', height = 300, width = 300)
             )
           )
         )
)
),
         

############################################################# PAGE 5 PROPOSAL #############################################################

            # 3nd tab
            navbarMenu("tab 3", 
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


#shinyApp()
shinyApp(ui = ui, server = server)

