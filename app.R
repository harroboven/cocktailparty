
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
                                              titlePanel("Summary Statistics"),
                                              # content of right column
                                              # Content1: Summary Table of dt.drinks
                                              tableOutput("summary.statistics")
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
                                         radioButtons(inputId = 'drinks.dist', 
                                                      label = 'Drinks distributed by:', 
                                                      choices = 
                                                        list('Alcoholic nature' = 'an',
                                                             'Drink type' = 'dt',
                                                             'Glass type' = 'gt',
                                                             'Complexity' = 'cp',
                                                             'Commonality' = 'cm', 
                                                             'Price' = 'p'
                                                             )
                                                      )
                                              )
                                     ),
                                     # right column
                                     column(6, 
                                            verticalLayout( 
                                              # Header right object
                                              titlePanel(" "),
                                              # content of right object
                                              # Distribution Barchart
                                              plotOutput(outputId = "drinks.dist.barChart")
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
                                              titlePanel(""),
                                              # content of right column
                                              # Content1: Table with top values from drinks.ordered selection
                                              tableOutput("drinks.ordered.top")
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
                                              p("Drinks filtered by:", 
                                                style = "font-family: 'times'; font-si16pt"
                                              ),
                                              # split into three columns to have input select next to each other
                                              fluidRow(
                                                # left column
                                                column(4, 
                                                       pickerInput("alcoholic.filter", "Alcoholic Nature:", 
                                                                   choices = l.is_alcoholic_values, 
                                                                   selected = NULL, 
                                                                   options = list(`actions-box` = TRUE), 
                                                                   multiple = TRUE
                                                                   )
                                                       ),
                                                # middle column
                                                column(4, 
                                                       pickerInput("category.filter", "Drink Type:", 
                                                                   choices = l.category_values, 
                                                                   selected = l.category_values, 
                                                                   options = list(`actions-box` = TRUE), 
                                                                   multiple = TRUE
                                                                   )
                                                       ),
                                                # right column
                                                column(4, 
                                                       pickerInput("glass.filter", "Glass Type:", 
                                                                   choices = l.glass_type_values, 
                                                                   selected = l.glass_type_values, 
                                                                   options = list(`actions-box` = TRUE), 
                                                                   multiple = TRUE
                                                                   )
                                                       )
                                                ),
                                              # RadioButtons - drinks ordered
                                              radioButtons('drinks.ordered', 'Drinks ordered by:', 
                                                           c('Complexity' = 'cp', 
                                                             'Commonality' = 'cm',
                                                             'Price' = 'p'
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
                                              # Ordered and filtered drinks barchart
                                              plotOutput(outputId = "drinks.ordered.filtering.barChart")
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
                                        sliderInput('weight.edges.drink',
                                                    label = 'Min. weight of edges:', 
                                                    min = 1, max = 15, value = 15, step = 1),
                                        plotOutput(outputId = 'plot.network.of.drinks')
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




############################################################# SERVER #############################################################


server <- function(input, output) {
  
  ################################### PAGE 1 PROPOSAL ##################################
  
  #Oberservation distribution histogram proposal page 1
  output$drinks.dist.barChart <- renderPlot({
    drinks.dist <- switch(input$drinks.dist, 
                          an = dt.drinks.filtered$is_alcoholic, 
                          dt = dt.drinks.filtered$category, 
                          gt = dt.drinks.filtered$glass_type, 
                          cp = dt.drinks.filtered$complexity,  
                          cm = dt.drinks.filtered$commonality, 
                          p = dt.drinks.filtered$price
                          )
    drinks.dist.title <- switch(input$drinks.dist, 
                               an = "Observation Distribution by Alcoholic Nature",  
                               dt = "Observation Distribution by Drink Type", 
                               gt = "Observation Distribution by Glass Type", 
                               cp = "Observation Distribution by Complexity", 
                               cm = "Observation Distribution by Commonality",  
                               p = "Observation Distribution by Price"
                               )
    
    drinks.dist.xlab <- switch(input$drinks.dist, 
                          an = "Alcoholic Nature",  
                          dt = "Drink Type", 
                          gt = "Glass Type", 
                          cp = "Complexity", 
                          cm = "Commonality",  
                          p = "Price"
                          )
    
    ggplot(dt.drinks.filtered, aes(drinks.dist)) +
      geom_bar(color = "steelblue", fill = "steelblue") + 
      chart.theme.1 +
      ggtitle(drinks.dist.title) + 
      xlab(drinks.dist.xlab) + 
      ylab("Frequency")
    })
  
  #Summary statistics table page 1
  output$summary.statistics <- renderTable({
    dt.drinks.summary.name <- dt.drinks.filtered[, .(covariates = "Drinks",
                                                     num = length(unique(name)),
                                                     min = NA, 
                                                     mean = NA, 
                                                     max = NA
                                                     )
                                                 ]
    
    dt.drinks.summary.alcoholic <- dt.drinks.filtered[, .(covariates = "Alcoholic Nature", 
                                                          num = length(unique(is_alcoholic)), 
                                                          min = NA, 
                                                          mean = NA, 
                                                          max = NA
                                                          )
                                                      ]
    
    dt.drinks.summary.category <- dt.drinks.filtered[, .(covariates = "Drink Categories", 
                                                         num = length(unique(category)), 
                                                         min = NA, 
                                                         mean = NA, 
                                                         max = NA
                                                         )
                                                     ]
    
    dt.drinks.summary.glass <- dt.drinks.filtered[, .(covariates = "Glass Types", 
                                                      num = length(unique(glass_type)),
                                                      min = NA, 
                                                      mean = NA, 
                                                      max = NA
                                                      )
                                                  ]
    
    dt.drinks.summary.ingredient <- dt.drinks[, .(covariates = "Drink Ingredients", 
                                                  num = length(unique(ingredient)), 
                                                  min = NA, 
                                                  mean = NA,  
                                                  max = NA
                                                  )
                                              ]
    
    dt.drinks.summary.complexity <- dt.drinks.filtered[, .(covariates = "Complexity of Recipe", 
                                                           num = length(unique(complexity)), 
                                                           min = min(complexity), 
                                                           mean = mean(complexity), 
                                                           max = max(complexity)
                                                           )
                                                       ]
    
    dt.drinks.summary.commonality <- dt.drinks.filtered[, .(covariates = "Commonality", 
                                                            num = length(unique(commonality)), 
                                                            min = NA, 
                                                            mean = NA, 
                                                            max = NA
                                                            )
                                                        ]
    
    dt.drinks.summary.price <- dt.drinks.filtered[, .(covariates = "Price of ingredients", 
                                                      num = length(unique(price)), 
                                                      min = min(na.omit(price)), 
                                                      mean = mean(na.omit(price)), 
                                                      max = max(na.omit(price))
                                                      )
                                                  ]
    
    # combine the different columns into one summary table
    dt.drinks.summary <- rbind(dt.drinks.summary.name, 
                               dt.drinks.summary.alcoholic, 
                               dt.drinks.summary.category, 
                               dt.drinks.summary.glass, 
                               dt.drinks.summary.ingredient, 
                               dt.drinks.summary.complexity, 
                               dt.drinks.summary.commonality, 
                               dt.drinks.summary.price
                               )
    dt.drinks.summary
    })
  
  ################################### PAGE 2 PROPOSAL ##################################
  # Top table proposal page 2 
  output$drinks.ordered.top <- renderTable({
    
    dt.drinks.ordered.filter <- reactive({
      dt.drinks.filtered[is_alcoholic %in% input$alcoholic.filter, ]
      })
    
    dt.drinks.ordered.filter <- reactive({
      dt.drinks.filtered[category %in% input$category.filter, ]
      })
    
    dt.drinks.ordered.filter <- reactive({
      dt.drinks.filtered[glass_type %in% input$glass.filter, ]
      })
    
    alcoholic.filter.top <- reactive({
      switch(input$drinks.ordered,  
             cc = dt.drinks.ordered.filter()$complexity, 
             cm = dt.drinks.ordered.filter()$commonality,
             p = dt.drinks.ordered.filter()$price
             )
      })
    
    dt.drinks.ordered <- dt.drinks.ordered.filter()[
      ifelse(alcoholic.filter.top() == dt.drinks.ordered.filter()$commonality, 
             dt.drinks.ordered.filter()$commonality == 3, 
             order(-rank(alcoholic.filter.top()))
             ), 
      list(dt.drinks.ordered.filter()$name, 
           dt.drinks.ordered.filter()$is_alcoholic, 
           dt.drinks.ordered.filter()$category, 
           dt.drinks.ordered.filter()$glass_type, 
           dt.drinks.ordered.filter()$complexity, 
           dt.drinks.ordered.filter()$commonality,
           dt.drinks.ordered.filter()$price
           )
      ]
    
    dt.drinks.ordered <- head(dt.drinks.ordered, 10)
    dt.drinks.ordered
    })
  
  # Graph proposal page 2
  output$drinks.ordered.filtering.barChart <- renderPlot({
    
    dt.drinks.ordered.filter <- reactive({
      dt.drinks.filtered[is_alcoholic %in% input$alcoholic.filter, ]
      })
    
    dt.drinks.ordered.filter <- reactive({
      dt.drinks.filtered[category %in% input$category.filter, ]
      })
    
    dt.drinks.ordered.filter <- reactive({
      dt.drinks.filtered[glass_type %in% input$glass.filter, ]
      })

    drinks.ordered.filter <- reactive({
      switch(input$drinks.ordered, 
             cp = dt.drinks.ordered.filter()$complexity, 
             cm = dt.drinks.ordered.filter()$commonality,
             p = dt.drinks.ordered.filter()$price
             )
    })
    
    drinks.ordered.title <- reactive({
      switch(input$drinks.dist, 
             an = "Observation Distribution by Alcoholic Nature", 
             dt = "Observation Distribution by Drink Type", 
             gt = "Observation Distribution by Glass Type", 
             cp = "Observation Distribution by Complexity", 
             cm = "Observation Distribution by Commonality", 
             p = "Observation Distribution by Price"
             )
      })
    
    drinks.ordered.xlab <- reactive({
      switch(input$drinks.dist, 
             an = "Alcoholic Nature",  
             dt = "Drink Type", 
             gt = "Glass Type", 
             cp = "Complexity", 
             cm = "Commonality",  
             p = "Price"
             )
    })
    
    ggplot(dt.drinks.ordered.filter(), aes(drinks.ordered.filter())) +
      geom_bar(color = "steelblue", fill = "steelblue") + 
      chart.theme.1 +
      ggtitle(drinks.ordered.title()) + 
      xlab(drinks.ordered.xlab()) + 
      ylab("Frequency")
  })
  
  
  ################################### PAGE 3 PROPOSAL ##################################
  # Network of ingredients
  # Network of drinks 
  # create constant objects - always stay the same
  all.drinks <- dt.drinks[, .(name = unique(name), type = TRUE)]
  all.ingredients <- dt.drinks[, .(name = unique(ingredient), type = FALSE)]
  all.vertices <- rbind(all.drinks, all.ingredients)
  #all.vertices <- all.vertices[!duplicated(all.vertices$id)]
  
  
  g.drinks.ingredients <- graph.data.frame(dt.drinks[, .(name, ingredient)],
                                           directed = FALSE,
                                           vertices = all.vertices)
  g.drinks.bp <- bipartite.projection(g.drinks.ingredients)$proj2
  g.ingredients.bp <- bipartite.projection(g.drinks.ingredients)$proj1
  
  
  
  # filter drinks by weight X
  g.drinks.bp <- reactive({
    delete.edges(g.drinks.bp, E(g.drinks.bp)[weight < input$weight.edges.drink])
  })
  # plot a graph for drinks with weight X
  output$plot.network.of.drinks <- renderPlot({
    plot.igraph(g.drinks.bp, vertex.label = NA, vertex.size = 0,3)
  })
  
  # filter ingredients by weight X 
  #g.ingredients <- delete.edges(g.ingredients, E(g.ingredients)[weight > X ])
  # plot a graph for ingredients with weight X
  # plot(g.ingredients, vertex.label = NA, vertex.size = 0,3)
  
  # create a subgraph with one selected drink X
  #g.one.drink <- induced.subgraph(g.drinks.ingredients, V(g.drinks.ingredients)$name == 'X')
  
  # create a subgraph with one selected ingredient X
  #g.one.ingredient <- induced.subgraph(g.drinks.ingredients, V(g.drinks.ingredients$name == 'X'))
  
  }

#shinyApp()
shinyApp(ui = ui, server = server)
