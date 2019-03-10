############################################################# JAVASCRIPT #############################################################

# For dropdown menu
actionLink <- function(inputId, ...) {
  tags$a(href='javascript:void',
         id=inputId,
         class='action-button',
         ...)
  }

############################################################# UI #############################################################

ui <- fluidPage(
  #Overall theme of our Shiny App
  theme = shinytheme("flatly"), 
  
  ######################################### Welcome Page ########################################
  
  # Title and tabpanels with drop-downs
  navbarPage(title = "Shiny Drinks", 
             #Start Page
             fluidRow(
               #left column
               column(4,
                      wellPanel(
                        img(src = 'cocktail-glass.png', height = 300, width = 300)
                        )
                      ),
               #right column
               column(8, 
                      wellPanel(
                        verticalLayout(
                          # header of right column
                          titlePanel("Let's have some drinks!"),
                          # content of right column
                          p("***Relevance Text***", 
                            style = "font-family: 'times'; font-si16pt"
                            )
                          )
                        )
                      )
               ),
  
  ######################################### Part 1 ########################################
  
  # 1st Drop-down tabpanels
  navbarMenu("Data Desription", 
             
             ######################################### Page 1 Proposal ########################################
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
                                 wellPanel(
                                   verticalLayout( 
                                     # Header right column
                                     titlePanel("Summary Statistics"),
                                     # content of right column
                                     # Content1: Summary Table of dt.drinks
                                     tableOutput("data.summary")
                                     )
                                   )
                                 )
                          ),
                        # 2nd block of page
                        fluidRow(
                          # left column
                          column(6,
                                 wellPanel(
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
                                                         'Ingredient Price' = 'ip',
                                                         'Ingredients Cost per Drink' = 'ic'
                                                         )
                                                  )
                                     )
                                   )
                                 ),
                          # right column
                          column(6, 
                                 wellPanel(
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
                        )
                      ), 
             
             ######################################### Page 2 Proposal ########################################
             
             # 2nd Drop-down item
             tabPanel("Drinks Explorer", 
                                 titlePanel("Drinks explorer"),
                                 fluidRow(
                                   column(3,
                                          verticalLayout(
                                            wellPanel(
                                              h4("Filter"),
                                              sliderInput("drink.explorer.complexity.filter", "Degree of Recipe Complexity",
                                                          min = min(dt.drinks.filtered$complexity), 
                                                          max = max(dt.drinks.filtered$complexity), 
                                                          value = c(median(dt.drinks.filtered$complexity), 
                                                                    mean(dt.drinks.filtered$complexity)
                                                                    ), 
                                                          sep = ""
                                                          ),
                                              sliderInput("drink.explorer.commonality.filter", "Degree of Commonality",
                                                          min = 1,
                                                          max = 3,
                                                          value = c(1, 3),
                                                          sep = ""
                                                          ),
                                              sliderInput("drink.explorer.cost.filter", "Ingredients Cost per Drink",
                                                          min = min(dt.drinks.filtered$adj_ingredients_cost),
                                                          max = max(dt.drinks.filtered$adj_ingredients_cost),
                                                          value = c(median(dt.drinks.filtered$adj_ingredients_cost), 
                                                                    mean(dt.drinks.filtered$adj_ingredients_cost)
                                                                    ),
                                                          sep = ""
                                                          ),
                                              selectInput("drink.explorer.alcoholic.filter", "Alcoholic Nature",
                                                          l.is_alcoholic_values,
                                                          selected = "All"
                                                          ),
                                              selectInput("drink.explorer.category.filter", "Type of Drink",
                                                          l.category_values,
                                                          selected = "All"
                                                          ),
                                              selectInput("drink.explorer.glass.filter", "Glass Type",
                                                          l.glass_type_values,
                                                          selected = "All"
                                                          )
                                              ),
                                            wellPanel(
                                              selectInput("drink.explorer.xvar", 
                                                          "X-axis variable", 
                                                          v.drink.explorer.axis.vars, 
                                                          selected = "complexity"
                                                          ),
                                              selectInput("drink.explorer.yvar", 
                                                          "Y-axis variable", 
                                                          v.drink.explorer.axis.vars, 
                                                          selected = "complexity"
                                                          )
                                              )
                                            )
                                          ),
                                   column(9,
                                          verticalLayout(
                                            ggvisOutput("drink_explorer"), 
                                            wellPanel(
                                              span("Number of drinks selected:", 
                                                   textOutput("drink.explorer.n_drinks")
                                                   )
                                              )
                                            )
                                          )
                                   )
                      )
             ),
  
  ######################################### Part 2 ########################################
  
  # 2nd tabpanel
  navbarMenu("Networking Exploration", 
             
             ######################################### Page 3 Proposal ########################################
             
             # 1st Drop-down item
             tabPanel("Exploration by Drinks", 
                      verticalLayout(
                        # header of whole page
                        titlePanel("Network Exploration by Drinks"),
                        # 1st block of page
                        fluidRow(
                          # left column 
                          column(6,
                                 wellPanel(
                                   #title of left object
                                   titlePanel("Summary Statistics of the Network by Drinks"),
                                   #content of left object
                                   tableOutput("drinks.network.summary")
                                 )
                          ),
                          # right column
                          column(6,
                                 wellPanel(
                                   #title of right object
                                   titlePanel("Centrality Measures by Drinks"),
                                   fluidRow(
                                     column(4, 
                                            # RadioButtons - distribution of obs.
                                            radioButtons('drinks.centrality.table', 'Choose Centrality Measure:', 
                                                         c('Degree' = 'dg',
                                                           'Closeness' = 'cl',
                                                           'Betweenness' = 'bt',
                                                           'Eigenvector' = 'ev'
                                                         )
                                            )
                                     ),
                                     column(8, 
                                            tableOutput("drinks.centrality.table")
                                     )
                                   )
                                 )
                          )
                        ),
                        # 2nd block of page
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
                                                      min = 1, max = 15, value = 15, step = 1)
                                          ),
                                   # right sub-column
                                   column(6,
                                          # right sub-column
                                          plotOutput(outputId = 'plot.network.of.drinks')))),
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
                                          p("PLACEHOLDER DROP DOWN"),
                                          selectInput('network.of.one.drink',
                                                      label = 'Network of one drink',
                                                      selected = NA,
                                                      choices = dt.drinks$name
                                          ),
                                          plotOutput(outputId = 'plot.network.of.one.drink')
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
             
             ######################################### Page 4 Proposal ########################################
             
             # 2nd Drop-down item
             tabPanel("Exploration by ingredients", 
                      verticalLayout(
                        # header of whole page
                        titlePanel("Exploration by ingredients"),
                        # 1st block of page
                        fluidRow(
                          # left column
                          column(6,
                                 wellPanel(
                                   # title of left object
                                   titlePanel("Table with summary statistics"),
                                   # content of left object
                                   tableOutput("ingredients.network.summary")
                                   )
                                 ),
                          # right column
                          column(6,
                                 wellPanel(
                                   #title of right object
                                   titlePanel("Centrality Measures by Ingredients"),
                                   fluidRow(
                                     column(4, 
                                            # RadioButtons - distribution of obs.
                                            radioButtons('ingredients.centrality.table', 'Choose Centrality Measure:', 
                                                         c('Degree' = 'dg',
                                                           'Closeness' = 'cl',
                                                           'Betweenness' = 'bt',
                                                           'Eigenvector' = 'ev'
                                                           )
                                                         )
                                            ),
                                     column(8, 
                                            tableOutput("ingredients.centrality.table")
                                            )
                                     )
                                   )
                                 )
                          ),
                        # 2nd block of page
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
                                          sliderInput('weight.edges.ingredient',
                                                      label = 'Min. weight of edges:', 
                                                      min = 1, max = 15, value = 15, step = 1),
                                          sliderInput('degree.vertices.ingredient',
                                                      label = 'Max. degree of vertices: ',
                                                      min = 1, max = 3, value = 3, step = 1)
                                          ),
                                   column(6,
                                          # right sub-column
                                          plotOutput(outputId = 'plot.network.of.ingredients')))),
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
                                          selectInput('network.of.one.ingredient',
                                                      label = 'Network of one ingredient',
                                                      selected = NA,
                                                      choices = dt.drinks$ingredient)
                                          ),
                                   # right sub-column
                                   column(6,
                                          plotOutput(outputId = 'plot.network.of.one.ingredient')
                                          )
                                   )
                                 )
                          )
                        )
                      ),
             
             ######################################### Page 5 Proposal ########################################
             
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
                                        sliderInput('max.cost.ingredient',
                                                    label = 'Max cost per ingredient',
                                                    min = 1, max = 50, value = 50, step = 1),
                                        sliderInput('max.cost.drink',
                                                    label = 'Max cost for at least one drink',
                                                    min = 1, max = 50, value = 100, step = 1)),
                                 column(4,
                                        p("Drink type", style = "font-family: 'times'; font-si16pt"),
                                        p("Placeholder dropdown", style = "font-family: 'times'; font-si16pt"),
                                        p("Preparation complexity", style = "font-family: 'times'; font-si16pt"),
                                        # SliderInput - Network of drinks
                                        sliderInput('preparation.complexity',
                                                    label = 'Preparation complexity',
                                                    min = 1, max = 50, value = 50, step = 1)),
                                 column(4,
                                        p("Glass type", style = "font-family: 'times'; font-si16pt"),
                                        p("Placeholder dropdown", style = "font-family: 'times'; font-si16pt"),
                                        p("Popularity", style = "font-family: 'times'; font-si16pt"),
                                        # SliderInput - Network of drinks
                                        sliderInput('popularity',
                                                    label = 'Popularity',
                                                    min = 1, max = 20, value = 80, step = 2)),
                                 p("Please choose your ingredient filters", style = "font-family: 'times'; font-si16pt"),
                                 column(4,
                                        p("Glass type", style = "font-family: 'times'; font-si16pt"),
                                        p("Placeholder dropdown", style = "font-family: 'times'; font-si16pt"),
                                        p("Popularity", style = "font-family: 'times'; font-si16pt"),
                                        # SliderInput - Network of drinks
                                        sliderInput('Popularity',
                                                    label = 'Popularity',
                                                    min = 1, max = 80, value = 80, step = 2),
                                        selectInput('alcoholic.nature',
                                                    label = 'Alcoholic nature',
                                                    choices = dt.drinks$is_galcoholic),
                                        selectInput('drink.type',
                                                    label = 'Drink type',
                                                    choices = dt.drinks$category),
                                        selectInput('glass.type',
                                                    label = 'Glass type',
                                                    choices = dt.drinks$glass_type))
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
  
  ######################################### Part 3 ########################################
  
  navbarMenu("Network Analysis", 
             
  ######################################### Page 6 Proposal ########################################
          
  # 1st Drop-down item
  tabPanel("Exploring Centrality Characteristics by Drinks",
           verticalLayout(
             titlePanel("Centrality Explorer by Drinks"),
             fluidRow(
               column(4,
                      wellPanel(
                        h4("Filter"),
                        sliderInput("analysis.drinks.complexity.filter", "Degree of Recipe Complexity",
                                    min = min(dt.drinks.analysis$complexity), 
                                    max = max(dt.drinks.analysis$complexity), 
                                    value = c(median(dt.drinks.analysis$complexity), 
                                              mean(dt.drinks.analysis$complexity)
                                    ), 
                                    sep = ""
                                    ),
                        sliderInput("analysis.drinks.commonality.filter", "Degree of Commonality",
                                    min = 1,
                                    max = 3,
                                    value = c(1, 3),
                                    sep = ""
                                    ),
                        sliderInput("analysis.drinks.cost.filter", "Ingredients Cost per Drink",
                                    min = min(dt.drinks.analysis$adj_ingredients_cost), 
                                    max = max(dt.drinks.analysis$adj_ingredients_cost), 
                                    value = c(median(dt.drinks.analysis$adj_ingredients_cost), mean(dt.drinks.analysis$adj_ingredients_cost)), 
                                    sep = ""
                                    ),
                        selectInput("analysis.drinks.alcoholic.filter", "Alcoholic Nature",
                                    l.is_alcoholic_values,
                                    selected = "All"
                                    ),
                        selectInput("analysis.drinks.category.filter", "Type of Drink",
                                    l.category_values,
                                    selected = "All"
                                    ),
                        selectInput("analysis.drinks.glass.filter", "Glass Type",
                                    l.glass_type_values,
                                    selected = "All"
                                    )
                        ),
                      wellPanel(
                        selectInput("analysis.drinks.xvar", 
                                    "X-axis variable", 
                                    v.analysis.drinks.axis.vars, 
                                    selected = "adj_ingredients_cost"
                                    ),
                        selectInput("analysis.drinks.yvar", 
                                    "Y-axis variable", 
                                    v.analysis.drinks.axis.vars, 
                                    selected = "drink_degree"
                                    )
                        )
                      ),
               column(8,
                      verticalLayout(
                        wellPanel(
                          ggvisOutput("drinks_analysis")
                        ),
                        wellPanel(
                          span("Number of Drinks selected:", 
                               textOutput("drinks.analysis.n_drinks")
                          )
                        )
                      )
               )
             )
             )
           ),
  
  ######################################### Page 7 Proposal ########################################
  
  tabPanel("Exploring Centrality Characteristics by Ingredients",
           verticalLayout(
             titlePanel("Centrality Explorer by Ingredients"),
             fluidRow(
               column(4,
                      wellPanel(
                        h4("Filter"),
                        sliderInput("analysis.ingredient.price.filter", "Ingredient Price",
                                    min = min(dt.ingredients.analysis$adj_ingredient_price), 
                                    max = max(dt.ingredients.analysis$adj_ingredient_price), 
                                    value = c(median(dt.ingredients.analysis$adj_ingredient_price), mean(dt.ingredients.analysis$adj_ingredient_price)), 
                                    sep = ""
                                    )
                        ),
                      wellPanel(
                        selectInput("analysis.ingredients.xvar", 
                                    "X-axis variable", 
                                    v.analysis.ingredients.axis.vars, 
                                    selected = "adj_ingredient_price"
                        ),
                        selectInput("analysis.ingredients.yvar", 
                                    "Y-axis variable", 
                                    v.analysis.ingredients.axis.vars, 
                                    selected = "ingredient_degree"
                        )
                      )
                      ),
               column(8,
                      verticalLayout(
                        wellPanel(
                          ggvisOutput("ingredient_analysis")
                        ),
                        wellPanel(
                          span("Number of Ingredients selected:", 
                               textOutput("ingredient.analysis.n_ingredients")
                               )
                          )
                        )
                      )
               )
               )
             ),
  
  ######################################### Page 8 Proposal ########################################
  
  tabPanel("Cocktail Party Planner",
           verticalLayout(
             titlePanel("TITLE PLACEHOLDER"),
             fluidRow(
               wellPanel(
                 h4("Available Stock"),
                 selectInput("ingredients.available",
                             "Available Stock",
                             l.all.ingredients,
                             selected = "All",
                             multiple = TRUE
                             ),
                 fluidRow(
                 p("Please specify the amount of the ingredients in ml:")),
                 fluidRow(
                   column(6,
                          uiOutput("ingredients.in.stock.table"))
                   # column(6,
                   #        uiOutput("amount.ingredients.in.stock.table"))
                 
                   #          )),
                   # column(4,
                   #        fluidRow(
                   #          column(6,
                   #                 p(input$ingredients.available[2])),
                   #          column(6,
                   #                 textInput("ingredient.2", input$ingredients.available[2], placeholder = "amount in ml"))
                   #          )),
                   # column(4,
                   #        fluidRow(
                   #          column(6,
                   #                 p(input$ingredients.available[3])),
                   #          column(6,
                   #                 textInput("ingredient.3", input$ingredients.available[3], placeholder = "amount in ml"))
                   #        ))
                 )
               )
             )
             )
             )
           )
  )
)



############################################################# SERVER #############################################################

server <- function(input, output, session) {
  
  ################################### PAGE 1 PROPOSAL ##################################
  
   ############ Oberservation distribution chart ############
  output$drinks.dist.barChart <- renderPlot({
    
    # needed to adjust for NA values in ingredient costs
    dt.drinks.filtered.costs <- dt.drinks.filtered[!is.na(ingredients_cost), ]
    
    drinks.dist <- switch(input$drinks.dist, 
                          an = dt.drinks.filtered$is_alcoholic, 
                          dt = dt.drinks.filtered$category, 
                          gt = dt.drinks.filtered$glass_type, 
                          cp = dt.drinks.filtered$complexity,  
                          cm = dt.drinks.filtered$commonality, 
                          ip = dt.drinks$ingredient_price,
                          ic = dt.drinks.filtered$adj_ingredients_cost
                          )
    
    drinks.dist.title <- switch(input$drinks.dist, 
                               an = "Observation Distribution by Alcoholic Nature",  
                               dt = "Observation Distribution by Drink Type", 
                               gt = "Observation Distribution by Glass Type", 
                               cp = "Observation Distribution by Complexity", 
                               cm = "Observation Distribution by Commonality",  
                               ip = "Observation Distribution by Ingredient Price",
                               ic = "Observation Distribution by Ingredients Cost per Drink"
                               )
    
    drinks.dist.xlab <- switch(input$drinks.dist, 
                          an = "Alcoholic Nature",  
                          dt = "Drink Type", 
                          gt = "Glass Type", 
                          cp = "Complexity", 
                          cm = "Commonality",  
                          ip = "Ingredient Price",
                          ic = "Ingredients Cost per Drink"
                          )
    data.drinks.dist <- switch(input$drinks.dist, 
                          an = dt.drinks.filtered, 
                          dt = dt.drinks.filtered, 
                          gt = dt.drinks.filtered, 
                          cp = dt.drinks.filtered,  
                          cm = dt.drinks.filtered, 
                          ip = dt.drinks,
                          ic = dt.drinks.filtered
                          )
    
    ggplot(data.drinks.dist, aes(drinks.dist)) +
      geom_bar(color = "steelblue", fill = "steelblue") + 
      chart.theme.1 +
      ggtitle(drinks.dist.title) + 
      xlab(drinks.dist.xlab) + 
      ylab("Frequency")
    })
  
   ############ Summary statistics table ############
  output$data.summary <- renderTable({

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
    
    dt.drinks.summary.ingredient.price <- dt.drinks[, .(covariates = "Ingredient Price", 
                                                        num = length(unique(ingredient_price)), 
                                                        min = min(na.omit(ingredient_price)), 
                                                        mean = mean(na.omit(ingredient_price)), 
                                                        max = max(na.omit(ingredient_price))
                                                        )
                                                    ]
    
    dt.drinks.summary.ingredients.cost <- dt.drinks.filtered[, .(covariates = "Ingredients Cost per Drink", 
                                                                 num = length(unique(adj_ingredients_cost)), 
                                                                 min = min(adj_ingredients_cost), 
                                                                 mean = mean(adj_ingredients_cost), 
                                                                 max = max(adj_ingredients_cost)
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
                               dt.drinks.summary.ingredient.price,
                               dt.drinks.summary.ingredients.cost
                               )
    dt.drinks.summary
    })
  
  
  ################################### PAGE 2 PROPOSAL ##################################

  # Filter the drinks, returning a data frame
  dt.drink.explorer <- reactive({
    # Due to dplyr issue #318, we need temp variables for input values
    min.complexity <- input$drink.explorer.complexity.filter[1]
    max.complexity <- input$drink.explorer.complexity.filter[2]
    min.commonality <- input$drink.explorer.commonality.filter[1]
    max.commonality <- input$drink.explorer.commonality.filter[2]
    min.ingredients.cost <- input$drink.explorer.cost.filter[1]
    max.ingredients.cost <- input$drink.explorer.cost.filter[2]
    
    # Apply filters
    m <- dt.drinks.filtered %>%
      filter(
        complexity >= min.complexity, 
        complexity <= max.complexity,
        commonality >= min.commonality,
        commonality <= max.commonality,
        adj_ingredients_cost >= min.ingredients.cost,
        adj_ingredients_cost <= max.ingredients.cost
        )
      
    # Filter by alcoholic nature
    if (input$drink.explorer.alcoholic.filter != "All") {
      m <- m %>% filter(is_alcoholic %in% input$drink.explorer.alcoholic.filter)
      }
    # Filter by drink type
    if (input$drink.explorer.category.filter != "All") {
      m <- m %>% filter(category %in% input$drink.explorer.category.filter)
      }
    # Filter by glass type
    if (input$drink.explorer.glass.filter != "All") {
      m <- m %>% filter(glass_type %in% input$drink.explorer.glass.filter)
      }
      
    m <- as.data.frame(m)
      
    m
    })
    
  # Generating tooltip text
  drink.explorer.tooltip <- function(x) {
    if (is.null(x)) return(NULL)
    if (is.null(x$id)) return(NULL)
      
    # Pick out the drink with this ID
    dt.drinks.filtered <- isolate(dt.drink.explorer())
    drink <- dt.drinks.filtered[dt.drinks.filtered$id == x$id, ]
      
    paste0("<b>", drink$name, "</b><br>",
          "Alcoholic Nature: ", drink$is_alcoholic, "<br>",
          "Drink Type: ", drink$category
          )
    }
    
  # A reactive expression with the ggvis plot
  vis.drink.explorer <- reactive({
    #Lables for axes
    xvar_name <- names(v.drink.explorer.axis.vars)[v.drink.explorer.axis.vars == input$drink.explorer.xvar]
    yvar_name <- names(v.drink.explorer.axis.vars)[v.drink.explorer.axis.vars == input$drink.explorer.yvar]

    xvar <- prop("x", as.symbol(input$drink.explorer.xvar))
    yvar <- prop("y", as.symbol(input$drink.explorer.yvar))
      
    dt.drink.explorer %>%
      ggvis(x = xvar, y = yvar) %>%
      layer_points(size := 50, size.hover := 200,
                   fillOpacity := 0.2, fillOpacity.hover := 0.5, 
                   key := ~id) %>%
      add_tooltip(drink.explorer.tooltip, "hover") %>%
      add_axis("x", title = xvar_name) %>%
      add_axis("y", title = yvar_name) %>%
      set_options(width = 636, height = 636)
    })
  
  vis.drink.explorer %>% bind_shiny("drink_explorer")
    
  output$drink.explorer.n_drinks <- renderText({
    nrow(dt.drink.explorer())
    })
  
  
  ################################### PAGE 3 PROPOSAL ##################################
  
   ############ Summary Table of drinks network ############
  output$drinks.network.summary <- renderTable({  
    
    l.num_nodes <- list("Number of Nodes", length(V(g.drinks.bp)))
    l.num_edges <- list("Number of Edges", length(E(g.drinks.bp)))
    l.mean_degree <- list("Average Degree", mean(dt.drinks.degree$drink_degree))
    l.clust_coeff <- list("Clustering Coefficient", transitivity(g.drinks.bp))
    l.mean_betweenness <- list("Average Betweenness", mean(dt.drinks.betweenness$drink_betweenness))
    l.mean_path_length <- list("Average Path Length", mean(distances(g.drinks.bp)))
    l.diameter <- list("Diameter", diameter(g.drinks.bp))
    
    dt.drinks.network.summary <- rbind(l.num_nodes,
                                       l.num_edges,
                                       l.mean_degree, 
                                       l.clust_coeff, 
                                       l.mean_betweenness, 
                                       l.mean_path_length, 
                                       l.diameter
                                       )
    dt.drinks.network.summary <- as.data.table(dt.drinks.network.summary)
    setnames(dt.drinks.network.summary, old = c("V1", "V2"), new = c("Name", "Value"))
    dt.drinks.network.summary
    })  
    
   ############ Centrality Measures Table of drink network ############
  output$drinks.centrality.table <- renderTable({ 
    
    drinks.centrality <- switch(input$drinks.centrality.table, 
                                dg = dt.drinks.degree$drink_degree, 
                                cl = dt.drinks.closeness$drink_closeness, 
                                bt = dt.drinks.betweenness$drink_betweenness, 
                                ev = dt.drinks.eigenvector$drink_eigenvector
                                )
      
    l.centrality.min <- list("Min ", min(drinks.centrality))
    l.centrality.median <- list("Median ", median(drinks.centrality))
    l.centrality.sd <- list("SD ", sd(drinks.centrality))
    l.centrality.max <- list("Max ", max(drinks.centrality))
    
    dt.drinks.centrality  <- rbind(l.centrality.min, 
                                   l.centrality.median, 
                                   l.centrality.sd, 
                                   l.centrality.max
                                   )
                                   
    dt.drinks.centrality <- as.data.table(dt.drinks.centrality)
    setnames(dt.drinks.centrality, old = c("V1", "V2"), new = c("Statistics", "Value"))
    dt.drinks.centrality
    })  
  
   ################## drinks network graph by weight ######################
  
  # plot a graph for drinks with weight X
  output$plot.network.of.drinks <- renderPlot({
    g.drinks.bp <- delete.edges(g.drinks.bp, 
                                E(g.drinks.bp)[weight < input$weight.edges.drink])
    
    plot.igraph(g.drinks.bp, vertex.label = NA, vertex.size = 2, edge.color = 'tomato',
                layout = layout_on_sphere, edge.arrow.size = 1)
  })
  
  ################## graph for a single drink ######################
  
  output$plot.network.of.one.drink <- renderPlot({
    
    
    neigh.nodes.drinks <- neighborhood(g.drinks.bp, order = 1, nodes = V(g.drinks.bp)$name == input$network.of.one.drink)[[1]]
    V(g.drinks.bp)$color <- 'green'
    V(g.drinks.bp)[name == input$network.of.one.drink]$color <- 'tomato'
    V(g.drinks.bp)[name == input$network.of.one.drink]$label <- paste0(input$network.of.one.drink)
    plot(induced.subgraph(g.drinks.bp, neigh.nodes.drinks),
         layout = layout_with_graphopt) 
    
  })
  
  ################################### PAGE 4 PROPOSAL ##################################
  
   ############ Summary Table of ingredient network ############
  output$ingredients.network.summary <- renderTable({
    
    l.num_nodes <- list("Number of Nodes", length(V(g.ingredients.bp)))
    l.num_edges <- list("Number of Edges", length(E(g.ingredients.bp)))
    l.mean_degree <- list("Average Degree", mean(dt.ingredients.degree$ingredient_degree))
    l.clust_coeff <- list("Clustering Coefficient", transitivity(g.ingredients.bp))
    l.mean_betweenness <- list("Average Betweenness", mean(dt.ingredients.betweenness$ingredient_betweenness))
    l.mean_path_length <- list("Average Path Length", mean(distances(g.ingredients.bp)))
    l.diameter <- list("Diameter", diameter(g.ingredients.bp))
    
    dt.ingredients.network.summary <- rbind(l.num_nodes,
                                       l.num_edges,
                                       l.mean_degree, 
                                       l.clust_coeff, 
                                       l.mean_betweenness, 
                                       l.mean_path_length, 
                                       l.diameter
                                       )
    
    dt.ingredients.network.summary <- as.data.table(dt.ingredients.network.summary)
    setnames(dt.ingredients.network.summary, old = c("V1", "V2"), new = c("Name", "Value"))
    dt.ingredients.network.summary
    })
  
   ############ Centrality Measures Table of ingredient network ############
  output$ingredients.centrality.table <- renderTable({
    
    ingredients.centrality <- switch(input$ingredients.centrality.table, 
                                     dg = dt.ingredients.degree$ingredient_degree, 
                                     cl = dt.ingredients.closeness$ingredient_closeness, 
                                     bt = dt.ingredients.betweenness$ingredient_betweenness, 
                                     ev = dt.ingredients.eigenvector$ingredient_eigenvector
                                     )
    
    l.centrality.min <- list("Min ", min(ingredients.centrality))
    l.centrality.median <- list("Median ", median(ingredients.centrality))
    l.centrality.sd <- list("SD ", sd(ingredients.centrality))
    l.centrality.max <- list("Max ", max(ingredients.centrality))
      
    dt.ingredients.centrality  <- rbind(l.centrality.min, 
                                        l.centrality.median, 
                                        l.centrality.sd, 
                                        l.centrality.max
                                        )
      
    dt.ingredients.centrality <- as.data.table(dt.ingredients.centrality)
    setnames(dt.ingredients.centrality, old = c("V1", "V2"), new = c("Statistics", "Value"))
    dt.ingredients.centrality
    })  
  
   ################## ingredients network graph by weight ######################
    
    output$plot.network.of.ingredients <- renderPlot({
      g.ingredients.bp <- delete.edges(g.ingredients.bp,
                                       E(g.ingredients.bp)[weight < input$weight.edges.ingredient])
      g.ingredients.bp <- delete.vertices(g.ingredients.bp, 
                                          V(g.ingredients.bp)[degree < input$degree.vertices.ingredient])
      plot.igraph(simplify(g.ingredients.bp), vertex.label = NA, vertex.size = deg.ingredients/2, 
                  edge.color = 'tomato', layout = layout_on_sphere,
                  edge.arrow.size = 1)
    })
    


  
   ################## graph for a single ingredient ######################
  
    output$plot.network.of.one.ingredient <- renderPlot({
      neigh.nodes.ingredients <- neighborhood(g.ingredients.bp, order = 1, nodes = V(g.ingredients.bp)$name == input$network.of.one.ingredient)[[1]]
      V(g.ingredients.bp)$color <- 'grey'
      V(g.ingredients.bp)[name == input$network.of.one.ingredient]$label <- paste0(input$network.of.one.ingredient)  
      E(g.ingredients.bp)$ltly <- 'dotted'
      V(g.ingredients.bp)[name == input$network.of.one.ingredient]$color <- 'tomato'
      plot(induced.subgraph(g.ingredients.bp, neigh.nodes.ingredients),
           layout = layout_with_graphopt) 
      # visIgraph(g.one.ingredient)
    })

  
  ################################### PAGE 5 PROPOSAL ##################################
  
   ################## bipartite visualisation ######################

  output$bipartite.drinks.ingredients <- renderPlot({
    all.drinks.alco <- dt.drinks[is_alcoholic == input$alcoholic.nature][,
                                                                         .(name = unique(name),
                                                                           type = TRUE)
                                                                         ]
    all.drinks.type <- dt.drinks[category == input$drink.type][,
                                                               .(name = unique(name),
                                                                 type = TRUE)
                                                               ]
    all.drinks.glass <- dt.drinks[category == input$glass.type][,
                                                                .(name = unique(name),
                                                                  type = TRUE)
                                                                ]
    all.vertices.filtered <- rbind(all.drinks.alco, all.drinks.type, all.drinks.glass, all.ingredients)
    V(g.drinks.ingredients)$color <- c("steel blue", "orange")[V(g.drinks.ingredients)$type+1]
    V(g.drinks.ingredients)$shape <- c("square", "circle")[V(g.drinks.ingredients)$type+1]
    g.drinks.ingredients.custom <- graph()
    plot(g.drinks.ingredients, vertex.label=NA, vertex.size=7, layout=layout_as_bipartite) 
    
  })
  
  
  ################################### PAGE 6 PROPOSAL ##################################
  
  # Filter the drinks, returning a data frame
  dt.analysis.drinks.filter <- reactive({
    # Due to dplyr issue #318, we need temp variables for input values
    min.complexity <- input$analysis.drinks.complexity.filter[1]
    max.complexity <- input$analysis.drinks.complexity.filter[2]
    min.commonality <- input$analysis.drinks.commonality.filter[1]
    max.commonality <- input$analysis.drinks.commonality.filter[2]
    min.ingredient.cost <- input$analysis.drinks.cost.filter[1]
    max.ingredient.cost <- input$analysis.drinks.cost.filter[2] 
    
    # Apply filters
    m <- dt.drinks.analysis %>%
      filter(
        complexity >= min.complexity, 
        complexity <= max.complexity,
        commonality >= min.commonality,
        commonality <= max.commonality,
        adj_ingredients_cost >= min.ingredient.cost, 
        adj_ingredients_cost <= max.ingredient.cost
      )
    
    # Filter by alcoholic nature
    if (input$analysis.drinks.alcoholic.filter != "All") {
      m <- m %>% filter(is_alcoholic %in% input$analysis.drinks.alcoholic.filter)
    }
    # Filter by drink type
    if (input$analysis.drinks.category.filter != "All") {
      m <- m %>% filter(category %in% input$analysis.drinks.category.filter)
    }
    # Filter by glass type
    if (input$analysis.drinks.glass.filter != "All") {
      m <- m %>% filter(glass_type %in% input$analysis.drinks.glass.filter)
    }
    
    m <- as.data.frame(m)
    
    m
  })
  
  # Generating tooltip text
  analysis.drinks_tooltip <- function(x) {
    if (is.null(x)) return(NULL)
    if (is.null(x$id)) return(NULL)
    
    #Pick out the drink with this ID
    dt.drinks.analysis <- isolate(dt.analysis.drinks.filter())
    analysis.drink <- dt.drinks.analysis[dt.drinks.analysis$id == x$id, ]
    
    paste0("<b>", analysis.drink$name, "</b><br>",
           "Ingredients Cost: ", round(analysis.drink$adj_ingredients_cost, 2), "<br>",
           "Degree: ", round(analysis.drink$name_degree, 2), "<br>",
           "Closeness: ", round(analysis.drink$name_closeness, 2), "<br>",
           "Betweenness: ", round(analysis.drink$name_betweenness, 2), "<br>",
           "Eigenvector: ", round(analysis.drink$name_eigenvector, 2), "<br>"
    )
  }
  
  # A reactive expression with the ggvis plot
  vis.drinks.analysis <- reactive({
    
    #Lables for axes
    xvar_name <- names(v.analysis.drinks.axis.vars)[v.analysis.drinks.axis.vars == input$analysis.drinks.xvar]
    yvar_name <- names(v.analysis.drinks.axis.vars)[v.analysis.drinks.axis.vars == input$analysis.drinks.yvar]
    
    xvar <- prop("x", as.symbol(input$analysis.drinks.xvar))
    yvar <- prop("y", as.symbol(input$analysis.drinks.yvar))
    
    dt.analysis.drinks.filter %>%
      ggvis(x = xvar, y = yvar) %>%
      layer_points(size := 50, size.hover := 200,
                   fillOpacity := 0.2, fillOpacity.hover := 0.5, 
                   key := ~id) %>%
      layer_model_predictions(model = "lm", stroke := "red", fill := "red") %>%
      add_tooltip(analysis.drinks_tooltip, "hover") %>%
      add_axis("x", title = xvar_name) %>%
      add_axis("y", title = yvar_name) %>%
      set_options(width = 636, height = 636)
  })
  
  vis.drinks.analysis %>% bind_shiny("drinks_analysis")
  
  output$drinks.analysis.n_drinks <- renderText({ 
    
    nrow(dt.analysis.drinks.filter()) 
    
  })
  
  ################################### PAGE 7 PROPOSAL ##################################
  
  # Filter the drinks, returning a data frame
  dt.analysis.ingredient.filter <- reactive({
    # Due to dplyr issue #318, we need temp variables for input values
    min.ingredient.price <- input$analysis.ingredient.price.filter[1]
    max.ingredient.price <- input$analysis.ingredient.price.filter[2] 
    
    # Apply filters
    m <- dt.ingredients.analysis %>%
      filter(
        adj_ingredient_price >= min.ingredient.price, 
        adj_ingredient_price <= max.ingredient.price
      )
    
    m <- as.data.frame(m)
    
    m
  })
  
  # Generating tooltip text
  ingredient_tooltip <- function(x) {
    if (is.null(x)) return(NULL)
    if (is.null(x$id)) return(NULL)

    #Pick out the drink with this ID
    dt.ingredients.analysis <- isolate(dt.analysis.ingredient.filter())
    analysis.ingredient <- dt.ingredients.analysis[dt.ingredients.analysis$id == x$id, ]

    paste0("<b>", analysis.ingredient$ingredient, "</b><br>",
           "Ingredient Price: ", round(analysis.ingredient$adj_ingredient_price, 2), "<br>",
           "Degree of Ingredient: ", round(analysis.ingredient$ingredient_degree, 2), "<br>",
           "Closeness of Ingredient: ", round(analysis.ingredient$ingredient_closeness, 2), "<br>",
           "Betweenness of Ingredient: ", round(analysis.ingredient$ingredient_betweenness, 2), "<br>",
           "Eigenvector of Ingredient: ", round(analysis.ingredient$ingredient_eigenvector, 2), "<br>"
           )
    }
  
  # A reactive expression with the ggvis plot
  vis.ingredient.analysis <- reactive({
    
    #Lables for axes
    xvar_name <- names(v.analysis.ingredients.axis.vars)[v.analysis.ingredients.axis.vars == input$analysis.ingredients.xvar]
    yvar_name <- names(v.analysis.ingredients.axis.vars)[v.analysis.ingredients.axis.vars == input$analysis.ingredients.yvar]
    
    xvar <- prop("x", as.symbol(input$analysis.ingredients.xvar))
    yvar <- prop("y", as.symbol(input$analysis.ingredients.yvar))
    
    dt.analysis.ingredient.filter %>%
      ggvis(x = xvar, y = yvar) %>%
      layer_points(size := 50, size.hover := 200,
                   fillOpacity := 0.2, fillOpacity.hover := 0.5, 
                   key := ~id) %>%
      layer_model_predictions(model = "lm", stroke := "red", fill := "red") %>%
      add_tooltip(ingredient_tooltip, "hover") %>%
      add_axis("x", title = xvar_name) %>%
      add_axis("y", title = yvar_name) %>%
      set_options(width = 636, height = 636)
  })
  
  vis.ingredient.analysis %>% bind_shiny("ingredient_analysis")
  
  output$ingredient.analysis.n_ingredients <- renderText({ 
    
    nrow(dt.analysis.ingredient.filter()) 
    
  })
  
  ################################### PAGE 8 PROPOSAL ##################################
  
  output$ingredients.in.stock.table <- renderTable({
    dt.ingredients.in.stock <- as.data.table(c(input$ingredients.available))
    colnames(dt.ingredients.in.stock)[1] <- "Ingredients"
    dt.ingredients.in.stock
  })
  
  
  # WIP!!!
  # output$amount.ingredients.in.stock.table <- renderTable({
  #   ingredient.table.length <- length(input$ingredients.available)
  #   ingredient.vector <- c(textInput(("ingredient "+ i),
  #                                    input$ingredients.available[i],
  #                                    placeholder = "amount in ml"))
  #   # for(i in ingredient.table.length){
  #   #   ingredient.vector <- c(ingredient.vector, textInput(("ingredient "+ i),
  #   #                                    input$ingredients.available[i],
  #   #                                    placeholder = "amount in ml"))
  #   # }
  #   ingredient.amount.input.table <- data.table(ingredient.vector)
  #   ingredient.amount.input.table
  #   })
  
  # 
  # output$amount.ingredients.in.stock.table <- reactive(renderTable({
  #   ingredient.table.length <- length(input$ingredients.available)
  #   ingredient.amount.input.table <- data.table()
  #   for(i in ingredient.table.length){
  #     ingredient.amount.input.table$V[i] <- textInput(("ingredient "+ i), input$ingredients.available[i], placeholder = "amount in ml")
  #   }
  # })
  # )  
    
  }

############################################################# ShinyApp #############################################################

shinyApp(ui = ui, server = server)
