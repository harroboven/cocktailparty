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
               column(12,
                      h1("Welcome to the Shiny Drinks App", style="text-align: center;")),
               #left column
                 column(12,
                        wellPanel(img(src = 'Cocktail picture2.jpg', width = "100%", 
                            style="display: block; margin-left: auto; margin-right: auto;"))),
                 # column(6,
                 #        wellPanel(
                 #          p("Table of Content", style = "font-family: 'times'; font-si16pt; font-size: 26pt"),
                 #        tags$ol(
                 #          tags$li("Data Summary"),
                 #          tags$li("Network Exploration"), 
                 #          tags$li("Network Analysis"))
                 #        )),
               #right column
               column(12, 
                      wellPanel(
                        verticalLayout(
                          # header of right column
                          titlePanel("Let's have some drinks!"),
                          # content of right column
                          p("Nothing can beat a refreshing drink at the right time in the right location. 
                            Drinks offer more than simply a nice taste, but they can be a great conversation starter, 
                            an expression of friendship or simply a tool to accelerate the night. Thus, a well-prepared
                            host of a party knows its audience and plans to offer the appropriate drinks.", 
                            style = "font-family: 'times'; font-si16pt; font-size: 20pt"),
                          p("In recent years the number of drinks has skyrocketed with ever new and extravagant creations. 
                            For many hosts the drinks jungle can become a challenge and the decision of which ingredients 
                            to buy a daunting task. Some drinks share the same ingredients, which can be reused or even 
                            utilized to hedge the risk of being left with a large stock of a certain ingredient. Simultaneously,
                            many hosts already have certain ingredients in stock, which could be used as the foundation for an
                            upcoming event.", 
                            style = "font-family: 'times'; font-si16pt; font-size: 20pt"),
                          p("To cut it short, an appropriate planning tool that gives an overview of the drinks landscape,
                            visualizes connecting ingredients between cocktails and that allows to consider the existing drinks 
                            in stock.", 
                            style = "font-family: 'times'; font-si16pt; font-size: 20pt"),
                          p("To tackle this issue, the shiny drinks app provides an analytics tool that examines different drinks, 
                            their ingredients and the relationship among them to optimize decision making along relevant parameters.
                            As such, this app can help the host of a private party in optimizing ingredient purchase decisions to 
                            avoid being left with an excessive amount of a certain ingredient. For actors in the hospitality industry
                            the application could for instance be leveraged to optimize the drinks portfolio in order to most 
                            effectively provide both traditional and extravagant drinks, or to offer large as well as smaller 
                            cocktails. Lastly, the application can also be a fun gadget for people who want to learn more about the 
                            connections between different cocktails.",
                            style = "font-family: 'times'; font-si16pt; font-size: 20pt"
                            ),
                          p("Let's have some drinks!", 
                            style = "font-family: 'times'; font-si16pt; font-size: 20pt"
                            )))),
               column(12,
                      wellPanel(
                        p("Table of Content", style = "font-family: 'times'; font-si16pt; font-size: 26pt"),
                        tags$ol(
                          tags$li("Data Summary"),
                          tags$li("Network Exploration"), 
                          tags$li("Network Analysis"))
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
                          column(12,
                                 verticalLayout(
                                   wellPanel(
                                   #content of left column
                                   p("On this page a descriptive overview of the entire data set is provided. ", 
                                     style = "font-family: 'times'; font-si16pt; font-size: 16pt"
                                   ),
                                   p("In the first half a static table summarizes the data. Specifically, the app contain information
                                     on drinks and ingredients. For the ingredients, the price is considered the most important 
                                     covariate. Concerning drinks, a variety of covariates is considered.", 
                                     style = "font-family: 'times'; font-si16pt; font-size: 16pt"
                                   ),
                                   p("In the second half of this page, the distribution of drinks is visualized. The criteria by which the drinks shall be distributed can be selected via button input.", 
                                     style = "font-family: 'times'; font-si16pt; font-size: 16pt"
                                   ))
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
                                 ),
                          column(6,
                                 wellPanel(
                                   titlePanel("Summary Statistics"),
                                   p("TABLE PLACEHOLDER")
                                 ))
                          ),
                        # 2nd block of page
                        fluidRow(
                          # left column
                          column(6,
                                 verticalLayout(
                                   wellPanel(
                                     verticalLayout(
                                       #Header of left column
                                       titlePanel("Distribution of Observations"),
                                       #content of left column
                                       p("Please choose a parameter* by which you would like to group the drinks", 
                                         style = "font-family: 'times'; font-si16pt; font-size: 16pt"
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
                                     ),
                                   wellPanel(
                                     p("*Parameter explanation"),
                                     p("Alcoholic Nature = whether a drink contains alcohol or not", style = "font-style: italic"),
                                     p("Drink Categories = the type of drink such as: shot, cocktail, etc.", style = "font-style: italic"),
                                     p("Glass Type = the class that the drink is typically served in such as: long glass", style = "font-style: italic"),
                                     p("Complexity of Recipe = the degree of difficulty of mixing the drink ", style = "font-style: italic"),
                                     p("Commonality = the degree to which the drink is known to the average drink consumer", style = "font-style: italic"),
                                     p("Ingredient Price = the price of an average package size of the respective ingredient", style = "font-style: italic")
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
                                     plotOutput(outputId = "drinks.dist.barChart", width = "100%")
                                     )
                                   )
                                 )
                          )
                        )
                      ), 
             
             ######################################### Page 2 Proposal ########################################
             
             # 2nd Drop-down item
             tabPanel("Drinks Data Explorer", 
                      wellPanel(        
                      titlePanel("Drinks Data Explorer")),
                      fluidRow(
                        column(12,
                               wellPanel(
                                             h4("On this page you have the opportunity to explore all drinks and customize your visualization.
                                             Initially the borders for the three continuous filters is set to the average and median. The
                                             categorical filters are set to include all drinks. At the bottom you can define the axes along

                                             which the drinks will be plotted"))),
                        column(3,
                               verticalLayout(
                                 wellPanel(
                                   h3("Filter"),
                                   sliderInput("drink.explorer.complexity.filter", "Degree of Recipe Complexity",
                                               min = 0,
                                               max = max(dt.drinks.filtered$complexity) + 1,
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
                                               min = 0,
                                               max = round(max(dt.drinks.filtered$adj_ingredients_cost), 0) + 2,
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
                                   h3("Axes parameter seletion"),
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
  navbarMenu("Network Exploration", 
             
             ######################################### Page 3 Proposal ########################################
             
             # 1st Drop-down item
             tabPanel("Exploration by Drinks", 
                      verticalLayout(
                        # header of whole page
                        titlePanel("Network Exploration by Drinks"),
                        # 1st block of page
                        fluidRow(
                          column(12,
                                 wellPanel(
                                   h4("On this page you get to further explore the network. The network of drinks is summarized
                                      along basic summary statistics, as well as, centrality measures. In the second half of the
                                      page the entire, as well as, a subset of the drinks network are visualized")
                                 )
                                 ),
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
                                 wellPanel(
                                 # title of left column
                                 titlePanel("Network of Drinks"),
                                 fluidRow(
                                   # left sub-column
                                   column(12,
                                          # left sub-column
                                          p("This visualization provides an overview of the strength of similarity among cocktails.
                                            Specifically, the weight of the edges represents the number of ingredients that two
                                            cocktails have in common.", 
                                            style = "font-family: 'times'; font-si16pt; font-size: 16pt"),
                                          
                                          # SliderInput - Network of drinks
                                          sliderInput('weight.edges.drink',
                                                      label = 'Min. number of ingredients in common:', 
                                                      min = 1, max = 10, value = 3, step = 1)
                                          ),
                                   # right sub-column
                                   column(12,
                                          # right sub-column
                                          
                                          visNetworkOutput(outputId = 'plot.network.of.drinks')))),

                          # right object
                          column(6,
                                 wellPanel(
                                 # title of right column
                                 titlePanel("Network of one Drink"),
                                 fluidRow(
                                   # left sub-column
                                   column(12,
                                          # Introtext
                                          p("This visualization provides an overview of the neighbouring network of one drink.", 
                                            style = "font-family: 'times'; font-si16pt; font-size: 16pt"),
                                          # Drink Choice
                                          selectInput('network.of.one.drink',
                                                      label = 'Network of one drink',
                                                      selected = dt.drinks$name[744],
                                                      choices = dt.drinks$name
                                          )))),
                                  # a graphic plot for network of one drink
                                 visNetworkOutput(outputId = 'plot.network.of.one.drink')
                                   
                                 
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
                            column(12,
                                   wellPanel(
                                    h4("On this page you get to further explore the network. The network of ingredients is summarized
                                    along basic summary statistics, as well as, centrality measures. In the second half of the
                                    page the entire, as well as, a subset of the ingredients network are visualized")
                                    )
                                 ),
                          # left column
                          column(6,
                                 wellPanel(
                                   # title of left object
                                   titlePanel("Summary Statistics of the Network of Ingredients"),
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
                                 wellPanel(
                                 # title of left column
                                 titlePanel("Network of Ingredients"),
                                 # 1st sub-block
                                 fluidRow(
                                   column(12,
                                          # left sub-column
                                          p("Introtext", style = "font-family: 'times'; font-si16pt"),
                                          p("Minimum weight of edges:", style = "font-family: 'times'; font-si16pt"),
                                          # SliderInput - Network of drinks based on their weights
                                          sliderInput('weight.edges.ingredient',
                                                      label = 'Min. weight of edges:', 
                                                      min = 1, max = 10, value = 3, step = 1)
                                          ))),
                                 # a graphic plot for a network of ingredients 
                                 visNetworkOutput(outputId = 'plot.network.of.ingredients')))),
            ),

                          # right object
                          column(6,
                                 wellPanel(
                                 # title of right column
                                 titlePanel("Network of one ingredient"),
                                 fluidRow(
                                   # left sub-column
                                   column(12,
                                          # Introtext
                                          p("Introtext", style = "font-family: 'times'; font-si16pt"),
                                          # Drink Choice
                                          p("Choose ingredients", style = "font-family: 'times'; font-si16pt"),
                                          # input selection for a network of one ingredient, which is linked to the server
                                          selectInput('network.of.one.ingredient',
                                                      label = 'Network of one ingredient',
                                                      selected = NA,
                                                      choices = dt.drinks$ingredient)
                                          ),
                                   # right sub-column
                                   column(12,
                                          # a graphic plot for a network of one ingredient 
                                          visNetworkOutput(outputId = 'plot.network.of.one.ingredient')
                                          )
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
                                        # SliderInputs - Network of drinks
                                        # Maximal cost of one ingredient and one drink 
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
               column(12,
                      wellPanel(
                        h4("This page allows you to explore the relation between the characteristics of drinks and their centrality.")
                      )
                      ),
               column(4,
                      wellPanel(
                        h3("Filter"),
                        sliderInput("analysis.drinks.complexity.filter", "Degree of Recipe Complexity",
                                    min = 0, 
                                    max = max(dt.drinks.analysis$complexity) + 2, 
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
                                    min = 0, 
                                    max = round(max(dt.drinks.analysis$adj_ingredients_cost), 0) + 2, 
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
                        h3("Axes parameter seletion"),
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
                          ggvisOutput("drinks_analysis"),
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
               column(12,
                      wellPanel(
                        h4("This page allows you to explore the relation between the characteristics of ingredients and their centrality."))
                      ),
               column(4,
                      wellPanel(
                        h3("Filter"),
                        sliderInput("analysis.ingredient.price.filter", "Ingredient Price",
                                    min = 0, 
                                    max = round(max(dt.ingredients.analysis$adj_ingredient_price), 0) + 2, 
                                    value = c(median(dt.ingredients.analysis$adj_ingredient_price), mean(dt.ingredients.analysis$adj_ingredient_price)), 
                                    sep = ""
                                    )
                        ),
                      wellPanel(
                        h3("Axes parameter seletion"),
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
                          ggvisOutput("ingredient_analysis"),
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
  
# Create the tab for the advanced analysis
  tabPanel("Cocktail Party Planner",
           verticalLayout(
             titlePanel("Cocktail Planner"),
             fluidRow(
               column(12,
                      h4("On this page you get to plan your cocktail night. You can specify the drinks that you already have
                         in stock and the drinks that you can create with those. Afterwards you can add additional ingredients
                         and see how the list and the network of feasible cocktails extends.")),
               wellPanel(
                 selectInput("ingredients.available",
                             "Available Stock",
                             l.all.ingredients,
                             selected = "water",
                             multiple = TRUE),
                 fluidRow(
                   # Table with all ingredients at hand
                   column(6,
                          uiOutput("ingredients.in.stock")),
                   # Oveview graph
                   column(6,
                          plotOutput(outputId = "tester")),
                   # Table output of all possible drinks
                   column(6,
                            p("These are the drinks you can mix with the ingredients in stock"),
                          tableOutput("table.test")),
                   column(6,
                          p("Hello")),
                   column(6,
                          plotOutput("plot.ingredients.in.stock"))
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
      add_axis("x", title = xvar_name, 
               properties = axis_props(
                 labels = list(fontSize = 16, stroke = "grey"),
                 title = list(fontSize = 16, dy =+ 25))
               ) %>%
      add_axis("y", title = yvar_name,
               properties = axis_props(
                 labels = list(fontSize = 16, stroke = "grey"),
                 title = list(fontSize = 16, dy =- 25))
               ) %>%
      add_axis("x", orient = "top", ticks = 0, 
               title = "Parameter Relations",
               properties = axis_props(
                 axis = list(stroke = "white"),
                 labels = list(fontSize = 0, stroke = "grey"),
                 title = list(fontSize = 20))
               ) %>%
      
      set_options(width = "100%", height = 636)
    })
  
  vis.drink.explorer %>% bind_shiny("drink_explorer")
    
  output$drink.explorer.n_drinks <- renderText({
    nrow(dt.drink.explorer())
    })
  
  
  ################################### PAGE 3 PROPOSAL ##################################
  
   ############ Summary Table of drinks network ############
  output$drinks.network.summary <- renderTable({  
    
    l.num_nodes <- list("Number of Nodes", format(length(V(g.drinks.bp))))
    l.num_edges <- list("Number of Edges", format(length(E(g.drinks.bp))))
    l.mean_degree <- list("Average Degree", round(mean(dt.drinks.degree$drink_degree), 2))
    l.clust_coeff <- list("Clustering Coefficient", round(transitivity(g.drinks.bp), 2))
    l.mean_betweenness <- list("Average Betweenness", round(mean(dt.drinks.betweenness$drink_betweenness), 2))
    l.mean_path_length <- list("Average Path Length", round(mean(distances(g.drinks.bp)), 2))
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
  # 1. create an output by plot rendering 
  output$plot.network.of.drinks <- renderVisNetwork({
    # delete edges with weights smaller than indicated by the input
    g.drinks.bp <- delete.edges(g.drinks.bp, 
                                E(g.drinks.bp)[weight < input$weight.edges.drink])
    # delete unconnected vertices to make the plot look more appealing 
    g.drinks.bp <- delete.vertices(g.drinks.bp, V(g.drinks.bp)[degree(g.drinks.bp) == 0])
    # convert the igraph object into a VisNetwork one
    vis.g.drinks.bp <- toVisNetworkData(g.drinks.bp)
    # define nodes and edges 
    # plot the graph 
   visNetwork(nodes = vis.g.drinks.bp$nodes, edges = vis.g.drinks.bp$edges,
              height = '500px', width = '200%') %>% 
     # define relevant parameters 
     visInteraction(dragNodes = TRUE, dragView = TRUE, zoomView = TRUE, hideEdgesOnDrag = FALSE, hideNodesOnDrag = FALSE, hover = TRUE) %>%
     visPhysics(stabilization = FALSE, solver = 'barnesHut') %>% 
     visEdges(smooth = FALSE) %>%
     visOptions(highlightNearest = list(enabled = T, degree = 1, hover = T))

    })
  
  
  ################## graph for a single drink ######################
  # create an output by rendering a vis plot 
  output$plot.network.of.one.drink <- renderVisNetwork({
    # extract the immeadiate neighbouring nodes of a selected drink 
  neigh.nodes.drinks <- neighborhood(g.drinks.bp, order = 1, nodes = V(g.drinks.bp)$name == input$network.of.one.drink)[[1]]
    # set the node colour, edge line pattern, colour the selected vertex into red, create labe lnames 
    V(g.drinks.bp)$color <- 'grey'
    E(g.drinks.bp)$ltly <- 'dotted'
    V(g.drinks.bp)[name == input$network.of.one.drink]$color <- 'tomato'
    V(g.drinks.bp)[name == input$network.of.one.drink]$label <- paste0(input$network.of.one.drink)
    
    # convert the igraph obejct into a VisNetwork one 
  vis.g.drinks.bp.neighbourhood <- toVisNetworkData(induced.subgraph(g.drinks.bp, neigh.nodes.drinks))
  visNetwork(nodes = vis.g.drinks.bp.neighbourhood$nodes, edges = vis.g.drinks.bp.neighbourhood$edges,
             height = '500px', width = '100%')%>% 
    # set plotting parameters 
    visIgraphLayout(randomSeed = TRUE) %>% 
    visInteraction(dragNodes = TRUE, dragView = TRUE, zoomView = TRUE, hideEdgesOnDrag = FALSE, hideNodesOnDrag = FALSE, hover = TRUE) %>%
    visPhysics(stabilization = FALSE, solver = 'barnesHut') %>% 
    visEdges(smooth = FALSE) %>%
    visOptions(highlightNearest = list(enabled = T, degree = 1, hover = T))
    
  })
  
  ################################### PAGE 4 PROPOSAL ##################################
  
   ############ Summary Table of ingredient network ############
  output$ingredients.network.summary <- renderTable({
    
    l.num_nodes <- list("Number of Nodes", format(length(V(g.ingredients.bp))))
    l.num_edges <- list("Number of Edges", format(length(E(g.ingredients.bp))))
    l.mean_degree <- list("Average Degree", round(mean(dt.ingredients.degree$ingredient_degree), 2))
    l.clust_coeff <- list("Clustering Coefficient", round(transitivity(g.ingredients.bp), 2))
    l.mean_betweenness <- list("Average Betweenness", round(mean(dt.ingredients.betweenness$ingredient_betweenness), 2))
    l.mean_path_length <- list("Average Path Length", round(mean(distances(g.ingredients.bp)), 2))
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
    
    output$plot.network.of.ingredients <- renderVisNetwork({
      g.ingredients.bp <- delete.edges(g.ingredients.bp,
                                       E(g.ingredients.bp)[weight < input$weight.edges.ingredient])
      g.ingredients.bp <- delete.vertices(g.ingredients.bp, V(g.ingredients.bp)[degree(g.ingredients.bp) == 0])
      deg.ingredients <- V(g.ingredients.bp)$degree
      V(g.ingredients.bp)$size <- deg.ingredients/2
      vis.g.ingredients.bp <- toVisNetworkData(g.ingredients.bp)
      
    visNetwork(nodes = vis.g.ingredients.bp$nodes, edges = vis.g.ingredients.bp$edges,
               height = '500px', width = '100%') %>% 
      visInteraction(dragNodes = TRUE, dragView = TRUE, zoomView = TRUE, hideEdgesOnDrag = FALSE, hideNodesOnDrag = FALSE, hover = TRUE) %>%
      visPhysics(stabilization = FALSE, solver = 'barnesHut') %>% 
      visEdges(smooth = FALSE) %>%
      visOptions(highlightNearest = list(enabled = T, degree = 1, hover = T))
      
      
    #  plot.igraph(g.ingredients.bp,vertex.label = NA, vertex.size = deg.ingredients/2, 
     #             edge.color = 'tomato', layout = layout_on_sphere,
      #            edge.arrow.size = 1)
    })
    


  
   ################## graph for a single ingredient ######################
  
  # create an output by rendering a vis plot 
  output$plot.network.of.one.ingredient <- renderVisNetwork({
      # extract the immeadiate neighbouring nodes of a selected drink 
      neigh.nodes.ingredients <- neighborhood(g.ingredients.bp, order = 1, nodes = V(g.ingredients.bp)$name == input$network.of.one.ingredient)[[1]]
      # set the node colour, edge line pattern, colour the selected vertex into red, create labe lnames 
      V(g.ingredients.bp)$color <- 'grey'
      V(g.ingredients.bp)[name == input$network.of.one.ingredient]$label <- paste0(input$network.of.one.ingredient)  
      E(g.ingredients.bp)$ltly <- 'dotted'
      V(g.ingredients.bp)[name == input$network.of.one.ingredient]$color <- 'tomato'
      # convert the igraph obejct into a VisNetwork one 
      vis.g.ingredients.bp.neighbourhood <- toVisNetworkData(induced.subgraph(g.ingredients.bp, neigh.nodes.ingredients))
      # plot 
       visNetwork(nodes = vis.g.ingredients.bp.neighbourhood$nodes, edges = vis.g.ingredients.bp.neighbourhood$edges,
                 height = '500px', width = '100%') %>% 
         # set plotting parameters 
         visIgraphLayout(randomSeed = TRUE)
       
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
      add_axis("x", title = xvar_name,
               properties = axis_props(
                 labels = list(fontSize = 16, stroke = "grey"),
                 title = list(fontSize = 16, dy =+ 25, stroke = "grey"))
               ) %>%
      add_axis("y", title = yvar_name,
               properties = axis_props(
                 labels = list(fontSize = 16, stroke = "grey"),
                 title = list(fontSize = 16, dy =- 25, stroke = "grey"))
               ) %>%
      add_axis("x", orient = "top", ticks = 0, 
               title = "Centrality by drink drink parameters",
               properties = axis_props(
                 axis = list(stroke = "white"),
                 labels = list(fontSize = 0, stroke = "grey"),
                 title = list(fontSize = 20))
               ) %>%
      set_options(width = "100%", height = 636)
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
      add_axis("x", title = xvar_name,
               properties = axis_props(
                 labels = list(fontSize = 16, stroke = "grey"),
                 title = list(fontSize = 16, dy =+ 25, stroke = "grey"))
               ) %>%
      add_axis("y", title = yvar_name,
               properties = axis_props(
                 labels = list(fontSize = 16, stroke = "grey"),
                 title = list(fontSize = 16, dy =- 25, stroke = "grey"))
               ) %>%
      add_axis("x", orient = "top", ticks = 0, 
               title = "Centrality by Ingredient Price",
               properties = axis_props(
                 axis = list(stroke = "white"),
                 labels = list(fontSize = 0, stroke = "grey"),
                 title = list(fontSize = 20))
               ) %>%
      set_options(width = "100%", height = 636)
  })
  
  vis.ingredient.analysis %>% bind_shiny("ingredient_analysis")
  
  output$ingredient.analysis.n_ingredients <- renderText({ 
    
    nrow(dt.analysis.ingredient.filter()) 
    
  })
  
  ################################### PAGE 8 PROPOSAL ##################################
 
    # Create a reactive data table that allows to specify all ingredients that are already in stock
    dt.ingredients.in.stock <- reactive({
      dt.ingredients.in.stock <- data.table(ingredient = input$ingredients.available)
    })
    
    # The out put for the stock of ingredients
    output$ingredients.in.stock <- renderTable({
      dt.ingredients.in.stock()
    })
    
    # Find drinks that can be build with the selected ingredients
    # Create a data table where all drinks are deleted whose ingredients are already in stock
    dt.drinks.deletion <- reactive({
      dt.drinks.ingredient.deletion <- dt.drinks[, list(ingredient, name)]
      dt.drinks.ingredient.deletion <- dt.drinks.ingredient.deletion[!(dt.drinks.ingredient.deletion$ingredient %in% dt.ingredients.in.stock()$ingredient)]
    })
   
    # Create a data table with all drinks that are possible with the ingredients at hand 
    dt.drinks.feasible <- reactive({
      dt.drinks.feasible <- dt.drinks[, list(unique(name))]
      dt.drinks.feasible <- dt.drinks[!(dt.drinks$name %in% dt.drinks.deletion()$name), list(unique(name))]
    })
      
    # Set up the igraph
      current.possible.drinks <- dt.drinks[,
                                           list(name = unique(name),
                                             type = TRUE
                                           )]
      current.available.ingredients <- dt.drinks[,
                                                 list(name = unique(ingredient),
                                                   type = FALSE
                                                 )]
      all.available.vertices <- rbind(current.possible.drinks,
                                      current.available.ingredients)


      g.drinks.ingredients.available <- graph_from_data_frame(dt.drinks[,
                                                                        list(name, ingredient)
                                                                        ],
                                                              directed = FALSE,
                                                              vertices = all.available.vertices)

      g.drinks.ingredients.available.bp <- bipartite.projection(g.drinks.ingredients.available)$proj2
      

      # Create a reactive graph
      g.drinks.feasible <- reactive({
        V(g.drinks.ingredients.available.bp)$color <- ifelse(V(g.drinks.ingredients.available.bp) %in% dt.drinks.feasible()$V1, "green", "white")
        plot.igraph(g.drinks.ingredients.available.bp, vertex.label = NA, vertex.size = 3,
                    layout = layout_nicely, edge.arrow.size = 1)
      })

      # create the output for the graph
    output$tester <- renderPlot({
      g.drinks.feasible()
    })
    
    # create the ourput for the feasible drinks
    output$table.test <- renderTable({
      dt.drinks.feasible()
    })
    
    # Additional graph output try
    
    output$plot.ingredients.in.stock <- renderPlot({
      neigh.nodes.ingredients.in.stock <- adjacent_vertices(g.drinks.ingredients.available.bp, dt.drinks.feasible()$V1)
      # V(g.drinks.ingredients.available.bp)$color <- 'grey'
      # V(g.drinks.ingredients.available.bp)[name %in% dt.drinks.feasible()]$label <- paste0(dt.drinks.feasible())  
      # E(g.drinks.ingredients.available.bp)$ltly <- 'dotted'
      # V(g.drinks.ingredients.available.bp)[name %in% dt.drinks.feasible()]$color <- 'tomato'
      # plot(induced.subgraph(g.ingredients.bp, neigh.nodes.ingredients.in.stock), layout = layout_with_graphopt) 
      # visIgraph(g.one.ingredient)
    })
    
    
  }

############################################################# ShinyApp #############################################################

shinyApp(ui = ui, server = server)
