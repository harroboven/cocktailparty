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
                                              sliderInput("complexity.filter", "Degree of Recipe Complexity",
                                                          min = min(dt.drinks.filtered$complexity), 
                                                          max = max(dt.drinks.filtered$complexity), 
                                                          value = c(median(dt.drinks.filtered$complexity), mean(dt.drinks.filtered$complexity)), 
                                                          sep = ""
                                                          ),
                                              sliderInput("commonality.filter", "Degree of Commonality", 
                                                          min = 1, 
                                                          max = 3, 
                                                          value = c(1, 3),
                                                          sep = ""
                                                          ),
                                              sliderInput("ingredients.cost.filter", "Ingredients Cost per Drink",
                                                          min = min(dt.drinks.filtered$complexity), 
                                                          max = max(dt.drinks.filtered$complexity), 
                                                          value = c(median(dt.drinks.filtered$complexity), mean(dt.drinks.filtered$complexity)), 
                                                          sep = ""
                                                          ),
                                              selectInput("alcoholic.filter", "Alcoholic Nature",
                                                          l.is_alcoholic_values,
                                                          selected = "All"
                                                          ), 
                                              selectInput("category.filter", "Type of Drink",
                                                          l.category_values, 
                                                          selected = "All"
                                                          ), 
                                              selectInput("glass.filter", "Glass Type",
                                                          l.glass_type_values, 
                                                          selected = "All"
                                                          )
                                              ), 
                                            wellPanel(
                                              selectInput("xvar", "X-axis variable", axis_vars, selected = "complexity"),
                                              selectInput("yvar", "Y-axis variable", axis_vars, selected = "ingredient_price")
                                              )
                                            )
                                          ),
                                   column(9,
                                          verticalLayout(
                                            ggvisOutput("plot1"), 
                                            wellPanel(
                                              span("Number of drinks selected:", 
                                                   textOutput("n_drinks")
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
  
  ######################################### Part 3 ########################################
  
  navbarMenu("tab 3", 
             
  ######################################### Page 6 Proposal ########################################
          
  # 1st Drop-down item
  tabPanel("Impact characteristics on centrality",
           verticalLayout(
             titlePanel("TITLE PLACEHOLDER"),
             fluidRow(
               column(6,
                      plotOutput(outputId = "drinks.complexity.degree")
               ),
               column(6,
                      plotOutput(outputId = "ingredients.price.degree")
                      )
               )
               )
             ),
  
  ######################################### Page 7 Proposal ########################################
  
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
                          uiOutput("test.ingredients")),
                   column(6,
                          uiOutput("test.test"))
                   # column(6,
                   #        plotOutput(outputId = 'drinks.network.ingredients')
                   #        ),
                   # column(6,
                   #        uiOutput("test.table"))
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



# test.x <- get.data.frame(g.drinks.bp)
# View(test.x)

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
  dt.drinks.ordered.filter <- reactive({
    # Due to dplyr issue #318, we need temp variables for input values
    min.complexity <- input$complexity.filter[1]
    max.complexity <- input$complexity.filter[2]
    min.commonality <- input$commonality.filter[1]
    max.commonality <- input$commonality.filter[2]  
    min.ingredients.cost <- input$ingredients.cost.filter[1]
    max.ingredients.cost <- input$ingredients.cost.filter[2] 
    
    # Apply filters
    m <- dt.drinks.filtered %>%
      filter(
        complexity >= min.complexity, 
        complexity <= max.complexity,
        commonality >= min.commonality, 
        commonality <= max.commonality,
        adj_ingredients_cost >= min.commonality, 
        adj_ingredients_cost <= max.commonality
        )
      
    # Filter by alcoholic nature
    if (input$alcoholic.filter != "All") {
      m <- m %>% filter(is_alcoholic %in% input$alcoholic.filter)
      }
    # Filter by drink type
    if (input$category.filter != "All") {
      m <- m %>% filter(category %in% input$category.filter)
      }
    # Filter by glass type
    if (input$glass.filter != "All") {
      m <- m %>% filter(glass_type %in% input$glass.filter)
      }
      
    m <- as.data.frame(m)
      
    m
    })
    
  # Generating tooltip text
  drink_tooltip <- function(x) {
    if (is.null(x)) return(NULL)
    if (is.null(x$id)) return(NULL)
      
    # Pick out the drink with this ID
    dt.drinks.filtered <- isolate(dt.drinks.ordered.filter())
    drink <- dt.drinks.filtered[dt.drinks.filtered$id == x$id, ]
      
    paste0("<b>", drink$name, "</b><br>",
          "Alcoholic Nature: ", drink$is_alcoholic, "<br>",
          "Drink Type: ", drink$category
          )
    }
    
  # A reactive expression with the ggvis plot
  vis <- reactive({
    # Lables for axes
    xvar_name <- names(axis_vars)[axis_vars == input$xvar]
    yvar_name <- names(axis_vars)[axis_vars == input$yvar]
      
    xvar <- prop("x", as.symbol(input$xvar))
    yvar <- prop("y", as.symbol(input$yvar))
      
    dt.drinks.ordered.filter %>%
      ggvis(x = xvar, y = yvar) %>%
      layer_points(size := 50, size.hover := 200,
                   fillOpacity := 0.2, fillOpacity.hover := 0.5, 
                   key := ~id) %>%
      add_tooltip(drink_tooltip, "hover") %>%
      add_axis("x", title = xvar_name) %>%
      add_axis("y", title = yvar_name) %>%
      set_options(width = 636, height = 636)
    })
    
    vis %>% bind_shiny("plot1")
    
    output$n_drinks <- renderText({ nrow(dt.drinks.ordered.filter()) })
  
  
  ################################### PAGE 3 PROPOSAL ##################################
  
  ############ Summary Table of drinks network ############
  output$drinks.network.summary <- renderTable({  
    
    l.num_nodes <- list("Number of Nodes", length(V(g.drinks.bp)))
    l.num_edges <- list("Number of Edges", length(E(g.drinks.bp)))
    l.mean_degree <- list("Average Degree", mean(degree(g.drinks.bp)))
    l.clust_coeff <- list("Clustering Coefficient", transitivity(g.drinks.bp))
    l.mean_betweenness <- list("Average Betweenness", mean(betweenness(g.drinks.bp)))
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
                                dg = degree(g.drinks.bp), 
                                cl = closeness(g.drinks.bp), 
                                bt = betweenness(g.drinks.bp), 
                                ev = eigen_centrality(g.drinks.bp)
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
    
  # plot a graph for drinks with weight X
  output$plot.network.of.drinks <- renderPlot({
    g.drinks.bp <- delete.edges(g.drinks.bp, 
                                E(g.drinks.bp)[weight < input$weight.edges.drink])
                                
    plot.igraph(g.drinks.bp, vertex.label = NA, vertex.size = 0,6, edge.color = 'yellow',
                layout = layout_as_star, edge.arrow.size = 2)
  })
  
  ################################### PAGE 4 PROPOSAL ##################################
  
  ############ Summary Table of ingredient network ############
  output$ingredients.network.summary <- renderTable({  
    
    l.num_nodes <- list("Number of Nodes", length(V(g.ingredients.bp)))
    l.num_edges <- list("Number of Edges", length(E(g.ingredients.bp)))
    l.mean_degree <- list("Average Degree", mean(degree(g.ingredients.bp)))
    l.clust_coeff <- list("Clustering Coefficient", transitivity(g.ingredients.bp))
    l.mean_betweenness <- list("Average Betweenness", mean(betweenness(g.ingredients.bp)))
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
  
  
  ################################### PAGE 7 PROPOSAL ##################################
  
  
    
  ingredients.in.stock <- reactive({
    dt.ingredients.in.stock <- data.table(ingredient = input$ingredients.available)
  })
  
  output$test.ingredients <- renderTable({
    ingredients.in.stock()
  })
  
  
  output$ingredients.in.stock.table <- renderTable({
    dt.ingredients.in.stock <- as.data.table(c(input$ingredients.available))
    colnames(dt.ingredients.in.stock)[1] <- "Ingredients"
    dt.ingredients.in.stock
  })

  dt.drinks.ingredient.deletion <- dt.drinks[, list(ingredient, name)]
  dt.only.drinks <- dt.drinks[, unique(name)]
  drink.list <- test.ingredients
  dt.drinks.ingredient.deletion <- dt.drinks.ingredient.deletion[!(dt.drinks.ingredient.deletion$ingredient %in% dt.ingredients.in.stocks$ingredient)]
  dt.drinks.feasible <- dt.only.drinks[!(dt.only.drinks %in% dt.drinks.ingredient.deletion)]

  output$test.test <- renderTable({
    dt.drinks.feasible
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
  
  
  # output$drinks.network.ingredients <- renderPlot({
  #   visNetwork(g.drinks.bp, vertex.label = NA, vertex.size = 3, edge.color = 'blue',
  #               edge.arrow.size = 2)
  # })
  # 
  # 
  # 
  # 
  
  
  
  # dt.drinks.ingredient.deletion <- dt.drinks.ingredient.deletion[name != c(input$ingredients.available)]


  output$test.table <- renderTable({
    dt.drinks.ingredient.deletion
  })
  
  ############ Centrality Measures Table of ingredient network ############
  output$ingredients.centrality.table <- renderTable({ 
    
    ingredients.centrality <- switch(input$ingredients.centrality.table, 
                                     dg = degree(g.ingredients.bp), 
                                     cl = closeness(g.ingredients.bp), 
                                     bt = betweenness(g.ingredients.bp), 
                                     ev = eigen_centrality(g.ingredients.bp)
                                     )
    
    l.centrality.min <- list("Min", min(ingredients.centrality))
    l.centrality.median <- list("Median", median(ingredients.centrality))
    l.centrality.sd <- list("SD", sd(ingredients.centrality))
    l.centrality.max <- list("Max", max(ingredients.centrality))
    
    dt.ingredients.centrality  <- rbind(l.centrality.min, 
                                   l.centrality.median, 
                                   l.centrality.sd, 
                                   l.centrality.max
                                   )
    
    dt.ingredients.centrality <- as.data.table(dt.ingredients.centrality)
    setnames(dt.ingredients.centrality, old = c("V1", "V2"), new = c("Statistics", "Value"))
    dt.ingredients.centrality
  })
  
  output$ingredients.price.degree <- renderPlot({
    ggplot(dt.ingredients.degrees.merged, aes(x = log_ingredient_price, y = degree)) + geom_point() + geom_smooth(method='lm')
  })
  
  output$drinks.complexity.degree <- renderPlot({
    ggplot(dt.drink.degrees, aes(x = log_complexity, y = degree)) + geom_point() + geom_smooth(method='lm')
  })
  }

############################################################# ShinyApp #############################################################

shinyApp(ui = ui, server = server)
