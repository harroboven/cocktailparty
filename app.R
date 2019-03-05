# This is the main file of the Cocktail Shiny App :)
library(ggplot2)
library(shiny)
library(data.table)
library(stringr)

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
                                         radioButtons(inputId = 'drinks.dist', 
                                                      label = 'Drinks distributed by:', 
                                                      choices = 
                                                        list('Alcoholic nature' = 'an',
                                                             'Drink type' = 'dt',
                                                             'Glass type' = 'gt',
                                                             'Complexity' = 'cc'
                                                             #'Popularity' = 'pp', 
                                                             #'Price' = 'pp'
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
                                                           c('Drink type' = 'dt',
                                                             'Glass type' = 'gt'
                                                             # 'Complexity' = 'cp'
                                                             # VALUES NEED TO BE CHANGED ACCORDING TO PROPOSAL
                                                             #'Popularity' = 'pp',
                                                             #'Price' = 'p'
                                                             )
                                                           ),
                                              p("Drinks filtered by:", 
                                                style = "font-family: 'times'; font-si16pt"
                                              ),
                                              selectInput("alcoholic.filter", "Alcoholic nature:", 
                                                          choices = l.is_alcoholic_values
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

    
  # SelectInput (Dropdown) - Network of one drink
    all.drinks <- drinks[, .(name = unique(name), type = TRUE)],

    all.ingredients <- drinks[, .(name = unique(ingredient), type = FALSE)],
    all.vertices <- rbind(all.drinks, all.ingredients),
    
    g.drinks <- graph.data.frame(drinks[, .(name, ingredient)],
                                 directed = FALSE,
                                 vertices = all.vertices),
    selectInput('d',
              label = 'Choose a drink',
              choices = drinks$name,
              selected = NA)
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
############################################################# SERVER #############################################################
############################################################# SERVER #############################################################


all.vertices <- rbind(all.drinks, all.ingredients)
all.vertices <- all.vertices[!duplicated(all.vertices$id)]
all.vertices[which(all.vertices[,1]=="Applecar"),1] <- "applecarr"
all.vertices[which(all.vertices[,1]=="Limeade"),1] <- "Llimeade"
all.vertices[which(all.vertices[,1]=="Applecarr"),1] <- "A.J."


g.drinks.ingredients <- graph.data.frame(drinks[, .(name, ingredient)],
                             directed = FALSE,
                             vertices = all.vertices)
g.drinks <- bipartite.projection(g.drinks.ingredients)$proj1

server <- function(input, output) {


  # SelectInput (Dropdown) - Network of one drink 
  reactivePlot()
  
}

  # Make the histogram based on the radio input
  output$drinks.dist.barChart <- renderPlot({
    drinks.dist <- switch(input$drinks.dist,
                         an = dt.drinks.filtered$is_alcoholic,
                         dt = dt.drinks.filtered$category,
                         gt = dt.drinks.filtered$glass_type,
                         cc = dt.drinks.filtered$complexity
                         #pp = drinks.filtered$popularity,
                         #p = drinks.filtered$price
    )
    
    ggplot(dt.drinks.filtered, aes(drinks.dist)) +
      geom_bar()
    })
  
  # Make the histogram based on the radio input
  output$drinks.ordered.filtering.barChart <- renderPlot({
    dt.drinks.alcoholic.filter <- dt.drinks.filtered[is_alcoholic == input$alcoholic.filter, ]
    alcoholic.filter <- switch(input$drinks.ordered,
                          dt = dt.drinks.alcoholic.filter$category,
                          gt = dt.drinks.alcoholic.filter$glass_type
                          # cc = dt.drinks.alcoholic.filter$complexity 
                          #pp = drinks.filtered$popularity,
                          #p = drinks.filtered$price
                          )
    
    ggplot(dt.drinks.alcoholic.filter, aes(alcoholic.filter)) +
      geom_bar()
  })
  }


#shinyApp()
shinyApp(ui = ui, server = server)
