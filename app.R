# This is the main file of the Cocktail Shiny App :)
library(ggplot2)
library(shiny)
library(data.table)

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
                                       radioButtons("dist", "Distribution type:",
                                                    c("Normal" = "norm",
                                                      "Uniform" = "unif",
                                                      "Log-normal" = "lnorm",
                                                      "Exponential" = "exp")),
                                       plotOutput("distPlot")

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
                                         radioButtons(inputId = 'var.choice', 
                                                      label = 'Drinks distributed by:', 
                                                      choices = 
                                                        list('Alcoholic nature' = 'an',
                                                             'Drink type' = 'dt',
                                                             'Glass type' = 'gt'
                                                             #'Complexity' = 'cc',
                                                             #'Popularity' = 'pp', 
                                                             #'Price' = 'pp'
                                                             )
                                                      )
                                       )
                                     ),
                                     # right object 
                                     verticalLayout( 
                                       # Header right object
                                       titlePanel("Header Object 2"),
                                       # histogram
                                       plotOutput(outputId = "histogram")
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
            # 2nd tabpanel
            tabPanel("tab 3", 
                     "contents"
                     )
            )
)


# Server
server <- function(input, output) {
  # Make the histogram based on the radio input
  output$histogram <- renderPlot({
    var.choice <- switch(input$var.choice,
                         an = drinks.filtered$is_alcoholic,
                         dt = drinks.filtered$category,
                         gt = drinks.filtered$glass_type
                         #cc = drinks.filtered$instructions,
                         #pp = drinks.filtered$popularity,
                         #p = drinks.filtered$price
    )
    
    ggplot(drinks.filtered, aes(var.choice)) +
      geom_bar()
    })
  
  
  output$distPlot <- renderPlot({
    dist <- switch(input$dist,
                   norm = rnorm,
                   unif = runif,
                   lnorm = rlnorm,
                   exp = rexp,
                   rnorm)
    
    hist(dist(500))
  })
  }


#shinyApp()
shinyApp(ui = ui, server = server)
