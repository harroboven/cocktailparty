# This is the main file of the Cocktail Shiny App :)

library(shiny)
# UI
# Define UI for cocktail app ----
ui <- fluidPage(
  # Title and tabpanels with drop-downs
  navbarPage(title = "Shiny Drinks",
            # 1st Drop-down tabpanels
             navbarMenu("Data Desription", 
                        # 1st Drop-down item
                        tabPanel("Summary of Data", 
                                 verticalLayout(
                                   # header of whole page
                                   titlePanel("Summary of Data"),
                                   # rest of page
                                   splitLayout( 
                                     # left object 
                                     verticalLayout(
                                       #title of left object
                                       titlePanel("Explanation of Summary"),
                                       #content of left object
                                       p("INTROTEXT", 
                                         style = "font-family: 'times'; font-si16pt")
                                       ),
                                     # right object 
                                     verticalLayout( 
                                       # object 1
                                       titlePanel("Header Object 2"),
                                       # object 2,
                                       img(src ='A.png', height = 140, width =300)
                                       )
                                     )
                                   )
                        ),
                        # 2nd Drop-down item
                        tabPanel("Data by Drinks", "content 2")
                        ),
            # 2nd tabpanel
            navbarMenu("Networking Exploration",
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
                       # 2nd Drop-down item
                       tabPanel("Exploration by ingredients", 
                                "contents"
                                ),
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

}


#shinyApp()
shinyApp(ui = ui, server = server)
