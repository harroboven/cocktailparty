# This is the main file of the Cocktail Shiny App :)

library(shiny)
# UI
# Define UI for cocktail app ----
ui <- fluidPage(
  # Title and tabpanels with drop-downs
  navbarPage(title = "Shiny Drinks",
            # 1st Drop-down tabpanels
             navbarMenu("Data Desription", 
                            tabPanel("Summary of Data",   
                                     splitLayout( 
                                       # object 1
                                       verticalLayout( 
                                         # object 1
                                         titlePanel("Header Object 1"),
                                         # object 2,
                                         img(src ='A.png', height = 140, width =300)
                                         ),
                                       # object 2
                                       verticalLayout( 
                                         # object 1
                                         titlePanel("Header Object 2"),
                                         # object 2,
                                         img(src ='A.png', height = 140, width =300)
                                         )
                                       )
                                     ),
                        tabPanel("Data by Drinks", "content 2")
                        ),
            # 2nd tabpanel
            tabPanel("tab 2", "contents"),
            # 2nd tabpanel
            tabPanel("tab 3", "contents")
  )
)


# Server
server <- function(input, output) {
}


#shinyApp()
shinyApp(ui = ui, server = server)
