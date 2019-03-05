# This is the main file of the Cocktail Shiny App :)
library(ggplot2)
library(shiny)
# UI
# Define UI for cocktail app ----
ui <- fluidPage(
  titlePanel("Hello boys! Your group seems to be awesome but I need your names!"),
  mainPanel(
    
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

)
View(all.drinks)
drinks <- drinks[!duplicated(drinks$id)]
all.drinks <- drinks[, .(id = unique(name), type = TRUE)]
all.ingredients <- drinks[, .(id = unique(ingredient), type = FALSE)]

all.vertices <- rbind(all.drinks, all.ingredients)
all.vertices <- all.vertices[!duplicated(all.vertices$id)]
all.vertices[which(all.vertices[,1]=="Applecar"),1] <- "applecarr"
all.vertices[which(all.vertices[,1]=="Limeade"),1] <- "Llimeade"
all.vertices[which(all.vertices[,1]=="Applecarr"),1] <- "A.J."


g.drinks.ingredients <- graph.data.frame(drinks[, .(name, ingredient)],
                             directed = FALSE,
                             vertices = all.vertices)
g.drinks <- bipartite.projection(g.drinks.ingredients)$proj1

# Server
server <- function(input, output) {

  # SelectInput (Dropdown) - Network of one drink 
  reactivePlot()
  
}
#shinyApp()
shinyApp(ui = ui, server = server)

