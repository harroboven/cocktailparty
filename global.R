# Global.R here

# If necessary, install the following libraries
install.packages("data.table")
install.packages("ggplot2")
install.packages("reshape2")
install.packages("stringr")
install.packages("shinyWidgets")
install.packages("shinythemes")
install.packages("ggvis")
install.packages("dplyr")
install.packages("RSQLite")
install.packages("dbplyr")
install.packages("visNetwork")



# Load the necessary libraries
library(data.table)
library(ggplot2)
library(stringr)
library(reshape2)
library(shinyWidgets)
library(igraph, quietly = TRUE, warn.conflicts = FALSE, verbose = FALSE)
library(shinythemes)
library(ggvis)
library(dplyr)
library(RSQLite)
library(dbplyr)
library(visNetwork, quietly = TRUE)

########## Data loading ###########
# Instruction that loads prepared data into memory
dt.drinks <- readRDS("./data/drinks.rds")

######## Buttons ##########
# preparing dt.drinks for button work by ensuring that only unique drinks
dt.drinks.filtered <- unique(dt.drinks, 
                             by = "id")

# select input button proposal paage 2
l.is_alcoholic_values <- c("All", sort(unique(dt.drinks.filtered$is_alcoholic), 
                                       decreasing = FALSE)) 

l.category_values <- c("All", sort(unique(dt.drinks.filtered$category), 
                                   decreasing = FALSE))

l.glass_type_values <- c("All", sort(unique(dt.drinks.filtered$glass_type), 
                                     decreasing = FALSE))

# # select input button proposal page 8

l.all.ingredients <- c("All", dt.drinks[, .(name = unique(ingredient))])


############## Plot themes #################

chart.theme.1 <- theme(plot.title = element_text(family = "Helvetica", face = "bold", size = (15)), 
                       legend.title = element_text(colour = "steelblue",  face = "bold.italic", family = "Helvetica"), 
                       legend.text = element_text(face = "italic", colour="steelblue4",family = "Helvetica"), 
                       axis.title = element_text(family = "Helvetica", size = (10), colour = "steelblue4"),
                       axis.text = element_text(family = "Courier", colour = "cornflowerblue", size = (10)))

############# Graphs ################
# create constant objects - always stay the same
all.drinks <- dt.drinks[, 
                        .(name = unique(name), 
                          type = TRUE
                        )
                        ]
all.ingredients <- dt.drinks[, 
                             .(name = unique(ingredient), 
                               type = FALSE
                             )
                             ]
all.vertices <- rbind(all.drinks, 
                      all.ingredients
)
#all.vertices <- all.vertices[!duplicated(all.vertices$id)]

g.drinks.ingredients <- graph_from_data_frame(dt.drinks[, 
                                                        .(name, ingredient)
                                                        ], 
                                              directed = FALSE,
                                              vertices = all.vertices
)
g.drinks.bp <- bipartite.projection(g.drinks.ingredients)$proj2
g.ingredients.bp <- bipartite.projection(g.drinks.ingredients)$proj1

# Variables that can be put on the x and y axes
v.drink.explorer.axis.vars <- c(
  "Recipe Complexity" = "complexity",
  "Commonality" = "commonality",
  "Ingredients Cost per Drink" = "adj_ingredients_cost"
)

################################### centrality measures ##################################

# drink network
df.drinks.degree <- data.frame(degree(g.drinks.bp))
dt.drinks.degree <- data.table(cbind(row.names(df.drinks.degree), 
                                     df.drinks.degree))
colnames(dt.drinks.degree)[1:2] <- c("name", "drink_degree")

df.drinks.closeness <- data.frame(closeness(g.drinks.bp))
dt.drinks.closeness <- data.table(cbind(row.names(df.drinks.closeness), 
                                        df.drinks.closeness))
colnames(dt.drinks.closeness)[1:2] <- c("name", "drink_closeness")

df.drinks.betweenness <- data.frame(betweenness(g.drinks.bp))
dt.drinks.betweenness <- data.table(cbind(row.names(df.drinks.betweenness), 
                                          df.drinks.betweenness))
colnames(dt.drinks.betweenness)[1:2] <- c("name", "drink_betweenness")

df.drinks.eigenvector <- data.frame(evcent(g.drinks.bp))
dt.drinks.eigenvector <- data.table(cbind(row.names(df.drinks.eigenvector), 
                                          df.drinks.eigenvector))
colnames(dt.drinks.eigenvector)[1:2] <- c("name", "drink_eigenvector")
dt.drinks.eigenvector <- dt.drinks.eigenvector[, 1:2]
dt.drinks.eigenvector$drink_eigenvector <- as.numeric(dt.drinks.eigenvector$drink_eigenvector)

dt.drinks.centrality <- merge(dt.drinks.degree, dt.drinks.closeness, by = "name")
dt.drinks.centrality <- merge(dt.drinks.centrality, dt.drinks.betweenness, by = "name")
dt.drinks.centrality <- merge(dt.drinks.centrality, dt.drinks.eigenvector, by = "name")

#ingredient network
df.ingredients.degree <- data.frame(degree(g.ingredients.bp))
dt.ingredients.degree <- data.table(cbind(row.names(df.ingredients.degree), 
                                          df.ingredients.degree))
colnames(dt.ingredients.degree)[1:2] <- c("ingredient", "ingredient_degree")

df.ingredients.closeness <- data.frame(closeness(g.ingredients.bp))
dt.ingredients.closeness <- data.table(cbind(row.names(df.ingredients.closeness), 
                                             df.ingredients.closeness))
colnames(dt.ingredients.closeness)[1:2] <- c("ingredient", "ingredient_closeness")

df.ingredients.betweenness <- data.frame(betweenness(g.ingredients.bp))
dt.ingredients.betweenness <- data.table(cbind(row.names(df.ingredients.betweenness), 
                                               df.ingredients.betweenness))
colnames(dt.ingredients.betweenness)[1:2] <- c("ingredient", "ingredient_betweenness")

df.ingredients.eigenvector <- data.frame(evcent(g.ingredients.bp))
dt.ingredients.eigenvector <- data.table(cbind(row.names(df.ingredients.eigenvector), 
                                               df.ingredients.eigenvector))
colnames(dt.ingredients.eigenvector)[1:2] <- c("ingredient", "ingredient_eigenvector")
dt.ingredients.eigenvector <- dt.ingredients.eigenvector[, 1:2]
dt.ingredients.eigenvector$ingredient_eigenvector <- as.numeric(dt.ingredients.eigenvector$ingredient_eigenvector)

dt.ingredients.centrality <- merge(dt.ingredients.degree, dt.ingredients.closeness, by = "ingredient")
dt.ingredients.centrality <- merge(dt.ingredients.centrality, dt.ingredients.betweenness, by = "ingredient")
dt.ingredients.centrality <- merge(dt.ingredients.centrality, dt.ingredients.eigenvector, by = "ingredient")

################################### PAGE 6 PROPOSAL ##################################

dt.drinks.centrality.complete <- merge(dt.drinks.centrality,
                                       dt.drinks, 
                                       by = "name" )

dt.drinks.centrality.complete$log_adj_ingredients_cost <- log(dt.drinks.centrality.complete$adj_ingredients_cost)
dt.drinks.centrality.complete$log_complexity <- log(dt.drinks.centrality.complete$complexity)

dt.drinks.analysis <- dt.drinks.centrality.complete[, 
                                                    list(name, 
                                                         is_alcoholic, 
                                                         category, 
                                                         glass_type, 
                                                         commonality,
                                                         complexity, 
                                                         log_complexity,
                                                         adj_ingredients_cost, 
                                                         log_adj_ingredients_cost, 
                                                         drink_degree, 
                                                         drink_closeness, 
                                                         drink_betweenness, 
                                                         drink_eigenvector
                                                    )
                                                    ]

dt.drinks.analysis <- unique(dt.drinks.analysis)

# add id for ingredients to enable tooltip feature
dt.drinks.analysis <- dt.drinks.analysis[, id := 1:length(name)]

# axis options for graph of drinks network
v.analysis.drinks.axis.vars <- c(
  "Commonality" = "commonality",
  "Complexity" = "complexity", 
  "Ingredients Cost per Drink" = "adj_ingredients_cost",  
  "Degree" = "drink_degree", 
  "Closeness" = "drink_closeness", 
  "Betweenness" = "drink_betweenness", 
  "Eigenvector" = "drink_eigenvector"
)

################################### PAGE 7 PROPOSAL ##################################

dt.ingredients.centrality.complete <- merge(dt.ingredients.centrality, 
                                            dt.drinks, 
                                            by = "ingredient")

dt.ingredients.centrality.complete$log_adj_ingredient_price <- log(dt.ingredients.centrality.complete$adj_ingredient_price)

dt.ingredients.analysis <- dt.ingredients.centrality.complete[, 
                                                              list(ingredient, 
                                                                   adj_ingredient_price, 
                                                                   log_adj_ingredient_price,
                                                                   ingredient_degree, 
                                                                   ingredient_closeness, 
                                                                   ingredient_betweenness, 
                                                                   ingredient_eigenvector
                                                              )
                                                              ]
dt.ingredients.analysis <- unique(dt.ingredients.analysis)

# add id for ingredients to enable tooltip feature
dt.ingredients.analysis <- dt.ingredients.analysis[, id := 1:length(ingredient)]

# axis options for graph of ingredient network
v.analysis.ingredients.axis.vars <- c(
  "Ingredient Price" = "adj_ingredient_price", 
  "Degree" = "ingredient_degree", 
  "Closeness" = "ingredient_closeness", 
  "Betweenness" = "ingredient_betweenness", 
  "Eigenvector" = "ingredient_eigenvector"
)

