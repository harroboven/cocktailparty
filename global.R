# Global.R here

# If necessary, install the following libraries
# install.packages("data.table")
# install.packages("ggplot2")
# install.packages("reshape2")
# install.packages("stringr")
# install.packages("shinyWidgets")
# install.packages("shinythemes")
# install.packages("ggvis")
# install.packages("dplyr")
# install.packages("RSQLite")
# install.packages("dbplyr")
# install.packages("visNetwork")



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
dt.longdrinks <- readRDS("./data/longdrinks.rds")
dt.drinks <- dt.longdrinks

########### Additional covariates ###############
# Add complexity of drink recipes
dt.drinks <- dt.drinks[, 
                       complexity := str_count(dt.drinks$instructions)]

# Delete observations with missing or fraudulent complexity data
dt.drinks <- dt.drinks[complexity > 0 & complexity < 550,  ]

# change name of column for better understanding
setnames(dt.drinks, 
         old = "price", 
         new = "ingredient_price")

########### Data cleaning ##############
# Delete non-standardized columns from dt.drinks. They are still in dt.longdrinks, in case you need to access the original ones.
dt.drinks[, 
          c("amount", "category", "unit", "instructions", "glass_type", "std.unit", "quantity") := NULL]

# Rename some of the standardized columns the way the non-standardized were called for ease of use and backwards compatibility
setnames(dt.drinks, 
         old = c("std.quantity", "std.glass", "std.category", "common_final"), 
         new = c("quantity", "glass_type", "category", "commonality"))

# Delete thumbnail (not using it) and era (very few observations).
dt.drinks[, 
          c("thumbnail", "era") := NULL]

# Adjust quantities of ingredients used that are provided in percentage
dt.drinks <- dt.drinks[ingredient != "nutmeg" & ingredient != "cinnamon" & quantity < 1 & glass_type == "shot.glasses" & measurement != "piece" & measurement != "pieces", adj_quantity1 := quantity * 44, by = "ingredient"]
dt.drinks <- dt.drinks[ingredient != "nutmeg" & ingredient != "cinnamon" & quantity < 1 & glass_type == "short.glasses" & measurement != "piece" & measurement != "pieces", adj_quantity2 := quantity * 120, by = "ingredient"]
dt.drinks <- dt.drinks[ingredient != "nutmeg" & ingredient != "cinnamon" & quantity < 1 & glass_type == "chalice.glasses" & measurement != "piece" & measurement != "pieces", adj_quantity3 := quantity * 330, by = "ingredient"]
dt.drinks <- dt.drinks[ingredient != "nutmeg" & ingredient != "cinnamon" & quantity < 1 & glass_type == "long.glasses" & measurement != "piece" & measurement != "pieces", adj_quantity4 := quantity * 340, by = "ingredient"]
dt.drinks <- dt.drinks[ingredient != "nutmeg" & ingredient != "cinnamon" & quantity < 1 & glass_type == "other" & measurement != "piece" & measurement != "pieces", adj_quantity5 := quantity * 340, by = "ingredient"]
dt.drinks <- dt.drinks[, adj_quantity6 := ifelse(!is.na(adj_quantity1), adj_quantity1,
                                                 ifelse(!is.na(adj_quantity2), adj_quantity2,
                                                        ifelse(!is.na(adj_quantity3), adj_quantity3,
                                                               ifelse(!is.na(adj_quantity4), adj_quantity4,
                                                                      ifelse(!is.na(adj_quantity5), adj_quantity5,
                                                                             NA
                                                                             )
                                                                      )
                                                               )
                                                        )
                                                 )
                       ]

############ add values for missing quantities ##############
# add values for missing ml quantities
dt.drinks <- dt.drinks[measurement == "ml", 
                       quantity_help_ml := median(na.omit(quantity)), 
                       by = "glass_type"]

# add values for missing g quantities
dt.drinks <- dt.drinks[measurement == "g", quantity_help_g := 2.5]

# add values for missing "" quantities
dt.drinks <- dt.drinks[measurement == "", 
                       quantity_help_SPACE := median(na.omit(quantity)), 
                       by = "glass_type"]

dt.drinks <- dt.drinks[, adj_quantity7 := ifelse(measurement == "piece", 1, NA)]
dt.drinks <- dt.drinks[, adj_quantity8 := ifelse(measurement == "pieces", 1, NA)]
dt.drinks <- dt.drinks[, adj_quantity9 := ifelse(!is.na(adj_quantity7), adj_quantity7, 
                                                 ifelse(!is.na(adj_quantity8), adj_quantity8, 
                                                        NA
                                                        )
                                                 )
                       ]

# Convert all quantity columns into one column
dt.drinks <- dt.drinks[, adj_quantity := ifelse(!is.na(adj_quantity6), adj_quantity6, 
                                                ifelse(!is.na(adj_quantity9), adj_quantity9, 
                                                       ifelse(!is.na(quantity), quantity, 
                                                              ifelse(!is.na(quantity_help_ml), quantity_help_ml, 
                                                                     ifelse(!is.na(quantity_help_g), quantity_help_g, 
                                                                            quantity_help_SPACE
                                                                            )
                                                                     )
                                                              )
                                                       )
                                                )
                       ]

# delete help quantity columns
dt.drinks <- dt.drinks[, 
                       c("adj_quantity1", 
                         "adj_quantity2", 
                         "adj_quantity3", 
                         "adj_quantity4", 
                         "adj_quantity5", 
                         "adj_quantity6", 
                         "adj_quantity7", 
                         "adj_quantity8", 
                         "adj_quantity9", 
                         "quantity_help_ml", 
                         "quantity_help_g", 
                         "quantity_help_piece", 
                         "quantity_help_pieces", 
                         "quantity_help_SPACE") := NULL]

# delete help quantity columns
dt.drinks <- dt.drinks[, 
                       c("adj_quantity1", "adj_quantity2", "adj_quantity3", "adj_quantity4", "adj_quantity5", "adj_quantity6", "adj_quantity7", "adj_quantity8", "adj_quantity9", "quantity_help_ml", "quantity_help_g", "quantity_help_piece", "quantity_help_pieces", "quantity_help_SPACE") := NULL]


# cost of a single ingredient per drink
dt.drinks <- dt.drinks[, 
                       cost_used_ingredient := 
                         round((ingredient_price / package_size * adj_quantity), 2), 
                       by = "ingredient"]

#add cost of all ingredients in a drink
dt.drinks <- dt.drinks[, 
                       ingredients_cost := sum(cost_used_ingredient), 
                       by = "id"]

# Create adjusted ingredients cost to handle NA values
dt.drinks <- dt.drinks[, 
                       adj_ingredients_cost := ifelse(!is.na(ingredients_cost), 
                                                      ingredients_cost, 
                                                      99
                       ), 
                       by = "id"]

# Create adjusted ingredient price to handle NA values
dt.drinks <- dt.drinks[, 
                       adj_ingredient_price := ifelse(!is.na(ingredient_price), 
                                                      ingredient_price, 
                                                      99
                       ), 
                       by = "id"]

# Reorder the columns for convenience
dt.drinks <- dt.drinks[, 
                       c("id", "ingredient", "quantity", "adj_quantity", "measurement", "package_size", "ingredient_price", "adj_ingredient_price", "cost_used_ingredient", "name", "is_alcoholic", 
                         "category", "glass_type", "commonality", "complexity", "ingredients_cost", "adj_ingredients_cost", "double_observation")]

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

# # select input button proposal page 7

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
