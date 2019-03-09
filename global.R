# Global.R here

########## Data loading ###########
# Instruction that loads prepared data into memory
dt.longdrinks <- readRDS("./data/longdrinks.rds")
dt.drinks <- dt.longdrinks

########### Additional covariates ###############
# Add complexity of drink recipes
dt.drinks <- dt.drinks[, 
                       complexity := str_count(dt.drinks$instructions)]

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

# add values for missing quantities
# add values for missing ml quantities
dt.drinks <- dt.drinks[measurement == "ml", 
                       quantity_help_ml := median(na.omit(quantity)), 
                       by = "glass_type"]

# add values for missing g quantities
dt.drinks <- dt.drinks[measurement == "g", 
                       quantity_help_g := median(na.omit(quantity)), 
                       by = "glass_type"]

# add values for missing piece quantities
dt.drinks <- dt.drinks[measurement == "piece", 
                       quantity_help_piece := median(na.omit(quantity)), 
                       by = "glass_type"]

# add values for missing pieces quantities
dt.drinks <- dt.drinks[measurement == "pieces", 
                       quantity_help_pieces := median(na.omit(quantity)), 
                       by = "glass_type"]

# add values for missing "" quantities
dt.drinks <- dt.drinks[measurement == "", 
                       quantity_help_SPACE := median(na.omit(quantity)), 
                       by = "glass_type"]

# Convert all quantity columns into one column
dt.drinks <- dt.drinks[, 
                       adj_quantity := ifelse(!is.na(quantity), quantity, 
                                              ifelse(!is.na(quantity_help_ml), quantity_help_ml, 
                                                     ifelse(!is.na(quantity_help_g), quantity_help_g, 
                                                            ifelse(!is.na(quantity_help_piece), quantity_help_piece, 
                                                                   ifelse(!is.na(quantity_help_pieces), quantity_help_pieces, 
                                                                          quantity_help_SPACE
                                                                          )
                                                                   )
                                                            )
                                                     )
                                              )
                       ]
# delete help quantity columns
dt.drinks <- dt.drinks[, 
                       c("quantity_help_ml", "quantity_help_g", "quantity_help_piece", "quantity_help_pieces", "quantity_help_SPACE") := NULL]

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

# select input button proposal paage 7

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
axis_vars <- c(
  "Recipe Complexity" = "complexity",
  "Commonality" = "commonality",
  "Ingredients Cost per Drink" = "adj_ingredients_cost"
)

################################### PAGE 6 PROPOSAL ##################################

# Calculate the datatable for advanced analysis
df.drink.degrees <- data.frame(degree(g.drinks.bp))
dt.drink.degrees <- data.table(cbind(row.names(df.drink.degrees), 
                                     df.drink.degrees))
colnames(dt.drink.degrees)[1:2] <- c("name", "degree")
dt.drink.degrees <- merge(dt.drink.degrees, 
                          dt.drinks, 
                          by = "name" )
dt.drink.degrees <- dt.drink.degrees[, 
                                     list(name, degree, complexity)]
dt.drink.degrees$log_complexity <- log(dt.drink.degrees$complexity)

################################### PAGE 7 PROPOSAL ##################################
# Calculate the datatable for advanced analysis
df.ingredients.degrees <- data.frame(degree(g.ingredients.bp))
dt.ingredients.degrees <- data.table(cbind(row.names(df.ingredients.degrees), 
                                           df.ingredients.degrees))
colnames(dt.ingredients.degrees)[1:2] <- c("ingredient", "degree")
dt.ingredients.degrees.merged <- merge(dt.ingredients.degrees, 
                                       dt.drinks, 
                                       by = "ingredient")
dt.ingredients.degrees.merged <- dt.ingredients.degrees.merged[, 
                                                               list(ingredient, degree, adj_ingredient_price)]
dt.ingredients.degrees.merged <- unique(dt.ingredients.degrees.merged)
dt.ingredients.degrees.merged$log_ingredient_price <- log(dt.ingredients.degrees.merged$adj_ingredient_price)

# add id for ingredients to enable tooltip feature
dt.ingredients.degrees.merged <- dt.ingredients.degrees.merged[, id := 1:length(ingredient)]

