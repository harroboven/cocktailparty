# Global.R here

########## Data loading ###########
# Instruction that loads prepared data into memory
dt.longdrinks <- readRDS("./data/longdrinks.rds")
dt.drinks <- dt.longdrinks

########### Additional covariates ###############
# Add complexity of drink recipes
dt.drinks <- dt.drinks[, complexity := str_count(dt.drinks$instructions)]

# change name of column for better understanding
setnames(dt.drinks, old = "price", new = "ingredient_price")

########### Data cleaning ##############
# Delete non-standardized columns from dt.drinks. They are still in dt.longdrinks, in case you need to access the original ones.
dt.drinks[, c("amount", "category", "unit", "instructions", "glass_type") := NULL]

# Rename some of the standardized columns the way the non-standardized were called for ease of use and backwards compatibility
setnames(dt.drinks, old = c("std.unit", "std.glass", "std.category", "common_final"), new = c("unit", "glass_type", "category", "commonality"))

# Delete thumbnail (not using it) and era (very few observations).
dt.drinks[, c("thumbnail", "era") := NULL]

######## Buttons ##########
# preparing dt.drinks for button work by ensuring that only unique drinks
dt.drinks.filtered <- unique(dt.drinks, by = "id")

# select input button proposal paage 2
l.is_alcoholic_values <- c("All", sort(unique(dt.drinks.filtered$is_alcoholic), decreasing = FALSE)) 

l.category_values <- c("All", sort(unique(dt.drinks.filtered$category), decreasing = FALSE))

l.glass_type_values <- c("All", sort(unique(dt.drinks.filtered$glass_type), decreasing = FALSE))


############## Plot themes #################

chart.theme.1 <- theme(plot.title = element_text(family = "Helvetica", face = "bold", size = (15)), 
                       legend.title = element_text(colour = "steelblue",  face = "bold.italic", family = "Helvetica"), 
                       legend.text = element_text(face = "italic", colour="steelblue4",family = "Helvetica"), 
                       axis.title = element_text(family = "Helvetica", size = (10), colour = "steelblue4"),
                       axis.text = element_text(family = "Courier", colour = "cornflowerblue", size = (10)))

############# Graphs ################
# create constant objects - always stay the same
all.drinks <- dt.drinks[, .(name = unique(name), type = TRUE)]
all.ingredients <- dt.drinks[, .(name = unique(ingredient), type = FALSE)]
all.vertices <- rbind(all.drinks, all.ingredients)
#all.vertices <- all.vertices[!duplicated(all.vertices$id)]


g.drinks.ingredients <- graph_from_data_frame(dt.drinks[, .(name, ingredient)],
                                              directed = FALSE,
                                              vertices = all.vertices)
g.drinks.bp <- bipartite.projection(g.drinks.ingredients)$proj2
g.ingredients.bp <- bipartite.projection(g.drinks.ingredients)$proj1

# Variables that can be put on the x and y axes
axis_vars <- c(
  "Recipe Complexity" = "complexity",
  "Commonality" = "commonality",
  "Ingredient price" = "ingredient_price"
)



