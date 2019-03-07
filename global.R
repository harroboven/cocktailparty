# Global.R here

# Instruction that loads prepared data into memory
dt.longdrinks <- readRDS("./data/longdrinks.rds")
dt.drinks <- dt.longdrinks

# Add complexity of drink recipes
dt.drinks <- dt.drinks[, complexity := str_count(dt.drinks$instructions)]

# Add commonality of drinks
dt.commonality <- read.csv("./data/common_drinks.csv")
colnames(dt.commonality)[colnames(dt.commonality)=="x"] <- "name"
dt.drinks <- merge(dt.drinks, dt.commonality[, c("name", "common_final")], by = "name", all.x = TRUE)
colnames(dt.drinks)[colnames(dt.drinks)=="common_final"] <- "commonality"
dt.drinks$commonality <- as.factor(dt.drinks$commonality)

# Delete non-standardized columns from dt.drinks. They are still in dt.longdrinks, in case you need to access the original ones.
dt.drinks[, c("amount", "category", "unit", "instructions", "glass_type") := NULL]

# Rename some of the standardized columns the way the non-standardized were called for ease of use and backwards compatibility
setnames(dt.drinks, old = c("std.unit", "std.glass", "std.category"), new = c("unit", "glass_type", "category"))

# Delete thumbnail (not using it) and era (very few observations).
dt.drinks[, c("thumbnail", "era") := NULL]

# Making sure dt.drinks.filtered exhibits the changes as well //// just unique drinks????
dt.drinks.filtered <- dt.drinks[unique(id), ]

# ???
l.is_alcoholic_values <- unique(dt.drinks$is_alcoholic)

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



