# Global.R here

########## Data loading ###########
# Instruction that loads prepared data into memory
dt.longdrinks <- readRDS("./data/longdrinks.rds")
dt.drinks <- dt.longdrinks

########### Additional covariates ###############
# Add complexity of drink recipes
dt.drinks <- dt.drinks[, complexity := str_count(dt.drinks$instructions)]

# Add commonality of drinks
dt.commonality <- read.csv("./data/common_drinks.csv")
colnames(dt.commonality)[colnames(dt.commonality)=="x"] <- "name"
dt.drinks <- merge(dt.drinks, dt.commonality[, c("name", "common_final")], by = "name")
colnames(dt.drinks)[colnames(dt.drinks) == "common_final.y"] <- "commonality"
dt.drinks$commonality <- as.factor(dt.drinks$commonality)

########### Data cleaning ##############
# Delete non-standardized columns from dt.drinks. They are still in dt.longdrinks, in case you need to access the original ones.
dt.drinks[, c("amount", "category", "unit", "instructions", "glass_type") := NULL]

# Rename some of the standardized columns the way the non-standardized were called for ease of use and backwards compatibility
setnames(dt.drinks, old = c("std.unit", "std.glass", "std.category"), new = c("unit", "glass_type", "category"))

# Delete thumbnail (not using it) and era (very few observations).
dt.drinks[, c("thumbnail", "era") := NULL]

######## Buttons ##########
# preparing dt.drinks for button work by ensuring that only unique drinks
dt.drinks.filtered <- unique(dt.drinks, by = "id")

# select input button proposal paage 2
l.is_alcoholic_values <- unique(dt.drinks.filtered$is_alcoholic)
l.category_values <- unique(dt.drinks.filtered$category)
l.glass_type_values <- unique(dt.drinks.filtered$glass_type)



