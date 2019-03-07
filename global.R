# Global.R here

# Instruction that loads prepared data into memory
dt.longdrinks <- readRDS("./data/longdrinks.rds")
dt.drinks <- dt.longdrinks

# Add complexity of drink recipes
dt.drinks <- dt.drinks[, complexity := str_count(dt.drinks$instructions)]

# Delete non-standardized columns from dt.drinks. They are still in dt.longdrinks, in case you need to access the original ones.
dt.drinks[, c("amount", "category", "unit", "instructions", "glass_type") := NULL]

# Rename some of the standardized columns the way the non-standardized were called for ease of use and backwards compatibility
setnames(dt.drinks, old = c("std.unit", "std.glass", "std.category"), new = c("unit", "glass_type", "category"))

# Delete thumbnail (not using it) and era (very few observations).
dt.drinks[, c("thumbnail", "era") := NULL]

# Making sure dt.drinks.filtered exhibits the changes as well
dt.drinks.filtered <- dt.drinks[unique(id), ]

# ???
l.is_alcoholic_values <- unique(dt.drinks$is_alcoholic)





