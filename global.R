# Global.R here

# Instruction that loads prepared data into memory
dt.drinks <- readRDS("./data/longdrinks.rds")

# Create column that states the complexity of the drink recipe
dt.drinks <- dt.drinks[, complexity := str_count(dt.drinks$instructions)]

# ???
dt.drinks.filtered <- dt.drinks[unique(id), ]

# ???
l.is_alcoholic_values <- unique(dt.drinks$is_alcoholic)





