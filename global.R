# Global.R here

# Instruction that loads prepared data into memory
dt.drinks <- readRDS("./data/longdrinks.rds")
dt.drinks.filtered <- drinks[unique(id), ]
l.is_alcoholic_values <- unique(drinks$is_alcoholic)
