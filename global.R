# Global.R here

# Instruction that loads prepared data into memory
drinks <- readRDS("./data/longdrinks.rds")
drinks.filtered <- drinks[unique(id), ]
