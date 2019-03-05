# This script gives the output needed for the proposal for our cocktailparty :)

# If necessary, install the following libraries
# install.packages("data.table")
# install.packages("ggplot2")
# install.packages("reshape2")
# install.packages("stringr")

# Load the necessary libraries
library(data.table)
library(ggplot2)
library(stringr)
library(reshape2)
library(igraph)
# Set working directory to Collaborative Cocktail Party (change to your own if necessary!)
setwd("/Users/Harro/Dropbox/BIM - Master/Network Data Analytics/Group Project/cocktailparty")

# Load the data set, turn it into a data table, call it "drinks" and call first column "id"
dt.drinks <- fread("all_drinks.csv", header = TRUE)
setnames(dt.drinks, "V1", "id")

# Delete the empty columns
empty.columns <- c("strIngredient13", "strIngredient14", "strIngredient15", "strMeasure13", "strMeasure14", "strMeasure15")
dt.drinks <- dt.drinks[, -empty.columns, with = FALSE]

# Merge the strIngredient with the accompanying strMeasure and put a "_" in between
for (i in 1:12) {
  # In dt.drinks a (new) column called "ingredients.with.amount(1,2,3 etc.)" equals
  dt.drinks[, paste0("ingredients.with.amount", i)] <-
  # The ingredient and the amount of that ingredient separated by an underscore
  paste(dt.drinks[[paste0("strIngredient", i)]],dt.drinks[[paste0("strMeasure", i)]] , sep = "_")
}

# Delete the single "_" and "_\n" now in the data table
dt.drinks[dt.drinks == "_"] <- ""
dt.drinks[dt.drinks == "_\n"] <- ""

# Delete the original (24) columns called "strIngredient" and "strMeasure"
dt.drinks <- dt.drinks[, -grep("^strIngredient", colnames(dt.drinks)), with = FALSE]
dt.drinks <- dt.drinks[, -grep("^strMeasure", colnames(dt.drinks)), with = FALSE]

# Reshape data table to long so that every ingredient with matching amount ("ingredients.with.amount")
# is in a different row and call it "longdrinks"
dt.longdrinks <- melt(dt.drinks, measure.vars = patterns("^ingredients.with.amount"), value.name = "ingredients.with.amount", na.rm = TRUE)

# Remove the whitespace and empty dashes from the "ingredients.with.amount" column
dt.longdrinks[["ingredients.with.amount"]] <- str_replace_all(dt.longdrinks[["ingredients.with.amount"]], "[_]\\n", "")

# Remove pseudo-observations for which "ingredients.with.amount" is empty
dt.longdrinks <- dt.longdrinks[ingredients.with.amount != ""]

# Split the ingredients.with.amount column into two new columns called "ingredient" and "amount"
dt.longdrinks <- dt.longdrinks[, c("ingredient", "amount") := tstrsplit(ingredients.with.amount, "_", fixed=TRUE)]

#Delete unnessary variables
dt.longdrinks[, c("dateModified", "idDrink", "strVideo", "variable", "ingredients.with.amount"):= NULL]

# Change the remaining variable names to comply with the styleguide
new.column.names <- c("id", "name", "is_alcoholic", "category", "thumbnail", 
                      "glass_type", "era", "instructions", "ingredient", "amount")
setnames(dt.longdrinks, c(colnames(dt.longdrinks)), new.column.names)


# Save it into an RDS file loaded by global.R
saveRDS(dt.longdrinks, file = "./data/longdrinks.rds")

# Write cleaned dataset to csv, so you guys can work with it directly :)
# After comment it out so the whole script can be run without saving a million copies.
# write.csv(dt.longdrinks, file="clean_longdrinks.csv")

# Get some basic descriptives of the variables
# df.longdrinks <- as.data.frame(dt.longdrinks)
# View(df.longdrinks)

# Counts the number of unique values per column after converting to lowercase
# calculate.n.unique <- function(x) {
#  return(length(unique(tolower(x))))
#}

# sapply(dt.longdrinks, calculate.n.unique)



