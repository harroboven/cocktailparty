# This script gives the output needed for the proposal for our cocktailparty :)

# If necessary, install the following libraries
# install.packages("data.table")
# install.packages("ggplot2")
# install.packages("reshape2")
# install.packages("stringr")
# install.packages("shinyWidgets")
# install.packages("shinythemes")
# install.packages("ggvis")
# installed.packages("dplyr")
# install.packages("RSQLite")
# install.packages("dbplyr")



# Load the necessary libraries
library(data.table)
library(ggplot2)
library(stringr)
library(reshape2)
library(shinyWidgets)
library(igraph)
library(shinythemes)
library(ggvis)
library(dplyr)
library(RSQLite)
library(dbplyr)

# Set working directory to Collaborative Cocktail Party (change to your own if necessary!)
# setwd("/Users/Harro/Dropbox/BIM - Master/Network Data Analytics/Group Project/cocktailparty")

# Load the data set, turn it into a data table, call it "drinks" and call first column "id"
dt.drinks <- fread("data/all_drinks.csv", header = TRUE)
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

# Change is_alcoholic, ingredient and glass_type columns to lowercase
dt.longdrinks$is_alcoholic <- tolower(dt.longdrinks$is_alcoholic)
dt.longdrinks$ingredient <- tolower(dt.longdrinks$ingredient)
dt.longdrinks$glass_type <- tolower(dt.longdrinks$glass_type)

# Make two new columns called "quantity" and "unit" from the "amount" column
dt.longdrinks[, quantity := trimws(gsub("[A-Za-z]*", "", amount))]
dt.longdrinks[, unit := trimws(gsub("[0-9//.,-]*", "", amount))]

# Standardize the unit as much as possible
standard.units <- c("oz", "cl", "tsp", "ml", "shot", "dl", "pint", "kg", "lb", "cup", "tblsp", "dash", "part")

standardize.unit <- function(unit) {
  for (i in 1:length(standard.units)) {
    if (length(grep(standard.units[i], unit) > 0)) {
      return(standard.units[i])
    } 
  }
  return("")
}

dt.longdrinks$std.unit <- mapply(standardize.unit, dt.longdrinks$unit)

# Make a new column called "std.category" in which to reduce "category" to "cocktail", "shot" and "other"
nonstandard.categories <- unique(dt.longdrinks[, category])
standard.categories <- c("cocktail", "shot", "other")

standardize.category <- function(category) {
  for (i in 1:length(nonstandard.categories)) {
    if (category == nonstandard.categories[1] | category == nonstandard.categories[5] | category == nonstandard.categories[8]) {
      return(standard.categories[1])
    } else if ( category == nonstandard.categories[2]) {
      return(standard.categories[2])
    } else {
      return(standard.categories[3])
    }
  }
}

dt.longdrinks$std.category <- mapply(standardize.category, dt.longdrinks$category)

# Make a new column called "std.glasstype" in which to reduce "glasstype"
nonstandard.glasses <- unique(dt.longdrinks[, glass_type])

long.glasses <- c("highball glass", "collins glass")
short.glasses <- c("old-fashioned glass", "punch bowl")
chalice.glasses <- c("cocktail glass", "champagne flute", "margarita/coupette glass", "martini glass", "whiskey sour glass", "white wine glass", "wine glass", "brandy snifter")
shot.glasses <- c("shot glass")

standard.glasses <- list(long.glasses, short.glasses, chalice.glasses, shot.glasses)
names(standard.glasses) <- c("long.glasses", "short.glasses", "chalice.glasses", "shot.glasses")

standardize.glass <- function(glass) {
  for (i in 1:length(standard.glasses)) {
    if (glass %in% standard.glasses[[i]]) {
      return(names(standard.glasses)[i])
    } 
  }
  return("other")
}

dt.longdrinks$std.glass <- mapply(standardize.glass, dt.longdrinks$glass_type)

# Change is_alcoholic column to "alcoholic" and "non-alcoholic" and indicate that Cherry Electric Lemonade is indeed alcoholic
ditch.optional <- function(x) {
  if (x == "") {
    return("alcoholic")
  }
  gsub("optional alcohol", "non alcoholic", x)
}
dt.longdrinks$is_alcoholic <- mapply(ditch.optional, dt.longdrinks$is_alcoholic)

# Make the "quantity" column numeric, delete fractions
kill.fraction <- function(quantity) {
  if (length(grep("/", quantity)) == 1) {
    slash.index <- as.numeric(gregexpr("/", quantity))
    numerator <- as.numeric(substring(quantity, slash.index - 1, slash.index - 1))
    print(numerator)
    denominator <- as.numeric(substring(quantity, slash.index + 1, slash.index + 1)) 
    decimal <- as.numeric(numerator) / as.numeric(denominator)
    sub("\\s{0,1}./.", substring(toString(decimal), 2), quantity)
  } else {
    return(quantity)
  }
}

dt.longdrinks$quantity <- mapply(kill.fraction, dt.longdrinks$quantity)

# Make "quantity" column numeric, delete dashes
kill.dashes <- function(quantity) {
  if (length(grep("-", quantity)) == 1) {
    slash.index <- as.numeric(gregexpr("-", quantity))
    numerator <- as.numeric(substring(quantity, slash.index - 1, slash.index - 1))
    print(numerator)
    denominator <- as.numeric(substring(quantity, slash.index + 1, slash.index + 1)) 
    decimal <- as.numeric(numerator) / as.numeric(denominator)
    sub("\\s{0,1}[0-9]-[0-9]", substring(toString(decimal), 2), quantity)
  } else {
    return(quantity)
  }
}

dt.longdrinks$quantity <- mapply(kill.dashes, dt.longdrinks$quantity)

# Make "quantity" column numeric, delete the rest of the crap
dt.longdrinks$quantity <- gsub("-", "", dt.longdrinks$quantity)
dt.longdrinks$quantity <- gsub("[(]", "", dt.longdrinks$quantity)
dt.longdrinks$quantity <- gsub("[)]", "", dt.longdrinks$quantity)
dt.longdrinks$quantity <- gsub("[,]", "", dt.longdrinks$quantity)
dt.longdrinks$quantity <- gsub("[0-9]\\s{1,5}.", "", dt.longdrinks$quantity)
dt.longdrinks$quantity <- trimws(dt.longdrinks$quantity)

# Ditch remaining things that cannot be converted to numeric and convert column to numeric
dt.longdrinks$quantity <- mapply(as.numeric, dt.longdrinks$quantity)

# Load the other datasets with the pricing data and commonality data
dt.commonality <- fread("data/drinks_common.csv", header = TRUE)
dt.pricing <- fread("data/ingredients_overview_updated.csv", header = TRUE)

# Identify double ingredients and prepare help column to work with them later on
dt.pricing <- dt.pricing[, adj_ingredient := ifelse(double_observation == "", Ingredient, double_observation), by = "Ingredient"]


# Remove observation marked as duplicates dt.commonality (may create orphaned ingredients, sth to look out for).
# Will leave pricing for later because for drinks to be correct, duplicate ingredients will need to be replaced by their synonym.
dt.commonality <- dt.commonality[is.na(double_Felix), ]
dt.commonality <- dt.commonality[is.na(DOUBLE_zanis), ]

# Merge dt.commonality with dt.longdrinks for commonality data (assuming drink names are unique)
setnames(dt.commonality, old = "x", new = "name")
dt.commonality <- dt.commonality[, c("name", "common_final")]
dt.commonality.longdrinks <- merge(dt.longdrinks, dt.commonality, by = "name")

# Merge dt.pricing with dt.commonality.longdrinks for pricing data (assuming the ingredient is unique)
dt.pricing <- dt.pricing[, c("Ingredient", "adj_ingredient", "measurement", "package size", "price")]
setnames(dt.pricing, old = c("Ingredient", "package size"), new = c("ingredient", "package_size"))
dt.commonality.longdrinks.pricing <- merge(dt.commonality.longdrinks, dt.pricing, by = "ingredient")

# Replace "ingredient" column by "adj_ingredient" column
dt.commonality.longdrinks.pricing <- dt.commonality.longdrinks.pricing[, ingredient := NULL]
setnames(dt.commonality.longdrinks.pricing, old = "adj_ingredient", new = "ingredient")

# Turn this rather long and ugly name back into dt.longdrinks for convenience and backwards compatibility
dt.longdrinks <- dt.commonality.longdrinks.pricing

# Convert std.units to "g" and "ml"
unit.conversion.factor <- c(29.6, 10, 5, 1, 44, 100, 473, 1000, 454, 237, 14)

standardize.quantity <- function(drink, quantity, std.unit) {
  if ("part" %in% dt.longdrinks[drink == name, std.unit]) {
    return(1/length(dt.longdrinks[drink == name, std.unit]))
  } else {
    for (i in 1:length(standard.units)) {
      if (std.unit %in% standard.units[[i]]) {
        return(unit.conversion.factor[i] * quantity)
      } 
    }
    return(NA)
  }
}

dt.longdrinks$std.quantity <- mapply(standardize.quantity, dt.longdrinks$name, dt.longdrinks$quantity, dt.longdrinks$std.unit)

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

# sapply(dt.longdrinks, calculate.n.unique)/


