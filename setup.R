
# Load the necessary libraries
library(data.table)
library(ggplot2)
library(stringr)
library(reshape2)
library(shinyWidgets)
library(igraph, quietly = TRUE, warn.conflicts = FALSE, verbose = FALSE)
library(shinythemes)
library(ggvis)
library(dplyr)
library(RSQLite)
library(dbplyr)
library(visNetwork, quietly = TRUE)

# This script gives the output needed for the proposal for our cocktailparty :)

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

# Delete category homemade liquer & all punch recipes because they are prepared in much larger quantities (up to >21l)
dt.longdrinks <- dt.longdrinks[category != "Homemade Liqueur", ]
dt.longdrinks <- dt.longdrinks[category != "Punch / Party Drink", ]

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

# Make "quantity" column numeric, delete the rest
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

dt.pricing <- fread("data/ingredients_overview_updated4.csv", header = TRUE)

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


dt.drinks <- dt.longdrinks

########### Additional covariates ###############
# Add complexity of drink recipes
dt.drinks <- dt.drinks[, 
                       complexity := str_count(dt.drinks$instructions)]

# Delete observations with missing or fraudulent complexity data
dt.drinks <- dt.drinks[complexity > 0 & complexity < 550,  ]

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

# Adjust quantities of ingredients used that are provided in percentage
dt.drinks <- dt.drinks[ingredient != "nutmeg" & ingredient != "cinnamon" & quantity < 1 & glass_type == "shot.glasses" & measurement != "piece" & measurement != "pieces", adj_quantity1 := quantity * 44, by = "ingredient"]
dt.drinks <- dt.drinks[ingredient != "nutmeg" & ingredient != "cinnamon" & quantity < 1 & glass_type == "short.glasses" & measurement != "piece" & measurement != "pieces", adj_quantity2 := quantity * 120, by = "ingredient"]
dt.drinks <- dt.drinks[ingredient != "nutmeg" & ingredient != "cinnamon" & quantity < 1 & glass_type == "chalice.glasses" & measurement != "piece" & measurement != "pieces", adj_quantity3 := quantity * 330, by = "ingredient"]
dt.drinks <- dt.drinks[ingredient != "nutmeg" & ingredient != "cinnamon" & quantity < 1 & glass_type == "long.glasses" & measurement != "piece" & measurement != "pieces", adj_quantity4 := quantity * 340, by = "ingredient"]
dt.drinks <- dt.drinks[ingredient != "nutmeg" & ingredient != "cinnamon" & quantity < 1 & glass_type == "other" & measurement != "piece" & measurement != "pieces", adj_quantity5 := quantity * 340, by = "ingredient"]
dt.drinks <- dt.drinks[, adj_quantity6 := ifelse(!is.na(adj_quantity1), adj_quantity1,
                                                 ifelse(!is.na(adj_quantity2), adj_quantity2,
                                                        ifelse(!is.na(adj_quantity3), adj_quantity3,
                                                               ifelse(!is.na(adj_quantity4), adj_quantity4,
                                                                      ifelse(!is.na(adj_quantity5), adj_quantity5,
                                                                             NA
                                                                      )
                                                               )
                                                        )
                                                 )
)
]

############ add values for missing quantities ##############
# add values for missing ml quantities
dt.drinks <- dt.drinks[measurement == "ml", 
                       quantity_help_ml := median(na.omit(quantity)), 
                       by = "glass_type"]

# add values for missing g quantities
dt.drinks <- dt.drinks[measurement == "g", quantity_help_g := 2.5]

# add values for missing "" quantities
dt.drinks <- dt.drinks[measurement == "", 
                       quantity_help_SPACE := median(na.omit(quantity)), 
                       by = "glass_type"]

dt.drinks <- dt.drinks[, adj_quantity7 := ifelse(measurement == "piece", 1, NA)]
dt.drinks <- dt.drinks[, adj_quantity8 := ifelse(measurement == "pieces", 1, NA)]
dt.drinks <- dt.drinks[, adj_quantity9 := ifelse(!is.na(adj_quantity7), adj_quantity7, 
                                                 ifelse(!is.na(adj_quantity8), adj_quantity8, 
                                                        NA
                                                 )
)
]

# Convert all quantity columns into one column
dt.drinks <- dt.drinks[, adj_quantity := ifelse(!is.na(adj_quantity6), adj_quantity6, 
                                                ifelse(!is.na(adj_quantity9), adj_quantity9, 
                                                       ifelse(!is.na(quantity), quantity, 
                                                              ifelse(!is.na(quantity_help_ml), quantity_help_ml, 
                                                                     ifelse(!is.na(quantity_help_g), quantity_help_g, 
                                                                            quantity_help_SPACE
                                                                     )
                                                              )
                                                       )
                                                )
)
]

# delete help quantity columns
dt.drinks <- dt.drinks[, 
                       c("adj_quantity1", 
                         "adj_quantity2", 
                         "adj_quantity3", 
                         "adj_quantity4", 
                         "adj_quantity5", 
                         "adj_quantity6", 
                         "adj_quantity7", 
                         "adj_quantity8", 
                         "adj_quantity9", 
                         "quantity_help_ml", 
                         "quantity_help_g", 
                         "quantity_help_piece", 
                         "quantity_help_pieces", 
                         "quantity_help_SPACE") := NULL]

# delete help quantity columns
dt.drinks <- dt.drinks[, 
                       c("adj_quantity1", "adj_quantity2", "adj_quantity3", "adj_quantity4", "adj_quantity5", "adj_quantity6", "adj_quantity7", "adj_quantity8", "adj_quantity9", "quantity_help_ml", "quantity_help_g", "quantity_help_piece", "quantity_help_pieces", "quantity_help_SPACE") := NULL]


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


# Save it into an RDS file loaded by global.R
saveRDS(dt.drinks, file = "./data/drinks.rds")

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


