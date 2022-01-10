install.packages("pastecs")
library(pastecs)
library(ggplot2)
library(data.table)

housing <- read_excel('week-7-housing.xlsx')

# Use the apply function on a variable in your dataset
Sale_Price_List <- list(housing$`Sale Price`)
lapply(Sale_Price_List, nchar)

# Use the aggregate function on a variable in your dataset
colnames(housing)[colnames(housing) == "Sale Price"] <- "Sale_Price"
aggregate(Sale_Price ~ zip5, housing, mean)

# Use the plyr function on a variable in your dataset – more specifically, I want to see you split some data,
# perform a modification to the data, and then bring it back together
library(plyr)
any(is.na(housing$square_feet_total_living))
any(is.na(housing$sq_ft_lot))
#   verified if there are any NA entries, it returned FALSE
total_living_meters <- with(housing, square_feet_total_living * 0.3048)
#   performed calculation to convert feet to meters
lot_to_living <- function(housing) {with(housing, sq_ft_lot / square_feet_total_living)}
lot_to_living <- function(housing) {housing$sq_ft_lot / housing$square_feet_total_living}
lot_to_living <- function(housing) { c(lot_to_living = with(housing, sq_ft_lot / square_feet_total_living)) }
prop_living_space <- ddply(housing, .variables = "Sale_Price", .fun = lot_to_living())

# Check distributions of the data
hist_sale_price <- ggplot(housing, aes(Sale_Price)) + geom_histogram(aes(y = ..density..), fill = "white", color = "black", binwidth = 45) + labs(title = "Housing Prices", x = "Sale Price", y = "Density")

# Identify if there are any outliers
#   This histogram shows a strong positive skew, with outliers on the high end of the price range


# Create at least 2 new variables
affordability <- ifelse(housing$Sale_Price < 250000, "AFFORDABLE", "TOO EXPENSIVE")

three_plus_bed <- ifelse(housing$bedrooms >= 3, "FAMILY SIZE", "FUN SIZE")
