# Read CSV Property Price Register Ireland CSV to R dataframe
property_price <- read.csv("Property_Price_Register_Ireland.csv",na.strings=c("","NA"))

#Assign propery name to columns
names(property_price)[1] <- "Sale_date"
names(property_price)[3] <- "Postal_code"
names(property_price)[5] <- "Price"
names(property_price)[7] <- "Vat_Exclusive"
names(property_price)[8] <- "Property_description"

str(property_price, strict.width = "cut")

attach(property_price)

#Change price to number
property_price$Price <- as.numeric(as.character(gsub("€| |,","",Price)))

# Add column Final_price by including vat for vat excluded records
Vat <- vector(mode="numeric", length=nrow(property_price))
Vat[Vat_Exclusive == "Yes"]  <- 113.5
Vat[Vat_Exclusive == "No"]  <- 1
property_price$Final_Price <- property_price$Price * Vat


#Change column Not full market price to Is_full_market_price
Is_full_market_price <- Not.Full.Market.Price
Is_full_market_price[Not.Full.Market.Price == "Yes"] <- "No"
Is_full_market_price[Not.Full.Market.Price == "No"] <- "Yes"
unique(Is_full_market_price)
property_price$Not.Full.Market.Price <- Is_full_market_price
names(property_price)[6] <- "Is_full_market_price"

# Change type column Sale_date to Date
property_price$Sale_date <- as.Date(as.character(Sale_date), "%d/%m/%Y")
str(property_price)

# Convert Irish data in Propert description to english
property_price$Property_description[grepl("Teach.*Nua", Property_description)] <- "New Dwelling house /Apartment"
property_price$Property_description[grepl("Teach.*imhe", Property_description)] <- "New Dwelling house /Apartment"

unique(property_price$Property_description)

#Add column property type (New/Second-Hand)
property_price$Property_type <- as.factor(sub(" .*$","",Property_description))

library(mice)
md.pattern(property_price,rotate.names = TRUE)

library(VIM)
missing_values <- aggr(property_price, prop = FALSE, numbers = TRUE)
missing_values$missings$Percentage <- missing_values$missings$Count / nrow(property_price) * 100
missing_values$missings

#Remove column Postal_code(3rd column) and Property.Size.Description(9th Column)
#which has more than 80% of values blank
property_price <- property_price[-c(3, 9)]

#detach(property_price)
