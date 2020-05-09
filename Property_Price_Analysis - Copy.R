#install.packages("reshape2")

#Functions
nvl <- function(x, y) {
  if(is.na(x))
    return(y)
  else 
    return(x)
}

set_na_to_mean <- function(df) {
  cn <- ncol(df)
  rn <- nrow(df)
  for (i in 2:rn)
    for (j in 2:cn)
      if(is.na(df[i,j]))
        df[i,j] = mean(c(df[i-1,j],df[i+1,j]),na.rm = TRUE)
        
  return(df)
}

find_out_layers <- function(df) {
  outl_data <- property_price[0, c(3,6,6)]
  names(outl_data)[2] <- "Min_price_in_layer"
  names(outl_data)[3] <- "Max_price_in_layer"
  lopar <- par(no.readonly = TRUE)
  cn <- ncol(df)
  #cn <- 25
  par(mfrow = c(4, 4))
  par(mar=c(1,1,1,1))
  for (j in 2:cn){
    out_layers <- boxplot.stats(df[,j])$out
    boxplot(df[,j],
            main = names(df)[j],
            sub = paste(out_layers))
    if(length(out_layers) > 0)
      outl_data <- rbind(outl_data,
                         data.frame(County = names(df)[j],
                                    Min_price_in_layer = min(boxplot.stats(df[,j])$stats)-1,
                                    Max_price_in_layer = max(boxplot.stats(df[,j])$stats)+1))
  }
  par(lopar)
  return(outl_data)
}

#tmp <- data.frame(Sale_YearMonth = df1[which(df1[,2] %in% boxplot.stats(df1[,2])$out),1],
#           Final_Price = df1[which(df1[,2] %in% boxplot.stats(df1[,2])$out),2],
#           County = names(df1)[2])

#tmp <- data.frame(Final_Price = max(df1[which(df1[,2] < min(boxplot.stats(df1[,2])$out)-1),2]),
#           County = names(df1)[2])


df1 <- property_price_NMC
boxplot.stats(df1[,22])
subset(df1,select = c(22))

names(property_price_NMC)[22]

# Read CSV Property Price Register Ireland CSV to R dataframe
property_price1 <- read.csv("Property_Price_Register_Ireland.csv",na.strings=c("","NA"))
property_price <- property_price1
opar <- par(no.readonly = TRUE)
#Assign propery name to columns
names(property_price)[1] <- "Sale_date"
names(property_price)[3] <- "Postal_code"
names(property_price)[5] <- "Price"
names(property_price)[7] <- "Vat_Exclusive"
names(property_price)[8] <- "Property_type"

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

#Shortern property type
# First Convert Irish data in Propert description to english
Short_property_type <- as.character(Property_type)
Short_property_type[grepl("Teach.*Nua", Short_property_type)] <- "New Dwelling house /Apartment"
Short_property_type[grepl("Teach.*imhe", Short_property_type)] <- "Second-Hand Dwelling house /Apartment"

Short_property_type[Short_property_type == "New Dwelling house /Apartment" ] <- "New"
Short_property_type[Short_property_type == "Second-Hand Dwelling house /Apartment" ] <- "Second-Hand"
property_price$Property_type <- as.factor(Short_property_type)

#Add colum year of sale
property_price$Sale_YearMonth <- format(property_price$Sale_date,"%Y%m")

#library(mice)
#md.pattern(property_price,rotate.names = TRUE)

#library(VIM)
#missing_values <- aggr(property_price, prop = FALSE, numbers = TRUE)
#missing_values$missings$Percentage <- missing_values$missings$Count / nrow(property_price) * 100
#missing_values$missings

#Remove column Postal_code(3rd column) and Property.Size.Description(9th Column)
#which has more than 80% of values blank
#With new Final price colum, price column is reduntant and can be deleted
#Vat added for vat excluded records, so vat_exclusive column no more required
property_price <- property_price[-c(3, 5, 7, 9)]

detach(property_price)
str(property_price, strict.width = "cut")

property_price_new <- subset(property_price,Property_type == "New")
property_price_second <- subset(property_price,Property_type == "Second-Hand")

property_price_NM <- aggregate(x = property_price_new$Final_Price,
                               by=list(County = property_price_new$County,
                                       Sale_YearMonth = property_price_new$Sale_YearMonth),
                               FUN = mean)
property_price_SM <- aggregate(x = property_price_second$Final_Price,
                               by=list(County = property_price_second$County,
                                       Sale_YearMonth = property_price_second$Sale_YearMonth),
                               FUN = mean)

library("reshape2")
property_price_NMC <- dcast(property_price_NM, Sale_YearMonth ~ County)
property_price_SMC <- dcast(property_price_SM, Sale_YearMonth ~ County)

#library(VIM)
#missing_values <- aggr(property_price_NMC, prop = FALSE, numbers = TRUE)
#missing_values <- aggr(property_price_SMC, prop = FALSE, numbers = TRUE)

property_price_NMC <- set_na_to_mean(property_price_NMC)
property_price_SMC <- set_na_to_mean(property_price_SMC)

library(VIM)
missing_values <- aggr(property_price_NMC, prop = FALSE, numbers = TRUE)
missing_values <- aggr(property_price_SMC, prop = FALSE, numbers = TRUE)

NMC_out_layers <- find_out_layers(property_price_NMC)
SMC_out_layers <- find_out_layers(property_price_SMC)
NMC_out_layers

prop_price_new_out <- property_price_new[0,]
for(i in 1:nrow(NMC_out_layers)) {
  prop_price_new_out <- rbind(prop_price_new_out,
                              subset(property_price_new,
                                     as.character(County) == as.character(NMC_out_layers[i,1]) &
                                       Final_Price < NMC_out_layers[i,2] &
                                       Final_Price > NMC_out_layers[i,3]))
}

tmp <- subset(property_price_new,
              as.character(County) == "Carlow" &
                (Final_Price < 8045532 |
                Final_Price > 34145057))

tmp <- subset(property_price_new,
              as.character(County) == "Carlow")

boxplot.stats(tmp$Final_Price)
boxplot(tmp$Final_Price)

scatter.smooth(x = tmp$Sale_YearMonth, 
               y = tmp$Final_Price, 
               main = "Sale_YearMonth ~ Carlow",
               xlab = "Car speed",
               ylab = "Stopping distance")

prop_price_new_out

#tmp <- subset(property_price_new,Sale_YearMonth == 201110 & County == "Carlow")

tmp <- property_price_NMC[property_price_NMC[,2] < 52613493-1,c(1,2)]

scatter.smooth(x = property_price_NMC$Sale_YearMonth, 
               y = property_price_NMC$Carlow, 
               main = "Sale_YearMonth ~ Carlow",
               xlab = "Car speed",
               ylab = "Stopping distance")


library(e1071)
plot(density(property_price_NMC$Carlow), 
     main = "Density Plot: Speed",
     ylab = "Frequency",
     sub = paste("Skewness:"))
