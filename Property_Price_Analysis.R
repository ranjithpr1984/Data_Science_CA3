#install.packages("reshape2")

opar <- par(no.readonly = TRUE)

#Functions
nvl <- function(x, y) {
  if(is.na(x))
    return(y)
  else 
    return(x)
}

num_to_date <- function(n_dt) {
  return(as.Date(tmp1,origin = "1970-01-01"))
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
  
  lopar <- par(no.readonly = TRUE)
  par(mfrow = c(3, 3))
  par(mar=c(1,1,1,1))
  
  out_layers <- df[0,]
  
  for(county_name in unique(df$County)) {
    county_price <- df[df$County == county_name,6]
    out_layer_stats <- boxplot.stats(county_price)$stats
    boxplot(county_price, main = county_name)
    out_layers <- rbind(out_layers,
                        subset(df,
                               County == county_name &
                                 (Final_Price < min(out_layer_stats) |
                                    Final_Price > max(out_layer_stats))))
  }
  par(lopar)
  return(out_layers)
}

remve_out_layers <- function(df) {
  
  in_layers <- df[0,]
  
  for(county_name in unique(df$County)) {
    county_price <- df[df$County == county_name,6]
    out_layer_stats <- boxplot.stats(county_price)$stats
    in_layers <- rbind(in_layers,
                       subset(df,
                             County == county_name &
                                Final_Price >= min(out_layer_stats) &
                               Final_Price <= max(out_layer_stats)))
  }
  return(in_layers)
}

display_skewness <- function(df) {
  
  library(e1071)
  library(crayon)
  
  lopar <- par(no.readonly = TRUE)
  par(mfrow = c(4, 4))
  par(mar=c(1,1,1,1))
  
  for(j in 2:ncol(df)) {
    county_skewness <- round(e1071::skewness(df[,j]), 2)
    if(abs(county_skewness) <= 0.5)
      cat(green(paste("Skewness of ", names(df)[j], " : ", county_skewness,"\n")))
    else if(abs(county_skewness) <= 1)
      cat(yellow(paste("Skewness of ", names(df)[j], " : ", county_skewness,"\n")))
    else
      cat(red(paste("Skewness of ", names(df)[j], " : ", county_skewness,"\n")))
    
    plot(density(df[,j]), main = paste("Density Plot: ",names(df)[j]), 
         ylab = "Frequency", 
         sub = paste("Skewness:",county_skewness))

    polygon(density(df[,j]), col = "red")
  }
  par(lopar)
}

display_normality <- function(df) {
  
  lopar <- par(no.readonly = TRUE)
  par(mfrow = c(4, 4))
  par(mar=c(2,1,1,1))
  
  for(j in 2:ncol(df)) {
    qqnorm(df[,j],main = names(df)[j])
    qqline(df[,j])
  }
  par(lopar)
}

find_correlation <- function(df) {
  lopar <- par(no.readonly = TRUE)
  par(mfrow = c(4, 4))
  par(mar=c(2,1,1,1))
  
  county_corr <- property_price[0, c(3,6)]
  names(county_corr)[2] <- "Correlation"
  
  for(j in 2:ncol(df)) {
    county_corr[j-1,1] <- names(df)[j]
    county_corr[j-1,2] <- round(cor(df[,1]
                                    ,df[,j]),2)
    scatter.smooth(x = df[,1], 
                   y = df[,j], 
                   main = names(df)[j],
                   xlab = "Month")
  }
  par(lopar)
  return(county_corr)
}

par(opar)
# Read CSV Property Price Register Ireland CSV to R dataframe
property_price <- read.csv("Property_Price_Register_Ireland.csv",na.strings=c("","NA"))

#Assign propery name to columns
names(property_price)[1] <- "Sale_date"
names(property_price)[3] <- "Postal_code"
names(property_price)[5] <- "Price"
names(property_price)[7] <- "Vat_Exclusive"
names(property_price)[8] <- "Property_type"
names(property_price)[9] <- "Size_desc"

str(property_price, strict.width = "cut")

library(VIM)
missing_values <- aggr(property_price, prop = FALSE, numbers = TRUE,cex.axis = .8)
missing_values$missings$Percentage <- missing_values$missings$Count / nrow(property_price) * 100
missing_values$missings

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

unique(Short_property_type)

Short_property_type[Short_property_type == "New Dwelling house /Apartment" ] <- "New"
Short_property_type[Short_property_type == "Second-Hand Dwelling house /Apartment" ] <- "Second-Hand"
property_price$Property_type <- as.factor(Short_property_type)

#library(mice)
#md.pattern(property_price,rotate.names = TRUE)

#Remove column Postal_code(3rd column) and Property.Size.Description(9th Column)
#which has more than 80% of values blank
#With new Final price colum, price column is reduntant and can be deleted
#Vat added for vat excluded records, so vat_exclusive column no more required
property_price <- property_price[-c(3, 5, 7, 9)]

detach(property_price)
str(property_price, strict.width = "cut")

#Add colum Month(and year) of sale
property_price$Sale_YearMonth <- format(property_price$Sale_date,"%Y%m")

prop_price_new <- subset(property_price,Property_type == "New")
prop_price_second <- subset(property_price,Property_type == "Second-Hand")

prop_price_new_out <- find_out_layers(prop_price_new)
prop_price_second_out <- find_out_layers(prop_price_second)

nrow(prop_price_new)
nrow(prop_price_new_out)

nrow(prop_price_second)
nrow(prop_price_second_out)

prop_price_new <- remve_out_layers(prop_price_new)
prop_price_second <- remve_out_layers(prop_price_second)

nrow(prop_price_new) + nrow(prop_price_new_out)
nrow(prop_price_second) + nrow(prop_price_second_out)

nrow(prop_price_new) / nrow(prop_price_new_out)
nrow(prop_price_second) / nrow(prop_price_second_out)

prop_price_NM <- aggregate(x = prop_price_new$Final_Price,
                           by=list(County = prop_price_new$County,
                                   Sale_YearMonth = prop_price_new$Sale_YearMonth),
                           FUN = mean)
prop_price_SM <- aggregate(x = prop_price_second$Final_Price,
                           by=list(County = prop_price_second$County,
                                   Sale_YearMonth = prop_price_second$Sale_YearMonth),
                           FUN = mean)

#library("reshape2")
prop_price_NMC <- reshape2::dcast(prop_price_NM, Sale_YearMonth ~ County)
prop_price_SMC <- reshape2::dcast(prop_price_SM, Sale_YearMonth ~ County)

str(prop_price_NMC)
str(prop_price_SMC)

library(VIM)
missing_values <- aggr(prop_price_NMC, prop = FALSE, numbers = TRUE,cex.axis = .8)
missing_values
missing_values <- aggr(prop_price_SMC, prop = FALSE, numbers = TRUE,cex.axis = .8)
missing_values

prop_price_NMC <- set_na_to_mean(prop_price_NMC)
prop_price_SMC <- set_na_to_mean(prop_price_SMC)

missing_values <- aggr(prop_price_NMC, prop = FALSE, numbers = TRUE)
missing_values <- aggr(prop_price_SMC, prop = FALSE, numbers = TRUE)

display_skewness(prop_price_NMC)
display_skewness(prop_price_SMC)

par(opar)
display_normality(prop_price_NMC)
display_normality(prop_price_SMC)

prop_price_NMC$Sale_YearMonth <- as.numeric(as.Date(paste(as.character(prop_price_NMC$Sale_YearMonth),"01"),"%Y%m%d"))
prop_price_SMC$Sale_YearMonth <- as.numeric(as.Date(paste(as.character(prop_price_SMC$Sale_YearMonth),"01"),"%Y%m%d"))
#pairs(prop_price_NMC[,1:5])

NMC_corr <- find_correlation(prop_price_NMC)
SMC_corr <- find_correlation(prop_price_SMC)

# Compare county time to price correlation
par(mfrow = c(1, 2))
colr <- numeric(length(NMC_corr$Correlation))
colr[NMC_corr$Correlation < 0 ] <- 2
colr[NMC_corr$Correlation > 0 ] <- 3
barplot(abs(Correlation) ~ County,
        data = NMC_corr,
        main = "County time to price correlation - New",
        xlab = "",
        ylab = "",
        las = 2,
        col=colr)
mtext(side=1, line=4, "County", font=2)
mtext(side=2, line=3, "Correlation", font=2)

colr <- numeric(length(SMC_corr$Correlation))
colr[SMC_corr$Correlation < 0 ] <- 2
colr[SMC_corr$Correlation > 0 ] <- 3
barplot(abs(Correlation) ~ County,
        data = SMC_corr,
        main = "County time to price correlation - Second hand",
        xlab = "",
        ylab = "",
        las = 2,
        col=colr)

mtext(side=1, line=4, "County", font=2)
mtext(side=2, line=3, "Correlation", font=2)

par(opar)

#normality_test <- shapiro.test(prop_price_NMC$Dublin)
#normality_test$p.value
#qqnorm(prop_price_NMC$Dublin)
#qqline(prop_price_NMC$Dublin)

normality_test_out <- lapply(prop_price_NMC[,2:27], shapiro.test)

normality_test_df <- data.frame(names(normality_test_out),
                                matrix(unlist(normality_test_out),
                                       nrow=length(normality_test_out),
                                       byrow=T)[,1:2])
names(normality_test_df)[1] <- "County"
names(normality_test_df)[2] <- "W"
names(normality_test_df)[3] <- "pvalue"

normality_test_df$W <- round(as.numeric(as.character(normality_test_df$W)),2)
normality_test_df$pvalue <- round(as.numeric(as.character(normality_test_df$pvalue)),2)

barplot(pvalue ~ County,
        data = normality_test_df,
        main = "Normality test p-value",
        xlab = "",
        ylab = "",
        las = 2,
        col=3)

for(i in 1:nrow(normality_test_df)) {
  if(normality_test_df[i,3] >= .05)
    test_result <- t.test(prop_price_NMC[,which(colnames(prop_price_NMC)==normality_test_df[i,1])],
                          prop_price_SMC[,which(colnames(prop_price_SMC)==normality_test_df[i,1])])
  else
    test_result <- wilcox.test(prop_price_NMC[,which(colnames(prop_price_NMC)==normality_test_df[i,1])],
                               prop_price_SMC[,which(colnames(prop_price_SMC)==normality_test_df[i,1])],
                               paired=TRUE)
  if(test_result$p.value < 0.05)
    print(paste("For ", normality_test_df[i,1],"null hypothesis can be rejected"))
  if(test_result$p.value >= 0.05)
    print(paste("For ", normality_test_df[i,1],"alternative hypothesis can be rejected"))
}

NMC_corr$pwr <- pwr.r.test(r=NMC_corr$Correlation, n=124)$power
SMC_corr$pwr <- pwr.r.test(r=SMC_corr$Correlation, n=124)$power

#test_result <- wilcox.test(prop_price_NMC$Dublin, prop_price_SMC$Dublin, paired=TRUE)
#test_result <- wilcox.test(prop_price_NMC$Tipperary, prop_price_SMC$Tipperary, paired=TRUE)


#scatter.smooth(x = prop_price_NMC$Sale_YearMonth, 
#               y = prop_price_NMC$Tipperary, 
#               main = "Tipperary",
#               xlab = "Month")
#qqnorm(prop_price_NMC$Tipperary)
#qqline(prop_price_NMC$Tipperary)
#
#plot(density(prop_price_NMC$Tipperary))
#polygon(density(prop_price_NMC$Tipperary), col = "red")#


#View(prop_price_new)
#pwr.r.test(r = 0.12	, power = .8 )
#tmp <- subset(prop_price_new, County = "Cavan",select = c(1,6))
#str(tmp)
#tmp$Sale_date <- as.numeric(tmp$Sale_date)
#cor(tmp$Sale_date,tmp$Final_Price)
