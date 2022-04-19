# Final Project
# Yiming Ge


library(readxl)
library(plyr)
#combine property sales from 2016 - 2019
#filenames <- list.files(path = "~/Desktop/Final Project", pattern = "*", full.names=TRUE)
#rolling_sales <- ldply(filenames, read_xls)
#write.csv(rolling_sales,"~/Desktop/Final Project/rolling_sales.csv", row.names = FALSE)

rolling_sales <- read.csv("~/Desktop/Final Project/rolling_sales.csv", header=TRUE, stringsAsFactors=TRUE)
summary(rolling_sales)
str(rolling_sales)
head(rolling_sales)

#data quality check
#setwd("~/Downloads")
#library(dataQualityR)
#checkDataQuality(data = rolling_sales, 
                 #out.file.num ="sa_num.csv", 
                 #out.file.cat= "sa_cat.csv")
#sa_num<-read.csv("~/Desktop/Final Project/sa_num.csv")
#sa_cat<-read.csv("~/Desktop/Final Project/sa_cat.csv")
#View(sa_num)   
#View(sa_cat)   


########################### DATA CLEANING #############################

#replace space in column names with underline
#names(rolling_sales) <- gsub(" ", "_", names(rolling_sales))

#remove rows with price less than $100,000 in the sales price column because they considered outliers
sales <- rolling_sales[!rolling_sales$SALE.PRICE < 10000,,drop=F]
sales <- sales[!sales$SALE.PRICE > 800000000,,drop=F]

#remove “apartment number” column because it has ~55% missing values
#remove "EASE.MENT" column because it only has NA Values
#remove "BLOCK" and "LOT"
#remove "TAX.CLASS.AT.PRESENT." and "BUILDING.CLASS.AT.PRESENT." as I found out that
#there two columns are the same with the two at time of sale.
col <- c("APARTMENT.NUMBER.", "EASE.MENT.", "BLOCK.", "LOT.", 
         "TAX.CLASS.AT.PRESENT.", "BUILDING.CLASS.AT.PRESENT.")
sales <- sales[,!names(sales) %in% col,drop=F]

#remove rows of years before 1900 in year build
table(rolling_sales$YEAR.BUILT.)
sales <- sales[!sales$YEAR.BUILT < 1900,,drop=F]
summary(sales$YEAR.BUILT)

#remove rows of NAs in sales date
sales <- sales[!is.na(sales$SALE.PRICE) == 'TRUE',,drop=F]

#second time quality check
library(dataQualityR)
checkDataQuality(data = sales, 
                 out.file.num ="sa_num.csv", 
                 out.file.cat= "sa_cat.csv")
sa_num<-read.csv("~/Desktop/Final Project/sa_num.csv")
sa_cat<-read.csv("~/Desktop/Final Project/sa_cat.csv")
View(sa_num)   
View(sa_cat)

#replace missing values with median
sales$RESIDENTIAL.UNITS.[is.na(sales$RESIDENTIAL.UNITS.)] <- median(na.omit(sales$RESIDENTIAL.UNITS.))
sales$COMMERCIAL.UNITS.[is.na(sales$COMMERCIAL.UNITS.)] <- median(na.omit(sales$COMMERCIAL.UNITS.))
sales$TOTAL.UNITS.[is.na(sales$TOTAL.UNITS.)] <- median(na.omit(sales$TOTAL.UNITS.))
sales$LAND.SQUARE.FEET.[is.na(sales$LAND.SQUARE.FEET.)] <- mean(na.omit(sales$LAND.SQUARE.FEET.))
sales$GROSS.SQUARE.FEET.[is.na(sales$GROSS.SQUARE.FEET.)] <- mean(na.omit(sales$GROSS.SQUARE.FEET.))
sales$YEAR.BUILT.[is.na(sales$YEAR.BUILT.)] <- median(na.omit(sales$YEAR.BUILT.))

#replace 0 with means
sales$LAND.SQUARE.FEET.[sales$LAND.SQUARE.FEET. == '0'] <- mean(na.omit(sales$LAND.SQUARE.FEET.))
sales$GROSS.SQUARE.FEET.[sales$GROSS.SQUARE.FEET. == '0'] <- mean(na.omit(sales$GROSS.SQUARE.FEET.))

#assign seasonality to sales date
library(lubridate)
getSeason <- function(input.date){
  numeric.date <- 100*month(input.date)+day(input.date)
  ## input Seasons upper limits in the form MMDD in the "break =" option:
  cuts <- base::cut(numeric.date, breaks = c(0,301,0601,0901,1231)) 
  # rename the resulting groups (could've been done within cut(...levels=) if "Winter" wasn't double
  levels(cuts) <- c("Winter","Spring","Summer","Fall")
  return(cuts)
}
sales$SEASON <- getSeason(sales$SALE.DATE.)

#combine BUILDING CLASS CATEGORY.
sales$BUILDING.CLASS.CATEGORY. <- substr(sales$BUILDING.CLASS.CATEGORY., 0,2)
#combine BUILDING.CLASS.AT.TIME.OF.SALE.
sales$BUILDING.CLASS.AT.TIME.OF.SALE. <- substr(sales$BUILDING.CLASS.AT.TIME.OF.SALE., 0, 1)
#convert tax class at time of sale to string
sales$TAX.CLASS.AT.TIME.OF.SALE. <- as.factor(sales$TAX.CLASS.AT.TIME.OF.SALE.)

#combine neighborhood
sales$NEIGHBORHOOD. <- as.character(sales$NEIGHBORHOOD.)
sales$NEIGHBORHOOD.[sales$NEIGHBORHOOD. == "GREENWICH VILLAGE-CENTRAL"] <- "GREENWICH VILLAGE"
sales$NEIGHBORHOOD.[sales$NEIGHBORHOOD. == "GREENWICH VILLAGE-WEST"] <- "GREENWICH VILLAGE"
sales$NEIGHBORHOOD.[sales$NEIGHBORHOOD. == "HARLEM-CENTRAL"] <- "HARLEM"
sales$NEIGHBORHOOD.[sales$NEIGHBORHOOD. == "HARLEM-EAST"] <- "HARLEM"
sales$NEIGHBORHOOD.[sales$NEIGHBORHOOD. == "HARLEM-UPPER"] <- "HARLEM"
sales$NEIGHBORHOOD.[sales$NEIGHBORHOOD. == "HARLEM-WEST"] <- "HARLEM"
sales$NEIGHBORHOOD.[sales$NEIGHBORHOOD. == "MIDTOWN CBD"] <- "MIDTOWN"
sales$NEIGHBORHOOD.[sales$NEIGHBORHOOD. == "MIDTOWN EAST"] <- "MIDTOWN"
sales$NEIGHBORHOOD.[sales$NEIGHBORHOOD. == "MIDTOWN WEST"] <- "MIDTOWN"
sales$NEIGHBORHOOD.[sales$NEIGHBORHOOD. == "UPPER EAST SIDE (59-79)"] <- "UPPER EAST SIDE"
sales$NEIGHBORHOOD.[sales$NEIGHBORHOOD. == "UPPER EAST SIDE (79-96)"] <- "UPPER EAST SIDE"
sales$NEIGHBORHOOD.[sales$NEIGHBORHOOD. == "UPPER EAST SIDE (96-110)"] <- "UPPER EAST SIDE"
sales$NEIGHBORHOOD.[sales$NEIGHBORHOOD. == "UPPER WEST SIDE (59-79)"] <- "UPPER WEST SIDE"
sales$NEIGHBORHOOD.[sales$NEIGHBORHOOD. == "UPPER WEST SIDE (79-96)"] <- "UPPER WEST SIDE"
sales$NEIGHBORHOOD.[sales$NEIGHBORHOOD. == "UPPER WEST SIDE (96-116)"] <- "UPPER WEST SIDE"
sales$NEIGHBORHOOD.[sales$NEIGHBORHOOD. == "WASHINGTON HEIGHTS LOWER"] <- "WASHINGTON HEIGHTS"
sales$NEIGHBORHOOD.[sales$NEIGHBORHOOD. == "WASHINGTON HEIGHTS UPPER"] <- "WASHINGTON HEIGHTS"
sales$NEIGHBORHOOD. <- as.factor(sales$NEIGHBORHOOD.)

table(sales$NEIGHBORHOOD.)

#dummified categorical data
library(caret)
library(dplyr)
sale.cat <- select(sales,
                 BUILDING.CLASS.CATEGORY., 
                 BUILDING.CLASS.AT.TIME.OF.SALE.,
                 TAX.CLASS.AT.TIME.OF.SALE.,
                 SEASON
                 )

str(sale.cat)
dataDummy <- dummyVars("~.",data=sale.cat, fullRank=F)
data.dummified <- as.data.frame(predict(dataDummy,sale.cat))
sales$names <- rownames(sales)
data.dummified$names <- rownames(data.dummified)
clean<-join(sales,data.dummified,type='left')
clean$

#write.csv(clean,"~/Desktop/Final Project/sales_cleaned.csv", row.names = FALSE)



########################### Model #############################
rolling_sales <- read.csv("~/Desktop/Final Project/sales_cleaned.csv", header=TRUE, stringsAsFactors=TRUE)
rolling_sales$netPrice<-rolling_sales$SALE.PRICE./ rolling_sales$GROSS.SQUARE.FEET.
library(dplyr)
rolling_sales<-rolling_sales %>%
  select(-SALE.PRICE.,-SALE.DATE.,-LAND.SQUARE.FEET.,-GROSS.SQUARE.FEET.)
neighborhoodlist <- split(rolling_sales, rolling_sales$NEIGHBORHOOD.)

alphabetCity<-neighborhoodlist$`ALPHABET CITY`
chelsea<-neighborhoodlist$CHELSEA
chinatown<-neighborhoodlist$CHINATOWN
civicCenter<-neighborhoodlist$`CIVIC CENTER`
clinton<-neighborhoodlist$CLINTON
eastVillage<-neighborhoodlist$`EAST VILLAGE`
fashion<-neighborhoodlist$FASHION
financial<-neighborhoodlist$FINANCIAL
flatiron<-neighborhoodlist$FLATIRON
gramercy<-neighborhoodlist$GRAMERCY
greenwichVillage<-neighborhoodlist$`GREENWICH VILLAGE`
harlem<-neighborhoodlist$HARLEM
inwood<-neighborhoodlist$INWOOD
javitsCenter<-neighborhoodlist$`JAVITS CENTER`
kipsBay<-neighborhoodlist$`KIPS BAY`
littleItaly<-neighborhoodlist$`LITTLE ITALY`
lowerEastSide<-neighborhoodlist$`LOWER EAST SIDE`
manhattanValley<-neighborhoodlist$`MANHATTAN VALLEY`
midtown<-neighborhoodlist$MIDTOWN
morningsideHeights<-neighborhoodlist$`MORNINGSIDE HEIGHTS`
murrayHill<-neighborhoodlist$`MURRAY HILL`
rooseveltIsland<-neighborhoodlist$`ROOSEVELT ISLAND`
soho<-neighborhoodlist$SOHO
southbridge<-neighborhoodlist$SOUTHBRIDGE
tribeca<-neighborhoodlist$TRIBECA
upperEastSide<-neighborhoodlist$`UPPER EAST SIDE`
upperWestSide<-neighborhoodlist$`UPPER WEST SIDE`
washingtonHeights<-neighborhoodlist$`WASHINGTON HEIGHTS`

#catboost feature importance
library(catboost)
for (neighborhood in neighborhoodlist){
  y_train <- unlist(neighborhood[c('netPrice')])
  X_train <- neighborhood %>% select(-netPrice)
  train_pool <- catboost.load_pool(data = X_train, label = y_train)
  catboost<- catboost.train(learn_pool = train_pool,params = list(loss_function = 'RMSE',iterations = 100))
  FI<-catboost.get_feature_importance(catboost, pool = train_pool, type = 'FeatureImportance',thread_count = -1)
  FI <- as.data.frame(FI, stringsAsFactors=FALSE)
  FI$name<-rownames(FI)
  print(neighborhood$NEIGHBORHOOD.[1])
  print(FI[order(-FI[1]),])
}

#linear regression
print('linear regression')
for (neighborhood in neighborhoodlist){
  linearMod <- lm(netPrice ~ BUILDING.CLASS.CATEGORY.+RESIDENTIAL.UNITS.+COMMERCIAL.UNITS.+TOTAL.UNITS.+YEAR.BUILT.+TAX.CLASS.AT.TIME.OF.SALE.+BUILDING.CLASS.AT.TIME.OF.SALE.+SEASON, data=neighborhood)
  print(neighborhood$NEIGHBORHOOD.[1])
  print(summary(linearMod))
}






