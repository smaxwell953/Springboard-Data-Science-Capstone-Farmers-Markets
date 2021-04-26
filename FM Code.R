#Set working directory
setwd("C:/Users/saraa/Desktop")

#Load original data and remove columns not relevant to analysis. These included columns with data not
#relevant to farmer's markets, the total number of markets in each county (as a change in this number is more
#vague in measuring the growth or decline), the percentages of each kind of market, and the percent change
#from 2009 to 2016 (as a percent increase from zero would produce invalid data).

fmdata <- read.csv("fe_local_original.csv")
fmdata <- fmdata[, -c(4:19,22,24,26,28,30,32,34,36,38,40:100)]

#Make titles of remaining columns easier to understand
colnames(fmdata)[colnames(fmdata)=="FMRKTPTH09"] <- "Markets2009"
colnames(fmdata)[colnames(fmdata)=="FMRKTPTH16"] <- "Markets2016"
colnames(fmdata)[colnames(fmdata)=="FMRKT_SNAP16"] <- "SNAP"
colnames(fmdata)[colnames(fmdata)=="FMRKT_WIC16"] <- "WIC"
colnames(fmdata)[colnames(fmdata)=="FMRKT_WICCASH16"] <- "WICCash"
colnames(fmdata)[colnames(fmdata)=="FMRKT_SFMNP16"] <- "Senior"
colnames(fmdata)[colnames(fmdata)=="FMRKT_CREDIT16"] <- "CreditCards"
colnames(fmdata)[colnames(fmdata)=="FMRKT_FRVEG16"] <- "Produce"
colnames(fmdata)[colnames(fmdata)=="FMRKT_ANMLPROD16"] <- "Animal"
colnames(fmdata)[colnames(fmdata)=="FMRKT_BAKED16"] <- "Baked"
colnames(fmdata)[colnames(fmdata)=="FMRKT_OTHERFOOD16"] <- "OtherFood"

#Convert numbers to percentages rounded to the nearest thousandth
fmdata$Markets2009 <- round(fmdata$Markets2009, 3)
fmdata$Markets2016 <- round(fmdata$Markets2016, 3)

#Add columns showing the different kinds of markets per 1,000 people
fmdata$SNAP1000 <- fmdata$SNAP*fmdata$Markets2016
fmdata$WIC1000 <- fmdata$WIC*fmdata$Markets2016
fmdata$WICCash1000 <- fmdata$WICCash*fmdata$Markets2016
fmdata$Senior1000 <- fmdata$Senior*fmdata$Markets2016
fmdata$CreditCards1000 <- fmdata$CreditCards*fmdata$Markets2016
fmdata$Produce1000 <- fmdata$Produce*fmdata$Markets2016
fmdata$Animal1000 <- fmdata$Animal*fmdata$Markets2016
fmdata$Baked1000 <- fmdata$Baked*fmdata$Markets2016
fmdata$OtherFood1000 <- fmdata$OtherFood*fmdata$Markets2016

#Add column showing the difference between Markets2009 and Markets2016
fmdata$Difference <- fmdata$Markets2016 - fmdata$Markets2009

#Add column showing whether or not farmer's markets increased their presence in counties between 2009 and 2016
fmdata$Increased <- ifelse(fmdata$Difference > '0',1,0)

#Create dummy variables for the different kinds of farmer's markets.
fmdata$Present_2009 <- ifelse(fmdata$Markets2009== '0',0,1)
fmdata$Present_2016 <- ifelse(fmdata$Markets2016== '0',0,1)
fmdata$Accepts_SNAP <- ifelse(fmdata$SNAP== '0',0,1)
fmdata$Accepts_WIC <- ifelse(fmdata$WIC== '0',0,1)
fmdata$Accepts_WICCash <- ifelse(fmdata$WICCash== '0',0,1)
fmdata$Accepts_Senior <- ifelse(fmdata$Senior== '0',0,1)
fmdata$Accepts_CreditCards <- ifelse(fmdata$CreditCards== '0',0,1)
fmdata$Sells_Produce <- ifelse(fmdata$Produce== '0',0,1)
fmdata$Sells_Animal <- ifelse(fmdata$Animal== '0',0,1)
fmdata$Sells_Baked <- ifelse(fmdata$Baked== '0',0,1)
fmdata$Sells_OtherFood <- ifelse(fmdata$OtherFood== '0',0,1)

#Create dummy variables for the states.
fmdata$Alabama <- ifelse(fmdata$State== 'AL',1,0)
fmdata$Alaska <- ifelse(fmdata$State== 'AK',1,0)
fmdata$Arizona <- ifelse(fmdata$State== 'AZ',1,0)
fmdata$Arkansas <- ifelse(fmdata$State== 'AR',1,0)
fmdata$California <- ifelse(fmdata$State== 'CA',1,0)
fmdata$Colorado <- ifelse(fmdata$State== 'CO',1,0)
fmdata$Connecticut <- ifelse(fmdata$State== 'CT',1,0)
fmdata$Delaware <- ifelse(fmdata$State== 'DE',1,0)
fmdata$DistrictofColumbia <- ifelse(fmdata$State== 'DC',1,0)
fmdata$Florida <- ifelse(fmdata$State== 'FL',1,0)
fmdata$Georgia <- ifelse(fmdata$State== 'GA',1,0)
fmdata$Hawaii <- ifelse(fmdata$State== 'HI',1,0)
fmdata$Idaho <- ifelse(fmdata$State== 'ID',1,0)
fmdata$Illinois <- ifelse(fmdata$State== 'IL',1,0)
fmdata$Indiana <- ifelse(fmdata$State== 'IN',1,0)
fmdata$Iowa <- ifelse(fmdata$State== 'IA',1,0)
fmdata$Kansas <- ifelse(fmdata$State== 'KS',1,0)
fmdata$Kentucky <- ifelse(fmdata$State== 'KY',1,0)
fmdata$Louisiana <- ifelse(fmdata$State== 'LA',1,0)
fmdata$Maine <- ifelse(fmdata$State== 'ME',1,0)
fmdata$Maryland <- ifelse(fmdata$State== 'MD',1,0)
fmdata$Massachusetts <- ifelse(fmdata$State== 'MA',1,0)
fmdata$Michigan <- ifelse(fmdata$State== 'MI',1,0)
fmdata$Minnesota <- ifelse(fmdata$State== 'MN',1,0)
fmdata$Mississippi <- ifelse(fmdata$State== 'MS',1,0)
fmdata$Missouri <- ifelse(fmdata$State== 'MO',1,0)
fmdata$Montana <- ifelse(fmdata$State== 'MT',1,0)
fmdata$Nebraska <- ifelse(fmdata$State== 'NE',1,0)
fmdata$Nevada <- ifelse(fmdata$State== 'NV',1,0)
fmdata$NewHampshire <- ifelse(fmdata$State== 'NH',1,0)
fmdata$NewJersey <- ifelse(fmdata$State== 'NJ',1,0)
fmdata$NewMexico <- ifelse(fmdata$State== 'NM',1,0)
fmdata$NewYork <- ifelse(fmdata$State== 'NY',1,0)
fmdata$NorthCarolina <- ifelse(fmdata$State== 'NC',1,0)
fmdata$NorthDakota <- ifelse(fmdata$State== 'ND',1,0)
fmdata$Ohio <- ifelse(fmdata$State== 'OH',1,0)
fmdata$Oklahoma <- ifelse(fmdata$State== 'OK',1,0)
fmdata$Oregon <- ifelse(fmdata$State== 'OR',1,0)
fmdata$Pennsylvania <- ifelse(fmdata$State== 'PA',1,0)
fmdata$RhodeIsland <- ifelse(fmdata$State== 'RI',1,0)
fmdata$SouthCarolina <- ifelse(fmdata$State== 'SC',1,0)
fmdata$SouthDakota <- ifelse(fmdata$State== 'SD',1,0)
fmdata$Tennessee <- ifelse(fmdata$State== 'TN',1,0)
fmdata$Texas <- ifelse(fmdata$State== 'TX',1,0)
fmdata$Utah <- ifelse(fmdata$State== 'UT',1,0)
fmdata$Vermont <- ifelse(fmdata$State== 'VT',1,0)
fmdata$Virginia <- ifelse(fmdata$State== 'VA',1,0)
fmdata$Washington <- ifelse(fmdata$State== 'WA',1,0)
fmdata$WestVirginia <- ifelse(fmdata$State== 'WV',1,0)
fmdata$Wisconsin <- ifelse(fmdata$State== 'WI',1,0)
fmdata$Wyoming <- ifelse(fmdata$State== 'WY',1,0)

#Create dummy variables for the regions and convert to numeric
fmdata$Midwest <- paste(fmdata$Illinois+fmdata$Indiana+fmdata$Iowa+fmdata$Kansas+fmdata$Michigan+fmdata$Minnesota+fmdata$Missouri+fmdata$Nebraska+fmdata$NorthDakota+fmdata$Ohio+fmdata$SouthDakota+fmdata$Wisconsin)
fmdata$Northeast <- paste(fmdata$Connecticut+fmdata$Delaware+fmdata$DistrictofColumbia+fmdata$Maine+fmdata$Maryland+fmdata$Massachusetts+fmdata$NewHampshire+fmdata$NewJersey+fmdata$NewYork+fmdata$Pennsylvania+fmdata$RhodeIsland+fmdata$Vermont)
fmdata$South <- paste(fmdata$Alabama+fmdata$Arkansas+fmdata$Florida+fmdata$Georgia+fmdata$Kentucky+fmdata$Louisiana+fmdata$Mississippi+fmdata$NorthCarolina+fmdata$Oklahoma+fmdata$SouthCarolina+fmdata$Tennessee+fmdata$Texas+fmdata$Virginia+fmdata$WestVirginia)
fmdata$West <- paste(fmdata$Alaska+fmdata$Arizona+fmdata$California+fmdata$Colorado+fmdata$Hawaii+fmdata$Idaho+fmdata$Montana+fmdata$Nevada+fmdata$NewMexico+fmdata$Oregon+fmdata$Utah+fmdata$Washington+fmdata$Wyoming)

fmdata$Midwest <- as.numeric(as.character(fmdata$Midwest))
fmdata$Northeast <- as.numeric(as.character(fmdata$Northeast))
fmdata$South <- as.numeric(as.character(fmdata$South))
fmdata$West <- as.numeric(as.character(fmdata$West))

#Replace NAs with zeroes
fmdata[is.na(fmdata)] <- 0

#Logarithmic transformation
signedlog10 = function(x) {
  ifelse(abs(x) <= 1, 0, sign(x)*log10(abs(x)))
}

#Apply logarithmic transformation, normalize data, and round to the nearest thousandth
fmdata$Markets_2009_norm <- signedlog10(fmdata$Markets2009)
fmdata$Markets_2009_norm <- rnorm(fmdata$Markets_2009_norm)
fmdata$Markets_2009_norm <- round(fmdata$Markets_2009_norm, 3)

fmdata$Markets_2016_norm <- signedlog10(fmdata$Markets2016)
fmdata$Markets_2016_norm <- rnorm(fmdata$Markets_2016_norm)
fmdata$Markets_2016_norm <- round(fmdata$Markets_2016_norm, 3)

fmdata$SNAP_norm <- signedlog10(fmdata$SNAP1000)
fmdata$SNAP_norm <- rnorm(fmdata$SNAP_norm)
fmdata$SNAP_norm <- round(fmdata$SNAP_norm, 3)

fmdata$WIC_norm <- signedlog10(fmdata$WIC1000)
fmdata$WIC_norm <- rnorm(fmdata$WIC_norm)
fmdata$WIC_norm <- round(fmdata$WIC_norm, 3)

fmdata$WICCash_norm <- signedlog10(fmdata$WICCash1000)
fmdata$WICCash_norm <- rnorm(fmdata$WICCash_norm)
fmdata$WICCash_norm <- round(fmdata$WICCash_norm, 3)

fmdata$Senior_norm <- signedlog10(fmdata$Senior1000)
fmdata$Senior_norm <- rnorm(fmdata$Senior_norm)
fmdata$Senior_norm <- round(fmdata$Senior_norm, 3)

fmdata$CreditCards_norm <- signedlog10(fmdata$CreditCards1000)
fmdata$CreditCards_norm <- rnorm(fmdata$CreditCards_norm)
fmdata$CreditCards_norm <- round(fmdata$CreditCards_norm, 3)

fmdata$Produce_norm <- signedlog10(fmdata$Produce1000)
fmdata$Produce_norm <- rnorm(fmdata$Produce_norm)
fmdata$Produce_norm <- round(fmdata$Produce_norm, 3)

fmdata$Animal_norm <- signedlog10(fmdata$Animal1000)
fmdata$Animal_norm <- rnorm(fmdata$Animal_norm)
fmdata$Animal_norm <- round(fmdata$Animal_norm, 3)

fmdata$Baked_norm <- signedlog10(fmdata$Baked1000)
fmdata$Baked_norm <- rnorm(fmdata$Baked_norm)
fmdata$Baked_norm <- round(fmdata$Baked_norm, 3)

fmdata$OtherFood_norm <- signedlog10(fmdata$OtherFood1000)
fmdata$OtherFood_norm <- rnorm(fmdata$OtherFood_norm)
fmdata$OtherFood_norm <- round(fmdata$OtherFood_norm, 3)

#Export data to CSV file
write.csv(fmdata, "fe_local_clean.csv")

# STATISTICS AND VISUALIZAION
# Open necessary libraries and load cleaned data.

library(plyr)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(plotly)
library(reshape2)
library(tidyr)
library(tinytex)
library(caret)

fmdata <- read.csv("fe_local_clean.csv")

#A function "percent" was added to convert calculations to percentages.
percent <- function(x, digits = 2, format = "f", ...) {
  paste0(formatC(100 * x, format = format, digits = digits, ...), "%")
}

#What percentage of counties saw an increase in farmer's markets per 1,000 people from 2009 to 2016?

moremkts <- fmdata[fmdata$Increased == "1",]
proportion_more <- nrow(moremkts)/nrow(fmdata)
percent(proportion_more)

#What percentage of these counties sell all food types?
allfoods <- data.frame(moremkts$Sells_Produce, moremkts$Sells_Animal, moremkts$Sells_Baked, moremkts$Sells_OtherFood)
count_allfoods <- allfoods[rowSums(allfoods == 0) < 1, ]
allfoods <- nrow(count_allfoods)/nrow(moremkts)
percent(allfoods)

#What percentage of all counties sell all types of food products?
allfoods_all <- data.frame(fmdata$Sells_Produce, fmdata$Sells_Animal, fmdata$Sells_Baked, fmdata$Sells_OtherFood)
count_allfoods_all <- allfoods_all[rowSums(allfoods_all == 0) < 1, ]
allfoods_all <- nrow(count_allfoods_all)/nrow(fmdata)
percent(allfoods_all)

#What percentage of these counties accept all noncash methods of payment?
noncash <- data.frame(moremkts$CreditCards, moremkts$Accepts_SNAP, moremkts$Accepts_WIC, moremkts$Accepts_WICCash, moremkts$Accepts_Senior)
count_noncash <- noncash[rowSums(noncash == 0) < 1, ]
noncash <- nrow(count_noncash)/nrow(moremkts)
percent(noncash)

#What percentage of all counties accept all noncash payments?
noncash_all <- data.frame(fmdata$CreditCards, fmdata$Accepts_SNAP, fmdata$Accepts_WIC, fmdata$Accepts_WICCash, fmdata$Accepts_Senior)
count_noncash_all <- noncash_all[rowSums(noncash_all == 0) < 1, ]
noncash_all <- nrow(count_noncash_all)/nrow(fmdata)
percent(noncash_all)

#What percentage of counties that saw increases are in the Midwest?
MoreMktMidwest <- data.frame(moremkts$Illinois, moremkts$Indiana, moremkts$Iowa, moremkts$Kansas, moremkts$Michigan, moremkts$Minnesota, moremkts$Missouri, moremkts$Nebraska, moremkts$NorthDakota, moremkts$Ohio, moremkts$SouthDakota, moremkts$Wisconsin)
count_midwest <- MoreMktMidwest[rowSums(MoreMktMidwest == 1), ]
midwest <- nrow(count_midwest)/nrow(moremkts)
percent(midwest)

#What percentage of all counties are in the Midwest?
MktMidwest <- data.frame(fmdata$Illinois, fmdata$Indiana, fmdata$Iowa, fmdata$Kansas, fmdata$Michigan, fmdata$Minnesota, fmdata$Missouri, fmdata$Nebraska, fmdata$NorthDakota, fmdata$Ohio, fmdata$SouthDakota, fmdata$Wisconsin)
count_midwest <- MktMidwest[rowSums(MktMidwest == 1), ]
midwest <- nrow(count_midwest)/nrow(fmdata)
percent(midwest)

#What percentage of counties that saw increases are in the Northeast?
MoreMktNortheast <- data.frame(moremkts$Connecticut, moremkts$Delaware, moremkts$DistrictofColumbia, moremkts$Maine, moremkts$Maryland, moremkts$Massachusetts, moremkts$NewHampshire, moremkts$NewJersey, moremkts$NewYork, moremkts$Pennsylvania, moremkts$RhodeIsland, moremkts$Vermont)
count_northeast <- MoreMktNortheast[rowSums(MoreMktNortheast == 1), ]
northeast <- nrow(count_northeast)/nrow(moremkts)
percent(northeast)

#What percentage of all counties are in the Northeast?
MktNortheast <- data.frame(fmdata$Connecticut, fmdata$Delaware, fmdata$DistrictofColumbia, fmdata$Maine, fmdata$Maryland, fmdata$Massachusetts, fmdata$NewHampshire, fmdata$NewJersey, fmdata$NewYork, fmdata$Pennsylvania, fmdata$RhodeIsland, fmdata$Vermont)
count_northeast <- MktNortheast[rowSums(MktNortheast == 1), ]
northeast <- nrow(count_northeast)/nrow(fmdata)
percent(northeast)

#What percentage of counties that saw increases are in the South?
MoreMktSouth <- data.frame(moremkts$Alabama, moremkts$Arkansas, moremkts$Florida, moremkts$Georgia, moremkts$Kentucky, moremkts$Louisiana, moremkts$Mississippi, moremkts$NorthCarolina, moremkts$Oklahoma, moremkts$SouthCarolina, moremkts$Tennessee, moremkts$Texas, moremkts$Virginia, moremkts$WestVirginia)
count_south <- MoreMktSouth[rowSums(MoreMktSouth == 1), ]
south <- nrow(count_south)/nrow(moremkts)
percent(south)

#What percentage of all counties are in the South?
MktSouth <- data.frame(fmdata$Alabama, fmdata$Arkansas, fmdata$Florida, fmdata$Georgia, fmdata$Kentucky, fmdata$Louisiana, fmdata$Mississippi, fmdata$NorthCarolina, fmdata$Oklahoma, fmdata$SouthCarolina, fmdata$Tennessee, fmdata$Texas, fmdata$Virginia, fmdata$WestVirginia)
count_south <- MktSouth[rowSums(MktSouth == 1), ]
south <- nrow(count_south)/nrow(fmdata)
percent(south)

#What percentage of counties that saw increases are in the West?
MoreMktWest <- data.frame(moremkts$Alaska, moremkts$Arizona, moremkts$California, moremkts$Colorado, moremkts$Hawaii, moremkts$Idaho, moremkts$Montana, moremkts$Nevada, moremkts$NewMexico, moremkts$Oregon, moremkts$Utah, moremkts$Washington, moremkts$Wyoming)
count_west <- MoreMktWest[rowSums(MoreMktWest == 1), ]
west <- nrow(count_west)/nrow(moremkts)
percent(west)

#What percentage of all counties are in the West?
MktWest <- data.frame(fmdata$Alaska, fmdata$Arizona, fmdata$California, fmdata$Colorado, fmdata$Hawaii, fmdata$Idaho, fmdata$Montana, fmdata$Nevada, fmdata$NewMexico, fmdata$Oregon, fmdata$Utah, fmdata$Washington, fmdata$Wyoming)
count_west <- MktWest[rowSums(MktWest == 1), ]
west <- nrow(count_west)/nrow(fmdata)
percent(west)

#From these calculations, the Midwest looks slightly overrepresented in counties that saw an increase in markets, the Northeast overrepresented, the South underrepresented, and the West slightly underrepresented.

#Histograms
#Comparing markets that sell produce in counties that saw an increase in farmer's markets with markets in all counties.
ggplot (moremkts, aes(Produce_norm)) + 
  geom_histogram(binwidth = 0.05,col="green3",fill="green4",alpha = .5)+
  ggtitle("Produce-selling markets/1,000 people in counties that saw increases")+
  scale_x_continuous(name="Markets (per 1,000 people)")+
  scale_y_continuous(name="Number of Counties")

ggplot (fmdata, aes(Produce_norm)) + 
  geom_histogram(binwidth = 0.05,col="green1",fill="green2",alpha = .5)+
  ggtitle("Produce-selling markets/1,000 people in all counties")+
  scale_x_continuous(name="Markets (per 1,000 people)")+
  scale_y_continuous(name="Number of Counties")

#Comparing markets that sell animal products in counties that saw an increase in farmer's markets with markets in all counties.
ggplot (moremkts, aes(Animal_norm)) + 
  geom_histogram(binwidth = 0.05,col="red3",fill="red4",alpha = .5)+
  ggtitle("Animal-selling markets/1,000 people in counties that saw increases")+
  scale_x_continuous(name="Markets (per 1,000 people)")+
  scale_y_continuous(name="Number of Counties")

ggplot (fmdata, aes(Animal_norm)) + 
  geom_histogram(binwidth = 0.05,col="red1",fill="red2",alpha = .5)+
  ggtitle("Animal-selling markets/1,000 people in all counties")+
  scale_x_continuous(name="Markets (per 1,000 people)")+
  scale_y_continuous(name="Number of Counties")

#### Comparing markets that sell baked goods in counties that saw an increase in farmer's markets with markets in all counties.
ggplot (moremkts, aes(Baked_norm)) + 
  geom_histogram(binwidth = 0.05,col="yellow3",fill="yellow4",alpha = .5)+
  ggtitle("Baked goods-selling markets/1,000 people in counties that saw increases")+
  scale_x_continuous(name="Markets (per 1,000 people)")+
  scale_y_continuous(name="Number of Counties")

ggplot (fmdata, aes(Baked_norm)) + 
  geom_histogram(binwidth = 0.05,col="yellow1",fill="yellow2",alpha = .5)+
  ggtitle("Baked goods-selling markets/1,000 people in all counties")+
  scale_x_continuous(name="Markets (per 1,000 people)")+
  scale_y_continuous(name="Number of Counties")

#### Comparing markets that sell other food products in counties that saw an increase in farmer's markets with markets in all counties.
ggplot (moremkts, aes(OtherFood_norm)) + 
  geom_histogram(binwidth = 0.05,col="blue3",fill="blue4",alpha = .5)+
  ggtitle("Other-selling markets/1,000 people in counties that saw increases")+
  scale_x_continuous(name="Markets (per 1,000 people)")+
  scale_y_continuous(name="Number of Counties")

ggplot (fmdata, aes(OtherFood_norm)) + 
  geom_histogram(binwidth = 0.05,col="blue1",fill="blue2",alpha = .5)+
  ggtitle("Other-selling markets/1,000 people in all counties")+
  scale_x_continuous(name="Markets (per 1,000 people)")+
  scale_y_continuous(name="Number of Counties")

#Comparing markets that accept credit cards in counties that saw an increase in farmer's markets with markets in all counties.
ggplot (moremkts, aes(CreditCards_norm)) + 
  geom_histogram(binwidth = 0.05,col="turquoise3",fill="turquoise4",alpha = .5)+
  ggtitle("Credit Card-accepting markets/1,000 people in counties that saw increases")+
  scale_x_continuous(name="Markets (per 1,000 people)")+
  scale_y_continuous(name="Number of Counties")

ggplot (fmdata, aes(CreditCards_norm)) + 
  geom_histogram(binwidth = 0.05,col="turquoise1",fill="turquoise2",alpha = .5)+
  ggtitle("Credit Card-accepting markets/1,000 people in all counties")+
  scale_x_continuous(name="Markets (per 1,000 people)")+
  scale_y_continuous(name="Number of Counties")

#Comparing markets that accept SNAP in counties that saw an increase in farmer's markets with markets in all counties.
ggplot (moremkts, aes(SNAP_norm)) + 
  geom_histogram(binwidth = 0.05,col="seagreen3",fill="seagreen4",alpha = .5)+
  ggtitle("SNAP-accepting markets/1,000 people in counties that saw increases")+
  scale_x_continuous(name="Markets (per 1,000 people)")+
  scale_y_continuous(name="Number of Counties")

ggplot (fmdata, aes(SNAP_norm)) + 
  geom_histogram(binwidth = 0.05,col="seagreen1",fill="seagreen2",alpha = .5)+
  ggtitle("SNAP-accepting markets/1,000 people in all counties")+
  scale_x_continuous(name="Markets (per 1,000 people)")+
  scale_y_continuous(name="Number of Counties")

#Comparing markets that accept WIC in counties that saw an increase in farmer's markets with markets in all counties.
ggplot (moremkts, aes(WIC_norm)) + 
  geom_histogram(binwidth = 0.05,col="darkgoldenrod3",fill="darkgoldenrod4",alpha = .5)+
  ggtitle("WIC-accepting markets/1,000 people in counties that saw increases")+
  scale_x_continuous(name="Markets (per 1,000 people)")+
  scale_y_continuous(name="Number of Counties")

ggplot (fmdata, aes(WIC_norm)) + 
  geom_histogram(binwidth = 0.05,col="darkgoldenrod1",fill="darkgoldenrod2",alpha = .5)+
  ggtitle("WIC-accepting markets/1,000 people in all counties")+
  scale_x_continuous(name="Markets (per 1,000 people)")+
  scale_y_continuous(name="Number of Counties")

#Comparing markets that accept WICCash in counties that saw an increase in farmer's markets with markets in all counties.
ggplot (moremkts, aes(WICCash_norm)) + 
  geom_histogram(binwidth = 0.05,col="hotpink3",fill="hotpink4",alpha = .5)+
  ggtitle("WICCash-accepting markets/1,000 people in counties that saw increases")+
  scale_x_continuous(name="Markets (per 1,000 people)")+
  scale_y_continuous(name="Number of Counties")

ggplot (fmdata, aes(WICCash_norm)) + 
  geom_histogram(binwidth = 0.05,col="hotpink1",fill="hotpink2",alpha = .5)+
  ggtitle("WICCash-accepting markets/1,000 people in all counties")+
  scale_x_continuous(name="Markets (per 1,000 people)")+
  scale_y_continuous(name="Number of Counties")

#Comparing markets that accept SFMNP in counties that saw an increase in farmer's markets with markets in all counties.
ggplot (moremkts, aes(Senior_norm)) + 
  geom_histogram(binwidth = 0.05,col="purple3",fill="purple4",alpha = .5)+
  ggtitle("SFMNP-accepting markets/1,000 people in counties that saw increases")+
  scale_x_continuous(name="Markets (per 1,000 people)")+
  scale_y_continuous(name="Number of Counties")

ggplot (fmdata, aes(Senior_norm)) + 
  geom_histogram(binwidth = 0.05,col="purple1",fill="purple2",alpha = .5)+
  ggtitle("SFMNP-accepting markets/1,000 people in all counties")+
  scale_x_continuous(name="Markets (per 1,000 people)")+
  scale_y_continuous(name="Number of Counties")

#The normalized data will be used to compare the means between all counties and counties with more markets in 2016.

t.test(moremkts$Produce_norm, fmdata$Produce_norm, alternative = "two.sided")
t.test(moremkts$Animal_norm, fmdata$Animal_norm, alternative = "two.sided")
t.test(moremkts$Baked_norm, fmdata$Baked_norm, alternative = "two.sided")
t.test(moremkts$OtherFood_norm, fmdata$OtherFood_norm, alternative = "two.sided")
t.test(moremkts$CreditCards_norm, fmdata$CreditCards_norm, alternative = "two.sided")
t.test(moremkts$SNAP_norm, fmdata$SNAP_norm, alternative = "two.sided")
t.test(moremkts$WIC_norm, fmdata$WIC_norm, alternative = "two.sided")
t.test(moremkts$WICCash_norm, fmdata$WICCash_norm, alternative = "two.sided")
t.test(moremkts$Senior_norm, fmdata$Senior_norm, alternative = "two.sided")

# Linear Models
#Here are the different types of food sold in all counties.
MktTypes <- data.frame(fmdata$Markets2016, fmdata$Produce1000, fmdata$Animal1000, fmdata$Baked1000,
                       fmdata$OtherFood1000)

ggplot(MktTypes, aes(MktTypes$fmdata.Markets2016, y = Markets, color = MktTypes)) + 
  geom_smooth(aes(y = fmdata.Produce1000, col = "Produce", fmdata.Markets2016), method ="lm", se = FALSE) + 
  geom_smooth(aes(y = fmdata.Animal1000, col = "Animal", fmdata.Markets2016), method = "lm", se = FALSE) + 
  geom_smooth(aes(y = fmdata.Baked1000, col = "Baked", fmdata.Markets2016), method = "lm", se = FALSE) + 
  geom_smooth(aes(y = fmdata.OtherFood1000, col = "Other", fmdata.Markets2016), method = "lm", se = FALSE) + 
  scale_color_manual("Food Types",
                     breaks = c("Produce", "Animal", "Baked", "Other"),
                     values = c("red", "yellow", "blue", "green4")) + 
  ggtitle("Types of Food Sold in All Counties") + 
  scale_x_continuous(name="Markets per 1,000 people")

#These are the non-cash methods of payment in all counties.
PayTypes <- data.frame(fmdata$Markets2016, fmdata$CreditCards1000, fmdata$SNAP1000, fmdata$WIC1000,
                       fmdata$WICCash1000, fmdata$Senior1000)

ggplot(PayTypes, aes(PayTypes$fmdata.Markets2016, y = Markets, color = PayTypes)) + 
  geom_smooth(aes(y = fmdata.CreditCards1000, col = "Credit Cards", fmdata.Markets2016), method = "lm", se = FALSE) + 
  geom_smooth(aes(y = fmdata.SNAP1000, col = "SNAP", fmdata.Markets2016), method = "lm", se = FALSE) + 
  geom_smooth(aes(y = fmdata.WIC1000, col = "WIC", fmdata.Markets2016), method = "lm", se = FALSE) + 
  geom_smooth(aes(y = fmdata.WICCash1000, col = "WICCash", fmdata.Markets2016), method = "lm", se = FALSE) + 
  geom_smooth(aes(y = fmdata.Senior1000, col = "Senior", fmdata.Markets2016), method = "lm", se = FALSE) +
  ggtitle("Non-Cash Methods of Payment in All Counties") + 
  scale_color_manual("Payment Method",
                     breaks = c("Credit Cards", "SNAP", "WIC", "WICCash", "Senior"),
                     values = c("turquoise4", "purple4", "seagreen", "gold3", "hotpink3")) + 
  scale_x_continuous(name="Markets per 1,000 people")

#### These are the types of food sold in counties With more farmer's markets in 2016.
MktTypesUp <- data.frame(moremkts$Markets2016, moremkts$Produce1000, moremkts$Animal1000, moremkts$Baked1000,
                         moremkts$OtherFood1000)

ggplot(MktTypesUp, aes(MktTypesUp$moremkts.Markets2016, y = Markets, color = MktTypesAll)) + 
  geom_smooth(aes(y = moremkts.Produce1000, col = "Produce", moremkts.Markets2016), method = "lm", se = FALSE) + 
  geom_smooth(aes(y = moremkts.Animal1000, col = "Animal", moremkts.Markets2016), method = "lm", se = FALSE) + 
  geom_smooth(aes(y = moremkts.Baked1000, col = "Baked", moremkts.Markets2016), method = "lm", se = FALSE) + 
  geom_smooth(aes(y = moremkts.OtherFood1000, col = "Other", moremkts.Markets2016), method = "lm", se = FALSE) + 
  scale_color_manual("Food Types",
                     breaks = c("Produce", "Animal", "Baked", "Other"),
                     values = c("red", "yellow", "blue", "green4")) + 
  ggtitle("Types of Food Sold in Counties With More Farmer's Markets in 2016") + 
  scale_x_continuous(name="Markets per 1,000 people")

#### These are the non-cash methods of payment in counties with more farmer's markets in 2016.
PayTypesUp <- data.frame(moremkts$Markets2016, moremkts$CreditCards1000, moremkts$SNAP1000, moremkts$WIC1000,
                         moremkts$WICCash1000, moremkts$Senior1000)

ggplot(PayTypesUp, aes(PayTypesUp$moremkts.Markets2016, y = Markets, color = PayTypesAll)) + 
  geom_smooth(aes(y = moremkts.CreditCards1000, col = "Credit Cards", moremkts.Markets2016), method = "lm", se = FALSE) + 
  geom_smooth(aes(y = moremkts.SNAP1000, col = "SNAP", moremkts.Markets2016), method = "lm", se = FALSE) + 
  geom_smooth(aes(y = moremkts.WIC1000, col = "WIC", moremkts.Markets2016), method = "lm", se = FALSE) + 
  geom_smooth(aes(y = moremkts.WICCash1000, col = "WICCash", moremkts.Markets2016), method = "lm", se = FALSE) + 
  geom_smooth(aes(y = moremkts.Senior1000, col = "Senior", moremkts.Markets2016), method = "lm", se = FALSE) +
  ggtitle("Non-Cash Methods of Payment in Counties With More Farmer's Markets in 2016") + 
  scale_color_manual("Payment Method",
                     breaks = c("Credit Cards", "SNAP", "WIC", "WICCash", "Senior"),
                     values = c("turquoise4", "purple4", "seagreen", "gold3", "hotpink3")) + 
  scale_x_continuous(name="Markets per 1,000 people")

# Geographical distribution
# Percentage of counties in each state that have farmer's markets. The proportions of counties in each state that saw increases from 2009 to 2016 were calculated.

ALmore <- moremkts[moremkts$Alabama == "1",]
ALall <- fmdata[fmdata$Alabama == "1",]
proportion_AL_up <- nrow(ALmore)/nrow(ALall)

AKmore <- moremkts[moremkts$Alaska == "1",]
AKall <- fmdata[fmdata$Alaska == "1",]
proportion_AK_up <- nrow(AKmore)/nrow(AKall)

AZmore <- moremkts[moremkts$Arizona == "1",]
AZall <- fmdata[fmdata$Arizona == "1",]
proportion_AZ_up <- nrow(AZmore)/nrow(AZall)

ARmore <- moremkts[moremkts$Arkansas == "1",]
ARall <- fmdata[fmdata$Arkansas == "1",]
proportion_AR_up <- nrow(ARmore)/nrow(ARall)

CAmore <- moremkts[moremkts$California == "1",]
CAall <- fmdata[fmdata$California == "1",]
proportion_CA_up <- nrow(CAmore)/nrow(CAall)

COmore <- moremkts[moremkts$Colorado == "1",]
COall <- fmdata[fmdata$Colorado == "1",]
proportion_CO_up <- nrow(COmore)/nrow(COall)

CTmore <- moremkts[moremkts$Connecticut == "1",]
CTall <- fmdata[fmdata$Connecticut == "1",]
proportion_CT_up <- nrow(CTmore)/nrow(CTall)

DEmore <- moremkts[moremkts$Delaware == "1",]
DEall <- fmdata[fmdata$Delaware == "1",]
proportion_DE_up <- nrow(DEmore)/nrow(DEall)

DCmore <- moremkts[moremkts$DistrictofColumbia == "1",]
DCall <- fmdata[fmdata$DistrictofColumbia == "1",]
proportion_DC_up <- nrow(DCmore)/nrow(DCall)

FLmore <- moremkts[moremkts$Florida == "1",]
FLall <- fmdata[fmdata$Florida == "1",]
proportion_FL_up <- nrow(FLmore)/nrow(FLall)

GAmore <- moremkts[moremkts$Georgia == "1",]
GAall <- fmdata[fmdata$Georgia == "1",]
proportion_GA_up <- nrow(GAmore)/nrow(GAall)

HImore <- moremkts[moremkts$Hawaii == "1",]
HIall <- fmdata[fmdata$Hawaii == "1",]
proportion_HI_up <- nrow(HImore)/nrow(HIall)

IDmore <- moremkts[moremkts$Idaho == "1",]
IDall <- fmdata[fmdata$Idaho == "1",]
proportion_ID_up <- nrow(IDmore)/nrow(IDall)

ILmore <- moremkts[moremkts$Illinois == "1",]
ILall <- fmdata[fmdata$Illinois == "1",]
proportion_IL_up <- nrow(ILmore)/nrow(ILall)

INmore <- moremkts[moremkts$Indiana == "1",]
INall <- fmdata[fmdata$Indiana == "1",]
proportion_IN_up <- nrow(INmore)/nrow(INall)

IAmore <- moremkts[moremkts$Iowa == "1",]
IAall <- fmdata[fmdata$Iowa == "1",]
proportion_IA_up <- nrow(IAmore)/nrow(IAall)

KSmore <- moremkts[moremkts$Kansas == "1",]
KSall <- fmdata[fmdata$Kansas == "1",]
proportion_KS_up <- nrow(KSmore)/nrow(KSall)

KYmore <- moremkts[moremkts$Kentucky == "1",]
KYall <- fmdata[fmdata$Kentucky == "1",]
proportion_KY_up <- nrow(KYmore)/nrow(KYall)

LAmore <- moremkts[moremkts$Louisiana == "1",]
LAall <- fmdata[fmdata$Louisiana == "1",]
proportion_LA_up <- nrow(LAmore)/nrow(LAall)

MEmore <- moremkts[moremkts$Maine == "1",]
MEall <- fmdata[fmdata$Maine == "1",]
proportion_ME_up <- nrow(MEmore)/nrow(MEall)

MDmore <- moremkts[moremkts$Maryland == "1",]
MDall <- fmdata[fmdata$Maryland == "1",]
proportion_MD_up <- nrow(MDmore)/nrow(MDall)

MAmore <- moremkts[moremkts$Massachusetts == "1",]
MAall <- fmdata[fmdata$Massachusetts == "1",]
proportion_MA_up <- nrow(MAmore)/nrow(MAall)

MImore <- moremkts[moremkts$Michigan == "1",]
MIall <- fmdata[fmdata$Michigan == "1",]
proportion_MI_up <- nrow(MImore)/nrow(MIall)

MNmore <- moremkts[moremkts$Minnesota == "1",]
MNall <- fmdata[fmdata$Minnesota == "1",]
proportion_MN_up <- nrow(MNmore)/nrow(MNall)

MSmore <- moremkts[moremkts$Mississippi == "1",]
MSall <- fmdata[fmdata$Mississippi == "1",]
proportion_MS_up <- nrow(MSmore)/nrow(MSall)

MOmore <- moremkts[moremkts$Missouri == "1",]
MOall <- fmdata[fmdata$Missouri == "1",]
proportion_MO_up <- nrow(MOmore)/nrow(MOall)

MTmore <- moremkts[moremkts$Montana == "1",]
MTall <- fmdata[fmdata$Montana == "1",]
proportion_MT_up <- nrow(MTmore)/nrow(MTall)

NEmore <- moremkts[moremkts$Nebraska == "1",]
NEall <- fmdata[fmdata$Nebraska == "1",]
proportion_NE_up <- nrow(NEmore)/nrow(NEall)

NVmore <- moremkts[moremkts$Nevada == "1",]
NVall <- fmdata[fmdata$Nevada == "1",]
proportion_NV_up <- nrow(NVmore)/nrow(NVall)

NHmore <- moremkts[moremkts$NewHampshire == "1",]
NHall <- fmdata[fmdata$NewHampshire == "1",]
proportion_NH_up <- nrow(NHmore)/nrow(NHall)

NJmore <- moremkts[moremkts$NewJersey == "1",]
NJall <- fmdata[fmdata$NewJersey == "1",]
proportion_NJ_up <- nrow(NJmore)/nrow(NJall)

NMmore <- moremkts[moremkts$NewMexico == "1",]
NMall <- fmdata[fmdata$NewMexico == "1",]
proportion_NM_up <- nrow(NMmore)/nrow(NMall)

NYmore <- moremkts[moremkts$NewYork == "1",]
NYall <- fmdata[fmdata$NewYork == "1",]
proportion_NY_up <- nrow(NYmore)/nrow(NYall)

NCmore <- moremkts[moremkts$NorthCarolina == "1",]
NCall <- fmdata[fmdata$NorthCarolina == "1",]
proportion_NC_up <- nrow(NCmore)/nrow(NCall)

NDmore <- moremkts[moremkts$NorthDakota == "1",]
NDall <- fmdata[fmdata$NorthDakota == "1",]
proportion_ND_up <- nrow(NDmore)/nrow(NDall)

OHmore <- moremkts[moremkts$Ohio == "1",]
OHall <- fmdata[fmdata$Ohio == "1",]
proportion_OH_up <- nrow(OHmore)/nrow(OHall)

OKmore <- moremkts[moremkts$Oklahoma == "1",]
OKall <- fmdata[fmdata$Oklahoma == "1",]
proportion_OK_up <- nrow(OKmore)/nrow(OKall)

ORmore <- moremkts[moremkts$Oregon == "1",]
ORall <- fmdata[fmdata$Oregon == "1",]
proportion_OR_up <- nrow(ORmore)/nrow(ORall)

PAmore <- moremkts[moremkts$Pennsylvania == "1",]
PAall <- fmdata[fmdata$Pennsylvania == "1",]
proportion_PA_up <- nrow(PAmore)/nrow(PAall)

RImore <- moremkts[moremkts$RhodeIsland == "1",]
RIall <- fmdata[fmdata$RhodeIsland == "1",]
proportion_RI_up <- nrow(RImore)/nrow(RIall)

SCmore <- moremkts[moremkts$SouthCarolina == "1",]
SCall <- fmdata[fmdata$SouthCarolina == "1",]
proportion_CA_up <- nrow(CAmore)/nrow(CAall)

SDmore <- moremkts[moremkts$SouthDakota == "1",]
SDall <- fmdata[fmdata$SouthDakota == "1",]
proportion_SD_up <- nrow(SDmore)/nrow(SDall)

TNmore <- moremkts[moremkts$Tennessee == "1",]
TNall <- fmdata[fmdata$Tennessee == "1",]
proportion_TN_up <- nrow(TNmore)/nrow(TNall)

TXmore <- moremkts[moremkts$Texas == "1",]
TXall <- fmdata[fmdata$Texas == "1",]
proportion_TX_up <- nrow(TXmore)/nrow(TXall)

UTmore <- moremkts[moremkts$Utah == "1",]
UTall <- fmdata[fmdata$Utah == "1",]
proportion_UT_up <- nrow(UTmore)/nrow(UTall)

VTmore <- moremkts[moremkts$Vermont == "1",]
VTall <- fmdata[fmdata$Vermont == "1",]
proportion_VT_up <- nrow(VTmore)/nrow(VTall)

VAmore <- moremkts[moremkts$Virginia == "1",]
VAall <- fmdata[fmdata$Virginia == "1",]
proportion_VA_up <- nrow(VAmore)/nrow(VAall)

WAmore <- moremkts[moremkts$Washington == "1",]
WAall <- fmdata[fmdata$Washington == "1",]
proportion_WA_up <- nrow(WAmore)/nrow(WAall)

WVmore <- moremkts[moremkts$WestVirginia == "1",]
WVall <- fmdata[fmdata$WestVirginia == "1",]
proportion_WV_up <- nrow(WVmore)/nrow(WVall)

WImore <- moremkts[moremkts$Wisconsin == "1",]
WIall <- fmdata[fmdata$Wisconsin == "1",]
proportion_WI_up <- nrow(WImore)/nrow(WIall)

WYmore <- moremkts[moremkts$Wyoming == "1",]
WYall <- fmdata[fmdata$Wyoming == "1",]
proportion_WY_up <- nrow(WYmore)/nrow(WYall)


#Group proportions of counties in each state that saw increases from 2009 to 2016 by region (Midwest, Northeast, South; West).
#Counties in the Midwest that saw increases
Midwest_State <- rep(c("IL", "IN", "IA", "KS", "MI", "MN", "MO", "NE", "ND", "OH", "SD", "WI"))

Midwest_Proportion_Increased <- c(as.numeric(proportion_IL_up), as.numeric(proportion_IN_up), as.numeric(proportion_IA_up), as.numeric(proportion_KS_up), as.numeric(proportion_MI_up), as.numeric(proportion_MN_up), as.numeric(proportion_MO_up), as.numeric(proportion_NE_up), as.numeric(proportion_ND_up), as.numeric(proportion_OH_up), as.numeric(proportion_SD_up), as.numeric(proportion_WI_up))

Midwest_MktsMore <- data.frame(Midwest_State, Midwest_Proportion_Increased)

Midwest_MktsMore$Midwest_State <-factor(Midwest_MktsMore$Midwest_State, 
                                        levels = c("IL", "IN", "IA", "KS", "MI", "MN", "MO", "NE", "ND", "OH", "SD", "WI"))

Midwest_Mkts_Up <- ggplot (Midwest_MktsMore, aes(Midwest_State, Midwest_Proportion_Increased))
Midwest_Mkts_Up + geom_bar(stat = "identity",fill="green4") + xlab("States") + ylab("Proportion") + ggtitle("Counties in Each State that saw Increases in Farmer's Markets (Midwest)")

#Counties in the Northeast that saw increases
Northeast_State <- rep(c("CT", "DE", "DC", "ME", "MD", "MA", "NH", "NJ", "NY", "PA", "RI", "VT"))

Northeast_Proportion_Increased <- c(as.numeric(proportion_CT_up), as.numeric(proportion_DC_up), as.numeric(proportion_DE_up), as.numeric(proportion_ME_up), as.numeric(proportion_MD_up), as.numeric(proportion_MA_up), as.numeric(proportion_NH_up), as.numeric(proportion_NJ_up), as.numeric(proportion_NY_up), as.numeric(proportion_PA_up), as.numeric(proportion_RI_up), as.numeric(proportion_VT_up))

Northeast_MktsMore <- data.frame(Northeast_State, Northeast_Proportion_Increased)

Northeast_MktsMore$Northeast_State <-factor(Northeast_MktsMore$Northeast_State, 
                                            levels = c("CT", "DE", "DC", "ME", "MD", "MA", "NH", "NJ", "NY", "PA", "RI", "VT"))

Northeast_Mkts_Up <- ggplot (Northeast_MktsMore, aes(Northeast_State, Northeast_Proportion_Increased))
Northeast_Mkts_Up + geom_bar(stat = "identity",fill="blue2") + xlab("States") + ylab("Proportion") + ggtitle("Counties in Each State that saw Increases in Farmer's Markets (Northeast)")


#Counties in the South that saw increases
South_State <- rep(c("AL", "AR", "FL", "GA", "KY", "LA", "MS", "NC", "OK", "SC", "TN", "TX", "VA", "WV"))

South_Proportion_Increased <- c(as.numeric(proportion_AL_up), as.numeric(proportion_AR_up), as.numeric(proportion_FL_up), as.numeric(proportion_GA_up), as.numeric(proportion_KY_up), as.numeric(proportion_LA_up), as.numeric(proportion_MS_up), as.numeric(proportion_NC_up), as.numeric(proportion_OK_up), as.numeric(proportion_SD_up), as.numeric(proportion_TN_up), as.numeric(proportion_TX_up), as.numeric(proportion_VA_up), as.numeric(proportion_WV_up))

South_MktsMore <- data.frame(South_State, South_Proportion_Increased)

South_MktsMore$State <-factor(South_MktsMore$South_State, 
                              levels = c("AL", "AR", "FL", "GA", "KY", "LA", "MS", "NC", "OK", "SC", "TN", "TX", "VA", "WV"))

South_Mkts_Up <- ggplot (South_MktsMore, aes(South_State, South_Proportion_Increased))
South_Mkts_Up + geom_bar(stat = "identity",fill="red2") + xlab("States") + ylab("Proportion") + ggtitle("Counties in Each State that saw Increases in Farmer's Markets (South)")

#Counties in the West that saw increases
West_State <- rep(c("AK", "AZ", "CA", "CO", "HI", "ID", "MT", "NV", "NM", "OR", "UT", "WA", "WY"))

West_Proportion_Increased <- c(as.numeric(proportion_AK_up), as.numeric(proportion_AZ_up), as.numeric(proportion_CA_up), as.numeric(proportion_CO_up), as.numeric(proportion_HI_up), as.numeric(proportion_ID_up), as.numeric(proportion_MT_up), as.numeric(proportion_NV_up), as.numeric(proportion_NM_up), as.numeric(proportion_OR_up), as.numeric(proportion_UT_up), as.numeric(proportion_WA_up), as.numeric(proportion_WY_up))

West_Mkts_More <- data.frame(West_State, West_Proportion_Increased)

West_Mkts_More$West_State <-factor(West_Mkts_More$West_State, 
                                   levels = c("AK", "AZ", "CA", "CO", "HI", "ID", "MT", "NV", "NM", "OR", "UT", "WA", "WY"))

West_Mkts_Up <- ggplot (West_Mkts_More, aes(West_State, West_Proportion_Increased))
West_Mkts_Up + geom_bar(stat = "identity",fill="gold2") + xlab("States") + ylab("Proportion") + ggtitle("Counties in Each State that saw Increases in Farmer's Markets (West)")


# REGRESSION ANALYSIS
#I choose to use supervised machine learning, specifically linear regression, because the outcome, the difference in markets per 1,000 people between 2009 and 2016, has an infinite number of possible values. Predictors include Produce1000, Animal1000, Baked1000, OtherFood1000, CreditCards1000, SNAP1000, WIC1000, WICCash1000, and Senior1000.

model1 = lm(Difference ~ Produce1000 + Animal1000 + Baked1000 + OtherFood1000 + CreditCards1000 + SNAP1000 +
              WIC1000 + WICCash1000 + Senior1000, data=fmdata)
summary(model1)
SSE1 = sum(model1$residuals^2)
SSE1

#The Animal, SNAP, WICCash, and Senior variables are not significant at less than 0.05, so these factors will not be used in the regression analysis.
model2 = lm(Difference ~ Produce1000 + Baked1000 + OtherFood1000 + CreditCards1000 + WIC1000, data=fmdata)
summary(model2)
SSE2 = sum(model2$residuals^2)
SSE2

#Now I will split the data, using 70% as a training sample and 30% as a test saample.
set.seed(150)
fmdata_Split <-createDataPartition(fmdata$Difference, p = 0.7, list=FALSE)
fmdata_Train <- fmdata[fmdata_Split, ]
fmdata_Test <- fmdata[-fmdata_Split, ]

model3 = lm(Difference ~ Produce1000 + Baked1000 + OtherFood1000 + CreditCards1000 + WIC1000, data=fmdata_Train)
summary(model3)
SSE3 = sum(model3$residuals^2)
SSE3

#The OtherFood variable is not significant at less than 1, so it too will be removed.
model4 = lm(Difference ~ Produce1000 + Baked1000 + CreditCards1000 + WIC1000, data=fmdata_Train)
summary(model4)
SSE4 = sum(model4$residuals^2)
SSE4

#I will see how well my linear regression model can predict the difference in markets per 1,000 people between the years 2009 and 2016 by measuring R^2, which represents the proportion of variability explained by the model. The closer R^2 is to 1, the more precise the prediction.
predictTest <- predict(model4, newdata=fmdata_Test)
SSE4 <- sum((fmdata_Test$Difference - predictTest) ^ 2)
SST4 <- sum((fmdata_Test$Difference - mean(fmdata_Test$Difference)) ^ 2)
SquaredR4 <- 1 - SSE4/SST4
percent(SquaredR4)

#Only 13.87% of the variability can be explained by this regression model, so the model is fairly weak at predicting the change in farmer's markets per 1,000 people.

plot(fmdata_Test$Difference, fmdata_Test$Produce1000 + fmdata_Test$Baked1000 + fmdata_Test$CreditCards1000 +
       fmdata_Test$WIC1000, main = "Regression Analysis for Farmer's Markets", xlab = "Difference in markets per 1,000 people", ylab = "Produce, Baked, Credit Cards, WIC")

abline(lm(fmdata_Test$Difference ~ fmdata_Test$Produce1000 + fmdata_Test$Baked1000 + fmdata_Test$CreditCards1000 + fmdata_Test$WIC1000))

#As this plot shows, not very many points are close to the line, which shows that this is a weak model. Let's try using model1, which has all variables.

predictTest <- predict(model1, newdata=fmdata_Test)
SSE1 <- sum((fmdata_Test$Difference - predictTest) ^ 2)
SST1 <- sum((fmdata_Test$Difference - mean(fmdata_Test$Difference)) ^ 2)
SquaredR1 <- 1 - SSE1/SST1
percent(SquaredR1)

#This model has a slightly better R-squared value, with 21.02% of variability explained by the model, though it is still weak even with all variables.
plot(fmdata_Test$Difference, fmdata_Test$Produce1000 + fmdata_Test$Animal1000 + fmdata_Test$Baked1000 +
       fmdata_Test$OtherFood1000 + fmdata_Test$CreditCards1000 + fmdata_Test$SNAP1000 + fmdata_Test$WIC1000 +
       fmdata_Test$WICCash1000 + fmdata_Test$Senior1000, main = "Regression Analysis for Farmer's Markets (All Variables)",
     xlab = "Difference in markets per 1,000 people", ylab = "All Food Types and Payment Methods")

abline(lm(fmdata_Test$Difference ~ fmdata_Test$Produce1000 + fmdata_Test$Animal1000 + fmdata_Test$Baked1000 +
            fmdata_Test$OtherFood1000 + fmdata_Test$CreditCards1000 + fmdata_Test$SNAP1000 +
            fmdata_Test$WIC1000 + fmdata_Test$WICCash1000 + fmdata_Test$Senior1000))

#Compared to the previous plot, this plot has a slightly greater share of points close to the line, showing that using all variables leads to a slightly stronger model.

#Now let's try excluding the outliers. This will involve excluding the values in the "normalized" columns greater than 1.5 or less than -1.5.

fmdata_decimals <- data.frame(fmdata$SNAP1000, fmdata$WIC1000, fmdata$WICCash1000, fmdata$Senior1000,
                              fmdata$CreditCards1000, fmdata$Produce1000, fmdata$Animal1000, fmdata$Baked1000,
                              fmdata$OtherFood1000, fmdata$Difference, fmdata$Markets_2009_norm,
                              fmdata$Markets_2016_norm, fmdata$SNAP_norm, fmdata$WIC_norm,
                              fmdata$WICCash_norm, fmdata$Senior_norm, fmdata$CreditCards_norm,
                              fmdata$Produce_norm, fmdata$Animal_norm, fmdata$Baked_norm,
                              fmdata$OtherFood_norm)

Markets_no_outliers <- fmdata_decimals[fmdata_decimals$fmdata.Markets_2009_norm <= 1.5,]
Markets_no_outliers <- Markets_no_outliers[Markets_no_outliers$fmdata.Markets_2009_norm >= -1.5,]
Markets_no_outliers <- Markets_no_outliers[Markets_no_outliers$fmdata.Markets_2016_norm <= 1.5,]
Markets_no_outliers <- Markets_no_outliers[Markets_no_outliers$fmdata.Markets_2016_norm >= -1.5,]
Markets_no_outliers <- Markets_no_outliers[Markets_no_outliers$fmdata.SNAP_norm <= 1.5,]
Markets_no_outliers <- Markets_no_outliers[Markets_no_outliers$fmdata.SNAP_norm >= -1.5,]
Markets_no_outliers <- Markets_no_outliers[Markets_no_outliers$fmdata.WIC_norm <= 1.5,]
Markets_no_outliers <- Markets_no_outliers[Markets_no_outliers$fmdata.WIC_norm >= -1.5,]
Markets_no_outliers <- Markets_no_outliers[Markets_no_outliers$fmdata.WICCash_norm <= 1.5,]
Markets_no_outliers <- Markets_no_outliers[Markets_no_outliers$fmdata.WICCash_norm >= -1.5,]
Markets_no_outliers <- Markets_no_outliers[Markets_no_outliers$fmdata.Senior_norm <= 1.5,]
Markets_no_outliers <- Markets_no_outliers[Markets_no_outliers$fmdata.Senior_norm >= -1.5,]
Markets_no_outliers <- Markets_no_outliers[Markets_no_outliers$fmdata.CreditCards_norm <= 1.5,]
Markets_no_outliers <- Markets_no_outliers[Markets_no_outliers$fmdata.CreditCards_norm >= -1.5,]
Markets_no_outliers <- Markets_no_outliers[Markets_no_outliers$fmdata.Produce_norm <= 1.5,]
Markets_no_outliers <- Markets_no_outliers[Markets_no_outliers$fmdata.Produce_norm >= -1.5,]
Markets_no_outliers <- Markets_no_outliers[Markets_no_outliers$fmdata.Animal_norm <= 1.5,]
Markets_no_outliers <- Markets_no_outliers[Markets_no_outliers$fmdata.Animal_norm >= -1.5,]
Markets_no_outliers <- Markets_no_outliers[Markets_no_outliers$fmdata.Baked_norm <= 1.5,]
Markets_no_outliers <- Markets_no_outliers[Markets_no_outliers$fmdata.Baked_norm >= -1.5,]
Markets_no_outliers <- Markets_no_outliers[Markets_no_outliers$fmdata.OtherFood_norm <= 1.5,]
Markets_no_outliers <- Markets_no_outliers[Markets_no_outliers$fmdata.OtherFood_norm >= -1.5,]

#Now build the model.
model5 = lm(fmdata.Difference ~ fmdata.Produce1000 + fmdata.Animal1000 + fmdata.Baked1000 + 
              fmdata.OtherFood1000 + fmdata.CreditCards1000 + fmdata.SNAP1000 + fmdata.WIC1000 +
              fmdata.WICCash1000 + fmdata.Senior1000, data=Markets_no_outliers)
summary(model5)
SSE5 = sum(model5$residuals^2)
SSE5

#Produce1000, Baked1000, CreditCards1000, SNAP1000, WICCash1000, and Senior1000 are not significant at less than 0.001, so these variables will be removed.
model6 = lm(fmdata.Difference ~ fmdata.Animal1000 + fmdata.OtherFood1000 + fmdata.WIC1000, data=Markets_no_outliers)
summary(model6)
SSE6 = sum(model6$residuals^2)
SSE6

#The R squared is a little worse at 25.99% without the outliers. Split the data from model 5 into training and test sets.
set.seed(150)
no_outlier_Split <-createDataPartition(Markets_no_outliers$fmdata.Difference, p = 0.7, list=FALSE)
no_outlier_Train <- Markets_no_outliers[no_outlier_Split, ]
no_outlier_Test <- Markets_no_outliers[-no_outlier_Split, ]

model7 = lm(fmdata.Difference ~ fmdata.Produce1000 + fmdata.Animal1000 + fmdata.Baked1000 + 
              fmdata.OtherFood1000 + fmdata.CreditCards1000 + fmdata.SNAP1000 + fmdata.WIC1000 +
              fmdata.WICCash1000 + fmdata.Senior1000, data=no_outlier_Train)
summary(model7)
SSE7 = sum(model7$residuals^2)
SSE7

predictTest <- predict(model7, newdata=no_outlier_Test)

#### Produce1000, Animal1000, Baked1000, OtherFood1000, and SNAP1000 are not significant below 0.001, so they are removed.
set.seed(150)
no_outlier_Split <-createDataPartition(Markets_no_outliers$fmdata.Difference, p = 0.7, list=FALSE)
no_outlier_Train <- Markets_no_outliers[no_outlier_Split, ]
no_outlier_Test <- Markets_no_outliers[-no_outlier_Split, ]

model8 = lm(fmdata.Difference ~ fmdata.CreditCards1000 + fmdata.WIC1000 +
              fmdata.WICCash1000 + fmdata.Senior1000, data=no_outlier_Train)
summary(model8)
SSE8 = sum(model8$residuals^2)
SSE8

predictTest <- predict(model8, newdata=no_outlier_Test)

#Plot the model.
plot(Markets_no_outliers$fmdata.Difference, Markets_no_outliers$fmdata.CreditCards1000 +
       Markets_no_outliers$fmdata.WIC1000 + Markets_no_outliers$fmdata.WICCash1000 + Markets_no_outliers$fmdata.Senior1000, main = "Regression Analysis for Farmer's Markets", xlab = "Difference in markets per 1,000 people", ylab = "Credit Cards, WIC, WICCash, Senior")

abline(lm(Markets_no_outliers$fmdata.Difference ~ Markets_no_outliers$fmdata.Animal1000 +
            Markets_no_outliers$fmdata.OtherFood1000))