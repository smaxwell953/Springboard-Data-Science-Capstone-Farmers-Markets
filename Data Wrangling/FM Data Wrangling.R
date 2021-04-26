setwd("C:/Users/saraa/Desktop")

fmdata <- read.csv("fe_local_original.csv") #Load data in RStudio

#Remove columns that do not pertain to farmer's markets
#Remove columns of total number of farmer's markets in each county or county equivalent.
#Remove percentages of each kind of market
#Remove the percent change from 2009 to 2016.

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

#Create dummy variables for the states
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

#Create dummy columns for the regions
fmdata$Midwest <- paste(fmdata$Illinois+fmdata$Indiana+fmdata$Iowa+fmdata$Kansas+fmdata$Michigan+fmdata$Minnesota+fmdata$Missouri+fmdata$Nebraska+fmdata$NorthDakota+fmdata$Ohio+fmdata$SouthDakota+fmdata$Wisconsin)
fmdata$Northeast <- paste(fmdata$Connecticut+fmdata$Delaware+fmdata$DistrictofColumbia+fmdata$Maine+fmdata$Maryland+fmdata$Massachusetts+fmdata$NewHampshire+fmdata$NewJersey+fmdata$NewYork+fmdata$Pennsylvania+fmdata$RhodeIsland+fmdata$Vermont)
fmdata$South <- paste(fmdata$Alabama+fmdata$Arkansas+fmdata$Florida+fmdata$Georgia+fmdata$Kentucky+fmdata$Louisiana+fmdata$Mississippi+fmdata$NorthCarolina+fmdata$Oklahoma+fmdata$SouthCarolina+fmdata$Tennessee+fmdata$Texas+fmdata$Virginia+fmdata$WestVirginia)
fmdata$West <- paste(fmdata$Alaska+fmdata$Arizona+fmdata$California+fmdata$Colorado+fmdata$Hawaii+fmdata$Idaho+fmdata$Montana+fmdata$Nevada+fmdata$NewMexico+fmdata$Oregon+fmdata$Utah+fmdata$Washington+fmdata$Wyoming)

#Region columns are characters, so convert to numeric
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

#Export cleaned data to CSV
write.csv(fmdata, "fe_local_clean.csv")