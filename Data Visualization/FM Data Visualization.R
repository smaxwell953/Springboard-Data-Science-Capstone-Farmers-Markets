setwd("C:/Users/saraa/Desktop")

library(plyr)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(plotly)
library(reshape2)
library(tidyr)
library(tinytex)

fmdata <- read.csv("fe_local_clean.csv")

##Different kinds of percentages
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
noncash <- data.frame(moremkts$CreditCards, moremkts$Accepts_SNAP, moremkts$Accepts_WIC, moremkts$Accepts_WICCash,
                      moremkts$Accepts_Senior)
count_noncash <- noncash[rowSums(noncash == 0) < 1, ]
noncash <- nrow(count_noncash)/nrow(moremkts)
percent(noncash)

#What percentage of all counties accept all noncash payments?
noncash_all <- data.frame(fmdata$CreditCards, fmdata$Accepts_SNAP, fmdata$Accepts_WIC, fmdata$Accepts_WICCash,
                          fmdata$Accepts_Senior)
count_noncash_all <- noncash_all[rowSums(noncash_all == 0) < 1, ]
noncash_all <- nrow(count_noncash_all)/nrow(fmdata)
percent(noncash_all)

#What percentage of these counties are in the Midwest?
MktMidwest <- data.frame(moremkts$Illinois, moremkts$Indiana, moremkts$Iowa, moremkts$Kansas, moremkts$Michigan, moremkts$Minnesota, moremkts$Missouri, moremkts$Nebraska, moremkts$NorthDakota, moremkts$Ohio, moremkts$SouthDakota, moremkts$Wisconsin)
count_midwest <- MktMidwest[rowSums(MktMidwest == 1), ]
midwest <- nrow(count_midwest)/nrow(moremkts)
percent(midwest)

#What percentage of all counties are in the Midwest?
MktMidwest <- data.frame(fmdata$Illinois, fmdata$Indiana, fmdata$Iowa, fmdata$Kansas, fmdata$Michigan, fmdata$Minnesota, fmdata$Missouri, fmdata$Nebraska, fmdata$NorthDakota, fmdata$Ohio, fmdata$SouthDakota, fmdata$Wisconsin)
count_midwest <- MktMidwest[rowSums(MktMidwest == 1), ]
midwest <- nrow(count_midwest)/nrow(fmdata)
percent(midwest)

#What percentage of these counties are in the Northeast?
MktNortheast <- data.frame(moremkts$Connecticut, moremkts$Delaware, moremkts$DistrictofColumbia, moremkts$Maine, moremkts$Maryland, moremkts$Massachusetts, moremkts$NewHampshire, moremkts$NewJersey, moremkts$NewYork, moremkts$Pennsylvania, moremkts$RhodeIsland, moremkts$Vermont)
count_northeast <- MktNortheast[rowSums(MktNortheast == 1), ]
northeast <- nrow(count_northeast)/nrow(moremkts)
percent(northeast)

#What percentage of all counties are in the Northeast?
MktNortheast <- data.frame(fmdata$Connecticut, fmdata$Delaware, fmdata$DistrictofColumbia, fmdata$Maine, fmdata$Maryland, fmdata$Massachusetts, fmdata$NewHampshire, fmdata$NewJersey, fmdata$NewYork, fmdata$Pennsylvania, fmdata$RhodeIsland, fmdata$Vermont)
count_northeast <- MktNortheast[rowSums(MktNortheast == 1), ]
northeast <- nrow(count_northeast)/nrow(fmdata)
percent(northeast)

#What percentage of these counties are in the South?
MoreMktSouth <- data.frame(moremkts$Alabama, moremkts$Arkansas, moremkts$Florida, moremkts$Georgia, moremkts$Kentucky, moremkts$Louisiana, moremkts$Mississippi, moremkts$NorthCarolina, moremkts$Oklahoma, moremkts$SouthCarolina, moremkts$Tennessee, moremkts$Texas, moremkts$Virginia, moremkts$WestVirginia)
count_south <- MoreMktSouth[rowSums(MoreMktSouth == 1), ]
south <- nrow(count_south)/nrow(moremkts)
percent(south)

#What percentage of all counties are in the South?
MktSouth <- data.frame(fmdata$Alabama, fmdata$Arkansas, fmdata$Florida, fmdata$Georgia, fmdata$Kentucky, fmdata$Louisiana, fmdata$Mississippi, fmdata$NorthCarolina, fmdata$Oklahoma, fmdata$SouthCarolina, fmdata$Tennessee, fmdata$Texas, fmdata$Virginia, fmdata$WestVirginia)
count_south <- MktSouth[rowSums(MktSouth == 1), ]
south <- nrow(count_south)/nrow(fmdata)
percent(south)

#What percentage of these counties are in the West?
MoreMktWest <- data.frame(moremkts$Alaska, moremkts$Arizona, moremkts$California, moremkts$Colorado, moremkts$Hawaii, moremkts$Idaho, moremkts$Montana, moremkts$Nevada, moremkts$NewMexico, moremkts$Oregon, moremkts$Utah, moremkts$Washington, moremkts$Wyoming)
count_west <- MoreMktWest[rowSums(MoreMktWest == 1), ]
west <- nrow(count_west)/nrow(moremkts)
percent(west)

#What percentage of all counties are in the West?
MktWest <- data.frame(fmdata$Alaska, fmdata$Arizona, fmdata$California, fmdata$Colorado, fmdata$Hawaii, fmdata$Idaho, fmdata$Montana, fmdata$Nevada, fmdata$NewMexico, fmdata$Oregon, fmdata$Utah, fmdata$Washington, fmdata$Wyoming)
count_west <- MktWest[rowSums(MktWest == 1), ]
west <- nrow(count_west)/nrow(fmdata)
percent(west)

#Histograms
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

#Comparing the means of the markets that saw increases with markets overall via two-sided t test

var.test(moremkts$Produce_norm, fmdata$Produce_norm, alternative = "two.sided")
var.test(moremkts$Animal_norm, fmdata$Animal_norm, alternative = "two.sided")
var.test(moremkts$Baked_norm, fmdata$Baked_norm, alternative = "two.sided")
var.test(moremkts$OtherFood_norm, fmdata$OtherFood_norm, alternative = "two.sided")
var.test(moremkts$CreditCards_norm, fmdata$CreditCards_norm, alternative = "two.sided")
var.test(moremkts$SNAP_norm, fmdata$SNAP_norm, alternative = "two.sided")
var.test(moremkts$WIC_norm, fmdata$WIC_norm, alternative = "two.sided")
var.test(moremkts$WICCash_norm, fmdata$WICCash_norm, alternative = "two.sided")
var.test(moremkts$Senior_norm, fmdata$Senior_norm, alternative = "two.sided")

#Plots
#Types of Food Sold in All Counties
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

#Non-Cash Methods of Payment in All Counties
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

#Types of Food Sold in Counties With More Farmer's Markets in 2016
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

#Non-Cash Methods of Payment in Counties With More Farmer's Markets in 2016
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

#Geographical distribution
#Calculate percentages of counties in each state that saw increases
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

#Make data frame
Midwest_State <- rep(c("IL", "IN", "IA", "KS", "MI", "MN", "MO", "NE", "ND", "OH", "SD", "WI"))

Midwest_Proportion_Increased <- c(as.numeric(proportion_IL_up), as.numeric(proportion_IN_up), as.numeric(proportion_IA_up), as.numeric(proportion_KS_up), as.numeric(proportion_MI_up), as.numeric(proportion_MN_up), as.numeric(proportion_MO_up), as.numeric(proportion_NE_up), as.numeric(proportion_ND_up), as.numeric(proportion_OH_up), as.numeric(proportion_SD_up), as.numeric(proportion_WI_up))

Midwest_MktsMore <- data.frame(Midwest_State, Midwest_Proportion_Increased)

Midwest_MktsMore$Midwest_State <-factor(Midwest_MktsMore$Midwest_State, 
                                levels = c("IL", "IN", "IA", "KS", "MI", "MN", "MO", "NE", "ND", "OH", "SD", "WI"))

Midwest_Mkts_Up <- ggplot (Midwest_MktsMore, aes(Midwest_State, Midwest_Proportion_Increased))
Midwest_Mkts_Up + geom_bar(stat = "identity",fill="green2") + xlab("Months") + ylab("Count") + ggtitle("Chickens & Eggs")

Northeast_State <- rep(c("CT", "DE", "DC", "ME", "MD", "MA", "NH", "NJ", "NY", "PA", "RI", "VT"))

Northeast_Proportion_Increased <- c(as.numeric(proportion_CT_up), as.numeric(proportion_DC_up), as.numeric(proportion_DE_up), as.numeric(proportion_ME_up), as.numeric(proportion_MD_up), as.numeric(proportion_MA_up), as.numeric(proportion_NH_up), as.numeric(proportion_NJ_up), as.numeric(proportion_NY_up), as.numeric(proportion_PA_up), as.numeric(proportion_RI_up), as.numeric(proportion_VT_up))

Northeast_MktsMore <- data.frame(Northeast_State, Northeast_Proportion_Increased)

Northeast_MktsMore$Northeast_State <-factor(Northeast_MktsMore$Northeast_State, 
                                  levels = c("CT", "DE", "DC", "ME", "MD", "MA", "NH", "NJ", "NY", "PA", "RI", "VT"))

Northeast_Mkts_Up <- ggplot (Northeast_MktsMore, aes(Northeast_State, Northeast_Proportion_Increased))
Northeast_Mkts_Up + geom_bar(stat = "identity",fill="blue2")

South_State <- rep(c("AL", "AR", "FL", "GA", "KY", "LA", "MS", "NC", "OK", "SC", "TN", "TX", "VA", "WV"))

South_Proportion_Increased <- c(as.numeric(proportion_AL_up), as.numeric(proportion_AR_up), as.numeric(proportion_FL_up), as.numeric(proportion_GA_up), as.numeric(proportion_KY_up), as.numeric(proportion_LA_up), as.numeric(proportion_MS_up), as.numeric(proportion_NC_up), as.numeric(proportion_OK_up), as.numeric(proportion_SD_up), as.numeric(proportion_TN_up), as.numeric(proportion_TX_up), as.numeric(proportion_VA_up), as.numeric(proportion_WV_up))

South_MktsMore <- data.frame(South_State, South_Proportion_Increased)

South_MktsMore$State <-factor(South_MktsMore$South_State, 
                              levels = c("AL", "AR", "FL", "GA", "KY", "LA", "MS", "NC", "OK", "SC", "TN", "TX", "VA", "WV"))

South_Mkts_Up <- ggplot (South_MktsMore, aes(South_State, South_Proportion_Increased))
South_Mkts_Up + geom_bar(stat = "identity",fill="red2")

West_State <- rep(c("AK", "AZ", "CA", "CO", "HI", "ID", "MT", "NV", "NM", "OR", "UT", "WA", "WY"))

West_Proportion_Increased <- c(as.numeric(proportion_AK_up), as.numeric(proportion_AZ_up), as.numeric(proportion_CA_up), as.numeric(proportion_CO_up), as.numeric(proportion_HI_up), as.numeric(proportion_ID_up), as.numeric(proportion_MT_up), as.numeric(proportion_NV_up), as.numeric(proportion_NM_up), as.numeric(proportion_OR_up), as.numeric(proportion_UT_up), as.numeric(proportion_WA_up), as.numeric(proportion_WY_up))

West_Mkts_More <- data.frame(West_State, West_Proportion_Increased)

West_Mkts_More$West_State <-factor(West_Mkts_More$West_State, 
                        levels = c("AK", "AZ", "CA", "CO", "HI", "ID", "MT", "NV", "NM", "OR", "UT", "WA", "WY"))

West_Mkts_Up <- ggplot (West_Mkts_More, aes(West_State, West_Proportion_Increased))
West_Mkts_Up + geom_bar(stat = "identity",fill="gold2")
