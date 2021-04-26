setwd("C:/Users/saraa/Desktop")
fmdata <- read.csv("fe_local_clean.csv")
library(ggplot2)
library(caret)

model1 = lm(Difference ~ Produce1000 + Animal1000 + Baked1000 + OtherFood1000 + CreditCards1000 + SNAP1000 +
              WIC1000 + WICCash1000 + Senior1000, data=fmdata)
summary(model1)
SSE1 = sum(model1$residuals^2)
SSE1

model2 = lm(Difference ~ Produce1000 + Baked1000 + OtherFood1000 + CreditCards1000 + WIC1000, data=fmdata)
summary(model2)
SSE2 = sum(model2$residuals^2)
SSE2

set.seed(150)
fmdata_Split <-createDataPartition(fmdata$Difference, p = 0.7, list=FALSE)
fmdata_Train <- fmdata[fmdata_Split, ]
fmdata_Test <- fmdata[-fmdata_Split, ]

#Build model using train
model3 = lm(Difference ~ Produce1000 + Baked1000 + OtherFood1000 + CreditCards1000 + WIC1000, data=fmdata_Train)
summary(model3)
SSE3 = sum(model3$residuals^2)
SSE3

set.seed(150)
fmdata_Split <-createDataPartition(fmdata$Difference, p = 0.7, list=FALSE)
fmdata_Train <- fmdata[fmdata_Split, ]
fmdata_Test <- fmdata[-fmdata_Split, ]

model4 = lm(Difference ~ Produce1000 + Baked1000 + CreditCards1000 + WIC1000, data=fmdata_Train)
summary(model4)
SSE4 = sum(model4$residuals^2)
SSE4
predictTest <- predict(model4, newdata=fmdata_Test)

predictTest <- predict(model4, newdata=fmdata_Test)
SSE4 <- sum((fmdata_Test$Difference - predictTest) ^ 2)
SST4 <- sum((fmdata_Test$Difference - mean(fmdata_Test$Difference)) ^ 2)
SSE4
SST4
SquaredR4 <- 1 - SSE4/SST4
SquaredR4

plot(fmdata_Test$Difference, fmdata_Test$Produce1000 + fmdata_Test$Baked1000 + fmdata_Test$CreditCards1000 +
       fmdata_Test$WIC1000, main = "Regression Analysis for Farmer's Markets", xlab = "Difference in markets per 1,000 people", ylab = "Produce, Baked, Credit Cards, WIC")

abline(lm(fmdata_Test$Difference ~ fmdata_Test$Produce1000 + fmdata_Test$Baked1000 + fmdata_Test$CreditCards1000 + fmdata_Test$WIC1000))

predictTest <- predict(model1, newdata=fmdata_Test)
SSE1 <- sum((fmdata_Test$Difference - predictTest) ^ 2)
SST1 <- sum((fmdata_Test$Difference - mean(fmdata_Test$Difference)) ^ 2)
SSE1
SST1
SquaredR1 <- 1 - SSE1/SST1
SquaredR1

plot(fmdata_Test$Difference, fmdata_Test$Produce1000 + fmdata_Test$Animal1000 + fmdata_Test$Baked1000 +
       fmdata_Test$OtherFood1000 + fmdata_Test$CreditCards1000 + fmdata_Test$SNAP1000 + fmdata_Test$WIC1000 +
       fmdata_Test$WICCash1000 + fmdata_Test$Senior1000, main = "Regression Analysis for Farmer's Markets (All Variables)",
     xlab = "Difference in markets per 1,000 people", ylab = "All Food Types and Payment Methods")

abline(lm(fmdata_Test$Difference ~ fmdata_Test$Produce1000 + fmdata_Test$Animal1000 + fmdata_Test$Baked1000 +
            fmdata_Test$OtherFood1000 + fmdata_Test$CreditCards1000 + fmdata_Test$SNAP1000 +
            fmdata_Test$WIC1000 + fmdata_Test$WICCash1000 + fmdata_Test$Senior1000))

#Remove outliers (standard deviation between -1.5 and 1.5)
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

#Produce1000, Animal1000, Baked1000, OtherFood1000, and SNAP1000 are not significant below 0.001, so they are removed.
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
