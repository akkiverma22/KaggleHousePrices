setwd("/Users/akashverma/Documents/Kaggle/HousePrices")

## load train data 
train_data <- read.csv("train.csv")

## get the number of rows and columns
dim(train_data)
#### [1] 1460   81

## get a view of data 
head(train_data)
str(train_data)
summary(train_data)

## check for missing values in all columns
MissingCheck <- sapply(train_data, function(x) sum(is.na(x))); 
MissingCheck
MissingCheck[MissingCheck>0]

#### LotFrontage        Alley   MasVnrType   MasVnrArea     BsmtQual     BsmtCond BsmtExposure 
####        259         1369            8            8           37           37           38 
#### BsmtFinType1 BsmtFinType2   Electrical  FireplaceQu   GarageType  GarageYrBlt GarageFinish 
####          37           38            1          690           81           81           81 
#### GarageQual   GarageCond       PoolQC        Fence  MiscFeature 
####          81           81         1453         1179         1406 

plot(train_data$SalePrice, train_data$LotArea)
range(train_data$SalePrice)
#### [1]  34900 755000
range(train_data$LotArea)
#### [1]   1300 215245
library(ggplot2)
qplot(train_data$SalePrice, train_data$LotArea)
ggplot(data=train_data, aes(train_data$SalePrice)) + geom_histogram ()
ggplot(data=train_data, aes(train_data$LotArea)) + geom_histogram ()


ggplot(data=train_data, aes(train_data$OverallQual)) + geom_histogram ()
ggplot(data=train_data, aes(train_data$OverallCond)) + geom_histogram ()

trainData_1 <- train_data[train_data$SalePrice < 442567.01, ]
trainData_2 <- trainData_1[trainData_1$LotArea < 30000, ]
summary(trainData_2)
plot(trainData_2$SalePrice, trainData_2$BsmtCond)
plot(trainData_2$SalePrice, trainData_2$BsmtExposure)
plot(trainData_2$SalePrice, trainData_2$BsmtFinType1)
plot(trainData_2$SalePrice, trainData_2$BsmtFinSF1)

linearModel <- lm( SalePrice ~ LotArea + Neighborhood + OverallQual
                   + YearBuilt + MasVnrType + ExterQual + BsmtQual
                    + YearRemodAdd + BsmtFinSF1 + BsmtExposure
                      + Condition1 + HouseStyle + Foundation
                    + Exterior1st + Exterior2nd , trainData_2)
summary(linearModel)

linearModel2 <- lm( SalePrice ~ LotArea + Neighborhood + OverallQual
                   + YearBuilt + MasVnrType + ExterQual + BsmtQual
                   + YearRemodAdd + BsmtFinSF1 + BsmtExposure
                   + Condition1 + HouseStyle 
                    + Exterior2nd , trainData_2)
summary(linearModel2)

test_data <- read.csv("test.csv")

predicted = predict(linearModel2,test_data)
Id <- test_data$Id
SalePrice <- predicted
graphdata <- data.frame(Id, SalePrice)

write.csv(graphdata, file = "LinearModel2Result.csv", row.names = FALSE)