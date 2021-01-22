library(ggplot2) # Data visualization 
library(readr) # CSV file I/0, e.g. the read_csv function 
# Input data files are available in the  " ../input/" directory.
# For example, running this (by clicking run or pressing Shift+Enter) will list the files in the input directory 

# Any results you write to the current directory are saved as output .
train = read.csv(" ../input/train.csv")
test = readcsv("../input/test.csv")


str(train)
str(test)

library(lattice)
library(grid)
library(DMwR)

trainImputeN = train 

# Central imputation is done on the train dataset to fit the regression and find statistical significant variables 
trainCentral = centralImputation(train)

lm.fit = lm(SalePrice~., data = trainCentral)
summary(lm.fit)
par(mfrow=c(2,2))
plot(lm.fit)
# Select statistical significant varaiable and ignore others ( ignoring p-value > 0.05)

# Dummyfying few statsitical significant variables
library(dummies)

KitchenQualVars = dummy(trainImputeN$KitchenQual)
trainImputeN = cbind(trainImputeN, KitchenQualVars)
trainImputeN = subset(trainImputeN, select =-c(KitchenQual))

GarageQualVars = dummy(trainImputeN$GarageQual)
trainImputeN = cbind(trainImputeN, GarageQualVars)
trainImputeN = subset(trainImputeN, select = -c(GarageQual))

# Also observed heteroscedasticity in the residual plots. Hence taking SalePrice^1/4 as the newly transformed response for predictors 
# Increasing the power of LotArea and YearBuilt and run the regression few times to identify that they are all statistically significant.
# With these observations, the new regression fit would be as below - 

lm.fit = lm((SalePrice^(1/4))~MSZoning+LotArea+GarageArea+LandSlope+Neighborhood+Condition1+YearBuilt+OverallQual+YearBuilt*OverallCond+YearRemodAdd+RoofMatl+BsmtFinSF1+BsmtFinSF2+BsmtUnfSF+X1stFlrSF+X2ndFlrSF+KitchenQualEx+KitchenQualFa+KitchenQualGd+ScreenPorch+PoolArea+GarageQualFa+GarageQualGd+GarageQualPo+GarageQualTA+WoodDeckSF+SaleCondition+I(LotArea^2)+I(LotArea^3)+I(LotArea^4)+I(LotArea^5)+I(LotArea^6)+I(YearBuilt^2)+I(YearBuilt^3)+I(YearBuilt^4), data = trainImputeN)
summary(lm.fit)
par(mfrow=c(2,2))
plot(lm.fit)

# Removing the outliers and high leverage points
trainImputeN = trainImputeN[-c(314,707,524,89,633),]

lm.fit = lm((SalePrice^(1/4))~MSZoning+LotArea+GarageArea+LandSlope+Neighborhood+Condition1+YearBuilt+OverallQual+YearBuilt*OverallCond+YearRemodAdd+RoofMatl+BsmtFinSF1+BsmtFinSF2+BsmtUnfSF+X1stFlrSF+X2ndFlrSF+KitchenQualEx+KitchenQualFa+KitchenQualGd+ScreenPorch+PoolArea+GarageQualFa+GarageQualGd+GarageQualPo+GarageQualTA+WoodDeckSF+SaleCondition+I(LotArea^2)+I(LotArea^3)+I(LotArea^4)+I(LotArea^5)+I(YearBuilt^2)+I(YearBuilt^3), data = trainImputeN)
summary(lm.fit)
par(mfrow=c(2,2))
plot(lm.fit)

# Removing the outliers and high leverage points
trainImputeN = trainImputeN[-c(250,336,463,1325),]

lm.fit = lm((SalePrice^(1/4))~MSZoning+LotArea+GarageArea+LandSlope+Neighborhood+Condition1+YearBuilt+OverallQual+YearBuilt*OverallCond+YearRemodAdd+RoofMatl+BsmtFinSF1+BsmtFinSF2+BsmtUnfSF+X1stFlrSF+X2ndFlrSF+KitchenQualEx+KitchenQualFa+KitchenQualGd+ScreenPorch+PoolArea+GarageQualFa+GarageQualGd+GarageQualPo+GarageQualTA+WoodDeckSF+SaleCondition+I(LotArea^2)+I(LotArea^3)+I(LotArea^4)+I(LotArea^5)+I(YearBuilt^2)+I(YearBuilt^3), data = trainImputeN)
summary(lm.fit)
par(mfrow=c(2,2))
plot(lm.fit)

# Imputing the values for test dataset
testImpute = test

boxplot(test$YearBuilt~test$MSZoning)
# The yearbulit  for 791 row of test dataset is 1900. From the box plot, infer that the zoning type = RM 
testImpute[c(791),"MSZoning"] = "RM"

# The yearbulit  for 791 row of test dataset is 1910,1952,1951. From the box plot, infer that the zoning type = RM 
testImpute[c(456,757,1445),"MSZoning"] = "RM"


KitchenQualVars = dummy(testImpute$KitchenQual)
testImpute = cbind(testImpute,KitchenQualVars)
testImpute  = subset(testImpute, select=-c(KitchenQual))

GarageQualVars = dummy(testImpute$GarageQual)
testImpute = cbind(testImpute,GarageQualVars)
testImpute  = subset(testImpute, select=-c(GarageQual))
