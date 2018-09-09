# set working directory
setwd("~/MSDS/SYS6018/Competitions/2.HousePrices/sys6018-competition-house-prices")

# import libraries
library(tidyverse)
library(caret)


# Read in the data --------------------------------------------------------

house_prices <- read.csv('../input/train.csv', stringsAsFactors = TRUE)


# Convert the data columns into appropriate formats -----------------------

rate_levels <- 1:10
house_prices$MSSubClass <- factor(house_prices$MSSubClass)
house_prices$OverallQual <- factor(house_prices$OverallQual, levels = rate_levels)
house_prices$OverallCond <- factor(house_prices$OverallCond, levels = rate_levels)
# house_prices$YearBuilt <- factor(house_prices$YearBuilt)
# house_prices$YearRemodAdd <- factor(house_prices$YearRemodAdd)
house_prices$MoSold <- factor(house_prices$MoSold)
# house_prices$YrSold <- factor(house_prices$YrSold)

# replace missing values in factors as NA string and integers as imputed values
cols <- colnames(house_prices)

for (i in 1:ncol(house_prices)){
        df_col <- house_prices[,cols[i]]
        if(is.factor(df_col)){
                # print(i)
                # levels <- levels(df_col)
                level_df <- table(df_col) %>% as.data.frame()
                level_df <- level_df[order(-level_df$Freq),]
                imputed_level <- level_df[1,1]
                df_col[is.na(df_col)] <- imputed_level
        }
        else {
                df_col[is.na(df_col)] = mean(df_col,na.rm = TRUE)
        }
        house_prices[,cols[i]] <- df_col
}


str(house_prices)

# set.seed(1234)
inTrain <- createDataPartition(y=house_prices$SalePrice,
                               p=0.80, list=FALSE)
training <- house_prices[inTrain, ]
validation <- house_prices[-inTrain, ]

validation[,a] <- factor(df_test[,a], levels = levels(df_train[,a]))

pr.lm1 <- lm(SalePrice ~ ., data = training)


getOption("max.print")
options(max.print = 2500)

summary(pr.lm1)

pr.lm2 <- lm(SalePrice ~ YearBuilt  + KitchenQual + GarageType + ScreenPorch + Fireplaces +
                     X1stFlrSF + X2ndFlrSF, data = training)

summary(pr.lm2)
pred <- (predict(pr.lm2, newdata = validation))
expect <- validation$SalePrice

mse2_valid <- mean((expect-pred)^2)
mse2_valid
# [1] 6552447129

# Jingyi
pr.lm3 <- lm(SalePrice ~ YearBuilt + BedroomAbvGr + TotRmsAbvGrd +
                                            + GarageArea + Neighborhood +
                                            KitchenQual + OverallCond, data = training)

summary(pr.lm3)
pred <- (predict(pr.lm3, newdata = validation))
expect <- validation$SalePrice

mse2_valid2 <- mean((expect-pred)^2)
mse2_valid2
# [1] 1772643629

# Alex
pr.lm4 <- lm(SalePrice ~ MSZoning+ LotArea+ Neighborhood+ YearBuilt+
                      GarageCars + OverallQual , data=training)

summary(pr.lm4)
pred <- (predict(pr.lm4, newdata = validation))
expect <- validation$SalePrice

mse2_valid3 <- mean((expect-pred)^2)
mse2_valid3
#[1] 1716235603

# significant change!
# training$OverallQual <- as.numeric(training$OverallQual)
# validation$OverallQual <- as.numeric(validation$OverallQual)

pr.lm5 <- lm(SalePrice ~ OverallQual+ YearBuilt+ GarageCars+ Neighborhood+
                      LotArea + KitchenQual + ScreenPorch + X1stFlrSF + X2ndFlrSF +
                     BsmtFinSF1+BsmtFinSF2 + BsmtUnfSF + RoofMatl  + MSZoning + 
                     PoolArea + GarageType, data=training)

summary(pr.lm5)
pred <- (predict(pr.lm5, newdata = validation))
expect <- validation$SalePrice

mse2_valid4 <- mean((expect-pred)^2)
mse2_valid4

#1102263946
#1230204676