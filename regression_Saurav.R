setwd("~/MSDS/SYS6018/Competitions/2.HousePrices/sys6018-competition-house-prices")

library(tidyverse)
library(caret)

house_prices <- read.csv('../input/train.csv', stringsAsFactors = TRUE)

rate_levels <- 1:10
house_prices$MSSubClass <- factor(house_prices$MSSubClass)
house_prices$OverallQual <- factor(house_prices$OverallQual, levels = rate_levels)
house_prices$OverallCond <- factor(house_prices$OverallCond, levels = rate_levels)
house_prices$YearBuilt <- factor(house_prices$YearBuilt)
house_prices$YearRemodAdd <- factor(house_prices$YearRemodAdd)
house_prices$MoSold <- factor(house_prices$MoSold)
house_prices$YrSold <- factor(house_prices$YrSold)

cols <- colnames(house_prices)

for (i in 1:ncol(house_prices)){
        df_col <- house_prices[,cols[i]]
        if(is.factor(df_col)){
                # print(i)
                levels <- levels(df_col)
                levels[length(levels) + 1] <- "NA"
                df_col <- factor(df_col, levels = levels)
                df_col[is.na(df_col)] <- "NA"
        }
        else {
                df_col[is.na(df_col)] = 0
        }
        house_prices[,cols[i]] <- df_col
}

str(house_prices)

# set.seed(1234)
inTrain <- createDataPartition(y=house_prices$SalePrice,
                               p=0.80, list=FALSE)
training <- house_prices[inTrain, ]
validation <- house_prices[-inTrain, ]

pr.lm1 <- lm(SalePrice ~ ., data = training)



getOption("max.print")
options(max.print = 2500)

summary(pr.lm1)

pr.lm2 <- lm(SalePrice ~ MSSubClass + LotArea + MSZoning + 
                     LotConfig + LandSlope + Neighborhood +
                     Condition1  + HouseStyle + OverallQual +
                     YearRemodAdd + RoofMatl + MasVnrArea + BsmtQual +
                     BsmtExposure + BsmtFinType1 + BsmtFinSF1 + BsmtFinSF2 + BsmtUnfSF +
                     X1stFlrSF + X2ndFlrSF + ScreenPorch +PoolArea + PoolQC +
                     MiscFeature, data = training)

summary(pr.lm2)
#+ Condition2
pred <- (predict(pr.lm2, newdata = validation))
expect <- validation$SalePrice

mse2_valid <- mean((expect-pred)^2)
mse2_valid

pr.lm3 <- lm(SalePrice ~ MSSubClass + LotArea + MSZoning + 
                     LandSlope + Neighborhood +
                    OverallQual +
                     YearRemodAdd + RoofMatl + MasVnrArea + BsmtQual +
                     BsmtExposure  + BsmtFinSF1 + BsmtFinSF2 + BsmtUnfSF +
                     X1stFlrSF + X2ndFlrSF + ScreenPorch +PoolArea + PoolQC, data = training)

summary(pr.lm3)
#+ Condition2
pred <- (predict(pr.lm3, newdata = validation))
expect <- validation$SalePrice

mse2_valid2 <- mean((expect-pred)^2)
mse2_valid2

# No change in mse!
