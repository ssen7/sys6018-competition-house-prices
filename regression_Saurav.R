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
house_prices$YearBuilt <- factor(house_prices$YearBuilt)
house_prices$YearRemodAdd <- factor(house_prices$YearRemodAdd)
house_prices$MoSold <- factor(house_prices$MoSold)
house_prices$YrSold <- factor(house_prices$YrSold)

# replace missing values in factors as NA string and integers as imputed values
cols <- colnames(house_prices)

for (i in 1:ncol(house_prices)){
        df_col <- house_prices[,cols[i]]
        if(is.factor(df_col)){
                # print(i)
                levels <- levels(df_col)
                level_df <- table(df_col) %>% as.data.frame()
                level_df <- level_df[order(-level_df$Freq),]
                imputed_level <- level_df[1,1]
                df_col[is.na(df_col)] <- imputed_level
        }
        else {
                df_col[is.na(df_col)] = mean(df_col, na.rm = TRUE)
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
# [1] 6552447129

pr.lm3 <- lm(SalePrice ~ MSSubClass + LotArea + MSZoning + 
                     LandSlope + Neighborhood +
                    OverallQual +
                     YearRemodAdd + MasVnrArea + BsmtQual +
                     BsmtExposure  + BsmtFinSF1 + BsmtFinSF2 + BsmtUnfSF +
                     X1stFlrSF + X2ndFlrSF + ScreenPorch +PoolArea + PoolQC, data = training)

summary(pr.lm3)
pred <- (predict(pr.lm3, newdata = validation))
expect <- validation$SalePrice

mse2_valid2 <- mean((expect-pred)^2)
mse2_valid2
# [1] 2228183393

# No change in mse!

pr.lm4 <- lm(SalePrice ~ MSSubClass  + MSZoning + 
                     Neighborhood +
                     OverallQual +
                     MasVnrArea + BsmtQual +
                     BsmtFinSF1 + BsmtFinSF2 + BsmtUnfSF +
                     X1stFlrSF + X2ndFlrSF , data = training)

summary(pr.lm4)
pred <- (predict(pr.lm4, newdata = validation))
expect <- validation$SalePrice

mse2_valid3 <- mean((expect-pred)^2)
mse2_valid3
#[1] 800617307
# significant change!
