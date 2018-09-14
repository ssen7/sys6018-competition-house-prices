# set working directory
setwd("~/MSDS/SYS6018/Competitions/2.HousePrices/sys6018-competition-house-prices")

# import libraries
library(tidyverse)
library(caret)

# Read in the data --------------------------------------------------------

# read all string variable as factors.
house_prices <- read.csv('../input/train.csv', stringsAsFactors = TRUE)


# Data Pre-processing -----------------------------------------------------

rate_levels <- 1:10
house_prices$MSSubClass <- factor(house_prices$MSSubClass)
house_prices$OverallQual <- factor(house_prices$OverallQual, levels = rate_levels)
house_prices$OverallCond <- factor(house_prices$OverallCond, levels = rate_levels)
# house_prices$YearBuilt <- factor(house_prices$YearBuilt)
# house_prices$YearRemodAdd <- factor(house_prices$YearRemodAdd)
house_prices$MoSold <- factor(house_prices$MoSold)
# house_prices$YrSold <- factor(house_prices$YrSold)

# replace missing values in factors as NA string and integers as imputed values

# get a list of all col_names
cols <- colnames(house_prices)
# loop over all the columns
for (i in 1:ncol(house_prices)){
        # extract each column into a vector: df_col
        df_col <- house_prices[,cols[i]]
        # if the column is a factor add the most frequent level of the variable as the imputed level
        if(is.factor(df_col)){
                # get frequencies of each level and store it in the data frame
                level_df <- table(df_col) %>% as.data.frame()
                # sort the data frame in descending order
                level_df <- level_df[order(-level_df$Freq),]
                # take the first value of the data frame
                imputed_level <- level_df[1,1]
                # replace all missing values with the imputed level
                df_col[is.na(df_col)] <- imputed_level
        }
        else {
                # fill the missing value of numerical columns as the mean of the whole column
                df_col[is.na(df_col)] = median(df_col,na.rm = TRUE)
        }
        # store the imputed column back to the data frame
        house_prices[,cols[i]] <- df_col
}

# describe the data frame
str(house_prices)

# Create training and validation sets -------------------------------------

# create training and validation sets
# set.seed(1234)
inTrain <- createDataPartition(y=house_prices$SalePrice,
                               p=0.80, list=FALSE)
training <- house_prices[inTrain, ]
validation <- house_prices[-inTrain, ]

# Attempt 1 ---------------------------------------------------------------
# using all the variables as predictors.
pr.lm1 <- lm(SalePrice ~ ., data = training)

# change the max print value
getOption("max.print")
options(max.print = 2500)

# print summary of the linear model
summary(pr.lm1)
# Multiple R-squared:  0.9491,	Adjusted R-squared:  0.9336

# pr.lm2 <- lm(SalePrice ~ YearBuilt  + KitchenQual + GarageType + ScreenPorch + Fireplaces +
#                      X1stFlrSF + X2ndFlrSF, data = training)
# 
# summary(pr.lm2)
# pred <- (predict(pr.lm2, newdata = validation))
# expect <- validation$SalePrice
# 
# mse2_valid <- mean((expect-pred)^2)
# mse2_valid
# [1] 6552447129


# Attempt 2 ---------------------------------------------------------------

pr.lm2 <- lm(SalePrice ~ YearBuilt + BedroomAbvGr + TotRmsAbvGrd +
                     + GarageArea + Neighborhood +
                     KitchenQual + OverallCond, data = training)

summary(pr.lm2)
# Multiple R-squared:  0.7669,	Adjusted R-squared:  0.7589 

pred <- (predict(pr.lm2, newdata = validation))
expect <- validation$SalePrice
mse2_valid_2 <- mean((expect-pred)^2)
mse2_valid_2
# [1] 1376585899


# Attempt 3 ---------------------------------------------------------------

pr.lm3 <- lm(SalePrice ~ MSZoning+ LotArea+ Neighborhood+ YearBuilt+
                     GarageCars + OverallQual , data=training)

summary(pr.lm3)
# Multiple R-squared:  0.7945,	Adjusted R-squared:  0.7872 

pred <- (predict(pr.lm3, newdata = validation))
expect <- validation$SalePrice
mse2_valid_3 <- mean((expect-pred)^2)
mse2_valid_3
#[1] 1716235603

## Changing overall quality and condition ratings into continuous variables did not yield better results
## Hence commenting them out
# training$OverallQual <- as.numeric(training$OverallQual)
# validation$OverallQual <- as.numeric(validation$OverallQual)


# Attempt 4 ---------------------------------------------------------------

pr.lm4 <- lm(SalePrice ~ OverallQual+ YearBuilt+ GarageCars+ Neighborhood+
                     LotArea + KitchenQual + ScreenPorch + X1stFlrSF + X2ndFlrSF +
                     BsmtFinSF1+BsmtFinSF2 + BsmtUnfSF + RoofMatl  + MSZoning + 
                     PoolArea + GarageType, data=training)

summary(pr.lm4)
# Multiple R-squared:  0.9036,	Adjusted R-squared:  0.8982 

pred <- (predict(pr.lm4, newdata = validation))
expect <- validation$SalePrice
mse2_valid_4 <- mean((expect-pred)^2)
mse2_valid_4
# [1] 1285494540

# This is the lowest MSE of all the models we have built so far. 
# Also all the variable are significant based on their p values.

# Proceeding with Attempt 4 as the final model.


# Prediction on Kaggle Test Data ------------------------------------------
pr.final <- lm(SalePrice ~ OverallQual+ YearBuilt+ GarageCars+ Neighborhood+
                     LotArea + KitchenQual + ScreenPorch + X1stFlrSF + X2ndFlrSF +
                     BsmtFinSF1+BsmtFinSF2 + BsmtUnfSF + RoofMatl  + MSZoning + 
                     PoolArea + GarageType, data=house_prices)

test <- read.csv('../input/test.csv', stringsAsFactors = TRUE)

rate_levels <- 1:10
test$MSSubClass <- factor(test$MSSubClass)
test$OverallQual <- factor(test$OverallQual, levels = rate_levels)
test$OverallCond <- factor(test$OverallCond, levels = rate_levels)
# test$YearBuilt <- factor(test$YearBuilt)
# test$YearRemodAdd <- factor(test$YearRemodAdd)
test$MoSold <- factor(test$MoSold)
# test$YrSold <- factor(test$YrSold)

df_col <- colnames(test)

for (i in 1:ncol(test)){
        # extract each column into a vector: df_col
        df_col <- test[,cols[i]]
        # if the column is a factor add the most frequent level of the variable as the imputed level
        if(is.factor(df_col)){
                # get frequencies of each level and store it in the data frame
                level_df <- table(df_col) %>% as.data.frame()
                # sort the data frame in descending order
                level_df <- level_df[order(-level_df$Freq),]
                # take the first value of the data frame
                imputed_level <- level_df[1,1]
                # replace all missing values with the imputed level
                df_col[is.na(df_col)] <- imputed_level
        }
        else {
                # fill the missing value of numerical columns as the mean of the whole column
                df_col[is.na(df_col)] = median(df_col,na.rm = TRUE)
        }
        # store the imputed column back to the data frame
        test[,cols[i]] <- df_col
}


SalePrice <- predict(pr.final, newdata = test)

kaggle_df <- data.frame(Id = test$Id, SalePrice = SalePrice)

write.csv(kaggle_df, 'prediction.csv', row.names = FALSE)
