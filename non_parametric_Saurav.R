# set working directory
setwd("~/MSDS/SYS6018/Competitions/2.HousePrices/sys6018-competition-house-prices")

# import libraries
library(tidyverse)
library(caret)
library(onehot)
library(reshape2)

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
                rm(level_df)
        }
        else {
                # fill the missing value of numerical columns as the mean of the whole column
                df_col[is.na(df_col)] = median(df_col,na.rm = TRUE)
        }
        # store the imputed column back to the data frame
        house_prices[,cols[i]] <- df_col
        rm(df_col)
        
}


# Standardizing All Values ------------------------------------------------

# taking all the best variables from our linear model

house_prices_sub <- house_prices[,c('OverallQual', 'YearBuilt', 'GarageCars', 'Neighborhood',
                                            'LotArea', 'KitchenQual', 'ScreenPorch', 'X1stFlrSF','X2ndFlrSF',
                                            'BsmtFinSF1','BsmtFinSF2', 'BsmtUnfSF', 'RoofMatl', 'MSZoning',
                                            'PoolArea','GarageType')]

str(house_prices_sub)

factors <- c('OverallQual','Neighborhood','KitchenQual','RoofMatl','MSZoning','GarageType')

house_price_contin <- house_prices_sub[,!names(house_prices_sub) %in% factors]
house_price_factor <- house_prices_sub[,factors]

# factor_df <- house_prices[,c('Id','OverallQual', 'KitchenQual', 'RoofMatl','MSZoning', 'GarageType')]

# factor_df <- dcast(melt(factor_df, id.vars='Id'), Id ~ variable + value, fun = length)

library(caret)
library(lattice)
library(ggplot2)
train_dummy <- dummyVars(~., data=house_price_factor, sep='_', levelsOnly = FALSE)
tr_dummy <- as.data.frame(predict(train_dummy, house_price_factor))


house_price_contin <- scale(house_price_contin)
tr_dummy
