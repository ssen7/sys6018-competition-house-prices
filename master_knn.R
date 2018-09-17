# import libraries
library(tidyverse)
library(caret)
library(onehot)
library(reshape2)
library(lattice)
library(ggplot2)

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

# defining the categorical predictors
factors <- c('OverallQual','Neighborhood','KitchenQual','RoofMatl','MSZoning','GarageType')

# separately standardize categorical and quantitative variables
house_price_contin <- house_prices_sub[,!names(house_prices_sub) %in% factors]
house_price_factor <- house_prices_sub[,factors]

# standardizing quantitative values
stddist <-function(column.i, max, min){
        
        result <- (column.i-min)/(max-min)
}

for (i in 1:ncol(house_price_contin)){
        max <- max(house_price_contin[,i])
        min <- min(house_price_contin[,i])
        house_price_contin[,i] = sapply(house_price_contin[,i], stddist, max= max, min=min)
        
}

# converting categorical variables to one hot encoding for distance calculations.
train_dummy <- dummyVars(~., data=house_price_factor, sep='_', levelsOnly = FALSE)
house_price_factor_onehot <- as.data.frame(predict(train_dummy, house_price_factor))

SalePrice <- house_prices$SalePrice

# creating final dataframe with scaled variable and one hot encoding
training_final <- cbind(house_price_factor_onehot, house_price_contin,SalePrice)


# KNN Implementation ------------------------------------------------------

test <- read.csv('../input/test.csv', stringsAsFactors = TRUE)

# Data Preprocess Test Data
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


# standardizing test data

test_sub <- test[,c('OverallQual', 'YearBuilt', 'GarageCars', 'Neighborhood',
                                    'LotArea', 'KitchenQual', 'ScreenPorch', 'X1stFlrSF','X2ndFlrSF',
                                    'BsmtFinSF1','BsmtFinSF2', 'BsmtUnfSF', 'RoofMatl', 'MSZoning',
                                    'PoolArea','GarageType')]

str(test_sub)

# defining the categorical predictors
factors <- c('OverallQual','Neighborhood','KitchenQual','RoofMatl','MSZoning','GarageType')

# separately standardize categorical and quantitative variables
test_sub_contin <- test_sub[,!names(house_prices_sub) %in% factors]
test_sub_factor <- test_sub[,factors]

# standardizing quantitative values
# stddist <-function(column.i, max, min){
#         
#         result <- (column.i-min)/(max-min)
# }

for (i in 1:ncol(test_sub_contin)){
        max <- max(test_sub_contin[,i])
        min <- min(test_sub_contin[,i])
        test_sub_contin[,i] = sapply(test_sub_contin[,i], stddist, max= max, min=min)
        
}

# converting categorical variables to one hot encoding for distance calculations.
train_dummy <- dummyVars(~., data=test_sub_factor, sep='_', levelsOnly = FALSE)
test_sub_factor_onehot <- as.data.frame(predict(train_dummy, test_sub_factor))

test_final <- cbind(test_sub_factor_onehot, test_sub_contin)

k = 5
y = 'SalePrice'

ncol_input <- ncol(test_final)

# firstcol = which(colnames(x)=="OverallQual")
# lastcol = which(colnames(x)=="PoolArea")

training_df <- training_final
training_df$distances <- NA

test_df <- test_final
# initialize SalePrice column in the test data frame
test_df$SalePrice <- NA

for (observation in (1:nrow(test_df))){
        ecd <- 0
        n <- nrow(training_final)
        # training_df <- training_final
        # input_df <- sub_validation_data[observation,c(firstcol,lastcol)]
        # input_df <- sub_validation_data[observation,(1:ncol_input)]
        distances_column <- numeric(ncol_input)
        for (i in 1:n){
                for (col in 1:ncol_input){
                        distances_column[col] <- (test_df[observation,col] - training_df[i,col])^2 # need to fix this line
                }
                ecd <- sqrt(sum(distances_column))
                training_df$distances[i] <- ecd
        }
        
        training_df_order <- training_df[order(training_df$distances),]
        test_df$SalePrice[observation] <- mean(training_df_order[1:k, c(y)])
        print(observation)
        # append the predicted row to our training data set
        training_df <- rbind(training_df,test_df$SalePrice[observation] )
        
}





