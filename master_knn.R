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
# clarify those variable as factor
rate_levels <- 1:10
house_prices$MSSubClass <- factor(house_prices$MSSubClass)
house_prices$OverallQual <- factor(house_prices$OverallQual, levels = rate_levels)
house_prices$OverallCond <- factor(house_prices$OverallCond, levels = rate_levels)
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

# a function for standardizing quantitative values
# standardization formula citation -> http://www.saedsayad.com/k_nearest_neighbors_reg.htm
stddist <-function(column.i, max, min){       
        result <- (column.i-min)/(max-min)
}
# loop over each column of the numerical variables and standardize them
for (i in 1:ncol(house_price_contin)){
        max <- max(house_price_contin[,i])
        min <- min(house_price_contin[,i])
        # use sapply to apply the standardizing function to the whole column         
        house_price_contin[,i] = sapply(house_price_contin[,i], stddist, max= max, min=min)     
}

# converting categorical variables to one hot encoding for distance calculations.
train_dummy <- dummyVars(~., data=house_price_factor, sep='_', levelsOnly = FALSE)
house_price_factor_onehot <- as.data.frame(predict(train_dummy, house_price_factor))

SalePrice <- house_prices$SalePrice

# creating final dataframe with scaled variable and one hot encoding
training_final <- cbind(house_price_factor_onehot, house_price_contin,SalePrice)


# KNN Implementation ------------------------------------------------------
# read in test csv file 
test <- read.csv('../input/test.csv', stringsAsFactors = TRUE)

# Data Preprocess Test Data
# clarify those variables as categorical variables
rate_levels <- 1:10
test$MSSubClass <- factor(test$MSSubClass)
test$OverallQual <- factor(test$OverallQual, levels = rate_levels)
test$OverallCond <- factor(test$OverallCond, levels = rate_levels)
test$MoSold <- factor(test$MoSold)
# we did not consider year built and year sold as categorical variables so that they could
# be evaluated on a continuous basis

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
# narrow down the variables to be rese
test_sub <- test[,c('OverallQual', 'YearBuilt', 'GarageCars', 'Neighborhood',
                                    'LotArea', 'KitchenQual', 'ScreenPorch', 'X1stFlrSF','X2ndFlrSF',
                                    'BsmtFinSF1','BsmtFinSF2', 'BsmtUnfSF', 'RoofMatl', 'MSZoning',
                                    'PoolArea','GarageType')]

str(test_sub)

# defining the categorical predictors
factors <- c('OverallQual','Neighborhood','KitchenQual','RoofMatl','MSZoning','GarageType')

# separately standardize categorical and quantitative variables
test_sub_contin <- test_sub[,!names(test_sub) %in% factors]
test_sub_factor <- test_sub[,factors]

# standardizing quantitative values
# stddist <-function(column.i, max, min){
#         
#         result <- (column.i-min)/(max-min)
# }

# loop over each column of test_sub_contin to standardize the values
for (i in 1:ncol(test_sub_contin)){
        max <- max(test_sub_contin[,i])
        min <- min(test_sub_contin[,i])
        # use sapply() to apply the standardization function to the whole column
        test_sub_contin[,i] = sapply(test_sub_contin[,i], stddist, max= max, min=min)      
}

# converting categorical variables to one hot encoding for distance calculations.
# train_dummy <- dummyVars(~., data=test_sub_factor, sep='_', levelsOnly = FALSE)
# ^ there were not the same number of levels in testing and training, so we are just using the training levels
test_sub_factor_onehot <- as.data.frame(predict(train_dummy, test_sub_factor))

test_final <- cbind(test_sub_factor_onehot, test_sub_contin)

# set a value to k
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
test_df$distances <- NA

for (observation in (1:nrow(test_df))){         # for each row in the testing data frame, evaluate the following
        ecd <- 0                                # initialize the eucilidean distance
        n <- nrow(training_final)               # n is the number of rows in the training set

        distances_column <- numeric(ncol_input) # initialize a distances vector with the length
                                                        # of the number of variables
        for (i in 1:n){                         # for each row in the training set
                for (col in 1:ncol_input){      # for each column in the training set
                        distances_column[col] <- (test_df[observation,col] - training_df[i,col])^2
                }       # ^ prepare to calculate the eucilidean distance by taking the square of column differences
                ecd <- sqrt(sum(distances_column)) # calculate the eucilidean distance
                training_df$distances[i] <- ecd    # set the eucilidean distance from current observation to the 
                                                        # appropriate position in current distance vector
        }
        
        # sort the training set by the 3 smallest eucilidean distances in order to calculate average of nearest neighbors
        training_df_order <- training_df[order(training_df$distances),] 
        # update the sales price for the current row in the testing data frame to match calculation of average of nearest neighbors 
        test_df$SalePrice[observation] <- mean(training_df_order[1:k, c(y)])
        # append the predicted row to our training data set
        training_df <- rbind(training_df,test_df[observation,] ) # in this version, we fixed the bug where values of old sale prices were being overwritten by new average
        print(observation) # allows us to keep track of the master observation loop's progress
        
}

final_prediction_df <- data.frame('Id' =test$Id)
final_prediction_df$SalePrice <-test_df$SalePrice




