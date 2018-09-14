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

# ==== Standardizing Distance ====
# house_price_contin <- scale(house_price_contin) we manually calculate the scale below

# standardized distance formula: (x-min)/(max-min)
# use sapply for each column

stddist <-function(column.i, max, min){
        
        result <- (column.i-min)/(max-min)
}

for (i in 1:ncol(house_price_contin)){
        max <- max(house_price_contin[,i])
        min <- min(house_price_contin[,i])
        house_price_contin[,i] = sapply(house_price_contin[,i], stddist, max= max, min=min)
        
}

scaled_data <- house_price_contin

SalePrice <- house_prices$SalePrice
training_final <- cbind(tr_dummy, scaled_data,SalePrice)
names(training_df)

# euclideanDist <- function(v, tr){
#         d = sqrt(sum(v-tr)^2)
#         return(d)
# }

# Algorithm for finding K nearest neighbors
# 1) Start with a new vector from our validation set (does not contain the Sales Price)
# Note: this means that the vector should be one less
# 2) Take the difference between each element xi in the vector and the remaining rows in training set
# 3) Keep track of K number of minimum distances between specific rows and the new vector

# input_df: new vector
# ncol_input: 1 to p (we do not have an outcome for each new value)
# ncol_training 1 to p+1 (included the outcome variable 'price')
# distances is another vector: from input df to training df
# y: continuous outcome variable (here, we are looking for price)

## Saurav
knn_custom <- function(input_df, training_df, k, y){
        pred <- c()
        ncol_input <- ncol(input_df)
        ncol_training <- ncol(training_df)
        distances <- sqrt(sum((input_df - training_df[,1:ncol_input] )**2))
        training_df$distances <- distances
        training_df_order <- training_df[order(distances),]
        # take the first k rows and average
        input_df$Prediction <- mean(training_df_order[1:k, ncol_input+1])
        
}

input_df <- training_final[1,(1:ncol(training_final)-1)] 
training_df <- training_final[2:nrow(training_final),]
k = 3
y = 'SalePrice'

knn_custom(input_df, training_df, k,y)

