
# Gromadzki, Alex
# Luo, Jingyi
# Sengupta, Saurav


# set working directory
#setwd("~/UVA/DSI/Fall 2018/SYS/Kaggle Competion 1/individual work gromadzki/housing all")
setwd("~/Desktop/sys6018-competition-house-prices")
# import libraries
library(tidyverse)
library(caret)
# library(onehot)
# library(reshape2)

# Read in the data --------------------------------------------------------

# read all string variable as factors.
house_prices <- read.csv('train.csv', stringsAsFactors = TRUE)

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
names(training_final)


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

# ==== Breaking up dataset into Training and Validation Data ====


# With KNN, we should probably start with more than 50% of the data.  I am going to split 70-30 Tr-Val

# 1460*.7 # got 70% of the data as training
sub <- sample(1:1460,size=1022)
sub_training_data <- training_final[sub,]
sub_validation_data <- training_final[-sub,]


###########################- No longer Working code - ###################


k = 3
y = 'SalePrice'

ncol_input <- ncol(sub_validation_data) - 1
# ncol_training <- ncol(sub_training_data)
training_df <- sub_training_data

# sub_training_data$distances <- NA
sub_training_data$PredictPrice <- sub_training_data$SalePrice


# firstcol = which(colnames(x)=="OverallQual")
# lastcol = which(colnames(x)=="PoolArea")



for (observation in (1:nrow(sub_validation_data))){
        ecd <- 0
        n <- nrow(sub_training_data)
        training_df <- sub_training_data
        # input_df <- sub_validation_data[observation,c(firstcol,lastcol)]
        input_df <- sub_validation_data[observation,(1:ncol_input)]
        
        distances_column <- numeric(ncol_input)
        for (single.row in 1:n){
              for (col in 1:ncol_input){
                distances_column[col] <- (input_df[1,col] - training_df[single.row,col])^2 # need to fix this line
              }
          ecd <- sqrt(sum(distances_column))
          training_df$distances[single.row] <- ecd
        }
        
        # the issue that I had was that I need to iterate through each of the lines in order to get the distance,
        # whereas I am only using the first row of the training df right now for each column

        training_df_order <- training_df[order(training_df$distances),]
        # sub_training_data$distances <- NULL
        input_df$SalePrice <- sub_validation_data$SalePrice[observation]
        input_df$PredictPrice <- mean(training_df_order[1:k, ncol_input+1])
        sub_training_data <- rbind(sub_training_data,input_df)
        
        print(as.character(observation))
}

write.table(sub_training_data, file = "train-val-knn.csv", row.names=F, sep=",")

###########################- Working code End - ###################


# 
# knn_custom <- function(input_df, training_df, k, y){
#         ncol_input <- ncol(input_df)
#         ncol_training <- ncol(training_df)
#         training_df$distances <- NA
#         for (rows in 1:nrow(training_df)){
#                 ecd <- 0
#                 distances_column <- numeric(ncol_input)
#                 for (col in 1:ncol_input){
#                         distances_column[col] <- (input_df[1,col] - training_df[rows,col])^2
#                         
#                 }
#                 ecd <- sqrt(sum(distances_column))
#                 training_df$distances[rows] <- ecd
#         }
#         # distances <- sqrt(sum((input_df - training_df[,1:ncol(input_df)] )**2))
#         # training_df$distances <- distances
#         training_df_order <- training_df[order(training_df$distances),]
#         # take the first k rows and average
#         prediction <- mean(training_df_order$distances[1:k])
#         return(prediction)
#         
# }
# 
# knn_custom(input_df, training_df, 7,y)
