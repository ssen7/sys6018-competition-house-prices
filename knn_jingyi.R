# Gromadzki, Alex
# Luo, Jingyi
# Sengupta, Saurav

# read training set and test set
setwd("~/Documents/GitHub/UVA_homework/kaggel_house_price_knn")

df_train <- read.csv("train.csv", header=TRUE)
df_test <- read.csv("test.csv", header=TRUE)

############### Data preprocessing for df_train #################

dim(df_train)
ncol(df_train)
# 81
length(df_train$Id)
# 1370
# the number of missing values for each row
results <- as.data.frame(colSums(is.na(df_train)))
missing_col <- subset(results, results != 0)
nrow(missing_col)
# 19
# LotFrontage                        259
# Alley                             1369
# MasVnrType                           8
# MasVnrArea                           8
# BsmtQual                            37
# BsmtCond                            37
# BsmtExposure                        38
# BsmtFinType1                        37
# BsmtFinType2                        38
# Electrical                           1
# FireplaceQu                        690
# GarageType                          81
# GarageYrBlt                         81
# GarageFinish                        81
# GarageQual                          81
# GarageCond                          81
# PoolQC                            1453
# Fence                             1179
# MiscFeature                       1406

# remove columns 'Alley', 'FireplaceQu', PoolQC', 'Fence', 'MiscFeature' because too many missings.

# the categorical data with missings
cat_data <- c('MasVnrType', 'BsmtQual', 'BsmtCond', 'BsmtExposure', 'BsmtFinType1', 'BsmtFinType2', 'Electrical',
              'GarageType', 'GarageFinish', 'GarageQual', 'GarageCond')

# the numerical data with missings
num_data_total <- c('MasVnrArea', 'LotFrontage', 'GarageYrBlt')

# filling the categorical missing values
df <- df_train                                                          # df[achar]: get a column vector with column name
for (achar in cat_data) {                                               # tail(X,1): get the last one element of X.
  df[achar][is.na(df[achar])] <- tail(names(sort(table(df[achar]))), 1) # table(): returns the unique values and their counts for each column
}                                                                       # names(): gets those unique values (here are the categorical levels) 
df_train <- df        # give df back to df_train
results <- as.data.frame(colSums(is.na(df_train)))
missing_col <- subset(results, results != 0)
nrow(missing_col)

# filling the numeric missing values with median value
num_data <- c('MasVnrArea', 'LotFrontage')
df <- df_train
for (achar in num_data) {
  df[achar][is.na(df[achar])] <- median(df[,achar], na.rm = TRUE)  
}                    # a comma in df[,achar] is very necessary to get only the value without header to calculate median
df_train <- df
sum(is.na(df_train$MasVnrArea))
sum(is.na(df_train$LotFrontage))

# filling the 'GarageYrBlt'
year_data <- c('GarageYrBlt')
row_index <- c(which(is.na(df_train$GarageYrBlt)))
for (i in row_index) {
  df_train$GarageYrBlt[i] <- df_train$YearBuilt[i]
}
sum(is.na(df_train$GarageYrBlt))

# remove the five columns with too many missing values
df_train <- df_train[, !(colnames(df_train) %in% c('Alley', 'FireplaceQu', 'PoolQC', 'Fence', 'MiscFeature'))]
sum(is.na(df_train))
# 0

## narrow down the variable to the those
df_train <- df_train[, colnames(df_train) %in% c('Id', 'OverallQual', 'YearBuilt', 'GarageCars', 'Neighborhood', 'LotArea', 
                                                 'KitchenQual', 'ScreenPorch', 'X1stFlrSF', 'X2ndFlrSF', 'BsmtFinSF1',
                                                 'BsmtFinSF2', 'BsmtUnfSF', 'RoofMatl', 'MSZoning', 'PoolArea', 'GarageType', 'SalePrice')]
dim(df_train)
# Dummy variables
# install.packages('caret')
# install.packages('lattice')
# install.packages('ggplot2')
library(caret)
library(lattice)
library(ggplot2)
train_dummy <- dummyVars(~., data=df_train, sep='_', levelsOnly = FALSE)
tr_dummy <- as.data.frame(predict(train_dummy, df_train))
unique(df_train$MSZoning)
# [1] RL      RM      C (all) FV      RH  
# you have to make sure the target variables are factor. e.g. if your Sales$year are numeric, 
# you have to convert them to factor fist: as.factor(Sales$year), then use dummyVars().

# feature scaling using standardization way: x=(x-x.min)/(x.max-x.min)
# use the property of vector
for (k in c(2:(ncol(tr_dummy)-1))) {
  x.max <- max(tr_dummy[, k])
  x.min <- min(tr_dummy[, k])
  x <- tr_dummy[, k]
  x <- (x-x.min)/(x.max-x.min)
  tr_dummy[,k] <- x
}

############### Data preprocessing for df_test #################
results <- as.data.frame(colSums(is.na(df_test)))
missing_col <- subset(results, results != 0)
nrow(missing_col)
# 33
# MSZoning                           4
# LotFrontage                      227
# Alley                           1352
# Utilities                          2
# Exterior1st                        1
# Exterior2nd                        1
# MasVnrType                        16
# MasVnrArea                        15
# BsmtQual                          44
# BsmtCond                          45
# BsmtExposure                      44
# BsmtFinType1                      42
# BsmtFinSF1                         1
# BsmtFinType2                      42
# BsmtFinSF2                         1
# BsmtUnfSF                          1
# TotalBsmtSF                        1
# BsmtFullBath                       2
# BsmtHalfBath                       2
# KitchenQual                        1
# Functional                         2
# FireplaceQu                      730
# GarageType                        76
# GarageYrBlt                       78
# GarageFinish                      78
# GarageCars                         1
# GarageArea                         1
# GarageQual                        78
# GarageCond                        78
# PoolQC                          1456
# Fence                           1169
# MiscFeature                     1408
# SaleType                           1

# remove columns 'Alley', 'FireplaceQu', PoolQC', 'Fence', 'MiscFeature' because too many missings.

# filling the categorical missing values
cat_data <- c('MasVnrType', 'BsmtQual', 'BsmtCond', 'BsmtExposure', 'BsmtFinType1', 'BsmtFinType2',
              'GarageType', 'GarageFinish', 'GarageQual', 'GarageCond', 'MSZoning', 'Utilities', 
              'Exterior1st', 'Exterior2nd', 'KitchenQual', 'Functional', 'SaleType')

df <- df_test                                                          # df[achar]: get a column vector with column name
for (achar in cat_data) {                                               # tail(X,1): get the last one element of X.
  df[achar][is.na(df[achar])] <- tail(names(sort(table(df[achar]))), 1) # table(): returns the unique values and their counts for each column
}                                                                       # names(): gets those unique values (here are the categorical levels) 
df_test <- df        # give df back to df_train
results <- as.data.frame(colSums(is.na(df_test)))
missing_col <- subset(results, results != 0)
nrow(missing_col)
# 16

# filling the numeric missing values using median value
num_data <- c('MasVnrArea', 'LotFrontage', 'BsmtFinSF1', 'BsmtFinSF2', 'BsmtUnfSF', 
              'TotalBsmtSF', 'BsmtFullBath', 'BsmtHalfBath', 'GarageCars', 'GarageArea')
df <- df_test
for (achar in num_data) {
  df[achar][is.na(df[achar])] <- median(df[,achar], na.rm = TRUE)  
}                    # a comma in df[,achar] is very necessary to get only the value without header to calculate median
df_test <- df
results <- as.data.frame(colSums(is.na(df_test)))
missing_col <- subset(results, results != 0)
nrow(missing_col)


# filling the 'GarageYrBlt'
row_index <- c(which(is.na(df_test$GarageYrBlt)))
for (i in row_index) {
  df_test$GarageYrBlt[i] <- df_test$YearBuilt[i]
}

# remove the five columns with too many missing values
df_test <- df_test[, !(colnames(df_test) %in% c('Alley', 'FireplaceQu', 'PoolQC', 'Fence', 'MiscFeature'))]
sum(is.na(df_test))
# 0

# narrow down the variables to research
df_test <- df_test[, colnames(df_test) %in% c('Id', 'OverallQual', 'YearBuilt', 'GarageCars', 'Neighborhood', 'LotArea', 
                                              'KitchenQual', 'ScreenPorch', 'X1stFlrSF', 'X2ndFlrSF', 'BsmtFinSF1',
                                              'BsmtFinSF2', 'BsmtUnfSF', 'RoofMatl', 'MSZoning', 'PoolArea', 'GarageType')]
dim(df_test)

# check level differences
factor_name <- names(Filter(is.factor, df_test))
for (a in factor_name) {
  print(paste(a, nlevels(df_test[,a]), nlevels((df_train[,a]))))
}
# based on the result, factor levels for variables in df_train are all larger than that in df_test.
# if for the case that smaller, this regression can not cover all levels and can not used for prediction. 
# It posssibly shows that variable should not be seen as a categorical one.

# use factor levels in the training set for the test data
factor_name <- names(Filter(is.factor, df_test))
for (a in factor_name) {
  df_test[,a] <- factor(df_test[,a], levels = levels(df_train[,a]))
}

# Dummy variable for categorical data
test_dummy <- dummyVars(~., data=df_test, sep='_', levelsOnly = FALSE) # create a dummy variable object
te_dummy <- as.data.frame(predict(test_dummy, df_test))   # use predict method to apply dummy variable object to df_test
class(te_dummy)

# feature scaling using standardization way: x=(x-x.min)/(x.max-x.min)
for (k in c(2:ncol(te_dummy))) {
  x.max <- max(te_dummy[, k])
  x.min <- min(te_dummy[, k])
  x <- te_dummy[, k]
  x <- (x-x.min)/(x.max-x.min)
  te_dummy[,k] <- x
}


############### KNN for regression #################
# splitting dataset for cross validation
library(caTools)                # import libary caTools for dataset splitting
set.seed(123)
split = sample.split(tr_dummy$YearBuilt, SplitRatio = 0.8)   # the returned value is TRUE(80%)/FALSE(20%)
training = subset(tr_dummy, split == TRUE)                    # if TRUE, for training
validation = subset(tr_dummy, split == FALSE)                  # if FALSE, for test

# create a knn function
predict_price <- c(length=nrow(validation))
validation <- validation[, -1]    # get rid of 'Id' column
training <- training[, -1]
a_training <- training   # a_training used a temparory training set for combind the calculated distance for each row from validation

# create a matrix for a_training to speed up 
num_train_row <- nrow(a_training)      # get the rows counts for a_training
num_train_col <- ncol(a_training) - 1  # get the column counts without 'SalePrice' column for a_training
# create a matrix for a_training
# rep(1:num_train_col, each=num_train_row): 
# 1:num_train_col: a sequence from 1 to num_train_col
# each=num_train_row: every element from the sequence is repeated num_train_row times
# nrow: the number of rows for the matrix
# by default, the matrix is filled by column.
# here, each=num_train_row, nrow=num_train_row (repetition equals row number), and matrix filled by column, so the dimension of the 
## resulted matrix is num_train_col * num_train_row, and the value of each column is the same with the column's index.
train_matrix <- matrix(rep(1:num_train_col, each=num_train_row), nrow = num_train_row)

# append each element from a_training to the new training matrix
# loop over each row of a_training
for (j in c(1:num_train_row)){
  # get a row from a_training & change it to a numeric list. The numeric list is favorable for mathematical operation(speed up calculation)
  a_row <- as.numeric(a_training[i, -ncol(a_training)]) # -ncol(a_training): means excluding the 'SalePrice'
  # loop over each column of a_row to get one element
  for (i in c(1:num_train_col)) {
    # append an element in row j and column i to the corresponding position of the new matrix
    train_matrix[j, i] <- a_row[i]
  }
}

# loop over each row of validation
k = 5
for (j in c(1:nrow(validation))){  
  a_row <- as.numeric(validation[j,-ncol(training)]) # get one row without 'SalePrice', and change it to numeric list to speed up
  dist_i <- numeric(length=nrow(a_training)) # create a numeric vector to place calculated distance for one row of validation with
  # all rows of training matrix
  # loop over each row of the training matrix 
  for (i in c(1:nrow(a_training))) {
    #train_row <- as.numeric(a_training[i, -ncol(a_training)])
    train_row <- train_matrix[i,]   # get one row from the training matrix
    dist_i[i] <- sqrt(sum(a_row-train_row)^2)  # get the euclidean distance of the two rows and place the distance in dist_i 
  }
  a_training$distance <- dist_i # append the dis_i to a_training
  a_sorted_training <- a_training[order(a_training$distance),] # sort a_training based on distance
  ave_k <- mean(a_sorted_training[1:k, 'SalePrice']) # get the average of five 'SalePrice' related to the first five shortest distances
  predict_price[j] <- ave_k # append the predicted price for the first row of validation to predict_price
}
#cbind(validation$SalePrice, predict_price)

# calculate mse
mse <- mean((validation$SalePrice-predict_price)^2)
rms <- sqrt(mse)
rms

# rms = 78905.65 (k=6)
# rms = 74226.26 (k=5)
# rms = 76246.1  (k=4)

############### Prediction(Test set) #################
# prediction for test set
prediction <- c(length=nrow(te_dummy$Id))
validation <- te_dummy[, -1] # here validation is the test set. -1 is to exclude 'Id' column
training <- tr_dummy[, -1]  # for predicting test set, use the whole training set to predict
a_training <- training

# create a training matrix for the whole train set(both training and valiation)
num_train_row <- nrow(a_training)
num_train_col <- ncol(a_training) - 1
train_matrix <- matrix(rep(1:num_train_col, each=num_train_row), nrow = num_train_row)

# append each element from a_training to the corresponding position of the new matrix
for (j in c(1:num_train_row)){
  a_row <- as.numeric(a_training[i, -ncol(a_training)])
  for (i in c(1:num_train_col)) {
    train_matrix[j, i] <- a_row[i]
  }
}

# get the distance for each row of valiation
k = 5
for (j in c(1:nrow(validation))){
  a_row <- as.numeric(validation[j,])
  dist_i <- numeric(length=nrow(a_training))
  for (i in c(1:nrow(a_training))) {
    #train_row <- as.numeric(a_training[i, -ncol(a_training)])
    train_row <- train_matrix[i,]
    dist_i[i] <- sqrt(sum(a_row-train_row)^2)  # calculate the eucledean distance
  }
  a_training$distance <- dist_i
  a_sorted_training <- a_training[order(a_training$distance),] # sort the training set based on distance
  ave_k <- mean(a_sorted_training[1:k, 'SalePrice'])  # get the averaged distance
  prediction[j] <- ave_k   # the final prediction of 'SalePrice' for validation
}

# create a dataframe containing 'Id' and 'SalePrice'
ka_df <- data.frame(Id=te_dummy$Id, SalePrice=prediction)

# save the dataframe as a csv file
write.csv(ka_df, 'prediction.csv', row.names = FALSE)
