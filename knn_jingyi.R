# read training set and test set
setwd("~/Desktop/sys6018-competition-house-prices")
df_train <- read.csv("train.csv", header=TRUE)
df_test <- read.csv("test.csv", header=TRUE)

############### Data preprocessing for df_train #################

dim(df_train)
ncol(df_train)
# 81
length(df_train$Id)
# 1370
# the number of missing values for each row
as.data.frame(colSums(is.na(df_train)))
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

# What left to impute:
# MasVnrType                           8 (impute with the most frequenct category)
# MasVnrArea                           8 (impute with the average of the column)
# LotFrontage                        259 (impute with the average of the column)
# BsmtQual                            37 (impute with the most frequenct category)
# BsmtCond                            37 (impute with the most frequenct category)
# BsmtExposure                        38 (impute with the most frequenct category)
# BsmtFinType1                        37 (impute with the most frequenct category)
# BsmtFinType2                        38 (impute with the most frequenct category)
# Electrical                           1 (impute with the most frequenct category)
# GarageType                          81 (impute with the most frequenct category)
# GarageYrBlt                         81 (impute with the corresponding 'YearBuilt')
# GarageFinish                        81 (impute with the most frequenct category)
# GarageQual                          81 (impute with the most frequenct category)
# GarageCond                          81 (impute with the most frequenct category)

# impute the missing values for the 11 variables
#table(df_train$BsmtQual)
# Ex  Fa  Gd  TA 
# 121  35 618 649
#sort(table(df_train$BsmtQual))
# Fa  Ex  Gd  TA 
# 35 121 618 649
#names(sort(table(df_train$BsmtQual)))
# "Fa" "Ex" "Gd" "TA"
#tail(names(sort(table(df_train$BsmtQual))), 1) # get the last 1 element.
# "TA"

# impute the missing values
#df_train$BsmtQual[is.na(df_train$BsmtQual)] <- 'TA'
# check if the missing value from 'KitchenQual' is imputed
#sum(is.na(df_train$BsmtQual))
# 0

# filling the categorical missing values
df_train$MasVnrType[is.na(df_train$MasVnrType)] <- tail(names(sort(table(df_train$MasVnrType))), 1)
df_train$BsmtQual[is.na(df_train$BsmtQual)] <- tail(names(sort(table(df_train$BsmtQual))), 1)
df_train$BsmtCond[is.na(df_train$BsmtCond)] <- tail(names(sort(table(df_train$BsmtCond))), 1)
df_train$BsmtExposure[is.na(df_train$BsmtExposure)] <- tail(names(sort(table(df_train$BsmtExposure))), 1)
df_train$BsmtFinType1[is.na(df_train$BsmtFinType1)] <- tail(names(sort(table(df_train$BsmtFinType1))), 1)
df_train$BsmtFinType2[is.na(df_train$BsmtFinType2)] <- tail(names(sort(table(df_train$BsmtFinType2))), 1)
df_train$Electrical[is.na(df_train$Electrical)] <- tail(names(sort(table(df_train$Electrical))), 1)
df_train$GarageType[is.na(df_train$GarageType)] <- tail(names(sort(table(df_train$GarageType))), 1)
df_train$GarageFinish[is.na(df_train$GarageFinish)] <- tail(names(sort(table(df_train$GarageFinish))), 1)
df_train$GarageQual[is.na(df_train$GarageQual)] <- tail(names(sort(table(df_train$GarageQual))), 1)
df_train$GarageCond[is.na(df_train$GarageCond)] <- tail(names(sort(table(df_train$GarageCond))), 1)
as.data.frame(colSums(is.na(df_train)))

# filling the numeric missing values
df_train$MasVnrArea <- ifelse(is.na(df_train$MasVnrArea), 
                              ave(df_train$MasVnrArea, FUN=function(x) mean(x, na.rm = TRUE)),
                              df_train$MasVnrArea)
df_train$LotFrontage <- ifelse(is.na(df_train$LotFrontage), 
                               ave(df_train$LotFrontage, FUN=function(x) mean(x, na.rm = TRUE)),
                               df_train$LotFrontage)
as.data.frame(colSums(is.na(df_train)))

# filling the 'GarageYrBlt'
row_index <- c(which(is.na(df_train$GarageYrBlt)))
for (i in row_index) {
  df_train$GarageYrBlt[i] <- df_train$YearBuilt[i]
}
#sum(is.na(df_train$GarageYrBlt))

# remove the five columns with too many missing values
df_train <- df_train[, !(colnames(df_train) %in% c('Alley', 'FireplaceQu', 'PoolQC', 'Fence', 'MiscFeature'))]
sum(is.na(df_train))
# 0

# Dummy variable for categorical data
# a test
#dummy()
#dummies <- as.data.frame.matrix(table(1: length(df_train$MSZoning), as.factor(df_train$MSZoning)))
#unique(df_train$MSZoning)
#  C(all) FV RH RL RM

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

# feature scaling
#tr_dummy[1, ncol(tr_dummy)]
tr_dummy[, 2:(ncol(tr_dummy)-1)] = scale(tr_dummy[, 2:(ncol(tr_dummy)-1)])
tr_dummy
dim(tr_dummy)
# [1] 1460 61

############### Data preprocessing for df_test #################

as.data.frame(colSums(is.na(df_test)))
# the same missing features with df_train, but different numbers of missings(second column for df_test)
# LotFrontage                        259     227  (impute with the average of the column)
# Alley                             1369     1352  -remove
# MasVnrType                           8     16   (impute with the most frequenct category)
# MasVnrArea                           8     15   (impute with the average of the column)
# BsmtQual                            37     44   (impute with the most frequenct category)
# BsmtCond                            37     45   (impute with the most frequenct category)
# BsmtExposure                        38     44   (impute with the most frequenct category)
# BsmtFinType1                        37     42   (impute with the most frequenct category)
# BsmtFinType2                        38     42   (impute with the most frequenct category)
# Electrical                           1
# FireplaceQu                        690     730  -remove
# GarageType                          81     76   (impute with the most frequenct category)
# GarageYrBlt                         81     78   (impute with the corresponding 'YearBuilt')
# GarageFinish                        81     78   (impute with the most frequenct category)
# GarageQual                          81     78   (impute with the most frequenct category)
# GarageCond                          81     78   (impute with the most frequenct category)
# PoolQC                            1453     1456  -remove
# Fence                             1179     1169  -remove
# MiscFeature                       1406     1408  -remove

# remove columns 'Alley', 'FireplaceQu', PoolQC', 'Fence', 'MiscFeature' because too many missings.

# new missing features for df_test
# MSZoning                            4 (impute with the most frequenct category)
# Utilities                           2 (impute with the most frequenct category)
# Exterior1st                         1 (impute with the most frequenct category)
# Exterior2nd                         1 (impute with the most frequenct category)
# BsmtFinSF1                          1 (impute with the average of the column)
# BsmtFinSF2                          1 (impute with the average of the column)
# BsmtUnfSF                           1 (impute with the average of the column)
# TotalBsmtSF                         1 (impute with the average of the column)
# BsmtFullBath                        2 (impute with the average of the column-(round?)
# BsmtHalfBath                        2 (impute with the average of the column-(round?)
# KitchenQual                         1 (impute with the most frequenct category)
# Functional                          2 (impute with the most frequenct category)
# GarageCars                          1 (impute with the average of the column-(round?)
# GarageArea                          1 (impute with the average of the column)
# SaleType                            1 (impute with the most frequenct category)

# filling the categorical missing values
# the same with df_train
df_test$MasVnrType[is.na(df_test$MasVnrType)] <- tail(names(sort(table(df_test$MasVnrType))), 1)
df_test$BsmtQual[is.na(df_test$BsmtQual)] <- tail(names(sort(table(df_test$BsmtQual))), 1)
df_test$BsmtCond[is.na(df_test$BsmtCond)] <- tail(names(sort(table(df_test$BsmtCond))), 1)
df_test$BsmtExposure[is.na(df_test$BsmtExposure)] <- tail(names(sort(table(df_test$BsmtExposure))), 1)
df_test$BsmtFinType1[is.na(df_test$BsmtFinType1)] <- tail(names(sort(table(df_test$BsmtFinType1))), 1)
df_test$BsmtFinType2[is.na(df_test$BsmtFinType2)] <- tail(names(sort(table(df_test$BsmtFinType2))), 1)
df_test$GarageType[is.na(df_test$GarageType)] <- tail(names(sort(table(df_test$GarageType))), 1)
df_test$GarageFinish[is.na(df_test$GarageFinish)] <- tail(names(sort(table(df_test$GarageFinish))), 1)
df_test$GarageQual[is.na(df_test$GarageQual)] <- tail(names(sort(table(df_test$GarageQual))), 1)
df_test$GarageCond[is.na(df_test$GarageCond)] <- tail(names(sort(table(df_test$GarageCond))), 1)
# new for df_test
df_test$MSZoning[is.na(df_test$MSZoning)] <- tail(names(sort(table(df_test$MSZoning))), 1)
df_test$Utilities[is.na(df_test$Utilities)] <- tail(names(sort(table(df_test$Utilities))), 1)
df_test$Exterior1st[is.na(df_test$Exterior1st)] <- tail(names(sort(table(df_test$Exterior1st))), 1)
df_test$Exterior2nd [is.na(df_test$Exterior2nd )] <- tail(names(sort(table(df_test$Exterior2nd ))), 1)
df_test$KitchenQual[is.na(df_test$KitchenQual)] <- tail(names(sort(table(df_test$KitchenQual))), 1)
df_test$Functional[is.na(df_test$Functional)] <- tail(names(sort(table(df_test$Functional))), 1)
df_test$SaleType[is.na(df_test$SaleType)] <- tail(names(sort(table(df_test$SaleType))), 1)

# filling the numeric missing values
df_test$MasVnrArea <- ifelse(is.na(df_test$MasVnrArea), 
                             ave(df_test$MasVnrArea, FUN=function(x) mean(x, na.rm = TRUE)),
                             df_test$MasVnrArea)
df_test$LotFrontage <- ifelse(is.na(df_test$LotFrontage), 
                              ave(df_test$LotFrontage, FUN=function(x) mean(x, na.rm = TRUE)),
                              df_test$LotFrontage)
df_test$BsmtFinSF1 <- ifelse(is.na(df_test$BsmtFinSF1), 
                             ave(df_test$BsmtFinSF1, FUN=function(x) mean(x, na.rm = TRUE)),
                             df_test$BsmtFinSF1)
df_test$BsmtFinSF2 <- ifelse(is.na(df_test$BsmtFinSF2), 
                             ave(df_test$BsmtFinSF2, FUN=function(x) mean(x, na.rm = TRUE)),
                             df_test$BsmtFinSF2)
df_test$BsmtUnfSF <- ifelse(is.na(df_test$BsmtUnfSF), 
                            ave(df_test$BsmtUnfSF, FUN=function(x) mean(x, na.rm = TRUE)),
                            df_test$BsmtUnfSF)
df_test$TotalBsmtSF <- ifelse(is.na(df_test$TotalBsmtSF), 
                              ave(df_test$TotalBsmtSF, FUN=function(x) mean(x, na.rm = TRUE)),
                              df_test$TotalBsmtSF)
df_test$BsmtFullBath <- ifelse(is.na(df_test$BsmtFullBath), 
                               ave(df_test$BsmtFullBath, FUN=function(x) mean(x, na.rm = TRUE)),
                               df_test$BsmtFullBath)
df_test$BsmtHalfBath <- ifelse(is.na(df_test$BsmtHalfBath), 
                               ave(df_test$BsmtHalfBath, FUN=function(x) mean(x, na.rm = TRUE)),
                               df_test$BsmtHalfBath)
df_test$GarageCars <- ifelse(is.na(df_test$GarageCars), 
                             ave(df_test$GarageCars, FUN=function(x) mean(x, na.rm = TRUE)),
                             df_test$GarageCars)
df_test$GarageArea <- ifelse(is.na(df_test$GarageArea), 
                             ave(df_test$GarageArea, FUN=function(x) mean(x, na.rm = TRUE)),
                             df_test$GarageArea)

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

# feature scaling
te_dummy[, 2:(ncol(te_dummy))] = scale(te_dummy[, 2:(ncol(te_dummy))])
te_dummy
dim(te_dummy)
# [1] 1460  60
class(te_dummy)

############### KNN for regression #################
# splitting dataset for cross validation
library(caTools)                # import libary caTools for dataset splitting
set.seed(123)
split = sample.split(tr_dummy$YearBuilt, SplitRatio = 0.8)   # the returned value is TRUE(80%)/FALSE(20%)
training = subset(tr_dummy, split == TRUE)                    # if TRUE, for training
validation = subset(tr_dummy, split == FALSE)                  # if FALSE, for test

# Create a Euclidean distance function
# v: a vector row of validation data
# tr: a vector row of train data
euclideanDist <- function(v, tr){
  d = sqrt((v-tr)^2)
  return(d)
}

# create a knn function
knn_func <- function(test_data, train_data, k) {
  pred <- c()
  for (i in c(1:nrow(test_data))) {
    dis_j <- c()
    for (j in c(1:nrow(train_data))) {
      eu_dis <- euclideanDist(test_data[i,], train_data[j,])
      dis_j <- c(dis_j, eudis)
    }
    dis_j <- dis_j(order(dis_j))
    pred <- c(pred, mean(dis_j[1:k]))
  }
  return(pred)
}

# accuracy of knn_func ???
validation$pred <- pred
mse_valid <- mean((validation$SalePrice-pred)^2)