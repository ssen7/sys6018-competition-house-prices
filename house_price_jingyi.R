# read training set and test set
setwd("~/Desktop/sys6018-competition-house-prices")
# reading in training dataset and test dataset
df_train <- read.csv("train.csv", header=TRUE)
df_test <- read.csv("test.csv", header=TRUE)

############### Data Preprocessing #################

# get the number of total variables and length of dataset
ncol(train)
# 81
length(df_test[, 'Id'])
# 1459
#colnames(train)[ncol(train)]

# get the number of missing values for each column
#colSums(is.na(df_train))

# create a subset with 8 independent & 1 dependent variable to be regressed
df_train_subset <- subset(df_train, select=c('YearBuilt','BedroomAbvGr', 'TotRmsAbvGrd',
                                             'GarageArea', 'Neighborhood', 'HouseStyle', 
                                             'KitchenQual', 'OverallCond', 'SalePrice'))
# get the number of missing values for each column for the subset
# no missing value for this subset of training data
colSums(is.na(df_train_subset))

# splitting dataset for cross validation
library(caTools)          # import libary caTools for dataset splitting
set.seed(123)
split = sample.split(df_train_subset$YearBuilt, SplitRatio = 0.8)   # the returned value is TRUE(80%)/FALSE(20%)
training_set = subset(df_train_subset, split == TRUE)               # if TRUE, for training
test_set = subset(df_train_subset, split == FALSE)                  # if FALSE, for test

# categorical data
df_train_subset$Neighborhood <- factor(df_train_subset$Neighborhood)
df_train_subset$HouseStyle <- factor(df_train_subset$HouseStyle)
df_train_subset$KitchenQual <- factor(df_train_subset$KitchenQual)

############### Multiple Linear Regression #################

# linear model regression using training set
regressor = lm(formula = SalePrice ~., data=training_set)
summary(regressor)
# based on P values, 'HouseStyle' is insignificant compared to others. 

# prediction using test set
y_pred <- predict(regressor, newdata = test_set[,-which(names(test_set)=='SalePrice')]) # which():get the index of 'SalePrice'
cbind(test_set$SalePrice, y_pred)


############### Predict the test dataset #################
# narrow down the variables
df_test_subset <- subset(df_test, select=c('YearBuilt','BedroomAbvGr', 'TotRmsAbvGrd',
                                           'GarageArea', 'Neighborhood', 'HouseStyle', 
                                           'KitchenQual', 'OverallCond'))
# calculate the number of missing values for each column
colSums(is.na(df_test_subset))
#   YearBuilt BedroomAbvGr TotRmsAbvGrd   GarageArea Neighborhood   HouseStyle  KitchenQual  OverallCond 
#      0            0            0            1            0            0            1            0 

# impute the missing value from 'GarageArea' with mean value of this column
df_test_subset$GarageArea <- ifelse(is.na(df_test_subset$GarageArea), 
                                    ave(df_test_subset$GarageArea, FUN=function(x) mean(x, na.rm = TRUE)),
                                    df_test_subset$GarageArea)
# check if the missing value in this column is imputed
sum(is.na(df_test_subset$GarageArea))
# 0

# impute the missing value from 'KitchenQual' with the most frequent element
# get the counts for each category
table(df_test_subset$KitchenQual)
# Ex  Fa  Gd  TA 
# 105  31 565 757 

# impute the missing value in 'KitchenQual' with 'TA' which happened the mosts
df_test_subset$KitchenQual[is.na(df_test_subset$KitchenQual)] <- 'TA'
# check if the missing value from 'KitchenQual' is imputed
sum(is.na(df_test_subset$KitchenQual))
# 0

# categorical data
df_test_subset$Neighborhood <- factor(df_test_subset$Neighborhood)
df_test_subset$HouseStyle <- factor(df_test_subset$HouseStyle)
df_test_subset$KitchenQual <- factor(df_test_subset$KitchenQual)

# linear model regression using all the subset of the df_train
regressor_2 <- lm(formula = SalePrice ~., data=df_train_subset)

# predict the SalePrice for the subset of df_test
SalePrice <- predict(regressor_2, newdata = df_test_subset)

# create a dataframe of two columns with names 'Id' and 'SalePrice'
ka_df <- data.frame(df_test$Id, SalePrice)
colnames(ka_df) <- c('Id', 'SalePrice')  # change the column names
#head(ka_df)

# save as a csv file
write.csv(ka_df, 'prediction.csv', row.names=FALSE)