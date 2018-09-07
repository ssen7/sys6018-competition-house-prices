# read training set and test set
setwd("~/Documents/GitHub/sys6018-competition-house-prices")
df_train <- read.csv("train.csv", header=TRUE)
df_test <- read.csv("test.csv", header=TRUE)

#summary(df_train)
#ncol(train)
# 81
#colnames(train)[ncol(train)]
#correlations <- lapply(train[,-ncol(train)], function(x) cor(train[, ncol(train)], numeric(1)))
#cor(train)
length(df_test[, 'Id'])
# 1459
colSums(is.na(df_train))
missing_values_columns <- 
df_train <- subset(df_train, select = -c('LotFrontage', 'Alley', ))