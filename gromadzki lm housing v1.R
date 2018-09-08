# Alex Gromadzki
# Housing Homework Assignment

library(tidyverse)
library(readr)  # Provides "read_csv" function
library(dplyr)  # Allows for nicer display of data frame

setwd("~/UVA/DSI/Fall 2018/SYS/Kaggle Competion 1/individual work gromadzki/")
address_all_datasets <- "~/UVA/DSI/Fall 2018/SYS/Kaggle Competion 1/individual work gromadzki/housing all"


# ====Reading in all Datasets====
setwd(address_all_datasets)


testing_data <- read_csv("test.csv")

original_data <- read_csv("train.csv")


sub <- sample(1:1460,size=730)

temp.orig <- mutate(original_data,
                    Alley = if_else(Alley=="NA", true = "NONE", false = Alley, missing = "NONE"))
temp.orig <- mutate(temp.orig,
                    LotFro2 = if_else(LotFrontage=="NA", true = 0, false = 1, missing = 0))

temp.orig$MSZoning <- factor(temp.orig$MSZoning)
temp.orig$Neighborhood <- factor(temp.orig$Neighborhood)
# temp.orig$YearBuilt <- factor(temp.orig$YearBuilt)
temp.orig$GarageCars <- factor(temp.orig$GarageCars)
temp.orig$SaleCondition <- factor(temp.orig$SaleCondition)


# ====Training Data====




training_data <- temp.orig[sub,]
training_with_lotfro <- training_data[(training_data$LotFro2 == 1),]

m.t.LotFro <- mean(training_with_lotfro$LotFrontage)
training_data <- mutate(training_data,
                    LotFrontage = if_else(LotFrontage=="NA", true = m.t.LotFro, false = as.double(LotFrontage), missing = m.t.LotFro))

# ====Validation Data====


validation_data <- temp.orig[-sub,]

valid_with_lotfro <- validation_data[(validation_data$LotFro2 == 1),]

m.v.LotFro <- mean(valid_with_lotfro$LotFrontage)
validation_data <- mutate(validation_data,
                        LotFrontage = if_else(LotFrontage=="NA", true = m.v.LotFro, false = as.double(LotFrontage), missing = m.v.LotFro))


# ===Attempt====

# MSZoning, LotArea, Neighborhood, YearBuilt, GarageCars, SaleCondition, OverallCond

training_glm1 <- glm(SalePrice ~ MSZoning+ LotArea+ Neighborhood+ YearBuilt+ 
                       GarageCars+ SaleCondition + OverallQual +OverallCond, data=training_data)
summary(training_glm1)

# Year doesn't look like it made a big difference (as factor), neither did sales condition
# when year is removed from factor, it is very significant!

training_glm2 <- glm(SalePrice ~ MSZoning+ LotArea+ Neighborhood+ YearBuilt+ 
                       GarageCars+ OverallQual +OverallCond, data=training_data)
summary(training_glm2)

# dropping sale condition makes barely any difference!



