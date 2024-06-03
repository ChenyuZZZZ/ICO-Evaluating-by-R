rm(list = ls())

library(tidyverse)
library(gplots)
library(knitr)
install.packages("ggthemes")
library(ggthemes)

install.packages("caret")
library(caret)

library(rpart)
library(rpart.plot)

library(dplyr)
install.packages("pROC")
library(pROC)

library(ROCR)
##1.EDA
nadata <- read.csv("D:/浏览器下载/LUBS5990M_courseworkData_2324.csv", fileEncoding="latin1")

str(nadata)


data <- na.omit(nadata)

# 使用 complete.cases() 同样可以达到删除含有 NA 的行的效果
data <- data[complete.cases(nadata), ]


##unique region

data$countryRegion <- factor(data$countryRegion)

data$countryRegion_encoded <- as.integer(data$countryRegion)

head(data[, c("countryRegion", "countryRegion_encoded")])

##unique platform
data$platform <- factor(data$platform)
data$platform_encoded <- as.integer(data$platform)

##create duration
head(data$startDate)
head(data$endDate)

data$startDate <- as.Date(data$startDate, format = "%d/%m/%Y")
data$endDate <- as.Date(data$endDate, format = "%d/%m/%Y")

data$campaign_duration <- as.numeric(difftime(data$endDate, data$startDate, units = "days"))

data <- subset(data, campaign_duration > 0)
data <- data %>% filter(campaign_duration > 0)

##create Cost
data$priceUSD <- as.numeric(data$priceUSD)
data$coinNum <- as.numeric(data$coinNum)

data$Cost <- data$priceUSD * data$coinNum

head(data)

data <- subset(data, Cost >= 0)
data <- data %>% filter(Cost >= 0)

#delete column
data <- data [, -c(1,3,7,8,9,13)]

data$success <- factor(data$success, levels = c("N", "Y"), 
                         labels = c("no", "yes"))
##model


# Determine the sample size for the training data (90% of the dataset)
smp_size <- floor(0.7 * nrow(data))


set.seed(12345)
train_ind <- sample(nrow(data), smp_size)
credit_train <- data[train_ind, ]
credit_test <- data[-train_ind, ]

# Randomly sample indices for the training data
train_index <- sample(seq_len(nrow(data)), size = sample)

# Create training and testing datasets
traindata <- data[train_index, ]
testdata <- data[-train_index, ]




library(C50)


model <- C5.0(select(traindata, -success), traindata$success)

summary(model)

plot(model, main = "Decision Tree Model")

#prediction
predictions <- predict(model, testdata)
# Load the gmodels package for CrossTable

library(gmodels)

# Compare the predictions to the actual outcomes
CrossTable(x = predictions, 
           y = testdata$success, 
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE, 
           dnn = c('predicted success', 'actual success'))

#improve performance
data_boost10 <- C5.0( select(traindata, -success), traindata$success, trials = 10, control = C5.0Control(noGlobalPruning = FALSE))

plot(data_boost10)

data_boost_pred10 <- predict(data_boost10, testdata)
CrossTable(data_boost_pred10, testdata$success, 
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('predicted success', 'actual success'))

##3.evaluation
#1.matrix
predictions_factor <- factor(predictions, levels = levels(testdata$success))
conf_matrix <- confusionMatrix(predictions_factor, testdata$success)
print(conf_matrix)

#2.AUC and ROC

pred_binary_DT <- ifelse(data_boost_pred10 == 'yes', 1, 0)
true_binary_DT <- ifelse(testdata$success == 'yes', 1, 0)
roc.curve_DT <- roc(pred_binary_DT, true_binary_DT)
plot(roc.curve_DT, col = "blue", main = "Decision Tree ROC Curve", auc.polygon = TRUE, grid = TRUE)

#3.precision and Recall
TP <- conf_matrix$table["yes", "yes"]
FP <- conf_matrix$table["no", "yes"]
FN <- conf_matrix$table["yes", "no"]

precision <- TP / (TP + FP)

recall <- TP / (TP + FN)

print(paste("Precision:", precision))
print(paste("Recall:", recall))

#4.f1
f1_score <- 2 * (precision * recall) / (precision + recall)

print(paste("F1 Score:", f1_score))
