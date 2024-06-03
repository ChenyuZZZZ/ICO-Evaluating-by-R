rm(list=ls())
nadata <- read.csv("D:/浏览器下载/LUBS5990M_courseworkData_2324.csv", fileEncoding="latin1")

str(nadata)

kable(head(nadata, 5))
kable(tail(nadata, 5))

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


data <- subset(data, Cost > 0)
data <- data %>% filter(Cost > 0)

#delete column
data_clean <- data [, -c(1,3,7,8,9,13)]



##之后找对位置
datas <- data_clean
datas$success <- factor(datas$success, levels = c("N", "Y"), 
                           labels = c("no", "yes"))

smp_size <- floor(0.7* nrow(datas))
set.seed(12345)
train_ind <- sample(nrow(datas), smp_size)
svm_train <- datas[train_ind, ]
svm_test <- datas[-train_ind, ]



c(1:3)

for (i in c(1:3)) {
  print(i)
}
library(kernlab)
kernetlist <- c('rbfdot','polydot','tanhdot','vanilladot')


for (i in kernetlist) {
  SVM_classifier <- ksvm(success ~ ., data = svm_train,
                         kernel = i)
  SVM_classifier
}


## Step 3: Evaluating model performance ----
library(tidyverse)

predictions <- predict(SVM_classifier, select(svm_test, -success))

results_s <- data.frame(Actual = factor(svm_test$success, levels = c("no", "yes")), 
                          Predicted = factor(predictions, levels = c("no", "yes")))
library(caret)
confusionMatrix(results_s$Predicted, results_s$Actual, positive = "yes")
library(pROC)
pred_binary_DT <- ifelse(predictions == 'yes', 1, 0)
true_binary_DT <- ifelse(svm_test$success == 'yes', 1, 0)
roc.curve_DT <- roc(pred_binary_DT, true_binary_DT)
plot(roc.curve_DT, col = "blue", main = "SVM ROC Curve", auc.polygon = TRUE,grid=TRUE)
