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
clean_data<- data [, -c(1,3,7,8,9,13)]


normalize <- function(x) { 
  return((x - min(x)) / (max(x) - min(x)))
}

data_norm <- as.data.frame(lapply(clean_data, function(x) if(is.numeric(x)) normalize(x) else x))

summary(data_norm$success)

smp_size <- floor(0.7 * nrow(data_norm))


# Set seed for reproducibility
set.seed(123)

# Sample indices for training
train_indices <- sample(seq_len(nrow(data_norm)), size = smp_size)

# Split the data
train_data <- data_norm[train_indices, ]
test_data <- data_norm[-train_indices, ]

train_labels <- data_norm[train_indices, "success"]
test_labels <- data_norm[-train_indices,"success"]

library(neuralnet)

nn_model <- neuralnet(success ~ ., data = train_data, hidden = c(1,1))  # You can adjust the number of hidden neurons
test_predictions <- predict(nn_model, test_data)
# Visualize the model
plot(nn_model)

# Correct usage of predictions
predicted_success <- predict(nn_model, test_data)  # No need to apply predict again on predictions

# Optionally, if you need to merge or manipulate predictions
predicted_success_df <- data.frame(actual = test_data$success, predicted = predicted_success)

# Check the structure or summary of predicted results
summary(predicted_success_df)


ANN_test_prob <- predict(nn_model, test_data, type = "raw")
ANN_test_prob
predicted_success_ANN <- predict(nn_model, test_data)



nn_results_1 <- data.frame(actual_type = test_labels,
                           predict_type = predicted_success_ANN)

nn_results_1$predicted_success <- factor(ifelse(nn_results_1$predict_type.2 > nn_results_1$predict_type.1, "Y", "N"))

write.csv(nn_results_1, "nn_results_1.csv", row.names = FALSE)
nn_results_1 <- read.csv("nn_results_1.csv", stringsAsFactors = TRUE)
head(nn_results_1)
head(subset(nn_results_1, actual_type != predicted_success))
nn_results_1$actual_type = factor(nn_results_1$actual_type)

library(caret)
confusionMatrix(nn_results_1$predicted_success, nn_results_1$actual_type, positive = "Y")
library(pROC)

#ANN
pred_binary_ANN<- ifelse(nn_results_1$predicted_success == 'Y', 1, 0)
true_binary_ANN<- ifelse(nn_results_1$actual_type == 'Y', 1, 0)

# Create ROC curve
roc.curve_ANN <- roc(pred_binary_ANN, true_binary_ANN)
plot(roc.curve_ANN, col = "blue", main = "ANN ROC Curve", auc.polygon = TRUE, grid = TRUE)


