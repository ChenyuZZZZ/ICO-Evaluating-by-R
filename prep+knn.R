library(tidyverse)
library(gplots)
library(knitr)
install.packages("ggthemes")
library(ggthemes)

install.packages("caret")
library(caret)

library(rpart)
library(rpart.plot)

install.packages("pROC")
library(pROC)
library(dplyr)
##1.EDA
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


data <- subset(data, Cost >= 0)
data <- data %>% filter(Cost >= 0)

#delete column
clean_data <- data [, -c(1,3,7,8,9,13)]
head(clean_data)
library(corrplot)
corrplot(cor(clean_data[,2:10]))

unique(clean_data$success)

# 如果 success 是字符型 "Y" 和 "N"
clean_data$success <- ifelse(clean_data$success == "Y", 1, 0)


logit_model <- glm(success ~ hasVideo + rating + priceUSD + teamSize + hasGithub +
                     hasReddit + coinNum + minInvestment + distributedPercentage +
                     countryRegion_encoded + platform_encoded, 
                   family = binomial(link = "logit"), data = clean_data)

# 查看模型摘要
summary(logit_model)







##2.Success propotion in region and its success propotion
summary_data <- data %>%
  group_by(countryRegion_encoded, success) %>%
  summarize(count = n(), .groups = 'drop')


ggplot(summary_data, aes(x = countryRegion_encoded, y = count, fill = success)) +
  geom_bar(stat = "identity") +
  labs(x = "Region", y = "Count", fill = "Success") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+ggtitle("Distribution of Success Per Region")


##3.success and platform

platform_counts <- data %>%
  count(platform) %>%
  arrange(desc(n))

# Extract the top 3 platforms
top_three_platforms <- platform_counts$platform[1:3]

# Create a new column grouping the platforms
data <- data %>%
  mutate(top_three = ifelse(platform %in% top_three_platforms, as.character(platform), "Other"))

# Summarize the count and calculate the percentage for each group
top_platform_counts <- data %>%
  count(top_three) %>%
  mutate(pct = n / sum(n) * 100)

# Create a pie chart
ggplot(top_platform_counts, aes(x = "", y = n, fill = top_three)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y") +
  labs(title = "Distribution Of Platforms", x = "", y = "") +
  theme_minimal() +
  theme(axis.text.x = element_blank(), axis.ticks = element_blank()) +
  geom_text(aes(label = paste0(round(pct, 1), "%")), position = position_stack(vjust = 0.5)) +
  scale_fill_manual(values = c("Ethereum" = "royalblue",
                               "Waves" = "lightgreen",
                               "Stellar" = "red",
                               "Other" = "grey"))

ethereum_data <- data %>%
  filter(platform == "Ethereum")

# Count the occurrences of each `success` category
success_counts <- ethereum_data %>%
  count(success) %>%
  mutate(pct = n / sum(n) * 100)

# Create a pie chart for the success distribution in Ethereum platform

ggplot(success_counts, aes(x = "", y = n, fill = success)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y") +
  labs(title = "Success Distribution in Ethereum Platform", x = "", y = "") +
  theme_minimal() +
  theme(axis.text.x = element_blank(), axis.ticks = element_blank()) +
  geom_text(aes(label = paste0(round(pct, 1), "%")), position = position_stack(vjust = 0.5)) +
  scale_fill_manual(values = c("Y" = "green", "N" = "red"))
#combine with general?

##4.the relation between duration and success
summary(data$campaign_duration)

campaign_duration_pct <- data %>%
  group_by(campaign_duration) %>%
  summarize(success_count = sum(success == "Y"),
            total_count = n()) %>%
  mutate(pct = success_count / total_count * 100)

# Check for missing values in campaign_duration and pct
sum(is.na(campaign_duration_pct$campaign_duration))
sum(is.na(campaign_duration_pct$pct))
# Check rows with missing values
campaign_duration_pct[!complete.cases(campaign_duration_pct), ]
campaign_duration_pct_clean <- campaign_duration_pct[complete.cases(campaign_duration_pct), ]


ggplot(campaign_duration_pct, aes(campaign_duration,pct)) + 
  geom_point(colour="royalblue", size=2.5) + ggtitle("The Relationship Between Duration And Success Rate") + 
  xlab("Duration (Days)") + ylab("Success Rate (%)") + 
  scale_x_continuous(breaks = c(0, 30, 50, 100, 150, 200, 250, 300, 350, 400, 450, 500, 550, 600, 650, 700, 750), limits = c(0, 776)) + geom_vline(xintercept=30, colour="red") + 
  theme_economist() + 
  theme(plot.title=element_text(hjust=0.5), axis.title=element_text(size=12, face="bold"))
##check the breaks

ggplot(campaign_duration_pct, aes(x = campaign_duration, y = success_count)) +
  geom_point(colour = "royalblue", size = 2.5) +
  ggtitle("The Relationship Between Duration and Success Count") +
  xlab("Duration (Days)") + ylab("Success Count") +
  scale_x_continuous(breaks = c(0, 30, 50, 100, 150, 200, 250, 300, 350, 400, 450, 500, 550, 600, 650, 700, 750), limits = c(0, 776)) +
  geom_vline(xintercept = 30, colour = "red") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5), axis.title = element_text(size = 12, face = "bold"))

ggplot(data[data$campaign_duration <= 3723,], aes(campaign_duration)) + geom_density(colour="royalblue4", linewidth=1) + 
  ggtitle("Distribution of Projects by Campaign Length") + xlab("Project Length (Days)") + 
  ylab("Density (%)") + scale_x_continuous(breaks=c(0, 30, 50, 100, 150, 200, 250, 300, 350, 400, 450, 500, 550, 600, 650, 700, 750), limits = c(0, 776)) + 
  geom_vline(xintercept=30, colour="red") + theme_economist() + 
  theme(plot.title=element_text(hjust=0.5), axis.title=element_text(size=12, face="bold"))

##5. coinNum per country

ggplot(data, aes(x = countryRegion_encoded, y = coinNum)) +
  stat_summary(fun = "mean", geom = "bar") +
  labs(x = "Region", y = "Average Value") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

##rela between cost and success
ggplot(data, aes(x = success, y = Cost)) +
  geom_boxplot() +
  labs(title = "Cost Distribution by Success", x = "Success", y = "Cost") +
  scale_y_log10()  # Use log scale to handle large variations in cost

##rela between rating and success
ggplot(data[data$rating<=4.8,], aes(rating)) + geom_density(colour="royalblue4", linewidth=1) + 
  ggtitle("Distribution of Projects by Rating") + xlab("Rating") + 
  ylab("Density (%)") + scale_x_continuous(breaks=c(0,1,2,3,4,5), limits = c(0, 5)) + 
  geom_vline(xintercept=30, colour="red") + theme_economist() + 
  theme(plot.title=element_text(hjust=0.5), axis.title=element_text(size=12, face="bold"))

## Part2 Model
#2.1 KNN

# 将目标列转换为因子

unique(data$success)
data$success <- tolower(data$success)

# 将文本统一为 "Y" 或 "N"
data$success <- ifelse(data$success %in% c("y", "yes"), "Y", "N")

# 转换为因子，设定正确的标签
data$success <- factor(data$success, levels = c("N", "Y"), labels = c("No", "Yes"))
# 转换和规范化数据
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}


data$rating <- as.numeric(as.character(data$rating))
data$countryRegion_encoded <- as.integer(data$countryRegion)
data$platform_encoded <- as.integer(data$platform)
data$priceUSD <- as.numeric(as.character(data$priceUSD))
data$teamSize <- as.numeric(as.character(data$teamSize))
data$coinNum <- as.numeric(as.character(data$coinNum))


# 规范化数值列
numeric_columns <- c("countryRegion_encoded", "priceUSD","teamSize","coinNum", "rating", "platform_encoded")
# 计算数据框中所有数值特征的方差
variances <- sapply(data, function(x) if(is.numeric(x)) var(x, na.rm = TRUE))
print(variances)



data[numeric_columns] <- lapply(data[numeric_columns], normalize)

# 保持目标列
data_normalized <- data
success <- data$success

# 设置种子并分割数据
set.seed(42)
trainIndex <- sample(1:nrow(data_normalized), size = round(0.7 * nrow(data_normalized)))

trainData <- data_normalized[trainIndex, ]
testData <- data_normalized[-trainIndex, ]

# 提取目标标签
train_labels <- success[trainIndex]
test_labels <- success[-trainIndex]

library(class)

# 设置 K 值
k <- 35

# 训练 KNN 模型
test_predictions <- knn(train = trainData[, -which(names(trainData) == "success")],
                        test = testData[, -which(names(testData) == "success")],
                        cl = train_labels, k = k)

#3. Evaluating model performance
#install.packages("gmodels")
library(gmodels)#various tools for model fitting

#Evaluate how well the predicted classes in the wbcd_test_pred 
#vector match up with the known values in the wbcd_test_labels vector
#Specifying prop.chisq=FALSE will remove the chi-square values that are not needed
CrossTable(x = test_labels, y = test_predictions, prop.chisq=FALSE)


