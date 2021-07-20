#Install and load required packages
if(!require(readr)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(corrplot)) install.packages("corrplot", repos = "http://cran.us.r-project.org")
if(!require(gridExtra)) install.packages("gridExtra", repos = "http://cran.us.r-project.org")
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(rpart)) install.packages("rpart", repos = "http://cran.us.r-project.org")
if(!require(rpart.plot)) install.packages("rpart.plot", repos = "http://cran.us.r-project.org")
if(!require(randomForest)) install.packages("randomForest", repos = "http://cran.us.r-project.org")

library (readr)
library(caret)
library(dplyr)
library(corrplot)
library(gridExtra)
library(tidyverse)
library(rpart)
library(rpart.plot)
library(randomForest)


#Download Heart Disease CSV File from Github
urlfile<-"https://raw.githubusercontent.com/danman13/Capstone/main/heart.csv"
heart<-read_csv(url(urlfile))
class(heart)

# Data is a Tibble...convert to a data frame
heart <- as.data.frame(heart)
class(heart)


#Data Exploration
dim(heart) #Look at observations - 303 rows and 14 columns
head(heart) #looking at first few rows to see what predictors we can use
str(heart) #all columns are numeric...no factors or strings
table(heart$output) # output column (what we are trying to predict) is binary 0/1
prop.table(table(heart$output)) #roughtly 55% of people in dataset have a heart attack

#rename classification variables as factors
#Reclass Sex to Male/Female
heart$sex <- factor(heart$sex)
levels(heart$sex) <- c("Female", "Male")

heart$cp <- factor(heart$cp)
heart$fbs <- factor(heart$fbs)
heart$restecg <- factor(heart$restecg)
heart$exng <- factor(heart$exng)
heart$slp <- factor(heart$slp)
heart$caa <- factor(heart$caa)
heart$thall <- factor(heart$thall)
heart$output <- factor(heart$output)

str(heart)
anyNA(heart) #No missing values

heart %>%
  ggplot(aes(x = factor(sex))) +
  geom_bar(fill = "red") + 
  xlab("Sex")

heart %>%
  ggplot(aes(x = age)) +
  geom_histogram(binwidth = 5, fill = "red") + 
  xlab("Age")

#Median age is around 55
heart %>%
  ggplot(aes(x = age)) +
  geom_boxplot() + 
  xlab("Age")

#Type 0 (Typical angina) is the most common followed by 2 (non-anginal)
heart %>%
  ggplot(aes(x = factor(cp))) +
  geom_bar(fill = "red") + 
  xlab("Chest Pain")

#Look at correlation for numeric data
numeric_vector <- c(1,4,5,8,10)
heart_numeric <- heart[,numeric_vector]
head(heart_numeric)
corelation <- cor(heart_numeric)
head(corelation)
corrplot(corelation, method="shade") #not many highly correlated variables

#Since we can't look at correlation data for non-numeric data, we can recreate the dataset in numeric only
heart2<-read_csv(url(urlfile))
heart2 <- as.data.frame(heart2)
cor_heart2 <- cor(heart2)
corrplot(cor_heart2,method = "shade") #observe that outcome has stronger correlation with certain variables like chest pain, restecg, or old peak

summary(heart)

#Look at how outcome impacts distribution of sex
sex1 <- heart %>% filter(output == 1) %>%
  ggplot(aes(x = factor(sex))) +
  geom_bar(fill = "Red")+
  xlab("Sex")+
  ggtitle("Sex when output is 1")

sex2 <- heart %>% filter(output == 0) %>%
  ggplot(aes(x = factor(sex))) +
  geom_bar(fill = "Red")+
  xlab("Sex")+
  ggtitle("Sex when output is 0")
grid.arrange(sex1, sex2) #when no heart attack present, it's more common for men to be present...fairly equal when Heart disease occurs

#side by side, age when outcome = 1, 0
age1 <- heart %>% filter(output == 1) %>%
  ggplot(aes(x = age)) +
  geom_histogram(binwidth = 5,fill = "Red")+
  xlab("Age")+
  ggtitle("Age when output is 1")

age2 <- heart %>% filter(output == 0) %>%
  ggplot(aes(x = age)) +
  geom_histogram(binwidth = 5,fill = "Red")+
  xlab("Age")+
  ggtitle("Age when output is 0")
grid.arrange(age1, age2) #skews slightly younger when heart attack is present

#side by side, trtbps when outcome = 1,0
bps1 <- heart %>% filter(output == 1) %>%
  ggplot(aes(x = trtbps)) +
  geom_histogram(binwidth = 5,fill = "Red")+
  xlab("BPS")+
  ggtitle("BPS when output is 1")

bps2 <- heart %>% filter(output == 0) %>%
  ggplot(aes(x = trtbps)) +
  geom_histogram(binwidth = 5,fill = "Red")+
  xlab("BPS")+
  ggtitle("BPS when output is 0")
grid.arrange(bps1, bps2) #higher avg BPS when outcome is 1

#side by side, chol when outcome = 1,0
chol1 <- heart %>% filter(output == 1) %>%
  ggplot(aes(x = chol)) +
  geom_histogram(binwidth = 5,fill = "Red")+
  xlab("Cholesterol")+
  ggtitle("Cholesterol when output is 1")

chol2 <- heart %>% filter(output == 0) %>%
  ggplot(aes(x = chol)) +
  geom_histogram(binwidth = 5,fill = "Red")+
  xlab("Cholesterol")+
  ggtitle("Cholesterol when output is 0")
grid.arrange(chol1, chol2) #With heart attack present, cholesterol is lower

#side by side, thalachh (max heart rate) when outcome = 1,0
maxhr1 <- heart %>% filter(output == 1) %>%
  ggplot(aes(x = thalachh)) +
  geom_histogram(binwidth = 5,fill = "Red")+
  xlab("Max HR")+
  ggtitle("Max HR when output is 1")

maxhr2 <- heart %>% filter(output == 0) %>%
  ggplot(aes(x = thalachh)) +
  geom_histogram(binwidth = 5,fill = "Red")+
  xlab("Max HR")+
  ggtitle("Max HR when output is 0")
grid.arrange(maxhr1, maxhr2) #higher max heart rate for those with heart disease

#side by side, chest pain type when outcome = 1,0
cp1 <- heart %>% filter(output == 1) %>%
  ggplot(aes(x = factor(cp))) +
  geom_bar(fill = "Red")+
  xlab("Chest Pain")+
  ggtitle("Chest Pain when output is 1")

cp2 <- heart %>% filter(output == 0) %>%
  ggplot(aes(x = factor(cp))) +
  geom_bar(fill = "Red")+
  xlab("Chest Pain")+
  ggtitle("Chest Pain when output is 0")
grid.arrange(cp1, cp2) #asymptomatic is more common when HD is present


#Split to training and test. Put 75% of the data into the training dataset and the remaining 25% into the test set
n.point <- nrow(heart)
sampling_rate <- 0.75
set.seed(1, sample.kind = "Rounding")
train <- sample(1:n.point, sampling_rate * n.point, replace = FALSE)
test <- setdiff(1:n.point, train)
heart_train <- subset(heart[train,])
heart_test <- subset(heart[test,])

#Convert training and test sets into data frames
heart_train <- as.data.frame(heart_train)
heart_test <- as.data.frame(heart_test)

#Peek at the training and test data and look at the number of rows
head(heart_train)
head(heart_test)
nrow(heart_train)
nrow(heart_test)

#Confirm no data class got changed
str(heart_train)
str(heart_test)

#fit a logistic regression model using sex, age, and cholesterol
set.seed(1, sample.kind = "Rounding")
fit_glm <- glm(output ~ sex + age + chol, data = heart_train, family = "binomial")
p_hat_logistic <- predict(fit_glm, heart_test)
y_hat_logistic <- factor(ifelse(p_hat_logistic > 0.5, 1, 0))
fit_glm_result <- confusionMatrix(data = y_hat_logistic, reference = heart_test$output)$overall[1]
fit_glm_result

results <- data.frame()
results <- data_frame(method="Logistic Regression - variables", accuracy = fit_glm_result)
results %>% knitr::kable()

#fit a 2nd logistic regression model using sex, age, cholesterol, and chest pain
set.seed(1, sample.kind = "Rounding")
fit_glm2 <- glm(output ~ sex + age + chol + cp, data = heart_train, family = "binomial")
p_hat_logistic2 <- predict(fit_glm2, heart_test)
y_hat_logistic2 <- factor(ifelse(p_hat_logistic2 > 0.5, 1, 0))
fit_glm_result2 <- confusionMatrix(data = y_hat_logistic2, reference = heart_test$output)$overall[1]
fit_glm_result2
results <- bind_rows(results, data_frame(method="Logistic Regression - 4 variables", accuracy = fit_glm_result2))
results %>% knitr::kable()

#fit a 3rd logistic regression model using sex, age, cholesterol, chest pain, and max heart rate
set.seed(1, sample.kind = "Rounding")
fit_glm3 <- glm(output ~ sex + age + chol + cp + thalachh, data = heart_train, family = "binomial")
p_hat_logistic3<- predict(fit_glm3, heart_test)
y_hat_logistic3 <- factor(ifelse(p_hat_logistic3 > 0.5, 1, 0))
fit_glm_result3 <- confusionMatrix(data = y_hat_logistic3, reference = heart_test$output)$overall[1]
fit_glm_result3
results <- bind_rows(results, data_frame(method="Logistic Regression - 5 variables", accuracy = fit_glm_result3))
results %>% knitr::kable()


confusionMatrix(data = y_hat_logistic2, heart_test$output) #high sensitivity but low specificity...probably not best model

###Use KNN
set.seed(1, sample.kind = "Rounding")
knn_fit <- knn3(output ~ ., data = heart_train, k = 5)
y_hat_knn <- predict(knn_fit, heart_test, type = "class")
knn_results <- confusionMatrix(data = y_hat_knn, reference = heart_test$output)$overall[1]
results <- bind_rows(results, data_frame(method="Knn - k = 3", accuracy = knn_results))
confusionMatrix(data = y_hat_knn, heart_test$output) 
results %>% knitr::kable()

#Use random forest
set.seed(1, sample.kind = "Rounding")
rf_fit <- randomForest(output ~ .,  data=heart_train)
y_hat_rf <- predict(rf_fit, heart_test)
rf_results <- confusionMatrix(data = y_hat_rf, reference = heart_test$output)$overall[1]
rf_results
confusionMatrix(data = y_hat_rf, heart_test$output) #.8421 accuracy. .6857 sensitivity vs .9756 specificity however in this case it's ok
#we would rather see a false positive here
results <- bind_rows(results, data_frame(method="Random Forest", accuracy = rf_results))
results %>% knitr::kable()
