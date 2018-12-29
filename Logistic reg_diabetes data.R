setwd("C:\\Users\\uidj1620\\Desktop\\Desktop\\Folder\\Logistic Regression")
pima <- read.csv(file = "diabetes.csv", head = TRUE, sep = ",")
library(caTools)
library(corrplot)
library(caret)
head(pima) # # visualize the header of Pima data
str(pima) # show the structure of the data

pima$Outcome <- as.factor(pima$Outcome)
## We use “sapply”" to check the number of missing values in each columns.
sapply(pima, function(x) sum(is.na(x)))

#Let’s produce the matrix of scatterplots
pairs(pima, panel = panel.smooth)

#we compute the matrix of correlations between the variables
corrplot(cor(pima[, -9]), type = "lower", method = "number")

# Apply Logistic Regression model
# Preparing the DataSet
set.seed(1234)
n <- nrow(pima)
train <- sample(n, trunc(0.70*n))
pima_training <- pima[train, ]
pima_testing <- pima[-train, ]

# Training The Model
glm_fm1 <- glm(Outcome ~., data = pima_training, family = binomial)
summary(glm_fm1)

##The result shows that the variables BloodPressure, SkinThickness, Insulin and
##Age are not statiscally significance. In other words, the p_values is greather than 0.01. 
##Therefore they will be removed.

glm_fm2 <- update(glm_fm1, ~. - BloodPressure - SkinThickness - Insulin - Age )
summary(glm_fm2)
##Update to use only the significant variable

## Plot the new model
par(mfrow = c(2,2))
plot(glm_fm2)

# Testing the Model
glm_probs <- predict(glm_fm2, newdata = pima_testing, type = "response")
glm_pred <- ifelse(glm_probs > 0.5, 1, 0)
#print("Confusion Matrix for logistic regression"); table(Predicted = glm_pred, Actual = pima_testing$Diabetes)
#confusionMatrix(glm_pred, pima_testing$Outcome ) # Confusion Matrix for logistic regression

# confusion matrix
table(Predicted = glm_pred, Actual = pima_testing$Outcome)

with(glm_fm2, pchisq(null.deviance - deviance, df.null-df.residual, lower.tail = F))



