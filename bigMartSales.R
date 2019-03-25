#directory path
path = "C:/Users/uidj1620/Desktop/Desktop/Folder/PCA"

#set working directory
setwd(path)

#load train the data
data <- read.csv("bigMartSales.csv", stringsAsFactors = F)

#impute missing values with median
data$Item_Weight[is.na(data$Item_Weight)] <- median(data$Item_Weight, na.rm = TRUE)

#impute 0 with median
data$Item_Visibility <- ifelse(data$Item_Visibility == 0, median(data$Item_Visibility),
                               data$Item_Visibility)

#find mode and impute
table(data$Outlet_Size, data$Outlet_Type)
data$Outlet_Size[data$Outlet_Size == ""] = "Other"

#remove the dependent and identifier variables
data1 <- subset(data, select = -c(Item_Outlet_Sales, Item_Identifier, Outlet_Identifier))

#check available variables
colnames(data1)

#check variable class
str(data1)

#load library
library(dummies)

#create a dummy data frame
new_data <- dummy.data.frame(data1, names = c("Item_Fat_Content","Item_Type",
                                              "Outlet_Establishment_Year","Outlet_Size",
                                              "Outlet_Location_Type","Outlet_Type"))
#check the data set
str(new_data)

trainSet = sort(sample(1:nrow(new_data), replace = F, size = nrow(new_data)/2))
testSet = -trainSet

#divide the new data
trainData <- new_data[trainSet,]
testData <- new_data[testSet,]

#principal component analysis
prin_comp <- prcomp(trainData, scale. = T)
names(prin_comp)

#outputs the mean of variables
prin_comp$center

#outputs the standard deviation of variables
prin_comp$scale

prin_comp$rotation
prin_comp$rotation[1:5,1:4]

biplot(prin_comp, scale = 0)

 #compute standard deviation of each principal component
std_dev <- prin_comp$sdev

#compute variance
pr_var <- std_dev^2

#check variance of first 10 components
pr_var[1:10]

#proportion of variance explained
prop_varex <- pr_var/sum(pr_var)
prop_varex[1:20]

#scree plot
plot(prop_varex, xlab = "Principal Component", ylab = "Proportion of Variance Explained", type = "b")

#cumulative scree plot
plot(cumsum(prop_varex), xlab = "Principal Component", 
     ylab = "Cumulative Proportion of Variance Explained",
     type = "b")

#add a training set with principal components
train.data <- data.frame(Item_Outlet_Sales = data$Item_Outlet_Sales[trainSet], prin_comp$x)

#we are interested in first 30 PCAs
train.data <- train.data[,1:31]

## For the prediction, one can use regression or any other suitable machine learning algorithm
## Just to show how to do it, we use decision trees here
## Do not worry about decision trees if do not feel comfortable. You can go ahead with regression

#run a decision tree
install.packages("rpart")
library(rpart)
rpart.model <- rpart(Item_Outlet_Sales ~ .,data = train.data[,1:30], method = "anova")
rpart.model

#transform test into PCA
test.data <- predict(prin_comp, newdata = testData)
test.data <- as.data.frame(test.data)

#select the first 30 components
test.data <- test.data[,1:10]

#make prediction on test data
rpart.pca.prediction <- predict(rpart.model, test.data)
length(rpart.prediction)
sqrt(mean((rpart.pca.prediction - data$Item_Outlet_Sales[testSet])^2))

rpart.model <- rpart(data$Item_Outlet_Sales[trainSet] ~ .,data = data1[trainSet,], method = "anova")

#make prediction on test data
rpart.prediction <- predict(rpart.model, data1[testSet,])
length(rpart.prediction)
sqrt(mean((rpart.prediction - data$Item_Outlet_Sales[testSet])^2))

