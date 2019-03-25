library(FactoMineR)

# You are a data science consultant for an investment firm
# You are working with property prices data set to estimate property price through regression modeling
# You are helping an investment firm make money through price arbitrage
# Develop a model to estimate property price

#Dist_Taxi - distance to nearest taxi stand from the property
#Dist_Market - distance to nearest grocery market from the property
#Dist_Hospital - distance to nearest hospital from the property
#Carpet - carpet area of the property in square feet
#Builtup - built-up area of the property in square feet
#Rainfall - annual rainfall in the area where property is located

##############################################################################

### Load the data for analysis ############

#data<-read.csv("propertyPrices.csv", stringsAsFactors = F)
data<-read.csv('C:/Users/uidj1620/Desktop/Desktop/Folder/PCA/propertyPricesRaw.csv')
head(data)
colnames(data)
numeric_predictors<-c("Dist_Taxi", "Dist_Market", "Dist_Hospital", "Carpet","Builtup", "Rainfall")
##############################################################################
### Data analysis
# Tagging predictor and response variables
numeric<-c("Dist_Taxi", "Dist_Market", "Dist_Hospital", "Carpet","Builtup", "Rainfall")
categoric <- c("Parking", "City_Category")
Target<-c("House_Price")

data = na.omit(data)

# Lets start analyzing data set  
# Suppose you started with scatter plots of carpet area and house price with and without the outliers. 
# You are particularly interested in the correlation coefficients

## Plot House Price Vs Carpet Area with Outliers and wiout Outliers
plot(data$House_Price, data$Carpet)
cor(data$House_Price, data$Carpet)

# You noticed that the extreme outlier (a mansion among the middle class houses) has a massive 
# leverage over the other observations. This has a huge impact on the correlation coefficient as well. 
# Essentially, in the presence of this outlier you are drawing the regression line between 
# two dots on the scattered plot : the extreme outlier and the whole bunch of data clubbed 
# together. 
# This is the reason the correction coefficient in this case is very close to perfect correlation. 
# When this outlier is removed the bunched up data looks like a more realistic scattered plot.
data_without_outliers = data[data$House_Price<10^8,]
plot(data_without_outliers$House_Price, data_without_outliers$Carpet)
cor(data_without_outliers$House_Price, data_without_outliers$Carpet)

# Now, you want to analyse all the numeric predictor variables against the response variable (house price) all at once. You will create scatter plots and correlation coefficeints in the matrix format. Keep an eye for correlation coefficient and scatter plot for carpet area and house price you had already analysed in the above animation.

# Functions to plot matrix correlation plot and display correlation coef. on bottom and top panels
panel.cor <- function(x, y, digits=3, prefix="", cex.cor) 
{
  usr <- par("usr"); on.exit(par(usr)) 
  par(usr = c(0, 1, 0, 1)) 
  r <- abs(cor(x, y)) 
  txt <- format(c(r, 0.123456789), digits=digits)[1] 
  txt <- paste(prefix, txt, sep="") 
  if(missing(cex.cor)) cex <- 0.8/strwidth(txt) 
  
  test <- cor.test(x,y) 
  Signif <- symnum(test$p.value, corr = FALSE, na = FALSE, 
                   cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
                   symbols = c("***", "**", "*", ".", " ")) 
  
  text(0.5, 0.5, txt, cex = 4*r^(0.3),col = "dodgerblue") 
  text(.8, .9, Signif, cex= 4, col=2) 
}

# Function for histogram in the diagonal panel
panel.hist <- function(x, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(usr[1:2], 0, 1.5))
  h <- hist(x, plot = FALSE)
  breaks <- h$breaks; nB <- length(breaks)
  y <- h$counts; y <- y/max(y)
  rect(breaks[-nB], 0, breaks[-1], y, col = "orange", ...)
}


# Matrix plot with outliers
pairs(data[,c(numeric,Target)],upper.panel = panel.cor,diag.panel=panel.hist,cex.labels = 2)

# Matrix plot without outliers
data_without_outliers<-data[data$House_Price<10^8,]
pairs(data_without_outliers[,c(numeric,Target)],upper.panel = panel.cor,diag.panel=panel.hist,cex.labels = 2)


#############################
# Before we jump to PCA, think of 6 input variables collectively as the human body and 
# the components generated from PCA as elements (oxygen, hydrogen, carbon etc.). 
# When you did the principal component analysis of these 6 variables 
# observe that just 3 components can explain ~90% of these variables i.e. (37.7 + 33.4 + 16.6 = 87.7%). 
# This means that you could reduce these 6 variables to 3 principal components by losing just 
# 10% of the information. That is not a bad bargain 50% reduction in variables at the cost of 
# 10% information. Moreover, these components will never have any multicollinearity between 
# them as they are orthogonal or perfectly uncorrelated.



##############################################################################
Data_for_PCA<-data[,numeric_predictors]
colnames(Data_for_PCA)
### Initiate principal component analysis ############

pr.out = prcomp(Data_for_PCA)

## Use PCA from FactoMineR package as following
pca<-PCA(Data_for_PCA)

#########################################
# Here, distance to taxi, market, and hospital have formed a composite variable (comp 1) 
# which explains 37.7% information in data. 
# Another, orthogonal axis (comp 2) explains the remaining 33.4% of variation through the 
# composite of carpet and built-up area. Rainfall is not a part of comp 1 or comp 2 but is a 
# 3rd orthogonal component. You can get this information in a much more friendly tabular form 
# to display composition of all the variables. Use this command in R.


pca$eig

### Identify the importance of each component ############

pca$eig

#     eigenvalue           percentage of variance      cumulative percentage of variance
#comp 1 2.261774548            37.69624247                          37.69624
#comp 2 2.003601906            33.39336510                          71.08961
#comp 3 0.995796091            16.59660151                          87.68621
#comp 4 0.563539807             9.39233012                          97.07854
#comp 5 0.174173734             2.90289557                          99.98143
#comp 6 0.001113914             0.01856523                         100.00000


###########################################
# We have got the percentage of data (variance) explained by each component. 
# Observe the second column named 'Eigenvalue'? 
# The eigenvalue is used in the principal component analysis to calculate the % variance explained. 
# For instance, if ywe divide eigenvalue of comp 1 with the sum of all the eigenvalues 
# {i.e. 2.262/(2.262+2.004.+0.001)} you will get 37.7%. 
# Thus, eigenvalue does have some cool properties.

# what about eigenvectors? 
# eigenvectors are the vector locations of these components. 
# Matrix multiplication of our original dataset with 'eigenvector-1' will generate the dataset 
# for comp 1. Similarly, you can generate data for other components as well. 
# This has essentially, rotated our data for predictor variables on orthogonal (eigenvector) axes.

# We can find the loading of our predictor variables on these components through a 
# correlation matrix constructed with these commands in R.
Correlation_Matrix=as.data.frame(round(cor(Data_for_PCA,pca$ind$coord)^2*100,0))
Correlation_Matrix[with(Correlation_Matrix, order(-Correlation_Matrix[,1])),]

# This correlation matrix tells us that 88% of the distance to the hospital is loaded on comp 1. 
# 100% of both carpet and built-up area is loaded on comp 2.

# If we remove component 4 to 6 from our data we will lose a little over 10% of the information. 
# This also means that ~39% of the information available in 'distance to market' will be lost 
# with component 4 & 5.

### Identify loading of variables on each component ############



dd<-as.data.frame(round(cor(Data_for_PCA,pca$ind$coord)^2*100,0))
dd[with(dd, order(-dd[,1])),]



#################################################

#Extract data for regression mode
numeric<-c("Dist_Taxi", "Dist_Market", "Dist_Hospital", "Carpet","Builtup", "Rainfall")
categoric <- c("Parking", "City_Category")
Target<-c("House_Price")

# Prepare train and test data for regression models

set.seed(42)
train <-sample(nrow(Clean_Data), 0.7*nrow(Clean_Data))
test<-setdiff(seq_len(nrow(Clean_Data)), train)

# 1st regression model with all the variables

Org_Reg<-lm(House_Price~.,data = Clean_Data[train,c(Target,numeric,categoric)])
summary(Org_Reg)
Estimate <- predict(Org_Reg, type="response", newdata=Clean_Data[test, c(numeric,categoric,Target)])
Observed <- subset(Clean_Data[test,c(numeric,categoric,Target)],select = Target)
format(cor(Estimate, Observed$House_Price)^2, digits=4)

# 2nd Regression model with principal components

require(FactoMineR)
Data_for_PCA<-Clean_Data[,numeric]
pca1<-PCA(Data_for_PCA)
PCA_data<-as.data.frame(cbind(Clean_Data[train,c(Target,categoric)],pca1$ind$coord[train,]))
Step_PCA_Reg<-step(lm(House_Price~.,data = PCA_data))
summary(Step_PCA_Reg)
PCA_Estimate <- predict(Step_PCA_Reg, type="response", newdata=cbind(Clean_Data[test,c(Target,categoric)],pca1$ind$coord[test,]))
format(cor(PCA_Estimate, Observed$House_Price)^2, digits=4)

# 3rd regression model with dominant variables

numeric_new<-c("Dist_Hospital", "Carpet")
New_Reg<-lm(House_Price~.,data = Clean_Data[train,c(Target,numeric_new,categoric)])
options(scipen=999)
summary(New_Reg)
New_Estimate <- predict(New_Reg, type="response", newdata=Clean_Data[test, c(numeric,categoric,Target)])
Observed <- subset(Clean_Data[test,c(numeric,categoric,Target)],select = Target)
format(cor(New_Estimate, Observed$House_Price)^2, digits=4)



