#Setting the working directory
rm(list = ls(all=TRUE))
setwd("E:/INSOFE_CPEE/Wk5/03102017_Day02/20170903_Batch_32_CSE7302c_Lab02_M_Linear_Regression_Activity ")
###Reading the train data and test data (evaluation)###
train_data<-read.csv("CustomerData.csv",header=T,sep=",")
test<-read.csv("Eval.csv",header=T)

###Understanding the data### 
str(train_data)

###Are all the variables with appropriate data type. If not we need to convert them###
#Observe that the city variable is numeric type. Convert that to factor
train_data$City<-as.factor(train_data$City)
test$City<-as.factor(test$City)
custid<-train_data[,1]
train_data<-train_data[,-1]

###Looking at the summary of the train_train_data###
summary(train_data)

###Do you observe any anomalies###

#Observe that the maximum values for age is 113. Is it an outlier
#Lets draw boxplots for it
boxplot(train_data$MinAgeOfChild,train_data$MaxAgeOfChild)

###Remove the extreme points from the analysis###
train_data1<-train_data[!((train_data$MinAgeOfChild== max(train_data$MinAgeOfChild))|(train_data$MinAgeOfChild== max(train_data$MinAgeOfChild))),]
summary(train_data1)

###Build the Linear Regression Model and obtain the summary###
mod_lm<-lm(TotalRevenueGenerated~.,data=train_data1)
summary(mod_lm)


###Interpretation of the output###
#Residual standard error: 43.31 on 3193 degrees of freedom
#Multiple R-squared:  0.7202,	Adjusted R-squared:  0.7192 
#F-statistic:   685 on 12 and 3193 DF,  p-value: < 2.2e-16
#The model is significant as p-value is lower than 5% significance level

###Plot the model###
par(mfrow=c(2,2))
plot(mod_lm)

###What are your observations###
#The assumptions of linear regressions are satisfied.
# ResidualsVsFitted - Model is linear
# Normal Q-Q: Error terms are normally distributed
#Scale-Location: Error terms have constant variances, no heteroscadasticity
#Residuals vs Leverage - No outlier >1 that can influence.

### Make Predictions using the model###
pred_1<-predict(mod_lm,newdata=train_data1)
regr.eval(train_data1$TotalRevenueGenerated,pred_1)
#mae          mse         rmse         mape 
#31.2463178 1867.7510866   43.2174859    0.1885158

pred_2<-predict(mod_lm,newdata=test)
#mae          mse         rmse         mape 
#30.4440899 1809.2906653   42.5357575    0.2044514

#Observation 1 - test data has mor eerror than train data. Overfitting issue?, Can try by reduicing dimensions.

###Evaluation on metric###
library(DMwR)
regr.eval(test$TotalRevenueGenerated,pred_2)


###Are the the variables really significant in explaining the 
##dependent variable/target. Can we reduce the dimensions ?
##Lets try to identify important variables in explaining the y
library(car)
vif(mod_lm)

library(MASS)
stepAIC(mod_lm)
names(train_data1)
#removed MaxAgeOfChild
mod_lm_mass<-lm(formula = TotalRevenueGenerated ~ City + NoOfChildren + MinAgeOfChild + 
                  Tenure + FrquncyOfPurchase + NoOfUnitsPurchased + FrequencyOFPlay + 
                  NoOfGamesPlayed + NoOfGamesBought+FavoriteChannelOfTransaction+
                  FavoriteGame,data = train_data)
summary(mod_lm_mass)

####What to be done with extreme points observed in the plots###

#Remove the points that are extreme.. Do we need to remove the points at all in the first place?
#What you observe in the plots are the rownames.
ext=c(1764,974,667)
train_data2<-train_data1[!rownames(train_data1)%in%ext,-14]
mod_lm_2<-lm(TotalRevenueGenerated~.,data=train_data2)
summary(mod_lm_2)
plot(mod_lm_2)
pred_3<-predict(mod_lm_2,newdata=train_data2)
regr.eval(train_data2$TotalRevenueGenerated,pred_3)[4] #Removed extreme point 
#mae          mse         rmse         mape 
#30.8783676 1770.2526220   42.0743701    0.1872557 

regr.eval(train_data1$TotalRevenueGenerated,pred_1)[4] 
#mae          mse         rmse         mape 
#31.2463178 1867.7510866   43.2174859    0.1885158

pred_4<-predict(mod_lm_2,newdata=test)
regr.eval(test$TotalRevenueGenerated,pred_4)[4] #Removed extreme points # mape = 0.20417
regr.eval(test$TotalRevenueGenerated,pred_2)[4]   # mape = 0.2044514

#Observation 2 - After reduced dimension(removed MaxAgeOfChild), Error for test data is still more than train data. There could be some overfitting problem. Using PCA

##Assignment 7a
#Model 1 - Using tain_data i.e. having extreame point in minAge 113
str(train_data)

#Building model on principal components
#train_data$City <- as.numeric(train_data$City)
train_data$City <- NULL
train_data$FavoriteChannelOfTransaction<-as.numeric(train_data$FavoriteChannelOfTransaction)
train_data$FavoriteGame <- as.numeric(train_data$FavoriteGame)
str(train_data)

train_data.predictors <- train_data[,-12]
str(train_data.predictors)
scaled.Predictors <- scale(train_data.predictors)
scaled.Predictors
pca <- princomp(scaled.Predictors)
summary(pca)
#Importance of components:
#                       Comp.1    Comp.2    Comp.3     Comp.4     Comp.5     Comp.6     Comp.7     Comp.8
#Standard deviation     1.8693256 1.1802985 1.1725712 1.04025497 0.99207205 0.96612985 0.91688727 0.69793456
#Proportion of Variance 0.3177698 0.1266854 0.1250320 0.09840616 0.08950125 0.08488162 0.07644948 0.04429677
#Cumulative Proportion  0.3177698 0.4444551 0.5694871 0.66789327 0.75739452 0.84227614 0.91872563 0.96302240
                        #Comp.9    Comp.10     Comp.11
#Standard deviation     0.49863967 0.35585857 0.177059341
#Proportion of Variance 0.02261082 0.01151589 0.002850889
#Cumulative Proportion  0.98563322 0.99714911 1.000000000

names(pca)
plot(pca)

# 9 PCA components explains about 98.56 % of variance. So limit to 9.
pc_traindata<-data.frame(pca$scores[,1:9],train_data$TotalRevenueGenerated)

#build model on pca data
mod_pc_lm1<-lm(train_data.TotalRevenueGenerated~.,data=pc_traindata)
summary(mod_pc_lm1)

#predict based on pca
#test$City <- as.numeric(test$City)
test$City <- NULL
test$FavoriteChannelOfTransaction<-as.numeric(test$FavoriteChannelOfTransaction)
test$FavoriteGame <- as.numeric(test$FavoriteGame)
str(test)

pc_test<-predict(pca,newdata=test[1:11])
pc_test<-data.frame(pc_test[,1:11])

#Apply new model on this transformed test data
pred_test_pc<-predict(mod_pc_lm1,newdata=pc_test)

#calculate evaluation metrics
regr.eval(test$TotalRevenueGenerated,pred_test_pc)
#mae          mse         rmse         mape 
#2.456351e+03 1.182522e+07 3.438782e+03 1.764803e+01

plot(pca)

#Model 2 using PCA, Using train_data1, removed extreame point in MinAgeOfChild
str(train_data1)

#Building model on principal components
#train_data1$City <- as.numeric(train_data1$City)
train_data1$City <- NULL
train_data1$FavoriteChannelOfTransaction<-as.numeric(train_data1$FavoriteChannelOfTransaction)
train_data1$FavoriteGame <- as.numeric(train_data1$FavoriteGame)
str(train_data1)

train_data1.predictors <- train_data1[,-12]
str(train_data1.predictors)
scaled.Predictors <- scale(train_data1.predictors)
scaled.Predictors
pca_2 <- princomp(scaled.Predictors)
summary(pca_2)

#Importance of components:
#                       Comp.1    Comp.2    Comp.3    Comp.4     Comp.5     Comp.6     Comp.7     Comp.8
#Standard deviation     1.8777166 1.2091011 1.0977269 1.0527521 0.99833995 0.96747167 0.90689618 0.72900932
#Proportion of Variance 0.3206291 0.1329438 0.1095800 0.1007848 0.09063578 0.08511759 0.07479248 0.04832913
#Cumulative Proportion  0.3206291 0.4535728 0.5631529 0.6639377 0.75457344 0.83969103 0.91448351 0.96281263
#                        Comp.9    Comp.10     Comp.11
#Standard deviation     0.50072904 0.35615599 0.177078409
#Proportion of Variance 0.02280071 0.01153515 0.002851504
#Cumulative Proportion  0.98561334 0.99714850 1.000000000

# 9 PCA components explains about 98.56 % of variance. So limit to 9.
pc_traindata1<-data.frame(pca_2$scores[,1:9],train_data1$TotalRevenueGenerated)

#build model on pca data
mod_pc_lm2<-lm(train_data1.TotalRevenueGenerated~.,data=pc_traindata1)

#predict based on pca
#test$City <- as.numeric(test$City)
test$City <- NULL
test$FavoriteChannelOfTransaction<-as.numeric(test$FavoriteChannelOfTransaction)
test$FavoriteGame <- as.numeric(test$FavoriteGame)
str(test)

pc_test<-predict(pca,newdata=test[1:11])
pc_test<-data.frame(pc_test[,1:11])

#Apply new model on this transformed test data
pred_test_pc<-predict(mod_pc_lm2,newdata=pc_test)

#calculate evaluation metrics
regr.eval(test$TotalRevenueGenerated,pred_test_pc)
#mae          mse         rmse         mape 
#3.293487e+03 1.303975e+07 3.611059e+03 2.405071e+01  

plot(pca)

#Findings/Conclusions
# Errors are much lower using PCA. Reduction in dimensions has resulted in a better model with less errors.
