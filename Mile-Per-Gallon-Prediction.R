MULTIPLE LINEAR REGRESSION ON MPG auto car
predict mp(Mile per Gallon) using multiple predictors in Dataset.

library(corrplot)
library(dplyr)
library(ggplot2)

data('Auto')
head(Auto)

str(Auto)
summary(Auto)

#in multiple regression we will use more than one predictiors
autodf<-select(Auto,-name)

summary(autodf)

plot(autodf, col="navy")

#Splitting Data
require(caTools)
sample = sample.split(autodf, SplitRatio = .80) #After splitting marks these rows as logical TRUE and the the remaining are marked as logical FALSE
trainDf<-subset(autodf, sample==TRUE)
testDF<-subset(autodf, sample==FALSE)

summary(trainDf)
summary(testDF)


##############___RULES___####################
If p-value ≤ 0.05 for each X, stop.  The regression equation is valid as is.
If there is an X with  p-value > 0.05, run another regression excluding this X.
If there are more than one X with p-value > 0.05, discard the one with highest p-value and run another regression.  
Do this until the equation contains only X variables with p-value ≤ 0.05.

#++++++++++++++++++++A PREDICTOR WITH A LOW P VALUE IS LIKELY TO BE MEANINGFUL TO MY MODEL
P VALUE < 0.05

#Fitting Models
fit<-lm(mpg~., data=trainDf)
summary(fit) #P value of cylinder & Acceleration show no significant relation with mpg

#correlation
cor = cor(testDF[1:8])
corrplot(cor, method = "number") #High correlation between displacement,horsepower and weight.

#As the cylinders don’t show any significant relation in the first model with mpg we will exclude cylinders. Out of displacement, horsepower, 
#and weight, the output of the first model shows that weight and mpg have a highly significant relation. We will use weight out of the other 
#collinear variables in the next model.Hide

model1<-lm(mpg~weight+year+origin, data=trainDf)
summary(model1) #no improvement on RSE & adjusted RS value.
plot(model1, which = 1) # outliers on the top-right side

model2<-lm(log(mpg)~weight+year+origin, data=trainDf)
summary(model2)#F-stat, Adjusted RS value increased
plot(model2, which = 1)

model3<-lm(log(mpg)~log(weight)+year+origin, data=autodf)
summary(model3)  
plot(model3, which = 1)  
#Higher P value for origin, lets exclude it and see because it does not have strong relation with mpg

model4<-lm(log(mpg)~log(weight)+year, data = autodf)
summary(model4)
#The F-stat & Adjusted R-Squared haved increased .Predictor p values<0.05.
plot(model4, which = 1)

#Accuracy
predictions<-predict(model4, testDF)
mse<-mean((testDF$mpg - predictions)^2)
print(mse)
492.0838

sigma(model4)/mean(testDF$mpg)
0.004912697

testDF$predValue<-predict(model4, testDF)
PredictValue<-data.frame(testDF$mpg, testDF$predValue)
names(PredictValue)<-c("mpg", "Prediction")
correlation<-cor(PredictValue)
correlation # accuracy is roughly 92%

head(PredictValue)

PredictValue$logMpg<-log(PredictValue$mpg)
head(PredictValue)

we used Multiple Linear Regression methods to find the mpg of the car. We had 8 columns out of which 4 were collinear to each other. we got the best model with only two predictors. 
log(mpg)=8.0395+log(weight)∗−.9341+year∗.0328



