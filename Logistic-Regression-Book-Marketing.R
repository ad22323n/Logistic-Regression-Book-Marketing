"Logistic Regression Book Marketing"

book<-read.csv("BookMarketing.csv")

str(book)
table(book$buyer_f)
# 0     1 
# 45478  4522
summary(book$buyer_f)
# mean = 0.09044 indicates proportion of 1's
sum(book$buyer_f)
0.09044 * 50000

##################Shotgun Vs Rifle
shotgun Approach-->
  send offer to all 50,000-->
  Total net profit = 18*buyers + (-2)*nonbuyer-->
  
  18 * 4522 + 45478 *(-2) # gives -9560  
45478+4522
4522/50000  # 0.09044
45478/50000  # 0.90956  
###########################################
Strategy - pilot test on a subset
randomly select 10,000 and mail to all of them and see who responds
Based on this, build a model that can help predict who will respond

## Splitting the Data
library(caTools) #for sample.split

# Randomly split data  
# sample.split will divide data into 2 sets  
# Specifying first set should have 20% of the data (training)  
# the second set (test data) will have 80% of the data  
# sample.split will make sure similar propotion of buyers are in both the first and the second set (done by specifying books$buyer_f as the first argument)

set.seed(99)
mySplit<-sample.split(book$buyer_f, SplitRatio = 0.20)
mySplit[1:20]

# Given a vector and a ratio value, yields a logical vector where the given ratio of the elements are
# assigned the value TRUE, and the remaining elements are assigned the value FALSE  
# Preserves relative ratios of categories in books$buyer_f  
# i.e., splits it so that both subsets have similar proportion of bookbuyers.

summary(mySplit) # gives 40000 FALSE and 10000 TRUE
myTrain<-subset(book, mySplit==1) # equivalent to mySplit==TRUE
myTest<- subset(book, mySplit == 0)
table(myTrain$buyer_f)

# 9096 0's and 904 1's  (0.0904 = proportion of buyers)  
# Baseline model, for each observation, prob(buy) = 0.0904.    
# We wil predict buy if prob(buy) > 0.5.  

overall accuracy = # correctly predicted/total # observations  
  9096/10000 = 0.9096  

mean(myTrain$buyer_f) #0.0904
mean(myTest$buyer_f) #0.09045

## Logistic Regression Model
# Run logistic regression predicting buyer_f based on all 
# variables except for observation # (row #)

modeL<-glm(buyer_f~.-obs_num, data=myTrain, family = binomial)
summary(modeL)

modeL2<-glm(buyer_f~. -cook_f, data = myTrain, family=binomial)
summary(modeL2)
#pur3 has p value of 0.095197 (> 0.05), not bad, but try deleting since there are many other variables.

modeL22<-glm(buyer_f~. -obs_num -cook_f -pur3, data = myTrain, family=binomial)
summary(modeL22)

########################Overall model Significant
#Null deviance = deviance of baseline (no predictors, has only intercept term)  (sort of like SST)
chidiff= modeL22$null.deviance - modeL22$deviance
dfdiff = modeL22$df.null - modeL22$df.residual
pchisq(chidiff, dfdiff, lower.tail = F)

### What is AIC? Can use to compare models
Akaiker Information Criterion -This is like adj R^2
But not like adj R^2, lower AIC is better

* Check for multicollinearity
library(car)
vif(modeL22)

vif > 5 - potential problem with multicollinearity, but no such problem in this case.

#Make prediction 
Fred : l_pur=8, n_pur=3, pur3=1, pur7=2, cook_f=0, atlas_f=0, art_f=0
modeL22<-glm(buyer_f ~ gender + l_pur + n_pur + pur4 + pur5 + pur6 + pur7 + atlas_f + art_f, data = myTrain, family = binomial)
fred<-data.frame(obs_num =100, gender=1,l_pur=8, n_pur=3, pur3=1, pur4 = 0, pur5 = 0, pur6 = 0, pur7=2, cook_f=0, atlas_f=0, art_f=0)
predict(modeL22, type= "response", newdata=fred)

modeL22$coeff
modeL22$coeff*c(1,1,8,3,0,0,0,2,0,0)
Z<-sum(modeL22$coff*c(1,1,8,3,0,0,0,2,0,0))
prob<-1/(1+exp(-Z))
predict(modeL22, newdata = fred) # this gives the logit too

odds<-prob/(1-prob)

#Bob same as Fred but has purchased "Italian Art"  
Coeff here is 0.568
Bob's z = Fred's z + 0.568  
Bob's odds = exp(Fred's z + 0.568) = Fred odds * exp(0.568)

exp(0.568051)
odds*exp(0.568051)

#DIRECT CALCULATION OF BOB's ODD
bob <- data.frame(gender = 1, l_pur=8, n_pur=3, pur3=1, pur4 = 0, pur5 = 0, pur6 = 0, pur7=2, cook_f=0, atlas_f=0, art_f=1)
prob<-predict(modeL22, type="response", newdata = bob)
odds<-prob/(1-prob)

modeL22$fitted.values[1:10] # Show first 10 probabilities
predictTrain<-modeL22$fitted.values
# or alternatively, we can do
# predictTrain <- predict(model, type = "response") 
==>   for each person whose prob. > 0.5, classify them as buyer

#This is a classification matrix (also called confusion matrix).  
table(myTrain$buyer_f, predictTrain>0.5)
(9020 + 185)/1000 #0.9205 = overall accuracy  (increased from 0.9096)
9020 + 76 #actual 0's
719 + 185 #actual 1's
185/904 # tpr = true positive/actual positive = 0.205  (called sensitivity)
9020/9096 # tnr = true negative/actual negative = 0.992  (called specificity)
1-9020/9096 # fpr = false positive/actual negative = 0.0084

Overall accuracy = proportion of sample that is correctly classified
Sensitivity = #true positives /# actual positives 
  specificity = #true negatives / #actual negatives = proportion of non-buyers that are correctly identified
  Hit rate ( in marketing) = # of true positives / #predicted positives = proportion of predicted buyers that are actual buyers
  
  What if threshold is higher? (e.g., classify as buyer only if prop>0.7)
fewer predicted positive, more predicted negative

#Make prediction on the test data (of 40,000 observations)
predictTest<-predict(modeL22, type = "response", newdata = myTest) gives probabilities for test data with the model built on train data

#Build a classification matrix with the prediction on the test data
table(myTest$buyer_f, predictTest > 0.5)
sum(predictTest > 0.5) # predicted to buy
sum(myTest$buyer_f)  # actual buyers
sum(predictTest > 0.5 & myTest$buyer_f) # predicted to buy & actually buy (True positives)

############# Economic Analysis 
Total net profit
Profit from training data of 10,000 + profit from testing data of 40,000

profit1<-20*sum(myTrain$buyer_f) # profit from buyers in the training group
cost1<-2*nrow(myTrain) # cost of sending offer to everyone in the training group
predBuyer<- predictTest > 0.4

# predBuyer is logical vector of who are predicted to buy from the test group
cost2<-2*sum(predBuyer)  # cost of sending offer to those predicted to buy
profit2<-20*sum(predBuyer & myTest$buyer_f) #profit from the actual buyers who were predicted to buy
totalNet<-profit1 - cost1 + profit2 - cost2 # ( ) lets you print the resulting totalNet value

########Lowering the threshold from 0.5 seems to yield higher total net profit.
# Calculate total net profit for different threshold values:  0.5, 0.49, 0.48, ..., 0.05.
# Use *record* to store the results.

record<-data.frame(Threshold= numeric(), TotalProfit=numeric())
for(t in seq(0.5, 0.05, -0.01)){
  profit1<-20*sum(myTrain$buyer_f)
  cost1<-2*nrow(myTrain)
  predBuyer<-predictTest > t
  cost2<-2*sum(predBuyer)
  profit2<-20*sum(predBuyer & myTest$buyer_f)
  totalNet<-profit1 - cost1 + profit2 - cost2
  record<-rbind(record, data.frame(Threshold=t, TotalProfit= totalNet))
}

plot(record$Threshold, record$TotalProfit)
record[which.max(record$TotalProfit),] # identify the threshold with highest net profit

# The optimal threshold is 0.12 with maximum total profit of $27,562.
# This says send the offer to the customers whose probability of purchase is higher than 0.12.

####Receiver operator characteristic (ROC) curve
ROC curve can be used to (1) select a good threshold value and to (2) measure model performance

table(myTrain$buyer_f, predictTrain > 0.7)
9077 + 87 = #correct predictions, overall accuracy = 0.9164
  87/(817 + 87) #tpr = 0.0962
19/(9077+19)

false possitive = 19
actual negative = 9077 + 19

# Increasing tpr leads to increasing fpr 
# What is the acceptable threshold ? 
# Plot tpr and fpr for various threshod values

library(ROCR)
ROCRpred<-prediction(predictTrain, myTrain$buyer_f)

Given a vector prob's (predictionTrain) and label of 1's and 0's' (myTrain$buyer_f), makes prediction for various values of thresholds 

ROCRperf<- performance(ROCRpred, "tpr", "fpr")
plot(ROCRperf, colorize=TRUE, print.cutoffs.at=seq(0,1, by=0.1), text.adj=c(-0.2, 1.7))

#text.adj: first value - horizontal adj (neg = right), second value - vertical adg (neg = above)
if threshod t = 0, every observation will satisfy predTrain > 0, so every obs will be classified as 1.
The tpr=1 and fpr =1. As t is increased from 0 to 1, both tpr and fpr decrease.
Choose t where tpr is relatively high fpr acceptable lower.

performance(ROCRpred, measure = "auc")@y.values

0.8343852 = area under the curve
This measures the performance of the model, closer to 1 the better. Min is 0.5 -this is equivalent to random guessing.