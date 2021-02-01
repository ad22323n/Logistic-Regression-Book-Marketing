bank<- read.csv("BankSalaries(1).csv")
library(ggplot2)

The Fifth National Bank of Springfield is facing a gender-discrimination suit. The charge is that its female employees receive substantially smaller salaries than its male employees.
The bank’s database of 208 employees is listed in this file. Here is a partial list of the data.

let's look at the structure of our data, then summarise it in order to see if it does provide evidence that females are discriminated against in terms of salary.
To do this, we will perform regression with Salary as the dependent variable and all other variables as independent variables.

str(bank)
summarizeColumns(bank)
summary(bank)

Here we will Compare the average salaries of the males and females
tapply(bank$Salary, bank$Gender, mean)

The above previous shows that The difference is $8,295.
The females are definitely earning less, but perhaps there is a reason; let's visualize our data in order to see how salary is distributed based on gender:

Some potential explanatory variables are categorical and cannot be measured on a quantitative scale.
Therefore, we will create dummy variables, also called indicator or 0-1 variables.
These are variables that indicate the category a given observation is in.

Exemple of what we will be doing in the next If in Excel:  Add a column with heading Female and enter 	=IF(G2=“Female”,1,0) in the first row.  Copy down.

bank$Gr<- factor(bank$Grade)
summary(bank$Gr)
bank$Ed<-factor(bank$Education, levels = 1:5, labels = c("HS","Some College",
                                                         "Bachelors", "Some Grad","Grad Degree"))
summary(bank$Ed)
str(bank)

In this following section, we will visualize the salary against Education Level to see if the degree has in impact on the salay:

ggplot(bank, aes(Salary)) + geom_histogram() + facet_wrap(~Ed)

ggplot(bank, aes(x=Ed, y=Salary)) + geom_boxplot()

Salary against Grade of the position: 
ggplot(bank, aes(Gr, Salary)) + geom_boxplot() 

fit<- lm(Salary ~ Years1 + Gender + Years1:Gender, data=bank)
summary(fit)
#MALE: SALARY = 34528.3 + 280 Years1 - 4098.3 + 1247.8 Years1
280 + 1247.8
34528.3 - 4098.3

#MALE's Salary = 30430 + 1527.8 Years1
#FEMALE: Salary = 34528.3 + 280 Years1

#++++++++++++++++++++PREDICT SALARY BASED ON GENDER
# Salary = 37210 + 8296 GenderMale
# 37210 was ave salary of female
# 8296 was the difference between ave salary of male and female

names(bank)
fit1<-lm(Salary ~ Years1 + Years2 + Age + Gender + PC.Job + Gr + Ed, data=bank)
summary(fit1)

fit2<-lm(Salary~ Years1 + Years2 + Gender + PC.Job + Gr + Ed, data=bank)
summary(fit2)

fit3<-lm(Salary~Years1 + Gender + PC.Job + Gr, data=bank)
summary(fit3)

fit4<-lm(Salary ~ Years1 + Gender + PC.Job + Gr, data=bank)
summary(fit4)

# Regression model:
# Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 
#  Salary = 27781.34 +  
#           441.90 Years1 +      
#    2785.68 GenderMale  +    
#      5376.26 PC.JobYes  +  0 Gr1 
#   2084.24 Gr2 + 6187.30 Gr3 + 10060.50 Gr4 +  
# 16052.48 Gr5 + 26584.57 Gr6

# Salary difference betwe male and female after accounting
# for the other factors (experience, pcjob, job grade)
# $2,786 compared to $8,296.

Predict expected Salary for the following:

# Alyssa: female, worked for 10 years, job grade 4
27781.34 +  441.90*10 +  5376.26*1 + 10060.50*1
# 47637.1 predicted salary

Alyssa<-data.frame(Gender="Female", Years1=10, Gr="4", PC.Job="Yes")
predict(fit4, newdata = Alyssa)
predict(fit4, newdata = Alyssa, interval = "prediction", level = 0.95)

Cathy<-data.frame(Gender="Female", Years1=10, Gr="1",PC.Job="Yes")
predict(fit4, newdata = Cathy) #37576.56
47637.06 - 37576.56 
# Difference is 10060.5

# Also, coeff of Gr1 = 0 (Gr1 was a reference category that was not included in regression)
# coeff of Gr4 = 10060.5.  So the difference in salary betw Cathy and Alyssa should be: 10060.5 - 0 = 10060.5
47637.06 - 10060.5

# Cathy's salary is expected to be 37576.56

# Beth: job grade is 5
# Doug: male

53629.08
47637.06 + 16052.48 - 10060.50 = 53629.04

# Doug
47637.06 +2785.68 = 50422.74

To conclude, we can say that men tend to get higher 👆  salary in this bank 🏦 than woment. The difference between the average salary was more than 5000.
