#Name: Anoop Nair
#Module: Data science & Analytics
#Student Id: R00223644 

######## set directory  #######

setwd("D:/STUDY MATERIAL/Masters DS & Analytics/DataScience & Analytics/Assignment")
#getwd()

####### Install and load required packages #######

library (ggplot2)
library(plotly)
library(tidyr)
library(pastecs)
library(tidyverse)
library(psych)
library(plyr)
library(e1071)
library(readxl)
library(lubridate)
library(anytime)
library(gganimate)
library(tibble)
library(formattable)
library(gridExtra)
library(GGally)
library(stats)
library(dplyr)
library(tree)
library(randomForest)


#########################################
#    LOADING & CLEANING DATA
#########################################


# Load the file csv file into a data frame ######

CreditData=read.csv("Credit_Risk_25_final-2.csv")
head(CreditData)
str(CreditData)

#write.csv(CreditData, "data.csv", row.names=TRUE)

#### change the long names  to concise and relevant ones ####

colnames(CreditData)=c('ID','Accounttype','CreditHistory','LoanReason',"SavingsAccount","Employment",
                       "MaritalStatus","Housing","JobType","ForeignNational","AccountActivemonths","CurrentResidenceDuration","Age","CreditStanding")

## New names of column names are as below, going forward we will use these names.

# ID --> ID  ;   
# Checking.Acct --> Accounttype
# Credit.History --> CreditHistory
# Loan.Reason --> LoanReason
# Savings.Acct --> SavingsAccount
# Employment --> Employment
# Personal.Status --> MaritalStatus
# Housing --> Housing
# Job.Type --> JobType
# Foreign.National --> ForeignNational
# Months.since.Checking.Acct.opened --> AccountActivemonths
# Residence.Time.In.current.district --> CurrentResidenceDuration
# Age --> Age
# Credit.Standing --> CreditStanding

######## convert categorical values into factor variable #######

CreditData$Accounttype=as.factor(CreditData$Accounttype)
CreditData$CreditHistory=as.factor(CreditData$CreditHistory)
CreditData$LoanReason=as.factor(CreditData$LoanReason)
CreditData$SavingsAccount=as.factor(CreditData$SavingsAccount)
CreditData$Employment=as.factor(CreditData$Employment)
CreditData$MaritalStatus=as.factor(CreditData$MaritalStatus)
CreditData$Housing=as.factor(CreditData$Housing)
CreditData$JobType=as.factor(CreditData$JobType)
CreditData$ForeignNational=as.factor(CreditData$ForeignNational)
CreditData$CreditStanding=as.factor(CreditData$CreditStanding)

#str(CreditData)
#colnames(CreditData)
########## check for missing/Blank values in all the columns #######

### There are no NA values in the table
colSums(is.na(CreditData))

######## convert age from numeric to integer value as we do not require decimal precision for age


CreditData$Age=as.integer(CreditData$Age)

### there are null values in the table though

## employement column  has 11 blank values
## Marital status has 6 blank values
## Housing has 5 blank values

###### imputing the blank values in the categorical columns with the their mode###

calmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
} ### this function is taken from a quick reference online reference from online

calmode(CreditData$Employment) # Short is the mode

### imputing with employment column blank values
CreditData["Employment"][CreditData["Employment"]==" "]="Short"

### imputing with Housing column, 5 blank values

calmode(CreditData$Housing) # Own is the mode

CreditData["Housing"][CreditData["Housing"]==""]="Own"

### imputing Marital status , 6 blank values

calmode(CreditData$MaritalStatus) # Single is the mode

CreditData["MaritalStatus"][CreditData["MaritalStatus"]==""]="Single"

### there is negative value in the CurrentResidenceDuration column ###

CreditData["CurrentResidenceDuration"][CreditData["CurrentResidenceDuration"]<0] # -2
count(CreditData["CurrentResidenceDuration"][CreditData["CurrentResidenceDuration"]<0]) #1

#### replacing the negative with an imputed median value for CurrentResidenceDuration =3
apply(CreditData,2,median)
CreditData["CurrentResidenceDuration"][CreditData["CurrentResidenceDuration"]<0]=3


###########################################################
#              Exploratory Data Analysis 
#              Question A
###########################################################

### Overall in the Dataset Bad Loans are 40.8 % and Good loans are 59.2 % 

round(prop.table(table(CreditData$CreditStanding))*100,1)
boxplot(table(CreditData$CreditStanding,CreditData$Accounttype))

### let us find the 2-D freq table for various categorical variables ###

#### Account Type ###
#### 40 % of bad loans has Accounttype as "0Balance"

round(prop.table(table(CreditData$Accounttype))*100,1)
round(prop.table(table(CreditData$CreditStanding,CreditData$Accounttype), margin = 1)*100,1)

##### Credit History ####
## 58 % of bad loans has CreditHistory as "Current"

round(prop.table(table(CreditData$CreditHistory))*100,1)
round(prop.table(table(CreditData$CreditStanding,CreditData$CreditHistory), margin = 1)*100,1)


#### LoanReason ####

## highest % of bad loans has LoanReason as "Car New" , "Furniture" , "Small Appliance"

round(prop.table(table(CreditData$LoanReason))*100,1)
round(prop.table(table(CreditData$CreditStanding,CreditData$LoanReason), margin = 1)*100,1)

##### Savings Account ####
## 68.4 % of bad loans has SavingsAccount as " Low"

round(prop.table(table(CreditData$SavingsAccount))*100,1)
round(prop.table(table(CreditData$CreditStanding,CreditData$SavingsAccount), margin = 1)*100,1)

#### Employment ####

## 46.8 % of bad loans has Employment as "Short" , "Very short" , "Long"

round(prop.table(table(CreditData$Employment))*100,1)
round(prop.table(table(CreditData$CreditStanding,CreditData$Employment), margin = 1)*100,1)
unique(CreditData$Employment)
#### MaritalStatus ###

## 51 % of bad loans has MaritalStatus as "Single" and 39% as "Divorced"

round(prop.table(table(CreditData$MaritalStatus))*100,1)
round(prop.table(table(CreditData$CreditStanding,CreditData$MaritalStatus), margin = 1)*100,1)


#### Housing #####
###### 62% of Bad accounts are have their own House ######

round(prop.table(table(CreditData$Housing))*100,1)
round(prop.table(table(CreditData$CreditStanding,CreditData$Housing), margin = 1)*100,1)

########### job type #######

##### around 67% of the Bad is contributed by skilled #####

round(prop.table(table(CreditData$JobType))*100,1)
round(prop.table(table(CreditData$CreditStanding,CreditData$JobType), margin = 1)*100,1)


######## foreignNational #######

##### about 67.5% of bad loans are from foreign national

round(prop.table(table(CreditData$ForeignNational))*100,1)
round(prop.table(table(CreditData$CreditStanding,CreditData$ForeignNational), margin = 1)*100,1)



######## Add new columns for continuous variables AccountActivemonths,CurrentResidenceDuration,Age

### GROUP ACTIVEMONTHS
str(CreditData)
min(CreditData$AccountActivemonths)
max(CreditData$AccountActivemonths)
CreditData$AccountActivemonths_grp=cut(CreditData$AccountActivemonths, breaks = c(0,10,20,30,40,50,Inf),include.lowest = TRUE)


CreditData[c("AccountActivemonths","AccountActivemonths_grp")]

round(prop.table(table(CreditData$AccountActivemonths_grp))*100,1)
round(prop.table(table(CreditData$CreditStanding,CreditData$AccountActivemonths_grp), margin = 1)*100,1)



#### GROUP CurrentResidenceDuration

min(CreditData$CurrentResidenceDuration)
max(CreditData$CurrentResidenceDuration)

CreditData$CurrentResidenceDuration_grp=cut(CreditData$CurrentResidenceDuration, breaks = c(1,2,3,Inf),include.lowest = TRUE)
CreditData[c("CurrentResidenceDuration","CurrentResidenceDuration_grp")]

round(prop.table(table(CreditData$CurrentResidenceDuration_grp))*100,1)
round(prop.table(table(CreditData$CreditStanding,CreditData$CurrentResidenceDuration_grp), margin = 1)*100,1)



##### GROUP AGE

min(CreditData$Age)
max(CreditData$Age)

CreditData$Age_grp=cut(CreditData$Age, breaks = c(10,20,30,40,50,Inf),include.lowest = TRUE,right = TRUE)
CreditData[c("Age","Age_grp")]
count(is.na(CreditData$Age_grp)==TRUE)

round(prop.table(table(CreditData$Age_grp))*100,1)
round(prop.table(table(CreditData$CreditStanding,CreditData$Age_grp), margin = 1)*100,1)





##### AccountActivemonths_grp

round(prop.table(table(CreditData$CreditStanding,CreditData$AccountActivemonths_grp), margin = 1)*100,1)


######### CurrentResidenceDuration_grp ###########

round(prop.table(table(CreditData$CreditStanding,CreditData$CurrentResidenceDuration_grp), margin = 1)*100,1)

############ Age_grp #########

round(prop.table(table(CreditData$CreditStanding,CreditData$Age_grp), margin = 1)*100,1)




####### central tendency for continuous variable
par(mfrow=c(1,2))

#### For AccountActivemonths ####


hist(CreditData$AccountActivemonths, col = "pink",main="Histogram of AccountActivemonths",xlab ="Account active in months")
boxplot(CreditData$AccountActivemonths, col = "pink",main="Boxplot of AccountActivemonths",ylab='Duration in months')
summary(CreditData$AccountActivemonths)
describe(CreditData$AccountActivemonths)
### For Age

hist(CreditData$Age, col = "pink",main="Histogram of Age",xlab ="Age in years")
boxplot(CreditData$Age, col = "pink",main="Boxplot of Age",ylab='Age in years')
summary(CreditData$Age)
describe(CreditData$Age)
### For CurrentResidenceDuration

hist(CreditData$CurrentResidenceDuration, col = "pink",main="Histogram of CurrentResidenceDuration",xlab ="CurrentResidenceDuration in months")
boxplot(CreditData$CurrentResidenceDuration, col = "pink",main="Boxplot of CurrentResidenceDuration",ylab='CurrentResidenceDuration in months')
summary(CreditData$CurrentResidenceDuration)
describe(CreditData$CurrentResidenceDuration)
###### Barplots for all the categorical variables ######

str(CreditData)

par(mfrow=c(2,2))
barplot(round(prop.table(table(CreditData$Accounttype))*100,1),col = "pink",main="Barplot of Accounttype",ylab='Percentage')
barplot(round(prop.table(table(CreditData$CreditHistory))*100,1),col = "pink",main="Barplot of CreditHistory",ylab='Percentage')
barplot(round(prop.table(table(CreditData$LoanReason))*100,1),col = "pink",main="Barplot of LoanReason",ylab='Percentage')
barplot(round(prop.table(table(CreditData$SavingsAccount))*100,1),col = "pink",main="Barplot of SavingsAccount",ylab='Percentage')

par(mfrow=c(2,2))
barplot(round(prop.table(table(CreditData$Employment))*100,1),col = "pink",main="Barplot of Employment",ylab='Percentage')
barplot(round(prop.table(table(CreditData$MaritalStatus))*100,1),col = "pink",main="Barplot of MaritalStatus",ylab='Percentage')
barplot(round(prop.table(table(CreditData$Housing))*100,1),col = "pink",main="Barplot of Housing",ylab='Percentage')
barplot(round(prop.table(table(CreditData$JobType))*100,1),col = "pink",main="Barplot of JobType",ylab='Percentage')


####

par(mfrow=c(2,2))

barplot(round(prop.table(table(CreditData$ForeignNational))*100,1),col = "pink",main="Barplot of ForeignNational",ylab='Percentage')
barplot(round(prop.table(table(CreditData$AccountActivemonths_grp))*100,1),col = "pink",main="Barplot of AccountActivemonths",ylab='Percentage',xlab='AccountActivemonths bracket')
barplot(round(prop.table(table(CreditData$Age_grp))*100,1),col = "pink",main="Barplot of Age",xlab ="Age Brackets in Years")
barplot(round(prop.table(table(CreditData$CurrentResidenceDuration_grp))*100,1),col = "pink",main="Barplot of CurrentResidenceDuration",ylab='Percentage',xlab ="CurrentResidenceDuration Brackets in months")

######## All the graphs together with their percentage ####


par(mfrow=c(1,3))

barplot(prop.table(table(CreditData$CreditStanding,CreditData$Accounttype), margin = 1)*100,
        col=1:nrow(CreditData),xlab="Accountype",ylab="Percentage",main="Credit standing split Accountypewise")

legend("topright",
       legend = c("Bad", "Good"),
       pch = 15,col=1:nrow(CreditData))
abline(h=40.8)

barplot(prop.table(table(CreditData$CreditStanding,CreditData$CreditHistory), margin = 1)*100,
        col=1:nrow(CreditData),xlab="CreditHistory",ylab="Percentage",main="Credit standing split CreditHistorywise")

legend("topright",
       legend = c("Bad", "Good"),
       pch = 15,col=1:nrow(CreditData))
abline(h=40.8)

barplot(prop.table(table(CreditData$CreditStanding,CreditData$LoanReason), margin = 1)*100,
        col=1:nrow(CreditData),xlab="LoanReason",ylab="Percentage",main="Credit standing split LoanReasonwise")

legend("topright",
       legend = c("Bad", "Good"),
       pch = 15,col=1:nrow(CreditData))
abline(h=40.8)


par(mfrow=c(1,3))



barplot(prop.table(table(CreditData$CreditStanding,CreditData$SavingsAccount), margin = 1)*100,
        col=1:nrow(CreditData),xlab="SavingsAccount",ylab="Percentage",main="Credit standing split SavingsAccountwise")

legend("topright",
       legend = c("Bad", "Good"),
       pch = 15,col=1:nrow(CreditData))
abline(h=40.8)


barplot(prop.table(table(CreditData$CreditStanding,CreditData$Employment), margin = 1)*100,
        col=1:nrow(CreditData),xlab="Employment",ylab="Percentage",main="Credit standing split Employmentwise")

legend("topright",
       legend = c("Bad", "Good"),
       pch = 15,col=1:nrow(CreditData))
abline(h=40.8)



barplot(prop.table(table(CreditData$CreditStanding,CreditData$MaritalStatus), margin = 1)*100,
        col=1:nrow(CreditData),xlab="MaritalStatus",ylab="Percentage",main="Credit standing split MaritalStatuswise")

legend("topright",
       legend = c("Bad", "Good"),
       pch = 15,col=1:nrow(CreditData))
abline(h=40.8)


par(mfrow=c(1,3))


barplot(prop.table(table(CreditData$CreditStanding,CreditData$Housing), margin = 1)*100,
        col=1:nrow(CreditData),xlab="Housing",ylab="Percentage",main="Credit standing split Housingwise")

legend("topright",
       legend = c("Bad", "Good"),
       pch = 15,col=1:nrow(CreditData))
abline(h=40.8)



barplot(prop.table(table(CreditData$CreditStanding,CreditData$JobType), margin = 1)*100,
        col=1:nrow(CreditData),xlab="JobType",ylab="Percentage",main="Credit standing split JobTypewise")

legend("topright",
       legend = c("Bad", "Good"),
       pch = 15,col=1:nrow(CreditData))
abline(h=40.8)



barplot(prop.table(table(CreditData$CreditStanding,CreditData$ForeignNational), margin = 1)*100,
        col=1:nrow(CreditData),xlab="ForeignNational",ylab="Percentage",main="Credit standing split ForeignNationalwise")

legend("topright",
       legend = c("Bad", "Good"),
       pch = 15,col=1:nrow(CreditData))
abline(h=40.8)

############## for the continuous variables group ##############
par(mfrow=c(1,3))


barplot(prop.table(table(CreditData$CreditStanding,CreditData$AccountActivemonths_grp), margin = 1)*100,
        col=1:nrow(CreditData),xlab="AccountActivemonths_grp",ylab="Percentage",main="Credit standing split AccountActivemonths_grp")

legend("topright",
       legend = c("Bad", "Good"),
       pch = 15,col=1:nrow(CreditData))
abline(h=40.8)


barplot(prop.table(table(CreditData$CreditStanding,CreditData$Age_grp), margin = 1)*100,
        col=1:nrow(CreditData),xlab="Age_grp",ylab="Percentage",main="Credit standing split Age_grp")

legend("topright",
       legend = c("Bad", "Good"),
       pch = 15,col=1:nrow(CreditData))
abline(h=40.8)


barplot(prop.table(table(CreditData$CreditStanding,CreditData$CurrentResidenceDuration_grp), margin = 1)*100,
        col=1:nrow(CreditData),xlab="CurrentResidenceDuration_grp",ylab="Percentage",main="Credit standing split CurrentResidenceDuration_grp")

legend("topright",
       legend = c("Bad", "Good"),
       pch = 15,col=1:nrow(CreditData))
abline(h=40.8)

####### Combinations of 3 variables (trivariate analysis) #####
#str(CreditData)

### majority of Bad loans is explained by CreditHistory and there is no further additions by SavingsAccount we can exclude it
round(prop.table(ftable(CreditData$CreditStanding,CreditData$SavingsAccount,CreditData$CreditHistory), margin = 1)*100,2)
round(prop.table(ftable(CreditData$CreditStanding,CreditData$SavingsAccount,CreditData$CreditHistory), margin = 1)*100,1)
## CreditHistory Vs ( Credithistory + Accounttype )
round(prop.table(ftable(CreditData$CreditStanding,CreditData$Accounttype,CreditData$CreditHistory), margin = 1)*100,1)
## CreditHistory Vs ( Credithistory + Employment )
round(prop.table(ftable(CreditData$CreditStanding,CreditData$Employment,CreditData$CreditHistory), margin = 1)*100,1)
## CreditHistory Vs ( Credithistory + Maritalstatus )
round(prop.table(ftable(CreditData$CreditStanding,CreditData$MaritalStatus,CreditData$CreditHistory), margin = 1)*100,1)
## CreditHistory Vs ( Credithistory + Housing )
round(prop.table(ftable(CreditData$CreditStanding,CreditData$Housing,CreditData$CreditHistory), margin = 1)*100,1)
## CreditHistory Vs ( Credithistory + Jobtype )
round(prop.table(ftable(CreditData$CreditStanding,CreditData$JobType,CreditData$CreditHistory), margin = 1)*100,1)
## CreditHistory Vs ( Credithistory + Jobtype )
round(prop.table(ftable(CreditData$CreditStanding,CreditData$JobType,CreditData$CreditHistory), margin = 1)*100,1)
## CreditHistory Vs ( Credithistory + Loanreason )
round(prop.table(ftable(CreditData$CreditStanding,CreditData$LoanReason,CreditData$CreditHistory), margin = 1)*100,1)
## CreditHistory Vs ( Credithistory + AccountActivemonths_grp )
round(prop.table(ftable(CreditData$CreditStanding,CreditData$AccountActivemonths_grp,CreditData$CreditHistory), margin = 1)*100,1)
## CreditHistory Vs ( Credithistory + Age_grp )
round(prop.table(ftable(CreditData$CreditStanding,CreditData$Age_grp,CreditData$CreditHistory), margin = 1)*100,1)
## CreditHistory Vs ( Credithistory + AccountActivemonths_grp )
round(prop.table(ftable(CreditData$CreditStanding,CreditData$CurrentResidenceDuration_grp,CreditData$CreditHistory), margin = 1)*100,1)


############################################
              #Question B
############################################

# split dataset into 75% / 25% , Also have maintained the same proportion

set.seed(644) # set a random seed last 3 digits of 3 of student Id

library(caret)

intrain<-createDataPartition(y=CreditData$CreditStanding,p=0.75,list=FALSE)
train=CreditData[intrain,] #nrow(train)
test=CreditData[-intrain,] #nrow(test)

#str(train)
#str(CreditData)
############################################
              #Question C
############################################

## entropy for categorical variables are calculated using the below function
## the function gets the probability of the dependent variable CreditStanding (Bad/Good)
## in each of the class of the categorical variable and multiply it by the log base 2 of the probability. 
## sum the -p*log(p) and then multiply to weighted value of the proportion of  class on the overall level.
## later the entropy value we have is subtracted from the overall entropy of CreditStanding to get the Information gain
## a reduction in entropy from overall means the node split will help in producing more pure leaf nodes.

##### Note: below code is taken from the lab exercises and tweaked to fit the current scenario.

entropy_tab <- function(x) { tabfun2 <- prop.table(table(train[,x],train[,14]) + 1e-6, margin = 1)
sum(prop.table(table(train[,x]))*rowSums(-tabfun2*log2(tabfun2)))}

#entropy_tab(5) #0.9661085 

##### Entropy of the whole train data is ####


s=prop.table(table(train[,14]))

Totalentropy=sum(-s*log2(s))

print(Totalentropy) #0.9752181

#overall entropy of our dependent variable creditstanding for train data  is 0.975218

########## for Consider All the categorical variables  #########
###  below we ran the entropy function on all the categorical variable using the training dataset from B
str(train)
r <- c(2,3,4,5,6,7,8,9,10)

r3 <- NULL
for (x in r) { r2 <- entropy_tab(x)
r3 <- c(r3,r2)

if (x == 10) print(c(min(r3),colnames(train[r[which.min(r3)]])))
}

# O/P of the above loop : "0.726726854088253" "CreditHistory"

## the minimium entropy we have is of CreditHistory with value of 0.726726854088253
## Hence credithistory provide the maximum information gain 
##and should be our rootnode as per non-binary splits on categorical variables

#### calculate information Gain ( This function is taken from the Lab exercise )

CreditStanding_overall <- prop.table(table(CreditData$CreditStanding))
entropy_total <-sum(-CreditStanding_overall*log2(CreditStanding_overall))

### Total intial entropy is 0.9752677

r <- c(2,3,4,5,6,7,8,9,10) ## indexes of categorical columns

r3 <- NULL
info_gain <- NULL
for (x in r) { r2 <- entropy_tab(x)

r3 <- c(r3,r2)
if (x == 10) 
{
  #print(c(min(r3),colnames(train[r[which.min(r3)]])))
  print(c("Entropy minimum",colnames(train[r[which.min(r3)]]), str(round(min(r3),5))))
  print(c("Info Gain maximum", round((entropy_total-min(r3)),5)))  # here the information gain is the total entropy-calculated entropy
  
}}

### output of above code
# "Entropy minimum" "CreditHistory"
# "Info Gain maximum" "0.24854" 

## Hence CreditHistory has to be our root node , So the first split amongst categorical variables will happen on it.

#######################################################################################
                      #Question D For Binary splits
#######################################################################################

## for Binary splits across all the categorical variables we need to process all the variables
## and find which class of which variable gives the best binary split

########### credit history did well having lowest entropy of0.9752677 for non binary splits#######

### add a new column in the train data ###
##### we loop across all the classes of all the categorical variables and decide on the 
### best split root node , i.e. having lowest entropy or  highest Information Gain.

#str(train)

columnval=c(2,3,4,5,6,7,8,9,10) # Indexes of categorical variables
#columnval=c(13)

train$CatGroups="A"  ### capturing the values of the categorical variables for each iteration
train$part="A"       ## capturing the splits of the variable each time while calculating the entropy
for (z in columnval){ # for loop to loop across all the columns
  
  #print (z)
  r <-as.character(unique(train[,c(z)])) # capturing distinct classes of a categorical variable
  entrop_val=NULL
  cat_val=NULL
  
  train$CatGroups=train[,c(z)]
  
  for (i in 1:length(r)){     # looping across each classes in the categorical variable
    for (j in 1:nrow(train)) # looping across each record
    {
      train[j,c(19)] <- ifelse(train[j,c(18)]==r[i],"TRUE","FALSE") # the class under consideration is marked as TRUE and which is not is FALSE
    }
    
    entrop_val <- c(entrop_val,entropy_tab(19))
    cat_val<-c(cat_val,r[i])
    train$CatGroups=train[,c(z)]
  }
  c=paste0(" The lowest entropy for binary split on ",colnames(train[z])," variable is on ",
           cat_val[which.min(entrop_val)]," class with an entropy of ",entrop_val[which.min(entrop_val)])
  print(c) # prints the lowest entropy split for each categorical variable
}


# o/p are as below:

#1)The lowest entropy for binary split on Accounttype variable is on No Acct class with an entropy of 0.943625284201302"
#2)The lowest entropy for binary split on CreditHistory variable is on Critical class with an entropy of 0.818396328923192
#3)The lowest entropy for binary split on LoanReason variable is on Retraining class with an entropy of 0.970210662651624
#4) The lowest entropy for binary split on SavingsAccount variable is on Low class with an entropy of 0.96872317888129
#5)The lowest entropy for binary split on Employment variable is on Short class with an entropy of 0.929425903897917
#6)The lowest entropy for binary split on MaritalStatus variable is on Divorced class with an entropy of 0.970435845679751
#7)The lowest entropy for binary split on Housing variable is on Own class with an entropy of 0.969690102018571
#8)The lowest entropy for binary split on JobType variable is on Unemployed class with an entropy of 0.967675879549833
#9)The lowest entropy for binary split on ForeignNational variable is on No class with an entropy of 0.974868168094076

####### Result ########

## comparing all the above outputs we can see that the first Binary split among categorical variables will be on 
## CreditHistory variable , on Critical class with an entropy of 0.818396328923192


#################################################################################
          #Question E  --- Adding continuous Variable into the entropy calculation
###################################################################################

###### code for Entropy taken from Lab  exercises ########


r <- c(2,3,4,5,6,7,8,9,10,11,12,13) ## capturing indexes of all variables inclusive of continuous variables too
r3 <- NULL
for (x in r) { r2 <- entropy_tab(x)
r3 <- c(r3,r2)
if (x == 13) print(c(min(r3),colnames(train[r[which.min(r3)]])))
}

# O/P :  "0.726726854088253" "CreditHistory"  

### After including the Continuous variables we still get CreditHistory as the Root node as it has
# lowest entropy of 0.726726854088253 amongst all the variables

####### calculating the root node split #######

########### for loop to get the split CreditHistory at which it has the lowest entropy

r <-as.character(unique(train$CreditHistory)) 
store=NULL
Critical=NULL
train$CatGroups=as.character(train$CreditHistory)

for (i in 1:length(r)){
  
  #print(r[i])
  train$CatGroups <- ifelse(train$CatGroups==r[i],"Less","More")
  
  store <- c(store,entropy_tab(18))
  Critical<-c(Critical,r[i])
  train$CatGroups=train$CreditHistory
}
c=paste0("The lowest entropy for binary split on CreditHistory variable is at value " ,Critical[which.min(store)]," with lowest entropy value of ",store[which.min(store)])
print(c)


#O/P : "The lowest entropy for binary split on CreditHistory variable is at value Critical with lowest entropy value of 0.818396328923192"

## So, the binary split for CreditHistory will happen at class Critical as the the entropy value of  0.818396328923192  is lowest.


####################################################################################################################
          #Question F & G  --- To find the 2nd split after the split of Root Node at CreditHistory="Critical"
#####################################################################################################################

#Now create 2 subsets of data one with Credithistory==critical and other with credithistory!="critical" and then run entropy calculation on them like before to ge the best split for each branch

#### separating the traindata sets into two different datasets called train_critical and train_notcritical as per rootnode split.
train_critical=subset(train, train$CreditHistory=="Critical")
str(train_critical)
unique(train_critical)
head(train_critical)
train_notcritical=subset(train, train$CreditHistory!="Critical")
str(train_notcritical)
head(train_notcritical)



##### For the split CreditHistory==Critical we find the variable and the class with lowest entropy like before #########

## for categorical variables
entropy_critical <- function(x) { tabfun2 <- prop.table(table(train_critical[,x],train_critical[,14]) + 1e-6, margin = 1)
sum(prop.table(table(train_critical[,x]))*rowSums(-tabfun2*log2(tabfun2)))}

columnval=c(2,3,4,5,6,7,8,9,10)

train_critical$CatGroups="A"
train_critical$part="A"

for (z in columnval){
  
  #print (z)
  r <-as.character(unique(train_critical[,c(z)]))
  entrop_val=NULL
  cat_val=NULL
  
  train_critical$CatGroups=train_critical[,c(z)]
  
  for (i in 1:length(r)){
    for (j in 1:nrow(train_critical))
    {
      train_critical[j,c(19)] <- ifelse(train_critical[j,c(18)]==r[i],"TRUE","FALSE")
    }
    
    entrop_val <- c(entrop_val,entropy_critical(19))
    cat_val<-c(cat_val,r[i])
    train_critical$CatGroups=train_critical[,c(z)]
  }
  c=paste0(" The lowest entropy for binary split on ",colnames(train_critical[z])," variable is on ",
           cat_val[which.min(entrop_val)]," class with an entropy of ",entrop_val[which.min(entrop_val)])
  print(c)
}

# O/P : 

#[1] " The lowest entropy for binary split on Accounttype variable is on No Acct class with an entropy of 0.0721844857919501"
#[1] " The lowest entropy for binary split on CreditHistory variable is on Critical class with an entropy of 0.10559111941575"
#[1] " The lowest entropy for binary split on LoanReason variable is on Car New class with an entropy of 0.0891659127604915"
#[1] " The lowest entropy for binary split on SavingsAccount variable is on Low class with an entropy of 0.100515588062664"
#[1] " The lowest entropy for binary split on Employment variable is on Short class with an entropy of 0.0891659127604915"
#[1] " The lowest entropy for binary split on MaritalStatus variable is on Single class with an entropy of 0.0921181370883574"
#[1] " The lowest entropy for binary split on Housing variable is on Own class with an entropy of 0.0965346107161751"
#[1] " The lowest entropy for binary split on JobType variable is on Skilled class with an entropy of 0.0973966012241296"
#[1] " The lowest entropy for binary split on ForeignNational variable is on Yes class with an entropy of 0.102572556781382"


### The entropy is very less for all variables, so we can stop at this as its close to a pure node and there is not much info gained by further splitiing

#### now lets check for continuous variables 

columnval=c(11,12,13) # continuous variables 

#columnval=c(11)

train_critical$part="A"
train_critical$CatGroups="A"
for (z in columnval){
  
  #print (z)
  r <-as.character(unique(train_critical[,c(z)]))
  entrop_val=NULL
  cat_val=NULL
  
  train_critical$CatGroups=train_critical[,c(z)]
  
  for (i in 1:length(r)){
    for (j in 1:nrow(train_critical))
    {
      train_critical[j,c(19)] <- ifelse(train_critical[j,c(18)]<=r[i],"TRUE","FALSE")
    }
    
    entrop_val <- c(entrop_val,entropy_critical(19))
    cat_val<-c(cat_val,r[i])
    train_critical$CatGroups=train_critical[,c(z)]
  }
  c=paste0(" The lowest entropy for binary split on ",colnames(train_critical[z])," variable is on value ",
           cat_val[which.min(entrop_val)],"  with an entropy of ",entrop_val[which.min(entrop_val)])
  print(c)
}

#O/P :
  
#[1] " The lowest entropy for binary split on AccountActivemonths variable is on value 19  with an entropy of 0.0926597552719548"
#[1] " The lowest entropy for binary split on CurrentResidenceDuration variable is on value 2  with an entropy of 0.0857022375710951"
#[1] " The lowest entropy for binary split on Age variable is on value 33  with an entropy of 0.0864445348353478"

##### All the entropies (for both categorical and numerical) are less than 0.1 and almost similar.It is as close to pure node value of 0.Hence we can stop splitting this node further.
### Hence this becomes a leaf node.

######################### For Credit History not equal to "Critical" do the same above process ##################

entropy_tab_notcritical <- function(x) { tabfun2 <- prop.table(table(train_notcritical[,x],train_notcritical[,14]) + 1e-6, margin = 1)
sum(prop.table(table(train_notcritical[,x]))*rowSums(-tabfun2*log2(tabfun2)))}### entropy function using train_notcritical dataset

columnval=c(2,3,4,5,6,7,8,9,10)

####### for categorical variables #########
train_notcritical$part="A"
train_notcritical$CatGroups="A"
for (z in columnval){
  
  #print (z)
  r <-as.character(unique(train_notcritical[,c(z)]))
  entrop_val=NULL
  cat_val=NULL
  
  train_notcritical$CatGroups=train_notcritical[,c(z)]
  
  for (i in 1:length(r)){
    for (j in 1:nrow(train_notcritical))
    {
      train_notcritical[j,c(19)] <- ifelse(train_notcritical[j,c(18)]==r[i],"TRUE","FALSE")
    }
    
    entrop_val <- c(entrop_val,entropy_tab_notcritical(19))
    cat_val<-c(cat_val,r[i])
    train_notcritical$CatGroups=train_notcritical[,c(z)]
  }
  c=paste0(" The lowest entropy for binary split on ",colnames(train_notcritical[z])," variable is on ",
           cat_val[which.min(entrop_val)]," class with an entropy of ",entrop_val[which.min(entrop_val)])
  print(c)
}
#O/P:
  
#[1] " The lowest entropy for binary split on Accounttype variable is on No Acct class with an entropy of 0.891360167268852"
#[1] " The lowest entropy for binary split on CreditHistory variable is on All Paid class with an entropy of 0.82765706316168"
#[1] " The lowest entropy for binary split on LoanReason variable is on Furniture class with an entropy of 0.904035448284478"
#[1] " The lowest entropy for binary split on SavingsAccount variable is on MedLow class with an entropy of 0.906709030309217"
#[1] " The lowest entropy for binary split on Employment variable is on Short class with an entropy of 0.867998987167841"
#[1] " The lowest entropy for binary split on MaritalStatus variable is on Single class with an entropy of 0.911866671428446"
#[1] " The lowest entropy for binary split on Housing variable is on Own class with an entropy of 0.909613410176217"
#[1] " The lowest entropy for binary split on JobType variable is on Unemployed class with an entropy of 0.899131550133122"
#[1] " The lowest entropy for binary split on ForeignNational variable is on No class with an entropy of 0.91322692956455"

# Out of all the  Binary categorical splits the CreditHistory variable's "Critical" class with an entropy of 0.759240106775731 has the maximum
# information gain.

###### lets check for continuous variables #########

#### now lets check for continuous variables ######

columnval=c(11,12,13) # continuous variables 

train_notcritical$part="A"
train_notcritical$CatGroups="A"
for (z in columnval){
  
  #print (z)
  r <-as.character(unique(train_notcritical[,c(z)]))
  entrop_val=NULL
  cat_val=NULL
  
  train_notcritical$CatGroups=train_notcritical[,c(z)]
  
  for (i in 1:length(r)){
    for (j in 1:nrow(train_notcritical))
    {
      train_notcritical[j,c(19)] <- ifelse(train_notcritical[j,c(18)]<=r[i],"TRUE","FALSE")
    }
    
    entrop_val <- c(entrop_val,entropy_tab_notcritical(19))
    cat_val<-c(cat_val,r[i])
    train_notcritical$CatGroups=train_notcritical[,c(z)]
  }
  c=paste0(" The lowest entropy for binary split on ",colnames(train_notcritical[z])," variable is on value ",
           cat_val[which.min(entrop_val)],"  with an entropy of ",entrop_val[which.min(entrop_val)])
  print(c)
}

#O/P:

#[1] " The lowest entropy for binary split on AccountActivemonths variable is on value 12  with an entropy of 0.910817049077196"
#[1] " The lowest entropy for binary split on CurrentResidenceDuration variable is on value 2  with an entropy of 0.909351116464091"
#[1] " The lowest entropy for binary split on Age variable is on value 26  with an entropy of 0.820336719660389"


##### amongst continuous variables we  have Age with value < 26 has the minimum entropy 0.820336719660389


### Hence when we combine the categorical and predictor variables we have the Age <=26 with the lowest entropy
## and with the next split.


#################################################################################
         #Question H use tree function to build a decision tree  
###################################################################################
set.seed(644)
par(mfrow=c(1,1))
#plot(1:30)
colnames(train)
#r <- c(2,3,4,5,6,7,8,9,10,15,16,17)
dt.train=train[,c(2,3,4,5,6,7,8,9,10,11,12,13,14)]
#str(dt.train)
#colnames(dt.train)

tree.CreditStanding <- tree ( CreditStanding ~.-CreditStanding , dt.train )

summary (tree.CreditStanding) # misclassification error or the training error rate is 0.1815 i.e 18.15% 

## Training accuracy is 83.6% accuracy

#### plot the above regression Tree

plot ( tree.CreditStanding )
text ( tree.CreditStanding , pretty = 0) 

### Hurray !!!!  the decision tree does the first two splits at CreditHistory: Critical & Age <27,
#which is same as what  we found in entropy.

set.seed(644)
tree.pred <- predict(tree.CreditStanding,test,type="class") # implement the decisin tree using tree () function
CreditStanding.test=test[,14]
table ( tree.pred, CreditStanding.test )### calculating the confusion matrix.

#(48+109)/(48+109+34+10)*100
# 78% accuracy

tree.CreditStanding
###### after pruning #####
# the c.tree() function does the tree pruning
# we are selecting prune.misclass as the metric for cross validation 
cv.CreditStanding_pruned <- cv.tree( tree.CreditStanding , FUN = prune.misclass ) 
cv.CreditStanding_pruned # we can see that tree with 9 terminal nodes has the minimum cross validation error of 132

####### lets use the prune,misclass() functionn to prune the tree to obtain 9 terminal node tree
set.seed(644)
prune.CreditStanding <-prune.misclass(tree.CreditStanding,best=9) ## pruned tree with 2 terminal nodes ###

plot (prune.CreditStanding)
text (prune.CreditStanding , pretty = 0)
#### now to check the performance of the pruned tree with the test data lets use predict function ###
prunedtree.pred <- predict (prune.CreditStanding,test,type = "class")

table ( prunedtree.pred,CreditStanding.test)
(47+110)/(47+110+35+9) #78.1 % accuracy.

# Accuracy for pruned tree doesn't change much and stays around 78%.# Hence the accuracy of decision tree is ~78%


#################################################################################
                          #Question I : use Random Forest to improve the accuracy
###################################################################################


library(randomForest)  # we use the random Forest library to run random forest algorithm on top of the data.

set.seed(644)

rf.Credit <- randomForest( CreditStanding ~ .- CreditStanding , data = dt.train , importance = TRUE ,do.trace=TRUE)
rf.Credit # 81.3 % accuracy in training test
(184+309)/(184+309+50+63) 

importance(rf.Credit)
varImpPlot(rf.Credit) ## to plot the importance of a variable

####### lets try running using 1000 trees
set.seed(644)

rf.Credit_1000 <- randomForest( CreditStanding ~ .- CreditStanding , data = dt.train ,ntree = 1000, importance = TRUE ,do.trace=TRUE)
rf.Credit_1000 #  there is not much improvement in the accuracy by increasing the number of subtrees in random forest

############ lets try changing the value of mtry

rf.Credit_mtry <- randomForest( CreditStanding ~ .- CreditStanding , data = dt.train ,mtry=9,ntree = 1000, importance = TRUE ,do.trace=TRUE)
rf.Credit_mtry #  there is not much improvement in the accuracy by increasing the number of subtrees in random forest

#(185+307)/(185+307+62+52)
### making the number of predictor to choose from at each split (mtry) to 12  Does not improve the accuracy.

########## predict on test ########

tree.pred_rf2 <- predict(rf.Credit, test, type = "class") # note using type = "prob"

tree.pred_rf2

table(tree.pred_rf2, test$CreditStanding)
(59+105)/(59+105+14+23) #81.5%


### Comparing at the test accuracy of random forest and decision tree we can say that Random forest
# is a Superior model to decision tree with high accuracy rate / lower misclassification value.


###################################################################################################
         #Question J : Due to GDPR exclude Age,Maritalstatus,Foreign from Model and repeat H & I
###################################################################################################

set.seed(644)

str(train)
tree.CreditStanding_gdpr <- tree ( CreditStanding ~.-Age-ForeignNational-MaritalStatus , dt.train )
summary (tree.CreditStanding_gdpr) # misclassification error is 0.2046 i.e. 20.46  %
plot (tree.CreditStanding_gdpr)
text (tree.CreditStanding_gdpr , pretty = 0)
tree.CreditStanding_gdpr # misclassification error rare=21.95%
### so the miss-classification training error rate increase as compared to the full model 



tree.pred_gdpr <- predict(tree.CreditStanding_gdpr,test,type="class")
CreditStanding.test=test[,14]
table (tree.pred_gdpr, CreditStanding.test )

(106+52)/(106+52+13+30) # 78.6

############ random forest following GDPR ##########
set.seed(644)

rf.Credit_gdpr <- randomForest( CreditStanding ~ .- CreditStanding -Age-ForeignNational-MaritalStatus , 
                                data = dt.train , importance = TRUE ,do.trace=TRUE)


testpred_gdpr <- predict(rf.Credit_gdpr, test, type = "class") # note using type = "prob"

varImpPlot(rf.Credit_gdpr)
table(testpred_gdpr, test$CreditStanding)

(98+58)/(58+98+24+21) #77.6%

### The accuracy of random forest have decreased from 81.5% for complete model to 77.6%
### Hence the excluded variables plays an important part producing accuracy for RandomForest model.


###################################################################################################
            #Question K : To Find Suspicious Pattern from the Data
###################################################################################################

#### convert ID to ordered date with any start value , here we have considered 2022-01-01 as the startdate
CreditData$Dates <- as.Date('2022-01-01') + CreditData$ID ## converting ID into date just to create a time series


# creating a column creditstandingflag and every good loan will have 1 and bad load will have -1
## This is done so that any suspicious pattern can appear magnified in the graph

CreditData$creditstandingflag[CreditData$CreditStanding=="Good"]=1
CreditData$creditstandingflag[CreditData$CreditStanding=="Bad"]=-1

# create another column cum_flag which will store the cumulative values of previous rows , again
# the goal is check if there is any weird pattern in the time series data.

CreditData[,"cum_flag"] <- cumsum(CreditData$creditstandingflag) 


### Now plot the graph were we plot Dates against  cum_flag column

x=ggplot(data=CreditData,aes(x=Dates, y=cum_flag,label=rownames(CreditData)))+geom_line(color="#69b3a2")
x+xlab("PseudoDates/ID")+ylab("Cumulative swiches between Good & Bad")+ggtitle("CreditStanding prediction trend as time/ID progress")


### We can see that records with ID from ID 477 till 533 we have large influx of good loans i.e for those 56 
# records we have 54 Good loans and only 2 bad loans.Looks like something suspicious has 
#happened , Looks like a flaw caused by Human involvement.
#because its too good of a scenario to appear true.

##############################################  END ##############################################################





