
# Bank Marketing 

## 1.Introduction:

# This project applies machine learning technique that go beyond standard linear regression. I had  the prospect to use a  publicly available  dataset  to resolve  the  trouble  of  my alternative. I sifted through the datasets available on Kaggle and chose a finance/bank related dataset. I work at a bank so I was geared in the direction of selecting a topic that’s applicable to the banking industry.
# 
# The aim of the project is to answer the following question::
# What type of behaviors do possible customers show that result in them extra likely to subscribe to a term deposit?
# 
# The  industry    difficulty is to  devise  a  objective marketing  strategy for the bank base on the behavioral data collected. The dataset is included in one of the obedience files and can be downloaded from Kaggle (https://www.kaggle.com/henriqueyamahata/bank-marketing).
# 
# The Dataset: 
# It contain 41,188 customer data on direct advertising campaign (phone calls) of a Portuguese banking organization. 
# 
# It has the following variables:
# Client: age, job, marital, education, default status, housing, and loan
# Campaign: last contact type, last contact month of year, last contact day of the week, and last contact duration
# Others: number of contacts performed in current campaign, number of days that passed by after the client was last contacted, number of contacts performed before this campaign, outcome of previous campaign, and whether a client has subscribed a term deposit
# 
# Key Steps Performed:
# I first use Data categorization to look at the set connected with direct marketing campaign of a Portuguese banking organization. The objective of the arrangement is to forecast if the client will donate to to a Term Deposit. Data Classification is the use of machine learning technique to put in order datasets into related sub-populations, not preceding specified in the dataset. This can expose hidden individuality inside data, and recognize hidden categories that new data belong  in. The rest of the key steps that were performed used the data science techniques of investigative Data Analysis in K-Nearest Neighbor.



  
  ## 2.Data Analysis:
  
  
  ### 2.1.Exploratory Analysis
  

rm(list = ls())
options(warn=-1)

if(!require(readr)) install.packages("readr", repos = "")
if(!require(tidyverse)) install.packages("tidyverse", repos = "")
if(!require(GGally)) install.packages("GGally", repos = "")
if(!require(glmnet)) install.packages("glmnet", repos = "")
if(!require(Matrix)) install.packages("Matrix", repos = "")
if(!require(DataExplorer)) install.packages("DataExplorer", repos = "")
if(!require(corrplot)) install.packages("corrplot", repos = "")
if(!require(caret)) install.packages("caret", repos = "")
if(!require(randomForest)) install.packages("randomForest", repos = "")
if(!require(class)) install.packages("class", repos = "")
if(!require(gmodels)) install.packages("gmodels", repos = "")
if(!require(dplyr)) install.packages("dplyr", repos = "")
if(!require(psych)) install.packages("psych", repos = "")


library(readr)
library(tidyverse)
library(GGally)
library(glmnet)
library(Matrix)
library(ggplot2)
library(DataExplorer)
library(corrplot)
library(caret)
library(randomForest)
library(class)
library(gmodels)
library(dplyr)
library(psych)


set.seed(1)


#Loading the dataset:

#setwd("C:/Users/1012233/Downloads/20191103 - R Studio Kaggle project")
#data.df <- read.csv("bank-additional-full.csv", header=TRUE, sep=";")

 
  
  ##Viewing the column names of the dataset:##

names(data.df)

  ##Column details of the dataset:##
str(data.df)

  ##Summary analysis of the dataset:##

summary(data.df)



  ### 2.2.Data Preparation
  
  ##We check if there are any missing values that exists:##

sum(is.na(data.df))
#There are no missing values in our dataset.

  
#In the above exploratory analysis, we observed that there are many variables with class=int; hence, we convert them into numeric class

##Converting quantitative values to numeric class:##

data.df$age <- as.numeric(data.df$age)
data.df$duration <- as.numeric(data.df$duration)
data.df$campaign <- as.numeric(data.df$campaign)
data.df$pdays <- as.numeric(data.df$pdays)
data.df$previous <- as.numeric(data.df$previous)



  
  
  ##Ordering the levels of month:##

data.df$month<- factor(data.df$month, ordered = TRUE, levels = c("mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec"))




  
#Since the target variable is a categorical variables with 2 possible values: yes, no; we transform it into a numerical denotation: 1,0 respectively

##Transforming the target variable as Yes=1 and No=0:##

table(data.df$y)



data.df <- data.df %>%
  mutate(y = ifelse(y=="yes", 1, 0))

data.df$y <- as.factor(data.df$y)
table(data.df$y)



  
  ### 2.3.Descriptive Analysis
  
  ##Let us look at the histogram of the input variables:##
  

plot_histogram(data.df[,-21],ggtheme = theme_gray(base_size = 10, base_family = "serif"))



  

mytable <- table(data.df$marital, data.df$y)
tab <- as.data.frame(prop.table(mytable, 2))
colnames(tab) <-  c("marital", "y", "perc")
ggplot(data = tab, aes(x = marital, y = perc, fill = y)) + 
  geom_bar(stat = 'identity', position = 'dodge', alpha = 2/3) + 
  xlab("Marital")+ylab("Percent")


##With deference to Marital Status there is not an experiential large difference in the quantity of people subscribe to term deposit and people without term deposits.##
  
  

mytable <- table(data.df$education, data.df$y)
tab <- as.data.frame(prop.table(mytable, 2))
colnames(tab) <-  c("education", "y", "perc")
ggplot(data = tab, aes(x = education, y = perc, fill = y)) + 
  geom_bar(stat = 'identity', position = 'dodge', alpha = 2/3) +
  theme(axis.text.x=element_text(angle=90,hjust=1)) +
  xlab("Education")+ylab("Percent")

##We can observe that customers who sign up for bank deposits, proportionally, have achieve a higher level of education, than those who didn’t sign up##



mytable <- table(data.df$month, data.df$y)
tab <- as.data.frame(prop.table(mytable, 2))
colnames(tab) <-  c("month", "y", "perc")
ggplot(data = tab, aes(x = month, y = perc, fill = y)) + 
geom_bar(stat = 'identity', position = 'dodge', alpha = 2/3) +
xlab("Month")+ylab("Percent")

##The month of May is when the highest number of calls were placed for marketing deposit. And the following months of April, September, and October is the time when a higher proportion of people subscribed for term deposits.##




mytable <- table(data.df$job, data.df$y)
tab <- as.data.frame(prop.table(mytable, 2))
colnames(tab) <-  c("job", "y", "perc")
ggplot(data = tab, aes(x = job, y = perc, fill = y)) + 
geom_bar(stat = 'identity', position = 'dodge', alpha = 2/3) +
theme(axis.text.x=element_text(angle=90,hjust=1)) +
xlab("Job")+ylab("Percent")

##We see there are higher size for customers sign up for the term deposits who have the job of admin, retired, and students.##

mytable <- table(data.df$default, data.df$y)
tab <- as.data.frame(prop.table(mytable, 2))
colnames(tab) <-  c("default", "y", "perc")
ggplot(data = tab, aes(x = default, y = perc, fill = y)) + 
geom_bar(stat = 'identity', position = 'dodge', alpha = 2/3) +
xlab("Default")+ylab("Percent")

##The data show that people who aren’t in default are a higher proportion of people who have subscribed for bank deposits##
  
  
  

mytable <- table(data.df$housing, data.df$y)
tab <- as.data.frame(prop.table(mytable, 2))
colnames(tab) <-  c("housing", "y", "perc")
ggplot(data = tab, aes(x = housing, y = perc, fill = y)) + 
  geom_bar(stat = 'identity', position = 'dodge', alpha = 2/3) +
  xlab("Housing")+ylab("Percent")

##We see that a high amount of people who have subscribe for bank deposit are home owner versus ones that don’t hold their own houses.##





mytable <- table(data.df$loan, data.df$y)
tab <- as.data.frame(prop.table(mytable, 2))
colnames(tab) <-  c("loan", "y", "perc")
ggplot(data = tab, aes(x = loan, y = perc, fill = y)) + 
geom_bar(stat = 'identity', position = 'dodge', alpha = 2/3) +
xlab("Loan")+ylab("Percent")

##We see the section of people who have subscribe and not subscribe to a term deposit is the identical for categories of the Loan.##





mytable <- table(data.df$contact, data.df$y)
tab <- as.data.frame(prop.table(mytable, 2))
colnames(tab) <-  c("contact", "y", "perc")
ggplot(data = tab, aes(x = contact, y = perc, fill = y)) + 
geom_bar(stat = 'identity', position = 'dodge', alpha = 2/3) +
xlab("Contact")+ylab("Percent")

##Customers who have cell phones, and therefore a more direct way of communicate, signed up for period deposits more than those who just had a landline telephone##





mytable <- table(data.df$day_of_week, data.df$y)
tab <- as.data.frame(prop.table(mytable, 2))
colnames(tab) <-  c("day_of_week", "y", "perc")
ggplot(data = tab, aes(x = day_of_week, y = perc, fill = y)) + 
geom_bar(stat = 'identity', position = 'dodge', alpha = 2/3) +
xlab("Day_of_week")+ylab("Percent")

##campaign that were perform midweek, on Tuesdays, Wednesdays, and Thursdays had a somewhat higher section of people who subscribe for bank deposit.##



mytable <- table(data.df$poutcome, data.df$y)
tab <- as.data.frame(prop.table(mytable, 2))
colnames(tab) <-  c("poutcome", "y", "perc")
ggplot(data = tab, aes(x = poutcome, y = perc, fill = y)) + 
geom_bar(stat = 'identity', position = 'dodge', alpha = 2/3) +
xlab("Outcome of previous marketing campaign")+ylab("Percent")

##Potential customers who successfully connected and responded in previous campaigns had a higher proportion of signing up for the term deposit.##






ggplot(data.df, aes(factor(y), duration)) + geom_boxplot(aes(fill = factor(y)))

##The longer the phone conversation the greater the conversion rate is for the potential customer to sign up for the term deposit. There are higher median and quartile ranges.##



ggplot(data.df, aes(factor(y), age)) + geom_boxplot(aes(fill = factor(y)))

##The age range for successful conversion has a slightly lower median, but higher quartile ranges.##












df_cor <- select_if(data.df, is.numeric) %>% cor()
corrplot(df_cor, method = "number")

##We see our target variable has a high positive correlation with duration and if the customer was involved and connected in a previous campaign, while there's negative correlation with Nr.employed (number of employees), pdays (number of days from last contact), Euribor3m (Euribor 3 month rate) and emp.var.rate (employee variation rate).##
  
  ##*
  
  
  
  
  ## 3.Data Modeling and Results:
  
  
  ### 3.1.Data Preparation
  
  
  ##Includes Data Modeling and performance missing values  for  period  were clean out because if duration=0 then y=“no” (no call was made). Thus, it doesn’t make logic to have 0 second duration. I  also filtered out education uneducated, and default yes since they only have 1 surveillance each. We can’t predict these circumstances if they happen to be in the examination data but not the train data##
  

data.df <- data.df %>%
  filter(duration != 0, education != "illiterate", default != "yes") %>%
  mutate(y = ifelse(y==1, 1, 0))


##Split the data into training and test datasets:##
  

set.seed(123)
trainIndex <- createDataPartition(data.df$y,
                                  p = 0.8, # training contains 80% of data
                                  list = FALSE)
dfTrain <- data.df[ trainIndex,]
dfTest  <- data.df[-trainIndex,]



dim(dfTrain)



dim(dfTest)

#The code and output above show that the train Data dataset has 8929 rows and 17 columns and the test Data dataset gas 2233 rows and 17 columns. The number of columns remains the same because the dataset was split up and down.


  
    
  ## 3.Data Modeling using KNN
  ##We will make a copy of our data set so that we can prepare it  for our K-NN classification. 


data_knn <- data.df

str(data_knn)



# Because K-NN algorithm involve influential distances between data points, we have to use numeric variables only. This is relevant only to independent variables. The objective variable for k-NN categorization should remain a factor  changeable. First, we scale the data  just  in case  our features are on different metrics. For example, if we had “duration” as a variable, it would be on a much better scale than “age”, which might be problematic given the k-NN relies on distance. Note that we are using the ‘scale’ function here, which income we are scaling to a z- score metric
# 
# We see that the variables “age”, “duration”, “campaign”, “pdays”, “previous”, “emp.var.rate”, “cons.price.idx”, “cons.conf.idx”, “euribor3m” and “nr.employed” are interger variables, which means they can be scaled.



data_knn[, c("age", "duration", "campaign", "pdays", "previous", "emp.var.rate", "cons.price.idx", "cons.conf.idx", "euribor3m","nr.employed")] <- scale(data_knn[, c("age", "duration", "campaign", "pdays", "previous", "emp.var.rate", "cons.price.idx", "cons.conf.idx", "euribor3m","nr.employed")])

head(data_knn)



str(data_knn)


##We can observe that the variables “job”, “marital”, “education”, “default”, “housing”, “loan”, “contact”, “month”, “day_of_week” and “poutcome” are thing variables that have two or more levels.##
  
  
  ## Then dummy code variables that have two levels, but are not numeric.##

data_knn$contact <- dummy.code(data_knn$contact)



  ##Next we dummy code variables that have three or more levels.##

job <- as.data.frame(dummy.code(data_knn$job))
marital <- as.data.frame(dummy.code(data_knn$marital))
education <- as.data.frame(dummy.code(data_knn$education))
default <- as.data.frame(dummy.code(data_knn$default))
housing <- as.data.frame(dummy.code(data_knn$housing))
loan <- as.data.frame(dummy.code(data_knn$loan))
month <- as.data.frame(dummy.code(data_knn$month))
day_of_week <- as.data.frame(dummy.code(data_knn$day_of_week))
poutcome <- as.data.frame(dummy.code(data_knn$poutcome))



  ##Rename "unknown" columns, so we don't have duplicate columns later).##

job <- rename(job, unknown_job = unknown)
marital <- rename(marital, unknown_marital = unknown)
education <- rename(education , unknown_education  = unknown)
default <- rename(default , unknown_default  = unknown)
housing <- rename(housing , unknown_housing  = unknown)
loan <- rename(loan , unknown_loan  = unknown)

default <- rename(default , yes_default  = yes)
default <- rename(default , no_default  = no)

housing <- rename(housing , yes_housing  = yes)
housing <- rename(housing , no_housing  = no)

loan <- rename(loan , yes_loan  = yes)
loan <- rename(loan , no_loan  = no)



  ##Combine new dummy variables with original data set.##

data_knn <- cbind(data_knn, job, marital, education, default, housing, loan, month, day_of_week,poutcome)




str(data_knn)



##Remove original variables that had to be dummy coded.##

data_knn <- data_knn %>% select(-one_of(c("job", "marital", "education", "default", "housing", "loan", "month", "day_of_week", "poutcome")))

head(data_knn)

#We are now ready for k-NN classification. We split the data into training and test sets. We partition 80% of the data into the training set and the remaining 20% into the test set.

##Splitting the dataset into Test and Train:##

set.seed(1234) # set the seed to make the partition reproducible

# 80% of the sample size
sample_size <- floor(0.8 * nrow(data_knn))


train_index <- sample(seq_len(nrow(data_knn)), size = sample_size)

# put outcome in its own object
knn_outcome <- data_knn %>% select(y)

# remove original variable from the data set
data_knn <- data_knn %>% select(-y)



# creating test and training sets that contain all of the predictors
knn_data_train <- data_knn[train_index, ]
knn_data_test <- data_knn[-train_index, ]

# Split outcome variable into training and test sets using the same partition as above.
knn_outcome_train <- knn_outcome[train_index, ]
knn_outcome_test <- knn_outcome[-train_index, ]




##Using 'class' package, we run k-NN classification on our data. We have to decide on the number of neighbors (k).This is an iterative exercise as we need to keep changing the value of k to dtermine the optimum performance. In our case, we started with k=10 till k=20, and finally got an optimum performance at k=17.


model_knn <- knn(train = knn_data_train, test = knn_data_test, cl = knn_outcome_train, k=17)


##Model evaluation:##


# put "knn_outcome_test" in a data frame
knn_outcome_test <- data.frame(knn_outcome_test)

# merge "model_knn" and "knn_outcome_test" 
knn_comparison_df <- data.frame(model_knn, knn_outcome_test)

# specify column names for "knn_comparison_df"
names(knn_comparison_df) <- c("KNN_Predicted_y", "KNN_Observed_y")

knn_comparison_df$KNN_Predicted_y <- as.factor(knn_comparison_df$KNN_Predicted_y)
knn_comparison_df$KNN_Observed_y <- as.factor(knn_comparison_df$KNN_Observed_y)

# inspect "knn_comparison_df" 
head(knn_comparison_df)



# Next, we compare our predicted values of deposit to our actual values. The confusion matrix gives an indication of how well our model predicted the actual values.
# The confusion matrix output also shows overall model statistics and statistics by class


confusionMatrix(knn_comparison_df$KNN_Observed_y,knn_comparison_df$KNN_Predicted_y)



# The K-nn test data consisted of 8238 observations. Out of which 7128 cases have been accurately predicted (TN->True Negatives) as negative class (0) which constitutes 87%. Also, 367 out of 8238 observations were accurately predicted (TP-> True Positives) as positive class (1) which constitutes 4%. Thus a total of 367 out of 8238 predictions where TP i.e, True Positive in nature.
# 
# There were 544 cases of False Positives (FP) meaning 544 cases out of 8238 were actually negative but got predicted as positive.
# 
# There were 199 cases of False Negatives (FN) meaning 199 cases were actually positive in nature but got predicted as negative.
# 
# Accuracy of the model is the correctly classified positive and negative cases divided by all ther cases.The total accuracy of the model is 91.13%, which means the model prediction is very accurate.
# 
#
  
  
  
  
  
  ## 4.Conclusion:
  
  ### Model Comparison:
#   K Nearest Neighbor are generating high accuracy when trained with the bank marketing dataset.
# 
# The parameter comparision for both the model is:
#   
#   Parameter	K-NN  Model	
 		| 	|
#  Accuracy	|	91.13%	|
 		| 	|
#  Sensitivity	|	93.03%	|
 		| 	|
#  Specificity	|	65.68%	|
 		| 	|
#  Pos Pred Value	|	97.31%	|
 		| 	|
#  Neg Pred Value	|	41.38%	|
 		| 	|
 

### Analysis Summary:
# The key insight derived from the in general analysis are:
# -People who aren’t in default are a higher amount of people who have subscribe for bank deposits.
# -Higher quantity of people who have subscribe for bank deposit are home owners versus ones that don’t own their own houses. 
# -The proportion of people who have subscribed and not subscribed to a term deposit is the same for categories of the Loan.
# -With respect to Marital Status, there is not an experiential large difference in the proportion of people subscribed to term deposits and people without term deposits.
# -The proportion of people who have subscribed and not subscribed to a term deposit is the same for categories of the Loan.
# -Customers who sign up for bank deposits, proportionally, have achieved a higher level of education, than those who didn’t sign up.
# -The months of April, September, October, and December is the time when a higher proportion of people subscribed for term deposits
# -There are higher proportions for customers signing up for the term deposits who have the jobs of admin, retired, and students
# -Customers who have cell phones, and therefore a more direct way of communicating, signed up for term deposits more than those who only had a landline telephone
# -The age range for successful conversion has a slightly lower median, but higher quartile ranges
#-Campaigns that were performed midweek, on Tuesdays, Wednesdays, and Thursdays had a slightly higher proportion of people who subscribed for bank deposit.
#-Potential customers who successfully connected and responded in previous campaigns had a higher proportion of signing up for the term deposit.
#-The longer the phone conversation the greater the conversion rate is for the potential customer to sign up for the term deposit. There are higher median and quartile ranges.
#
# ###Target Market Strategy
#The business problem defined in the introduction is to devise a target marketing strategy for the bank based on the behavioral data collected. We discovered the kinds of observations and behaviors of potential customers that result in them more likely to subscribe to a term deposit
#Using the above insights, the bank should devise a target marketing strategy that is customized towards potential customers with those who already have an existing account with the bank and have higher education. Those customers who generally are either employed in admin related jobs, or are students, or retired are those the bank can further pull. 
#Those customers who are easily accessible with a mobile number will be the ones to answer the call, the key is to have an engaging and personable conversation with the customer and establish a relationship where they feel comfortable signing up for a term deposit with the bank. 
#Those customers who were part of the previous campaign should be contacted by the bank again because we saw following up and having a continued dialogue and relationship results in a higher number of those who sign up. Also, in order to improve the probability of success, the campaigns should be launched in the last third of the calendar year when people are thinking of saving for the future and preparing for year-end taxes.

# ### Future Work:

# Data analytics is usually used to examine and work with big data such as the one we provided in the project here. It eases the cross-examination of the  data and the methods of finding relationships  within the data, so it becomes easier. There  is  a lo t of  things that we can do in future upon the existing model such as determining the right day of the week and time for each of the target audience or build custom models for individual clusters to further improve the prediction rate and reduce the error rate.
#The work that has been done on this modeling and analysis is a great start for the bank to acquire more customers in an efficient way. There can always be improvements and tweaking as more data comes in, as well as exploring niches and clusters within the data. What a great start to enhancing the target market strategy!##*
  