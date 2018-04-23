# Meta data - plot
# Meta data - ideas

# Stepwise regression

library(caret) # Split data
library(ROCR) # AUC
library(dplyr) # %>% 

library(rpart) #CART 
library(rpart.plot) # CART
library(randomForest) # CART


rm(list = ls())
gc()
# Loading Data
# load("H1B_Raw.RData") # raw data stored as h1b_raw
load("H1B.RData") # "cleaned" data stored as h1b

# Drop certain columns 
h1b.edit = h1b %>% 
  
  select(-CASE_NUMBER,
         -CASE_STATUS, # leads to algorithm did not converge 
         -NAICS_CODE, # too specific, too many!
         
         # Factors - sparcified to 0.5%
         #-SOC_CODE,
         #-EMPLOYER_CITY,
         #-WORKSITE_CITY,
         #-EMPLOYER_STATE,
         #-WORKSITE_STATE,
         #-EMPLOYER_POSTAL_CODE,
         #-WORKSITE_POSTAL_CODE,              
         
         # NAs 6
         #-FULL_TIME_POSITION, 
         
         -EMPLOYER_COUNTRY, # Once NAs are omitted, this only has 1 factor level            
         #-AGENT_REPRESENTING_EMPLOYER,            
         #-H1B_DEPENDENT, 
         #-WILLFUL_VIOLATOR,
         #-LABOR_CON_AGREE,
         
         # Need to do things first 3+2+2=7 
         -EMPLOYER_NAME,
         -AGENT_ATTORNEY_NAME,
         -JOB_TITLE
         #-PW_STD,
         #-WAGE_STD
         #-CASE_SUBMITTED
         #-DECISION_DATE
  ) 

# Reverse result; Predict negative
h1b.edit$RESULT = as.factor((as.numeric(as.character(h1b$RESULT))-1)^2)

# Sparcify some factors
lf1 = names(which(prop.table(table(h1b.edit$SOC_CODE)) < 0.005))
levels(h1b.edit$SOC_CODE)[levels(h1b.edit$SOC_CODE) %in% lf1] <- "Other"
lf2 = names(which(prop.table(table(h1b.edit$EMPLOYER_CITY)) < 0.005))
levels(h1b.edit$EMPLOYER_CITY)[levels(h1b.edit$EMPLOYER_CITY) %in% lf2] <- "Other"
lf3 = names(which(prop.table(table(h1b.edit$WORKSITE_CITY)) < 0.005))
levels(h1b.edit$WORKSITE_CITY)[levels(h1b.edit$WORKSITE_CITY) %in% lf3] <- "Other"
lf4 = names(which(prop.table(table(h1b.edit$EMPLOYER_STATE)) < 0.005))
levels(h1b.edit$EMPLOYER_STATE)[levels(h1b.edit$EMPLOYER_STATE) %in% lf4] <- "Other"
lf5 = names(which(prop.table(table(h1b.edit$WORKSITE_STATE)) < 0.005))
levels(h1b.edit$WORKSITE_STATE)[levels(h1b.edit$WORKSITE_STATE) %in% lf5] <- "Other"
lf6 = names(which(prop.table(table(h1b.edit$EMPLOYER_POSTAL_CODE)) < 0.005))
levels(h1b.edit$EMPLOYER_POSTAL_CODE)[levels(h1b.edit$EMPLOYER_POSTAL_CODE) %in% lf6] <- "Other"
lf7 = names(which(prop.table(table(h1b.edit$WORKSITE_POSTAL_CODE)) < 0.005))
levels(h1b.edit$WORKSITE_POSTAL_CODE)[levels(h1b.edit$WORKSITE_POSTAL_CODE) %in% lf7] <- "Other"

# Dates
h1b.edit$PERIOD =  h1b.edit$DECISION_DATE - h1b.edit$CASE_SUBMITTED
h1b.edit$WAGE_DIFF = h1b.edit$WAGE_STD - h1b.edit$PW_STD

# Round 2 - drop more  columns 
h1b.edit = h1b.edit %>% 
  
  select(-PW_STD,
         -WAGE_STD,
         -CASE_SUBMITTED,
         -DECISION_DATE
  ) 

# Deal with factors
h1b.edit = na.omit(h1b.edit) # drop missing values

# Split into training and test
set.seed(16)
h1b.split = createDataPartition(h1b.edit$RESULT, p = 0.1, list = FALSE) # Uses only 10% of database
h1b.split2 = createDataPartition(h1b.split, p = 0.7, list = FALSE) # Split into test and training
h1b.tmp = h1b.edit[h1b.split,]
h1b.train = h1b.tmp[h1b.split2,]
h1b.test = h1b.tmp[-h1b.split2,]

# Model 1: Logistical Regression
h1b.LR1 = glm(RESULT ~ ., data=h1b.train, family=binomial)
?glm

summary(h1b.LR1)

h1b.LR1.p = predict(h1b.LR1, newdata=h1b.test, type="response")
h1b.LR1.p.thresh  = (h1b.LR1.p > 0.5) #arbitrary
h1b.LR1.cm = table(h1b.test$RESULT, h1b.LR1.p.thresh)

h1b.LR1.rocr.p = prediction(h1b.LR1.p, h1b.test$RESULT)
plot(performance(h1b.LR1.rocr.p, "tpr", "fpr")) # Receiver Operating Characteristic
abline(0,1) # blind model
as.numeric(performance(h1b.LR1.rocr.p, "auc")@y.values) # [TBC number in base case]






#########################################
# Can be safely deleted/ignored; useful infor debugging only
#########################################
table(h1b$CASE_NUMBER) # OK, not used
table(h1b$CASE_STATUS) # OK
table(h1b$CASE_SUBMITTED) # OK
table(h1b$DECISION_DATE) # OK
table(h1b$EMPLOYER_NAME) # OK
table(h1b$EMPLOYER_CITY) # OK
table(h1b$EMPLOYER_STATE) # OK, 50+7 states
table(h1b$EMPLOYER_POSTAL_CODE) # Zip codes  OK
summary(h1b$EMPLOYER_COUNTRY) # Contains NAs - DECIDE HOW TO TREAT 
summary(h1b$AGENT_REPRESENTING_EMPLOYER) # Contains NAs - DECIDE HOW TO TREAT 
table(h1b$AGENT_ATTORNEY_NAME) # OK 
table(h1b$JOB_TITLE) # OK
summary(h1b$SOC_CODE) # OK
table(h1b$NAICS_CODE) # OK
table(h1b$TOTAL_WORKERS) # OK
table(h1b$NEW_EMPLOYMENT) # OK
table(h1b$CONTINUED_EMPLOYMENT) # OK
table(h1b$CHANGE_PREVIOUS_EMPLOYMENT) # OK
table(h1b$NEW_CONCURRENT_EMPLOYMENT) # OK
table(h1b$CHANGE_EMPLOYER) # OK
table(h1b$AMENDED_PETITION) # OK
summary(h1b$EMPLOYER_COUNTRY) # Contains NAs - DECIDE HOW TO TREAT 
summary(h1b$AGENT_REPRESENTING_EMPLOYER) # Contains NAs - DECIDE HOW TO TREAT 
summary(h1b$FULL_TIME_POSITION) # Contains NAs - DECIDE HOW TO TREAT 
summary(h1b$H1B_DEPENDENT) # Contains NAs - DECIDE HOW TO TREAT 
summary(h1b$WILLFUL_VIOLATOR) # Contains NAs - DECIDE HOW TO TREAT 
summary(h1b$LABOR_CON_AGREE) # Contains NAs - DECIDE HOW TO TREAT 
table(h1b$WORKSITE_CITY) # Text OK
table(h1b$WORKSITE_STATE) # OK, 50+7 states
table(h1b$WORKSITE_POSTAL_CODE) # ZIP codes OK
min(h1b$PW_STD) # OK
min(h1b$WAGE_STD) # OK
table(h1b.edit$RESULT) # OK