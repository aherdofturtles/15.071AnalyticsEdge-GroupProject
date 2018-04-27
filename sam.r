# Month of application? Removes ex-post analysis

library(caret) # Split data
library(ROCR) # AUC
library(dplyr) # %>% 
library(rpart) #CART 
library(rpart.plot) # CART
library(randomForest) # CART
library(ggplot2) # Plots
library(tm) # Text Mining

####################
# Section 1 - Data Wrangling
####################

rm(list = ls())

# Loading Data
# load("H1B_Raw.RData") # raw data stored as h1b_raw
load("H1B.RData") # "cleaned" data stored as h1b

h1b.edit = select(h1b,
                  -CASE_NUMBER, # Description
                  -CASE_STATUS, # Withdrawn is ex-post to dependent variable
                  -NAICS_CODE, # Not sparse enough
                  # -JOB_TITLE,
                  # -EMPLOYER_NAME, 
                  # -AGENT_ATTORNEY_NAME,
                  -EMPLOYER_COUNTRY # too many NAs
                  )

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

# Meta Data
regex = function(c,t) {return(ifelse(grepl(t, c, perl=T), 1, 0))}

# Meta Data - Job title
job_corpus = Corpus(VectorSource(h1b.edit$JOB_TITLE))
job_corpus.dtm = DocumentTermMatrix(job_corpus, control = list(tolower = F)) # 603,833 x 13631

job_corpus.pf = colSums(as.matrix(removeSparseTerms(job_corpus.dtm, 0.99)))
job_corpus.pf2 = job_corpus.pf[order(job_corpus.pf, decreasing=T)]

h1b.edit$JOB_TITLE_POPULAR = regex(h1b$JOB_TITLE,paste(names(job_corpus.pf2)[1:5], collapse="|")) #1-5 top names
h1b.edit$JOB_TITLE_SENIOR = regex(h1b$JOB_TITLE,"CHIEF|SENIOR|HEAD|DIRECTOR|PRESIDENT")
h1b.edit$JOB_TITLE_JUNIOR = regex(h1b$JOB_TITLE,"ASSISTANT|JUNIOR|AUDITOR|ASSOCIATE|ANALYST")

h1b.edit$JOB_TITLE_JUNIOR_ASSISTANT = regex(h1b$JOB_TITLE,"ASSISTANT")
h1b.edit$JOB_TITLE_JUNIOR_JUNIOR = regex(h1b$JOB_TITLE,"JUNIOR")
h1b.edit$JOB_TITLE_JUNIOR_AUDITOR = regex(h1b$JOB_TITLE,"AUDITOR")
h1b.edit$JOB_TITLE_JUNIOR_ASSOCIATE = regex(h1b$JOB_TITLE,"ASSOCIATE")
h1b.edit$JOB_TITLE_JUNIOR_ANALYST = regex(h1b$JOB_TITLE,"ANALYST") # or trainee

# h1b.edit$JOB_TITLE_POPULAR = regex(h1b$JOB_TITLE,"(?=.*DATA)(?=.*MACHINE)")
# count(filter(h1b, grepl("CHIEF",JOB_TITLE)))

# Meta Data - Employer
employer_corpus = Corpus(VectorSource(h1b.edit$EMPLOYER_NAME))
employer_corpus.dtm = DocumentTermMatrix(employer_corpus, control = list(tolower = F))

employer_corpus.pf = colSums(as.matrix(removeSparseTerms(employer_corpus.dtm, 0.99)))
employer_corpus.pf2 = employer_corpus.pf[order(employer_corpus.pf, decreasing=T)]

h1b.edit$EMPLOYER_NAME_POPULAR = regex(h1b$EMPLOYER_NAME,paste(names(employer_corpus.pf2)[1:5], collapse="|"))

# Meta Data - Agent Attorney
aa_corpus = Corpus(VectorSource(h1b.edit$AGENT_ATTORNEY_NAME))
aa_corpus.dtm = DocumentTermMatrix(aa_corpus, control = list(tolower = F))

aa_corpus.pf = colSums(as.matrix(removeSparseTerms(aa_corpus.dtm, 0.99)))
aa_corpus.pf2 = aa_corpus[order(aa_corpus.pf, decreasing=T)]

h1b.edit$AGENT_ATTORNEY_POPULAR = regex(h1b$AGENT_ATTORNEY_NAME,paste(names(aa_corpus.pf2)[1:5], collapse="|"))

# Round 2 - drop more  columns
h1b.edit = select(h1b.edit,
                  -JOB_TITLE,
                  -EMPLOYER_NAME,
                  -AGENT_ATTORNEY_NAME,
                  -DECISION_DATE,
                  -CASE_SUBMITTED,
                  -WAGE_STD,
                  -PW_STD
                  )

####################
# Section 2 - Omitting NAs and Splitting Dataset
####################

# Deal with factors
h1b.edit = na.omit(h1b.edit) # drop missing values - significantly reduces dataset 201,993

# Split into training and test
set.seed(16)
h1b.split = createDataPartition(h1b.edit$RESULT, p = 0.05, list = FALSE) # Uses only p% of database
h1b.split2 = createDataPartition(h1b.split, p = 0.7, list = FALSE) # Split into test and training
h1b.tmp = h1b.edit[h1b.split,]
h1b.train = h1b.tmp[h1b.split2,]
h1b.test = h1b.tmp[-h1b.split2,]

####################
# Section 3 - Logistical Regression
####################

# Model 1: Logistical Regression
h1b.LR1 = glm(RESULT ~ .
              -JOB_TITLE_POPULAR
              -JOB_TITLE_SENIOR
              -JOB_TITLE_JUNIOR
              -JOB_TITLE_JUNIOR_ASSISTANT
              -JOB_TITLE_JUNIOR_JUNIOR
              -JOB_TITLE_JUNIOR_AUDITOR
              -JOB_TITLE_JUNIOR_ASSOCIATE
              -JOB_TITLE_JUNIOR_ANALYST
              -EMPLOYER_NAME_POPULAR
              -AGENT_ATTORNEY_POPULAR
              , data=h1b.train, family=binomial)
summary(h1b.LR1)

h1b.LR1.p = predict(h1b.LR1, newdata=h1b.test, type="response")

h1b.LR1.p.thresh  = (h1b.LR1.p > 0.01) #arbitrary
h1b.LR1.cm = table(h1b.test$RESULT, h1b.LR1.p.thresh)
h1b.LR1.rocr.p = prediction(h1b.LR1.p, h1b.test$RESULT)
h1b.LR1.auc = as.numeric(performance(h1b.LR1.rocr.p, "auc")@y.values) # 0.760193 @ 10% partition / 0.7263595 @ 5% 

# Model 2: LR + Metadata
h1b.LR2 = glm(RESULT ~ .,data=h1b.train, family=binomial)
summary(h1b.LR2)

h1b.LR2.p = predict(h1b.LR2, newdata=h1b.test, type="response")

h1b.LR2.p.thresh  = (h1b.LR2.p > 0.01)
h1b.LR2.cm = table(h1b.test$RESULT, h1b.LR2.p.thresh)
h1b.LR2.rocr.p = prediction(h1b.LR2.p, h1b.test$RESULT)
h1b.LR2.auc = as.numeric(performance(h1b.LR2.rocr.p, "auc")@y.values) # 0.72618 @ 5% data partition

# Model 3: LR+Metadata using Stepwise Regression
#################### UNCOMMENT THIS FOR FINAL
 # h1b.LR3 = step(h1b.LR2)
#
# 4 significant variables found in stepwise for 5% dataset (>90%)
h1b.LR3 = glm(RESULT ~
                JOB_TITLE_POPULAR +
                JOB_TITLE_JUNIOR_ASSOCIATE+
                NEW_CONCURRENT_EMPLOYMENT +
                AGENT_REPRESENTING_EMPLOYER +
                H1B_DEPENDENT +
                LABOR_CON_AGREE +
                PERIOD +
                WAGE_DIFF
                ,data=h1b.train, family=binomial)
###################

summary(h1b.LR3)

h1b.LR3.p = predict(h1b.LR3, newdata=h1b.test, type="response")

h1b.LR3.p.thresh  = (h1b.LR3.p > 0.01)
h1b.LR3.cm = table(h1b.test$RESULT, h1b.LR3.p.thresh)
h1b.LR3.rocr.p = prediction(h1b.LR3.p, h1b.test$RESULT)
h1b.LR3.auc = as.numeric(performance(h1b.LR3.rocr.p, "auc")@y.values) # 0.82771 @ 5% data partition

# Linear Regression Summary
h1b.LR1.cm
h1b.LR2.cm
h1b.LR3.cm

sumLR = round(matrix(c(
  h1b.LR1.cm[2,2]/(h1b.LR1.cm[2,1]+h1b.LR1.cm[2,2]),h1b.LR2.cm[2,2]/(h1b.LR2.cm[2,1]+h1b.LR2.cm[2,2]),h1b.LR3.cm[2,2]/(h1b.LR3.cm[2,1]+h1b.LR3.cm[2,2]),
  h1b.LR1.cm[1,2]/(h1b.LR1.cm[1,2]+h1b.LR1.cm[1,1]),h1b.LR2.cm[1,2]/(h1b.LR2.cm[1,2]+h1b.LR2.cm[1,1]),h1b.LR3.cm[1,2]/(h1b.LR3.cm[1,2]+h1b.LR3.cm[1,1]),
  h1b.LR1.auc,h1b.LR2.auc,h1b.LR3.auc),ncol=3,byrow=T),5)
colnames(sumLR) = c("LR1","LR+Meta","LR+Meta+SW")
rownames(sumLR) = c("TPR","FPR","AUC")
sumLR

# plot(performance(h1b.LR1.rocr.p, "tpr", "fpr"))
# abline(0,1)
# plot(performance(h1b.LR2.rocr.p, "tpr", "fpr"))
# abline(0,1)
# plot(performance(h1b.LR3.rocr.p, "tpr", "fpr"))
# abline(0,1)
  
####################
# Section 4 - CART Regression
####################

# THIS SECTION IS NOT COMPLETE

h1b.CART1 = rpart(RESULT ~., data = h1b.train, method="class") # initial model
prp(h1b.CART1)

set.seed(16) #random number
h1b.CART2 = train(y = h1b.train$RESULT,
                        x = subset(h1b.train, select = -c(RESULT)),
                        method = "rpart",
                        trControl = trainControl(method = "cv", number = 10),
                        tuneGrid = data.frame(.cp = seq(.001, .02, .001))) 

h1b.CART2$results$cp[which.max(h1b.CART2$results$Accuracy)] # best cp is 0.006-0.008
prp(h1b.CART2$finalModel)










# 
# h1b.meta = subset(h1b, select = c(EMPLOYER_NAME, AGENT_ATTORNEY_NAME, JOB_TITLE))
# 
# h1b.meta %>%
#   group_by(JOB_TITLE) %>%
#   summarise(COUNT = n()) %>%
#   arrange(desc(COUNT)) %>% ungroup() -> common_jobs
# 
# ggplot(common_jobs[1:15,], aes(x=reorder(JOB_TITLE,COUNT),y=COUNT)) +
#   geom_bar(stat = "identity", fill = "blue") + coord_flip() +
#   xlab("JOB TITLE") + ylab("TOTAL NO. OF APPLICATIONS")
# 
# dj = h1b.meta %>% group_by(JOB_TITLE) %>% top_n(n=50) %>% ungroup()
# 
# 
# ## Selecting by nr
# ggplot(data = dj, aes(x = reorder(JOB_TITLE,nr), y = nr)) +  
#   geom_bar(stat="identity", fill="gold", colour="black") +
#   coord_flip() + theme_bw(base_size = 10)  +
#   labs(title="", x ="Job title (top 50)", y = "Number of applications")
# 
# 
# lf8 = names(which(prop.table(table(h1b.meta$EMPLOYER_NAME)) < 0.1))
# levels(h1b.meta$EMPLOYER_NAME)[levels(h1b.meta$EMPLOYER_NAME) %in% lf8] <- "Other"
# table(h1b.meta$EMPLOYER_NAME)
##############################
 












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
table(h1b$RESULT) # OK