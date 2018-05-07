load("h1b_Edit.RData")
library(dplyr)
library(caret)
str(h1b.edit)

#=================1. Adjust Data Structure=================

#Convert MONTH_APPLIED to 12 Columns
summary(h1b.edit$MONTH_APPLIED)
h1b.edit$Jan = ifelse(h1b.edit$MONTH_APPLIED == "1", 1, 0)
h1b.edit$Feb = ifelse(h1b.edit$MONTH_APPLIED == "2", 1, 0)
h1b.edit$Mar = ifelse(h1b.edit$MONTH_APPLIED == "3", 1, 0)
h1b.edit$Apr = ifelse(h1b.edit$MONTH_APPLIED == "4", 1, 0)
h1b.edit$May = ifelse(h1b.edit$MONTH_APPLIED == "5", 1, 0)
h1b.edit$Jun = ifelse(h1b.edit$MONTH_APPLIED == "6", 1, 0)
h1b.edit$Jul = ifelse(h1b.edit$MONTH_APPLIED == "7", 1, 0)
h1b.edit$Aug = ifelse(h1b.edit$MONTH_APPLIED == "8", 1, 0)
h1b.edit$Sep = ifelse(h1b.edit$MONTH_APPLIED == "9", 1, 0)
h1b.edit$Oct = ifelse(h1b.edit$MONTH_APPLIED == "10", 1, 0)
h1b.edit$Nov = ifelse(h1b.edit$MONTH_APPLIED == "11", 1, 0)
h1b.edit$Dec = ifelse(h1b.edit$MONTH_APPLIED == "12", 1, 0)

#Convert WORKSITE_STATE
summary(h1b.edit$WORKSITE_STATE)
h1b.edit$CA = ifelse(h1b.edit$WORKSITE_STATE == "CA", 1, 0)
h1b.edit$TX = ifelse(h1b.edit$WORKSITE_STATE == "TX", 1, 0)
h1b.edit$NJ = ifelse(h1b.edit$WORKSITE_STATE == "NJ", 1, 0)
h1b.edit$NY = ifelse(h1b.edit$WORKSITE_STATE == "NY", 1, 0)
h1b.edit$IL = ifelse(h1b.edit$WORKSITE_STATE == "IL", 1, 0)

#choose subset of data with Result=0
h1b.pass=h1b.edit[h1b.edit$RESULT==0,]

#======================2. Clustering============================

#======2.1 Create a new dataframe with WAGE_DIFF and 12 MONTH_APPLIED======
clust.month=h1b.pass %>% select(WAGE_DIFF,Jan,Feb,Mar,Apr,May,Jun,Jul,Aug,Sep,Oct,Nov,Dec)

#Preprocess data 
pp1<-preProcess(clust.month,method=c("center","scale"))
clust1=predict(pp1,clust.month)

#Run K-means clustering
set.seed(144)
km1=kmeans(clust1,iter.max = 100,8)
km1$centers
table(km1$cluster)

#Hirachy clustering not working, Otherwise run below code.
#d1=dist(clust1)
#hclust.mod1=hclust(d1,method="ward.D2")

#======2.2 Create a new dataframe with WAGE_DIFF and TOP5 WORKSITE_STATE======
clust.state=h1b.pass %>% select(WAGE_DIFF,CA,TX,NJ,NY,IL)

pp2<-preProcess(clust.state,method=c("center","scale"))
clust2=predict(pp2,clust.state)

set.seed(144)
km2=kmeans(clust2,iter.max = 100,8)
km2$centers
table(km2$cluster)

#======2.3 Create a new dataframe with WAGE_DIFF with TOP5 WORKSITE_STATE and TOP5 MONTH_APPLIED======
clust.state_month=h1b.pass %>% select(WAGE_DIFF,CA,TX,NJ,NY,IL,Jan,Feb,Mar,Apr,May)

pp3<-preProcess(clust.state_month,method=c("center","scale"))
clust3=predict(pp3,clust.state_month)

set.seed(144)
km3=kmeans(clust3,iter.max = 100,8)
km3$centers
table(km3$cluster)



