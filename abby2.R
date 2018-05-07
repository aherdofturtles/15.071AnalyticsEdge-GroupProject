load("h1b_Edit.RData")
library(dplyr)
library(caret)
str(h1b.edit)

# =================1. Adjust Data Structure=================

# Convert MONTH_APPLIED to 12 Columns
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

# choose subset of data with Result=0
h1b.pass=h1b.edit[h1b.edit$RESULT==0,]

# ======================2. Clustering============================

# ======2.1 Create a new dataframe with WAGE_DIFF and 12 MONTH_APPLIED======
clust.month=h1b.pass %>% select(WAGE_DIFF,Jan,Feb,Mar,Apr,May,Jun,Jul,Aug,Sep,Oct,Nov,Dec)

# Normalized data 
pp1<-preProcess(clust.month,method=c("center","scale"))
clust1=predict(pp1,clust.month)

# Run K-means clustering
set.seed(144)
km1=kmeans(clust1,iter.max = 100,4)

km1$centers
km.clusters1=km1$cluster
km1$tot.withinss
table(km.clusters1)


# Hirachical clustering not working because of huge dataset, Below codes are based on 10000 rows
clust1d=clust1[1:10000,]
d1=dist(clust1d)
hclust.mod1=hclust(d1,method="ward.D2")

# Plot Dendrogram
plot(hclust.mod1, labels=F, ylab="Dissimilarity")

# Scree Plot 
hc.dissim1 <- data.frame(k = seq_along(hclust.mod1$height),
                        dissimilarity = rev(hclust.mod1$height))
plot(hc.dissim1$k, hc.dissim1$dissimilarity, type="l")
plot(hc.dissim1$k, hc.dissim1$dissimilarity, type="l", xlim=c(0,50))

h.clusters1=cutree(hclust.mod1, 4)
aggregate(clust1d, by=list(h.clusters1), mean)
table(h.clusters1)


# ======2.2 Create a new dataframe with WAGE_DIFF and TOP5 WORKSITE_STATE======
clust.state=h1b.pass %>% select(WAGE_DIFF,CA,TX,NJ,NY,IL)

pp2<-preProcess(clust.state,method=c("center","scale"))
clust2=predict(pp2,clust.state)

set.seed(144)
km2=kmeans(clust2,iter.max = 100,6)

km2$centers
km.clusters2=km2$cluster
km2$tot.withinss
table(km.clusters2)

# Hirachical clustering on 100 rows
clust2d=clust2[1:100,]
d2=dist(clust2d)
hclust.mod2=hclust(d2,method="ward.D2")

par(mar=c(1,1,1,1))
plot(hclust.mod2, labels=F, ylab="Dissimilarity")

hc.dissim2 <- data.frame(k = seq_along(hclust.mod2$height),
                         dissimilarity = rev(hclust.mod2$height))
plot(hc.dissim2$k, hc.dissim2$dissimilarity, type="l")
plot(hc.dissim2$k, hc.dissim2$dissimilarity, type="l", xlim=c(0,10))

h.clusters2=cutree(hclust.mod2, 6)
aggregate(clust2d, by=list(h.clusters2), mean)
table(h.clusters2)

# ======2.3 Create a new dataframe with WAGE_DIFF with TOP5 WORKSITE_STATE and TOP5 MONTH_APPLIED======
clust.state_month=h1b.pass %>% select(WAGE_DIFF,CA,TX,NJ,NY,IL,Jan,Feb,Mar,Apr,May)

pp3<-preProcess(clust.state_month,method=c("center","scale"))
clust3=predict(pp3,clust.state_month)

set.seed(144)
km3=kmeans(clust3,iter.max = 100,6)

km3$centers
km.clusters3=km3$cluster
km3$tot.withinss
table(km.clusters3)

# Hirachical clustering on 100 rows
clust3d=clust3[1:100,]
d3=dist(clust3d)
hclust.mod3=hclust(d3,method="ward.D2")

par(mar=c(1,1,1,1))
plot(hclust.mod3, labels=F, ylab="Dissimilarity")

hc.dissim3 <- data.frame(k = seq_along(hclust.mod3$height),
                         dissimilarity = rev(hclust.mod3$height))
plot(hc.dissim3$k, hc.dissim2$dissimilarity, type="l")
plot(hc.dissim3$k, hc.dissim2$dissimilarity, type="l", xlim=c(0,10))

h.clusters3=cutree(hclust.mod3, 6)
aggregate(clust3d, by=list(h.clusters3), mean)
table(h.clusters3)

