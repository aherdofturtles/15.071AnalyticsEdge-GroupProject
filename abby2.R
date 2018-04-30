load("H1B.RData")
library(caret)
str(h1b)
hist(h1b$WAGE_STD)

pp<-preProcess(h1b,method=c("center","scale"))
h1b.scaled=predict(pp,h1b)

set.seed(144)
km<-kmeans(h1b.scaled,iter.max = 100,8)
km$centers

d=dist(h1b.scaled)
hclust.mod=hclust(d,method="ward.D2")

