rm(list=ls())
gc()

# KNN k-nearest neighbors
library(class)
library(gmodels)

#prepare data
set.seed(12345) #set random seed in order to repeat the result
data(iris)
iris_rand <- iris[order(runif(150)),]
iris_z <- as.data.frame(scale(iris_rand[,-5])) #z score normalize
train <- iris_z[1:105,]
test <- iris_z[106:150,]
train.label <- iris_rand[1:105,5]
test.label <- iris_rand[106:150,5]

#kNN
pred <- knn(train,test,train.label,k=10)

#comfusion matrix 
CrossTable(pred,test.label,prop.r = F,prop.t = F,prop.chisq = F)

# Naive Bayes
library(e1071)
library(gmodels)

set.seed(12345) #set random seed in order to repeat the result
iris_rand <- iris[order(runif(150)),]
train <- iris_rand[1:105,-5]
test <- iris_rand[106:150,-5]
train.label <- iris_rand[1:105,5]
test.label <- iris_rand[106:150,5]

#tranform numerical variable to classified variable
conver_counts <- function(x){
  q <- quantile(x)
  sect1 <- which(q[1] <= x & x<= q[2])
  sect2 <- which(q[2 ]< x & x <= q[3])
  sect3 <- which(q[3]< x & x <= q[4])
  sect4 <- which(q[4]< x & x <= q[5])
  x[sect1] <- 1
  x[sect2] <- 2
  x[sect3] <- 3
  x[sect4] <- 4
  return(x)
}
train <- apply(train,2,conver_counts)

#naiveBayes
m <- naiveBayes(train,train.label,laplace=1)
pred <- predict(m,test,type="class") 

#comfusion matrix 
CrossTable(pred,test.label,prop.r = F,prop.t = F,prop.chisq = F)

