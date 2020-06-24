#load packages
library(randomForest)
library(rCharts)

#use iris dataset, 70% to train and 30% to verify
index <- sample(2,nrow(iris),replace = TRUE,prob=c(0.7,0.3))
traindata <- iris[index==1,]
testdata <- iris[index==2,]

#the number of decision trees is set by "ntree"
set.seed(1234)
rf_ntree <- randomForest(Species~.,data=traindata,ntree=300)
plot(rf_ntree)
iris_rf <- randomForest(Species ~ ., data=traindata, ntree=100,
+ proximity=TRUE)
iris_rf

#draw the error rate of each tree
plot(iris_rf)

#analyze the importance of each variable
importance(iris_rf)
varImpPlot(iris_rf)

#see the nodes of each tree using "treesize"
head(treesize(iris_rf,terminal = TRUE))
count_data <- as.data.frame(plyr::count(treesize(iris_rf,
+ terminal = TRUE)))
head(count_data,5)

#explore the results interactively using rPlot
rPlot(x='bin(x,1)',y='freq',data=count_data,type='bar')

#predict using the trained model
iris_pred <- predict(iris_rf,newdata=testdata)
table(iris_pred,testdata$Species)
plot(margin(iris_rf, testdata$Species))

#train using different variables to find the most important ones
n <- length(names(traindata))
set.seed(1234)
for (i in 1:(n-1)){
+     model <- randomForest(Species~., data = traindata, mtry = i)
+     err <- mean(model$err.rate)
+     print(err)
+ }
iris_rf_1 <- randomForest(Species ~ ., data=traindata,mtry=3, ntree=100, proximity=TRUE)
iris_rf_1

