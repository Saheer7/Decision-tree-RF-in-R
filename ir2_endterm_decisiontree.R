library(ggplot2)   #For plots
library(lattice)   #For plots and for caret
library(caret)     #ML package
library(rpart)     #ulibrary for rpart function for CART
library(rpart.plot)#plot trees
library(pROC)      #For ROC curve
library(maptree)   #For mapping trees (plot)
library(partykit)   #for ctree, plot
library(MLmetrics)

ovdata=read.csv("C:/Users/91944/Documents/R/praxis/data/overtrain_smoke.csv")
undata=read.csv("C:/Users/91944/Documents/R/praxis/data/undertrain_smoke.csv")
test_data=read.csv("C:/Users/91944/Documents/R/praxis/data/validate_smoke.csv")
ovndata=read.csv("C:/Users/91944/Documents/R/praxis/data/overtrain_nonsmoke.csv")
unndata=read.csv("C:/Users/91944/Documents/R/praxis/data/undertrain_nonsmoke.csv")
ntest_data=read.csv("C:/Users/91944/Documents/R/praxis/data/validate_nonsmoke1.csv")


# train a Classification Tree
fit <- rpart(stroke~., data=ovdata, method="class",cp=0.013) 
fit2<- rpart(stroke~., data=undata, method="class",cp=0.02)

# display results data-1
plotcp(fit)
print(fit)
plot(fit)
text(fit)
rpart.plot(fit,main="Classification Tree",extra = 100)


# make predictions
predictions = predict(fit, test_data[,1:10],type='class')
#print(predictions)   
#Confusion Matrix

table(test_data$stroke,predictions)
Accuracy(predictions,test_data$stroke)
F1_Score(predictions,test_data$stroke)
#Convert predictions factor to numbeic
predictions=as.numeric(as.character(predictions))

#Area under curve
auc(test_data$stroke,predictions)   

plot(roc(test_data$stroke,predictions))  #Roc plot


library(ROCR)
#Plot ROC curve
ROCpred <- prediction(predictions,test_data$stroke)
ROCperf <- performance(ROCpred,"tpr","fpr")
plot(ROCperf)


#display results data-2  #########Uv data
plotcp(fit2)
print(fit2)
plot(fit2)
text(fit2)
rpart.plot(fit2,main="Classification Tree",extra = 100)

# make predictions
predictions = predict(fit2, test_data[,1:10],type='class')
#print(predictions)   
#Confusion Matrix

table(test_data$stroke,predictions)
Accuracy(predictions,test_data$stroke)
F1_Score(predictions,test_data$stroke)
#Convert predictions factor to numbeic
predictions=as.numeric(as.character(predictions))

#Area under curve
auc(test_data$stroke,predictions)   

plot(roc(test_data$stroke,predictions))  #Roc plot

###################################################NON SMOKE


# train a Classification Tree
fit3<- rpart(stroke~., data=ovndata, method="class",cp=0.014) 
fit4<- rpart(stroke~., data=unndata, method="class",cp=0.011)

# display results data-1
plotcp(fit3)
print(fit3)
plot(fit3)
text(fit3)
rpart.plot(fit3,main="Classification Tree",extra = 101)


# make predictions
predictions = predict(fit3, ntest_data[,1:9],type='class')
#print(predictions)   
#Confusion Matrix

table(ntest_data$stroke,predictions)
Accuracy(predictions,ntest_data$stroke)
F1_Score(predictions,ntest_data$stroke)
#Convert predictions factor to numbeic
predictions=as.numeric(as.character(predictions))

#Area under curve
auc(ntest_data$stroke,predictions)   

plot(roc(ntest_data$stroke,predictions))  #Roc plot


library(ROCR)
#Plot ROC curve
ROCpred <- prediction(predictions,test_data$stroke)
ROCperf <- performance(ROCpred,"tpr","fpr")
plot(ROCperf)


#display results data-4  #########Uv data
plotcp(fit4)
print(fit4)
plot(fit4)
text(fit4)
rpart.plot(fit4,main="Classification Tree",extra = 100)

# make predictions
predictions = predict(fit4, ntest_data[,1:9],type='class')
#print(predictions)   
#Confusion Matrix

table(ntest_data$stroke,predictions)
Accuracy(predictions,ntest_data$stroke)
F1_Score(predictions,ntest_data$stroke)
#Convert predictions factor to numbeic
predictions=as.numeric(as.character(predictions))

#Area under curve
auc(ntest_data$stroke,predictions)   

plot(roc(ntest_data$stroke,predictions))  #Roc plot

################################################Random Forest
library(randomForest)
fit5=randomForest(stroke~., data=ovdata )

# make predictions
predictions = predict(fit5, newdata=test_data[,1:10],type="class")
predictions=ifelse(predictions>0.07,1,0)  #Threshold increases then accuracy increases but 1s predicted decreases 
#print(predictions)   
#Confusion Matrix

table(test_data$stroke,predictions)


Accuracy(predictions,test_data$stroke)
F1_Score(predictions,test_data$stroke)
#Convert predictions factor to numbeic
predictions=as.numeric(as.character(predictions))

#Area under curve
auc(test_data$stroke,predictions)   

plot(roc(test_data$stroke,predictions))  #Roc plot
