install.packages("e1071")
library(e1071)

train<-read.csv("short_prac_train.csv")
train$label<-as.factor(train$label)
test<-read.csv("short_prac_test.csv")
test$label<-as.factor(test$label)


ptm <- proc.time() # Time training.
train.tune<-tune.svm(label~.,data=train,kernel='polynomial',degree=5)
proc.time() - ptm
# Polynomial kernel with 5 degrees, chosen in according to Burgess (NIPS, 1996)
train.tune$best.model

##  Predict and Evaluate
train.svm<-svm(label~.,train,cost=train.tune$best.model$cost,gamma=train.tune$best.model$gamma,coef0=train.tune$best.model$coef0,kernel='polynomial',degree=5)
table(test$label,predict(train.svm,newdata=test[,-1]))
sum(diag(table(test$label,predict(train.svm,newdata=test[,-1]))))/nrow(test)

### Result is accurracy of 0.92.
