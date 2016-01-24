library(e1071)
ptm <- proc.time()
train<-read.csv("short_prac_train.csv")
train$label<-as.factor(train$label)
test<-read.csv("short_prac_test.csv")
test$label<-as.factor(test$label)

train.tune<-tune.svm(label~.,data=train,kernel='polynomial',degree=5)
# Polynomial kernel with 5 degrees, chosen in according to Burgess (NIPS, 1996)
train.tune$best.model
proc.time() - ptm