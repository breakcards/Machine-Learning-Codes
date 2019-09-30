library(rpart); library(randomForest); library(caret)
set.seed(980)

dat<- read.csv("E:/Nikhil Data/sem2/datasets/wdbc.csv")    #Breast Cancer data
y<- dat[,-1]
data=y[sample(1:nrow(y)),]
cv<-cut(1:nrow(data),breaks=5, labels=F) #To apply k folds
acc3<-c()

mt=seq(2,30,by=2)
for (i in mt)
  {
   acc2<-c()
   for(k in 1:5) {
    fold<-which(cv == k)
    test<-data[fold,]
    train<-data[-fold,]
    test_class<-test[,1]
    
    acc1<-c()
    nt<-seq(50,500, by=50)
    for(j in nt) {
      model<- randomForest( M ~. , data =train ,ntree=j, mtry=i, importance=TRUE,proximity=TRUE)
      predValid<- predict(model,test)
      cmat<-confusionMatrix(predValid,test$M)
      over<-as.numeric(cmat$overall['Accuracy'])
      acc1<-c(acc1,over)
    }
    acc2<-c(acc2,max(acc1))
  }
 acc3<-c(acc3,mean(acc2))
}
dframe<-data.frame(mt,acc3)
acc_max<-which.max(dframe[,2])
mtryy<-dframe[acc_max,1]
accuracy<-dframe[acc_max,2]
show(data.frame(mtryy,accuracy))