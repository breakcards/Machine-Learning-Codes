data<- read.csv("E:/Nikhil Data/sem2/datasets/wdbc.csv")   #Breast Cancer dtaset
y<- data[,-1]
library(rpart)
library(randomForest)
set.seed(980)
data<- sample (2,nrow(data),replace=TRUE,prob=c(0.75,0.25))
train_data<- y[data==1,]
test_data<-  y[data==2,-1]
test_data1<- y[data==2,1]

mtry<- seq(2,30,by=2)
forest<-seq(50,500,by=50)
result<-c()
for( i in mtry) 
  {
   overall_acc<-c() 
   for( j in forest) {
    model<- randomForest(M ~.,data =train_data,mtry=i,ntree=j,importance=TRUE,proximity=TRUE)
    predValid<- predict(model,test_data)
    
    accuracy<- mean(as.numeric(predValid) == as.numeric(test_data1))*100
    overall_acc<-c(overall_acc,accuracy)
  }
  result<- c(result,overall_acc)
}

res<- matrix(result,length(mtry),length(forest),byrow=T)
x<-NULL
for( i in 1:length(mtry) ) 
  {
  
   z<- c( mtry[i],max(res[i,]),forest[which(res[i,]==max(res[i,]))][1])
   x<- c(x,z)
  }
x_mat<-matrix(x,15,3,byrow=T)
k<-c("mtry","accuracy","ntree")
x_mat<-data.frame(x_mat)
names(x_mat) <- k

mtrya<- x_mat[which(x_mat[,2] == max(x_mat[,2]))[1],1]
ntreea<-x_mat[which(x_mat[,2] == max(x_mat[,2]))[1],3]
cat("The maximum efficiency", max(x_mat[,2]), "for given mtry is: " , mtry1, "with tree", ntree1)