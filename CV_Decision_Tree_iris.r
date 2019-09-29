CrossValidation=function(n)                                     # n is number of folds
{
  library(rpart)
  DataSet=iris
  shuff=DataSet[sample(1:nrow(DataSet)),]
  CV<-cut(1:nrow(DataSet),breaks=n, labels=FALSE)       # n Folds
  acc=NULL
  for( j in 1:n) 
  {
    
    testData<- shuff[which(j==CV,arr.ind=T),]
    trainData<- shuff[-(which(j==CV,arr.ind=T)),]
    
    tree=rpart(Species~Sepal.Length+Sepal.Width+Petal.Length+Petal.Width,trainData,method="class")   #for tree
    plot(tree)                                            
    text(tree, use.n = TRUE)
    pd=round(predict(tree,testData))
    Npred=NULL
    for (i in 1:nrow(pd)) 
    {
      pred=as.numeric(which.max(pd[i,]))
      Npred=c(Npred,pred)
    }
    co=(Npred==as.numeric(testData$Species))               # compare actual and predicted
    
    accuracy=sum(co)/length(Npred)                        # accuracy of each folds
    acc=c(acc,accuracy)           
    print(accuracy)
  }
  return(Accuracy=mean(acc))                             
}