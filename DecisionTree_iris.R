#=======================================================================================================
# Program For Decision Tree Using iris Data:- 
#=======================================================================================================
library(rpart) #library for Decision tree for generation of parts
Dataset=iris  
shuff=Dataset[sample(1:nrow(Dataset)),] # Shuffling the given dataset using sample command
trainData=shuff[1:125,] # divide that data into Test & train 
testData=shuff[126:150,]
tree=rpart(Species ~ Sepal.Length+ Sepal.Width+Petal.Length+Petal.Width,trainData,method="class") # for generating tree
plot(tree)                            #plotting the tree 
text(tree, use.n = TRUE)  
predicted=round(predict(tree,testData)) #rounding the values 
Npredict=NULL # creating the vector for Predicting the class
for (i in 1:nrow(predicted)) 
{
  prediction=as.numeric(which.max(predicted[i,]))
  Npredict=c(Npredict,prediction)
}
compare=(Npredict==as.numeric(testData$Species))               # compare actual classes and predicted classes
accuracy=sum(compare)/length(Npredict)*100                      # accuracy
print(accuracy)                  #printing the accuracy