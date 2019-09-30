
iris
tmp<-iris[-(1:50),]
r<-tmp[sample(nrow(tmp)),]
x<-r[c(1:80),]
y<-r[c(81:100),]
  
  #Euclidian distance function
  distance<-c()
  t<-c()
  z<-c()
  class<-c()
  accr<-c()
  
  
  for(i in 1:20)
    
  {
    for(j in 1:80)
    {
      distance<-sqrt((x[j,1]- y[i,1])^2 + (x[j,2] - y[i,2])^2 + (x[j,3]- y[i,3])^2 + (x[j,4]- y[i,4])^2)
      z<-c(z,distance)
    }
  }
  
  
  
  t1<-matrix(z,20,80,byrow=T)
  ff<-c()
  accr<-c()
  
  
  
  k<-c(1,3,5,7,9)
  for(v in k)
  {
  for(m in 1:20)  #finding class
  {
    dd<-which.max(as.numeric(summary(x$Species[(order(t1[m,]))[1:v]])))
   
    ff<-c(ff,dd)
    
  }
  
  
  act_class<-as.numeric(y$Species)
  
  accuracy<-((sum(act_class==ff))/length(ff))*100  #calculating accuracy
  accr<-c(accr,accuracy)
  }
  

z<-data.frame(k,accr)  #Resulting accuracies for different values of k

