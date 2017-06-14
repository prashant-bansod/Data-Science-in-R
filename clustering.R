

##Kmeans Clustering

setwd("D:\\ML\\clustering")
#

--------------------------------------Step1 : Reading and exploring the dataset

install.packages("nutshell")
library(nutshell)
data(sanfrancisco.home.sales)
?sanfrancisco.home.sales
data<-sanfrancisco.home.sales

dim(data)
str(data)
names(data)
summary(data)

#Subsetting Data 
sample<-na.omit(data[,c("price", "bedrooms", "squarefeet")])

dim(sample)
str(sample)
names(sample)

##--------------------------------------Step2 : Scaling the data ---------------------------------------------


sample[,"price"]-mean(sample[,"price"]))/(sd(sample[,"price"])
sample[,"bedrooms"]-mean(sample[,"bedrooms"]))/(sd(sample[,"bedrooms"])
                                                                                    
list<-names(sample)
scaled_data<-data.frame(rownum<-1:2256)
for(i in 1:length(list))
{

  x<-(sample[,i]-mean(sample[,i]))/(sd(sample[,i]))
  scaled_data<-cbind(scaled_data,x)
  names(scaled_data)[i+1]<-paste("scaled_",list[i])
  print(list[i])

}
head(scaled_data)

scaled_data<-scaled_data[,-1]
# 
sample<-cbind(sample,scaled_data)
names(sample)
# head(sample)


##--------------------------------------Step3 : kmeans algorithm ---------------------------------------------


set.seed(200)
names(sample)
fit.km<-kmeans(sample[,4:6],3)

#We will get a list object
fit.km$size #No of observations in each cluster
fit.km$withinss #Within sum of squares metric for each cluster
fit.km$totss #The total sum of squares
fit.km$tot.withinss #Total within-cluster sum of squares, i.e., sum(withinss)
fit.km$betweenss #The between-cluster sum of squares, i.e. totss-tot.withinss
head(fit.km$cluster)
##--------------------------------------Step4 : finding the optimal number of clusters (k value) ---------------------------------------------

#Creating a screeplot-plot of cluster's tot.withinss wrt number of clusters

wss<-1:15
number<-1:15

for (i in 1:15)
  
{
  wss[i]<-kmeans(sample[,4:6],i)$betweenss
}

#Shortlised optimal number of clusters : between 7 and 9


library(ggplot2)
data<-data.frame(wss,number)
p<-ggplot(data,aes(x=number,y=wss),color="red")
p+geom_point()+scale_x_continuous(breaks=seq(1,20,1))

##--------------------------------------Step5a : Rerunning the algorithm with k=7(optimal no) ---

#Building 7 cluster model
set.seed(100)
fit.km<-kmeans(sample[,4:6],7)

##Merging the cluster output with original data

head(fit.km$cluster)
sample$cluster<-fit.km$cluster

##--------------------------------------Step5b : Profiling the clusters-----------

#Cluster wise Aggregates
cmeans<-aggregate(sample[,1:3],by=list(sample$cluster),FUN=mean)
cmeans
dim(cmeans)

#Population Aggregates
apply(sample[,1:3],2,mean)
apply(sample[,1:3],2,sd)

list1<-names(cmeans)

#Z score calculation
#z score =population_mean - group_mean /population_sd
for(i in 1:length(list1))
{
y<-(cmeans[,i+1] - apply(sample[,1:3],2,mean)[i])/(apply(sample[,1:3],2,sd)[i])
cmeans<-cbind(cmeans,y)
names(cmeans)[i+4]<-paste("z",list1[i+1],sep="_")
print(list1[i+1])
}

cmeans<-cmeans[,-8]
names(cmeans)

##-------------------------Step6a : Rerunning the algorithm with k=9(in order to compare)----

#Build a 9 clusters

fit.km1<-kmeans(sample[,1:3],9)

sample$cluster9<-fit.km1$cluster


##--------------------------------------Step6b : Profiling the clusters---------------------------------------------

#Cluster summary
cmeans1<-aggregate(sample[,1:3],by=list(sample$cluster9),mean)

#Population summary
apply(sample[,1:3],2,mean)
apply(sample[,1:3],2,sd)
cmeans1

#z-score calculation function
list2<-names(cmeans1)
#z score =population_mean - cluster_group_mean /population_sd
for(i in 1:length(list2))
{
  print(i)
  y<-(cmeans1[,i+1] - apply(sample[,1:3],2,mean)[i])/(apply(sample[,1:3],2,sd)[i])
  cmeans1<-cbind(cmeans1,y)
  names(cmeans1)[i+4]<-paste("z",list2[i+1],sep="_")
  print(list2[i+1])
}

cmeans1<-cmeans1[,-8]
names(cmeans1)

#Note : The higher the value of z, the different is the group from the population




##--------------------------------------Step7 : Visualise the clusters---------------------------------------------

#Plotting groups across two dimensions

library(ggplot2)

#sample data
#For cluster7
#bedroom Vs Squarefeet
p<-ggplot(sample,aes(x=bedrooms,y=squarefeet))
p+geom_point(aes(colour=as.factor(cluster)))
  


p<-ggplot(sample,aes(x=price,y=bedrooms))
p+geom_point(aes(colour=as.factor(cluster)))

 
