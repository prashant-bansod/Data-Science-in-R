
mmix<-read.csv("MMix.csv",header=TRUE,stringsAsFactors=FALSE)

dim(mmix)
str(mmix)
head(mmix)
tail(mmix)

#DV=1/0 variable :presence /absence of Sales

#Dv = Sales

#summary statistics
summary(mmix)
summary(mmix$NewVolSales)

#checking outliers
x<-boxplot(mmix$NewVolSales)
out<-x$out


#Outlier treatment
x$out
index<-mmix$NewVolSales %in% x$out
sum(index)

#Treating outliers values
mmix$NewVolSales[index]<-mean(mmix$NewVolSales,na.rm=TRUE)
summary(mmix$NewVolSales)

#checking missing values
colSums(is.na(mmix))
summary(mmix)


##--------------------------------Exploratory Analysis -------------------------------------#
library(ggplot2)

##Univariate Analysis

qplot(mmix$NewVolSales)

hist(mmix$NewVolSales)
hist(mmix$Base.Price)


##Bivariate analysis 

#Viz
with(mmix,qplot(NewVolSales,Base.Price))
with(mmix,qplot(NewVolSales,InStore))
qplot(mmix$NewVolSales,mmix$Radio)

#Correlations
cor(mmix$NewVolSales,mmix$Base.Price)
with(mmix,cor(NewVolSales,Radio))
with(mmix,cor(NewVolSales,InStore))
with(mmix,cor(LnSales,InStore))


with(mmix,qplot(log(NewVolSales),InStore))

##Creating Indicator Variables
unique(mmix$Website.Campaign)
table(mmix$Website.Campaign)
mmix$FB<-ifelse(mmix$Website.Campaign=="Facebook",1,0)


##Creating New Variables
#Data TRansformations
mmix$LnSales<-log(mmix$NewVolSales)
mmix$LnPrice<-log(mmix$Base.Price)

mmix$OfflineSpend<-mmix$Radio+mmix$TV+mmix$InStore


#Creating price buckets - converting numeric variable into categorical
mmix$Price_Bkt[mmix$Base.Price < 15.03]<-"Low"
mmix$Price_Bkt[mmix$Base.Price >= 15.03 & mmix$Base.Price < 15.33]<-"Avg"
mmix$Price_Bkt[mmix$Base.Price >= 15.33 & mmix$Base.Price < 15.64]<-"High"
mmix$Price_Bkt[mmix$Base.Price >= 15.64]<-"V.High"



##--------------------------------Training and Test Splits -------------------------------------#

sampling<-sort(sample(nrow(mmix), nrow(mmix)*.7))
head(sampling)
length(sampling)


#Select training sample
train<-mmix[sampling,]

test<-mmix[-sampling,]
nrow(train)
nrow(test)

#looking at DV
summary(train$NewVolSales)
summary(test$NewVolSales)

##--------------------------------Building models -------------------------------------#

##Building SimpLe Linear Regression Model

attach(mmix)

Reg<-lm(NewVolSales~Base.Price,data=train)

sales=A+B(base.price)
Newvolsales=47000-1784(base.price)

#Checking summary of the regression object "Reg"
Reg
summary(Reg)

#Metrics to assess a model:
#Rsquare
#Coefficients
#P values : Significance levels of the IDV's
#Residuals distribution

#Factor variables as IDV's
Reg<-lm(NewVolSales~as.factor(Price_Bkt),data=mmix)
summary(Reg)
#Creating dummy for low bucket since it is significant
mmix$PrizeBktLow<-ifelse(mmix$Price_Bkt=="Low",1,0)

attach(mmix)
Reg<-lm(NewVolSales~PrizeBktLow,data=mmix)
summary(Reg)

#Getting the formula
formula(Reg)

#Iteration 1
Mulreg<-lm(NewVolSales~Base.Price+as.factor(Website.Campaign),data=train)
Mulreg
summary(Mulreg)

Mulreg<-lm(NewVolSales~Base.Price+InStore+WebCamp,data=train)
Mulreg
summary(Mulreg)

mmix$WebCamp<-ifelse(mmix$Website.Campaign=="Website Campaign",1,0)

#Iteration 2
Mulreg<-lm(NewVolSales~Base.Price+InStore+Radio+TV+as.factor(Website.Campaign),data=mmix)
Mulreg
summary(Mulreg)

#Iteration 3
Mulreg<-lm(NewVolSales~LnPrice+InStore+WebCamp,data=mmix)
Mulreg
summary(Mulreg)


##--------------------------------Testing models -------------------------------------#

##Getting predicted values
PredSales<-predict(Mulreg,newdata=test)
head(PredSales)

test$pred<-PredSales
test$resid<-test$NewVolSales - test$pred

PredSales<-predict(Mulreg)
length(PredSales)


##Finding Residuals
ResSales<-resid(Mulreg,newdata=test)
head(ResSales)

plot(ResSales)

##Plotting Residuals vs Predicted/actual Values
##Checking Heteroskedastcity - exists if there is a pattern between predicted values and error

plot(test$pred,test$resid,abline(0,0))


##Plotting actual vs predicted values
plot(test$NewVolSales,col="blue",type="l")
lines(PredSales,col="red",type="l")

# exploring relationships among features: correlation matrix
cor(train[c("InStore", "Base.Price", "NewVolSales", "Radio")])





##--------------------------------checking for multicollinearity within variables -------------------------------------#


library(car)
vif(Mulreg)

