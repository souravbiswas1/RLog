#Setting the path of the working directory::
setwd("E:\\Jigsaw\\Analytics with R\\Work\\Logistic_Regression")

library(dplyr)
library(gains)
library(irr)
library(caret)


#Reading the file::
gf<-read.csv("goodforu (1).csv")

#Some exploratory data analysis and findings::
summary(gf)

#Checking the no of rows & columns::
dim(gf)

#Names of the column name::
names(gf)

#Structure of the dataset::
str(gf)

#Total no of missing value::
sum(is.na(gf))

#Segregating the customers having good and bad perception upon Brand A as per the given rankings::
gf<-gf%>%mutate(Target_A=ifelse(X23>4,1,0))
#gf<-select(gf,-X23)






#Question No.2::
Zero_count<-gf%>%filter(Target_A==0)%>%summarise(Total_Zero=n())

#making the variable numeric::
Zero_count$Total_Zero<-as.numeric(Zero_count$Total_Zero)

#Total no of rows::
Total_Count<-gf%>%summarise(Count=n())

#Percentage of score 4 or less::
Percentage<-round(((Zero_count$Total_Zero/Total_Count$Count)*100),2)







#Question No.3::
One_count<-gf%>%filter(Target_A==1)%>%summarise(Total_One=n())
print(One_count)






#Question No.10::
gf%>%filter(X2==1)%>%summarise(Total_Count=n())





#Question No.11::
gf%>%filter(X16==1)%>%summarise(Total_Count=n())






#Question No.12::
gf%>%filter(X9==1)%>%summarise(Total_Count=n())







#Question No.4::
#Data preparation for Zero Trans fat variables for Brand A::
gf$transFat_A<-ifelse(gf$X9==2,0,1)
gf$farmGrown_A<-ifelse(gf$X2==2,0,1)
gf$naturalOils_A<-ifelse(gf$X16==2,0,1)
gf$miniProc_A<-ifelse(gf$X30>4,1,0)


#Exploratory data analysis::
#Farm grown ingredients analysis:
table(gf$farmGrown_A,gf$Target_A)


#Zero gram trans fat analysis::
table(gf$transFat_A,gf$Target_A)


#Analysis with whether the chips are made with natural oils::
table(gf$naturalOils_A,gf$Target_A)


#Analysis with whether the chips are minimally processed::
table(gf$miniProc_A,gf$Target_A)


#Splitting the dataset::
set.seed(200)
index<-sample(nrow(gf),0.70*nrow(gf),replace = F)
train<-gf[index,]
test<-gf[-index,]

#Building the model::
mod1<-glm(Target_A~transFat_A+farmGrown_A+naturalOils_A+miniProc_A,data = train,family = "binomial")
summary(mod1)

#Predicting the probability of the test dataset using the existing model::
pred<-predict(mod1,type = "response",newdata = test)
head(pred)
summary(pred)

#Finding the proportion of the good perception on Brand A according to the initial dataset::
round((One_count$Total_One/Total_Count$Count),3)

test$pred<-ifelse(pred>0.49,1,0)

#The Kappa metric::
kappa2(data.frame(test$Target_A,test$pred))

#the confusion matrix::
confusionMatrix(factor(test$pred),factor(test$Target_A),positive = "1")

#Creating gain chart::
gains(test$Target_A,predict(mod1,type = "response",newdata = test),groups = 10)

test$prob<-predict(mod1,type = "response",newdata = test)

quantile(test$prob,prob=c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,0.99))

#Plotting ROC,the area under the curve::
library(pROC)
g <- roc(Target_A ~ prob, data = test)
plot(g)





#X2---Farm grown ingredients...
#X9---zero gram trans fat...
#X16---Made with natural oils...
#X30---Minimally processed...

#Question No.4,5,6,7,8::
mod2<-glm(Target_A~X2+X9+X16+X30,data = gf,family = "binomial")
summary(mod2)


#-----------------------------------------------------------------------------------------------------


#Building the model on original dataset::
mod3<-glm(Target_A~transFat_A+farmGrown_A+naturalOils_A+miniProc_A,data = train,family = "binomial")
summary(mod3)

#Predicting the probability of the original dataset using the existing model::
pred<-predict(mod3,type = "response",newdata = gf)
head(pred)
summary(pred)

#Finding the proportion of the good perception on Brand A according to the initial dataset::
round((One_count$Total_One/Total_Count$Count),3)

gf$pred<-ifelse(pred>0.49,1,0)

#The Kappa metric::
kappa2(data.frame(gf$Target_A,gf$pred))

#the confusion matrix::
confusionMatrix(factor(gf$pred),factor(gf$Target_A),positive = "1")

#Creating gain chart::
gains(gf$Target_A,predict(mod3,type = "response",newdata = gf),groups = 10)

gf$prob<-predict(mod3,type = "response",newdata = gf)

quantile(gf$prob,prob=c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0))

#Plotting ROC,the area under the curve::
library(pROC)
h <- roc(Target_A ~ prob, data = gf)
plot(h)

#Scoring of the customer as per the perception on Brand A::
targeted<-gf[gf$prob>0.68 & gf$prob<0.81,"Panel.ID"]
targeted<-as.data.frame(targeted)
write.csv(targeted,"targeted.csv",row.names = F)





