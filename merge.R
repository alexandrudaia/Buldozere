##############################################train derivation##########################################
library(randomForest)
library(ggplot2)
setwd("D:/buldozer")
test = read.csv("D:/buldozer/test_set.csv")
train = read.csv("D:/buldozer/train_set.csv")
test=test[,-1]

train$quote_date=as.Date(train$quote_date,"%Y-%m-%d")
test$quote_date=as.Date(test$quote_date, "%Y-%m-%d")

year = as.numeric(format(train$quote_date, format = "%Y"))
month = as.numeric(format(train$quote_date, format = "%m"))

train=cbind(year,month,train)
year = as.numeric(format(test$quote_date, format = "%Y"))
month = as.numeric(format(test$quote_date, format = "%m"))

test=cbind(year,month,test)
train=train[,-which(names(train)=="quote_date")]
test=test[,-which(names(test)=="quote_date")]

train[,c(1:2,4:8)]=sapply(train[,c(1:2,4:8)],as.numeric)

test[,c(1:2,4:8)]=sapply(test[,c(1:2,4:8)],as.numeric)

energies=c()
for( i in 1:nrow(train))
{
  t=table(as.numeric(train[i,c(1:2,4:8)]))
  probabilities=t/7
  e=sum(probabilities^2)
  energies=c(energies,e)
}
train=cbind(energies,train)
energies=c()
for( i in 1:nrow(test))
{
  t=table(as.numeric(test[i,c(1:2,4:8)]))
  probabilities=t/7
  e=sum(probabilities^2)
  energies=c(energies,e)
}
slope=c()
test=cbind(energies,test)
slope=(train$energies-train$quantity)/train$quantity
train=cbind(slope,train)
slope=c()
slope=(test$energies-test$quantity)/test$quantity
test=cbind(slope,test)

write.csv(train,"trainDerived.csv")
write.csv(test,"testDerived.csv")
###################################################bills deriv############################################
#for quantities I put zero where is NA
bill = read.csv("D:/buldozer/bill_of_materials.csv")
for(i in 1:8)
{
  bill[which(is.na(bill[,paste0("quantity_",i)])),paste0("quantity_",i)]=0
}
#i realy coul make energies for this
energQuantity=c()
for( i in 1:nrow(bill))
{
  t=table(as.numeric(bill[i,c(3,5,7,9,11,13)]))
  probabilities=t/6
  e=sum(probabilities^2)
  energQuantity=c(energQuantity,e)
}
bill=cbind(energQuantity,bill)
#make  numeric  the componen id's
for(i in 1:8)
{
  bill[,paste0("component_id_",i)]=sapply(bill[,paste0("component_id_",i)],as.numeric)
}
#where is na  I will put -1
for(i in 1:8)
{
  bill[which(is.na(bill[,paste0("component_id_",i)])),paste0("component_id_",i)]=-1
}

#I will make energy for that  also
energComp=c()
for( i in 1:nrow(bill))
{
  t=table(as.numeric(bill[i,c(3,5,7,9,11,13,15,17)]))
  probabilities=t/8
  e=sum(probabilities^2)
  energComp=c(energComp,e)
}
bill=cbind(energComp,bill)
#energBill=c()
#for( i in 1:nrow(bill))
#{
#  t=table(as.numeric(bill[i,4:19]))
#  probabilities=t/16
#  e=sum(probabilities^2)
#  energBill=c(energBill,e)
#}
#bill=cbind(energBill,bill)
#write.csv(bill,"billfeatures.csv")
###############################merge train derived  with bill features############################
train=merge(train,bill,by="tube_assembly_id",all.x = T,sort=F)
test=merge(test,bill,by="tube_assembly_id",all.x = T,sort=F)
#there is   graphical correlation with this stuff  and  cost
# no I am  goind  o derive this one  with dim
sbq=(train$energComp-train$quantity)/train$quantity
train=cbind(sbq,train)
sbq=(test$energComp-test$quantity)/test$quantity

test=cbind(sbq,test)
badFeatures=c("component_id_4","component_id_5","component_id_6","component_id_7","component_id_8",
"quantity_4","quantity_5","quantity_6","quantity_7","quantity_8","slope","energQuantity")
idxTrain=which(names(train)%in%badFeatures)
idxTest=which(names(test)%in%badFeatures)
train=train[,-idxTrain]
test=test[,-idxTest]
train=train[,-3]
test=test[,-3]
tempTrain=train
tempTest=test

############################################tube#################################################
tube=read.csv("tube.csv",header=T,sep=",")
train=merge(train[,c(2,10)],tube,by="tube_assembly_id",sort=F,all.x=T)
test=merge(test[,1:3],tube,by="tube_assembly_id",sort=F,all.x=T)
test=test[,-c(2:3)]
#make energi  for numerical stuff
train[,9:12]=ifelse(train[,9:12]=="N",0,1)
test[,8:11]=ifelse(test[,8:11]=="N",0,1)
energiesend=c()
for( i in 1:nrow(train))
{
  t=table(as.numeric(train[i,c(9:12,16:17)]))
  probabilities=t/6
  e=sum(probabilities^2)
  energiesend=c(energiesend,e)
}
 
train=cbind(energiesend,train)
energiesend=c()
for( i in 1:nrow(test))
{
  t=table(as.numeric(test[i,c(8:11,15:16)]))
  probabilities=t/6
  e=sum(probabilities^2)
  energiesend=c(energiesend,e)
}
 
test=cbind(energiesend,test)
train=train[,-c(3,17,18)]
test=test[,-c(16,17)]
train=cbind(tempTrain,train[,-2])
test=cbind(tempTest,test[,-2])
 
train=train[,-c(which(names(train)%in%c("end_x_1x","end_x_2x")))]
test=test[,-c(which(names(test)%in%c("end_x_1x","end_x_2x")))]
zdmen=(train$diameter-train$energiesend)/(train$diameter)
train=cbind(train,zdmen)
zdmen=(test$diameter-test$energiesend)/(test$diameter)
test=cbind(test,zdmen)
train=train[,-c(which(names(train)%in%c("num_boss","end_a_2x")))]
test=test[,-c(which(names(test)%in%c("num_boss","end_a_2x")))]
library(Metrics)
##################################################deal with specs################################################
tempTrain=train
tempTest=test
specs=read.csv("specs.csv",sep=",",header=T)
train=merge(train[,c(2,10)],specs,by="tube_assembly_id",sort=F,all.x=T)
test=merge(test[,1:3],specs,by="tube_assembly_id",sort=F,all.x=T)
test=test[,-c(2:3)]
energSpec=c()
for( i in 1:nrow(train))
{
  t=table(as.numeric(train[i,c(3:12)]))
  probabilities=t/10
  e=sum(probabilities^2)
  energSpec=c(energSpec,e)
}
train=cbind(train,energSpec)
energSpec=c()
for( i in 1:nrow(test))
{
  t=table(as.numeric(test[i,c(2:11)]))
  probabilities=t/10
  e=sum(probabilities^2)
  energSpec=c(energSpec,e)
}
test=cbind(test,energSpec)
train=cbind(tempTrain,train[,-c(1,2,12,11,10,9)])
test=cbind(tempTest,test[,-c(1,11,10,9,8)])
tempTrain=train
tempTest=test


######################################## this is making train and  test with energies  and without trash columns








model=randomForest(train$cost ~., data = train[,-2], ntree=30, do.trace=2, importance=TRUE,na.action =na.roughfix)
p=predict(model,train)
rmse(as.numeric(train$cost),p)


finalModel= gbm.fit(x=as.matrix(as.numeric(train[,c(1:10,12:29)])),y=as.matrix(train[,11]),distribution="gaussian",n.trees=3000,interaction.depth=5,shrinkage=0.01,n.minobsinnode=10,verbose=T,var.monotone=c())

finalModel= gbm.fit(x=as.matrix(train[,c(1:8,10:27)]),y=as.matrix(train[,9]),distribution="gaussian",n.trees=3000,interaction.depth=5,shrinkage=0.01,n.minobsinnode=10,verbose=T,var.monotone=c())


#####################################xgb#####################################################################
library(xgboost)
library(Matrix)
tr.mf  <- model.frame(as.formula(paste("cost ~",paste(names(train[,c(1,3:10,11:28)]),collapse = "+"))),train)
tr =Matrix(unlist( train[,c(1,3:10,11:28)]))







