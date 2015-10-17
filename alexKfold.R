train=read.csv('d:/buldozer/generateTrain.csv',header=T,sep=",") 
test=read.csv('d:/buldozer/generateTest.csv',header=T,sep=",")
train=train[,-45]
library(randomForest)
library(caret)
library(Metrics)
 
test=test[,-44]
tempTrain=train
tempTest=test
train$id<-seq.int(nrow(train))
test$id=seq.int(nrow(test))
trainavg=mean(train$cost)
trainsd=sd(train$cost)
cutoff=trainavg+4*trainsd
#features trash or with  high energy(near zero  variance)
idx=c('component_id_7','component_id_8','quantity_8','quantity_7','quantity_6','quantity_5')
train=train[,-which(names(train)%in%idx)]
test=test[,-which(names(test)%in%idx)]
trainNormal=train[train$cost<=cutoff,]
trainOutlier=train[train$cost>cutoff,]
 
train=read.csv("d:/buldozer/ensembleGenTrain.csv")
library(caret)
 #shuffle
cvError=c()
 #newTrain=c()
 for( i in 1:2)
 {
 
 train=train[sample(nrow(train)),]
 #split
 cvSplit <- createDataPartition(train$cost, p = .7,
                                   list = FALSE,
                                   times = 1)
 trainfold=train[cvSplit,]
 testfold=train[-cvSplit,]
 #forest=randomForest(trainfold$cost~.,data=trainfold,ntree=10,do.trace=F)
 l=train(units~trainfold,method="lm",data=trainfold)
 p=predict(forest,testfold)
 
 print(paste0("RMSLE error forcurre fold=",rmsle(p,testfold$cost)))
 cvError=c(cvError,rmsle(p,testfold$cost))
 
 #newTrain=as.data.frame(rbind)
 
 }
 print(paste0("Mean of cross validation error :",mean(cvError)))
 print(paste0("Std of cross validation error :",sd(cvError)))
 
 
