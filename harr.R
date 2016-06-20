setwd("C:/Users/Vinod/Desktop/DSTB/Practical ML/Project")
library(caret)
train=read.csv("pml-training.csv")
miss=as.numeric(lapply(train,function(x){sum(is.na(x))}))
i=which(train$new_window=="yes")
j=which(miss==19216)
ptrain=train[-i,]
ptrain=ptrain[,-j]
for(i in 4:92)
{
  ptrain[,i]=as.numeric(ptrain[,i])
}
i=nzv(ptrain)
ptrain=ptrain[,-i]
intrain=createDataPartition(ptrain$classe,p=0.7,list=F)
tptrain=ptrain[intrain,]
teptrain=ptrain[-intrain,]
m1=train(classe~total_accel_forearm+yaw_forearm+pitch_forearm+roll_forearm+total_accel_dumbbell+yaw_dumbbell+pitch_dumbbell+roll_dumbbell+total_accel_arm+yaw_arm+pitch_arm+roll_arm+total_accel_belt+yaw_belt+pitch_belt+roll_belt,data=tptrain,method="rf")
test_set_acc=sum(predict(m1,teptrain)==teptrain$classe)/dim(teptrain)[1]
test=read.csv("pml-testing.csv")
ptest=test[,-j]
for(t in 4:92)
{
  ptest[,t]=as.numeric(ptest[,t])
}
ptest=ptest[,-i]
predict(m1,ptest)