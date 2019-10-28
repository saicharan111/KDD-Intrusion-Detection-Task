library(readxl)
library(caret)
library(mlbench)

# df1,df2,df3 stroes the results of C5.0,SVM and RandomForest Respectively.

df1 <- data.frame(Iteration=integer(),
                  Accuracy=double(),
                  FPR=double(), FNR=double(), FalsePos=integer(), FalseNeg=integer())
df2 <- data.frame(Iteration=integer(),
                  Accuracy=double(),
                  FPR=double(),FNR=double(),FalsePos=integer(), FalseNeg=integer())
df3 <- data.frame(Iteration=integer(),
                  Accuracy=double(),
                  FPR=double(),FNR=double(),FalsePos=integer(), FalseNeg=integer())

df4 <- data.frame(Iteration=integer(),
                  Accuracy=double(),
                  FPR=double(),FNR=double(),FalsePos=integer(), FalseNeg=integer())

df5 <- data.frame(Iteration=integer(),
                  Accuracy=double(),
                  FPR=double(),FNR=double(),FalsePos=integer(), FalseNeg=integer())


kddcup <- read.table("/home/charan/Desktop/Rcode/kddcup.data_10_percent", sep = ",")
#kddcup = subset(kddcup, select = -c(V2,V3,V4) )
#kddcup <- read.csv("C:/Users/mohananand/Desktop/kddcup.data_10_percent_corrected", header=FALSE)
View(kddcup)
vec<-rep(0,nrow(kddcup))
#View(kddcup1)
for (i in 1:nrow(kddcup))
{
  if( kddcup[i,42]=="back." || kddcup[i,42]=="land." || kddcup[i,42]=="neptune." || kddcup[i,42]=="pod." || kddcup[i,42]=="smurf." || kddcup[i,42]=="teardrop."){
    #dos attack
    vec[i]=1 #1==DOS
  }
  
  else if(kddcup[i,42]=="buffer_overflow."|| kddcup[i,42]=="loadmodule."|| kddcup[i,42]=="perl."|| kddcup[i,42]=="rootkit."){
    #u2r
    vec[i]=2 #2==u2r
  }
  else if(kddcup[i,42]=="ftp_write."|| kddcup[i,42]=="guess_passwd."|| kddcup[i,42]=="imap."|| kddcup[i,42]=="multihop."|| kddcup[i,42]=="phf."|| kddcup[i,42]=="spy."|| kddcup[i,42]=="warezclient."|| kddcup[i,42]=="warezmaster."){
    #r21
    vec[i]=3
  }
  else if(kddcup[i,42]=="ipsweep."|| kddcup[i,42]=="nmap."|| kddcup[i,42]=="portsweep."|| kddcup[i,42]=="satan."){
    #probe
    vec[i]=4
  }
  else
  {
    #do nthing
    vec[i]=0
  }
}

View(kddcup)
vec2<-vec
vec2<-as.data.frame(vec2)
kcup<-cbind(kddcup,vec2)
kcup$vec2<-as.factor(kcup$vec2)
kcup<-kcup[,c(-42,-2,-3,-4)]
View(kcup)
##############################

#set.seed(4150)
k<-sample(nrow(kcup))
kcup<-kcup[k,]
#View(kddcup)
#print("Selecting train and test data set")
#set.seed(4150)
splitIndex <- createDataPartition(kcup$vec2, p = .75,list = FALSE,times = 1)
trainSplit  <- kcup[splitIndex,]
testSplit  <- kcup[-splitIndex,]
rm(splitIndex )
#print("Done")
#View(trainSplit)

### Common Parametres setting
control <- trainControl(method="repeatedcv", number=3, repeats=3)
#seed <- 4150
metric <- "Accuracy"

### C5.0 ###
fit.rpart <- train(vec2~., data=trainSplit, method="rpart", metric=metric, trControl=control)
### Testing C50 ####
rpartPred<-predict(fit.rpart,testSplit)
a2<-confusionMatrix(rpartPred,testSplit$vec2)
print(sprintf("rpart Results for Iteration: %d", i))
print(sprintf("Accuracy: %f", a2$overall[1]))
print(sprintf("False Positive Ratio: %f", a2$table[1,2]/(a2$table[1,2]+a2$table[2,2])))
print(sprintf("False Negative Ratio: %f", a2$table[2,1]/(a2$table[2,1]+a2$table[1,1])))
print(sprintf("Number of False Positive: %d", a2$table[1,2]))
print(sprintf("Number of False Negatives: %d", a2$table[2,1]))
df2[i,1]<-i
df2[i,2]<-a2$overall[1]
df2[i,3]<-a2$table[1,2]/(a2$table[1,2]+a2$table[2,2])
df2[i,4]<-a2$table[2,1]/(a2$table[2,1]+a2$table[1,1])
df2[i,5]<-a2$table[1,2]
df2[i,6]<-a2$table[2,1]
#SAVING MODEL FOR THE FUTURE USE
saveRDS(fit.rpart, "/home/charan/Desktop/Rcode/final_model_Ids_rpart.rds")
#CHECKINH MODEL IS WORKING PROPERLY OR NOT
super_model <- readRDS("./final_model_Ids_rpart.rds")
c50Pred_new<-predict(super_model,testSplit)