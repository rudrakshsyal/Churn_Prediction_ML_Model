library(lubridate)
library(plyr)
library(usdm)
# library(Amelia)
library(caTools)
library(reshape)
library(reshape2)
library(rpart)
library(rpart.plot)
library(party)
library (randomForest)
library (ROCR)
library(C50)
##### 9. LR + RANDOM_FOREST + TREE_BASED (R.E.G.R.E.S.S.I.O.N) ----
LogSummary<-list()
RanImportance<-list()
DecisionTree<-list()
LRModel<-list()
RFModel<-list()
CARTModel<-list()

Comped<-data.frame(Run=integer(),
                   LRAccuracy=numeric(),
                   LRSpecif=numeric(),
                   LRSensit=numeric(),
                   AUC_ROC=numeric(),
                   Precision=numeric(),
                   Recall=numeric(),
                   AUC_PR=numeric(),
                   AUC_SS=numeric(),
                   AUC_L=numeric(),
                   LRAccuracy_train=numeric(),
                   LRSpecif_train=numeric(),
                   LRSensit_train=numeric(),
                   AUC_train=numeric(),
                   Precision_train=numeric(),
                   Recall_train=numeric(),
                   Precision_Recall_train=numeric()
)
## Removing Practice_ID + CITY + THREE ATTRIBUTES (coz they assume a single value) --
# R_101 <- read.csv("../Ray Usage Attributes/To be Fed into Usage Model/Ray_ABS_Appt.csv", stringsAsFactors = F)[,-1]
# R_101 <- as.data.frame(Master_File)
# Old_df <- read.csv("../Ray Usage Attributes/To be Fed into Usage Model/Ray_ABS_Appt.csv", stringsAsFactors = F)[,-1]
# R_101$Incre_Month_2 <- Old_df$Incre_Month.2[match(R_101$ray_practice_id, Old_df$ray_practice_id)]
# R_101$Incre_Month_3 <- Old_df$Incre_Month.3[match(R_101$ray_practice_id, Old_df$ray_practice_id)]
# R_101$Categ_Month1_High <- Old_df$Categ_Month.1.High[match(R_101$ray_practice_id, Old_df$ray_practice_id)]
# R_101$Categ_Month1_Low <- Old_df$Categ_Month.1.Low[match(R_101$ray_practice_id, Old_df$ray_practice_id)]
R_101 <- as.data.frame(temp_F)
R_101$Type[R_101$Type == "Hunt"] <- 0
R_101$Type[R_101$Type == "Upsell"] <- 1

t <- as.data.frame(R_101$ray_practice_id)
R_101$ray_practice_id<-NULL
# R_101$Reference <- R_101$Categ_Month.1 <- R_101$Categ_Month.2 <- R_101$Categ_Month.3 <- R_101$Month.1 <- R_101$Month.2 <-R_101$Sqrt_Month.1 <- R_101$Sqrt_Month.2 <- R_101$Sqrt_Month.3 <- R_101$Average_3Months <- R_101$Days_Used <- R_101$Log1_C_Month.1 <- R_101$Log1_C_Month.2 <- R_101$Log1_C_Month.3 <- R_101$C_Month.2 <- R_101$C_Month.3 <- R_101$Sqrt_Average_3Months <- R_101$D_Month.2 <- R_101$D_Month.3 <- R_101$Max_3Months <- R_101$Categ_Month.1.High <- R_101$Categ_Month.2.High <- R_101$Categ_Month.1.Low <- R_101$Categ_Month.2.Low <- R_101$Month.3 <- NULL
R_101$VN <- NULL
R_101$Reco <- NULL
R_101$Log_ABS_Appt <- NULL
R_101$Categ_Avg_Calendar_High <- NULL
R_101$Review_Reco <- NULL

R_101$ABS_Appt <- NULL
R_101$Review <- NULL
R_101$Categ_ABS_Appt_High_Medium <- R_101$Categ_Reco_High_Medium <- R_101$Categ_Review_High_Medium <- NULL
R_101$Log_Review <- NULL
R_101$ABS_VN <- R_101$ABS_VN_2 <- NULL
R_101$Reference <-R_101$Upsell_Month <-  R_101$Upsell <- R_101$EMR <- R_101$Billing <- R_101$Calendar <- NULL
R_101$Log_Avg_Billing <- NULL
R_101$Log_Avg_Calendar <- NULL
R_101$Log_Avg_EMR <- NULL
R_101$Categ_Avg_EMR_High <- R_101$Categ_Avg_Billing_High <- NULL
R_101$Categ_Avg_EMR <- R_101$Categ_Avg_Billing <- R_101$Categ_Avg_Calendar <- NULL
R_101$Avg_Calendar <- NULL
R_101$Avg_EMR <- NULL
R_101$Avg_Billing <- NULL
R_101$USAGE <- NULL
R_101$Reco_Review <- NULL

# R_101 <- R_101[c("Type", "DQS_Mean","Conv_Prob_Score" , "Churn_Prob_Score")]

R_101$Consult_Fee_Mean <- NULL
R_101$Doc_Photos_Mean <- NULL
R_101$City_Factor <- NULL
R_101$Loc_Factor <- NULL
R_101$Qual_Count_Mean <- NULL
R_101$Dental <- NULL
##
R_101$DQS_Mean <- NULL
R_101$Female_Doc <- NULL
R_101$Doc_Count <- NULL
##
R_101$Org_Mean <- NULL
R_101$Clinic_Photos <- NULL
R_101$Exp_Mean <- NULL

R_101$Churn_Prob_Score <- NULL
R_101$Conv_Prob_Score <- NULL

R_101$GP <- NULL
R_101$Physio <- NULL
R_101$Altmed <- NULL
R_101$Multi_Spec <- NULL
R_101$Surgeon <- NULL
R_101$Pedia <- NULL
R_101$Gynaec <- NULL

# write.csv(R_101, "../CoMatrix-1.csv")

# R_101[,4] <- NULL
# R_101 <- R_101[c("Type", "Log_Avg", "Decay_1Month", "Categ_Avg_High","Categ_Month1_High", "Categ_Month1_Low")]

R_101$Reference <- R_101$Upsell <- NULL

R_101$RevenueInvestPreHunt <- R_101$RevenueBetweenHunt_Upsell <- R_101$DiscountGiven <- R_101$LPO_Discount_Given <- NULL

R_101$Chat_CI <- R_101$Chat_CI/R_101$ChronosIssues
R_101$Call_CI <- R_101$Call_CI/R_101$ChronosIssues 
R_101$Email_CI <- R_101$Email_CI/R_101$ChronosIssues 

R_101$Issue_Product_Ray <- R_101$Issue_Product_Ray/R_101$ChronosIssues
R_101$Issue_Product_Reach <- R_101$Issue_Product_Reach/R_101$ChronosIssues
R_101$Issue_Product_Practo_com <- R_101$Issue_Product_Practo_com/R_101$ChronosIssues

R_101[is.na(R_101)] <- 0

R_101$Chat_CI <- R_101$Call_CI <- R_101$Email_CI <- NULL
R_101$Chat_ATH <- R_101$Call_ATH <- R_101$Email_ATH <- NULL
R_101$Issue_Product_Ray <- R_101$Issue_Product_Reach <- R_101$Issue_Product_Practo_com <- NULL
R_101$Avg_TAT_Hrs_ATH <- NULL
R_101$No_Issues <- NULL
R_101$ChronosIssues <- NULL

R_101 <- R_101[,c(1:6)]
R_101$RevenueInvestPreHunt <- NULL
R_101$RevenueBetweenHunt_Upsell <- NULL
R_101$RevenueInvestInReach <- NULL
R_101$DiscountGiven <- NULL
R_101$LPO_Discount_Given <- NULL

# write.csv(R_101, "../CoMatrix.csv")

temp2<-R_101
temp2$NewDV <- temp2$Type
temp2$Type <- NULL
temp2$NewDV <- as.numeric(temp2$NewDV)
# temp2 <- temp2[,c(14,1:13)]
for(i in 1:10){
  print(i)
  Comped[i,1]<-i
  
  
  #----------- Partial Sampling
  temp2 <- temp2[sample(nrow(temp2)),]
  set.seed(1)
  split <- sample.split(temp2$NewDV, SplitRatio = 0.70)
  
  train<-subset(temp2,split==T)
  test<- subset(temp2,split==F)
  
  LRModel[[i]]<- Logistic <- glm(NewDV ~.,family=binomial(link='logit'),data=train)
  LogSummary[[i]]<- summary(Logistic)
  # write.csv(train,"../train.csv",row.names = F)
  # write.csv(test,"../test.csv",row.names = F)
  #anova(model,test = "Chisq")
  
  l<-as.data.frame(predict(Logistic,newdata=test,type='response'))
  # l<-as.data.frame(predict(Logistic,newdata=temp2,type='response'))
  
  colnames(l)<-'fitted.results'
  
  var<-unlist(mean(temp2$NewDV))
  l$prob<-ifelse(l$fitted.results>var,1,0)
  # l$prob<-ifelse(l$fitted.results>0.5,1,0)
  l$NewDV<-test$NewDV
  # l$Customer<-temp2$Customer
  l$fitted.results<-NULL
  Comped[i,2]<-accuracy <- mean(l$prob == test$NewDV)
  # Comped[i,2]<-accuracy <- mean(l$prob == temp2$Customer)
  
  # l<-count(l,c('prob','NewDV'))
  l <- aggregate(prob+NewDV~prob+NewDV, data = l, FUN = length)
  
  Comped[i,3]<-l[1,3]/(l[1,3]+l[2,3]) #Spec
  
  Comped[i,4]<-l[4,3]/(l[3,3]+l[4,3]) #Sens
  
  Comped$LRAccuracy <- (l[1,3]+l[4,3])/(l[1,3]+l[2,3]+l[3,3]+l[4,3])
  
  # ---------------------- Random Forest
  #   RFModel[[i]] <-RandomForestModel <- suppressWarnings(randomForest(Customer ~., data=train, ntree=70,importance=T))
  #   
  #   l<-as.data.frame(predict(RandomForestModel,newdata=test,type='response'))
  #   colnames(l)<-'fitted.results'
  #   
  #   l$prob<-ifelse(l$fitted.results>var,1,0)
  #   l$Customer<-test$Customer
  #   l$fitted.results<-NULL
  #   Comped[i,5]<-accuracy <- mean(l$prob == test$Customer)
  #   l<-count(l,c('prob','Customer'))
  #   #summary(RandomForestModel)
  #   #RanImportance[[i]]<-importance(RandomForestModel)
  #   Comped[i,6]<-l[1,3]/(l[1,3]+l[2,3])
  #   
  #   Comped[i,7]<-l[4,3]/(l[3,3]+l[4,3])
  
  #   #------ Tree split
  # tree<-rpart(Churn ~., data=train, method='anova',control=rpart.control(minsplit=200))
  #   tree<-rpart(Customer ~., data=train)#,control=rpart.control(minsplit=200)
  #   tree<-prune(tree,cp=tree$cptable[which.min(tree$cptable[,"xerror"]),"CP"])
  #   
  #   l<-as.data.frame(predict(tree,newdata=test))
  #   colnames(l)<-'fitted.results'
  #   
  #   l$prob<-ifelse(l$fitted.results>var,1,0)
  #   l$Customer<-test$Customer
  #   l$fitted.results<-NULL
  #   Comped[i,8]<-accuracy <- mean(l$prob == test$Customer)
  #   l<-count(l,c('prob','Customer'))
  #   
  #   Comped[i,9]<-l[1,3]/(l[1,3]+l[2,3])
  #   
  #   Comped[i,10]<-l[4,3]/(l[3,3]+l[4,3])
  #   
  #   summary(tree)
  #   
  #   DecisionTree[[i]]<- prp(tree,type=2,extra=1)
}
mean(Comped$LRAccuracy)
mean(Comped$RFAccuracy)
mean(Comped$DTAccuracy)
max(Comped$LRAccuracy)

## Finding Correlation & Removing the Attributes with HIGH CORRELATION --
cols.num <- c("has_organization", "has_software", "has_parking", "has_wifi", "has_computer", "has_ac", "has_receptionist")
temp <- R_101[1:7]
temp[cols.num] <- sapply(temp[cols.num], as.numeric)
sapply(temp, class)
write.csv(cor(temp),"../Correlation - v1.csv")
## HIGH Correlation in TWO ATTRIBUTES - has_computer & has_internet, so removing the attribute of has_internet --
R_101$has_internet <- NULL
## Running the model again --
##########################################################################
##### 10. Matching the PROBABILITY SCORES against the Lead List ----
l<-as.data.frame(predict(Logistic,newdata=R_101,type='response'))
colnames(l)<-'fitted.results'
temp_final <- cbind(t, R_101, l)
temp_final$fitted.results <- temp_final$fitted.results*100
write.csv(temp_final, "../Results - Model - 29k.csv")
##########################################################################
