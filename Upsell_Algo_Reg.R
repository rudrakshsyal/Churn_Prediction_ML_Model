Usage <- read.csv("../../Post_20-12/Master_Filev4_1.csv",stringsAsFactors = F)[,-1]
Feedback <- read.csv("../../Post_20-12/Master_Filev4_2.csv", stringsAsFactors = F)[,-1]
Profile_Orders <- read.csv("../../Post_20-12/Master_Filev4_5.csv",stringsAsFactors = F)[,-1]

Model <- merge(Usage, Feedback, by = c("ray_practice_id", "Type", "Reference", "Upsell", "Upsell_Month"), all.x = T)
Model <- merge(Model, Profile_Orders, by = c("ray_practice_id", "Type", "Reference", "Upsell", "Upsell_Month"), all.x = T)

Model2 <- Model

Model <- Model[,c("ray_practice_id", "Type", "Reference", "Upsell", "Upsell_Month", "Log_Avg_Calendar", "Reco_S.x", "Doc_Count", "Locality_Factor", "City_Factor", "Invest_In_Reach", "Feature_Score", "Rise_Dip_Usage")]

Model2 <- Model2[,c("ray_practice_id", "Type", "Reference", "Upsell", "Upsell_Month", "Log_Avg_Billing", "Log_Avg_EMR", "Review_S", "ABS_Appt.x", "VN", "Exp_Mean", "DQS", "Profile_Quality_Score", "Is_Dental", "Is_AltMed", "Is_GP", "Is_Physio", "Is_Surgeon", "Is_SuperSpec", "Discount_Given", "Invest_In_Practo")]

Model$Type[Model$Type == "Hunt"] <- 0
Model$Type[Model$Type == "Upsell"] <- 1

Model2$Type[Model2$Type == "Hunt"] <- 0
Model2$Type[Model2$Type == "Upsell"] <- 1

Model$Locality_Factor[is.na(Model$Locality_Factor)] <- 1
Model$City_Factor[is.na(Model$City_Factor)] <- 1

Model2$Exp_Mean[is.na(Model2$Exp_Mean)] <- 12.9
Model2$DQS[is.na(Model2$DQS)] <- 81.3


Model <- merge(Model, Model2, by = c("ray_practice_id", "Type", "Reference", "Upsell", "Upsell_Month"), all.x = T)

# write.csv(Model, "C:/Users/Practo/Dropbox/BI Shared Folder/Mohit/Model.csv")

Model$Invest_In_Reach <- Model$Log_Avg_Billing <- Model$Log_Avg_EMR <- Model$Cal_0_Rank <- Model$Review_S <- NULL
Model$CFee_Mean  <- NULL

# write.csv(Model,"../../check2.csv")

##### Prepping for Model Run -----

#check table
table(Model$Type)

#check classes distribution
prop.table(table(Model$Type))

#Logit
R_101 <- as.data.frame(Model)
t <- as.data.frame(R_101$ray_practice_id)
R_101$Reference <- R_101$Upsell <- R_101$Upsell_Month <- R_101$ray_practice_id <- NULL

##### Scaling -----

R_101$Log_Avg_Calendar <- scale(R_101$Log_Avg_Calendar, center = T, scale = T)
R_101$Reco_S.x <- scale(R_101$Reco_S.x, center = T, scale = T)
R_101$Doc_Count <- scale(R_101$Doc_Count, center = T, scale = T)
R_101$Invest_In_Practo <- scale(R_101$Invest_In_Practo, center = T, scale = T)
R_101$Feature_Score <- scale(R_101$Feature_Score, center = T, scale = T)
R_101$Rise_Dip_Usage <- scale(R_101$Rise_Dip_Usage, center = T, scale = T)
R_101$Locality_Factor <- scale(R_101$Locality_Factor, center = T, scale = T)
R_101$City_Factor <- scale(R_101$City_Factor, center = T, scale = T)
R_101$ABS_Appt.x <- scale(R_101$ABS_Appt.x, center = T, scale = T)
R_101$VN <- scale(R_101$VN, center = T, scale = T)
R_101$Exp_Mean <- scale(R_101$Exp_Mean, center = T, scale = T)
R_101$DQS <- scale(R_101$DQS, center = T, scale = T)
R_101$Profile_Quality_Score <- scale(R_101$Profile_Quality_Score, center = T, scale = T)
R_101$Is_Dental <- scale(R_101$Is_Dental, center = T, scale = T)
R_101$Is_AltMed <- scale(R_101$Is_AltMed, center = T, scale = T)
R_101$Is_GP <- scale(R_101$Is_GP, center = T, scale = T)
R_101$Is_Physio <- scale(R_101$Is_Physio, center = T, scale = T)
R_101$Is_Surgeon <- scale(R_101$Is_Surgeon, center = T, scale = T)
R_101$Is_SuperSpec <- scale(R_101$Is_SuperSpec, center = T, scale = T)
R_101$Discount_Given <- scale(R_101$Discount_Given, center = T, scale = T)

##### Logit Run -----

temp2<-R_101
temp2$NewDV <- temp2$Type
temp2$Type <- NULL
temp2$NewDV <- as.numeric(temp2$NewDV)
for(i in 1:10){
  print(i)
  Comped[i,1]<-i
  
  
  #----------- Partial Sampling
  temp2 <- temp2[sample(nrow(temp2)),]
  # set.seed(1)
  split <- sample.split(temp2$NewDV, SplitRatio = 0.70)
  
  train<-subset(temp2,split==T)
  test<- subset(temp2,split==F)
  set.seed(100)
  train <- ovun.sample(NewDV ~ ., data = train, method = "over", N = length(train$NewDV[train$NewDV == 0])*2)$data
  
  LRModel[[i]]<- Logistic <- glm(NewDV ~.,family=binomial(link='logit'),data=train)
  LogSummary[[i]] <- summary(Logistic)
  
  l<-as.data.frame(predict(Logistic,newdata=test,type='response'))
  # l2 <- as.data.frame(predict(Logistic,newdata=train,type='response')) 
  
  # pr2 <- prediction(l2, train$NewDV)
  pr <- prediction(l, test$NewDV)
  prf <- performance(pr, measure = "tpr", x.measure = "fpr")
  # plot(prf)
  auc <- performance(pr, measure = "auc")
  # auc2 <- performance(pr2, measure = "auc")
  auc <- auc@y.values[[1]]
  # auc2 <- auc2@y.values[[1]]
   
  Comped[i,5] <- auc
  # Comped[i,32] <- auc2
  
  # mean(l$fitted.results[l$prob == 0])
  # mean(l$fitted.results[l$prob == 1])
  
  colnames(l)<-'fitted.results'
  # colnames(l2)<-'fitted.results'
  
  # write.csv(l,"../../Post_20-12/l2.csv")
  
  var<-unlist(mean(train$NewDV))
  l$prob<-ifelse(l$fitted.results>var,1,0)
  l$NewDV<-test$NewDV
  l$fitted.results<-NULL
  Comped[i,2]<-accuracy <- mean(l$prob == test$NewDV)
  
#   l2$prob<-ifelse(l2$fitted.results>var,1,0)
#   l2$NewDV<-train$NewDV
#   l2$fitted.results<-NULL
#   Comped[i,29]<-accuracy <- mean(l2$prob == train$NewDV)
  
  # Comped[i,8] <- pr.curve(scores.class0 = l[['prob']], scores.class1 = l[['NewDV']], curve = T)[[2]]
  # Comped[i,35] <- pr.curve(scores.class0 = l2[['prob']], scores.class1 = l2[['NewDV']], curve = T)[[2]]
  
  l <- aggregate(prob+NewDV~prob+NewDV, data = l, FUN = length)
  # l2 <- aggregate(prob+NewDV~prob+NewDV, data = l2, FUN = length)
  
  Comped[i,3]<- l[1,3]/(l[1,3]+l[2,3]) #Spec
  
  Comped[i,4]<- l[4,3]/(l[3,3]+l[4,3]) #Sens
  
  Comped[i,2] <- (l[1,3]+l[4,3])/(l[1,3]+l[2,3]+l[3,3]+l[4,3])
  
  # Comped[i,6] <- l[4,3]/(l[2,3]+l[4,3]) #Precision
    
  # Comped[i,7] <-  l[4,3]/(l[3,3]+l[4,3]) #Recall

  ####
  
  # Comped[i,30]<- l2[1,3]/(l2[1,3]+l2[2,3]) #Spec
  
  # Comped[i,31]<- l2[4,3]/(l[3,3]+l2[4,3]) #Sens
  
  # Comped[i,29] <- (l2[1,3]+l[4,3])/(l2[1,3]+l[2,3]+l2[3,3]+l[4,3])
  
  # Comped[i,33] <- l2[4,3]/(l[2,3]+l2[4,3]) #Precision
  
  # Comped[i,34] <-  l2[4,3]/(l[3,3]+l2[4,3]) #Recall
}
mean(Comped$LRAccuracy)
max(Comped$LRAccuracy)
mean(Comped$LRSensit)
mean(Comped$AUC_ROC)

# write.csv(Comped, "../../Post_20-12/Comped_ROSE.csv")

##### GBM Run -----

LogSummary<-list()
GBModel<-list()
Comped2<-data.frame(Run=integer(),
                   Accuracy=numeric(),
                   GBSpecif=numeric(),
                   GBSensit=numeric(),
                   AUC_ROC=numeric(),
                   Precision=numeric(),
                   Recall=numeric(),
                   AUC_PR=numeric(),
                   AUC_SS=numeric(),
                   AUC_L=numeric(),
                   GBAccuracy_train=numeric(),
                   GBSpecif_train=numeric(),
                   GBSensit_train=numeric(),
                   AUC_train=numeric(),
                   Precision_train=numeric(),
                   Recall_train=numeric(),
                   Precision_Recall_train=numeric()
)
temp2<-R_101
temp2$NewDV <- temp2$Type
temp2$Type <- NULL
temp2$NewDV <- as.numeric(temp2$NewDV)
for(i in 1:10){
  print(i)
  Comped2[i,1]<-i
  
  
  #----------- Partial Sampling
  temp2 <- temp2[sample(nrow(temp2)),]
  set.seed(1)
  split <- sample.split(temp2$NewDV, SplitRatio = 0.70)
  
  train<-subset(temp2,split==T)
  test<- subset(temp2,split==F)
  # set.seed(100)
  train <- ovun.sample(NewDV ~ ., data = train, method = "over", N = length(train$NewDV[train$NewDV == 0])*2)$data
  
  # fitControl <- trainControl(method = "repeatedcv", number = 2, repeats = 2)
  GBModel[[i]]<- GB <- gbm(NewDV~., 
                           distribution = "bernoulli", 
                           data=train, 
                           n.trees = 400,
                           interaction.depth = 10,
                           verbose = F)
  LogSummary[[i]] <- summary(GB)
  
  l <- as.data.frame(predict(GB, newdata = test, n.trees = 400, type='response'))
  # l2 <- as.data.frame(predict(GB, newdata = train, n.trees = 400, type='response'))
  
  # pr2 <- prediction(l2, train$NewDV)
  pr <- prediction(l, test$NewDV)
  prf <- performance(pr, measure = "tpr", x.measure = "fpr")
  # plot(prf)
  auc <- performance(pr, measure = "auc")
  # auc2 <- performance(pr2, measure = "auc")
  auc <- auc@y.values[[1]]
  # auc2 <- auc2@y.values[[1]]
  
  Comped2[i,5] <- auc
  # Comped2[i,14] <- auc2
  
  colnames(l)<-'fitted.results'
  # colnames(l2)<-'fitted.results'
  
  var<-unlist(mean(train$NewDV))
  l$prob<-ifelse(l$fitted.results>var,1,0)
  l$NewDV<-test$NewDV
  l$fitted.results<-NULL
  Comped2[i,2]<-accuracy <- mean(l$prob == test$NewDV)
  
  # l2$prob<-ifelse(l2$fitted.results>var,1,0)
  # l2$NewDV<-train$NewDV
  # l2$fitted.results<-NULL
  # Comped2[i,11]<-accuracy <- mean(l2$prob == train$NewDV)
  
  Comped2[i,8] <- pr.curve(scores.class0 = l[['prob']], scores.class1 = l[['NewDV']], curve = T)[[2]]
  # Comped2[i,17] <- pr.curve(scores.class0 = l2[['prob']], scores.class1 = l2[['NewDV']], curve = T)[[2]]
  
  l <- aggregate(prob+NewDV~prob+NewDV, data = l, FUN = length)
  # l2 <- aggregate(prob+NewDV~prob+NewDV, data = l2, FUN = length)
  
  Comped2[i,3]<- l[1,3]/(l[1,3]+l[2,3]) #Spec
  
  Comped2[i,4]<- l[4,3]/(l[3,3]+l[4,3]) #Sens
  
  Comped2[i,2] <- (l[1,3]+l[4,3])/(l[1,3]+l[2,3]+l[3,3]+l[4,3])
  
  Comped2[i,6] <- l[4,3]/(l[2,3]+l[4,3]) #Precision
  
  Comped2[i,7] <-  l[4,3]/(l[3,3]+l[4,3]) #Recall
  
  ####
  
  # Comped2[i,12]<- l2[1,3]/(l2[1,3]+l2[2,3]) #Spec
  
  # Comped2[i,13]<- l2[4,3]/(l[3,3]+l2[4,3]) #Sens
  
  # Comped2[i,11] <- (l2[1,3]+l[4,3])/(l2[1,3]+l[2,3]+l2[3,3]+l[4,3])
  
  # Comped2[i,15] <- l2[4,3]/(l[2,3]+l2[4,3]) #Precision
  
  # Comped2[i,16] <-  l2[4,3]/(l[3,3]+l2[4,3]) #Recall
}
# mean(Comped2$Accuracy)
# max(Comped2$Accuracy)
# mean(Comped2$GBSensit)
# mean(Comped2$AUC_ROC)

write.csv(Comped2, "../../Post_20-12/Comped_final.csv")

l <- as.data.frame(predict(GB, newdata = temp2, n.trees = 400, type='response'))
colnames(l)<-'fitted.results'
temp_final <- cbind(t, temp2, l)
temp_final$fitted.results <- temp_final$fitted.results*100
write.csv(temp_final, "../../Post_20-12/Results-4566_2.csv")

##### Gradient Boosting Method (GBM) [[[[[ MOHIT ]]]]] -----

temp2<-R_101
temp2$NewDV <- temp2$Type
temp2$Type <- NULL
temp2$NewDV <- as.numeric(temp2$NewDV)

sample_dev_val <- sample(2,nrow(temp2),replace = T,prob = c(0.7,0.3))
dev <- temp2[sample_dev_val==1,]
val <- temp2[sample_dev_val==2,]

var <- mean(dev$NewDV)

fitControl <- trainControl(method = "repeatedcv", number = 2, repeats = 2)

system.time(gbm <- train(as.factor(NewDV) ~ ., data = dev, method = "gbm", trControl = fitControl,verbose = FALSE))

dev$predicted_dv <- predict(gbm, dev,type= "prob")[,2] 
val$predicted_dv <- predict(gbm, val,type= "prob")[,2] 

dev$predicted_dv[dev$predicted_dv>var] <- 1
dev$predicted_dv[dev$predicted_dv<=var] <- 0
val$predicted_dv[val$predicted_dv>var] <- 1
val$predicted_dv[val$predicted_dv<=var] <- 0

#Area under ROC curve
dev_auc <- roc.curve(dev$NewDV, dev$predicted_dv, plotit = F)[[2]]
val_auc <- roc.curve(val$NewDV, val$predicted_dv, plotit = F)[[2]]

#Actual dv=1 split
dev_dv <- nrow(subset(dev,dev$dv==1))
val_dv <- nrow(subset(val,val$dv==1))

#Predicted dv=1 split
dev_predicted_dv <- nrow(subset(dev,dev$predicted_dv==1))
val_predicted_dv <- nrow(subset(val,val$predicted_dv==1))

dev_accuracy <- nrow(subset(dev,dev$predicted_dv==dev$NewDV))/nrow(dev)
val_accuracy <- nrow(subset(val,val$predicted_dv==val$NewDV))/nrow(val)

table(val$predicted_dv,val$NewDV)

summary(gbm)

##### AUC Calculations & Balancing techniques -----

p <- as.data.frame(predict(Logistic, newdata = test, type = 'response'))
pr <- prediction(p, test$NewDV)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)

auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc

#over sampling
data_balanced_over <- ovun.sample(NewDV ~ ., data = train, method = "over", N = 4177*2)$data
table(data_balanced_over$NewDV)

data_balanced_under <- ovun.sample(NewDV ~ ., data = temp2, method = "under", N = 389*2)$data
table(data_balanced_under$NewDV)

data_balanced_both <- ovun.sample(NewDV ~ ., data = temp2, method = "both", p = 0.5, N = 4566)$data
table(data_balanced_both$NewDV)

data.rose <- ROSE(NewDV ~ ., data = temp2)$data
table(data.rose$NewDV)

#build Logit Models
logit.ROSE <- glm(NewDV ~.,family=binomial(link='logit'),data = data.rose)
logit.over <- glm(NewDV ~.,family=binomial(link='logit'),data = data_balanced_over)
logit.under <- glm(NewDV ~.,family=binomial(link='logit'),data = data_balanced_under)
logit.both <- glm(NewDV ~.,family=binomial(link='logit'),data = data_balanced_both)

#make predictions on unseen data
pred.logit.ROSE <- predict(logit.ROSE, newdata = temp2)
pred.logit.over <- predict(logit.over, newdata = temp2)
pred.logit.under <- predict(logit.under, newdata = temp2)
pred.logit.both <- predict(logit.both, newdata = temp2)

#AUC ROSE
p <- predict(Logistic, newdata = test, type = 'response')
pr <- prediction(p, test$NewDV)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)

auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc

#AUC Oversampling
p <- predict(Logistic, newdata = test, type = 'response')
pr <- prediction(p, test$NewDV)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)

auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc

#AUC Undersampling
p <- predict(Logistic, newdata = test, type = 'response')
pr <- prediction(p, test$NewDV)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)

auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc

#AUC Both
p <- predict(Logistic, newdata = test, type = 'response')
pr <- prediction(p, test$NewDV)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)

auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc

##### k-fold Cross Validation & LOOCV -----

fpr <- NULL
fnr <- NULL
k <- 500
pbar <- create_progress_bar('text')
pbar$init(k)
acc <- NULL
set.seed(1)

for(i in 1:k)
{
  # Train-test splitting
  # 95% of samples -> fitting
  # 5% of samples -> testing
  
  data.rose <- ROSE(NewDV ~ ., data = temp2)$data
  table(data.rose$NewDV)
  
  smp_size <- floor(0.70 * nrow(data.rose))
  index <- sample(seq_len(nrow(data.rose)),size=smp_size)
  train <- data.rose[index, ]
  test <- data.rose[-index, ]
  
  # Fitting
  model <- glm(NewDV~.,family=binomial,data=train)
  
  # Predict results
  results_prob <- predict(model, newdata = test, type='response')
  
  # If prob > var then 1, else 0
  var<-unlist(mean(data.rose$NewDV))
  results <- ifelse(results_prob > var,1,0)
  
  # Actual answers
  answers <- test$NewDV
  
  # Accuracy calculation
  misClasificError <- mean(answers != results)
  
  # Collecting results
  acc[i] <- 1-misClasificError
  
  # Confusion matrix
  cm <- confusionMatrix(data=results, reference=answers)
  fpr[i] <- cm$table[2]/(nrow(data.rose)-smp_size)
  fnr[i] <- cm$table[3]/(nrow(data.rose)-smp_size)
  
  pbar$step()
}
cm$overall
cm$table
mean(acc)

par(mfcol=c(1,2)) # ?????

# Histogram of accuracy
hist(acc,xlab='Accuracy',ylab='Freq',col='red',border='blue',density=30)

# Boxplot of accuracy
boxplot(acc,col='green',border='blue',horizontal=T,xlab='Accuracy', main='Accuracy CV')

# Confusion matrix and plots of fpr and fnr
mean(fpr)
mean(fnr)
hist(fpr,xlab='% of fnr',ylab='Freq',main='FPR',
     col='cyan',border='blue',density=30)
hist(fnr,xlab='% of fnr',ylab='Freq',main='FNR',
     col='cyan',border='blue',density=30)

# LOOCV

acc <- NULL

p <- nrow(temp2)
pbar <- create_progress_bar('text')
pbar$init(p)

for(i in 1:p)
{
  # Train-test splitting
  # 4565 samples -> fitting
  # 1 sample -> testing
  train <- temp2[-i,]
  test <- temp2[i,]
  
  # Fitting
  model <- glm(NewDV~.,family=binomial,data=train)
  
  # Predict results
  results_prob <- predict(model,newdata = test,type='response')
  
  # If prob > var then 1, else 0
  var<-unlist(mean(temp2$NewDV))
  results <- ifelse(results_prob > var,1,0)
  
  # Actual answers
  answers <- test$NewDV
  
  # Calculate accuracy
  misClasificError <- mean(answers != results)
  
  # Collecting results
  acc[i] <- 1-misClasificError
#   cm <- confusionMatrix(data=results, reference=answers)
#   fpr[i] <- cm$table[2]/(nrow(temp2)-smp_size)
#   fnr[i] <- cm$table[3]/(nrow(temp2)-smp_size)
  
  pbar$step()
}

# Average accuracy of the model
mean(acc)

# Histogram of the model accuracy
hist(acc,xlab='Accuracy',ylab='Freq',main='Accuracy LOOCV',
     col='cyan',border='blue',density=30)

##### AUC Calculations & Balancing techniques -----

p <- as.data.frame(predict(Logistic, newdata = test, type = 'response'))
pr <- prediction(results_prob, test$NewDV)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)

auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc

##### Final -----

temp <- R_101
temp$ray_practice_id <- NULL
colnames(temp)[1] <- c("NewDV")
temp$predicted <- predict(GB, newdata = temp, n.trees = 400, type='response')
R_101 <- cbind(R_101, temp$predicted)

write.csv(R_101, "../../final.csv")

