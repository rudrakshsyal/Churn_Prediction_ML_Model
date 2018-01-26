##### Version - 3 Post-Meeting & Small Sample and dv ##### -----

Master_Filev3_1 <- read.csv("../../Post_20-12/Master_File_vFour.csv",stringsAsFactors = F)
Master_Filev3_1$practice_id <- Master_Filev3_1$Hunting_Date <- Master_Filev3_1$HD_Updated <- Master_Filev3_1$Upsell_Date <- Master_Filev3_1$UD_Updated <- NULL
colnames(Master_Filev3_1) <- c("ray_practice_id", "Type", "Reference", "Upsell", "Upsell_Month")

# Amalgamation of Data -----
Master_Filev3_1 <- Master_Filev3_1[c("ray_practice_id", "Type", "Reference", "Upsell", "Upsell_Month")]
Master_Filev3_1 <- Master_Filev3_1[order(Master_Filev3_1$ray_practice_id),]
Dates <- as.data.frame(unique(Month_Mapping$Reference))
colnames(Dates) <- c("date")
Dates <- as.data.frame(Dates[order(Dates$date),])
colnames(Dates) <- c("date")
Master_Filev3_1$Type <- Master_Filev3_1$Reference <- Master_Filev3_1$Upsell <- Master_Filev3_1$Upsell_Month <- NULL

raw <- expand.grid(Master_Filev3_1[['ray_practice_id']],Dates[['date']])
colnames(raw) <- c("ray_practice_id", "date")
raw$conc <- paste0(raw$ray_practice_id, sep="-", raw$date)

# Ray_ABS_Appt
Ray_ABS_Appt$conc <- paste0(Ray_ABS_Appt$ray_practice_id, sep = "-", Ray_ABS_Appt$Reference)
raw$Ray_ABS_Appt <- Ray_ABS_Appt$count[match(raw$conc, Ray_ABS_Appt$conc)]
raw$Ray_ABS_Appt[is.na(raw$Ray_ABS_Appt)] <- 0
# Ray_Appnts
Ray_Appnts$conc <- paste0(Ray_Appnts$ray_practice_id, sep = "-", Ray_Appnts$Reference)
raw$Ray_Appnts <- Ray_Appnts$count[match(raw$conc, Ray_Appnts$conc)]
raw$Ray_Appnts[is.na(raw$Ray_Appnts)] <- 0
# Ray_Appnt_Files
Ray_Appnt_Files$conc <- paste0(Ray_Appnt_Files$ray_practice_id, sep = "-", Ray_Appnt_Files$Reference)
raw$Ray_Appnt_Files <- Ray_Appnt_Files$count[match(raw$conc, Ray_Appnt_Files$conc)]
raw$Ray_Appnt_Files[is.na(raw$Ray_Appnt_Files)] <- 0
# Ray_Imm
Ray_Imm$conc <- paste0(Ray_Imm$ray_practice_id, sep = "-", Ray_Imm$Reference)
raw$Ray_Imm <- Ray_Imm$count[match(raw$conc, Ray_Imm$conc)]
raw$Ray_Imm[is.na(raw$Ray_Imm)] <- 0
# Ray_Invoices
Ray_Invoices$conc <- paste0(Ray_Invoices$ray_practice_id, sep = "-", Ray_Invoices$Reference)
raw$Ray_Invoices <- Ray_Invoices$count[match(raw$conc, Ray_Invoices$conc)]
raw$Ray_Invoices[is.na(raw$Ray_Invoices)] <- 0
# Ray_Pay
Ray_Pay$conc <- paste0(Ray_Pay$ray_practice_id, sep = "-", Ray_Pay$Reference)
raw$Ray_Pay <- Ray_Pay$count[match(raw$conc, Ray_Pay$conc)]
raw$Ray_Pay[is.na(raw$Ray_Pay)] <- 0
# Ray_PG
Ray_PG$conc <- paste0(Ray_PG$ray_practice_id, sep = "-", Ray_PG$Reference)
raw$Ray_PG <- Ray_PG$count[match(raw$conc, Ray_PG$conc)]
raw$Ray_PG[is.na(raw$Ray_PG)] <- 0
# Ray_PF
Ray_PF$conc <- paste0(Ray_PF$ray_practice_id, sep = "-", Ray_PF$Reference)
raw$Ray_PF <- Ray_PF$count[match(raw$conc, Ray_PF$conc)]
raw$Ray_PF[is.na(raw$Ray_PF)] <- 0
# Ray_PVS
Ray_PVS$conc <- paste0(Ray_PVS$ray_practice_id, sep = "-", Ray_PVS$Reference)
raw$Ray_PVS <- Ray_PVS$count[match(raw$conc, Ray_PVS$conc)]
raw$Ray_PVS[is.na(raw$Ray_PVS)] <- 0
# Ray_Prescrip
Ray_Prescrip$conc <- paste0(Ray_Prescrip$ray_practice_id, sep = "-", Ray_Prescrip$Reference)
raw$Ray_Prescrip <- Ray_Prescrip$count[match(raw$conc, Ray_Prescrip$conc)]
raw$Ray_Prescrip[is.na(raw$Ray_Prescrip)] <- 0
# Ray_Soap
Ray_Soap$conc <- paste0(Ray_Soap$ray_practice_id, sep = "-", Ray_Soap$Reference)
raw$Ray_Soap <- Ray_Soap$count[match(raw$conc, Ray_Soap$conc)]
raw$Ray_Soap[is.na(raw$Ray_Soap)] <- 0

# Ray_Calendar
raw$Calendar <- raw$Ray_ABS_Appt + raw$Ray_Appnts

# Ray_EMR
raw$EMR <- raw$Ray_Imm + raw$Ray_PG + raw$Ray_PVS + raw$Ray_Prescrip + raw$Ray_Soap + raw$Ray_Appnt_Files

# Ray_Billing
raw$Billing <- raw$Ray_PF + raw$Ray_Invoices + raw$Ray_Pay

##
raw$conc <- raw$Ray_ABS_Appt <- raw$Ray_Appnts <- raw$Ray_Appnt_Files <- raw$Ray_Imm <- raw$Ray_Invoices <- raw$Ray_Pay <- raw$Ray_PG <- raw$Ray_PF <- raw$Ray_PVS <- raw$Ray_Prescrip <- raw$Ray_Soap <- NULL
##

colnames(raw) <- c("ray_practice_id", "date", "Calendar", "EMR", "Billing")

Master_Filev3_1 <- read.csv("../../Post_20-12/Master_File_vFour.csv", stringsAsFactors = F)
Master_Filev3_1$practice_id <- Master_Filev3_1$Hunting_Date <- Master_Filev3_1$HD_Updated <- Master_Filev3_1$Upsell_Date <- Master_Filev3_1$UD_Updated <- NULL
colnames(Master_Filev3_1) <- c("ray_practice_id", "Type", "Reference", "Upsell", "Upsell_Month")

# IDV Preparation -----
raw$Reference <- Master_Filev3_1$Reference[match(raw$ray_practice_id, Master_Filev3_1$ray_practice_id)]
raw$upsell <- Master_Filev3_1$Upsell[match(raw$ray_practice_id, Master_Filev3_1$ray_practice_id)]
raw <- subset(raw, raw$date >= raw$Reference)
raw <- subset(raw, raw$date <= raw$upsell)
## Appnts ##
temp_Calendar <- aggregate(Calendar~ray_practice_id, data = raw, FUN = sum)
## EMR ##
temp_EMR <- aggregate(EMR~ray_practice_id, data = raw, FUN = sum)
## Billing ##
temp_Billing <- aggregate(Billing~ray_practice_id, data = raw, FUN = sum)

Master_Filev3_1$Calendar <- temp_Calendar$Calendar[match(Master_Filev3_1$ray_practice_id, temp_Calendar$ray_practice_id)]
Master_Filev3_1$EMR <- temp_EMR$EMR[match(Master_Filev3_1$ray_practice_id, temp_EMR$ray_practice_id)]
Master_Filev3_1$Billing <- temp_Billing$Billing[match(Master_Filev3_1$ray_practice_id, temp_Billing$ray_practice_id)]

## IDV-1: Average -----
temp_Calendar <- aggregate(Calendar~ray_practice_id, data = raw, FUN = mean)
Master_Filev3_1$Avg_Calendar <- temp_Calendar$Calendar[match(Master_Filev3_1$ray_practice_id, temp_Calendar$ray_practice_id)]
Master_Filev3_1$Log_Avg_Calendar <- log(Master_Filev3_1$Avg_Calendar + 1)

temp_EMR <- aggregate(EMR~ray_practice_id, data = raw, FUN = mean)
Master_Filev3_1$Avg_EMR <- temp_EMR$EMR[match(Master_Filev3_1$ray_practice_id, temp_EMR$ray_practice_id)]
Master_Filev3_1$Log_Avg_EMR <- log(Master_Filev3_1$Avg_EMR + 1)

temp_Billing <- aggregate(Billing~ray_practice_id, data = raw, FUN = mean)
Master_Filev3_1$Avg_Billing <- temp_Billing$Billing[match(Master_Filev3_1$ray_practice_id, temp_Billing$ray_practice_id)]
Master_Filev3_1$Log_Avg_Billing <- log(Master_Filev3_1$Avg_Billing + 1)

## IDV-2: Categorical Average Variables -----
Master_Filev3_1$Categ_Avg_Calendar <- ifelse(Master_Filev3_1$Avg_Calendar >= 150, "High", ifelse(Master_Filev3_1$Avg_Calendar >= 50, "Medium", "Low"))
Master_Filev3_1$Categ_Avg_EMR <- ifelse(Master_Filev3_1$Avg_EMR >= 50, "High", ifelse(Master_Filev3_1$Avg_EMR >= 10, "Medium", "Low"))
Master_Filev3_1$Categ_Avg_Billing <- ifelse(Master_Filev3_1$Avg_Billing >= 15, "High", ifelse(Master_Filev3_1$Avg_Billing >= 3, "Medium", "Low"))

Master_Filev3_1$Categ_Avg_EMR_High[Master_Filev3_1$Categ_Avg_EMR == "High"] <- 1
Master_Filev3_1$Categ_Avg_EMR_High[is.na(Master_Filev3_1$Categ_Avg_EMR_High)] <- 0

Master_Filev3_1$Categ_Avg_Calendar_High[Master_Filev3_1$Categ_Avg_Calendar == "High"] <- 1
Master_Filev3_1$Categ_Avg_Calendar_High[is.na(Master_Filev3_1$Categ_Avg_Calendar_High)] <- 0

Master_Filev3_1$Categ_Avg_Billing_High[Master_Filev3_1$Categ_Avg_Billing == "High"] <- 1
Master_Filev3_1$Categ_Avg_Billing_High[is.na(Master_Filev3_1$Categ_Avg_Billing_High)] <- 0

Master_Filev3_1$USAGE <- Master_Filev3_1$Avg_Calendar + Master_Filev3_1$Avg_Billing + Master_Filev3_1$Avg_EMR

raw$difference <- raw$upsell - raw$Reference + 1
raw$difference[raw$difference >= 10] <- 10
raw$new_upsell <- raw$Reference + raw$difference - 1

raw <- subset(raw, raw$date >= raw$Reference)
raw <- subset(raw, raw$date <= raw$new_upsell)

raw$upsell <- NULL
raw$dummy_month <- raw$date - raw$Reference + 1
raw$diff_2 <- raw$difference/2

raw$final[raw$dummy_month - raw$diff_2 <= 0.5] <- 1
raw$final[is.na(raw$final)] <- 2


temp <- aggregate(ray_practice_id~ray_practice_id, data = raw, FUN = length)



write.csv(Master_Filev3_1, "../../Post_20-12/Master_Filev4_1.csv")

# Recommendations #####################################################################################

Master_Filev3_1 <- Master_Filev3_1[,c(1:5)]

Ray_Reco <- read.csv("https://www.periscopedata.com/api/practo/chart/csv/8c2c6f54-a096-fc42-58c3-5e0caddbe9ff")

Ray_Reco$Reference <- Month_Mapping$Reference[match(Ray_Reco$date, Month_Mapping$Date)]

raw <- expand.grid(Master_Filev3_1[['ray_practice_id']],Dates[['date']])
colnames(raw) <- c("ray_practice_id", "date")
raw$conc <- paste0(raw$ray_practice_id, sep="-", raw$date)

Ray_Reco$conc <- paste0(Ray_Reco$ray_practice_id, sep = "-", Ray_Reco$Reference)
raw$Ray_Reco <- Ray_Reco$count[match(raw$conc, Ray_Reco$conc)]
raw$Ray_Reco[is.na(raw$Ray_Reco)] <- 0

raw$Reference <- Master_Filev3_1$Reference[match(raw$ray_practice_id, Master_Filev3_1$ray_practice_id)]
raw$upsell <- Master_Filev3_1$Upsell[match(raw$ray_practice_id, Master_Filev3_1$ray_practice_id)]
raw2 <- subset(raw, raw$date <= raw$upsell)
raw <- subset(raw, raw$date >= raw$Reference)
raw <- subset(raw, raw$date <= raw$upsell)

temp_Reco <- aggregate(Ray_Reco~ray_practice_id, data = raw, FUN = mean)
temp_Reco_S <- aggregate(Ray_Reco~ray_practice_id, data = raw2, FUN = mean)

Master_Filev3_1$Reco <- temp_Reco$Ray_Reco[match(Master_Filev3_1$ray_practice_id, temp_Reco$ray_practice_id)]
Master_Filev3_1$Reco_S <- temp_Reco_S$Ray_Reco[match(Master_Filev3_1$ray_practice_id, temp_Reco_S$ray_practice_id)]

Master_Filev3_1$Categ_Reco <- ifelse(Master_Filev3_1$Reco >= 50, "High", ifelse(Master_Filev3_1$Reco >= 5, "Medium", "Low"))
Master_Filev3_1$Categ_Reco_High_Medium[Master_Filev3_1$Categ_Reco == "High" | Master_Filev3_1$Categ_Reco == "Medium"] <- 1
Master_Filev3_1$Categ_Reco_High_Medium[is.na(Master_Filev3_1$Categ_Reco_High_Medium)] <- 0
Master_Filev3_1$Categ_Reco <- NULL

# Master_Filev3_1$Reco_Review <- Master_Filev3_1$Reco/Master_Filev3_1$Review
# Master_Filev3_1$Reco_Review[is.infinite(Master_Filev3_1$Reco_Review)] <- 0
# Master_Filev3_1$Reco_Review[is.na(Master_Filev3_1$Reco_Review)] <- 0

# Reviews #####################################################################################

Ray_Review <- read.csv("https://www.periscopedata.com/api/practo/chart/csv/472b243a-93f8-9677-e75b-c35e487767eb")

Ray_Review$Reference <- Month_Mapping$Reference[match(Ray_Review$date, Month_Mapping$Date)]

raw <- expand.grid(Master_Filev3_1[['ray_practice_id']],Dates[['date']])
colnames(raw) <- c("ray_practice_id", "date")
raw$conc <- paste0(raw$ray_practice_id, sep="-", raw$date)

Ray_Review$conc <- paste0(Ray_Review$ray_practice_id, sep = "-", Ray_Review$Reference)
raw$Ray_Review <- Ray_Review$count[match(raw$conc, Ray_Review$conc)]
raw$Ray_Review[is.na(raw$Ray_Review)] <- 0

raw$Reference <- Master_Filev3_1$Reference[match(raw$ray_practice_id, Master_Filev3_1$ray_practice_id)]
raw$upsell <- Master_Filev3_1$Upsell[match(raw$ray_practice_id, Master_Filev3_1$ray_practice_id)]
raw2 <- subset(raw, raw$date <= raw$upsell)
raw <- subset(raw, raw$date >= raw$Reference)
raw <- subset(raw, raw$date <= raw$upsell)

temp_Review <- aggregate(Ray_Review~ray_practice_id, data = raw, FUN = mean)
temp_Review_S <- aggregate(Ray_Review~ray_practice_id, data = raw2, FUN = mean)

Master_Filev3_1$Review <- temp_Review$Ray_Review[match(Master_Filev3_1$ray_practice_id, temp_Review$ray_practice_id)]
Master_Filev3_1$Review_S <- temp_Review_S$Ray_Review[match(Master_Filev3_1$ray_practice_id, temp_Review_S$ray_practice_id)]

Master_Filev3_1$Categ_Review <- ifelse(Master_Filev3_1$Review >= 50, "High", ifelse(Master_Filev3_1$Review >= 5, "Medium", "Low"))
Master_Filev3_1$Categ_Review_High_Medium[Master_Filev3_1$Categ_Review == "High" | Master_Filev3_1$Categ_Review == "Medium"] <- 1
Master_Filev3_1$Categ_Review_High_Medium[is.na(Master_Filev3_1$Categ_Review_High_Medium)] <- 0
Master_Filev3_1$Categ_Review <- NULL

Master_Filev3_1$Log_Review <- log(Master_Filev3_1$Review + 1)

Master_Filev3_1$Review_Reco <- Master_Filev3_1$Review/(Master_Filev3_1$Reco + 1)
Master_Filev3_1$Review_Reco[is.infinite(Master_Filev3_1$Review_Reco)] <- 0
Master_Filev3_1$Review_Reco[is.na(Master_Filev3_1$Review_Reco)] <- 0

# ABS Appointments #####################################################################################

Ray_ABS_Appt <- read.csv("https://www.periscopedata.com/api/practo/chart/csv/0e2b93f4-0345-0bdb-df9b-b33ea3793296")

Ray_ABS_Appt$Reference <- Month_Mapping$Reference[match(Ray_ABS_Appt$date, Month_Mapping$Date)]

raw <- expand.grid(Master_Filev3_1[['ray_practice_id']],Dates[['date']])
colnames(raw) <- c("ray_practice_id", "date")
raw$conc <- paste0(raw$ray_practice_id, sep="-", raw$date)

Ray_ABS_Appt$conc <- paste0(Ray_ABS_Appt$ray_practice_id, sep = "-", Ray_ABS_Appt$Reference)
raw$Ray_ABS_Appt <- Ray_ABS_Appt$count[match(raw$conc, Ray_ABS_Appt$conc)]
raw$Ray_ABS_Appt[is.na(raw$Ray_ABS_Appt)] <- 0

raw$Reference <- Master_Filev3_1$Reference[match(raw$ray_practice_id, Master_Filev3_1$ray_practice_id)]
raw$upsell <- Master_Filev3_1$Upsell[match(raw$ray_practice_id, Master_Filev3_1$ray_practice_id)]
raw <- subset(raw, raw$date >= raw$Reference)
raw <- subset(raw, raw$date <= raw$upsell)

temp_ABS_Appt <- aggregate(Ray_ABS_Appt~ray_practice_id, data = raw, FUN = mean)

Master_Filev3_1$ABS_Appt <- temp_ABS_Appt$Ray_ABS_Appt[match(Master_Filev3_1$ray_practice_id, temp_ABS_Appt$ray_practice_id)]

Master_Filev3_1$Categ_ABS_Appt <- ifelse(Master_Filev3_1$ABS_Appt >= 50, "High", ifelse(Master_Filev3_1$ABS_Appt >= 5, "Medium", "Low"))
Master_Filev3_1$Categ_ABS_Appt_High_Medium[Master_Filev3_1$Categ_ABS_Appt == "High" | Master_Filev3_1$Categ_ABS_Appt == "Medium"] <- 1
Master_Filev3_1$Categ_ABS_Appt_High_Medium[is.na(Master_Filev3_1$Categ_ABS_Appt_High_Medium)] <- 0
Master_Filev3_1$Categ_ABS_Appt <- NULL

Master_Filev3_1$Log_ABS_Appt <- log(Master_Filev3_1$ABS_Appt + 1)

# mean(Master_Filev3_1$Reco)

# VN Calls #####################################################################################

Ray_VN <- read.csv("https://www.periscopedata.com/api/practo/chart/csv/2c81afc5-9843-6d6e-6c09-642e3a0670eb")

Ray_VN$Reference <- Month_Mapping$Reference[match(Ray_VN$date, Month_Mapping$Date)]

raw <- expand.grid(Master_Filev3_1[['ray_practice_id']],Dates[['date']])
colnames(raw) <- c("ray_practice_id", "date")
raw$conc <- paste0(raw$ray_practice_id, sep="-", raw$date)

Ray_VN$conc <- paste0(Ray_VN$ray_practice_id, sep = "-", Ray_VN$Reference)
raw$Ray_VN <- Ray_VN$count[match(raw$conc, Ray_VN$conc)]
raw$Ray_VN[is.na(raw$Ray_VN)] <- 0

raw$Reference <- Master_Filev3_1$Reference[match(raw$ray_practice_id, Master_Filev3_1$ray_practice_id)]
raw$upsell <- Master_Filev3_1$Upsell[match(raw$ray_practice_id, Master_Filev3_1$ray_practice_id)]
raw <- subset(raw, raw$date >= raw$Reference)
raw <- subset(raw, raw$date <= raw$upsell)

temp_VN <- aggregate(Ray_VN~ray_practice_id, data = raw, FUN = mean)

Master_Filev3_1$VN <- temp_VN$Ray_VN[match(Master_Filev3_1$ray_practice_id, temp_VN$ray_practice_id)]

Master_Filev3_1$ABS_VN <- Master_Filev3_1$ABS_Appt + Master_Filev3_1$VN
Master_Filev3_1$ABS_VN_2 <- Master_Filev3_1$ABS_Appt + (Master_Filev3_1$VN)/2

write.csv(Master_Filev3_1, "../../Post_20-12/Master_Filev4_2.csv")

##### Practo_Profiles -----

PP <- read.csv("../../../Practo Profiles.csv", stringsAsFactors = F)
Practice_Owners <- read.csv("../../Post_20-12/Practice_owners.csv")
Master_Filev3_1 <- Master_Filev3_1[,1:5]

temp <- aggregate(doctor_id~ray_practice_id, data = PP, FUN = length)
Master_Filev3_1$Doc_Count <- temp$doctor_id[match(Master_Filev3_1$ray_practice_id, temp$ray_practice_id)]

temp <- aggregate(consultation_fee~ray_practice_id, data = PP, FUN = mean)
Master_Filev3_1$Consult_Fee_Mean <- temp$consultation_fee[match(Master_Filev3_1$ray_practice_id, temp$ray_practice_id)]

PP$Exp <- 2017 - as.numeric(PP$Experience)
temp <- aggregate(Exp~ray_practice_id, data = PP, FUN = mean)
Master_Filev3_1$Exp_Mean <- temp$Exp[match(Master_Filev3_1$ray_practice_id, temp$ray_practice_id)]

PP$Qual1 <- nchar(PP$qualifications)
PP$sub <- gsub("\\|", "", PP$qualifications)
PP$Qual2 <- nchar(PP$sub)
PP$Qual_Count <- PP$Qual1 - PP$Qual2 + 1

temp <- aggregate(Qual_Count~ray_practice_id, data = PP, FUN = mean)
Master_Filev3_1$Qual_Count_Mean <- temp$Qual_Count[match(Master_Filev3_1$ray_practice_id, temp$ray_practice_id)]

temp <- aggregate(number_of_photos~ray_practice_id, data = PP, FUN = mean)
Master_Filev3_1$Clinic_Photos <- temp$number_of_photos[match(Master_Filev3_1$ray_practice_id, temp$ray_practice_id)]

temp <- aggregate(doctor_photos~ray_practice_id, data = PP, FUN = mean)
Master_Filev3_1$Doc_Photos_Mean <- temp$doctor_photos[match(Master_Filev3_1$ray_practice_id, temp$ray_practice_id)]

temp <- aggregate(organizations~ray_practice_id, data = PP, FUN = mean)
Master_Filev3_1$Org_Mean <- temp$organizations[match(Master_Filev3_1$ray_practice_id, temp$ray_practice_id)]

Master_Filev3_1 <- Master_Filev3_1[c("ray_practice_id", "Type", "Reference", "Upsell", "Upsell_Month", "Doc_Count", "Consult_Fee_Mean", "Exp_Mean", "Qual_Count_Mean", "Clinic_Photos", "Doc_Photos_Mean", "Org_Mean")]

write.csv(Master_Filev3_1, "../../Post_20-12/Master_Filev4_3.csv")

Master_Filev3_5 <- as.data.frame(Master_Filev3_1)
Master_Filev3_5 <- Master_Filev3_5[order(Master_Filev3_5$ray_practice_id),]

DQS <- read.csv("https://www.periscopedata.com/api/practo/chart/csv/2bb62ce8-fe5c-415c-6491-27e841da8815")
DQS$lookup <- Master_Filev3_5$ray_practice_id[match(DQS$ray_practice_id, Master_Filev3_5$ray_practice_id)]
DQS <- subset(DQS, !is.na(DQS$lookup))
temp_dqs <- aggregate(dqs~ray_practice_id, data = DQS, FUN = mean)

Master_Filev3_5$DQS <- temp_dqs$dqs[match(Master_Filev3_5$ray_practice_id, temp_dqs$ray_practice_id)]

Master_Filev3_5$Locality <- PP$locality[match(Master_Filev3_5$ray_practice_id, PP$ray_practice_id)]
Master_Filev3_5$Locality[is.na(Master_Filev3_5$Locality)] <- "Unknown"
Master_Filev3_5$City <- PP$city[match(Master_Filev3_5$ray_practice_id, PP$ray_practice_id)]
Master_Filev3_5$City[is.na(Master_Filev3_5$City)] <- "Unknown"

PP$consultation_fee[PP$consultation_fee <= 50] <- 50
PP$consultation_fee[PP$consultation_fee >= 1500] <- 1500
temp_CFee <- aggregate(consultation_fee~ray_practice_id, data = PP, FUN = mean)
Master_Filev3_5$CFee_Mean <- temp_CFee$consultation_fee[match(Master_Filev3_5$ray_practice_id, temp_CFee$ray_practice_id)]

temp <- Master_Filev3_5[,c(14:16)]
temp <- subset(temp, !is.na(temp$CFee_Mean))
temp_loc <- aggregate(CFee_Mean~Locality, data = temp, FUN = mean)
temp_city <- aggregate(CFee_Mean~City, data = temp, FUN = mean)
temp_loc$City <- temp$City[match(temp_loc$Locality, temp$Locality)]
temp_loc$City_CFee_Mean <- temp_city$CFee_Mean[match(temp_loc$City, temp_city$City)]
temp_loc$Locality_Factor <- temp_loc$CFee_Mean / temp_loc$City_CFee_Mean
temp_loc$India_CFee_Mean <- mean(temp_loc$City_CFee_Mean)
temp_loc$City_Factor <- temp_loc$City_CFee_Mean / temp_loc$India_CFee_Mean

Master_Filev3_5$Locality_Factor <- temp_loc$Locality_Factor[match(Master_Filev3_5$Locality, temp_loc$Locality)]
Master_Filev3_5$City_Factor <- temp_loc$City_Factor[match(Master_Filev3_5$City, temp_loc$City)]

PP$Award1 <- nchar(PP$award)
PP$sub <- gsub("\\|", "", PP$award)
PP$Award2 <- nchar(PP$sub)
PP$Award_Count <- PP$Award1 - PP$Award2 + 1

PP$M1 <- nchar(PP$memberships)
PP$sub <- gsub("\\|", "", PP$memberships)
PP$M2 <- nchar(PP$sub)
PP$Membership_Count <- PP$M1 - PP$M2 + 1

PP$S1 <- nchar(PP$services)
PP$sub <- gsub("\\|", "", PP$services)
PP$S2 <- nchar(PP$sub)
PP$Services_Count <- PP$S1 - PP$S2 + 1

temp_A <- data.frame(PP)
temp_A <- subset(temp_A, temp_A$Award2 > 2)
temp_A <- aggregate(Award_Count~ray_practice_id, data = temp_A, FUN = mean)

Master_Filev3_5$Award_Count <- temp_A$Award_Count[match(Master_Filev3_5$ray_practice_id, temp_A$ray_practice_id)]

temp_A <- data.frame(PP)
temp_A <- subset(temp_A, temp_A$Award2 > 2)
temp_A <- aggregate(Award2~ray_practice_id, data = temp_A, FUN = mean)

Master_Filev3_5$Award_Text_Length <- temp_A$Award2[match(Master_Filev3_5$ray_practice_id, temp_A$ray_practice_id)]

temp_M <- data.frame(PP)
temp_M <- subset(temp_M, temp_M$M2 > 2)
temp_M <- aggregate(Membership_Count~ray_practice_id, data = temp_M, FUN = mean)

Master_Filev3_5$Membership_Count <- temp_M$Membership_Count[match(Master_Filev3_5$ray_practice_id, temp_M$ray_practice_id)]

temp_M <- data.frame(PP)
temp_M <- subset(temp_M, temp_M$M2 > 2)
temp_M <- aggregate(M2~ray_practice_id, data = temp_M, FUN = mean)

Master_Filev3_5$Membership_Text_Length <- temp_M$M2[match(Master_Filev3_5$ray_practice_id, temp_M$ray_practice_id)]

temp_S <- data.frame(PP)
temp_S <- subset(temp_S, temp_S$S2 > 2)
temp_S <- aggregate(Services_Count~ray_practice_id, data = temp_S, FUN = mean)

Master_Filev3_5$Services_Count <- temp_S$Services_Count[match(Master_Filev3_5$ray_practice_id, temp_S$ray_practice_id)]

temp_S <- data.frame(PP)
temp_S <- subset(temp_S, temp_S$S2 > 2)
temp_S <- aggregate(S2~ray_practice_id, data = temp_S, FUN = mean)

Master_Filev3_5$Services_Text_Length <- temp_S$S2[match(Master_Filev3_5$ray_practice_id, temp_S$ray_practice_id)]

write.csv(Master_Filev3_5, "../../Post_20-12/Master_Filev4_4.csv")

Master_Filev3_2 <- read.csv("../../Post_20-12/Master_Filev4_2.csv", stringsAsFactors = F)

Master_Filev3_5$Reco <- Master_Filev3_2$Reco[match(Master_Filev3_5$ray_practice_id, Master_Filev3_2$ray_practice_id)]
Master_Filev3_5$Reco_S <- Master_Filev3_2$Reco_S[match(Master_Filev3_5$ray_practice_id, Master_Filev3_2$ray_practice_id)]
Master_Filev3_5$ABS_Appt <- Master_Filev3_2$ABS_Appt[match(Master_Filev3_5$ray_practice_id, Master_Filev3_2$ray_practice_id)]
Master_Filev3_5$Appt__Reco <- Master_Filev3_5$ABS_Appt/Master_Filev3_5$Reco
Master_Filev3_5$Appt__Reco_S <- Master_Filev3_5$ABS_Appt/Master_Filev3_5$Reco_S

PP$lookup <- Master_Filev3_5$ray_practice_id[match(PP$ray_practice_id, Master_Filev3_5$ray_practice_id)]
PP <- subset(PP, !is.na(PP$lookup))

temp <- aggregate(speciality~ray_practice_id, data = PP, paste, collapse = "|")
PP$Spec_grp <- temp$speciality[match(PP$ray_practice_id, temp$ray_practice_id)]

Master_Filev3_5$Spec_Grp <- PP$Spec_grp[match(Master_Filev3_5$ray_practice_id, PP$ray_practice_id)]

Master_Filev3_5$Is_Dental[grepl('dent',tolower(Master_Filev3_5$Spec_Grp)) | grepl('dont',tolower(Master_Filev3_5$Spec_Grp)) | grepl('oral',tolower(Master_Filev3_5$Spec_Grp))] <- 1
Master_Filev3_5$Is_Dental[is.na(Master_Filev3_5$Is_Dental)] <- 0

Master_Filev3_5$Is_AltMed[grepl('ayurved',tolower(Master_Filev3_5$Spec_Grp)) | grepl('homeop',tolower(Master_Filev3_5$Spec_Grp))] <- 1
Master_Filev3_5$Is_AltMed[is.na(Master_Filev3_5$Is_AltMed)] <- 0

Master_Filev3_5$Is_GP[grepl('family',tolower(Master_Filev3_5$Spec_Grp)) | grepl('practic',tolower(Master_Filev3_5$Spec_Grp)) | grepl('physic',tolower(Master_Filev3_5$Spec_Grp))] <- 1
Master_Filev3_5$Is_GP[is.na(Master_Filev3_5$Is_GP)] <- 0

Master_Filev3_5$Is_Physio[grepl('physio',tolower(Master_Filev3_5$Spec_Grp)) | grepl('therap',tolower(Master_Filev3_5$Spec_Grp)) | grepl('physical',tolower(Master_Filev3_5$Spec_Grp))] <- 1
Master_Filev3_5$Is_Physio[is.na(Master_Filev3_5$Is_Physio)] <- 0

Master_Filev3_5$Is_Surgeon[grepl('surge',tolower(Master_Filev3_5$Spec_Grp))] <- 1
Master_Filev3_5$Is_Surgeon[is.na(Master_Filev3_5$Is_Surgeon)] <- 0

Master_Filev3_5$Is_SuperSpec[grepl('cardio',tolower(Master_Filev3_5$Spec_Grp)) | grepl('neuro',tolower(Master_Filev3_5$Spec_Grp)) | grepl('opthal',tolower(Master_Filev3_5$Spec_Grp))] <- 1
Master_Filev3_5$Is_SuperSpec[is.na(Master_Filev3_5$Is_SuperSpec)] <- 0

write.csv(Master_Filev3_5, "../../Post_20-12/Master_Filev4_5.csv")

##### USAGE MoM Assembling -----

Calendar <- rbind(Ray_ABS_Appt, Ray_Appnts)
Calendar$lookup <- Master_Filev3_1$ray_practice_id[match(Calendar$ray_practice_id, Master_Filev3_1$ray_practice_id)]
Calendar <- subset(Calendar, !is.na(Calendar$lookup))

EMR <- rbind(Ray_Soap, Ray_Prescrip, Ray_PVS, Ray_PG, Ray_Imm, Ray_Appnt_Files, Ray_Appnts)
EMR$lookup <- Master_Filev3_1$ray_practice_id[match(EMR$ray_practice_id, Master_Filev3_1$ray_practice_id)]
EMR <- subset(EMR, !is.na(EMR$lookup))

Billing <- rbind(Ray_PF, Ray_Pay, Ray_Invoices)
Billing$lookup <- Master_Filev3_1$ray_practice_id[match(Billing$ray_practice_id, Master_Filev3_1$ray_practice_id)]
Billing <- subset(Billing, !is.na(Billing$lookup))

Dates <- as.data.frame(unique(Month_Mapping))
Dates$Date <- NULL
colnames(Dates) <- c("date")
Dates <- as.data.frame(Dates[order(Dates$date),])
colnames(Dates) <- c("date")

Usage_2 <- expand.grid(Master_Filev3_1[['ray_practice_id']],Dates[['date']])
colnames(Usage_2) <- c("ray_practice_id", "date")
Usage_2$conc <- paste0(Usage_2$ray_practice_id, sep="-", Usage_2$date)

Usage_2$Calendar <- Calendar$count[match(Usage_2$conc, Calendar$conc)]
Usage_2$Calendar[is.na(Usage_2$Calendar)] <- 0

Usage_2$EMR <- EMR$count[match(Usage_2$conc, EMR$conc)]
Usage_2$EMR[is.na(Usage_2$EMR)] <- 0

Usage_2$Billing <- Billing$count[match(Usage_2$conc, Billing$conc)]
Usage_2$Billing[is.na(Usage_2$Billing)] <- 0

Usage_2$Total_Usage <- Usage_2$Calendar + Usage_2$EMR + Usage_2$Billing
Usage_2 <- Usage_2[order(Usage_2$ray_practice_id, Usage_2$date),]

Usage_2$conc <- NULL
# Usage_2$Calendar <- NULL
# Usage_2$EMR <- NULL
# Usage_2$Billing <- NULL
# Usage_2$Total_Usage <- NULL
Usage_2 <- subset(Usage_2, Usage_2$date >= 1)

temp <- cast(Usage_2, ray_practice_id~date, mean, value = "Total_Usage")

temp$Reference <- Master_Filev3_1$Reference[match(temp$ray_practice_id, Master_Filev3_1$ray_practice_id)]
temp$Upsell <- Master_Filev3_1$Upsell[match(temp$ray_practice_id, Master_Filev3_1$ray_practice_id)]
temp$Upsell_Month <- Master_Filev3_1$Upsell_Month[match(temp$ray_practice_id, Master_Filev3_1$ray_practice_id)]
temp$Type <- Master_Filev3_1$Type[match(temp$ray_practice_id, Master_Filev3_1$ray_practice_id)]

write.csv(temp, "../../Post_20-12/cast.csv")

##### Orders Data Preparation -----

Orders <- read.csv("../../Post_20-12/Order(N).csv")

Orders_Fabric <- subset(Orders, grepl('fabric', tolower(Orders$Master_Plan_2)))
Orders_Fabric$lookup <- Master_Filev3_5$ray_practice_id[match(Orders_Fabric$practice_id, Master_Filev3_5$ray_practice_id)]
Orders_Fabric <- subset(Orders_Fabric, !is.na(Orders_Fabric$lookup))

temp <- aggregate(Revenue~lookup, data = Orders_Fabric, FUN = sum)

Master_Filev3_5$Invest_In_Reach <- temp$Revenue[match(Master_Filev3_5$ray_practice_id, temp$lookup)]
Master_Filev3_5$Invest_In_Reach[is.na(Master_Filev3_5$Invest_In_Reach)] <- 0

Orders$lookup <- Master_Filev3_5$ray_practice_id[match(Orders$practice_id, Master_Filev3_5$ray_practice_id)]
Orders <- subset(Orders, !is.na(Orders$lookup))
Orders_Ray <- subset(Orders, grepl('ray', tolower(Orders$Master_Plan_2)))

temp <- aggregate(amt_discount~lookup, data = Orders_Ray, FUN = sum)

Master_Filev3_5$Discount_Given <- temp$amt_discount[match(Master_Filev3_5$ray_practice_id, temp$lookup)]
Master_Filev3_5$Discount_Given[is.na(Master_Filev3_5$Discount_Given)] <- 0

temp <- aggregate(Revenue~lookup, data = Orders, FUN = sum)

Master_Filev3_5$Invest_In_Practo <- temp$Revenue[match(Master_Filev3_5$ray_practice_id, temp$lookup)]

write.csv(Master_Filev3_5, "../../Post_20-12/Master_Filev4_5.csv")

# Master_Filev3_5 <- Master_Filev3_5[complete.cases(Master_Filev3_5),]

