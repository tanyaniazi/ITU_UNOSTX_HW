install.packages('dplyr')
install.packages('readr')
library('dplyr')
library('readr')

dataset = read_csv('/Users/user/Desktop/Ad\ Topics\ in\ IE/project/data/data-kidpan-multiorgan.csv')

###the next lines of code is to remove all the columns that have total null values and nothing else
nulls = c()
for (i in select(dataset, 1:491)){
  nulls <- append(nulls, all(is.na(i)))
}
which(nulls == TRUE)
dataset <- dataset[, - which(nulls == TRUE)]

#removing the ID columns that does not have any effect on the calculations
dataset <- subset(dataset, select=-c(TRR_ID_CODE))

#a reusable function to count the NA in each column.
na_count <- function(dataset){
  sapply(dataset,function(x) sum(is.na(x)))    
}


#in the column ABO, we had different A types and AB types (A1, A2)(A1B, A2B) that needed to be in one group with A and AB:
#dataset$ABO2 <- ifelse(dataset$ABO == 'A1', 'A', ifelse(dataset$ABO == 'A2', 'A', ifelse(dataset$ABO == 'A1B', 'AB', ifelse(dataset$ABO == 'A2B', 'AB', dataset$ABO ))))
#dataset$ABO_DON2 <- ifelse(dataset$ABO_DON == 'A1', 'A', ifelse(dataset$ABO_DON == 'A2', 'A', ifelse(dataset$ABO_DON == 'A1B', 'AB', ifelse(dataset$ABO_DON == 'A2B', 'AB', dataset$ABO_DON ))))

#dropping the ABO column from the dataframe
dataset <- subset(dataset, select=-c(ABO, ABO_DON))

#renaming the new column ABO2 to ABO to keep the dataset with its original var names
#dataset <- dataset %>% rename("ABO" = "ABO2")
#dataset <- dataset %>% rename("ABO_DON" = "ABO_DON2")


#this line of code tells us what is the percentage of missing value in each column
(colMeans(is.na(dataset)))*100
#this one shows which of the columns have missing values more than %95 of the sample
length(which((colMeans(is.na(dataset)))*100 > 95))

#this line of code is for seeing the datatype of columns that have NA values above 0.95 of the sample.
#we are more inclined to remove those columns that have various characters and have more than %95 of the column empty. 
#for example the column CITIZEN_COUNTRY is character and there is the code version of it in another column so we can drop this one.
lapply(dataset[,names(which((colMeans(is.na(dataset)))*100 > 95))], typeof)

#to see which ones are characters and how many of them are there and in which column they are positioned
lapply(dataset[,names(which((colMeans(is.na(dataset)))*100 > 95))], typeof) == 'character'
sum(lapply(dataset[,names(which((colMeans(is.na(dataset)))*100 > 95))], typeof) == 'character')
which((lapply(dataset[,names(which((colMeans(is.na(dataset)))*100 > 95))], typeof) == 'character') == TRUE)

#dropping the columns that have NA values above %95 and have characters as their values
b <- names(which((lapply(dataset[,names(which((colMeans(is.na(dataset)))*100 > 95))], typeof) == 'character') == TRUE))
dataset <- dataset[ , !(names(dataset) %in% b)]

#dropping the columns that have NA values above %95 and have doubles as their values
d <- names(which((colMeans(is.na(dataset)))*100 >= 95))
dataset <- dataset[, !(names(dataset) %in% d)]

#to replace any value above 80 and NA with Unknown
dataset$A1.2 <- ifelse(dataset$A1 > 80 | is.na(dataset$A1) , "Unknown", dataset$A1)
dataset$A2.2 <- ifelse(dataset$A2 > 80 | is.na(dataset$A2) , "Unknown", dataset$A2)
dataset <- subset(dataset, select=-c(A1, A2))
dataset <- dataset %>% rename("A1" = "A1.2")
dataset <- dataset %>% rename("A2" = "A2.2")


#to see what age interval was considered either P or A and apply to the rest of the data
summary(dataset[dataset$AGE_GROUP == 'A' & is.na(dataset$AGE) == FALSE, c('AGE_GROUP', 'AGE')]$AGE)
summary(dataset[dataset$AGE_GROUP == 'P' & is.na(dataset$AGE) == FALSE, c('AGE_GROUP', 'AGE')]$AGE)
dataset$AGE_GROUP.2 <- ifelse(dataset$AGE > 17, "A", ifelse(dataset$AGE <= 17 , "P", dataset$AGE))

#this way we reduced NA values from 8880 to 8742
dataset <- subset(dataset, select=-c(AGE_GROUP))
dataset <- dataset %>% rename("AGE_GROUP" = "AGE_GROUP.2")

dataset$AGE_GROUP <- ifelse(is.na(dataset$AGE_GROUP) , "Unknown", dataset$AGE_GROUP)
dataset <- subset(dataset, select=-c(AGE))

#grouping AGE_DON to P and A just like recipient
dataset$AGE_DON_GROUP <- ifelse(dataset$AGE_DON > 17, "A", ifelse(dataset$AGE_DON <= 17 , "P", dataset$AGE_DON))
dataset <- subset(dataset, select=-c(AGE_DON))
dataset <- subset(dataset, select=-c(AGE_DIAB))
dataset <- subset(dataset, select=-c(ACTIVATE_DATE, ACADEMIC_LEVEL_TCR, ACADEMIC_PRG_TCR, 
                                     ACADEMIC_LEVEL_TRR,ACADEMIC_PRG_TRR,
                                     BMI_TCR))


dataset$AMYLASE <- ifelse(is.na(dataset$AMYLASE) , "Unknown", ifelse(dataset$AMYLASE > 122 , "above_122", dataset$AMYLASE))
dataset$ANTIBODY_TESTED <- ifelse(is.na(dataset$ANTIBODY_TESTED) , "Unknown", dataset$ANTIBODY_TESTED)
dataset$ANTIHYPE_DON <- ifelse(is.na(dataset$ANTIHYPE_DON) , "Unknown", dataset$ANTIHYPE_DON)
dataset$ARGININE_DON <- ifelse(is.na(dataset$ARGININE_DON) , "Unknown", dataset$ARGININE_DON)

dataset$B1 <- ifelse(dataset$B1 > 82 | is.na(dataset$B1) , "Unknown", dataset$B1)
dataset$B2 <- ifelse(dataset$B2 > 82 | is.na(dataset$B2) , "Unknown", dataset$B2)

dataset$ACUTE_REJ_EPI_KI <- ifelse(is.na(dataset$ACUTE_REJ_EPI_KI) , "Unknown", dataset$ACUTE_REJ_EPI_KI)
dataset$BLOOD_INF_DON <- ifelse(is.na(dataset$BLOOD_INF_DON) , "Unknown", dataset$BLOOD_INF_DON)
dataset$BMIS <- ifelse(is.na(dataset$BMIS) , "Unknown", dataset$BMIS)


#filling NULL values with mean of the BMIs
dataset$BMI_CALC[is.na(dataset$BMI_CALC)] <- mean(dataset$BMI_CALC,na.rm = TRUE)
dataset$BMI_DON_CALC[is.na(dataset$BMI_DON_CALC)] <- mean(dataset$BMI_DON_CALC,na.rm = TRUE)
dataset$BUN_DON[is.na(dataset$BUN_DON)] <- mean(dataset$BUN_DON,na.rm = TRUE)

dataset$BW4 <- ifelse(dataset$BW4 > 97 | is.na(dataset$BW4), 'Unknown', dataset$BW4)
dataset$BW6 <- ifelse(dataset$BW6 > 97 | is.na(dataset$BW6), 'Unknown', dataset$BW6)

dataset$C1 <- ifelse(is.na(dataset$C1) | dataset$C1 == 97, "Unknown", ifelse(dataset$C1 > 100 , "above_100", dataset$C1))
dataset$C2 <- ifelse(is.na(dataset$C2) | dataset$C2 == 97, "Unknown", ifelse(dataset$C2 > 100 , "above_100", dataset$C2))
dataset$DR51 <- ifelse(is.na(dataset$DR51) | dataset$DR51 == 998, "Unknown", ifelse(dataset$DR51 < 6 & dataset$DR51 > 0, "1_to_5", dataset$DR51))
dataset$DR51_2 <- ifelse(is.na(dataset$DR51_2) | dataset$DR51_2 == 998, "Unknown", ifelse(dataset$DR51_2 < 6 & dataset$DR51_2 > 0, "1_to_5", dataset$DR51_2))
dataset$DR52 <- ifelse(is.na(dataset$DR52), "Unknown", ifelse(dataset$DR52 < 8 & dataset$DR52 > 0, "1_to_7", dataset$DR52))
dataset$DR52_2 <- ifelse(is.na(dataset$DR52_2), "Unknown", ifelse(dataset$DR52_2 < 8 & dataset$DR52_2 > 0, "1_to_7", dataset$DR52_2))
dataset$DR53 <- ifelse(is.na(dataset$DR53) | dataset$DR53 == 998, "Unknown", ifelse(dataset$DR53 < 4 & dataset$DR53 > 0, "1_to_3", dataset$DR53))
dataset$DR53_2 <- ifelse(is.na(dataset$DR53_2) | dataset$DR53_2 == 998, "Unknown", ifelse(dataset$DR53_2 < 4 & dataset$DR53_2 > 0, "1_to_3", dataset$DR53_2))
dataset$DQ1 <- ifelse(is.na(dataset$DQ1) | dataset$DQ1 == 97, "Unknown", ifelse(dataset$DQ1 > 99 , "above_100", dataset$DQ1))
dataset$DQ2 <- ifelse(is.na(dataset$DQ2) | dataset$DQ2 == 97, "Unknown", ifelse(dataset$DQ2 > 99 , "above_100", dataset$DQ2))
dataset$CANCER_SITE_DON <- ifelse(is.na(dataset$CANCER_SITE_DON) | dataset$CANCER_SITE_DON > 35, "Unknown", ifelse(dataset$CANCER_SITE_DON > 1 & dataset$CANCER_SITE_DON < 36 , "2_to_35", dataset$CANCER_SITE_DON))
dataset$CARDARREST_NEURO <- ifelse(dataset$CARDARREST_NEURO == 'U' | is.na(dataset$CARDARREST_NEURO), "Unknown", dataset$CARDARREST_NEURO)
dataset$CDC_RISK_HIV_DON <- ifelse(dataset$CDC_RISK_HIV_DON == 'U' | is.na(dataset$CDC_RISK_HIV_DON), "Unknown", dataset$CDC_RISK_HIV_DON)
dataset$CITIZENSHIP <- ifelse(dataset$CITIZENSHIP == 998 | is.na(dataset$CITIZENSHIP), "Unknown", dataset$CITIZENSHIP)
dataset$CITIZENSHIP_DON <- ifelse(dataset$CITIZENSHIP_DON == 998 | is.na(dataset$CITIZENSHIP_DON), "Unknown", dataset$CITIZENSHIP_DON)
dataset$CLIN_INFECT_DON <- ifelse(dataset$CLIN_INFECT_DON == 'U' | is.na(dataset$CLIN_INFECT_DON), "Unknown", dataset$CLIN_INFECT_DON)
dataset$CMV_DON <- ifelse(dataset$CMV_DON == 'I' | dataset$CMV_DON == 'U' | dataset$CMV_DON == 'ND' | is.na(dataset$CMV_DON), "Unknown", dataset$CMV_DON)
dataset$CMV_IGG <- ifelse(dataset$CMV_IGG == 'I' | dataset$CMV_IGG == 'U' | dataset$CMV_IGG == 'ND' | is.na(dataset$CMV_IGG), "Unknown", dataset$CMV_IGG)
dataset$CMV_IGM <- ifelse(dataset$CMV_IGM == 'I' | dataset$CMV_IGM == 'U' | dataset$CMV_IGM == 'ND' | is.na(dataset$CMV_IGM), "Unknown", dataset$CMV_IGM)
dataset$CMV_STATUS <- ifelse(dataset$CMV_STATUS == 'I' | dataset$CMV_STATUS == 'U' | dataset$CMV_STATUS == 'ND' | is.na(dataset$CMV_STATUS), "Unknown", dataset$CMV_STATUS)
dataset$COD_CAD_DON <- ifelse(dataset$COD_CAD_DON == 999 | is.na(dataset$COD_CAD_DON), "Unknown", dataset$COD_CAD_DON)

dataset <- subset(dataset, select=-c(COD_KI, COD_OSTXT_KI)) #post_op death reason not related to survival, also specified details for DDonr
dataset <- subset(dataset, select=-c(COMPOSITE_DEATH_DATE)) #Ptime is calculated based on this 

dataset$CONTIN_OTH_DRUG_DON <- ifelse(dataset$CONTIN_OTH_DRUG_DON == 'U' | is.na(dataset$CONTIN_OTH_DRUG_DON), "Unknown", dataset$CONTIN_OTH_DRUG_DON)
dataset$CONTIN_ALCOHOL_OLD_DON <- ifelse(dataset$CONTIN_ALCOHOL_OLD_DON == 'U' | is.na(dataset$CONTIN_ALCOHOL_OLD_DON), "Unknown", dataset$CONTIN_ALCOHOL_OLD_DON)
dataset$CONTIN_CIG_DON <- ifelse(dataset$CONTIN_CIG_DON == 'U' | is.na(dataset$CONTIN_CIG_DON), "Unknown", dataset$CONTIN_CIG_DON)
dataset$CONTIN_COCAINE_DON <- ifelse(dataset$CONTIN_COCAINE_DON == 'U' | is.na(dataset$CONTIN_COCAINE_DON), "Unknown", dataset$CONTIN_COCAINE_DON)

dataset <- subset(dataset, select=-c(CREAT_CLEAR))  #WL health data is not of use, also 14763 NULL
dataset <- subset(dataset, select=-c(CTR_CODE, DIURETICS_DON)) #transplant code is of no importance

dataset$DA1 <- ifelse(is.na(dataset$DA1) | dataset$DA1 == 97, "Unknown", ifelse(dataset$DA1 > 100 , "above_100", dataset$DA1))
dataset$DA2 <- ifelse(is.na(dataset$DA2) | dataset$DA2 == 97, "Unknown", ifelse(dataset$DA2 > 100 , "above_100", dataset$DA2))

dataset$DAYSWAIT_ALLOC[is.na(dataset$DAYSWAIT_ALLOC)] <- mean(dataset$DAYSWAIT_ALLOC,na.rm = TRUE)
dataset$DAYSWAIT_CHRON[is.na(dataset$DAYSWAIT_CHRON)] <- mean(dataset$DAYSWAIT_CHRON,na.rm = TRUE)
dataset <- subset(dataset, select=-c(DAYSWAIT_CHRON_KI, DIALYSIS_DATE, DIAL_DATE, DIET_DON, DISCHARGE_DATE))

dataset$DB1 <- ifelse(is.na(dataset$DB1) | dataset$DB1 == 97, "Unknown", ifelse(dataset$DB1 > 100 , "above_100", dataset$DB1))
dataset$DB2 <- ifelse(is.na(dataset$DB2) | dataset$DB2 == 97, "Unknown", ifelse(dataset$DB2 > 100 , "above_100", dataset$DB2))

dataset$DDAVP_DON <- ifelse(dataset$DDAVP_DON == 'U' | is.na(dataset$DDAVP_DON), "Unknown", dataset$DDAVP_DON)
dataset$DDR1 <- ifelse(is.na(dataset$DDR1) | dataset$DDR1 == 97, "Unknown", ifelse(dataset$DDR1 > 100 , "above_100", dataset$DDR1))
dataset$DDR2 <- ifelse(is.na(dataset$DDR2) | dataset$DDR2 == 97, "Unknown", ifelse(dataset$DDR2 > 100 , "above_100", dataset$DDR2))

dataset$DEATH_CIRCUM_DON <- ifelse(dataset$DEATH_CIRCUM_DON == 997 | is.na(dataset$DEATH_CIRCUM_DON), "Unknown", dataset$DEATH_CIRCUM_DON)
dataset$DEATH_MECH_DON <- ifelse(dataset$DEATH_MECH_DON == 997 | is.na(dataset$DEATH_MECH_DON), "Unknown", dataset$DEATH_MECH_DON)
dataset <- subset(dataset, select=-c(DGN_TCR, DGN_OSTXT_TCR, DIAG_OSTXT_KI))
dataset$DIAB <- ifelse(dataset$DIAB == 998 | is.na(dataset$DIAB), "Unknown", ifelse(dataset$DIAB == 2 | dataset$DIAB == 3 | dataset$DIAB == 4 | dataset$DIAB == 5, "Y", dataset$DIAB))
dataset$DIABETES_DON <- ifelse(dataset$DIABETES_DON == "U" | is.na(dataset$DIABETES_DON), "Unknown", dataset$DIABETES_DON)

dataset$DIAL_TRR <- ifelse(dataset$DIAL_TRR == 'U' | is.na(dataset$DIAL_TRR), "Unknown", dataset$DIAL_TRR)
dataset$DON_RETYP <- ifelse(is.na(dataset$DON_RETYP), "Unknown", dataset$DON_RETYP)

dataset <- subset(dataset, select=-c(DONATION, DONOR_ID, DOPAMINE_DON_OLD, DR1, DR2, DRUGTRT_COPD, DUCT_MGMT))
dataset$DRMIS <- ifelse(is.na(dataset$DRMIS), "Unknown", dataset$DRMIS)

dataset$EBV_IGM_CAD_DON <- ifelse(dataset$EBV_IGM_CAD_DON == 'U' | is.na(dataset$EBV_IGM_CAD_DON), "Unknown", dataset$EBV_IGM_CAD_DON)
dataset$EBV_IGG_CAD_DON <- ifelse(dataset$EBV_IGG_CAD_DON == 'U' | is.na(dataset$EBV_IGG_CAD_DON), "Unknown", dataset$EBV_IGG_CAD_DON)
dataset$EBV_SEROSTATUS <- ifelse(dataset$EBV_SEROSTATUS == 'U' | is.na(dataset$EBV_SEROSTATUS), "Unknown", dataset$EBV_SEROSTATUS)

dataset$ECD_DONOR <- ifelse(is.na(dataset$ECD_DONOR), "Unknown", dataset$ECD_DONOR)
dataset$EDUCATION <- ifelse(dataset$EDUCATION == 998 | is.na(dataset$EDUCATION), "Unknown", dataset$EDUCATION)
dataset$FUNC_STAT_TRF <- ifelse(is.na(dataset$FUNC_STAT_TRF) | dataset$FUNC_STAT_TRF == 998, "Unknown", dataset$FUNC_STAT_TRF)


dataset <- subset(dataset, select=-c(BMI_CALC, END_DATE, OPO_CTR_CODE, CITIZENSHIP, CITIZENSHIP_DON, ETHNICITY))
dataset$END_BMI_CALC[is.na(dataset$END_BMI_CALC)] <- mean(dataset$END_BMI_CALC,na.rm = TRUE)
dataset$END_STAT <- ifelse(is.na(dataset$END_STAT), "Unknown", dataset$END_STAT)
dataset$END_STAT_KI <- ifelse(is.na(dataset$END_STAT_KI), "Unknown", dataset$END_STAT_KI)

dataset$ETHCAT <- ifelse(dataset$ETHCAT == 998 | is.na(dataset$ETHCAT), "Unknown", dataset$ETHCAT)
dataset$ETHCAT_DON <- ifelse(dataset$ETHCAT_DON == 998 | is.na(dataset$ETHCAT_DON), "Unknown", dataset$ETHCAT_DON)

dataset$EXH_PERIT_ACCESS <- ifelse(dataset$EXH_PERIT_ACCESS == 'U' | is.na(dataset$EXH_PERIT_ACCESS), "Unknown", dataset$EXH_PERIT_ACCESS)
dataset$EXH_VASC_ACCESS <- ifelse(dataset$EXH_VASC_ACCESS == 'U' | is.na(dataset$EXH_VASC_ACCESS), "Unknown", dataset$EXH_VASC_ACCESS)
dataset$EXTRACRANIAL_CANCER_DON <- ifelse(dataset$EXTRACRANIAL_CANCER_DON == 'U' | is.na(dataset$EXTRACRANIAL_CANCER_DON), "Unknown", dataset$EXTRACRANIAL_CANCER_DON)

dataset <- subset(dataset, select=-c(FAILDATE_KI, FIRST_WK_DIAL,FUNC_STAT_TRR, FUNC_STAT_TCR))
dataset$FUNC_STAT_TRR <- ifelse(dataset$FUNC_STAT_TRR == 998 | is.na(dataset$FUNC_STAT_TRR), "Unknown", dataset$FUNC_STAT_TRR)
dataset$GENDER_DON <- ifelse(is.na(dataset$GENDER_DON), "Unknown", dataset$GENDER_DON)
dataset <- subset(dataset, select=-c(GFR,GFR_DATE, GRF_PLACEM, GRF_STAT_KI, GRF_STAT_PA, GRF_FAIL_CAUSE_TY_KI))

dataset$HBSAB_DON <- ifelse(dataset$HBSAB_DON == 'U' | is.na(dataset$HBSAB_DON), "Unknown", dataset$HBSAB_DON)
dataset$HBV_CORE_DON <- ifelse(dataset$HBV_CORE_DON == 'U' | is.na(dataset$HBV_CORE_DON), "Unknown", dataset$HBV_CORE_DON)
dataset$HBV_CORE <- ifelse(dataset$HBV_CORE == 'U' | is.na(dataset$HBV_CORE), "Unknown", dataset$HBV_CORE)
dataset$HBV_NAT <- ifelse(dataset$HBV_NAT == 'U' | is.na(dataset$HBV_NAT), "Unknown", dataset$HBV_NAT)
dataset$HBV_NAT_DON <- ifelse(dataset$HBV_NAT_DON == 'U' | is.na(dataset$HBV_NAT_DON), "Unknown", dataset$HBV_NAT_DON)
dataset$HBV_SUR_ANTIGEN_DON <- ifelse(dataset$HBV_SUR_ANTIGEN_DON == 'U' | is.na(dataset$HBV_SUR_ANTIGEN_DON), "Unknown", dataset$HBV_SUR_ANTIGEN_DON)
dataset$HBV_SUR_ANTIGEN <- ifelse(dataset$HBV_SUR_ANTIGEN == 'U' | is.na(dataset$HBV_SUR_ANTIGEN), "Unknown", dataset$HBV_SUR_ANTIGEN)
dataset$HBV_SURF_TOTAL <- ifelse(dataset$HBV_SURF_TOTAL == 'U' | is.na(dataset$HBV_SURF_TOTAL), "Unknown", dataset$HBV_SURF_TOTAL)
dataset$HCV_NAT <- ifelse(dataset$HCV_NAT == 'U' | is.na(dataset$HCV_NAT), "Unknown", dataset$HCV_NAT)
dataset$HCV_NAT_DON <- ifelse(dataset$HCV_NAT_DON == 'U' | is.na(dataset$HCV_NAT_DON), "Unknown", dataset$HCV_NAT_DON)
dataset$HCV_SEROSTATUS <- ifelse(dataset$HCV_SEROSTATUS == 'U' | is.na(dataset$HCV_SEROSTATUS), "Unknown", dataset$HCV_SEROSTATUS)
dataset$HEP_C_ANTI_DON <- ifelse(dataset$HEP_C_ANTI_DON == 'U' | is.na(dataset$HEP_C_ANTI_DON), "Unknown", dataset$HEP_C_ANTI_DON)
dataset$HEPARIN_DON <- ifelse(dataset$HEPARIN_DON == 'U' | is.na(dataset$HEPARIN_DON), "Unknown", dataset$HEPARIN_DON)

dataset <- subset(dataset, select=-c(HGT_CM_CALC, HGT_CM_DON_CALC, HGT_CM_TCR))
dataset$HIST_ALCOHOL_OLD_DON <- ifelse(dataset$HIST_ALCOHOL_OLD_DON == 'U' | is.na(dataset$HIST_ALCOHOL_OLD_DON), "Unknown", dataset$HIST_ALCOHOL_OLD_DON)
dataset$HIST_CANCER_DON <- ifelse(dataset$HIST_CANCER_DON == 'U' | is.na(dataset$HIST_CANCER_DON), "Unknown", dataset$HIST_CANCER_DON)
dataset$HIST_CIG_DON <- ifelse(dataset$HIST_CIG_DON == 'U' | is.na(dataset$HIST_CIG_DON), "Unknown", dataset$HIST_CIG_DON)
dataset$HIST_COCAINE_DON <- ifelse(dataset$HIST_COCAINE_DON == 'U' | is.na(dataset$HIST_COCAINE_DON), "Unknown", dataset$HIST_COCAINE_DON)
dataset$HIST_DIABETES_DON <- ifelse(dataset$HIST_DIABETES_DON == 998 | is.na(dataset$HIST_DIABETES_DON), "Unknown", dataset$HIST_DIABETES_DON)
dataset$HIST_IV_DRUG_OLD_DON <- ifelse(dataset$HIST_IV_DRUG_OLD_DON == 'U' | is.na(dataset$HIST_IV_DRUG_OLD_DON), "Unknown", dataset$HIST_IV_DRUG_OLD_DON)
dataset$HIST_OTH_DRUG_DON <- ifelse(dataset$HIST_OTH_DRUG_DON == 'U' | is.na(dataset$HIST_OTH_DRUG_DON), "Unknown", dataset$HIST_OTH_DRUG_DON)

dataset$HIV_NAT_DON <- ifelse(is.na(dataset$HIV_NAT_DON), "Unknown", dataset$HIV_NAT_DON)
dataset$HIV_NAT <- ifelse(is.na(dataset$HIV_NAT), "Unknown", dataset$HIV_NAT)
dataset$HIV_SEROSTATUS <- ifelse(dataset$HIV_SEROSTATUS == 'U' | is.na(dataset$HIV_SEROSTATUS), "Unknown", dataset$HIV_SEROSTATUS)
dataset$HLAMIS <- ifelse(is.na(dataset$HLAMIS), "Unknown", dataset$HLAMIS)

dataset <- subset(dataset, select= -c(HOME_STATE_DON))
dataset$HTLV1_OLD_DON <- ifelse(dataset$HTLV1_OLD_DON == 'U' | is.na(dataset$HTLV1_OLD_DON), "Unknown", dataset$HTLV1_OLD_DON)
dataset$HTLV2_OLD_DON <- ifelse(dataset$HTLV2_OLD_DON == 'U' | is.na(dataset$HTLV2_OLD_DON), "Unknown", dataset$HTLV2_OLD_DON)
dataset$HYPERTENS_DUR_DON <- ifelse(is.na(dataset$HYPERTENS_DUR_DON), "Unknown", dataset$HYPERTENS_DUR_DON)

dataset <- subset(dataset, select= -c(INIT_DATE, INIT_EPTS, INIT_HGT_CM, INIT_OPO_CTR_CODE, INIT_PEAK_PRA, INIT_STAT, INIT_AGE, INIT_BMI_CALC, INIT_CPRA, INIT_CURRENT_PRA, CURRENT_PRA)) #WL data

dataset$INO_PROCURE_AGENT_1 <- ifelse(is.na(dataset$INO_PROCURE_AGENT_1), "Unknown", dataset$INO_PROCURE_AGENT_1)
dataset$INOTROP_AGENTS <- ifelse(is.na(dataset$INOTROP_AGENTS), "Unknown", dataset$INOTROP_AGENTS)
dataset$INOTROP_SUPPORT_DON <- ifelse(dataset$INOTROP_SUPPORT_DON == 'U' | is.na(dataset$INOTROP_SUPPORT_DON), "Unknown", dataset$INOTROP_SUPPORT_DON)
dataset$INSULIN_DON <- ifelse(dataset$INSULIN_DON == 'U' | is.na(dataset$INSULIN_DON), "Unknown", dataset$INSULIN_DON)
dataset$INTRACRANIAL_CANCER_DON <- ifelse(dataset$INTRACRANIAL_CANCER_DON == 'U' | is.na(dataset$INTRACRANIAL_CANCER_DON), "Unknown", dataset$INTRACRANIAL_CANCER_DON)

dataset <- subset(dataset, select= -c(LISTING_CTR_CODE, LT_ONE_WEEK_DON))
dataset$LT_KI_BIOPSY <- ifelse(is.na(dataset$LT_KI_BIOPSY), "Unknown", dataset$LT_KI_BIOPSY)
dataset$LT_KI_GLOMERUL <- ifelse(is.na(dataset$LT_KI_GLOMERUL), "Unknown", dataset$LT_KI_GLOMERUL)
dataset$MALIG <- ifelse(dataset$MALIG == 'U' | is.na(dataset$MALIG), "Unknown", dataset$MALIG)
dataset$MALIG_TRR <- ifelse(dataset$MALIG_TRR == 'U' | is.na(dataset$MALIG_TRR), "Unknown", dataset$MALIG_TRR)
dataset$MALIG_TCR_KI <- ifelse(dataset$MALIG_TCR_KI == 'U' | is.na(dataset$MALIG_TCR_KI), "Unknown", dataset$MALIG_TCR_KI)
dataset$MALIG_TCR_PA <- ifelse(dataset$MALIG_TCR_PA == 'U' | is.na(dataset$MALIG_TCR_PA), "Unknown", dataset$MALIG_TCR_PA)

dataset <- subset(dataset, select= -c(MAX_KDPI_IMPORT_NON_ZERO_ABDR, MAX_KDPI_IMPORT_ZERO_ABDR,MAX_KDPI_LOCAL_NON_ZERO_ABDR, MAX_KDPI_LOCAL_ZERO_ABDR))
dataset$MED_COND_TRR <- ifelse(is.na(dataset$MED_COND_TRR), "Unknown", dataset$MED_COND_TRR)
dataset$MULTIORG <- ifelse(is.na(dataset$MULTIORG), "Unknown", dataset$MULTIORG)
dataset$NON_HRT_DON <- ifelse(is.na(dataset$NON_HRT_DON), "Unknown", dataset$NON_HRT_DON)
dataset$NPKID <- ifelse(is.na(dataset$NPKID), "Unknown", dataset$NPKID)
dataset$NPPAN <- ifelse(is.na(dataset$NPPAN), "Unknown", dataset$NPPAN)
dataset$NUM_PREV_TX <- ifelse(is.na(dataset$NUM_PREV_TX), "Unknown", dataset$NUM_PREV_TX)
dataset$ON_DIALYSIS <- ifelse(is.na(dataset$ON_DIALYSIS), "Unknown", dataset$ON_DIALYSIS)
dataset$OPER_TECH <- ifelse(is.na(dataset$OPER_TECH), "Unknown", dataset$OPER_TECH)
dataset$ORG_REC_ON <- ifelse(is.na(dataset$ORG_REC_ON), "Unknown", dataset$ORG_REC_ON)
dataset$ORGAN <- ifelse(is.na(dataset$ORGAN), "Unknown", dataset$ORGAN)

dataset$OTHER_HYPERTENS_MED_DON <- ifelse(dataset$OTHER_HYPERTENS_MED_DON == 'U' | is.na(dataset$OTHER_HYPERTENS_MED_DON), "Unknown", dataset$OTHER_HYPERTENS_MED_DON)
dataset$OTHER_INF_DON <- ifelse(is.na(dataset$OTHER_INF_DON), "Unknown", dataset$OTHER_INF_DON)
dataset <- subset(dataset, select= -c(OTHER_INF_OSTXT_DON, PAYBACK, PERM_STATE, PREV_MALIG_TY))
dataset$PERIP_VASC <- ifelse(dataset$PERIP_VASC == 'U' | is.na(dataset$PERIP_VASC), "Unknown", dataset$PERIP_VASC)
dataset$PRE_TX_TXFUS <- ifelse(dataset$PRE_TX_TXFUS == 'U' | is.na(dataset$PRE_TX_TXFUS), "Unknown", dataset$PRE_TX_TXFUS)

dataset$PRETREAT_MED_DON_OLD <- ifelse(dataset$PRETREAT_MED_DON_OLD == 'U' | is.na(dataset$PRETREAT_MED_DON_OLD), "Unknown", dataset$PRETREAT_MED_DON_OLD)
dataset$PREV_KI_TX <- ifelse(is.na(dataset$PREV_KI_TX), "Unknown", dataset$PREV_KI_TX)
dataset$PREV_PA_TX <- ifelse(is.na(dataset$PREV_PA_TX), "Unknown", dataset$PREV_PA_TX)

dataset$PREV_PREG <- ifelse(dataset$PREV_PREG == 998 | is.na(dataset$PREV_PREG), "Unknown", dataset$PREV_PREG)
dataset$PREV_TX <- ifelse(is.na(dataset$PREV_TX), "Unknown", dataset$PREV_TX)

dataset <- subset(dataset, select= -c(PREV_TX_ANY, PREV_TX_ANY_N, PT_CODE))
dataset <- subset(dataset, select= -c(PRI_PAYMENT_TRR_PA, PRI_PAYMENT_TRR_KI, PRI_PAYMENT_TCR_PA, PRI_PAYMENT_TCR_KI))
dataset$PROTEIN_URINE <- ifelse(dataset$PROTEIN_URINE == 'U' | is.na(dataset$PROTEIN_URINE), "Unknown", dataset$PROTEIN_URINE)

dataset$PSTATUS <- ifelse(is.na(dataset$PSTATUS), "Unknown", dataset$PSTATUS)
dataset <- subset(dataset, select= -c(PT_OTH2_OSTXT_DON, PT_OTH1_OSTXT_DON, PT_OTH3_OSTXT_DON, PULM_INF_CONF_DON))

dataset$PT_DIURETICS_DON <- ifelse(dataset$PT_DIURETICS_DON == 'U' | is.na(dataset$PT_DIURETICS_DON), "Unknown", dataset$PT_DIURETICS_DON)
dataset$PT_STEROIDS_DON <- ifelse(dataset$PT_STEROIDS_DON == 'U' | is.na(dataset$PT_STEROIDS_DON), "Unknown", dataset$PT_STEROIDS_DON)
dataset$PT_T3_DON <- ifelse(dataset$PT_T3_DON == 'U' | is.na(dataset$PT_T3_DON), "Unknown", dataset$PT_T3_DON)
dataset$PT_T4_DON <- ifelse(dataset$PT_T4_DON == 'U' | is.na(dataset$PT_T4_DON), "Unknown", dataset$PT_T4_DON)
dataset$PULM_INF_DON <- ifelse(is.na(dataset$PULM_INF_DON), "Unknown", dataset$PULM_INF_DON)
dataset$PUMP_KI <- ifelse(is.na(dataset$PUMP_KI), "Unknown", dataset$PUMP_KI)

dataset$PX_STAT <- ifelse(is.na(dataset$PX_STAT), "Unknown", dataset$PX_STAT)
dataset <- subset(dataset, select= -c(PX_STAT_DATE))

dataset$RA1 <- ifelse(is.na(dataset$RA1) | dataset$RA1 == 97, "Unknown", ifelse(dataset$RA1 > 100 , "above_97", dataset$RA1))
dataset$RA2 <- ifelse(is.na(dataset$RA2) | dataset$RA1 == 97, "Unknown", ifelse(dataset$RA2 > 100 , "above_97", dataset$RA2))
dataset$RB1 <- ifelse(is.na(dataset$RB1) | dataset$RA1 == 97, "Unknown", ifelse(dataset$RB1 > 100 , "above_97", dataset$RB1))
dataset$RB2 <- ifelse(is.na(dataset$RB2) | dataset$RA1 == 97, "Unknown", ifelse(dataset$RB2 > 100 , "above_97", dataset$RB2))
dataset$RDA1 <- ifelse(is.na(dataset$RDA1) | dataset$RA1 == 97, "Unknown", ifelse(dataset$RDA1 > 100 , "above_97", dataset$RDA1))
dataset$RDA2 <- ifelse(is.na(dataset$RDA2) | dataset$RA1 == 97, "Unknown", ifelse(dataset$RDA2 > 100 , "above_97", dataset$RDA2))
dataset$RDB1 <- ifelse(is.na(dataset$RDB1) | dataset$RA1 == 97, "Unknown", ifelse(dataset$RDB1 > 100 , "above_97", dataset$RDB1))
dataset$RDB2 <- ifelse(is.na(dataset$RDB2) | dataset$RA2 == 97, "Unknown", ifelse(dataset$RDB2 > 100 , "above_97", dataset$RDB2))
dataset$RDDR1 <- ifelse(is.na(dataset$RDDR1) | dataset$RDDR1 == 97, "Unknown", ifelse(dataset$RDDR1 > 100 , "above_97", dataset$RDDR1))
dataset$RDDR2 <- ifelse(is.na(dataset$RDDR2) | dataset$RDDR2 == 97, "Unknown", ifelse(dataset$RDDR2 > 100 , "above_97", dataset$RDDR2))
dataset$RDR1 <- ifelse(is.na(dataset$RDR1) | dataset$RDR1 == 97, "Unknown", ifelse(dataset$RDR1 > 100 , "above_97", dataset$RDR1))
dataset$RDR2 <- ifelse(is.na(dataset$RDR2) | dataset$RDR2 == 97, "Unknown", ifelse(dataset$RDR2 > 100 , "above_97", dataset$RDR2))

dataset <- subset(dataset, select= -c(RESUM_MAINT_DIAL_DT, REM_CD, REC_ON_PUMP, REC_ON_ICE, RECOV_OUT_US, RECOVERY_DATE, REFERRAL_DATE, REGION))

dataset$REJTRT_KI <- ifelse(is.na(dataset$REJTRT_KI) | dataset$REJTRT_KI == '.', "Unknown", dataset$REJTRT_KI)
dataset$REJCNF_KI <- ifelse(is.na(dataset$REJCNF_KI), "Unknown", dataset$REJCNF_KI)
dataset$RT_KI_BIOPSY <- ifelse(is.na(dataset$RT_KI_BIOPSY), "Unknown", dataset$RT_KI_BIOPSY)
dataset$RT_KI_GLOMERUL <- ifelse(is.na(dataset$RT_KI_GLOMERUL), "Unknown", dataset$RT_KI_GLOMERUL)

dataset$SERUM_CREAT[is.na(dataset$SERUM_CREAT)] <- mean(dataset$SERUM_CREAT,na.rm = TRUE)
dataset <- subset(dataset, select= -c(SHARE_TY, SSDMF_DEATH_DATE, STATUS_DDR, STATUS_TCR))
dataset$SKIN_CANCER_DON <- ifelse(dataset$SKIN_CANCER_DON == 'U' | is.na(dataset$SKIN_CANCER_DON), "Unknown", dataset$SKIN_CANCER_DON)
dataset$TATTOOS <- ifelse(dataset$TATTOOS == 'U' | is.na(dataset$TATTOOS), "Unknown", dataset$TATTOOS)

dataset$TRTREJ6M_KI <- ifelse(is.na(dataset$TRTREJ6M_KI), "Unknown", dataset$TRTREJ6M_KI)
dataset$TRTREJ1Y_KI <- ifelse(is.na(dataset$TRTREJ1Y_KI), "Unknown", dataset$TRTREJ1Y_KI)
dataset$TX_PROCEDUR_TY_KI <- ifelse(is.na(dataset$TX_PROCEDUR_TY_KI), "Unknown", dataset$TX_PROCEDUR_TY_KI)
dataset$TX_PROCEDUR_TY_PA <- ifelse(is.na(dataset$TX_PROCEDUR_TY_PA), "Unknown", dataset$TX_PROCEDUR_TY_PA)

dataset$TX_TYPE <- ifelse(is.na(dataset$TX_TYPE), "Unknown", dataset$TX_TYPE)
dataset$TXHRT <- ifelse(is.na(dataset$TXHRT), "Unknown", dataset$TXHRT)
dataset$TXINT <- ifelse(is.na(dataset$TXINT), "Unknown", dataset$TXINT)
dataset$TXKID <- ifelse(is.na(dataset$TXKID), "Unknown", dataset$TXKID)
dataset$TXLIV <- ifelse(is.na(dataset$TXLIV), "Unknown", dataset$TXLIV)
dataset$TXPAN <- ifelse(is.na(dataset$TXPAN), "Unknown", dataset$TXPAN)
dataset$URINE_INF_DON <- ifelse(is.na(dataset$URINE_INF_DON), "Unknown", dataset$URINE_INF_DON)
dataset$USE_WHICH_PRA <- ifelse(is.na(dataset$USE_WHICH_PRA), "Unknown", dataset$USE_WHICH_PRA)

dataset <- subset(dataset, select= -c(VAL_DT_DDR, VAL_DT_TCR, VAL_DT_TRR, VASC_MGMT))
dataset$VASODIL_DON <- ifelse(is.na(dataset$VASODIL_DON), "Unknown", dataset$VASODIL_DON)
dataset$VDRL_DON <- ifelse(dataset$VDRL_DON == 'U' | is.na(dataset$VDRL_DON), "Unknown", dataset$VDRL_DON)
dataset$VEN_EXT_GRF <- ifelse(is.na(dataset$VEN_EXT_GRF), "Unknown", dataset$VEN_EXT_GRF)
dataset <- subset(dataset, select= -c(WGT_KG_CALC, WGT_KG_DON_CALC, WGT_KG_TCR, WL_ID_CODE))

dataset$WL_ORG <- ifelse(is.na(dataset$WL_ORG), "Unknown", dataset$WL_ORG)
dataset <- subset(dataset, select= -c(WLLI, WLIN, WLHR, WT_QUAL_DATE, WORK_INCOME_TRR, WORK_INCOME_TCR, ADMIT_DATE_DON))


write_csv(dataset, '/Users/user/Desktop/dataset.csv' )








