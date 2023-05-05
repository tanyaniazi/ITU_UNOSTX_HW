install.packages('plyr')
install.packages('matrixStats')

library ('plyr')
library('matrixStats')


df = read_csv('/Users/user/Desktop/dataset.csv')

df <- subset(df, select= -c(RA1, RA2, RB1, RB2, RDA1, RDA2, RDB1, RDB2, RDDR1, RDDR2))  #the WL data is more complete. keeping those instead.
df <- subset(df, select= -c(CONTIN_OTH_DRUG_DON, CONTIN_COCAINE_DON, CONTIN_CIG_DON, CONTIN_ALCOHOL_OLD_DON))   #there are identical columns for History of addiction which is more complete
df <- subset(df, select= -c(CANCER_SITE_DON))

df$TXLNG <- df2$TXLNG  #retrieving the TXLNG column back which had many NULL but it is important to have 
df$TXLNG <- ifelse(is.na(df$TXLNG), 'Unknown', df$TXLNG)

ldply(df, function(c) sum(c =="Unknown")/15768) #to see frequency of Unknown in columns that NA were replaces with Unknown.

df$TXINT <- ifelse(df$TXINT == 'S' | df$TXINT == 'W', 'Y', df$TXINT)
df$TXLIV <- ifelse(df$TXLIV == 'S' | df$TXLIV == 'W', 'Y', df$TXLIV)
df$TXPAN <- ifelse(df$TXPAN == 'S' | df$TXPAN == 'W', 'Y', df$TXPAN)



df <- df[!(df$TXKID == "Unknown"), ]  #removing the rows where there is no Kidney transplant



df$CREAT_TRR[is.na(df$CREAT_TRR)] <- mean(df$CREAT_TRR,na.rm = TRUE)
df$CREAT_DON[is.na(df$CREAT_DON)] <- mean(df$CREAT_DON,na.rm = TRUE)
df$COLD_ISCH_KI[is.na(df$COLD_ISCH_KI)] <- mean(df$COLD_ISCH_KI,na.rm = TRUE)

df <- subset(df, select= -c(DATA_WAITLIST, DATA_TRANSPLANT, DWFG_KI, END_CPRA, END_EPTS, GSTATUS_PA, GTIME_KI, GTIME_PA))
df$DIAG_KI <- ifelse(df$DIAG_KI == 999 | is.na(df$DIAG_KI), "Unknown", df$DIAG_KI)
df$END_CPRA_DETAIL[is.na(df$END_CPRA_DETAIL)] <- mean(df$END_CPRA_DETAIL,na.rm = TRUE)
df$GSTATUS_KI <- ifelse(is.na(df$GSTATUS_KI), "Unknown", df$GSTATUS_KI)

#a lot of critical values of ptime is null in the column KDPI, so it is risky to impute the NA values. 
df <- subset(df, select= -c(KDPI, KDRI_RAO, L_FIN_FLOW_RATE_TX, L_FIN_RESIST_TX, PEAK_PRA)) #so we delete the column, KDRI_RAO has perfect correlation with KDRI_MED
df$KDRI_MED[is.na(df$KDRI_MED)] <- median(df$KDRI_MED,na.rm = TRUE) #since SDV is high, imputing with median
df$LOS[is.na(df$LOS)] <- median(df$LOS,na.rm = TRUE)
df$SGOT_DON[is.na(df$SGOT_DON)] <- median(df$SGOT_DON,na.rm = TRUE)
df$SGPT_DON[is.na(df$SGPT_DON)] <- median(df$SGPT_DON,na.rm = TRUE)
df$TBILI_DON[is.na(df$TBILI_DON)] <- median(df$TBILI_DON,na.rm = TRUE)

df <- subset(df, select= -c(USE_WHICH_PRA, WL_ORG, LIPASE, ADMISSION_DATE, PERM_STATE_TRR, END_OPO_CTR_CODE))
df$CREAT6M[is.na(df$CREAT6M)] <- mean(df$CREAT6M,na.rm = TRUE)
df$CREAT1Y[is.na(df$CREAT1Y)] <- mean(df$CREAT1Y,na.rm = TRUE)
df$ART_RECON <- ifelse(df$ART_RECON == 999 | is.na(df$ART_RECON), "Unknown", df$ART_RECON)

df$AGE_DON_GROUP <- ifelse(is.na(df$AGE_DON_GROUP), "Unknown", df$AGE_DON_GROUP)
df$ABO_MAT <- ifelse(is.na(df$ABO_MAT), "Unknown", df$ABO_MAT)
df$DISTANCE[is.na(df$DISTANCE)] <- median(df$DISTANCE,na.rm = TRUE)
df$TOT_SERUM_ALBUM[is.na(df$TOT_SERUM_ALBUM)] <- mean(df$TOT_SERUM_ALBUM,na.rm = TRUE)
df$AMIS <- ifelse(is.na(df$AMIS), "Unknown", df$AMIS)


df <- subset(df, select= -c(INIT_WGT_KG, VEN_EXT_GRF))

#any days less than 90 is considered not survival and survival for above. 
#df$PTIME <- ifelse(is.na(df$PTIME) | df$PTIME > 90, "Survival", 'Not_Survival')

#temporary
df <- subset(df, select= -c(TX_DATE))
df$ETHCAT <- ifelse(is.na(df$ETHCAT), "Unknown", df$ETHCAT)

df$TXLIV <- ifelse(df$TXLIV == "no", "N", df$TXLIV)
df$TXLNG <- ifelse(df$TXLNG == "no", "N", df$TXLNG)
df$TXHRT <- ifelse(df$TXHRT == "no", "N", df$TXHRT)
df$TXINT <- ifelse(df$TXINT == "no", "N", df$TXINT)
df$TXPAN <- ifelse(df$TXPAN == "no", "N", df$TXPAN)

df$HIV_NAT <- ifelse(df$HIV_NAT == "U", "Unknown", df$HIV_NAT)
df <- subset(df, select= -c(MULTIORG))  #we already know by other columns

df$ANTIHYPE_DON <- ifelse(df$ANTIHYPE_DON == "U", "Unknown", df$ANTIHYPE_DON)
df$ARGININE_DON <- ifelse(df$ARGININE_DON == "U", "Unknown", df$ARGININE_DON)

df$VASODIL_DON <- ifelse(df$VASODIL_DON == "U", "Unknown", df$VASODIL_DON)
df$HIST_HYPERTENS_DON <- ifelse(df$HIST_HYPERTENS_DON == "U" | is.na(df$HIST_HYPERTENS_DON), "Unknown", df$HIST_HYPERTENS_DON)
df$DOBUT_DON_OLD <- ifelse(df$DOBUT_DON_OLD == "U" | is.na(df$DOBUT_DON_OLD), "Unknown", df$DOBUT_DON_OLD)
df$DON_TY <- ifelse(is.na(df$DON_TY), "Unknown", df$DON_TY)


df <- subset(df, select= -c(FUNC_STAT_TRR, STATUS_TRR, FUNC_STAT_TRF, TRTREJ1Y_KI))

write_csv(df, file = "/Users/user/Desktop/final.csv") #exporting the dataset















