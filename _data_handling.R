##### Gathering kids and adults ##### 
   df_raw$`Participant Id` <- as.numeric(paste0(1,substr(df_raw$`Participant Id`,2,3)))
   df_kids <- df_raw[df_raw$Project == "ERITA Adolescent",!grepl("CCNES_A_",colnames(df_raw))]
   df_adults <- df_raw[df_raw$Project == "ERITA Parent",grepl("Participant Id|CCNES_A_",colnames(df_raw))]
   df_raw <- merge(df_kids,df_adults,by="Participant Id")
   
   rm(df_kids,df_adults)
   
##### Conforming data from raw-format to sim_data ##### 
   df <- df_raw[df_raw$`Participant Id` < 400,grepl("Participant Id|Group",colnames(df_raw))]
   
   df$`Gender` <- df_raw$demo1_1_SCREEN
   df$Gender[grepl("pi",tolower(df$Gender))] <- "Female"
   df$Gender[grepl("tra",tolower(df$Gender))] <- "Transgender"
   
   df$`Age` <- df_raw$demo1_2_SCREEN
   
   df$`Nationality` <- df_raw$demo1_4_SCREEN
   df$Nationality[grepl("dan|dk",tolower(df$Nationality))] <- "Danish"
   df$Nationality[grepl("polsk",tolower(df$Nationality))] <- "European"
   
   df$`School` <- df_raw$demo1_5_SCREEN
   df$School[grepl("stx",tolower(df$School))] <- "High-school"
   df$School[grepl("folk",tolower(df$School))] <- "Middle-school"
   df$School[grepl("an",tolower(df$School))] <- "Other"
   df$School[grepl("ikke",tolower(df$School))] <- "No School"
   df$School[grepl("eft",tolower(df$School))] <- "Boarding school"
   
   df$`Parental status` <- df_raw$demo1_6_SCREEN
   df$`Parental status`[grepl("gift",tolower(df$`Parental status`))] <- "Married"
   df$`Parental status`[grepl("skilt",tolower(df$`Parental status`))] <- "Divorced"
   df$`Parental status`[grepl("and",tolower(df$`Parental status`))] <- "Other"

##### DSHI-Y (awaiting, updated) ##### 
df_dshiy <- df_raw[df_raw$`Participant Id` < 400,grepl("Participant Id|DSHICANDI",colnames(df_raw))]
suppressWarnings(df_dshiy[,c(2:ncol(df_dshiy))] <- lapply(df_dshiy[,c(2:ncol(df_dshiy))], as.numeric))
   
df_dshiy_base <- df_dshiy[!is.na(df_dshiy$DSHICANDI_1_SCREEN),grepl("Participant Id|a_spec_SCREEN",colnames(df_dshiy))]
df_dshiy_fu <- df_dshiy[!is.na(df_dshiy$`DSHICANDI follo_1_FU`),grepl("Participant Id|a_spec_FU",colnames(df_dshiy))]

df_dshiy_base$`DSHI-Y (baseline)` <- rowSums(df_dshiy_base[,c(2:ncol(df_dshiy_base))],na.rm=T)
df_dshiy_base <- df_dshiy_base[,c(1,ncol(df_dshiy_base))]
df_dshiy_fu$`DSHI-Y (follow-up)` <- rowSums(df_dshiy_fu[,c(2:ncol(df_dshiy_fu))],na.rm=T)
df_dshiy_fu <- df_dshiy_fu[,c(1,ncol(df_dshiy_fu))]

df <- merge(df,df_dshiy_base,by="Participant Id",all.x=T)
df <- merge(df,df_dshiy_fu,by="Participant Id",all.x=T)

rm(df_dshiy, df_dshiy_base, df_dshiy_fu)

##### Kidscreen-10 (correct) ##### 
df_kidscreen <- df_raw[df_raw$`Participant Id` < 400,grepl("Participant Id|Group|KIDSCREEN",colnames(df_raw))]
df_kidscreen <- df_kidscreen[!grepl("11_",colnames(df_kidscreen))]
df_kidscreen[,c(3:ncol(df_kidscreen))] <- lapply(df_kidscreen[,c(3:ncol(df_kidscreen))], as.numeric)

df$`Kidscreen-10 (baseline)` <- rowSums(df_kidscreen[,grepl("_SCRE",colnames(df_kidscreen))])
df$`Kidscreen-10 (follow-up)` <- rowSums(df_kidscreen[,grepl("_POS",colnames(df_kidscreen))])

kidscreen_conversion <- data.frame(rbind(
         c(10,-4.327),c(11,-3.206), 
         c(12,-2.675),c(13,-2.319), 
         c(14,-2.049),c(15,-1.831),
         c(16,-1.645),c(17,-1.483), 
         c(18,-1.339),c(19,-1.207),
         c(20,-1.085),c(21,-0.971),
         c(22,-0.863),c(23,-0.760),
         c(24,-0.660),c(25,-0.563),
         c(26,-0.468),c(27,-0.374),
         c(28,-0.281),c(29,-0.187),
         c(30,-0.093),c(31,0.002),
         c(32,0.100),c(33,0.199),
         c(34,0.302),c(35,0.409),
         c(36,0.520),c(37,0.637),
         c(38,0.760),c(39,0.891),
         c(40,1.031),c(41,1.183),
         c(42,1.348),c(43,1.529),
         c(44,1.732),c(45,1.961),
         c(46,2.226),c(47,2.545),
         c(48,2.951),c(49,3.533),
         c(50,4.703),c(NA,NA) ))

for(i in 1:nrow(df)){
   suppressWarnings(df$`Kidscreen-10 (baseline)`[i] <- kidscreen_conversion[kidscreen_conversion[,1] == df$`Kidscreen-10 (baseline)`[i],2])
   suppressWarnings(df$`Kidscreen-10 (follow-up)`[i] <- kidscreen_conversion[kidscreen_conversion[,1] == df$`Kidscreen-10 (follow-up)`[i],2])
}
df$`Kidscreen-10 T-values (baseline)` <- 
   (((df$`Kidscreen-10 (baseline)` - 1.2078) / 1.03377) * 10 + 50)
df$`Kidscreen-10 T-values (follow-up)` <- 
   (((df$`Kidscreen-10 (follow-up)` - 1.2078) / 1.03377) * 10 + 50)

rm(df_kidscreen, kidscreen_conversion)

##### DASS-21 (correct) ##### 
df_dass21 <- df_raw[df_raw$`Participant Id` < 400,grepl("Participant Id|Group|DASS",colnames(df_raw))]
df_dass21[,c(3:ncol(df_dass21))] <- lapply(df_dass21[,c(3:ncol(df_dass21))], as.numeric)

df_dass_ba <- df_dass21[,grepl("_SCRE",colnames(df_dass21))]
df_dass_fu <- df_dass21[,grepl("_POS",colnames(df_dass21))]

df$`DASS-21 Depression (baseline)` <- rowSums(df_dass_ba[,grepl("_3_|_5_|_10_|_13_|_16_|_17_|_21_",colnames(df_dass_ba))])
df$`DASS-21 Anxiety (baseline)` <- rowSums(df_dass_ba[,grepl("_2_|_4_|_7_|_9_|_15_|_19_|_20_",colnames(df_dass_ba))])
df$`DASS-21 Stress (baseline)` <- rowSums(df_dass_ba[,grepl("_1_|_6_|_8_|_11_|_12_|_14_|_18_",colnames(df_dass_ba))])
df$`DASS-21 Depression (follow-up)` <- rowSums(df_dass_fu[,grepl("_3_|_5_|_10_|_13_|_16_|_17_|_21_",colnames(df_dass_fu))])
df$`DASS-21 Anxiety (follow-up)` <- rowSums(df_dass_fu[,grepl("_2_|_4_|_7_|_9_|_15_|_19_|_20_",colnames(df_dass_fu))])
df$`DASS-21 Stress (follow-up)` <- rowSums(df_dass_fu[,grepl("_1_|_6_|_8_|_11_|_12_|_14_|_18_",colnames(df_dass_fu))])

rm(df_dass21,df_dass_ba,df_dass_fu)

##### Self-Injury (awaiting, updated) ##### 
df_dshiy <- df_raw[df_raw$`Participant Id` < 400,grepl("Participant Id|Group|DSHICANDI",colnames(df_raw))]
df_dshiy <- df_dshiy[,grepl("Participant Id|follo_",colnames(df_dshiy))]
df_dshiy <- df_dshiy[!is.na(df_dshiy[,2]),]
df_dshiy <- df_dshiy[grepl("Participant Id|a_",colnames(df_dshiy))]
df_dshiy <- df_dshiy[!grepl("spec_",colnames(df_dshiy))]

df_dshiy <- data.frame(cbind(df_dshiy$`Participant Id`,(rowSums(df_dshiy[,c(2:ncol(df_dshiy))],na.rm=T) > 0)*1))
colnames(df_dshiy) <- c("Participant Id","Self-injury")

df <- merge(df,df_dshiy,by="Participant Id",all.x=T)

rm(df_dshiy)

##### Sick days (awaiting, updated) #####
df_sick <- df_raw[df_raw$`Participant Id` < 400,grepl("Participant Id|Sygedage",colnames(df_raw))]

df_sick$Sygedage_1_SCREEN <- gsub("2-3","2.5",df_sick$Sygedage_1_SCREEN)
df_sick$Sygedage_1_SCREEN <- gsub("1-3","2",df_sick$Sygedage_1_SCREEN)
df_sick$Sygedage_1_SCREEN <- gsub("et par stykker","2",df_sick$Sygedage_1_SCREEN)
df_sick$Sygedage_1_SCREEN <- gsub("Jeg er på halv tid, men ikke aftalte sygedage er det 5-7","6",df_sick$Sygedage_1_SCREEN)
df_sick$Sygedage_1_SCREEN <- gsub("3 dage om ugen","12",df_sick$Sygedage_1_SCREEN)
df_sick$Sygedage_1_SCREEN <- gsub("omkring 4, er mit bud","4",df_sick$Sygedage_1_SCREEN)

df_sick$Sygedage_1_POST <- gsub("ingen","0",df_sick$Sygedage_1_POST)
df_sick$Sygedage_1_POST <- gsub("Måske 3 - 4","3.5",df_sick$Sygedage_1_POST)
df_sick$Sygedage_1_POST <- gsub("Har ikke været i skole hele måneden.","30",df_sick$Sygedage_1_POST)
df_sick$Sygedage_1_POST <- gsub("Jeg er sygemeldt","30",df_sick$Sygedage_1_POST)
df_sick$Sygedage_1_POST <- gsub("Ret mange.. måske 7?","7",df_sick$Sygedage_1_POST)
df_sick$Sygedage_1_POST <- gsub("2-3?","2.5",df_sick$Sygedage_1_POST)
df_sick$Sygedage_1_POST <- gsub("7\\?","7",df_sick$Sygedage_1_POST)
df_sick$Sygedage_1_POST <- gsub("2.5\\?","2.5",df_sick$Sygedage_1_POST)
df_sick$Sygedage_1_POST <- gsub("Ingen sygedage, men mange dage hvor jeg kun havde undervisning noget af dagen","0",df_sick$Sygedage_1_POST)

suppressWarnings(df_sick$Sygedage_1_SCREEN <- as.numeric(df_sick$Sygedage_1_SCREEN)/30)
suppressWarnings(df_sick$Sygedage_1_POST <- as.numeric(df_sick$Sygedage_1_POST)/30)

colnames(df_sick)[2:3] <- c("Sick days (baseline)","Sick days (follow-up)")

df <- merge(df,df_sick,by="Participant Id",all.x=T)

rm(df_sick)

##### DERS-16 (correct) ##### 
df_ders16 <- df_raw[df_raw$`Participant Id` < 400,grepl("Participant Id|Group|DERS",colnames(df_raw))]
df_ders16[,c(3:ncol(df_ders16))] <- lapply(df_ders16[,c(3:ncol(df_ders16))], as.numeric)

df$`DERS-16 (baseline)` <- rowSums(df_ders16[,grepl("_SCRE",colnames(df_ders16))])

rm(df_ders16)

##### BSL-supplement (correct) #####
df_bsls <- df_raw[df_raw$`Participant Id` < 400,grepl("Participant Id|Group|BSL-S",colnames(df_raw))]
df_bsls[,c(3:ncol(df_bsls))] <- lapply(df_bsls[,c(3:ncol(df_bsls))], as.numeric)

df_bsls <- df_bsls[,!grepl("_12_",colnames(df_bsls))]

df$`BSL-supplement (baseline)`  <- rowSums(df_bsls[,grepl("_SCRE",colnames(df_bsls))])
df$`BSL-supplement (follow-up)` <- rowSums(df_bsls[,grepl("_POS",colnames(df_bsls))])

rm(df_bsls)

##### CCNES-APP (correct) #####
df_ccnesapp <- df_raw[df_raw$`Participant Id` < 400,grepl("Participant Id|Group|CCNES_APP",colnames(df_raw))]
df_ccnesapp[,c(3:ncol(df_ccnesapp))] <- lapply(df_ccnesapp[,c(3:ncol(df_ccnesapp))], as.numeric)

df$`CCNES-APP Distress reaction (follow-up)` <- rowSums(df_ccnesapp[,grepl("_1a_|_2d_|_3f_|_4d_|_5a_|_6b_|_7f_|_8a_|_9d_",colnames(df_ccnesapp))])/9
df$`CCNES-APP Punitive reactions (follow-up)` <- rowSums(df_ccnesapp[,grepl("_1e_|_2f_|_3b_|_4c_|_5e_|_6c_|_7b_|_8e_|_9c_",colnames(df_ccnesapp))])/9
df$`CCNES-APP Expressive encouragment (follow-up)` <- rowSums(df_ccnesapp[,grepl("_1b_|_2c_|_3e_|_4f_|_5b_|_6d_|_7e_|_8b_|_9e_",colnames(df_ccnesapp))])/9
df$`CCNES-APP Emotion-focused reactions (follow-up)` <- rowSums(df_ccnesapp[,grepl("_1c_|_2b_|_3c_|_4a_|_5c_|_6f_|_7d_|_8c_|_9a_",colnames(df_ccnesapp))])/9
df$`CCNES-APP Problem-focused reactions (follow-up)` <- rowSums(df_ccnesapp[,grepl("_1f_|_2e_|_3a_|_4b_|_5f_|_6e_|_7a_|_8f_|_9b_",colnames(df_ccnesapp))])/9
df$`CCNES-APP Minimization reactions (follow-up)` <- rowSums(df_ccnesapp[,grepl("_1d_|_2a_|_3d_|_4e_|_5d_|_6a_|_7c_|_8d_|_9f_",colnames(df_ccnesapp))])/9

rm(df_ccnesapp)

##### CCNES-A (awaiting, updated) #####
df_ccnesa <- df_raw[df_raw$`Participant Id` < 400,grepl("Participant Id|Group|CCNES_A_",colnames(df_raw))]
df_ccnesa[,c(3:ncol(df_ccnesa))] <- lapply(df_ccnesa[,c(3:ncol(df_ccnesa))], as.numeric)

df_ccnesa_ba <- df_ccnesa[,grepl("_SCRE",colnames(df_ccnesa))]
df_ccnesa_fu <- df_ccnesa[,grepl("_POS",colnames(df_ccnesa))]

df$`CCNES-A Distress reaction (baseline)` <- rowSums(df_ccnesa_ba[,grepl("_1a_|_2d_|_3f_|_4d_|_5a_|_6b_|_7f_|_8a_|_9d_",colnames(df_ccnesa_ba))])/9
df$`CCNES-A Punitive reactions (baseline)` <- rowSums(df_ccnesa_ba[,grepl("_1e_|_2f_|_3b_|_4c_|_5e_|_6c_|_7b_|_8e_|_9c_",colnames(df_ccnesa_ba))])/9
df$`CCNES-A Expressive encouragment (baseline)` <- rowSums(df_ccnesa_ba[,grepl("_1b_|_2c_|_3e_|_4f_|_5b_|_6d_|_7e_|_8b_|_9e_",colnames(df_ccnesa_ba))])/9
df$`CCNES-A Emotion-focused reactions (baseline)` <- rowSums(df_ccnesa_ba[,grepl("_1c_|_2b_|_3c_|_4a_|_5c_|_6f_|_7d_|_8c_|_9a_",colnames(df_ccnesa_ba))])/9
df$`CCNES-A Problem-focused reactions (baseline)` <- rowSums(df_ccnesa_ba[,grepl("_1f_|_2e_|_3a_|_4b_|_5f_|_6e_|_7a_|_8f_|_9b_",colnames(df_ccnesa_ba))])/9
df$`CCNES-A Minimization reactions (baseline)` <- rowSums(df_ccnesa_ba[,grepl("_1d_|_2a_|_3d_|_4e_|_5d_|_6a_|_7c_|_8d_|_9f_",colnames(df_ccnesa_ba))])/9


df$`CCNES-A Distress reaction (follow-up)` <- rowSums(df_ccnesa_fu[,grepl("_1a_|_2d_|_3f_|_4d_|_5a_|_6b_|_7f_|_8a_|_9d_",colnames(df_ccnesa_fu))])/9
df$`CCNES-A Punitive reactions (follow-up)` <- rowSums(df_ccnesa_fu[,grepl("_1e_|_2f_|_3b_|_4c_|_5e_|_6c_|_7b_|_8e_|_9c_",colnames(df_ccnesa_fu))])/9
df$`CCNES-A Expressive encouragment (follow-up)` <- rowSums(df_ccnesa_fu[,grepl("_1b_|_2c_|_3e_|_4f_|_5b_|_6d_|_7e_|_8b_|_9e_",colnames(df_ccnesa_fu))])/9
df$`CCNES-A Emotion-focused reactions (follow-up)` <- rowSums(df_ccnesa_fu[,grepl("_1c_|_2b_|_3c_|_4a_|_5c_|_6f_|_7d_|_8c_|_9a_",colnames(df_ccnesa_fu))])/9
df$`CCNES-A Problem-focused reactions (follow-up)` <- rowSums(df_ccnesa_fu[,grepl("_1f_|_2e_|_3a_|_4b_|_5f_|_6e_|_7a_|_8f_|_9b_",colnames(df_ccnesa_fu))])/9
df$`CCNES-A Minimization reactions (follow-up)` <- rowSums(df_ccnesa_fu[,grepl("_1d_|_2a_|_3d_|_4e_|_5d_|_6a_|_7c_|_8d_|_9f_",colnames(df_ccnesa_fu))])/9

rm(df_ccnesa,df_ccnesa_ba,df_ccnesa_fu)

##### NEQ (awaiting, updated) #####
df_neq <- df_raw[df_raw$`Participant Id` < 400,grepl("Participant Id|NEQ",colnames(df_raw))]

df_neq <- df_neq[,!grepl("_21_",colnames(df_neq))]

df_neq[,c(3:ncol(df_neq))] <- lapply(df_neq[,c(3:ncol(df_neq))], as.numeric)

df_neq <- df_neq[!is.na(rowSums(df_neq[,grepl("_1_|_2_|_3_|_4_|_5_|_6_|_7_|_8_|_9_|_10_|_11_|_12_|_13_|_14_|_15_|_16_|_17_|_18_|_19_|_20_",colnames(df_neq))])),]

df_neq[,grepl("_1b_|_2b_|_3b_|_4b_|_5b_|_6b_|_7b_|_8b_|_9b_|_10b_|_11b_|_12b_|_13b_|_14b_|_15b_|_16b_|_17b_|_18b_|_19b_|_20b_",colnames(df_neq))]  <- (df_neq[,grepl("_1b_|_2b_|_3b_|_4b_|_5b_|_6b_|_7b_|_8b_|_9b_|_10b_|_11b_|_12b_|_13b_|_14b_|_15b_|_16b_|_17b_|_18b_|_19b_|_20b_",colnames(df_neq))] == 0)*1

for(i in ncol(df_neq):1){
   if(grepl("b_",colnames(df_neq)[i])){
      truelist <- (df_neq[,i] == 1)*1
   }else if(grepl("a_",colnames(df_neq)[i])){
      df_neq[,i] <- df_neq[,i]*truelist
   }
}

df_neq_rest <- data.frame(df_neq$`Participant Id`)
colnames(df_neq_rest) <- "Participant Id"

df_neq_rest$`NEQ Freq. negative effects  ` <- rowSums(df_neq[,grepl("_1_|_2_|_3_|_4_|_5_|_6_|_7_|_8_|_9_|_10_|_11_|_12_|_13_|_14_|_15_|_16_|_17_|_18_|_19_|_20_",colnames(df_neq))])
df_neq_rest$`NEQ Freq. negative effects from treatment` <- rowSums(df_neq[,grepl("_1b_|_2b_|_3b_|_4b_|_5b_|_6b_|_7b_|_8b_|_9b_|_10b_|_11b_|_12b_|_13b_|_14b_|_15b_|_16b_|_17b_|_18b_|_19b_|_20b_",colnames(df_neq))], na.rm=T)
df_neq_rest$`NEQ Freq. negative effects from other circumstances` <- df_neq_rest$`NEQ Freq. negative effects  `-df_neq_rest$`NEQ Freq. negative effects from treatment`

df_neq_rest$`NEQ Freq. Fac. 1 (Symptoms) from treatment` <- rowSums(df_neq[,grepl("_1b_|_2b_|_3b_|_4b_|_6b_|_7b_|_8b_|_10b_",colnames(df_neq))], na.rm=T)
df_neq_rest$`NEQ Freq. Fac. 2 (Quality) from treatment` <- rowSums(df_neq[,grepl("_15b_|_16b_|_17b_|_18b_|_19b_|_20b_",colnames(df_neq))], na.rm=T)
df_neq_rest$`NEQ Freq. Fac. 3 (Dependency) from treatment` <- df_neq[,grepl("_14b_",colnames(df_neq))]
df_neq_rest$`NEQ Freq. Fac. 3 (Dependency) from treatment`[is.na(df_neq_rest$`NEQ Freq. Fac. 3 (Dependency) from treatment`)] <- 0
df_neq_rest$`NEQ Freq. Fac. 4 (Stigma) from treatment`  <- rowSums(df_neq[,grepl("_9b_|_11b_",colnames(df_neq))], na.rm=T)
df_neq_rest$`NEQ Freq. Fac. 5 (Hopelessness) from treatment`  <- rowSums(df_neq[,grepl("_5b_|_12b_|_13b_",colnames(df_neq))], na.rm=T)

df_neq_rest$`NEQ Neg. impact from treatment` <- rowSums(df_neq[,grepl("_1a_|_2a_|_3a_|_4a_|_5a_|_6a_|_7a_|_8a_|_9a_|_10a_|_11a_|_12a_|_13a_|_14a_|_15a_|_16a_|_17a_|_18a_|_19a_|_20a_",colnames(df_neq))], na.rm=T)
df_neq_rest$`NEQ Neg. impact Fac. 1 (Symptoms) from treatment` <- rowSums(df_neq[,grepl("_1a_|_2a_|_3a_|_4a_|_6a_|_7a_|_8a_|_10a_",colnames(df_neq))], na.rm=T)
df_neq_rest$`NEQ Neg. impact Fac. 2 (Quality) from treatment`  <- rowSums(df_neq[,grepl("_15a_|_16a_|_17a_|_18a_|_19a_|_20a_",colnames(df_neq))], na.rm=T)
df_neq_rest$`NEQ Neg. impact Fac. 3 (Dependency) from treatment` <- df_neq[,grepl("_14a_",colnames(df_neq))]
df_neq_rest$`NEQ Neg. impact Fac. 3 (Dependency) from treatment`[is.na(df_neq_rest$`NEQ Neg. impact Fac. 3 (Dependency) from treatment`)] <- 0
df_neq_rest$`NEQ Neg. impact Fac. 4 (Stigma) from treatment` <- rowSums(df_neq[,grepl("_9a_|_11a_",colnames(df_neq))], na.rm=T)
df_neq_rest$`NEQ Neg. impact Fac. 5 (Hopelessness) from treatment` <- rowSums(df_neq[,grepl("_5a_|_12a_|_13a_",colnames(df_neq))], na.rm=T)

df <- merge(df,df_neq_rest,by="Participant Id",all.x=T)

rm(df_neq, df_neq_rest)

##### SDQ (correct) #####
df_sdq <- df_raw[df_raw$`Participant Id` < 400,grepl("Participant Id|Group|SDQ_B",colnames(df_raw))]

df_sdq <- df_sdq[,!grepl("_26|_27|_28|_29|_30",colnames(df_sdq))]

df_sdq[,c(3:ncol(df_sdq))] <- lapply(df_sdq[,c(3:ncol(df_sdq))], as.numeric)

df$`SDQ Prosocial (baseline)` <- rowSums(df_sdq[,grepl("_1_|_4_|_9_|_17_|_20_",colnames(df_sdq))])
df$`SDQ Emotion (baseline)` <- rowSums(df_sdq[,grepl("_3_|_8_|_13_|_16_|_24_",colnames(df_sdq))])
df$`SDQ ADHD (baseline)` <- rowSums(df_sdq[,grepl("_2_|_10_|_15_",colnames(df_sdq))])+4-rowSums(df_sdq[,grepl("_21_|_25_",colnames(df_sdq))])
df$`SDQ CD (baseline)` <- rowSums(df_sdq[,grepl("_5_|_12_|_18_|_22_",colnames(df_sdq))])+2-df_sdq[,grepl("_7_",colnames(df_sdq))]
df$`SDQ Companion (baseline)` <- rowSums(df_sdq[,grepl("_6_|_19_|_23_",colnames(df_sdq))])+4-rowSums(df_sdq[,grepl("_11_|_14_",colnames(df_sdq))])
df$`SDQ Externalising (baseline)` <- df$`SDQ CD (baseline)`+df$`SDQ ADHD (baseline)`
df$`SDQ Internalising (baseline)` <- df$`SDQ Emotion (baseline)`+df$`SDQ Companion (baseline)`
df$`SDQ Sum (baseline)` <- df$`SDQ Externalising (baseline)`+df$`SDQ Internalising (baseline)`

rm(df_sdq)

##### ECR-RC (correct) #####
df_ecrrc <- df_raw[df_raw$`Participant Id` < 400,grepl("Participant Id|Group|ECR-RC",colnames(df_raw))]

df_ecrrc[,c(3:ncol(df_ecrrc))] <- lapply(df_ecrrc[,c(3:ncol(df_ecrrc))], as.numeric)

df$`ECR-RC Anxiety (baseline)` <- rowSums(df_ecrrc[,grepl("_1_|_2_|_3_|_4_|_5_|_6_",colnames(df_ecrrc))])/6
df$`ECR-RC Avoidance (baseline)` <- (rowSums(df_ecrrc[,grepl("_7_|_8_|_9_",colnames(df_ecrrc))])+24-(rowSums(df_ecrrc[,grepl("_10_|_11_|_12_",colnames(df_ecrrc))])) )/6

rm(df_ecrrc)

#Consollidate group column name
colnames(df)[1:2] <- c("pt_id","group")

#Completion of follow-up
df$`Completion of follow-up` <- (!is.na(df$`DSHI-Y (follow-up)`) == TRUE)*1
