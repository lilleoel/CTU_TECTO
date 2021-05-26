n <- 128

df <- NULL
set.seed(730)

df$pt_id <- c(1:n)
df$group <- var_rand(n,2)

# Baseline variables ----
   df$`Gender` <- var_categories(n,c("Male","Female","Transgender","Other"))
   df$`Age` <- var_beta(n,13,17,"right")
   df$`Nationality` <- var_categories(n,c("Danish","Other"))
   df$`Parental education level` <- var_sample(n,0,8)
   df$`Parental nationality` <- var_categories(n,c("Danish","Danish and other","Other"))
   df$`Full-scale IQ` <- var_sample(n,70,120)
   df$`OCD-subtype` <- var_categories(n,c("Predominantly obsessional thoughts or ruminations",
                                          "Predominantly compulsive acts","Mixed obsessional thoughts and acts"))

   #Comorbidities
   df$`Comorbidities|Depressive disorders` <- var_sample(n,0,1)
   df$`Comorbidities|Anxiety disorders` <- var_sample(n,0,1)
   df$`Comorbidities|Adjustment disorders` <- var_sample(n,0,1)
   df$`Comorbidities|Eating disorders` <- var_sample(n,0,1)
   df$`Comorbidities|Personality disorders` <- var_sample(n,0,1)
   df$`Comorbidities|Aspergers Syndrome` <- var_sample(n,0,1)
   df$`Comorbidities|Hyperkinetic disorders` <- var_sample(n,0,1)
   df$`Comorbidities|Conduct disorders` <- var_sample(n,0,1)
   df$`Comorbidities|Tics/Tourettes syndrome` <- var_sample(n,0,1)
   df$`Comorbidities|Elimination disorders` <- var_sample(n,0,1)

# Baseline psychopathology ----
   df$`SRS_0` <- var_sample(n,4,400)   
     

# Family characteristics ----
   df$`FES - Relationship Dimensions|Cohesion` <-  var_sample(n,0,100)
   df$`FES - Relationship Dimensions|Expressiveness` <-  var_sample(n,0,100)
   df$`FES - Relationship Dimensions|Conflict` <-  var_sample(n,0,100)
   df$`FES - Personal Growth Dimensions|Independence` <-  var_sample(n,0,100)
   df$`FES - Personal Growth Dimensions|Achievement Orientation` <-  var_sample(n,0,100)
   df$`FES - Personal Growth Dimensions|Intellectual-Cultural Orientation` <-  var_sample(n,0,100)
   df$`FES - Personal Growth Dimensions|Active-Recreational Orientation` <-  var_sample(n,0,100)
   df$`FES - Personal Growth Dimensions|Moral-Religious Emphasis` <-  var_sample(n,0,100)
   df$`FES - System Maintenance Dimensions|Oranization` <-  var_sample(n,0,100)
   df$`FES - System Maintenance Dimensions|Control` <-  var_sample(n,0,100)

   
#Primary outcome ----
 
   #CY-BOCS
   df$`CY-BOCS_0` <- var_sample(n,0,40)
   df$`CY-BOCS_4` <- var_sample(n,0,40)
   df$`CY-BOCS_8` <- var_sample(n,0,40)
   df$`CY-BOCS_16` <- var_sample(n,0,40)
 
   df$CBOCS_grup[df$`CY-BOCS_16` >= 0 & df$`CY-BOCS_16` <= 10] <- "0-10 (Remitted)"
   df$CBOCS_grup[df$`CY-BOCS_16` >= 11 & df$`CY-BOCS_16` <= 15] <- "11-15 (Subclinical)"
   df$CBOCS_grup[df$`CY-BOCS_16` >= 16 & df$`CY-BOCS_16` <= 24] <- "16-24 (Moderately ill)"
   df$CBOCS_grup[df$`CY-BOCS_16` >= 25 & df$`CY-BOCS_16` <= 40] <- "25-40 (Severely ill)"
   df$CBOCS_grup <- as.factor(df$CBOCS_grup)
   
#Seconday outcomes -----

   # #Kidscreen-52
   df$`Kidscreen-10_0` <- var_beta(n,-4.327,4.703,"left",digs=2)
   df$`Kidscreen-10_4` <- var_beta(n,-4.327,4.703,"left",digs=2)
   df$`Kidscreen-10_8` <- var_beta(n,-4.327,4.703,"left",digs=2)
   df$`Kidscreen-10_16` <- var_beta(n,-4.327,4.703,"left",digs=2)

   #NEQ
   df$`NEQ_4` <- var_sample(n,0,32)
   df$`NEQ_8` <- var_sample(n,0,32)
   df$`NEQ_16` <- var_sample(n,0,32)
   df$`Negative effect burden` <- (df$`NEQ_4`+df$`NEQ_8`+df$`NEQ_16`)/16
   

#Exploratory outcomes ----

   # #SAE
   # df$`SAE_follow-up)` <- var_sample(n, 16, 80)

   #COIS
   df$`COIS-R_0` <- var_sample(n,0,32)
   df$`COIS-R_4` <- var_sample(n,0,32)
   df$`COIS-R_8` <- var_sample(n,0,32)
   df$`COIS-R_16` <- var_sample(n,0,32)

   #CGI-S
   df$`CGI-S_0` <- var_sample(n,0,32)
   df$`CGI-S_4` <- var_sample(n,0,32)
   df$`CGI-S_8` <- var_sample(n,0,32)
   df$`CGI-S_16` <- var_sample(n,0,32)
   
   #CGI-I
   df$`CGI-I_4` <- var_sample(n, 0, 44)
   df$`CGI-I_8` <- var_sample(n,0,32)
   df$`CGI-I_16` <- var_sample(n,0,32)
   
   #CGAS
   df$`CGAS_0` <- var_sample(n, 0, 44)
   df$`CGAS_16` <- var_sample(n,0,32)

   #K-SADS-PL
   df$`K-SADS-PL_0` <- var_sample(n, 0, 44)
   df$`K-SADS-PL_16` <- var_sample(n,0,32)

   #TOCS
   df$`TOCS_0` <- var_sample(n, 0, 44)
   df$`TOCS_16` <- var_sample(n,0,32)
   
   #TAFQ-A
   df$`TAFQ-A_0` <- var_sample(n, 0, 44)
   df$`TAFQ-A_4` <- var_sample(n,0,32)
   df$`TAFQ-A_8` <- var_sample(n, 0, 44)
   df$`TAFQ-A_16` <- var_sample(n,0,32)
   
   #FAS
   df$`FAS_0` <- var_sample(n, 0, 44)
   df$`FAS_4` <- var_sample(n,0,32)
   df$`FAS_8` <- var_sample(n, 0, 44)
   df$`FAS_16` <- var_sample(n,0,32)
   
   #PSS
   df$`PSS_0` <- var_sample(n, 0, 44)
   df$`PSS_4` <- var_sample(n,0,32)
   df$`PSS_8` <- var_sample(n, 0, 44)
   df$`PSS_16` <- var_sample(n,0,32)
   
#Comorbidities
   df$`Depressive disorders|Mild depressive episode (F32.0)` <- var_sample(n,0,1)
   df$`Depressive disorders|Moderate depressive episode (F32.1)` <- var_sample(n,0,1)
   df$`Depressive disorders|Severe depressive episode without psychotic symptoms (F32.2)` <- var_sample(n,0,1)
   df$`Anxiety disorders|Agoraphobia (F40.0)` <- var_sample(n,0,1)
   df$`Anxiety disorders|Social phobias (F40.1)` <- var_sample(n,0,1)
   df$`Anxiety disorders|Specific phobias (F40.2)` <- var_sample(n,0,1)
   df$`Anxiety disorders|Panic disorder (F41.0)` <- var_sample(n,0,1)
   df$`Anxiety disorders|Generalized anxiety disorder (F41.1)` <- var_sample(n,0,1)
   df$`Adjustment disorders|Acute stress reaction (F43.0)` <- var_sample(n,0,1)
   df$`Adjustment disorders|Post-traumatic stress disorder (F43.1)` <- var_sample(n,0,1)
   
   
   
#Creating the df
   df <- as.data.frame(df, check.names=FALSE)

   df$Remitted <- 0
   df$Remitted[df$`CY-BOCS_16` >= 0 & df$`CY-BOCS_16` <= 10] <- 1
   
   df$Response <- 0
   df$Response[df$`CY-BOCS_0` < 0.70*df$`CY-BOCS_16`] <- 1