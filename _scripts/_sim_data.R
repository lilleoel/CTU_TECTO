n <- 128

df <- NULL
set.seed(730)

df$pt_id <- c(1:n)
df$group <- var_rand(n,2)

#Baseline variables ----
   df$`Gender` <- var_categories(n,c("Male","Female","Transgender","Other"))
   df$`Age` <- var_beta(n,13,17,"right")
   df$`Nationality` <- var_categories(n,c("Danish","Other"))
   df$`Parental education level` <- var_categories(n,c("High-school (finished)", "2-year grad-school","3-year grad-school", "5-year grad-school", "Ph.d. or equal level"))
   df$`Parental nationality` <- var_categories(n,c("Danish","Danish and other","Other"))
   df$`Parental stress scale (PSS)` <-  var_sample(n,4,400)
   df$`Social responsiveness scale (SRS)` <- var_sample(n,4,400)
   df$`Intelligence (WISC-V/WAIS-IV)` <- var_sample(n,4,400)
   
#FES ----
   df$`Cohesion` <-  var_sample(n,0,100)
   df$`Expressiveness` <-  var_sample(n,0,100)
   df$`Conflict` <-  var_sample(n,0,100)
   df$`Independence` <-  var_sample(n,0,100)
   df$`Achievement Orientation` <-  var_sample(n,0,100)
   df$`Intellectual-Cultural Orientation` <-  var_sample(n,0,100)
   df$`Active-Recreational Orientation` <-  var_sample(n,0,100)
   df$`Moral-Religious Emphasis` <-  var_sample(n,0,100)
   df$`Oranization` <-  var_sample(n,0,100)
   df$`Control` <-  var_sample(n,0,100)

   
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

   #Comorbidities
   df$`Depression_0` <- var_sample(n,0,1)
   df$`Agoraphobia_0` <- var_sample(n,0,1)
   df$`Social phobias_0` <- var_sample(n,0,1)
   df$`Specific phobias_0` <- var_sample(n,0,1)
   df$`Separation anxiety disorder_0` <- var_sample(n,0,1)
   df$`Generalized anxiety disorders_0` <- var_sample(n,0,1)
   df$`Anorexia nervosa_0` <- var_sample(n,0,1)
   df$`Anxious personality disorder_0` <- var_sample(n,0,1)
   df$`Adjustment disorders_0` <- var_sample(n,0,1)
   df$`Asperger syndrome_0` <- var_sample(n,0,1)
   df$`ADHD_0` <- var_sample(n,0,1)
   df$`Oppositional defiant disorder_0` <- var_sample(n,0,1)
   df$`Transient tics_0` <- var_sample(n,0,1)
   df$`Chronic tics/Tourette’s syndrome_0` <- var_sample(n,0,1)
   df$`Nonorganic enuresis_0` <- var_sample(n,0,1)
   df$`Nonorganic encopresis_0` <- var_sample(n,0,1)
   
#Creating the df
   df <- as.data.frame(df, check.names=FALSE)

