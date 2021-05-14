###### LIBRARIES ######
library(knitr)
library(kableExtra)
library(huxtable)
library(flextable)
library(Publish)
library(ggplot2)
library(gridExtra)
library(Hmisc)
library(nlme)
library(ggpubr)
library(RColorBrewer)
library(ggfortify)
set_flextable_defaults(fonts_ignore=TRUE)
knitr::opts_chunk$set(echo=FALSE, message=FALSE, warning=FALSE, fig.width = 7, fig.align = "center")


###### SIMULATE DATA ######
var_sample <- function(n, min, max){ #  <- for normaldistributed data
   return(as.numeric(as.character(sample(min:max, n, replace=T))))
}
var_rand <- function(n, no_groups){ # <- for grouping
   return(c(rep("A",n/no_groups),rep("B",n/no_groups))[sample(1:n, n, replace=F)])
}

var_categories <- function(n, categories){ # <- for grouping
   return(sample(categories, n, replace=T))
}
var_beta <- function(n, min, max, side,digs=0){ #  <- for skewed variables
   if(side == "right") return(round(min+(max-min)*rbeta(n,5,2),digits=digs))
   if(side == "left") return(round(min+(max-min)*rbeta(n,2,5),digits=digs))
}

###### HELPER FUNCTIONS ######
   FitFlextableToPage <- function(ft, pgwidth = 6){
      ft_out <- ft %>% autofit()
      return(width(ft_out, width = dim(ft_out)$widths*pgwidth /(flextable_dim(ft_out)$widths)))
   }
   quiet <- function(x) { 
      sink(tempfile()) 
      on.exit(sink()) 
      invisible(force(x)) 
   } 

###### TABLE ###### 

   tbl_func <- function(df, strat_var, vars = NULL, Grepl=FALSE, p_val=NA, remove.first.col = F,
                        first.col.name = ""){
      require(Publish)
      
      if(Grepl) vars <- colnames(df)[grepl(paste0(vars,collapse="|"),colnames(df))]
      vars <- paste0("`",vars,"`")
      
      temp <- univariateTable(formula(paste0(strat_var, "~",paste0(vars,collapse="+"))),data=df, compare.groups = F)
      temp <- quiet(print(temp))
      
      for(i in unique(temp$Variable)){
         if(length(levels(as.factor(df[[i]]))) == 2 &
            any(levels(as.factor(df[[i]])) %in% "0")){
            temp$Variable[which(temp$Variable %in% i)+1] <- temp$Variable[which(temp$Variable %in% i)]
            temp <- temp[!(temp$Variable == i & temp$Level == levels(as.factor(df[[i]]))[1]),] 
         }
      }
      
      if(!is.na(p_val)) temp$`p-value` <- ""
      if(!is.na(p_val)) temp$`p-value`[p_val[1]] <- p_val[2] 
      
      
      colnames(temp)[1] <- "  "
      colnames(temp)[2] <- " "
      colnames(temp) <- gsub("\\(","\n\\(",colnames(temp))
      colnames(temp) <- gsub(paste0(strat_var," = "),"",colnames(temp))
      
      temp[] <- lapply(temp,FUN=function(x) gsub("NaN \\(NA\\)","",x))

      if(remove.first.col) temp <- temp[,-1]
      if(first.col.name != "") colnames(temp)[1] <- first.col.name
      
      temp[temp[[2]] == "1",2] <- ""
      
      return(FitFlextableToPage(align(flextable(data.frame(temp, check.names = F)),align="center",part="header")))
   }

###### Diagnostics scores fig ######
   figScores <- function(df, var_name, xminmax=c(-0.5,16.5), 
                         simple=T, as_perc=F, lin_reg=F, 
                         lin_reg_simple=F){
      df_fig <- df[,grepl(paste0("pt_id|group|",var_name),colnames(df))]
      df_fig <- reshape(df_fig, direction = "long",idvar = c("pt_id","group"),varying = 3:ncol(df_fig),sep="_")
      names(df_fig)[names(df_fig) == var_name] <- "result"
      
      figgen <- ggplot(df_fig, aes(x=time,y=result, color=group)) + 
         stat_summary(fun=mean, geom="point", position=position_dodge(width=1)) +
         stat_summary(fun=mean, geom="line", position=position_dodge(width=1)) +
         stat_summary(fun.data=mean_cl_normal, geom="errorbar", position=position_dodge(width=1), width=0.8) +
         theme_minimal() + ylab(var_name) + xlab("weeks") + theme(legend.position = "bottom", legend.title = element_blank(),
                                                                  plot.margin = unit(c(0,0,0,0),"cm"), axis.title.x = element_blank()) +
         scale_color_brewer(palette="Dark2")
      
      if(simple){
         figgen <- figgen + theme(legend.position = "none", axis.text.x = element_blank()) +
            scale_x_continuous(breaks = NULL, limits=xminmax) 
      }else{
         figgen <- figgen + scale_x_continuous(breaks = c(0,4,8,16), labels=c(paste(c(0,4,8,16), "weeks")), limits=xminmax) +
            theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank())
      }
      if(as_perc){
         figgen <- figgen + scale_y_continuous(labels = scales::percent_format(accuracy = 5L)) 
      }
      
      if(lin_reg){
         df_linreg <- df_fig[df_fig$time == 0 | df_fig$time == 16,]
         df_linreg <- reshape(df_linreg, idvar = c("pt_id","group"), timevar = "time", direction = "wide")
         colnames(df_linreg) <- c("pt_id","group","baseline","followup")
         lin_reg <- lm(followup~group+baseline, data = df_linreg)
         lin_p <- summary(lin_reg)$coefficients
         lin_p <- round(lin_p[grepl("group",row.names(lin_p)),ncol(lin_p)],digits=3)
         if(lin_p == 0){ lin_p <- "p < 0.001"}else{
            lin_p <- paste0("p = ",lin_p)
         }
         figgen <- figgen + annotate("text",x=Inf,y=Inf,label=lin_p, hjust=1.1, vjust=-1.1, color="black", angle=90)
      }
      if(lin_reg_simple){
         df_linreg <- df_fig[df_fig$time == 16,]
         colnames(df_linreg) <- c("pt_id","group","time","followup")
         lin_reg <- lm(followup~group, data = df_linreg)
         lin_p <- summary(lin_reg)$coefficients
         lin_p <- round(lin_p[grepl("group",row.names(lin_p)),ncol(lin_p)],digits=3)
         if(lin_p == 0){ lin_p <- "p < 0.001"}else{
            lin_p <- paste0("p = ",lin_p)
         }
         
      figgen <- figgen + annotate("text",x=Inf,y=Inf,label=lin_p, hjust=1.1, vjust=-1.1, color="black", angle=90)
      }
      return(figgen)
   }
      

###### ROD BELOW ######   
   
   
   
   
   
      
   
###### ASSUMPTION PLOT ###### 
   #var <- "CY-BOCS"
   fig_assum <- function(df, var){
      
      
      
      
      #Residuals QQ
      gg_resQQ <- function(LM) { # argument: a linear model  
         y <- quantile(LM$resid[!is.na(LM$resid)], c(0.25, 0.75))
         x <- qnorm(c(0.25, 0.75))
         slope <- diff(y)/diff(x)
         int <- y[1L] - slope * x[1L]
         p <- ggplot(LM, aes(sample=.resid)) +
            stat_qq(alpha = 0.5) +
            geom_abline(slope = slope, intercept = int, color="blue")
         
         return(p)
      }
      
      var <- input$df
      a1 <- gg_resQQ(input$lin_reg) + theme_minimal() + ggtitle("Residual QQ-plot") + 
         theme(plot.title = element_text(hjust=0.5,face="bold",size=10), plot.title.position = "panel",
               plot.margin = unit(c(0,0,0,0),"cm"), legend.position = "none")
      
      homogein <- data.frame(fitted=fitted(input$lin_reg), residuals=residuals(input$lin_reg))
      a2 <- ggplot(data=homogein, aes(x=fitted, y=residuals)) +
         geom_point() +
         geom_smooth(method="lm",se=F) + theme_minimal() + ggtitle("Homogeneity") +
         theme( plot.title = element_text(hjust=0.5,face="bold",size=10), plot.title.position = "panel",
                plot.margin = unit(c(0,0,0,0),"cm"), legend.position = "none")
      
      if(!is.na(input$lin_reg_log)){
         b1 <- gg_resQQ(input$lin_reg_log) + theme_minimal() + ggtitle("Residual QQ-plot (log)") + 
            theme(plot.title = element_text(hjust=0.5,face="bold",size=10), plot.title.position = "panel",
                  plot.margin = unit(c(0,0,0,0),"cm"), legend.position = "none")
         
         homogein <- data.frame(fitted=fitted(input$lin_reg_log), residuals=residuals(input$lin_reg_log))
         b2 <- ggplot(data=homogein, aes(x=fitted, y=residuals)) +
            geom_point() +
            geom_smooth(method="lm",se=F) + theme_minimal() + ggtitle("Homogeneity (log)") +
            theme( plot.title = element_text(hjust=0.5,face="bold",size=10), plot.title.position = "panel",
                   plot.margin = unit(c(0,0,0,0),"cm"), legend.position = "none")
      }else{
         b1 <- ggplot() + theme_minimal() + ggtitle("Residual QQ-plot (log)") +
            theme(plot.title = element_text(hjust=0.5,face="bold",size=10), plot.title.position = "panel",
                  plot.margin = unit(c(0,0,0,0),"cm"), legend.position = "none")
         b2 <- ggplot() + theme_minimal() + ggtitle("Homogeneity (log)") + 
            theme(plot.title = element_text(hjust=0.5,face="bold",size=10), plot.title.position = "panel",
                  plot.margin = unit(c(0,0,0,0),"cm"), legend.position = "none")
      }
      
      return(grid.arrange(a1,a2,b1,b2,ncol=4, top=paste0("Assumptions for linear regression | ",name)))
   }
   
   #LINEAR REGRESSION
   
   
   

   
   
   
   
   
   

   
   
   ###### MWU ######
   
   MWU_test <- function(df,var,digs=2){
      
      var <- gsub("\\(","\\\\(",var)
      var <- gsub("\\)","\\\\)",var)
      
      test <- wilcox.test(df[df$group == "A",grepl(var,colnames(df))],df[df$group == "B",grepl(var,colnames(df))], correct=F,conf.int=T)
      
      output <- NULL
      output$test <- test
      output$estimate <- round(test$estimate[[1]],digits=digs)
      output$p <- round(test$p.value,digits=digs*2)
      output$lcl <- round(test$conf.int[1],digits=digs)
      output$ucl <- round(test$conf.int[2],digits=digs)
      
      output$est_ci <- paste0(output$estimate, " (", output$lcl, "-", output$ucl,")")
      
      return(output)
   }
   
###### Linear regression ######
   
   lin_reg <- function(df,var,time=c("baseline","follow-up")){
      
      temp <- df[,grepl(paste0(var,"|group|pt_id"),colnames(df))]
      temp1 <- temp[,grepl(paste0(paste0(time, collapse="|"),"|group|pt_id"),colnames(temp))]
      colnames(temp1) <- c("pt_id","group","baseline","followup")
      temp1$baseline <- as.numeric(temp1$baseline)
      temp1$followup <- as.numeric(temp1$followup)
      lin_reg <- lm(followup~group+baseline, data = temp1)
      
      output <- NULL
      lin_p <- summary(lin_reg)$coefficients
      output$lin_reg <- lin_reg
      output$lin_p <- round(lin_p[grepl("group",row.names(lin_p)),ncol(lin_p)],digits=5)
      
      if(min(temp1$baseline,na.rm=T) > 0 & min(temp1$followup,na.rm=T) > 0){
         temp1$log_baseline <- log10(temp1$baseline)
         temp1$log_followup <- log(temp1$followup)
         lin_reg_log <- lm(log_followup~group+log_baseline, data = temp1)
         lin_log_p <- summary(lin_reg_log)$coefficients
         output$lin_reg_log <- lin_reg_log
         output$lin_log_p <- round(lin_log_p[grepl("group",row.names(lin_log_p)),ncol(lin_log_p)],digits=5)
      }else{
         output$lin_reg_log <- NA
         output$lin_log_p <- NA
      }
      
      output$df <- temp1
      
      return(output)
   }
   
###### Logistic regression ######
   
   log_reg <- function(df,var){
      
      OR_table <- function(x, d = 3, intercept = F){
         
         temp1 <- as.data.frame(exp(coef(x)))
         temp2 <- as.data.frame(exp(confint(x)))
         temp3 <- as.data.frame(summary(x)$coef[,4])
         rownames(temp2) <- rownames(temp1)
         results <- as.data.frame(cbind(temp1,cbind(temp2,temp3)))
         colnames(results) <- c("or","lcl","ucl","p")
         results <- round(results,digits=3)
         if(intercept == FALSE){
            results <- results[c(2:nrow(results)),]
         }
         return(results)
      }
      
      temp1 <- df[,grepl(paste0(var,"|group|pt_id"),colnames(df))]
      colnames(temp1) <- c("pt_id","group","outcome")
      
      #temp1 <- read.csv2("log_to_RR.csv")
      
      temp1$outcome <- as.factor(temp1$outcome)
      temp1$group <- as.factor(temp1$group)
      log_reg <- glm(outcome~group, data = temp1, family="binomial")
      
      output <- NULL
      output$or_table <- OR_table(log_reg)
      output$or_p <- output$or_table[ncol(output$or_table)]
      
      
      p <- predict(object = log_reg,
                   newdata = temp1,
                   type = "response",
                   se.fit = TRUE)
      mult <- qnorm(0.5*(1-0.95))
      out <- cbind(p$fit,
                   p$se.fit,
                   p$fit+p$se.fit*mult,
                   p$fit-p$se.fit*mult)
     
      colnames(out) <- c("rr", "Std.Err", "lcl", "ucl")
      
      out <- data.frame(unique(out))
      out <- out[,c("rr","lcl","ucl")]
      out <- out[1,]/out[2,]
      out[,c("lcl","ucl")] <- 0
      out$p <- 0
       output$rr_table <- out
      
      rr <- data.frame(t(c(epitools::riskratio(temp1$group,temp1$outcome)$measure[2,],NA)))
      colnames(rr) <- c("rr", "lcl", "ucl","p")
      output$rr_table <- rr
      
      # if(min(temp1$baseline,na.rm=T) > 0 & min(temp1$followup,na.rm=T) > 0){
      #    temp1$log_baseline <- log10(temp1$baseline)
      #    temp1$log_followup <- log(temp1$followup)
      #    lin_reg_log <- lm(log_followup~group+log_baseline, data = temp1)
      #    lin_log_p <- summary(lin_reg_log)$coefficients
      #    output$lin_reg_log <- lin_reg_log
      #    output$lin_log_p <- round(lin_log_p[grepl("group",row.names(lin_log_p)),ncol(lin_log_p)],digits=5)
      # }else{
      #    output$lin_reg_log <- NA
      #    output$lin_log_p <- NA
      # }
      # 
      # output$df <- temp1
      
      return(output)
   }   
   
   



###### Continuous figure ###### 
   fig_num_cont <- function(df, var, x_axis = " ", y_axis,type){
      
      temp <- df[,grepl(paste0(var,"|group|pt_id"),colnames(df))]
      temp1 <- NULL
      for(i in c(3:ncol(temp))){
         time <- strsplit(colnames(temp)[i],"_")[[1]][length(strsplit(colnames(temp)[i],"_")[[1]])]
         temp1 <- data.frame(rbind(temp1,cbind(temp$pt_id,temp$group,time,temp[[i]])))
      }
      colnames(temp1) <- c("pt_id","group","time","result")
      temp1$result <- as.numeric(temp1$result)
      
      median_IQR <- function(x) {
         data.frame(y = median(x), # Median
                    ymin = quantile(x)[2], # 1st quartile
                    ymax = quantile(x)[4])  # 3rd quartile
      }
      
      mean_CI <- function(x) {
         data.frame(y = mean(x), # Median
                    ymin = t.test(x)$conf.int[1], # 1st quartile
                    ymax = t.test(x)$conf.int[2])  # 3rd quartile
      }
      
      if(type == "median"){
         g1 <- ggplot(data=temp1, aes(x=time, y=result, color=group, group=group)) + 
            stat_summary(fun=median, geom="point", position=position_dodge(width=0.25)) +
            stat_summary(fun=median, geom="line", position=position_dodge(width=0.25)) +
            stat_summary(fun.data=median_IQR, geom="errorbar", width=0.0, position=position_dodge(width=0.25)) +
            theme_minimal() + labs(x=x_axis,y=y_axis,title="Median (IQR)") +
            theme(plot.title = element_text(hjust=0.5,face="bold",size=10), plot.title.position = "panel",
                  legend.title = element_blank(), plot.margin = unit(c(0,0,0,0),"cm"),
                  legend.margin = unit(c(0,0,0,0),"cm"))
      }else{
         g1 <- ggplot(data=temp1, aes(x=time, y=result, color=group, group=group)) + 
            stat_summary(fun=mean, geom="point", position=position_dodge(width=0.25)) +
            stat_summary(fun=mean, geom="line", position=position_dodge(width=0.25)) +
            stat_summary(fun.data=mean_CI, geom="errorbar", width=0.2, position=position_dodge(width=0.25)) +
            theme_minimal() + labs(x=x_axis,y=y_axis, title="Mean (95%CI)") +
            theme(plot.title = element_text(hjust=0.5,face="bold",size=10), plot.title.position = "panel",
                  legend.title = element_blank(), plot.margin = unit(c(0,0,0,0),"cm"),
                  legend.margin = unit(c(0,0,0,0),"cm"), legend.spacing = unit(0, 'cm'))
      }
      return(g1)
   }

###### Continuous ###### 

   