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
library(Rmisc)
library(colorspace)
library(grid)

set_flextable_defaults(fonts_ignore=TRUE)
knitr::opts_chunk$set(echo=FALSE, message=FALSE, warning=FALSE, fig.width = 7, fig.align = "center")

watermarkGrob <- function(){
   annotation_custom(grid::textGrob("SIMULATED DATA",gp=gpar(fontsize=16, fontface="bold",lineheight=1,alpha=0.5,col="red"),rot=30),
                     xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf)
}

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
      
      df[[strat_var]] <- as.factor(df[[strat_var]])
      
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
      
      if(any(grepl("\\|",temp[,1]))){
         category <- data.frame(t(data.frame(strsplit(temp[grepl("\\|",temp[,1]),1],"\\|"))))
         if(nrow(category) > 1) category[duplicated(category[,1]),1] <- " "
      
         temp[grepl("\\|",temp[,1]),2] <- category[,2]
         temp[grepl("\\|",temp[,1]),1] <- category[,1]
      }
      
      return(FitFlextableToPage(align(flextable(data.frame(temp[,c(1:4)], check.names = F)),align="center",part="header")))
   }

###### Diagnostics scores fig ######
   figScores <- function(df, var_name, xminmax=c(-0.5,19.5), 
                         simple=T, hide_legend = T, as_perc=F, lin_reg=F, 
                         lin_reg_simple=F, parents="no"){
      df_fig <- df[,grepl(paste0("pt_id|group|",var_name),colnames(df))]
      if(parents=="no"){ 
         df_fig <- df_fig[,!grepl("parents",colnames(df_fig))] 
         df_fig$group <- paste0("Participants ", df_fig$group)
         
         colored <- brewer.pal(n = 4, name = "Set1")[3:4]
         alphaed <- c(1,1)
         linetyped <- c("solid","solid")
      }
      if(parents=="yes"){ 
         df_temppartici <- df_fig[,!grepl("parents",colnames(df_fig))] 
         df_temppartici$group <- paste0("Participants ", df_temppartici$group)
         
         df_tempparent <- df_fig[,grepl("pt_id|group|parents",colnames(df_fig))] 
         df_tempparent$group <- paste0("Parents ", df_tempparent$group)
         colnames(df_tempparent) <- gsub("parents/","",colnames(df_tempparent))
         
         df_fig <- rbind(df_tempparent,df_temppartici)
         
         colored <- brewer.pal(n = 4, name = "Set1")[1:4]
         alphaed <- c(0.5,0.5,1,1)
         linetyped <- c("dashed","dashed","solid","solid")
      }
      if(parents=="only"){ 
         df_fig$group <- paste0("Parents ", df_fig$group)

         colored <- brewer.pal(n = 4, name = "Set1")[1:2]
         alphaed <- c(0.5,0.5)
         linetyped <- c("dashed","dashed")
      }
      df_fig <- reshape(df_fig, direction = "long",idvar = c("pt_id","group"),varying = 3:ncol(df_fig),sep="_")
      names(df_fig)[names(df_fig) == var_name] <- "result"
      
      df_fig$alpha <- 1
      df_fig$alpha[grepl("Parent",df_fig$group)] <- 0.5
      
      df_fig$linet <- "dashed"
      df_fig$linet[grepl("Parent",df_fig$group)] <- "solid"
      
      scaleFUN <- function(x) sprintf("%.1f", x)
      
      df_fig$pt_id <- as.numeric(df_fig$pt_id)
      df_fig$group <- as.factor(df_fig$group)
      df_fig$time <- as.numeric(df_fig$time)
      df_fig$result <- as.numeric(df_fig$result)
      
      figgen <- ggplot(df_fig, aes(x=time,y=result, color=group)) + 
         stat_summary(fun=mean, geom="point", position=position_dodge(width=1)) +
         stat_summary(fun=mean, geom="line", position=position_dodge(width=1), aes(linetype=group, alpha=group)) +
         stat_summary(fun.data=mean_cl_normal, geom="errorbar", position=position_dodge(width=1), width=0.8, aes(alpha=group)) +
         theme_minimal() + ylab(var_name) + xlab("weeks") + theme(legend.position = "bottom", legend.title = element_blank(),
                                                                  plot.margin = unit(c(0.05,0,0.05,0),"cm"), 
                                                                  axis.title.x = element_blank(), 
                                                                  axis.title=element_text(size=9,face="bold")) +   
         scale_alpha_manual(values=alphaed, guide=FALSE) +
         scale_linetype_manual(values=linetyped, guide=FALSE) +
         scale_y_continuous(label=scaleFUN) +
         scale_color_manual(values=colored)   + watermarkGrob()
      
      
      if(simple){
         figgen <- figgen + theme(axis.text.x = element_blank()) +
            scale_x_continuous(breaks = NULL, limits=xminmax) 
      }else{
         figgen <- figgen + scale_x_continuous(breaks = c(0,4,8,16), labels=c(paste(c(0,4,8,16), "weeks")), limits=xminmax) +
            theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank())
      }
      if(as_perc){
         figgen <- figgen + scale_y_continuous(labels = scales::percent_format(accuracy = 5L)) 
      }
      
      if(hide_legend){
         figgen <- figgen + theme(legend.position = "none")
      }
      
      if(lin_reg){
         df_linreg <- df_fig[df_fig$time == 0 | df_fig$time == 16,]
         df_linreg <- reshape(df_linreg, idvar = c("pt_id","group"), timevar = "time", direction = "wide")
         colnames(df_linreg) <- c("pt_id","group","baseline","followup")
         
         p_label <- NULL
         if(parents=="no"|parents=="yes"){
            part_df_linreg <- df_linreg[!grepl("Parent",df_linreg$group),]
            
            part_lin_reg <- lm(followup~group+baseline, data = part_df_linreg)
            part_lin_p <- summary(part_lin_reg)$coefficients
            part_lin_p <- round(part_lin_p[grepl("group",row.names(part_lin_p)),ncol(part_lin_p)],digits=2)
            if(part_lin_p == 0){ part_lin_p <- "p<0.01"}else{
               part_lin_p <- paste0("p=",part_lin_p)
            }  
            p_label <- paste0("Participants: ", part_lin_p)
         }
         if(parents=="yes"|parents=="only"){
            parent_df_linreg <- df_linreg[!grepl("Partic",df_linreg$group),]
            
            parent_lin_reg <- lm(followup~group+baseline, data = parent_df_linreg)
            parent_lin_p <- summary(parent_lin_reg)$coefficients
            parent_lin_p <- round(parent_lin_p[grepl("group",row.names(parent_lin_p)),ncol(parent_lin_p)],digits=2)
            if(parent_lin_p == 0){ parent_lin_p <- "p<0.01"}else{
               parent_lin_p <- paste0("p=",parent_lin_p)
            }  
            p_label <- paste0(p_label, "\n Parents: ", parent_lin_p)
         }
         figgen <- figgen + annotate("text",x=Inf,y=Inf,label=p_label, hjust=1, vjust=1.1, color="black", size=3)
      }
      
      if(lin_reg_simple){
         df_linreg <- df_fig[df_fig$time == 16,]
         df_linreg <- reshape(df_linreg, idvar = c("pt_id","group"), timevar = "time", direction = "wide")
         colnames(df_linreg) <- c("pt_id","group","baseline","followup")
         
         p_label <- NULL
         if(parents=="no"|parents=="yes"){
            part_df_linreg <- df_linreg[!grepl("Parent",df_linreg$group),]
            
            part_lin_reg <- lm(followup~group, data = part_df_linreg)
            part_lin_p <- summary(part_lin_reg)$coefficients
            part_lin_p <- round(part_lin_p[grepl("group",row.names(part_lin_p)),ncol(part_lin_p)],digits=2)
            if(part_lin_p == 0){ part_lin_p <- "p<0.01"}else{
               part_lin_p <- paste0("p=",part_lin_p)
            }  
            p_label <- paste0("Participants: ", part_lin_p)
         }
         if(parents=="yes"|parents=="only"){
            parent_df_linreg <- df_linreg[!grepl("Partic",df_linreg$group),]
            
            parent_lin_reg <- lm(followup~group, data = parent_df_linreg)
            parent_lin_p <- summary(parent_lin_reg)$coefficients
            parent_lin_p <- round(parent_lin_p[grepl("group",row.names(parent_lin_p)),ncol(parent_lin_p)],digits=2)
            if(part_lin_p == 0){ parent_lin_p <- "p<0.01"}else{
               parent_lin_p <- paste0("p=",parent_lin_p)
            }  
            p_label <- paste0(p_label, "\n Parents: ", parent_lin_p)
         }
         figgen <- figgen + annotate("text",x=Inf,y=Inf,label=p_label, hjust=1, vjust=1, color="black", size=3)
         
      }
      return(figgen)
   }
      
   g_legend<-function(a.gplot){
      tmp <- ggplot_gtable(ggplot_build(a.gplot))
      leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
      legend <- tmp$grobs[[leg]]
      return(legend)}
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

   