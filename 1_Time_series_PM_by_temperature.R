remove(list = ls())
library(tidyverse);library(rio);library(dlnm);library(splines);
library(tsModel);library(mgcv);library(mixmeta);library(RcppRoll);library(metafor)

setwd("E:/OneDrive/")
data_use <- import("Example_data/Time_serie_test_data.csv") %>% 
  select(c(1:12))

pollution <- c("PM2.5","BC")
P_like <- tibble()
regions <- unique(data_use$district)
city_code <- unique(data_use$city_code)
data <- lapply(regions,function(x) data_use[data_use$district==x,])
names(data) <- regions
overall_result <- tibble()

for (pol_i in pollution) {  
    cat(pol_i," ")
##############################################
#define matrix to store the results
  result1 <- matrix(NA,length(regions),5,dimnames=list(regions,paste("b",seq(5),sep=""))) 
  result2 <- matrix(NA,length(regions),5,dimnames=list(regions,paste("b",seq(5),sep=""))) 
  result3 <- matrix(NA,length(regions),5,dimnames=list(regions,paste("b",seq(5),sep=""))) 

for(i in regions) {
  # PRINT
  cat(i,"")
  
  # LOAD
  sub <- data[[i]]
  
  #Generate time-trend covariates
  sub <- sub [order(sub$date),]   
  sub$date1 <- 1:length(sub$date)
  sub$month  <- as.factor(months(sub$date))
  sub$year   <- as.factor(format(sub$date, format="%Y") )
  sub$dow    <- as.factor(weekdays(sub$date))
  
  #Calculate temperature humidity at lag01
  sub <- sub %>%
    mutate(temp = (l0avg_temp+l1avg_temp)/2)
 
  #Categoried temperature into groups by 25th and 75th quantities
  qk<-quantile(sub$temp,na.rm=T,probs=c(0.25,0.75)) 
  sub$qgroup[sub$temp<=qk[1]]<-"q1"
  sub$qgroup[sub$temp>qk[1] & sub$temp<=qk[2]]<-"q2"
  sub$qgroup[sub$temp>qk[2]] <- "q3"
  
 
  
  sub["analysis"] <- sub[(paste0(pol_i,"lag01"))]
  sub$rhu2 = roll_mean(sub$l0rhu, n = 4, align = "right", fill = NA)
  ############################################################
  # 运行CLR模型
  #第一阶段各城市的模型
  tryCatch({
    
    model <- gam(PSN_NO~analysis:qgroup+ns(temp,df= 6)+ns(rhu2, df=3) + ns(date1,df=8*length(unique(year)))
               +as.factor(Holiday)
               +as.factor(dow),
               control=list(maxit=100),
               family=quasipoisson,sub,na.action="na.exclude")
    ## Model coefficients of temperature group 1
    coefs1 <- summary(model)
    result1[i,]<- cbind(beta = coefs1[["p.coeff"]][["analysis:qgroupq1"]],
                        se = coefs1[["se"]][["analysis:qgroupq1"]], 
                        district = i,
                        pollution  = pol_i,
                        Q = "1")
    ## Model coefficients of temperature group 2
    coefs1 <- summary(model)
    result2[i,]<- cbind(beta = coefs1[["p.coeff"]][["analysis:qgroupq2"]],
                        se = coefs1[["se"]][["analysis:qgroupq2"]], 
                        district = i,
                        pollution  = pol_i,
                        Q = "2")
    ## Model coefficients of temperature group 3
    coefs1 <- summary(model)
    result3[i,]<- cbind(beta = coefs1[["p.coeff"]][["analysis:qgroupq3"]],
                        se = coefs1[["se"]][["analysis:qgroupq3"]], 
                        district = i,
                        pollution  = pol_i,
                        Q = "3")
 
  }, error=function(e){cat("Error",conditionMessage(e), "\n")})    
}
  
  ####Likelihood ratio test##
  result_all <- rbind(result1,result2,result3)
  result_all <- as.data.frame(result_all)
  colnames(result_all) <- c("beta","se","city","PM","Q")
  temp1 <- result_all %>% 
    mutate(beta = as.numeric(beta),
           se = as.numeric(se),
           Q = as.factor(Q))
  library(metafor)
  likeli <- summary(rma(yi = beta,
                        sei = se,
                        mods = Q,
                        data = temp1,
                        method="REML")) 
  
  P_temp = data.frame(P = likeli[["pval"]][2],
                     
                      pollution = pol_i)
  
  P_like <- rbind(P_like,P_temp)
  
  #Pooled city-specific estimates by random effects model with each IQR increase for temperature group 1
  final_result <- result1
   
  meta_result1 <- data.frame() 
  final_result <- as.data.frame(final_result)
  colnames(final_result) <- c("beta","se","city","PM","Q")
  temp1 <- final_result %>% 
    mutate(beta = as.numeric(beta),
           se = as.numeric(se))
  
  variable_names <- c("PM2.5","BC")
  values <- c(31.78809,
              1.785468) 
  
  IQR <- data.frame(Variable = variable_names, Value = values)
  IQR_p <- IQR$Value[which(IQR$Variable==paste0(pol_i))] 
  
  summary <- summary(rma.uni(yi = beta,
                             sei = se,
                             data = temp1,
                             method="REML")) 
  beta=summary$b
  se = summary$se
  RR <-exp(beta*IQR_p)
  RR_L <- exp((beta-1.96*se)*IQR_p)
  RR_H <- exp((beta+1.96*se)*IQR_p)
  percent_change <- RR-1
  percent_change_L <- RR_L-1
  percent_change_H <- RR_H-1
  res <- data.frame(cbind(RR = RR,
                          RR_L = RR_L,
                          RR_H = RR_H,
                          beta = beta,
                          se = se,
                          P_value = summary$pval,
                          percent_change = percent_change*100,
                          percent_change_L = percent_change_L*100,
                          percent_change_H = percent_change_H*100,
                          pollution = pol_i,
                          IQR = IQR_p,
                          Q1 = "Q1"))
  meta_result1 <- rbind(meta_result1,res)
  
  
  #Pooled city-specific estimates by random effects model with each IQR increase for temperature group 2
  final_result <- result2
  final_result      
  
  meta_result2 <- data.frame() 
  final_result <- as.data.frame(final_result)
  colnames(final_result) <- c("beta","se","city","PM","Q")
  temp1 <- final_result %>% 
    mutate(beta = as.numeric(beta),
           se = as.numeric(se))
  variable_names <- c("PM2.5","BC")
  values <- c(31.78809,
              1.785468) 
  
  IQR <- data.frame(Variable = variable_names, Value = values)
  IQR_p <- IQR$Value[which(IQR$Variable==paste0(pol_i))] 
 
  summary <- summary(rma.uni(yi = beta,
                             sei = se,
                             data = temp1,
                             method="REML")) 
  beta=summary$b
  se = summary$se
  RR <-exp(beta*IQR_p)
  RR_L <- exp((beta-1.96*se)*IQR_p)
  RR_H <- exp((beta+1.96*se)*IQR_p)
  percent_change <- RR-1
  percent_change_L <- RR_L-1
  percent_change_H <- RR_H-1
  res <- data.frame(cbind(RR = RR,
                          RR_L = RR_L,
                          RR_H = RR_H,
                          beta = beta,
                          se = se,
                          P_value = summary$pval,
                          percent_change = percent_change*100,
                          percent_change_L = percent_change_L*100,
                          percent_change_H = percent_change_H*100,
                          pollution = pol_i,
                          IQR = IQR_p,
                          Q1 = "Q2"))
  meta_result2 <- rbind(meta_result2,res)
  
  
 #Pooled city-specific estimates by random effects model with each IQR increase for temperature group 3
  final_result <- result3
        
  #meta合并效应
  library(metafor)
  meta_result3 <- data.frame() 
  final_result <- as.data.frame(final_result)
  colnames(final_result) <- c("beta","se","city","PM","Q")
  temp1 <- final_result %>% 
    mutate(beta = as.numeric(beta),
           se = as.numeric(se))
  
  variable_names <- c("PM2.5","BC")
  values <- c(31.78809,
              1.785468) 
  
  
  IQR <- data.frame(Variable = variable_names, Value = values)
  IQR_p <- IQR$Value[which(IQR$Variable==paste0(pol_i))] 

  summary <- summary(rma.uni(yi = beta,
                             sei = se,
                             data = temp1,
                             method="REML")) 
  beta=summary$b
  se = summary$se
  RR <-exp(beta*IQR_p)
  RR_L <- exp((beta-1.96*se)*IQR_p)
  RR_H <- exp((beta+1.96*se)*IQR_p)
  percent_change <- RR-1
  percent_change_L <- RR_L-1
  percent_change_H <- RR_H-1
  res <- data.frame(cbind(RR = RR,
                          RR_L = RR_L,
                          RR_H = RR_H,
                          beta = beta,
                          se = se,
                          P_value = summary$pval,
                          percent_change = percent_change*100,
                          percent_change_L = percent_change_L*100,
                          percent_change_H = percent_change_H*100,
                          pollution = pol_i,
                          IQR = IQR_p,
                          Q1 = "Q3"))
  meta_result3 <- rbind(meta_result3,res)
  
  
  
  

  overall_result1 <- rbind(meta_result1,meta_result2,meta_result3)
  overall_result <- rbind(overall_result,overall_result1)
}

colnames(overall_result) <- c("RR","RR_L","RR_H","beta","se",
                              "P_value","percent_change","percent_change_L","percent_change_H","pollution","IQR","Q")

final_res <- overall_result
final_res <- final_res %>% 
  mutate(value = sprintf("%.2f (%.2f, %.2f)",
                         as.numeric(final_res$percent_change), 
                         as.numeric(final_res$percent_change_L),
                         as.numeric(final_res$percent_change_H)))

write.csv(final_res,paste0("P_by_temp.csv"))
  
write.csv(P_like,paste0("Likelihood_test.csv"))

