remove(list = ls())
library(tidyverse);library(rio);library(dlnm);library(splines);
library(tsModel);library(mgcv);library(mixmeta);library(RcppRoll);library(metafor)

setwd("E:/OneDrive/")
data_use <- import("Example_data/Time_serie_test_data.csv")%>% 
  select(c(1:12))
  

pollution <- c("BC")
#Take total major cardiovasular events as an examle.
meta_relag <- read.csv("BC_by_temperature_result.csv") %>% 
  mutate(l = beta-1.96*se,
         h = beta+1.96*se)
rr <- as.data.frame(meta_relag, stringsAsFactors = F)
af_all <- tibble()
####总体####

  regions <- unique(data_use$district)
  
  ann_final1 <- tibble()
  ann_final2 <- tibble()
  ann_final3 <- tibble()
  
  for(i in regions) {
    # PRINT
    cat(i,"")
    
    # LOAD
    sub <- data_use %>% 
      filter(district == i)
    #Generate time-trend covariates
    sub <- sub [order(sub$date),]   
    sub$date1 <- seq(1, nrow(sub))
    
    #Calculate temperature humidity at lag01
    sub <- sub %>%
      mutate(temp = (l0avg_temp+l1avg_temp)/2)
    #Categoried temperature into groups by 25th and 75th quantities
    qk<-quantile(sub$temp,na.rm=T,probs=c(0.25,0.75)) 
    
    sub$qgroup[sub$temp<=qk[1]]<-"q1"
    sub$qgroup[sub$temp>qk[1] & sub$temp<=qk[2]]<-"q2"
    sub$qgroup[sub$temp>qk[2]] <- "q3"
    
    #Calculate average BC  concentrate and total hospital admissions in each temperature stratum
    pol_bytemp <- sub %>% 
      group_by(qgroup) %>% 
      summarise(pol = mean(BClag01,na.rm=T),
                PSN_NO = sum(PSN_NO,na.rm = T))
    #Extract effect estimates associated with BC in each temperature stratum
    rr_use1 <- subset(rr,  pollution == "BC" & Q == "Q1" )
    rr_use2 <- subset(rr, pollution == "BC" & Q == "Q2" )
    rr_use3 <- subset(rr, pollution == "BC" & Q == "Q3" )
    
    # sum <- read.csv(paste0("/home/phd_lhm/Yibao/Data/大类疾病每年总住院人数/",m,"分城市每年住院人数.csv"))
    sum_add <- aggregate(sub$PSN_NO, c(list(format(sub$date, "%Y"),sub$qgroup)), FUN=sum, na.rm=T) %>% 
      dplyr::rename(PSN_NO = x)
    
    #Calculate attributable number in temperature stratum 1
    sub1 <- sub %>% 
      filter(qgroup == "q1") %>% 
      mutate(AN = (exp(rr_use1$beta*BClag01)-1)*PSN_NO/exp(rr_use1$beta*BClag01),
             AN_L = (exp(rr_use1$l*BClag01)-1)*PSN_NO/exp(rr_use1$l*BClag01),
             AN_H = (exp(rr_use1$h*BClag01)-1)*PSN_NO/exp(rr_use1$h*BClag01))
    
    #Calculate attributable number in temperature stratum 2
    sub2 <- sub %>% 
      filter(qgroup == "q2") %>% 
      mutate(AN = (exp(rr_use2$beta*BClag01)-1)*PSN_NO/exp(rr_use2$beta*BClag01),
             AN_L = (exp(rr_use2$l*BClag01)-1)*PSN_NO/exp(rr_use2$l*BClag01),
             AN_H = (exp(rr_use2$h*BClag01)-1)*PSN_NO/exp(rr_use2$h*BClag01))
    
    #Calculate attributable number in temperature stratum 3
    sub3 <- sub %>% 
      filter(qgroup == "q3") %>% 
      mutate(AN = (exp(rr_use3$beta*BClag01)-1)*PSN_NO/exp(rr_use3$beta*BClag01),
             AN_L = (exp(rr_use3$l*BClag01)-1)*PSN_NO/exp(rr_use3$l*BClag01),
             AN_H = (exp(rr_use3$h*BClag01)-1)*PSN_NO/exp(rr_use3$h*BClag01))
    
    #Calculate attributable number in temperature stratum 1 by year
    sub_ann1 <- aggregate(sub1[, c(16:18)], list(format(sub1$date, "%Y")), FUN=sum, na.rm=T) %>% 
      mutate(district = i)
    sub_ann1 <- left_join(sub_ann1,subset(sum_add,sum_add$Group.2=="q1"),by="Group.1")
    
    #Calculate attributable number in temperature stratum 2 by year
    sub_ann2 <- aggregate(sub2[, c(16:18)], list(format(sub2$date, "%Y")), FUN=sum, na.rm=T) %>% 
      mutate(district = i)
    sub_ann2 <- left_join(sub_ann2,subset(sum_add,sum_add$Group.2=="q2"),by="Group.1")
    
    #Calculate attributable number in temperature stratum 3 by year
    sub_ann3 <- aggregate(sub3[, c(16:18)], list(format(sub3$date, "%Y")), FUN=sum, na.rm=T) %>% 
      mutate(district = i)
    sub_ann3 <- left_join(sub_ann3,subset(sum_add,sum_add$Group.2=="q3"),by="Group.1")
    
    
    ann_final1  <- rbind(ann_final1,sub_ann1)
    ann_final2  <- rbind(ann_final2,sub_ann2)
    ann_final3  <- rbind(ann_final3,sub_ann3)
  }
  
  #Pooled city-specific attributable number to nationwide in each temperature stratum
  af_all1 <- ann_final1 %>% 
    group_by(Group.1) %>% 
    summarise(AN = sum(AN,na.rm = T),PSN_NO = sum(PSN_NO,na.rm=T),
              AN_L = sum(AN_L,na.rm = T),AN_H = sum(AN_H,na.rm = T))
  
  af_all2 <- ann_final2 %>% 
    group_by(Group.1) %>%
    summarise(AN = sum(AN,na.rm = T),PSN_NO = sum(PSN_NO,na.rm=T),
              AN_L = sum(AN_L,na.rm = T),AN_H = sum(AN_H,na.rm = T))
  
  af_all3 <- ann_final3 %>% 
    group_by(Group.1) %>% 
    summarise(AN = sum(AN,na.rm = T),PSN_NO = sum(PSN_NO,na.rm=T),
              AN_L = sum(AN_L,na.rm = T),AN_H = sum(AN_H,na.rm = T))
  
  #Calculate annual attributable fraction in each temperature stratum
  af_all1 <- af_all1 %>% 
    mutate(AF = (AN/PSN_NO)*100,
           AF_L = (AN_L/PSN_NO)*100,
           AF_H = (AN_H/PSN_NO)*100,
           Q = "Q1")
  
  af_all2 <- af_all2 %>% 
    mutate(AF = (AN/PSN_NO)*100,
           AF_L = (AN_L/PSN_NO)*100,
           AF_H = (AN_H/PSN_NO)*100,
           Q = "Q2")
  
  af_all3 <- af_all3 %>% 
    mutate(AF = (AN/PSN_NO)*100,
           AF_L = (AN_L/PSN_NO)*100,
           AF_H = (AN_H/PSN_NO)*100,
           Q = "Q3")
  
  af_all <- rbind(af_all,af_all1,af_all2,af_all3)
  
 
write.csv(af_all,"BC_AF_by_temp_by_year.csv")


#Calculate total attributable fraction
fin_HAN <- read.csv("BC_AF_by_temp_by_year.csv")
fin_HAN_total <- aggregate(
  fin_HAN[, c(2:8)],  
  by = list( Q = fin_HAN$Q),  
  FUN = sum, 
  na.rm = TRUE  
)
fin_HAN_total["AF_all"] <- fin_HAN_total["AN"] / fin_HAN_total["PSN_NO"] * 100
fin_HAN_total["AF_low"] <- fin_HAN_total["AN_L"] / fin_HAN_total["PSN_NO"] * 100
fin_HAN_total["AF_high"] <- fin_HAN_total["AN_H"] / fin_HAN_total["PSN_NO"] * 100
