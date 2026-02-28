
remove(list = ls())
library(tidyverse)
library(mgcv)
library(lme4)
library(rio)
setwd("E:/OneDrive/")

#### IMPORT DATA####
panel_0 <- import("Github_0227/Example_data/Example_panel_data.csv")

###### 2.1 对BC的修饰 ####
panelBC <- matrix(0,0,14)
colnames(panelBC) <- c('pollutant','lag','outcome',"Group","beita",'se','t','pvalue','mean','lower','upper',"pc",'lcl','ucl')
for (j in c("Alls")) {
  for (i in c("1","2","3","4","5")){
    cat(j," ",i)
    
    panel_0[,"outcome"]=panel_0[,j];
    panel_0[,"observe"]=panel_0[,paste0("BC_",i)];
    panel_0[,"tempT"]=panel_0[,paste0("Temp_",i)];
    panel_0[,"tempRH"]=panel_0[,paste0("RH_",i)]; 
    observe_1=subset(panel_0,tempT<=20.9)
    observe_2=subset(panel_0,20.9<tempT & tempT<=28.9)
    observe_3=subset(panel_0,tempT>28.9)    
    tryCatch( { 
      mmodel_1 <- lme4::glmer(outcome ~ 
                                age+
                                factor(gender)+
                                BMI+
                                factor(DOW)+
                                DOS+
                                SDOS.10000+
                                tempRH+
                                factor(hour)+
                                observe+
                                (1 | subject), na.action=na.omit, data = observe_1,
                              family = poisson,nAGQ=0)
      mmodel_2 <- lme4::glmer(outcome ~ 
                                age+
                                factor(gender)+
                                BMI+
                                factor(DOW)+
                                DOS+
                                SDOS.10000+
                                tempRH+
                                factor(hour)+
                                observe+
                                (1 | subject), na.action=na.omit, data = observe_2,
                              family = poisson,nAGQ=0)
      mmodel_3 <- lme4::glmer(outcome ~ 
                                age+
                                factor(gender)+
                                BMI+
                                factor(DOW)+
                                DOS+
                                SDOS.10000+
                                tempRH+
                                factor(hour)+
                                observe+
                                (1 | subject), na.action=na.omit, data = observe_3,
                              family = poisson,nAGQ=0)
      
      takevalue=length(summary(mmodel_1)$coefficients)
      x1=summary(mmodel_1)$coefficients[takevalue/4];
      x2=summary(mmodel_1)$coefficients[takevalue*2/4];
      x3=summary(mmodel_1)$coefficients[takevalue*3/4];
      x4=summary(mmodel_1)$coefficients[takevalue];
      x5=exp(1.9*x1);x6=exp(1.9*(x1-(1.96*x2)));x7=exp(1.9*(x1+(1.96*x2)));
      x8=(exp(1.9*x1)-1)*100;x9=(exp(1.9*(x1-(1.96*x2)))-1)*100;x10=(exp(1.9*(x1+(1.96*x2)))-1)*100;
      x22=1
      panelBC %<>% rbind(c("BC",i,j,x22,x1,x2,x3,x4,x5,x6,x7,x8,x9,x10));
      
      takevalue=length(summary(mmodel_2)$coefficients)
      x1=summary(mmodel_2)$coefficients[takevalue/4];
      x2=summary(mmodel_2)$coefficients[takevalue*2/4];
      x3=summary(mmodel_2)$coefficients[takevalue*3/4];
      x4=summary(mmodel_2)$coefficients[takevalue];
      x5=exp(1.9*x1);x6=exp(1.9*(x1-(1.96*x2)));x7=exp(1.9*(x1+(1.96*x2)));
      x8=(exp(1.9*x1)-1)*100;x9=(exp(1.9*(x1-(1.96*x2)))-1)*100;x10=(exp(1.9*(x1+(1.96*x2)))-1)*100;
      x22=2
      panelBC %<>% rbind(c("BC",i,j,x22,x1,x2,x3,x4,x5,x6,x7,x8,x9,x10));
      
      
      takevalue=length(summary(mmodel_3)$coefficients)
      x1=summary(mmodel_3)$coefficients[takevalue/4];
      x2=summary(mmodel_3)$coefficients[takevalue*2/4];
      x3=summary(mmodel_3)$coefficients[takevalue*3/4];
      x4=summary(mmodel_3)$coefficients[takevalue];
      x5=exp(1.9*x1);x6=exp(1.9*(x1-(1.96*x2)));x7=exp(1.9*(x1+(1.96*x2)));
      x8=(exp(1.9*x1)-1)*100;x9=(exp(1.9*(x1-(1.96*x2)))-1)*100;x10=(exp(1.9*(x1+(1.96*x2)))-1)*100;
      x22=3
      panelBC %<>% rbind(c("BC",i,j,x22,x1,x2,x3,x4,x5,x6,x7,x8,x9,x10));
      
    },
    error = function(e){
      print(c(i,j,'error'))
    }
    )
    
  }
}
write.csv(panel_0,"Github_0227/Example_data/Example_panel_data.csv")


