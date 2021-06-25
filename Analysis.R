library(readxl)
library(tidyverse)
library(ggpubr)
library(xlsx)
library(ggplot2)
library(stringr)
library(ggstatsplot)
library(stats)

#Load dataset
DB_RIO <- read_excel("20103 Base de datos consolidada EDAS.xlsx",sheet = "DB_RIO")

#Exploration

# how to find outliers in r - upper and lower range
# how to find outliers in r
parametro="DBO5 (mg/L)"

x <- unlist(DB_RIO %>% filter(`Parámetro`==parametro) %>% select(`VALOR_NEW`))

Q <- quantile(x, probs=c(.25, .75), na.rm = TRUE)
# how to find outliers in r - calculate Interquartile Range
iqr <- IQR(x,na.rm=TRUE)
up <-  Q[2]+1.5*iqr # Upper Range  
low<- Q[1]-1.5*iqr # Lower Range
#using mad to detect outliers
low_mad <- median(x,na.rm = T)-3*mad(x,na.rm = T)
up_mad <- median(x,na.rm = T)+3*mad(x, na.rm = T)

g1 <-   ggplot(data=DB_RIO %>% filter(`Parámetro`==parametro), aes(x=`Estación`, y=VALOR_NEW))+ 
        geom_boxplot()+ geom_jitter(aes(color=`Temporada`),size=0.8)+ 
        geom_hline(yintercept = up,color="blue")+geom_hline(yintercept = low,color="blue")+
        geom_hline(yintercept=up_mad)+geom_hline(yintercept = low_mad)+
        ylim(0,NA)+theme_bw()+ylab("Nitrógeno de Nitrato (mg/L)")+xlab(NULL)+
        scale_x_discrete(limits=c("Río Cruces en Punucapa",
                                  "Río CalleCalle antes Cuesta Soto",
                                  "R Calle Calle antes C Cau Cau","Cuesta Soto", 
                                  "Los Pelúes", "Las Mulatas",
                                  "Río Valdivia en Transbordador",
                                  "Angachilla","Tornagaleones",
                                  "Valdivia entre Guacamayo y Cutipay"), 
                         labels = function(`Estación`) str_wrap(`Estación`, width = 15))+
        theme(axis.text.x = element_text(size=9, face="bold"),
              legend.title = element_text(size = 11),
              legend.text = element_text(size = 11),
              axis.text.y = element_text(size=11),
              legend.position="bottom")




ggplot(data=DB_RIO %>% filter(`Parámetro`=="DQO (mg/L)"), aes(x=`Estación`, y=VALOR_NEW))+ 
        geom_line(aes())+ geom_jitter(aes(color=`Temporada`),size=0.8)+ 
        geom_hline(yintercept = up,color="blue")+geom_hline(yintercept = low,color="blue")+
        geom_hline(yintercept=up_mad)+geom_hline(yintercept = low_mad)+
        ylim(0,NA)+theme_bw()+ylab("DQO (mg/L)")+xlab(NULL)+
             theme(axis.text.x = element_text(size=9, face="bold"),
              legend.title = element_text(size = 11),
              legend.text = element_text(size = 11),
              axis.text.y = element_text(size=11),
              legend.position="bottom")

png('N-NO3.png', units="in", width=10, height=5, res=400)
g1
dev.off()

# g2<-ggplot(data=DB_RIO %>% filter(`Parámetro`==parametro & VALOR_NEW>low & VALOR_NEW<up), aes(x=`Estación`, y=VALOR_NEW))+ 
#         geom_boxplot()+ geom_jitter(aes(color=`Temporada`))+ 
#         ylim(0,NA)+theme_bw()+ylab(parametro)+xlab(NULL)+
#         scale_x_discrete(limits=c("Río Cruces en Punucapa",
#                                   "Río CalleCalle antes Cuesta Soto",
#                                   "R Calle Calle antes C Cau Cau","Cuesta Soto", 
#                                   "Los Pelúes", "Las Mulatas",
#                                   "Río Valdivia en Transbordador",
#                                   "Angachilla","Tornagaleones",
#                                   "Valdivia entre Guacamayo y Cutipay"), 
#                          labels = function(`Estación`) str_wrap(`Estación`, width = 15))+
#         theme(axis.text.x = element_text(size=10, face="bold"),
#               legend.title = element_text(size = 11),
#               legend.text = element_text(size = 11),
#               axis.text.y = element_text(size=11),
#               legend.position="bottom")

# Basic summary stats
#::::::::::::::::::::::::::::::::::::::::::::::::
# Compute summary statistics
summary.stats <- DB_RIO %>% select(`Estación`,`Parámetro`,VALOR_NEW) %>% filter(`Parámetro`=="DBO5 (mg/L)") %>% 
        group_by(`Estación`) %>%
        #get_summary_stats(type="full")
        get_summary_stats(show = c("n","min","max","q1","median","q3","iqr","mean")) %>% select(-variable)
summary.stats

#quantile(DB_RIO %>% filter(`Parámetro`=="DBO5 (mg/L)") %>% select(VALOR_NEW), probs=c(.25, .75), na.rm = TRUE)

#save summary table in excel
openxlsx::write.xlsx(summary.stats, file = "dbo.xlsx")


summary.stats <- DB_RIO %>% select(`Estación`,`Parámetro`,VALOR_NEW) %>% filter(`Parámetro`=="Coliformes Fecales (NMP/100 ml)") %>% 
        group_by(`Estación`) %>%
        #get_summary_stats(type="full")
        get_summary_stats(show = c("n","min","max","q1","median","q3","iqr","mean")) %>% select(-variable)
summary.stats

#quantile(DB_RIO %>% filter(`Parámetro`=="DBO5 (mg/L)") %>% select(VALOR_NEW), probs=c(.25, .75), na.rm = TRUE)

openxlsx::write.xlsx(summary.stats, file = "CF.xlsx")



summary.stats <- DB_RIO %>% select(`Estación`,`Parámetro`,VALOR_NEW) %>% filter(`Parámetro`=="Fósforo Total (mg/L)") %>% 
        group_by(`Estación`) %>%
        #get_summary_stats(type="full")
        get_summary_stats(show = c("n","min","max","q1","median","q3","iqr","mean")) %>% select(-variable)
summary.stats

#quantile(DB_RIO %>% filter(`Parámetro`=="DBO5 (mg/L)") %>% select(VALOR_NEW), probs=c(.25, .75), na.rm = TRUE)

openxlsx::write.xlsx(summary.stats, file = "P Total.xlsx")



summary.stats <- DB_RIO %>% select(`Estación`,`Parámetro`,VALOR_NEW) %>% filter(`Parámetro`=="Oxígeno Disuelto (mg/L)") %>% 
        group_by(`Estación`) %>%
        #get_summary_stats(type="full")
        get_summary_stats(show = c("n","min","max","q1","median","q3","iqr","mean")) %>% select(-variable)
summary.stats

#quantile(DB_RIO %>% filter(`Parámetro`=="DBO5 (mg/L)") %>% select(VALOR_NEW), probs=c(.25, .75), na.rm = TRUE)

openxlsx::write.xlsx(summary.stats, file = "OD.xlsx")



summary.stats <- DB_RIO %>% select(`Estación`,`Parámetro`,VALOR_NEW) %>% filter(`Parámetro`=="Nitrógeno de Nitrato") %>% 
        group_by(`Estación`) %>%
        #get_summary_stats(type="full")
        get_summary_stats(show = c("n","min","max","q1","median","q3","iqr","mean")) %>% select(-variable)
summary.stats

#quantile(DB_RIO %>% filter(`Parámetro`=="DBO5 (mg/L)") %>% select(VALOR_NEW), probs=c(.25, .75), na.rm = TRUE)

openxlsx::write.xlsx(summary.stats, file = "N-NO3.xlsx")



# summary.stats <- DB_RIO %>% select(`Estación`,`Parámetro`,VALOR_NEW) %>% filter(`Parámetro`=="Nitrógeno de Nitrito") %>% 
#         group_by(`Estación`) %>%
#         #get_summary_stats(type="full")
#         get_summary_stats(show = c("n","min","max","q1","median","q3","iqr","mean")) %>% select(-variable)
# summary.stats
# 
# #quantile(DB_RIO %>% filter(`Parámetro`=="DBO5 (mg/L)") %>% select(VALOR_NEW), probs=c(.25, .75), na.rm = TRUE)
# # 
# openxlsx::write.xlsx(summary.stats, file = "N-NO2.xlsx")
