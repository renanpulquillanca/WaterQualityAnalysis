library(readxl)
library(tidyverse)
library(ggpubr)
library(xlsx)

library(ggplot2)
library(stringr)


ggplot(data=DB_RIO %>% filter(`Parámetro`=="Nitrógeno Kjeldahl (mg N/L)"), aes(x=`Estación`, y=VALOR_NEW))+ 
        geom_boxplot()+ geom_jitter(aes(color=`Temporada`))+ 
        ylim(0,NA)+theme_bw()+ylab("mg/L")+xlab(NULL)+
        scale_x_discrete(limits=c("Río Cruces en Punucapa",
                                  "Río CalleCalle antes Cuesta Soto",
                                  "R Calle Calle antes C Cau Cau","Cuesta Soto", 
                                  "Los Pelúes", "Las Mulatas",
                                  "Río Valdivia en Transbordador",
                                  "Angachilla","Tornagaleones",
                                  "Valdivia entre Guacamayo y Cutipay"), 
                         labels = function(`Estación`) str_wrap(`Estación`, width = 15))+
        theme(axis.text.x = element_text(size=14, face="bold"),
              legend.title = element_text(size = 15),
              legend.text = element_text(size = 14),
              axis.text.y = element_text(size=15),
              legend.position="bottom")


DB_RIO <- read_excel("20103 Base de datos consolidada EDAS.xlsx",sheet = "DB_RIO")

# Basic summary stats
#::::::::::::::::::::::::::::::::::::::::::::::::
# Compute summary statistics
summary.stats <- DB_RIO %>% select(`Estación`,`Parámetro`,VALOR_NEW) %>% filter(`Parámetro`=="DBO5 (mg/L)") %>% 
        group_by(`Estación`) %>%
        #get_summary_stats(type="full")
        get_summary_stats(show = c("n","min","max","q1","median","q3","iqr","mean")) %>% select(-variable)
summary.stats

#quantile(DB_RIO %>% filter(`Parámetro`=="DBO5 (mg/L)") %>% select(VALOR_NEW), probs=c(.25, .75), na.rm = TRUE)

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



summary.stats <- DB_RIO %>% select(`Estación`,`Parámetro`,VALOR_NEW) %>% filter(`Parámetro`=="Nitrógeno de Nitrito") %>% 
        group_by(`Estación`) %>%
        #get_summary_stats(type="full")
        get_summary_stats(show = c("n","min","max","q1","median","q3","iqr","mean")) %>% select(-variable)
summary.stats

#quantile(DB_RIO %>% filter(`Parámetro`=="DBO5 (mg/L)") %>% select(VALOR_NEW), probs=c(.25, .75), na.rm = TRUE)

openxlsx::write.xlsx(summary.stats, file = "N-NO2.xlsx")
