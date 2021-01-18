# `2020-fall/analysis` klasörü # 

library(rjson)
library(dplyr)
library(ggplot2)
library(plyr)
library(data.table)
library(ggthemes)
library(rdrop2)
library(shiny)
library(AER)
library(stargazer)
library(broom)
library(kableExtra)
library(pastecs)
library(pander)
library(haven)
library(tidyverse)
library(foreign)
library(readstata13)

PumsTableBase = read.dta13("~/Desktop/Emek_ps1/2020-fall/data/raw/NEW7080.dta", encoding="UTF-8")
colnames(PumsTableBase)[1]="AGE"
colnames(PumsTableBase)[2]="AGEQ"
colnames(PumsTableBase)[4]="EDUC"
colnames(PumsTableBase)[5]="ENOCENT"
colnames(PumsTableBase)[6]="ESOCENT"
colnames(PumsTableBase)[9]="LWKLYWGE"
colnames(PumsTableBase)[10]="MARRIED"
colnames(PumsTableBase)[11]="MIDATL"
colnames(PumsTableBase)[12]="MT"
colnames(PumsTableBase)[13]="NEWENG"
colnames(PumsTableBase)[16]="CENSUS"
colnames(PumsTableBase)[18]="QOB"
colnames(PumsTableBase)[19]="RACE"
colnames(PumsTableBase)[20]="SMSA"
colnames(PumsTableBase)[21]="SOATL"
colnames(PumsTableBase)[24]="WNOCENT"
colnames(PumsTableBase)[25]="WSOCENT"
colnames(PumsTableBase)[27]="YOB"

PumsTable = filter(PumsTableBase, PumsTableBase$CENSUS == "70")
PumsTable80 = filter(PumsTableBase, PumsTableBase$CENSUS == "80")

PumsTable$Z = (PumsTable$QOB == 2 | PumsTable$QOB == 3 | PumsTable$QOB == 4) * 1
ttest.lwklywge <- t.test(PumsTable$LWKLYWGE ~ Z, PumsTable)
ttest.educ     <- t.test(PumsTable$EDUC ~ Z, PumsTable)
library(systemfit)
sur  <- systemfit(list(first  = PumsTable$EDUC ~  Z,second = PumsTable$LWKLYWGE ~ Z),data   = PumsTable,method = "SUR")
wald <- deltaMethod(sur,"second_Z / first_Z")
wald.estimate <- (mean(PumsTable$lwklywge[PumsTable$Z == 1]) - mean(PumsTable$lwklywge[PumsTable$Z == 0])) /
    +     (mean(PumsTable$educ[PumsTable$Z  == 1]) - mean(PumsTable$educ[PumsTable$Z == 0]))
wald.se       <- wald.estimate^2 
ols <- lm(PumsTable$LWKLYWGE ~ PumsTable$EDUC, PumsTable)
lwklywge.row <- c(ttest.lwklywge$estimate[1],
                   ttest.lwklywge$estimate[2],
                   ttest.lwklywge$estimate[2] - ttest.lwklywge$estimate[1])
educ.row     <- c(ttest.educ$estimate[1],
                   ttest.educ$estimate[2],
                   ttest.educ$estimate[2] - ttest.educ$estimate[1])
wald.row.est <- c(NA, NA, wald$Estimate)
wald.row.se  <- c(NA, NA, wald$SE)
ols.row.est <- c(NA, NA, summary(ols)$coef["PumsTable$EDUC", 'Estimate'])
ols.row.se  <- c(NA, NA, summary(ols)$coef["PumsTable$EDUC" , 'Std. Error'])
table           <- rbind(lwklywge.row, educ.row,wald.row.est, wald.row.se, ols.row.est, ols.row.se)
colnames(table) <- c("1.Çeyrekte Doğanlar",
                     "Diğer Çeyrekte Doğanlar",
                    "Fark")
rownames(table) <- c("Haftalık Kazanç",
                      "Eğitim Yılı",
                      "Wald estimate",
                      "Wald std hata",
                      "OLS estimate",
                      "OLS std hata")
()table %>%
  kbl() %>%
  kable_styling()


fit_IV_70 = ivreg(PumsTable$LWKLYWGE ~ PumsTable$EDUC + PumsTable$AGE + PumsTable$AGEQ + PumsTable$MARRIED + factor(PumsTable$YOB) |  PumsTable$AGE + PumsTable$AGEQ + PumsTable$MARRIED +  factor(PumsTable$YOB) * factor(PumsTable$QOB), model=TRUE)

tidy(fit_IV_70) %>% pander  

fit_IV_80 = ivreg(PumsTable80$LWKLYWGE ~ PumsTable80$EDUC + PumsTable80$AGE + PumsTable80$AGEQ +  PumsTable80$MARRIED + factor(PumsTable80$YOB) |  PumsTable80$AGE + PumsTable80$AGEQ + PumsTable80$MARRIED + factor(PumsTable80$YOB) * factor(PumsTable80$QOB), model=TRUE)

  tidy(fit_IV_80) %>% pander



                      
                      
                      
                      
                      
    






        

                      
                      
                      


