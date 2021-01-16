# `Faruk Sarikaya Odev2 MD` klasörü
library(rjson)
library(dplyr)
library(kableExtra)
library(pastecs)
library(tidyverse)
library(foreign)
library(magrittr)
library(qwraps2)
library(formattable)
library(haven)
library(readstata13)
library(broom)

#Verileri tablolara aktarıyoruz
Table2010 = read.dta13("~/Desktop/ikt533_Emek/2020-fall/data/raw/hia/hia2010.dta")
Table2011 = read.dta13("~/Desktop/ikt533_Emek/2020-fall/data/raw/hia/hia2011.dta")
Table2012 = read.dta13("~/Desktop/ikt533_Emek/2020-fall/data/raw/hia/hia2012.dta")
Table2013 = read.dta13("~/Desktop/ikt533_Emek/2020-fall/data/raw/hia/hia2013.dta")

#Tabloları joinlemeden önce period ayrımı yapabilmek için tüm dataya period ve Foreign_trade eklenmiştir.
Table2010$period= c("2010")
Table2010$foreign_trade= c("299.428")
Table2011$period= c("2011")
Table2011$foreign_trade= c("375.749")
Table2012$period= c("2012")
Table2012$foreign_trade= c("389.007")
Table2013$period= c("2013")
Table2013$foreign_trade= c("403.464")


#Ayrı tablolara aktarılmış veriyi tek bir tablo içerisine join ederiyoruz. Böylece değişkenlerimizi daha kolay çekeceğiz. 
hiaData1 = full_join(Table2010,Table2011)
hiaData2 = full_join(Table2012,Table2013)
hia_Data = full_join(hiaData1,hiaData2)

#Tek tabloda çalışacağımız için ayrı tablolara gerek kalmamıştır. Diskte yer kaplamaması için siliyoruz
remove(Table2010)
remove(Table2011)
remove(Table2012)
remove(Table2013)

#NativePopulation:Yabancı ülkelerde doğanlar ve 2010 yılından sonra türkiyede ikamet edenler hariç bırakılarak 15-64 yas grubu içerisinde olan tüm işçi nüfusu"
nat_pop = hia_Data %>% 
filter(S6_GRUP %in% c(4:13)) %>% 
filter(NUTS2 %in% c(12,13,20,21,22,23,24,25,26)) %>% 
filter(!(S7 ==2 & S8B <= 2010))

#Tablo 3 - Treatment:  MEN ve AGE ortalamaları hesaplandı
mean_T= sqldf::sqldf ("select count (S3) as Cinsiyet, S3, AVG(S6) as AGE, Period From nat_pop where NUTS2 in(12,13,24,25,26) Group By period, S3")
mean_T2 = sqldf::sqldf("Select Cinsiyet,  SUM(Cinsiyet) as MEN, AVG (AGE) as AGE, period from mean_T group by period")
mean_T2$MEN = mean_T2$Cinsiyet / mean_T2$MEN

#Tablo 3 - Treatment: Married, Yüksek Okul ve Urban ortalamaları hesaplandı
mean_T3 = sqldf::sqldf ("select SUM (S24) Married, S24 as MD,S13 as HS, KIRKENT as Urban, Period From nat_pop where NUTS2 in(12,13,24,25,26) Group By period, S24, S13, KIRKENT")
mean_T2$Married = (sqldf::sqldf ("select SUM(Married) MR From mean_T3 Where MD = 2 Group By period")) / (sqldf::sqldf ("select SUM (Married) MR From mean_T3 Group By period"))
mean_T2$HS = (sqldf::sqldf ("select SUM (Married) as HS From mean_T3 Where HS in (4,5,6) Group By period")) / (sqldf::sqldf ("select SUM (Married) as HS From mean_T3 Group By period"))
mean_T2$Urban = (sqldf::sqldf ("select SUM (Married) as Urban From mean_T3 Where Urban = 2 Group By period")) / (sqldf::sqldf ("select SUM (Married) as Urban From mean_T3 Group By period"))
mean_T2$Obser= sqldf::sqldf ("select count (S3) as Observation From nat_pop where NUTS2 in(12,13,24,25,26) Group By period")

#Tablo 3 - Control:  MEN ve AGE ortalamaları hesaplandı
mean_C= sqldf::sqldf ("select count (S3) as Cinsiyet, S3, AVG(S6) as AGE, Period From nat_pop where NUTS2 in(20,21,22,23) Group By period, S3")
mean_C2 = sqldf::sqldf("Select Cinsiyet,  SUM(Cinsiyet) as MEN, AVG (AGE) as AGE, period from mean_C group by period")
mean_C2$MEN = mean_C2$Cinsiyet / mean_C2$MEN

#Tablo 3 - Control: Married, Yüksek Okul ve Urban ortalamaları hesaplandı
mean_C3 = sqldf::sqldf ("select SUM (S24) Married, S24 as MD,S13 as HS, KIRKENT as Urban, Period From nat_pop where NUTS2 in(20,21,22,23) Group By period, S24, S13, KIRKENT")
mean_C2$Married = (sqldf::sqldf ("select SUM(Married) MR From mean_C3 Where MD = 2 Group By period")) / (sqldf::sqldf ("select SUM (Married) MR From mean_C3 Group By period"))
mean_C2$HS = (sqldf::sqldf ("select SUM (Married) as HS From mean_C3 Where HS in (4,5,6) Group By period")) / (sqldf::sqldf ("select SUM (Married) as HS From mean_C3 Group By period"))
mean_C2$Urban = (sqldf::sqldf ("select SUM (Married) as Urban From mean_C3 Where Urban = 2 Group By period")) / (sqldf::sqldf ("select SUM (Married) as Urban From mean_C3 Group By period"))
mean_C2$Obser= sqldf::sqldf ("select count (S3) as Observation From nat_pop where NUTS2 in(20,21,22,23) Group By period")

#Tablo 3 Tabloları ters çevirdim
mean_T2 <- data.frame(t(mean_T2[-1]))
colnames(mean_T2) = c(2010,2011,2012,2013)
mean_C2 <- data.frame(t(mean_C2[-1]))
colnames(mean_C2) = c(2010,2011,2012,2013)

Odev2_Table3 = full_join(mean_T2,mean_C2)
Odev2_Table3 <- Odev2_Table3[-c(3), ]
text_tbl3 <- data.frame("Degiskenler" = c("Men", "Age", "Married", "High School and above", "Urban", " #of observations","Men", "Age", "Married", "High School and above", "Urban", " #of observations"))
Odev2_Table3$items = text_tbl3
Odev2_Table3 <- subset(Odev2_Table3, select=c(5,1,2,3,4))

#Tablo 3 Tamamlandı
kbl(Odev2_Table3) %>% kable_styling(bootstrap_options = "striped", full_width = F, position = "left") %>% pack_rows("Treatment Area", 1, 7) %>% pack_rows("Control Area", 7, 12)

# Table 5 start
# Create Dummy Variables
#tre_per :  Treatment zamanından öncesi 0, sonrası 1
#tre_area: Bölge olarak tedaviye maruz kalanlar 1, kalmayanlar 0
#R_T: Zaman ve tedavi arasında bir etkileşim oluşturuyoruz
#inf_emp: Sosyal Güvenlik Kuruluşuna kayıtlı değil 1, else 0
#for_emp: Sosyal Güvenlik Kuruluşuna kayıtlı 1, else 0

# Table 5 değerleri: 
dif_np = nat_pop %>% 
mutate(tre_per= as_factor(ifelse(period >= 2012, 1, 0)), 
tre_area = as.integer(ifelse(NUTS2 %in% c(12,13,24,25,26),1,0)), 
inf_emp =  ifelse(S42 == 2,1,0),
for_emp =  ifelse(S42 == 1,1,0),
gender_men = ifelse(S3==1,1,0),
gender_women = ifelse(S3==2,1,0),
maritual = ifelse(DURUM == 1 & S24 == 2, 1,0),
low_edu = ifelse(DURUM == 1 & S13 %in% c(1,2,3), 1,0),
high_edu = ifelse(DURUM == 1 & S13 %in% c(4,5,6), 1,0),
u_st = ifelse(DURUM == 1 & KIRKENT == 2, 1,0))

#1.Kolon
R_Treg = lm(inf_emp ~ tre_area * tre_per + maritual + u_st + low_edu + gender_men, data = dif_np)
tidy(R_Treg)

#2.Kolon  we control for the annual trade volumes
 F_reg = lm(log(as.integer(foreign_trade)) ~ (tre_area * tre_per) + maritual + u_st + low_edu + gender_men, data = dif_np)
tidy(F_reg)

#6.Tablo labor force
 lf_reg = lm(l_force ~ (tre_area * tre_per) + maritual + u_st + low_edu + gender_men, data = dif_np)
tidy(lf_reg)






 