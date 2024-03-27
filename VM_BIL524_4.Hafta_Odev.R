#####  R'de kullanılan temel fonksiyonlar #####

#çalışılan dosyaların kayıt yerini öğrenme ve oluşturma(çalışma dizini)
getwd()
setwd()

#Bir fonksiyon oluşturma
karekok<-function(x){return(x^0.5)} #karekok alma fonksiyonu
print(karekok(100))

#İkişer sayma aracı
ikisersay <- seq(2, 10, by=2) #başlama sayısı, bitiş sayısı, kaçar arlıkla
ikisersay

#veri dosyası yükleme
#yerel diskden yükleme
veri <- read.csv("C:/Users/atila/OneDrive/Masaüstü/R W D/Youth_Tobacco_Survey__YTS__Data.csv")

#R programındaki mevcut örnek veri dosyalarını yükleme, görüntüleme 
install.packages("DALEX")
library(DALEX)
library(dplyr)
library(caret)
data("titanic")
titanic
View(titanic)
titan <- titanic  #titanic veri setini yeniden adlandırma (başka isime atama)
View(titan)
head(titan) # verinin ilk 6 satırını görmek için
dim(titan)   # verinin satır ve sütün sayısını görmek için

#Değişken isimlerini değiştirmek
install.packages("dplyr")
library(dplyr)
titan_cins <- rename(titan, cinsiyet=gender) #gender, cinsiyet olarak değişti.
head(titan_cins)
#veriyi dışarıya aktarma(not: başarısız oldu)
install.packages("readr")
library(readr)

titan_cins_yeni <- titan_cins
write_csv(titan_cins_yeni, path="C:/Users/atila/OneDrive/Masaüstü/R W D")
View(titan_cins_yeni)
#yapılan işlemlerin görüntülenmesi

ls()

#ilgili bir paketten ilgili fonksiyonu 

install.packages("tidyverse")
library(tidyverse)

df <- data.frame(titan_cins)
titan_cins
dim(titan_cins)
head(titan_cins)
head(df) #aynı veri

#verinin tibble formatında yazılması

install.packages("tidyverse")
library(tidyverse)

df_tibble = as_table(df)
df_tibble

#: ile yeniden isim verme

df_tibble_yas = dplyr::rename(titan,yas=age)

#sütün adlarını büyük-küçük harfe dönüştürmek

titanic_b = dplyr::rename_all(titanic,toupper)
View(titanic_b)

#alt kümeler
names(titanic)
names(titanic_b)

titanic_b_SUR=titanic_b$SURVIVED
titanic_b_SUR
dim(titanic_b_SUR) #classuygun değil null döndürür vektör olarak tanımlıdeğil

#select fonksiyonu
titanic_age=select(titanic,age)
titanic_age
dim(titanic_age) # vektör olarak tanımlı

#filter fonksiyonu  (Not:başarısız oldu)
titanic_1 = filter(titanic, fare>20 | fare<25 )

str(titanic_1)
View(titanic_1)

#Pipe fonksiyonu
View(titanic)
names(titanic)

titanic_pip=titanic %>% filter(age >30 & fare==13.000) %>% select(gender,country,age,fare)
View(titanic_pip)

#yeni sütün oluşturma

titanic_pip$yenicolon=titanic_pip$fare*3 #fare colonundaki değerleri 3 ile çarpıp yeni bir kolon oluşturuyoruz
View(titanic_pip)


#mutate fonksiyonu

titanic_mut = mutate(titanic, colon1=fare*3)
View(titanic_mut)

titanic_mut2 = mutate(titanic,
                      ucret= ifelse(
                       fare <=20,
                       "düşük",
                        ifelse(fare <=80,
                          "orta"  ,
                         "yüksek"
   )))

View(titanic_mut2)

#sütün sıralama

arrange(titanic_mut2, desc(fare))

#transmute fonksiyonu
transmute(titanic, newcol=fare/2,fare, age, country)



