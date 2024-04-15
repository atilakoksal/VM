        

          #***************** ÖDEV: Keşifsel Veri Analizi RStudio Uygulaması ************#
        
        
        #********************** A- Veri Üreterek Keşifsel Veri Analizi :*********************
        #************************************************************************************
       

#Uygulamada kullanılmak üzere rasgele bir veri seti oluşturma:

#Gerekli paketleri indirme ve kütüphaneleri kurma:
install.packages("MASS")
library(MASS)

# Veri setini oluşturma

# Aynı rasgele verilerin tekrar üretebilmek için seed belirleme:
set.seed(123) 

# 50'şer adet x1, x2, x3 bağımsız değişkenleri ve  y bağımlı değişkeni elde etme:
# n: veri adedi, mean: değişkenlerin ortalama değeri, sd:değişkenlerin standart sapması
n <- 50 
x1 <- rnorm(n, mean  = 15, sd = 3) # ortalaması 15, standart sapması 3 olan 50 adet veri üretimi
x2 <- rnorm(n, mean = 8, sd = 2) 
x3 <- rnorm(n, mean = 6, sd = 1) 
y <- 2 + 3*x1 - 1.5*x2 + 10*x3  + rnorm(n, mean = 0, sd = 1.5) #sondaki terim hata terimidir.

#Elde edile sentetik verileri veri çerçevesine dönüştürme:
df <- data.frame(y, x1, x2, x3)

#oluşturulan df veri setini görüntüleme, özellik ve yapısını inceleme:
df
head(df) #ilk 6 gözlem
View(df) #data frame'i bütün bir tablo olarak görüntüleme.
str(df)  #değişken ve gözlem sayısı ile data tiplerini görme.
summary(df) #değişkenlerin özet istatiksel bilgilerini görme.

#Elde edilen veri setini csv ve excell formatında dışa aktarma:

install.packages("readr")
library(readr)
write_csv(df, path = "df_csv.csv")  

install.packages("writexl")
library(writexl)
write_xlsx(df, path = "df_excell.xlsx")

#(Dış kaynaktan veri okutulacak ise read_excel veya read_csv gibi fonksiyonlar kullanılır.)

#Aktarım ve Gerekli ise Dönüşüm (verileri faktör ve sayısal değere dönüştürme)
# Oluşturulan veri seti üzerinden yeni değişkenler tanımlama:
set.seed(123)
n <- 50 
x1_fac <- rnorm(n, mean  = 15, sd = 3) 
x2_fac <- rnorm(n, mean = 8, sd = 2) 
x3_fac <- rnorm(n, mean = 6, sd = 1) 
y_fac <- 2 + 3*x1 - 1.5*x2 + 10*x3  + rnorm(n, mean = 0, sd = 1.5) 

# x1_fac değişkenini faktörel değişkene dönüştürme
x1_f_factor <- as.factor(x1_fac)

# x2_fac değişkenini sayısal değişkene dönüştürme
x2_f_numeric <- as.numeric(x2_fac)

# Değişken türlerini kontrol etme
str(x1_f_factor)
str(x2_f_numeric)

#Veri kalitesi
#eksik veri tespiti ve doldurma

# Örnek veri setini oluşturma
set.seed(123)
n <- 50
x1_ev <- rnorm(n, mean  = 15, sd = 3) 
x2_ev <- rnorm(n, mean = 8, sd = 2) 
x3_ev <- rnorm(n, mean = 6, sd = 1) 
y_ev <- 2 + 3*x1 - 1.5*x2 + 10*x3  + rnorm(n, mean = 0, sd = 1.5) 

# Eksik veri tespiti
is_na_x1 <- is.na(x1_ev)
is_na_x2 <- is.na(x2_ev)

is_na_x1 # veriyi kendimiz ürettiğimiz için tüm değerler false çıkıyor.
is_na_x2 # bu eksik veri yok anlamı taşıyor.
 
        #*******************************************************************************              
              #Dışa aktardığımız "df_excell.xlsx" dosyasından bazı verileri 
              #silerek işlemi tekrarlayalım.
    
              library(readxl)
              df_ev= read_excel("df_excell_ev.xlsx")
              df_ev
              x1_ev1 <- df_ev$x1_ev1 #değişkenleri tanımlamak için
              x2_ev1 <- df_ev$x2_ev1
              
              is_na_x1 <- is.na(x1_ev1) # Eksik veri tespiti
              is_na_x2 <- is.na(x2_ev1)
              
              is_na_x1 # excell dosyasında bazı veriler silindiği için true değerler çıkıyor.
              is_na_x2 # bu eksik değerlerin varlığını gösteriyor.
              
              View(df_ev) # Çalıştırdığımızda NA değerler görünmektedir.
        #****************************************************************************
# Eksik verileri ortalama ile doldurma

mean_x1_ev1 <- mean(x1_ev1, na.rm = TRUE)  # NA olmayan değerlerin ortalaması=15.06882 
x1_filled <- ifelse(is.na(x1_ev1), mean_x1_ev1, x1_ev1) # bu değer, NA ların yerine atanıyor.

mean_x1_ev1 # ortalama değer (15.06882 )
x1_ev1      # NA ihtiva eden değişken
x1_filled   # NA yerine ortalama değer (15.06882 ) atanmış değişken

# Eksik değerleri LOCF yöntemiyle doldurma (bir önceki değerle doldurma)

install.packages("zoo")
library(zoo)

x1_ev2 =df_ev$x1_ev1             # NA ihtiva eden değişken
x1_ev2_filled <- na.locf(x1_ev2) #LOCF yöntemiyle NA'ların doldurulduğu değişken.
x1_ev2                           #NA değerler görülmektedir.
x1_ev2_filled                    #NA değerler doldurulmuş (NA'lara bir önceki değer atanmış)


# Eksik verileri medyan ile doldurma

y_ev2=df_ev$y_ev1                               # NA ihtiva eden değişken
median_y_ev <- median(y_ev2, na.rm = TRUE)      # NA olmayan değerlerin medyanı=95.44683 
y_filled <- ifelse(is.na(y_ev2), median_y_ev, y_ev2) #NA ların yerine atama

y_ev2     #NA değerler görülmektedir.
y_filled  #NA değerler medyan ile  doldurulmuş


#Aykırı değer tespiti
# Veri setini oluşturma
set.seed(123)
data <- rnorm(500)  # 0 ortalamalı bir veri üretir.
sort(data)          #üretilen veriyi sıralayarak uç değerleri görebiliriz.
                    
# Kutu grafiği oluşturma

boxplot(data) # uç değerleri grafikte görebiliriz.
              #(-2.660922798 ; -2.643148952  ve +3.241039935)
              # bu değerlerin aykırı değer olma ihtimalleri var.

        #*******************************************************************************            
            # gamma dağılımıda göre 100 adet veri üretip bir deneme yaparsak;
            
            set.seed(123)
            gamma <- rgamma(100, shape = 2)
            boxplot(gamma)  
            sort(gamma) #aykırı değer durumunun arttığı gözlemlenir.
            
        #********************************************************************************            

# Z-puanı hesaplama (gamma dağılımı ile üretilen değerlerden devam edelim)
            
z_scores <- scale(gamma) #z puanları 3 ün üstünde olanlar aykırı değer kabul edilir. 
                                  
# Aykırı değerleri belirleme
aykırıdeger <- abs(z_scores) > 3
print(aykırıdeger)  # 49. veri 3.19370376 olup true döndüğünden aykırı değerdir.

# Alt ve üst çeyreklikleri hesaplama
Q1 <- quantile(gamma, 0.25)
Q3 <- quantile(gamma, 0.75)

# Alt ve üst sınırı hesaplama
IQR <- Q3 - Q1
lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR

# Aykırı değerleri belirleme
outliers <- gamma < lower_bound | gamma > upper_bound 
print(outliers) # Çok sayıda TRUE veriyor. 
print(lower_bound) #-0.8524085 olarak bulunur
print(upper_bound) # 3.91406 olarak bulunur. bunların dışındakiler  aykırı değerdir.
sort(gamma)        # 6 adet aykırı değer vardır. (3.91406 den büyük değerler)             

#Dağılımları keşif


# Örnek veri setini oluşturma
set.seed(123)
gamma <- rgamma(100, shape = 2)

# Histogram oluşturma
hist(gamma)

# Kutu grafiği oluşturma
boxplot(gamma)

# Q-Q plot oluşturma
qqnorm(gamma)
qqline(gamma)  # veri seti normal dağılımdan sapma göstermektedir.

# Kantillerden yararlanma
summary(gamma)

# Örnek veri setini oluşturma
set.seed(123)
x <- rgamma(100, shape = 2) # Bağımsız değişken
y <- 1.3*x + rnorm(100, mean = 0, sd = 0.5) # Bağımlı değişken

# Korelasyon katsayısını hesaplama
correlation <- cor(x, y)

# Korelasyon katsayısını ekrana yazdırma
print(correlation) # korelasyon katsayısı: 0.9334061 

#multicollinearity
# Örnek veri setini oluşturma
set.seed(123)
x1 <- rgamma(100, shape = 2) # Bağımsız değişken 1
x2 <- rnorm(100) # Bağımsız değişken 2
x3 <- rnorm(100) # Bağımsız değişken 3
y <- 2*x1 + 3*x2 - 1.5*x3 + rnorm(100, mean = 0, sd = 0.5) # Bağımlı değişken


# Çoklu doğrusal regresyon modelini oluşturma
model <- lm(y ~ x1 + x2 + x3)

# Model özetini alma
summary(model)   # x1=1.93240   x2=3.00838   x3=-1.46639 bulundu.

# Bağımsız değişkenler arasındaki korelasyonu hesaplama

correlation_matrix <- cor(data.frame(x1, x2, x3))
print(correlation_matrix) #bağımsız değişkenler rasgele seçildiğinden aralarında korelasyon yok.

# corrplot paketini yükleme
install.packages("corrplot")
library(corrplot)

# Korelasyon matrisini görselleştirme

corrplot(correlation_matrix, method = "color")

# Bağımsız değişkenlerin grafiğini çizme
par(mfrow=c(1,3)) # Grafiklerin yan yana yerleştirilmesi için ayar
plot(x1, y, main = "x1 vs. y", xlab = "x1", ylab = "y", col = "blue", pch = 16)
plot(x2, y, main = "x2 vs. y", xlab = "x2", ylab = "y", col = "red", pch = 16)
plot(x3, y, main = "x3 vs. y", xlab = "x3", ylab = "y", col = "green", pch = 16)


# Veriyi standartlaştırma
x1_standardized <- scale(x1)
x2_standardized <- scale(x2)
x3_standardized <- scale(x3)
y_standardized <- scale(y)

# Standartlaştırılmış veriyi kontrol etme
summary(x1_standardized)
summary(x2_standardized)
summary(x3_standardized)
summary(y_standardized)  #Standartlaştırılmış veride mean değerleri 0 çıkar.


# caTools paketini yükleme
install.packages("caTools")
library(caTools)

# Veriyi test ve eğitim alt kümelerine böleme (70% eğitim, 30% test verisi olarak)
split <- sample.split(y, SplitRatio = 0.7) 
train_data <- subset(data.frame(x1, x2, x3, y), split == TRUE)
test_data <- subset(data.frame(x1, x2, x3, y), split == FALSE)

# Verinin boyutlarını kontrol etme
dim(train_data) # train_data 70 satır ve 4 sütündan oluşuyor.
dim(test_data)  # test_data  30 satır ve 4 sütündan oluşuyor. Veri bölme işlemi yapılmış.



      #********************** B- Veri Çekerek Keşifsel Veri Analizi :**********************
      #**************************************************************************************


# Veri kaynağı ve Tanıtımı: Ödevde kullanılmasına karar verilen veri "mtcars" adını taşımakta ve RStudio'da 
#hazır olarak bulunmaktadır.Veriler 1974 Motor Trend ABD dergisinden alınmıştır ve 32 otomobilin (1973-74 model)  
#teknik  performansını gösteren 11 önemli  veri içermektedir.

# 1 mpg	Miles/(US) gallon.........................:1 galon yakıtla kaç mil gittiği....=y 
# 2 cyl	Number of cylinders.......................:motordaki silindir sayısı..........=x1 
# 3	disp	Displacement (cu.in.)...................:inç küp olarak silindir hacmı......=x2
# 4	hp	Gross horsepower..........................:beygir gücü olarak motor gücü......=x3
# 5	drat	Rear axle ratio.........................:arka aks çevirme oranı
# 6 wt	Weight (1000 lbs).........................:1000 libre olarak ağırlık..........=x4
# 7	qsec	1/4 mile time...........................:çeyrek mile ulaşma süresi..........=x5 
# 8	vs	Engine (0 = V-shaped, 1 = straight).......:silindir sıralanışı
# 9	am	Transmission (0 = automatic, 1 = manual)..:vites tipi
# 10	gear	Number of forward gears...............:ileri vites sayısı
# 11	carb	Number of carburetors.................:karbüratör sayısı

# Yukarıdaki tabloda bağımlı ve bağımsız değişkenler ile bunlara yapılacak atamalar gösterilmiştir. 

data(mtcars) #mtcars veri setini yükleme.
head(mtcars) #ilk 6 gözlem
View(mtcars) #data frame'i bütün bir tablo olarak görüntüleme.
str(mtcars)  #değişken ve gözlem sayısı ile data tiplerini görme.
summary(mtcars) #değişkenlerin özet istatiksel bilgilerini görme.

#Kullanılacak bağımlı değişken ve bağımsız değişkenleri atama, veri çerçevesi oluşturma:

y  = mtcars$mpg
x1 = mtcars$cyl
x2 = mtcars$disp
x3 = mtcars$hp
x4 = mtcars$wt
x5 = mtcars$qsec

df = data.frame(y, x1, x2, x3, x4, x5)
View(df)
str(df)

# Eksik veri tespiti

is_na_df <- is.na(df)
print(is_na_df)    # mtcars verisi içinde NA veri bulunmadığından tüm değerler FALSE  çıkıyor.
# bu eksik veri yok anlamı taşıyor.

#Deneme yapmak için eksik veri oluşturma:
df_ev = df
# 1. sütun 2. satırdaki veriyi NA yapmak
df_ev [2, 1] <- NA
# 2. sütun 3. satırdaki veriyi NA yapmak
df_ev [3, 2] <- NA
# 3. sütun 5. satırdaki veriyi NA yapmak
df_ev [5, 3] <- NA
# 4. sütun 4. satırdaki veriyi NA yapmak
df_ev [4, 4] <- NA
# 5. sütun 6. satırdaki veriyi NA yapmak
df_ev [6, 5] <- NA
# 6. sütun 4. satırdaki veriyi NA yapmak
df_ev [4, 6] <- NA
View(df_ev) # "df_ev" veri çerçevesinde 6 adet eksik (NA) veri oluştu.
is_na_df_ev <- is.na(df_ev)
print(is_na_df_ev)  #TRUE lar NA ları gösterir.


# Eksik verileri ortalama ile doldurma

install.packages("dplyr")
install.packages("tidyr")
library(dplyr)
library(tidyr)
install.packages("zoo")  
library(zoo)

df_filled <- df_ev %>% na.aggregate() # Tüm sütunlar için NA değerleri sütun ortalamalarıyla doldurur.
print(df_filled)
print(df_ev) #karşılaştırma için

# Eksik değerleri LOCF yöntemiyle doldurma (bir önceki değerle doldurma)

install.packages("zoo")
library(zoo)

y_ev  = df_ev$y     # NA ihtiva eden değişkenler
x1_ev = df_ev$x1
x2_ev = df_ev$x2
x3_ev = df_ev$x3
x4_ev = df_ev$x4
x5_ev = df_ev$x5

y_ev_filled  <- na.locf(y_ev)     #LOCF yöntemiyle NA'ların doldurulduğu değişkenler.
x1_ev_filled <- na.locf(x1_ev)
x2_ev_filled <- na.locf(x2_ev)
x3_ev_filled <- na.locf(x3_ev)
x4_ev_filled <- na.locf(x4_ev)
x5_ev_filled <- na.locf(x5_ev)

# Eksik verileri medyan ile doldurma

y_ev  = df_ev$y                                     # NA ihtiva eden değişken
median_y_ev <- median(y_ev, na.rm = TRUE)           # NA olmayan değerlerin medyanı=95.44683 
y_filled <- ifelse(is.na(y_ev), median_y_ev, y_ev)  #NA ların yerine atama

y_ev      #NA değerler görülmektedir.
y_filled  #NA değerler medyan ile  doldurulmuş


# Z-puanı hesaplama
z_scores <- scale(df) #z puanları 3 ün üstünde olanlar aykırı değer kabul edilir. 

# Aykırı değerleri belirleme

aykırıdeger <- abs(z_scores) > 3
print(aykırıdeger)  # Tüm değerler FALSE  döndüğünden aykırı değer yoktur.
sort(z_scores)

# Kutu grafiği oluşturma
boxplot(df) # uç değerleri grafikte görebiliriz.

# Alt ve üst çeyreklikleri hesaplama

Q1 <- quantile(df$y, 0.25)
Q3 <- quantile(df$y, 0.75)

# Alt ve üst sınırı hesaplama
IQR <- Q3 - Q1
lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR

# Aykırı değerleri belirleme
outliers <- df$y < lower_bound | df$y > upper_bound # 1 adet TRUE veriyor.

print(lower_bound) # 4.3625 olarak bulunur
print(upper_bound) # 33.8625 olarak bulunur. bunların dışındakiler  aykırı değerdir.
sort(df$y)         # 1 adet aykırı değer vardır (33.9).

#diğer değişkenlerdeki aykırı değerler de aynı yöntemle saptanır.

#Dağılımları keşif

# Histogram oluşturma
hist(y)
hist(x1)
hist(x2)
hist(x3)
hist(x4)
hist(x5)

# Kutu grafiği oluşturma
boxplot(df)

# Q-Q plot oluşturma
qqnorm(df$x2)
qqline(df$x2)  # veri seti normal dağılımdan sapma göstermektedir.

# Kantillerden yararlanma
summary(df)

# Korelasyon katsayısını hesaplama
correlation1 <- cor(x1, y)
correlation2 <- cor(x2, y)
correlation3 <- cor(x3, y)
correlation4 <- cor(x4, y)
correlation5 <- cor(x5, y)

# Korelasyon katsayısını ekrana yazdırma
print(correlation1) # korelasyon katsayısı: -0.852162 
print(correlation2) # korelasyon katsayısı: -0.8475514 
print(correlation3) # korelasyon katsayısı: -0.7761684 
print(correlation4) # korelasyon katsayısı: -0.8676594
print(correlation5) # korelasyon katsayısı:  0.418684

#y bağımlı değişkeninin x5 hariç bütün bağımsız deşikşenlerle kuvvetli negatif
#korelasyonu vardır. örneğin silindir sayısı artıkça aynı miktardaki yakıtla 
#daha az yol gitmektedir.

#multicollinearity

# Çoklu doğrusal regresyon modelini oluşturma
model <- lm(y ~ x1 + x2 + x3 + x4 +x5)

# Model özetini alma
summary(model)   

# regresyon modeli: 
# y= 35.87361 - 1.15608 x1 + 0.01195 x2 - 0.01584 x3 + -4.22527 x4 + 0.25382 x5

# Bağımsız değişkenler arasındaki korelasyonu hesaplama

correlation_matrix <- cor(data.frame(x1, x2, x3 ,x4, x5))
print(correlation_matrix) 

# corrplot paketini yükleme
install.packages("corrplot")
library(corrplot)

# Korelasyon matrisini görselleştirme

corrplot(correlation_matrix, method = "color")

# Bağımsız değişkenlerin grafiğini çizme
par(mfrow=c(1,5)) # Grafiklerin yan yana yerleştirilmesi için ayar
plot(x1, y, main = "x1 vs. y", xlab = "x1", ylab = "y", col = "blue", pch = 16)
plot(x2, y, main = "x2 vs. y", xlab = "x2", ylab = "y", col = "red", pch = 16)
plot(x3, y, main = "x3 vs. y", xlab = "x3", ylab = "y", col = "orange", pch = 16)
plot(x4, y, main = "x4 vs. y", xlab = "x4", ylab = "y", col = "green", pch = 16)
plot(x5, y, main = "x5 vs. y", xlab = "x5", ylab = "y", col = "brown", pch = 16)


# Veriyi standartlaştırma
x1_standardized <- scale(x1)
x2_standardized <- scale(x2)
x3_standardized <- scale(x3)
x4_standardized <- scale(x2)
x5_standardized <- scale(x3)
y_standardized <- scale(y)

# Standartlaştırılmış veriyi kontrol etme
summary(x1_standardized)
summary(x2_standardized)
summary(x3_standardized)
summary(x4_standardized)
summary(x5_standardized)
summary(y_standardized)  #Standartlaştırılmış veride mean değerleri 0 çıkar.


# caTools paketini yükleme
install.packages("caTools")
library(caTools)

# Veriyi test ve eğitim alt kümelerine böleme (70% eğitim, 30% test verisi olarak)
split <- sample.split(y, SplitRatio = 0.7) 
train_data <- subset(data.frame(x1, x2, x3, x4, x5, y), split == TRUE)
test_data <- subset(data.frame(x1, x2, x3, x4, x5, y), split == FALSE)

# Verinin boyutlarını kontrol etme
dim(train_data) # train_data 22 satır ve 6 sütündan oluşuyor.
dim(test_data)  # test_data  10 satır ve 6 sütündan oluşuyor. Veri bölme işlemi yapılmış.
                

                
                #**************************SON*****************************
                #**********************************************************

