####################################################
#                                                  #
#  Title : Tugas Data Wrangling BODT Camp IYKRA    #
#  Author : Anugrah Yudha Pranata                  #
#                                                  #
####################################################

# Pemanggilan library
library(dplyr)
library(readxl)
library(ggplot2)
library(ggpubr)
library(tidyr)
library(randomForest)
library(ROCR)
library(caret)
library(rpart)
library(RANN)
library(dummies)
library(mice)
library(ROCR)
library(pROC)
library(InformationValue)
library(caTools)
library(tidyverse)

# Background:
# Problem yang akan di-solve adalah Supervised, tepatnya Classification Problem
# Terdapat dataset German Credit yang memiliki:
# 13 + 4 variabel kategorikal (nominal)
# 3 variabel numerik
# 1 variabel identifier
# 1 variabel target: default (tipe data nominal/kategorikal)

# Menge-load data
df <- read_xls("D:/BODT Camp IYKRA/Materi/#16/german_credit_missing.xls", sheet = "german_credit_missing")

# Melihat struktur dataset
str(df)

# Dataset disimpan dengan nama variabel 'df' dengan tipe data frame.
df <- data.frame(df)

# Melihat rangkuman isi dari dataset
summary(df)

# Rangkuman isi dataset yang didapatkan:
# 1. Missing value terdapat di hampir setiap variabel
# 2. Beberapa variabel yang memiliki tipe data chr atau character disimpan kembali sebagai variabel/feature bertipe factor

# Tujuan data wrangling ini adalah untuk menyiapkan data agar dapat melakukan machine learning/klasifikasi.
# Maka, persiapan data ini harus memperhatikan tujuan tersebut (dengan maksud untuk memodifikasi data tersebut)
# Dari 1000 observasi, terdapat 300 observasi yang menghasilkan default = 0 atau ditolak aplikasi pengajuan kreditnya
# Atau dapat pula dikatakan sebagai terdapat 700 observasi yang menghasilkan default = 1 atau diterima aplikasi pengajuan kreditnya

# Hapus Variabel Identifier
df <- df[,-1]

# Penyesuaian tipe data dari chr atau character menjadi factor, selain variabel ke-3, 6, dan 14
for (i in 1:ncol(df)){
  if(i %in% c(1, 2, 4, 5, 7, 8, 10, 11, 13, 15, 16, 18, 20, 21)) df[,i] <- as.factor(df[,i])
}

# Variabel ke-9, -12, -17, dan -19 juga dijadikan faktor berdasarkan business context
for (i in 1:ncol(df)){
  if(i %in% c(9, 12, 17, 19)) df[,i] <- as.factor(df[,i])
}

# Recode Labels pada tipe data Factor
df$account_check_status <- factor(df$account_check_status, labels = c("A12", "A13", "A14", "A11"))
df$credit_history <- factor(df$credit_history, labels = c("A22", "A25", "A24", "A23", "A21"))
df$purpose <- factor(df$purpose, labels = c("A310", "A37", "A31", "A32", "A35", "A36", "A34", "A33", "A38", "A39"))
df$savings <- factor(df$savings, labels = c("A45", "A42", "A43", "A44", "A41"))
df$present_emp_since <- factor(df$present_emp_since, labels = c("A55", "A52", "A53", "A54", "A51"))
df$installment_as_income_perc <- factor(df$installment_as_income_perc, labels = c("A61", "A62", "A63", "A64"))
df$personal_status_sex <- factor(df$personal_status_sex, labels = c("A74", "A73", "A72", "A71"))
df$other_debtors <- factor(df$other_debtors, labels = c("A82", "A81", "A83"))
df$present_res_since <- factor(df$present_res_since, labels = c("A91", "A92", "A93", "A94"))
df$property <- factor(df$property, labels = c("A102", "A103", "A101", "A104"))
df$other_installment_plans <- factor(df$other_installment_plans, labels = c("A111", "A113", "A112"))
df$housing <- factor(df$housing, labels = c("A121", "A122", "A123"))
df$credits_this_bank <- factor(df$credits_this_bank, labels = c("A131", "A132", "A133", "A134"))
df$job <- factor(df$job, labels = c("A144", "A143", "A141", "A142"))
df$people_under_maintenance <- factor(df$people_under_maintenance, labels = c("A151", "A152"))
df$telephone <- factor(df$telephone, labels = c("A162", "A161"))
df$foreign_worker <- factor(df$foreign_worker, labels = c("A172", "A171"))


# Melihat kembali dataframe
summary(df)

# Melihat karakteristik missing value dan mencari tahu di (baris dan kolom) mana saja missing value tersebut
sum(is.na(df))
# Terdapat 210 missing value

sapply(df, function(x) sum(is.na(x)))
rowSums(is.na(df))
which(is.na(df), arr.ind = TRUE)
# Ternyata banyak yang missing value di lebih dari 1 kolom/variabel untuk observasi yang sama
# Column yang punya missing value: 3, 4, 5, 6, 7, 8, 10, 11, 12, 13, 14, 15, 16, 17

# Simpan jumlah missing value per row dalam matrix
na.row <- c(rowSums(is.na(df)))

na.row <- data.frame(na.row) %>% 
  filter(na.row != 0)
summary(na.row)
ggplot(na.row, aes(na.row)) + geom_bar()

# Data disimpan dalam variabel temporer tanpa missing value terlebih dahulu: df2
df2 <- df %>% drop_na()
summary(df2)

sd(df2$duration_in_month)
sd(df2$credit_amount)
sd(df2$age)

# Uji Kenormalan
shapiro.test(df2$age)
shapiro.test(df2$credit_amount)
shapiro.test(df2$duration_in_month)

# Terbukti bahwa seluruh variabel bertipe data numerik pada dataset tidak berdistribusi normal (p-value < 0.05)

# Hitung-hitungan outlier untuk distribusi normal
mean(df2$age) + 3*sd(df2$age)
mean(df2$credit_amount) + 3*sd(df2$credit_amount)
mean(df2$duration_in_month) + 3*sd(df2$duration_in_month)

# Visualisasi
ggplot(data = df2, aes(x = age)) + geom_density()
ggplot(data = df2, aes(x = credit_amount)) + geom_density()

# Duration in Month merupakan variabel bertipe data numerik diskrit, kurang tepat untuk diplotkan dengan menggunakan density
ggplot(data = df2, aes(x = age)) + geom_bar()
ggplot(data = df2, aes(x = duration_in_month)) + geom_bar()

ggplot(data = df2, aes(x = age)) + geom_bar(aes(fill = default))
ggplot(data = df2, aes(x = duration_in_month)) + geom_bar(aes(fill = default))

ggplot(data = df2, aes(x = default, y = age)) + geom_boxplot()
ggplot(data = df2, aes(x = default, y = credit_amount)) + geom_boxplot()
ggplot(data = df2, aes(x = default, y = duration_in_month)) + geom_boxplot()


ggplot(data = df2, aes(x = duration_in_month, y = age)) + geom_jitter(aes(colour = default))
ggplot(data = df2, aes(x = duration_in_month, y = credit_amount)) + geom_jitter(aes(colour = default))
ggplot(data = df2, aes(x = age, y = credit_amount)) + geom_jitter(aes(colour = default))


ggplot(data = df2, aes(x = account_check_status)) + geom_bar(aes(fill = default))
ggplot(data = df2, aes(x = credit_history)) + geom_bar(aes(fill = default))
ggplot(data = df2, aes(x = purpose)) + geom_bar(aes(fill = default))
ggplot(data = df2, aes(x = savings)) + geom_bar(aes(fill = default))
ggplot(data = df2, aes(x = present_emp_since)) + geom_bar(aes(fill = default))
ggplot(data = df2, aes(x = installment_as_income_perc)) + geom_bar(aes(fill = default))
ggplot(data = df2, aes(x = personal_status_sex)) + geom_bar(aes(fill = default))
ggplot(data = df2, aes(x = other_debtors)) + geom_bar(aes(fill = default))
ggplot(data = df2, aes(x = present_res_since)) + geom_bar(aes(fill = default))
ggplot(data = df2, aes(x = property)) + geom_bar(aes(fill = default))
ggplot(data = df2, aes(x = other_installment_plans)) + geom_bar(aes(fill = default))
ggplot(data = df2, aes(x = housing)) + geom_bar(aes(fill = default))
ggplot(data = df2, aes(x = credits_this_bank)) + geom_bar(aes(fill = default))
ggplot(data = df2, aes(x = job)) + geom_bar(aes(fill = default))
ggplot(data = df2, aes(x = people_under_maintenance)) + geom_bar(aes(fill = default))
ggplot(data = df2, aes(x = telephone)) + geom_bar(aes(fill = default))
ggplot(data = df2, aes(x = foreign_worker)) + geom_bar(aes(fill = default))


# Bangun Model Random Forest
# Splitting data
set.seed(123)

inTrain <- createDataPartition(y = df2$default,
                               p = 0.7,
                               list = FALSE)

training <- df2[ inTrain,]
test <- df2[-inTrain,]

rf_classifier2 = randomForest(default ~ ., data=training, ntree=100, mtry=2, importance=TRUE)
varImpPlot(rf_classifier2)

pred.val <- predict(rf_classifier2, test, type = "class")
misClasificError <- mean(pred.val != test$default)
table(pred.val, test$default)
print(paste('Accuracy',1-misClasificError))
# Accuracy: 0.744274809160305 / 0.751908396946565 / 0.755725190839695

# AUC
pr <- prediction(as.numeric(pred.val), as.numeric(test$default))
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)

auc <- performance(pr, measure = "auc")
auc@y.values[[1]]
# Area under Curve value: 0.6165691


# Menghapus outlier berdasarkan selang kepercayaan 99%
# Data kemudian disimpan dalam variabel temporer (Tanpa Missing Value, Outlier dihapus): df3
df3 <- df2

df3 <- df3 %>% 
  filter(age <= mean(df2$age) + 3*sd(df2$age))
df3 <- df3 %>% 
  filter(credit_amount <= mean(df2$credit_amount) + 3*sd(df2$credit_amount))
df3 <- df3 %>% 
  filter(duration_in_month <= mean(df2$duration_in_month) + 3*sd(df2$duration_in_month))

summary(df3)

# Splitting data
set.seed(123)

inTrain <- createDataPartition(y = df3$default,
                               p = 0.7,
                               list = FALSE)

training <- df3[ inTrain,]
test <- df3[-inTrain,]

rf_classifier3 = randomForest(default ~ ., data=training, ntree=100, mtry=2, importance=TRUE)
varImpPlot(rf_classifier3)

pred.val <- predict(rf_classifier3, test, type = "class")
misClasificError <- mean(pred.val != test$default)
table(pred.val, test$default)
print(paste('Accuracy',1-misClasificError))
# Accuracy: 0.761904761904762

# AUC
pr <- prediction(as.numeric(pred.val), as.numeric(test$default))
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)

auc <- performance(pr, measure = "auc")
auc@y.values[[1]]
# Area under Curve value: 0.6083333

# Interpretasi hasil: akurasi meningkat, namun membuat nilai AUC menurun.
# Penghapusan outlier pada variabel dependen ternyata dapat membuat model menjadi lebih baik dalam mengklasifikan hasil secara keseluruhan
# Namun, model menjadi sedikit lebih buruk dalam membedakan hasil yang baik dan yang buruk. Oleh sebab 


# Membuat Dummy Variables
# Disimpan dalam variabel temporer (tanpa missing value, outlier dihapus, dengan dummy variable): df4
df4 <- cbind(dummy.data.frame(df3[,2:21], sep = "."), df3["default"])
summary(df4)
ncol(df4)


# Bangun Random Forrest Model
# Splitting data
set.seed(123)

inTrain <- createDataPartition(y = df4$default,
                               p = 0.7,
                               list = FALSE)

training <- df4[ inTrain,]
test <- df4[-inTrain,]

rf_classifier4 = randomForest(default ~ ., data=training, ntree=100, mtry=2, importance=TRUE)
varImpPlot(rf_classifier4)

pred.val <- predict(rf_classifier4, test, type = "class")
misClasificError <- mean(pred.val != test$default)
table(pred.val, test$default)
print(paste('Accuracy',1-misClasificError))
# Accuracy: 0.718253968253968

# AUC
pr <- prediction(as.numeric(pred.val), as.numeric(test$default))
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)

auc <- performance(pr, measure = "auc")
auc@y.values[[1]]
# Area under Curve value: 0.5111111

# Interpretasi hasil: akurasi menurun, begitu pula dengan nilai AUC.
# Pembuatan dummy variable ternyata tidak membuat model menjadi lebih baik


# Data scaling
# Disimpan dalam variabel temporer (tanpa missing value, outlier dihapus, dengan dummy variable, di-scale): df5
max <- apply(df4[,1:71], 2, max)
min <- apply(df4[,1:71], 2, min)
df5 <- cbind(as.data.frame(scale(df4[,1:71], center = min, scale = max - min)), df4["default"])


# Bangun Random Forrest Model
# Splitting data
set.seed(123)

inTrain <- createDataPartition(y = df5$default,
                               p = 0.7,
                               list = FALSE)

training <- df5[ inTrain,]
test <- df5[-inTrain,]

rf_classifier5 = randomForest(default ~ ., data=training, ntree=100, mtry=2, importance=TRUE)
varImpPlot(rf_classifier5)

pred.val <- predict(rf_classifier5, test, type = "class")
misClasificError <- mean(pred.val != test$default)
table(pred.val, test$default)
print(paste('Accuracy',1-misClasificError))
# Accuracy: 0.718253968253968

# AUC
pr <- prediction(as.numeric(pred.val), as.numeric(test$default))
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)

auc <- performance(pr, measure = "auc")
auc@y.values[[1]]
# Area under Curve value: 0.5111111

# Interpretasi hasil: akurasi tidak mengalami perubahan, begitu pula dengan nilai AUC.
# Scaling data ternyata tidak membuat model menjadi lebih baik (sama saja)


# Data Scaling dari data tanpa outlier (tanpa dummy variable)
# Disimpan dalam variabel temporer (tanpa missing value, outlier dihapus, scaled): df6
# Data scaling
max <- apply(df3[,c(3, 6, 14)], 2, max)
min <- apply(df3[,c(3, 6, 14)], 2, min)
df6 <- as.data.frame(scale(df3[,c(3, 6, 14)], center = min, scale = max - min))
Vars <- names(df3)
df6 <- cbind(df6, df3[Vars])
# Hapus variabel yang ganda
df6 <- df6[, -c(6, 9, 17)]

# Bangun Random Forrest Model
# Splitting data
set.seed(123)

inTrain <- createDataPartition(y = df6$default,
                               p = 0.7,
                               list = FALSE)

training <- df6[ inTrain,]
test <- df6[-inTrain,]

rf_classifier6 = randomForest(default ~ ., data=training, ntree=100, mtry=2, importance=TRUE)
varImpPlot(rf_classifier6)

pred.val <- predict(rf_classifier6, test, type = "class")
misClasificError <- mean(pred.val != test$default)
table(pred.val, test$default)
print(paste('Accuracy',1-misClasificError))
# Accuracy: 0.76984126984127

# AUC
pr <- prediction(as.numeric(pred.val), as.numeric(test$default))
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)

auc <- performance(pr, measure = "auc")
auc@y.values[[1]]
# Area under Curve value: 0.6180556

# Interpretasi hasil: akurasi meningkat, begitu pula dengan nilai AUC.
# Scaling data ternyata dapat membuat model menjadi lebih baik
# (Untuk kasus ini, tapi tidak untuk kasus yang lain)


# Data Treatment untuk Missing Value
# Disimpan dalam variabel temporer: df7
df7 <- df
sum(is.na(df7))

# Simpan jumlah missing value per row dalam matrix
na.row <- c(rowSums(is.na(df7)))

# Hapus semua row yang memiliki missing value lebih dari 1 karena akan susah untuk memprediksi nilai untuk imputasinya
df7 <- df7[na.row <= 1,]
summary(df7)
sum(is.na(df7))
sapply(df7, function(x) sum(is.na(x)))

miceMod <- mice(df7[,!names(df7) %in% "default"], method = "rf")
df7 <- cbind(mice::complete(miceMod), df7["default"])

# Bangun Random Forrest Model
# Splitting data
set.seed(123)

inTrain <- createDataPartition(y = df7$default,
                               p = 0.7,
                               list = FALSE)

training <- df7[ inTrain,]
test <- df7[-inTrain,]

rf_classifier7 = randomForest(default ~ ., data=training, ntree=100, mtry=2, importance=TRUE)
varImpPlot(rf_classifier7)

pred.val <- predict(rf_classifier7, test, type = "class")
misClasificError <- mean(pred.val != test$default)
table(pred.val, test$default)
print(paste('Accuracy',1-misClasificError))
# Accuracy: 0.74061433447099 / 0.744027303754266

# AUC
pr <- prediction(as.numeric(pred.val), as.numeric(test$default))
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)

auc <- performance(pr, measure = "auc")
auc@y.values[[1]]
# Area under Curve value: 0.598845

# Interpretasi hasil: akurasi menurun (dibandingkan dengan jika seluruh NA atau missing value dihapuskan), begitu pula dengan nilai AUC.
# Imputasi data tidak mempuat model menjadi lebih baik.


# Data Scaling dari data tanpa missing value, hanya untuk variabel "Credit Amount" berskala [0:100]
# Disimpan dalam variabel temporer (tanpa missing value, outlier dihapus, scaled): df8
# Data scaling
df8 <- df3
df8$credit_amount <- scale(df8$credit_amount, center = FALSE, max(df8$credit_amount, na.rm = TRUE)/100)
summary(df8)

# Bangun Random Forrest Model
# Splitting data
set.seed(123)

inTrain <- createDataPartition(y = df8$default,
                               p = 0.7,
                               list = FALSE)

training <- df8[ inTrain,]
test <- df8[-inTrain,]

rf_classifier8 = randomForest(default ~ ., data=training, ntree=100, mtry=2, importance=TRUE)
varImpPlot(rf_classifier8)

pred.val <- predict(rf_classifier8, test, type = "class")
misClasificError <- mean(pred.val != test$default)
table(pred.val, test$default)
print(paste('Accuracy',1-misClasificError))
# Accuracy: 0.761904761904762

# AUC
pr <- prediction(as.numeric(pred.val), as.numeric(test$default))
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)

auc <- performance(pr, measure = "auc")
auc@y.values[[1]]
# Area under Curve value: 0.6083333

# Interpretasi hasil: akurasi tidak berubah, begitu juga dengan nilai AUC.
# Scaling data tidak membuat model menjadi lebih baik.


##########################################################################################################################################################################################################################
# KESIMPULAN:
# Data German Missing ini pada awalnya belum bersih untuk dianalisis lebih lanjut, di antaranya disebabkan:
# 1. Data ini memiliki missing value
# 2. Data ini tidak berdistribusi normal, meskipun tidak ada outlier yang berarti selain variable "Credit Amount"
# 3. Data ini memiliki beragam tipe data, baik numerik maupun kategorikal
# 4. Untuk tipe data numerik, skala nilai yang dimiliki oleh masing-masing variabel berbeda
#
# Oleh karena itu, beberapa data treatment yang dilakukan pada penyiapan data ini, antara lain:
# 1. Menghapus observasi yang memiliki jumlah missing value banyak (lebih dari 1)
# 2. Menghapus observasi yang dianggap memiliki outlier (untuk variabel bertipe data numerik)
# 3. Melakukan imputasi data berdasarkan metode Random Forrest (karena tipe data variabel target yang merupakan kategorikal)
# 4. Melakukan data scaling untuk variabel-variabel numerik, untuk seluruh variabel, maupun variabel tertentu saja
# 5. Melakukan kombinasi dari perlakuan-perlakuan di atas.
#
# HASIL:
# Jika dilakukan pemodelan terhadap data setelah dilakukan berbagai perlakuan di atas, model yang dibuat sudah cukup baik (memiliki akurasi sekitar 0.70 hingga 0.76)
# Adapun masing-masing perlakukan di atas memberikan pengaruh tersendiri terhadap model yang dihasilkan, dengan perubahan nilai akurasi sekitar -0.04 hingga +0.05
# Berbagai kombinasi perlakuan dapat dilakukan untuk memperbaiki data agar dapat menghasilkan model yang lebih baik.
# Untuk langkah ke depannya, beberapa hal perlu diperhatikan/dapat dilakukan, seperti mengganti metode imputasi data, mengubah urutan pemberian perlakuan, atau melakukan normalisasi data untuk tipe data numerik.
#
# Sekian dan terima kasih.
# 
##########################################################################################################################################################################################################################