################################################
#                                              #
#  Title : Tugas R dplyr BODT Camp IYKRA       #
#  Author : Anugrah Yudha Pranata              #
#                                              #
################################################

# Pemanggilan library
library(dplyr)
library(tidyverse)
library(readxl)

# Menge-load data
setwd("D:/BODT Camp IYKRA/Materi/#21/Sesi 2")
df <- read_excel("german_credit_easy.xls", sheet = "german_credit_easy")

# Melihat struktur dataset
str(df)

# Dataset disimpan dengan nama variabel 'df' dengan tipe data frame.
df <- data.frame(df)

# Melihat rangkuman isi dari dataset
summary(df)

# Penyesuaian tipe data dari chr atau character menjadi factor, selain variabel ke-3, 6, dan 14
for (i in 1:ncol(df)){
  if(i %in% c(2, 3, 5, 6, 8, 9, 11, 12, 14, 16, 17, 19, 21, 22)) df[,i] <- as.factor(df[,i])
}

# Variabel ke-10, -13, -18, dan -20 juga dijadikan faktor berdasarkan business context
for (i in 1:ncol(df)){
  if(i %in% c(10, 13, 18, 20)) df[,i] <- as.factor(df[,i])
}

# Melihat kembali rangkuman isi dari dataset
summary(df)

# Assignment:
# No. 1 Descriptive:
# What's the number of customer and average credit amount for each loan purpose from loan with credit amount more than 10,000?

# Untuk ditampilkan

df %>%
  filter(credit_amount > 10000) %>% 
  group_by(purpose) %>% 
  summarize(Number_of_customer = n(), Credit_amount_Average = mean(credit_amount))

# Disimpan dalam tabel sementara bernama "temp1"

temp1 <- df %>%
  filter(credit_amount > 10000) %>% 
  group_by(purpose) %>% 
  summarize(Number_of_customer = n(), Credit_amount_Average = mean(credit_amount))

# No. 2 Transform:
# Create new variable Productive_Foreign with the following criteria:
#    If the customer is foreign worker, and within 18-55 then productive foreign
#    If the customer is foreign worker, and not within 18-55 then non_productive foreign
#    If the customer is local, and within 18-55 then productive local
#    If the customer is local, and not within 18-55 then non_productive local

# Untuk ditampilkan

df %>%
  mutate(Productive_Foreign = case_when(foreign_worker == "yes" & age >= 18 & age <= 55 ~ "productive foreign",
                                        foreign_worker == "yes" & (age < 18 | age > 55) ~ "non_productive foreign",
                                        foreign_worker == "no" & age >= 18 & age <= 55 ~ "productive local",
                                        TRUE ~ "non_productive local"
  ))

# Disimpan dalam dataset sementara bernama "temp2", variabel Productive_Foreign disimpan sebagai variabel bertipe data factor

temp2 <- df %>%
  mutate(Productive_Foreign = factor(case_when(foreign_worker == "yes" & age >= 18 & age <= 55 ~ "productive foreign",
                                               foreign_worker == "yes" & (age < 18 | age > 55) ~ "non_productive foreign",
                                               foreign_worker == "no" & age >= 18 & age <= 55 ~ "productive local",
                                               TRUE ~ "non_productive local" #TRUE digunakan ketika kondisi sebelumnya tidak terpenuhi
  )))

# Melihat rangkuman dari dataset temp2
summary(temp2)
summary(temp2$Productive_Foreign)