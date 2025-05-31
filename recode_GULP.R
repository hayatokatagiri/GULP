library(tidyverse)
library(devtools)
#remotes::install_github("hayatokatagiri/qrecode") #qrecodeの読み込み
library(qrecode)

#GULPデータの読み込み
df_urban <- read.csv("1468.csv")   #大都市調査
df_rural <- read.csv("1469.csv")   #非大都市調査

#マージ（大都市調査と非大都市調査）
df_urban_processed <- df_urban %>%
  mutate(city_type = "大都市調査") # 新しい変数 'city_type' を追加し、"大都市調査"という値を入れる
df_rural_processed <- df_rural %>%
  mutate(city_type = "非大都市調査") # 新しい変数 'city_type' を追加し、"非大都市調査"という値を入れる

df <- bind_rows(df_urban_processed, df_rural_processed)

#リコード###############################
#属性・SES####
##年齢####
table(df$SQ2, useNA='always')
df$age <- NA
df$age[df$SQ2 == 0] <- 19
df$age[df$SQ2 == 1] <- (20 + 24) /2
df$age[df$SQ2 == 2] <- (25 + 29) /2
df$age[df$SQ2 == 3] <- (30 + 34) /2
df$age[df$SQ2 == 4] <- (35 + 39) /2
df$age[df$SQ2 == 5] <- (40 + 44) /2
df$age[df$SQ2 == 6] <- (45 + 49) /2
df$age[df$SQ2 == 7] <- (50 + 54) /2
df$age[df$SQ2 == 8] <- (55 + 59) /2
df$age[df$SQ2 == 9] <- (60 + 64) /2
df$age[df$SQ2 == 10] <- (65 + 69) /2
df$age[df$SQ2 == 11] <- 70
table(df$age, useNA='always')

##性別####
table(df$SQ3, useNA='always')
df$fmd <- NA
df$fmd[df$SQ3 == 2] <- 1
df$fmd[df$SQ3 == 1] <- 0

##既婚ダミー####
table(df$F1, useNA='always')
df$married <- NA
df$married[df$F1 == 1] <- 1
df$married[df$F1 == 2 |df$F1 == 3] <- 0
table(df$married)

##教育年数####
table(df$F5, useNA='always')
df$edu <- NA
df$edu[df$F5 == 1] <- 7.5
df$edu[df$F5 == 2] <- 14
df$edu[df$F5 == 3] <- 16
df$edu[df$F5 == 4] <- 18
df$edu[df$F5 == 5] <- NA
table(df$edu, useNA='always')

##世帯年収####
table(df$F6, useNA='always')
df$income <- NA
df$income[df$F6 == 1] <- 0
df$income[df$F6 == 2] <- 100
df$income[df$F6 == 3] <- (100+200)/2
df$income[df$F6 == 4] <- (200+300)/2
df$income[df$F6 == 5] <- (300+400)/2
df$income[df$F6 == 6] <- (400+500)/2
df$income[df$F6 == 7] <- (500+700)/2
df$income[df$F6 == 8] <- (700+900)/2
df$income[df$F6 == 9] <- (900+1200)/2
df$income[df$F6 == 10] <- (1200+1500)/2
df$income[df$F6 == 11] <- (1500+2000)/2
df$income[df$F6 == 12] <- 2000
df$income[df$F6 == 13] <- NA
table(df$income, useNA='always')

##居住年数####
table(df$SQ4, useNA='always')
df$liv_year <- NA
df$liv_year[df$SQ4 == 1] <- 1
df$liv_year[df$SQ4 == 2] <- (1 + 5) / 2
df$liv_year[df$SQ4 == 3] <- (5 + 10) / 2
df$liv_year[df$SQ4 == 4] <- (10 + 20) / 2
df$liv_year[df$SQ4 == 5] <- 20
table(df$liv_year, useNA='always')

#地域SC関連項目###################
#地域への信頼（逆転）
table(df$Q4_1, useNA='always')
df$local_trust <- NA
df$local_trust[df$Q4_1 == 5] <- 1
df$local_trust[df$Q4_1 == 4] <- 2
df$local_trust[df$Q4_1 == 3] <- 3
df$local_trust[df$Q4_1 == 2] <- 4
df$local_trust[df$Q4_1 == 1] <- 5
table(df$local_trust, useNA='always')


#アウトカム#######################
#主観的幸福感
#GULPの先行研究（埴淵編 2022:89）で、地方の方が大都市より幸福感が
#低いという結果あり。
table(df$Q12, useNA='always')
df$happiness <- q_rev(df$Q12)
table(df$happiness, useNA='always')

#主観的健康感
table(df$A4, useNA='always')
df$SRH <- NA
df$SRH[df$A4 == 5] <- 1
df$SRH[df$A4 == 4] <- 2
df$SRH[df$A4 == 3] <- 3
df$SRH[df$A4 == 2] <- 4
df$SRH[df$A4 == 1] <- 5
table(df$SRH, useNA='always')