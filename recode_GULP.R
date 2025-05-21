library()

#GULPデータの読み込み

df_urban <- read.csv("1468.csv")   #大都市調査
df_rural <- read.csv("1469.csv")   #非大都市調査
df_mail <- read.csv("1470.csv")   #全国郵送調査

# 読み込んだデータフレームの先頭を確認（オプション）
head(df_urban)
head(df_rural)
head(df_mail)