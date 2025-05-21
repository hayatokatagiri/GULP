library(tidyverse)

#GULPデータの読み込み
df_urban <- read.csv("1468.csv")   #大都市調査
df_rural <- read.csv("1469.csv")   #非大都市調査

###マージ（大都市調査と非大都市調査）####
df_urban_processed <- df_urban %>%
  mutate(survey_type = "大都市調査") # 新しい変数 'survey_type' を追加し、"大都市調査"という値を入れる
df_rural_processed <- df_rural %>%
  mutate(survey_type = "非大都市調査") # 新しい変数 'survey_type' を追加し、"非大都市調査"という値を入れる
# 共通の列名だけを選んでデータフレームを整形
df_urban_selected <- df_urban_processed %>%
  select(all_of(common_cols), survey_type) # 共通列と新しく追加したsurvey_typeを選択
df_rural_selected <- df_rural_processed %>%
  select(all_of(common_cols), survey_type) # 共通列と新しく追加したsurvey_typeを選択
# 2つのデータフレームを縦に結合
# bind_rowsは、列の順序が異なっても、列名が一致すれば正しく結合できる
# 共通の列だけを選んでいれば問題ない
df <- bind_rows(df_urban_selected, df_rural_selected)
---------------------------------------------------------------------

####リコード####
# 結合されたデータフレームの構造と最初の数行を確認
str(df)
head(df)
tail(df) # データが結合されていることを確認するために最後の方も確認

####属性・SES####
#既婚ダミー
table(df$F1, useNA='always')
df$married <- NA
df$married[df$F1 == 1] <- 1
df$married[df$F1 == 2 |df$F1 == 3] <- 0
table(df$married)

