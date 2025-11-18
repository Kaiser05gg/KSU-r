############################################################
# FirstCourse05.R
# データ・AI活用実践（初級） 第5回（気候区分・クラスタリング）
# ipynb から完全変換した R スクリプト（全コード＋解説）
############################################################


############################################################
# Section 1: データのダウンロード
############################################################

# pref47_temp.csv（47都道府県の月平均気温）
csvurl <- "https://www.cc.kyoto-su.ac.jp/~ogohara/lecture/DataAI_FirstCourse/pref47_temp.csv"
download.file(csvurl, destfile = "pref47_temp.csv")

# JIS.csv（都道府県コード）
csvurl <- "https://www.cc.kyoto-su.ac.jp/~ogohara/lecture/DataAI_FirstCourse/JIS.csv"
download.file(csvurl, destfile = "JIS.csv")


############################################################
# Section 2: ライブラリ読み込み
############################################################

library(tidyverse)  # ggplot2、dplyrなどを含む便利セット


############################################################
# Section 3: 気温データ読み込み
############################################################

df <- read_csv("pref47_temp.csv", locale = locale(encoding = "Shift_JIS"))
df    # 中身確認


############################################################
# Section 4: 地図用パッケージのインストール（Colab向け）
############################################################

system("apt-get install -y libudunits2-dev libgdal-dev libgeos-dev libproj-dev")
install.packages("sf")

install.packages("NipponMap")

system("apt-get install libprotobuf-dev protobuf-compiler")
system("apt-get install libjq-dev")
install.packages("geojsonio")


############################################################
# Section 5: 地図描画ライブラリ読み込み
############################################################

library(sf)
library(NipponMap)


############################################################
# Section 6: 不要列の削除
############################################################

# 左端に連番列があれば削除
df47 <- df[, -1]


############################################################
# Section 7: 要約統計の計算
############################################################

# 都道府県ごとの "Min / Max / Mean / Median" など summary を取得
mtsum <- apply(df47[, -1], 2, summary)

# 行列を転置してデータフレーム化
dfsum <- data.frame(t(mtsum))


############################################################
# Section 8: shapefile（日本地図）読み込み
############################################################

map <- read_sf(
  system.file("shapes/jpn.shp", package = "NipponMap")[1],
  crs = "+proj=longlat +datum=WGS84"
)


############################################################
# Section 9: JISコード読み込み
############################################################

jisdf <- read_csv("JIS.csv", locale = locale(encoding = "Shift_JIS"))


############################################################
# Section 10: 都道府県名の処理（city / pref 対応表）
############################################################

city <- c('那覇', '松江', '松山', '高松', '神戸', '津', '彦根', '金沢', '名古屋',
          '前橋', '甲府', '横浜', '熊谷', '宇都宮', 'つくば（館野）', '仙台', '盛岡', '札幌')

pref <- c('沖縄', '島根', '愛媛', '香川', '兵庫', '三重', '滋賀', '石川', '愛知',
          '群馬', '山梨', '神奈川', '埼玉', '栃木', '茨城', '宮城', '岩手', '北海道')


############################################################
# Section 11: dfsum に JISコードを付与
############################################################

for (n in jisdf$都道府県名) {

  nlen <- str_length(n)

  # 北海道だけ「道」なので例外処理
  if (n == "北海道") {
    prefname <- n
  } else {
    prefname <- str_sub(n, end = nlen - 1)  # 「県」「府」を取り除く
  }

  # city で一致するか / pref で一致するか判断
  if (is.na(match(prefname, pref))) {
    name_in_dfsum <- prefname
  } else {
    name_in_dfsum <- city[which(pref == prefname)]
  }

  # JISコードを追加
  dfsum[name_in_dfsum, "jiscode"] <-
    jisdf[which(jisdf$都道府県名 == n), "都道府県コード"]
}


############################################################
# Section 12: 地図と平均気温を結合
############################################################

map[, "temperature"] <- dfsum[order(dfsum$jiscode), "Mean"]


############################################################
# Section 13: 平均気温を地図にプロット
############################################################

library(RColorBrewer)

ggplot(map, aes(fill = temperature)) +
  geom_sf() +
  labs(title = "Mean Temperature") +
  scale_fill_gradientn(colours = topo.colors(9))


############################################################
# Section 14: 平均気温ヒストグラム
############################################################

g <- ggplot(dfsum, aes(x = Mean)) +
  geom_histogram()
plot(g)


############################################################
# Section 15: 平均気温が14度未満の都道府県
###
