############################################################
# FirstCourse04.R
# データ・AI活用実践（初級） 第4回
# ipynb の全コードを再現＋丁寧な日本語コメント付き
############################################################


############################################################
# Section 1: 気温データ(pref47_temp.csv)のダウンロード
############################################################

# ここは覚えなくてもOK（講義用の定例コード）
csvurl <- "https://www.cc.kyoto-su.ac.jp/~ogohara/lecture/DataAI_FirstCourse/pref47_temp.csv"

# pref47_temp.csv として保存
download.file(csvurl, destfile = "pref47_temp.csv")

############################################################
# Section 2: 地図描画に必要なパッケージのインストール
############################################################

# NOTE: Colab で日本語地図を使うための環境整備
system("apt-get install -y libudunits2-dev libgdal-dev libgeos-dev libproj-dev")  # sf に必要
install.packages("sf")

install.packages("NipponMap")   # 日本地図の shapefile を含む

system("apt-get install libprotobuf-dev protobuf-compiler")
system("apt-get install libjq-dev")
install.packages("geojsonio")   # geojson を扱うパッケージ


############################################################
# Section 3: ライブラリ読み込み
############################################################

library(tidyverse)
library(sf)
library(NipponMap)


############################################################
# Section 4: CSV の読み込み
############################################################

df <- read_csv("pref47_temp.csv", locale = locale(encoding = "Shift_JIS"))
df     # データ確認


############################################################
# Section 5: 左端の不要な列を削除（前回の続きの処理）
############################################################

# 左端に番号だけの不要列がある場合は削除
df47 <- df[, -1]

summary(df47)   # 要約統計


############################################################
# Section 6: 列（各都道府県）の summary をまとめる
############################################################

# apply() で 各都道府県の "Min / 1stQu / Median / Mean ..." を取得
mtsum <- apply(df47[, -1], 2, summary)
mtsum

# 行列を転置して見やすくデータフレーム化
dfsum <- data.frame(t(mtsum))
dfsum

mtsum["Mean",]    # 全体の平均気温一覧


############################################################
# Section 7: 日本語フォントの準備（Colab向け）
############################################################

# 日本語が文字化けする環境ではフォントを追加
system("apt-get install -y fonts-noto-cjk")

# Noto Sans 日本語フォントを使用
theme_set(theme_bw(base_family = "Noto Sans CJK JP"))


############################################################
# Section 8: 都道府県ごとの平均気温のバーグラフ
############################################################

barplot(
  mtsum["Mean", ],
  las = 2,   # x 軸の文字を縦書きに
  xlab = "Prefecture",
  ylab = "Mean temperature [deg_C]"
)


############################################################
# Section 9: 日本地図 shapefile の読み込み
############################################################

map <- read_sf(
  system.file("shapes/jpn.shp", package = "NipponMap")[1],
  crs = "+proj=longlat +datum=WGS84"
)

map   # 地図オブジェクトの内容を確認


############################################################
# Section 10: shapefile に含まれる 'population' の例の地図
############################################################

ggplot(map, aes(fill = population)) +
  geom_sf() +
  labs(title = "Population")

############################################################
# Section 11: データ結合の準備（行名の確認）
############################################################

rownames(dfsum)   # df の都道府県名
map$name          # NipponMap の都道府県名


############################################################
# Section 12: JISコードのダウンロードと読み込み
############################################################

# ここは覚えなくてもOK
csvurl <- "https://www.cc.kyoto-su.ac.jp/~ogohara/lecture/DataAI_FirstCourse/JIS.csv"
download.file(csvurl, destfile = "JIS.csv")

jisdf <- read_csv("JIS.csv", locale = locale(encoding = "Shift_JIS"))
jisdf


############################################################
# Section 13: 都道府県名を JISコードへ変換する処理
# dfsum の行＝都道府県名を、JISコードに紐付ける
############################################################

city <- c('那覇', '松江', '松山', '高松', '神戸', '津', '彦根', '金沢', '名古屋',
          '前橋', '甲府', '横浜', '熊谷', '宇都宮', 'つくば（館野）', '仙台', '盛岡', '札幌')

pref <- c('沖縄', '島根', '愛媛', '香川', '兵庫', '三重', '滋賀', '石川', '愛知',
          '群馬', '山梨', '神奈川', '埼玉', '栃木', '茨城', '宮城', '岩手', '北海道')

for (n in jisdf$都道府県名) {
  
  nlen <- str_length(n)
  
  # 「北海道」は特例でそのまま、それ以外は最後の1文字（県/府）を切り落とす
  if (n == "北海道") {
    prefname <- n
  } else {
    prefname <- str_sub(n, end = nlen - 1)
  }

  # df の行名が city ベースか、pref ベースかで対応
  if (is.na(match(prefname, pref))) {
    # city ベース
    name_in_dfsum <- prefname
  } else {
    # pref → city の対応表
    name_in_dfsum <- city[which(pref == prefname)]
  }

  # dfsum に jiscode 列を追加（都道府県ごとに）
  dfsum[name_in_dfsum, "jiscode"] <-
    jisdf[which(jisdf$都道府県名 == n), "都道府県コード"]
}

# jiscode の昇順で並べた dfsum
dfsum[order(dfsum$jiscode), ]


############################################################
# Section 14: shapefile と気温データの結合
############################################################

# map の順番に合わせて dfsum を並び替え
map[, "temperature"] <- dfsum[order(dfsum$jiscode), "Mean"]

map   # 確認（temperature が追加されている）


############################################################
# Section 15: 日本地図に平均気温を塗り分ける
############################################################

library(RColorBrewer)

ggplot(map, aes(fill = temperature)) +
  geom_sf() +
  labs(title = "Mean Temperature") +
  scale_fill_gradientn(colours = topo.colors(9))


############################################################
# 第4回の全コードはここまでです。
# ipynb の内容を完全に再現し、VSCode / RStudio で動作します。
############################################################
