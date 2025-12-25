############################################################
# ABtest05.R
# ipynb → R 完全変換（全コード＋丁寧な日本語コメント）
# テーマ：多群比較・分散分析（ANOVA）・F分布
############################################################


############################################################
# Section 1：K群へのランダム割り当て
############################################################

set.seed(1)

n <- 300        # 全サンプル数
K <- 3          # 群の数（3群）

# 各群に同じ人数が割り当たるようにランダム化
group <- sample(rep(1:K, n / K))
print(group)


############################################################
# Section 2：群ごとに平均が異なるデータ生成
############################################################

# 群ごとに平均値を変える（例：群1=5, 群2=6, 群3=7）
mu <- c(5, 6, 7)

# 観測値 y を生成（正規分布）
y <- rnorm(n, mean = mu[group], sd = 1)

# 群3のデータを確認
y[group == 3]


############################################################
# Section 3：群ごとの要約統計量（sapply）
############################################################

# group ごとにデータを分ける
by_grp <- split(y, group)

# 群ごとのサンプルサイズ
n_g <- sapply(by_grp, length)

# 群ごとの平均
mean_g <- sapply(by_grp, mean)

# 群ごとの標準偏差
sd_g <- sapply(by_grp, sd)

# 表にまとめる
summary_table <- data.frame(
  n = n_g,
  mean = mean_g,
  sd = sd_g
)

print(summary_table)

# → 群ごとに平均値が異なっていることが確認できる


############################################################
# Section 4：3群比較（箱ひげ図）
############################################################

boxplot(y ~ factor(group),
        xlab = "Group",
        ylab = "Value",
        main = "Boxplot of 3 groups")

# 平均値を黒点で重ねる
points(1:3, tapply(y, group, mean), pch = 19)


############################################################
# Section 5：バイオリンプロット（参考）
############################################################

set.seed(1)

n <- 300
g <- rep(1:3, each = 100)
y <- rnorm(n, mean = mu[g], sd = 1)

vioplot <- function(x, at){
  d <- density(x)
  d$y <- d$y / max(d$y) * 0.25
  polygon(c(at - d$y, rev(at + d$y)),
          c(d$x, rev(d$x)),
          border = NA, col = rgb(0.5, 0.5, 0.5, 0.5))
}

plot(NULL, xlim = c(0.5, 3.5), ylim = range(y),
     xaxt = "n", xlab = "Group", ylab = "Value",
     main = "Violin plot of 3 groups")
axis(1, at = 1:3)

for(i in 1:3){
  vioplot(y[g == i], i)
}

points(1:3, tapply(y, g, mean), pch = 19)


############################################################
# Section 6：F分布の可視化（自由度を変える）
############################################################

x <- seq(0, 5, length = 400)

# df1 = df2 = nu
plot(x, df(x, 1, 1), type = "l", lwd = 2,
     ylab = "density", main = "F distribution")
lines(x, df(x, 2, 2), col = "red", lwd = 2)
lines(x, df(x, 5, 5), col = "darkgreen", lwd = 2)
lines(x, df(x, 10, 10), col = "purple", lwd = 2)

legend("topright",
       legend = c("nu=1", "nu=2", "nu=5", "nu=10"),
       col = c("black", "red", "darkgreen", "purple"),
       lty = 1, bty = "n")


############################################################
# Section 7：F分布（df1=2 固定）
############################################################

plot(x, df(x, 2, 2), type = "l", lwd = 2,
     ylab = "density", main = "F distribution (df1 = 2)")
lines(x, df(x, 2, 5), col = "red", lwd = 2)
lines(x, df(x, 2, 10), col = "darkgreen", lwd = 2)

legend("topright",
       legend = c("df2=2", "df2=5", "df2=10"),
       col = c("black", "red", "darkgreen"),
       lty = 1, bty = "n")


############################################################
# Section 8：分散分析（ANOVA）
############################################################

set.seed(1)

n <- 300
group <- sample(rep(1:3, n / 3))
y <- rnorm(n, mean = mu[group], sd = 1)

# aov(): analysis of variance
fit <- aov(y ~ factor(group))

# 結果表示
print(summary(fit))


############################################################
# Section 9：F値とp値の確認
############################################################

s <- summary(fit)[[1]]

cat("F =", s[1, "F value"],
    " p =", s[1, "Pr(>F)"], "\n")

# p値が有意水準（例：0.05）より小さければ、
# 「群間の平均には差がある」と判断する


############################################################
# Section 10：応用例（動画広告A/B/Cの滞在時間）
############################################################

# 動画A, B, C の3種類の広告を想定
set.seed(2)

n <- 300
group <- sample(rep(c("A", "B", "C"), n / 3))

# 各広告の平均滞在時間（秒）
mu_ad <- c(A = 30, B = 35, C = 40)

# 観測データ
time <- rnorm(n, mean = mu_ad[group], sd = 5)

# 分散分析
fit_ad <- aov(time ~ group)

print(summary(fit_ad))

s <- summary(fit_ad)[[1]]
cat("F =", s[1, "F value"],
    " p =", s[1, "Pr(>F)"], "\n")

# → 3広告の平均滞在時間に差があるかを検定できる
# 演習課題
boxplot(
  y ~ factor(group),
  names = c("A", "B", "C", "D"),
  ylab = "滞在時間（秒）",
  main = "動画広告ごとの滞在時間（Boxplot）"
)

# 分散分析（ANOVA）
fit <- aov(y ~ factor(group))
s <- summary(fit)[[1]]

cat(
  "F = ", s[1, "F value"],
  "  p = ", s[1, "Pr(>F)"], "\n"
)

############################################################
# 以上：ABtest05 の全コードを完全収録！
############################################################
