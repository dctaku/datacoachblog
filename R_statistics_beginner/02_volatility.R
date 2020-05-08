## データのバラツキ ##

#データ読み込み
#単位変換：インチからセンチメートル、ポンドからキログラム
data("women")
women$height <- round(women$height / 0.39370,1)
women$weight <- round(women$weight / 2.2046,1)
#データ構造確認
str(women)
#class(women)
#dim(women)
#ざっくり確認
summary(women)
#グラフで確認
par(mfrow=c(1,3))
hist(women$height)
hist(women$weight)
boxplot(women)
par(mfrow=c(1,1))

# 分位数
#四分位数
quantile(women$height)
#最小値、最大値
range(women$height)
# 四分位範囲
quantile(women$height,0.75)-quantile(women$height,0.25)
IQR(women$height)

# 偏差
women$height - mean(women$height)

# 分散
#不偏分散
var(women$height)
#母分散
variance <- function(x){
  n <- length(x)
  var(x)*(n-1)/n
}
variance(women$height)

# 標準偏差
sd(women$height)
sqrt(var(women$height))

# 外れ値
sample <- c(women$height, 250)
boxplot(sample)

# 変動係数
apply(women,2,mean)
apply(women,2,sd)
apply(women,2,sd)/apply(women,2,mean)