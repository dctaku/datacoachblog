##母平均の推定

# データ読み込み
#単位変換：インチからセンチメートル、ポンドからキログラム
data("women")
women$height <- round(women$height / 0.39370,1)
n <- length(women$height)
mean <- mean(women$height)
sd <- sd(women$height)
hist(women$height)
n
mean
sd

# 点推定
xbar <- mean
xbar
# 区間推定、母分散未知かつn=15だからt分布を使う
#sdは不偏分散だから信頼区間の式はn-1ではなくn
cf <- 0.95
df <- n-1
cftile <- qt(cf+(1-cf)/2,df)
c(xbar - cftile * sd / sqrt(n),xbar + cftile * sd / sqrt(n))

#１コマンドで（t検定）
t.test(data)

#正規分布でやってみる（母分散は仮数値として不偏分散）
cftile <- qnorm(cf+(1-cf)/2)
c(xbar - cftile * sd / sqrt(n),xbar + cftile * sd / sqrt(n))


##母比率の推定
#データ仮定：TV視聴率20%（900世帯）
n <- 900
p <- 0.2
x <- n * p
x

#区間推定
cf <- 0.95
cftile <- qnorm(cf+(1-cf)/2)
cftile
c(p - cftile * sqrt((p*(1-p)/n)),p + cftile * sqrt((p*(1-p)/n)))

#１コマンドで（２項検定）
binom.test(x,n)


##母分散の推定
#データの取得
data("women")
women$height <- round(women$height / 0.39370,1)
n <- length(women$height)
var <- var(women$height)
n
var

#区間推定
cf <- 0.95
cftile1 <- qchisq(cf+(1-cf)/2,n-1)
cftile2 <- qchisq((1-cf)/2,n-1)
cftile1
cftile2
c(n * var / cftile1,n * var / cftile2)
sqrt(c(n * var / cftile2,n * var / cftile1))
sqrt(var)


##母相関係数の推定
#データの取得
#データ読み込み
#単位変換：インチからセンチメートル、ポンドからキログラム
data("women")
women$height <- round(women$height / 0.39370,1)
women$weight <- round(women$weight / 2.2046,1)

plot(women$height,women$weight)
r <- cor(women$height,women$weight, method="pearson")
r
n <- length(women$height)
n

#区間推定
#Fisherのz変換
fisher <- function(x){
  log((1+x)/(1-x))/2
}
#z変換の逆変換
ifisher <- function(x){
  (exp(2*x)-1)/(exp(2*x)+1)
}

zr <- fisher(r)
cf <- 0.95
cftile <- qnorm(cf+(1-cf)/2)
cftile
zl <- zr - cftile / sqrt(n-3)
zu <- zr + cftile / sqrt(n-3)
c(ifisher(zl),ifisher(zu))

#１コマンドで（無相関の検定）
cor.test(women$height,women$weight)
