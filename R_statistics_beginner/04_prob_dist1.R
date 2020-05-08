## 確率分布 ##

## 一様分布 サイコロ
# 乱数シミュレーション
n <- c(10,100,1000,10000)
par(mfrow=c(2,2))
for(i in n){
  dice_sim <- floor(runif(i,1,7))
  hist(dice_sim,breaks=c(0,1,2,3,4,5,6),probability=T,col="gray", main = paste("n = ", i))
}
mean(dice_sim)
var(dice_sim)
sd(dice_sim)

# 期待値と分散の理論値
a <- 1
b <- 6
mu <- (a+b)/2
var <- ((b-a)^2/12)
sd <- sqrt(var)
mu
var
sd

## 二項分布 コイン投げ
# 乱数シミュレーション
size <- 10
p <- 0.5
n <- c(10,100,1000,10000)
par(mfrow=c(2,2))
for(i in n){
  coin_sim <- rbinom(i,size,p)
  hist(coin_sim,probability=T,col="gray",main = paste("n = ", i))
}
mean(coin_sim)
var(coin_sim)
sd(coin_sim)

# 期待値と分散の理論値
mu <- size * p
var <- size * p * (1-p)
sd <- sqrt(var)
mu
var
sd

# 確率密度関数: size回のうちx回成功する確率
size <- 10
p <- 0.5
x <- 3
dbinom(x,size,p)
#計算確認
choose(size, x) * p^x * p^(size-x)

# 累積確率
pbinom(x,size,p)
#計算確認
ruiseki = 0
for(i in 0:x){
  ruiseki <- ruiseki + dbinom(i,size,p)
}
ruiseki

# 確率点
qbinom(ruiseki,size,p)

# 確率密度関数描画
size <- 50
p <- 0.5
x<-0:40
par(mfrow=c(1,2))
plot(x,dbinom(x,size,p),type="h",lwd=3,col="gray")
plot(x,pbinom(x,size,p),type="h",lwd=3,col="gray")


## 正規分布
#乱数シミュレーション
mu<-10
sigma<-20
par(mfrow=c(1,1))
normal_sim <- rnorm(1000, mean = mu,sd = sigma)
hist(normal_sim)

sample_mean <- mean(normal_sim)
sample_sd <- sd(normal_sim)
sample_mean
sample_sd

#確率密度関数
norm_dist <- function(x,mu,sigma){
  1/(sqrt(2*pi)*sigma)*exp(-(x-mu)^2/(2*sigma^2))
}
norm_dist(0,0,1)
dnorm(0)

#いろいろな正規分布描画
x<-seq(-4,4,0.1)
curve(dnorm(x,0,0.5),from=-4,to=4,type="l",col="blue")
curve(dnorm(x,0,1),add=T,col="pink")
curve(dnorm(x,0,1.5),add=T,col="green")
curve(dnorm(x,-2,1),add=T,col="orange")


#標準化:x-mu / sigma
n_sample <- (normal_sim - sample_mean) / sample_sd
hist(n_sample)
mean(n_sample)
sd(n_sample)

#標準正規分布よく使うタイル点
#2.5%,5%,95%,97.5%タイル点
qnorm(0.025)
qnorm(0.05)
qnorm(0.95)
qnorm(0.975)

#シグマ区間
pnorm(1) - pnorm(-1)
pnorm(2) - pnorm(-2)
pnorm(3) - pnorm(-3)

## 歪度と尖度
#歪度と尖度を含むパッケージを読み込み
library(moments)

data("EuStockMarkets")
str(EuStockMarkets)
dax <- EuStockMarkets[,1]
r_dax <- diff(log(dax)) *100
hist(r_dax)
mean(r_dax)
sd(r_dax)
skewness(r_dax)
kurtosis(r_dax)


## ポアソン分布
#乱数シミュレーション
n <- c(100,1000,5000,10000)
p <- 0.002
#lambda <- n*p
par(mfrow=c(2,2))
for (i in n) {
  pois_sim <- rpois(i,i*p)
  hist(pois_sim,probability=T,col="gray", main = paste("n=",i))
}
mean(pois_sim)
sd(pois_sim)

mu <- n[4]*p
var <- mu
sd <- sqrt(var)
mu
sd

#確率密度関数
pois_dist <- function(x,lambda){
  exp(-lambda)*lambda^x/factorial(x)
}
lambda <- 20
x <- 15
pois_dist(x,lambda)
dpois(x,lambda = lambda)

## カイ二乗分布
#乱数シミュレーション
n <- c(100,1000,5000,10000)
df <- 2
par(mfrow=c(2,2))
for (i in n) {
  chi_sim <- rchisq(i,df)
  hist(chi_sim,probability=T,col="gray", main = paste("n=",i))
}
mean(chi_sim)
sd(chi_sim)

mu <- df
var <- 2*df
sd <- sqrt(var)
mu
sd

#自由度の違うグラフ
par(mfrow=c(1,1))
x<-seq(0,20,0.1)
curve(dchisq (x,2),from=0,to=20, col="blue")
curve(dchisq (x,4),add=T, col="pink")
curve(dchisq (x,6),add=T, col="orange")
curve(dchisq (x,8),add=T, col="green")
curve(dchisq (x,10),add=T, col="purple")

## F分布
#乱数シミュレーション
n <- c(100,1000,5000,10000)
df1 <- 10
df2 <- 15

par(mfrow=c(2,2))
for (i in n) {
  f_sim <- rf(i,df1,df2)
  hist(f_sim,probability=T,col="gray", main = paste("n=",i))
}
mean(f_sim)
sd(f_sim)

mu <- df2/(df2-2)
var <- 2*df2^2*(df1+df2-2)/(df1*(df2-2)^(2)*(df2-4))
sd <- sqrt(var)
mu
sd

#自由度の違うグラフ
par(mfrow=c(1,1))
x<-seq(0,3,0.1)
curve(df(x,1,10),from=0,to=3,col="blue")
curve(df(x,10,1),add=T, col="pink")
curve(df(x,5,5),add=T, col="green")

## t分布
#乱数シミュレーション
n <- c(100,1000,5000,10000)
df <- 30

par(mfrow=c(2,2))
for (i in n) {
  t_sim <- rt(i,df)
  hist(t_sim,probability=T,col="gray", main = paste("n=",i))
}
mean(t_sim)
sd(t_sim)

mu <- 0
var <- df /(df-2)
sd <- sqrt(var)
mu
sd

#自由度の違うグラフ
par(mfrow=c(1,1))
curve(dt(x,30),from=-4,to=4, col="blue", ylab = "t")
curve(dt(x,5),add=T, col="pink")
curve(dt(x,1),add=T, col="green")


x<-seq(0,3,0.1)
curve(df(x,1,10),from=0,to=3,col="blue")
curve(df(x,10,1),add=T, col="pink")
curve(df(x,5,5),add=T, col="green")