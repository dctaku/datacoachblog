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