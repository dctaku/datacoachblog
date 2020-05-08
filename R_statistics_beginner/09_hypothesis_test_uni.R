#仮説検定

## １標本のt検定
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

## 母平均の検定
mu <- 158
#t検定
t.test(women$height, mu=mu)

## 母比率の検定
prop.test(300,400,p=0.7)

## 無相関の検定
#データ読み込み
#単位変換：インチからセンチメートル、ポンドからキログラム
data("women")
women$height <- round(women$height / 0.39370,1)
women$weight <- round(women$weight / 2.2046,1)
plot(women$height,women$weight)
length(women$height)
#無相関検定
cor.test(women$height,women$weight)