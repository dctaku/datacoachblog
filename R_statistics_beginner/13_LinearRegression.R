## 単回帰分析
## データ準備
#サンプルデータ読み込み
data('cars')
head(cars)
#散布図
plot(cars)
#相関係数
cor(cars$speed, cars$dist)

## 単回帰分析
#モデル当てはめ
cars.lm <- lm(dist~speed, data=cars)
summary(cars.lm)

#残差
residuals(cars.lm)
#cars.lm$residuals

#予測値
yhat <- predict(cars.lm)
u <- residuals(cars.lm)
data.frame(cars, yhat, u)

#グラフによる分析
plot(cars)
abline(cars.lm, lwd=2)

#残差分析
par(mfrow=c(2,2))
plot(cars.lm)
par(mfrow=c(1,1))

## 重回帰分析
#データ確認
str(airquality)
#class(airquality)
#head(airquality)
#対散布図
pairs(airquality[,1:4])
#相関行列
mcor <- cor(airquality[,1:4],use = "complete")
mcor
library(corrplot)
corrplot(mcor)

#モデルあてはめ
air.lm <- lm(Ozone~Solar.R+Wind+Temp, data=airquality)
summary(air.lm)

#回帰診断
plot(air.lm)
library(car)
vif(air.lm)

#交互作用モデルあてはめ
air.lm2 <- lm(Ozone~(Solar.R+Wind+Temp)^2, data=airquality)
summary(air.lm2)
#plot(air.lm2)

#変数選択
air.lm3 <- step(air.lm2)
summary(air.lm3)
round(coefficients(air.lm3),2)
plot(air.lm3)
vif(air.lm3)

#標準化
airquality$sOzone <- scale(airquality$Ozone)
airquality$sSolar.R <- scale(airquality$Solar.R)
airquality$sWind <- scale(airquality$Wind)
airquality$sTemp <- scale(airquality$Temp)

#モデル当てはめ
air.lm4 <- lm(sOzone~(sSolar.R+sWind+sTemp)^2, data=airquality)
air.lm5 <- step(air.lm4)
summary(air.lm5)

#回帰診断
par(mfrow=c(2,2))
vif(air.lm5)
plot(air.lm5)
