## いろいろな平均 ##

## 算術平均
#データ読み込み
#単位変換：インチからセンチメートル、ポンドからキログラム
data("women")
women$height <- round(women$height / 0.39370,1)
women$weight <- round(women$weight / 2.2046,1)

#出力
head(women)
mean(women$height)
hist(women$height)
dim(women)

## 幾何平均

#データ読み込み
#月次データから年次データ変換
#年成長率
data("AirPassengers")
yAirPassengers <- NULL
for (i in 1:12){
  yAirPassengers <- c(yAirPassengers,sum(AirPassengers[i:(i+11)]))
}
yr <- NULL
for (i in 1:(length(yAirPassengers)-1)){
  yr <- c(yr,yAirPassengers[i+1]/yAirPassengers[i])
}

#幾何平均
gmean <- max(cumprod(yr)^(1/length(yr)))
#gmean <- exp(mean(log(abs(yr))))
#算術平均
amean <- mean(yr)

#出力
#初期値から幾何平均で最後の値と一致確認
barplot(yAirPassengers)
yAirPassengers[1]*gmean^11
yAirPassengers[1]*amean^11
yAirPassengers[12]


## 調和平均
#データ作成
dist <- rep(1,10)
speed <- round(runif(10,4,10),0)
time <- dist / speed

#調和平均
hmean <- length(speed)/sum(1/speed)
#算術平均
amean <- mean(speed)

#出力
dist
speed
time
sum(dist) / sum(time)
hmean
amean