## 変数の関連性 ##

#データ読み込み
#単位変換：インチからセンチメートル、ポンドからキログラム
data("women")
women$height <- round(women$height / 0.39370,1)
women$weight <- round(women$weight / 2.2046,1)
#グラフで確認
plot(women$weight,women$height)

# ピアソンの積率相関計数
cor(women$height,women$weight, method="pearson")

#共分散から計算
corvar <- var(women$height, women$weight)
sd_h <- sd(women$height)
sd_w <- sd(women$weight)
corvar / (sd_h * sd_w)

# スピアマンの順位相関係数
#データ取得
data("cars")
str(cars)
plot(cars$speed,cars$dist)

#相関係数算出
cor(cars$speed,cars$dist, method="pearson")
cor(cars$speed,cars$dist, method="spearman")

#順位から計算
rank_s <- rank(cars$speed)
rank_d <- rank(cars$dist)
cor(rank_s, rank_d, method="pearson")

# ケンドールの順位相関係数
#データ作成
j2018 <- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18)
j2017 <- c(1,15,2,11,7,13,3,14,10,9,12,5,16,8,17,6,4,18)
team_name <- c("Kawasaki","Hiroshima","Kashima","Sapporo","Urawa","FTokyo","COsaka","Shimizu","GOsaka","Kobe","Sendai","Yokohama","Shonan","Tosu","Nagoya","Iwata","Kashiwa","Nagasaki")
jrank <- cbind(j2017,j2018)
row.names(jrank) <- team_name
plot(j2017,j2018,type="n",xlim=c(18,1),ylim=c(18,1))
text(j2017,j2018,team_name)

#相関係数算出
cor(jrank[,1],jrank[,2], method="spearman")
cor(jrank[,1],jrank[,2], method="kendall")

#順位から計算
#関数定義
hikaku <- function(x1,x2,y1,y2){
  if(((x1<x2)&&(y1<y2))||((x1>x2)&&(y1>y2))){
    1
  } else if(((x1<x2)&&(y1>y2))||(x1>x2)&&(y1<y2)) {
    2
  } else{
    3
  }
}
kumiawase2 <- function(n){
  n*(n-1)/2
}
#順位行列を作成して計算
n <- length(j2018)
rankmat <- matrix(0,n-1,n-1)
for(i in 1:(n-1)){
  for(j in (i+1):n){
    rankmat[j-1,i] <- hikaku(j2017[i],j2017[j],j2018[i],j2018[j])
  }
}
dimnames(rankmat) <- list(team_name[2:n],team_name[1:(n-1)])
(sum(rankmat==1)-sum(rankmat==2))/kumiawase2(n)
cor(jrank[,1],jrank[,2], method="kendall")
