# ２群の仮説検定

## 対応のない２郡の平均の差
#データ取得
#EuStockMarketsは、欧州の主要株価指数の1991年〜1998年の日次データ
#ドイツDAX、スイスSMI、フランスCAC、英国FTSE

#データ加工
stock <- EuStockMarkets
r_stock <- diff(log(stock))
par(mfrow=c(1,2))
hist(r_stock[,"DAX"])
hist(r_stock[,"CAC"])

#平均・標準偏差確認
apply(r_stock, 2, mean)
apply(r_stock, 2, sd)

#t検定（等分散仮定）
t.test(r_stock[,"DAX"], r_stock[,"CAC"],var.equal = T)

#F検定（等分散の検定）
var.test(r_stock[,"DAX"], r_stock[,"CAC"])

#t検定（ウェルチ・等分散仮定しない）
t.test(r_stock[,"DAX"], r_stock[,"FTSE"],var.equal = F)


## 対応のある２群の平均の検定
#データ取得
data("sleep")
#データ加工
be <- sleep[1:10,]
af <- sleep[11:20,]
data <- af[,1] - be[,1]
matrix(c(be[,1],af[,1],data),10,3)
#t検定
t.test(data)

## 対応のない２郡の比率の差の検定
prop.test(c(120,120),c(4000, 3000))

