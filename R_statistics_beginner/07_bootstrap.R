## ブートストラップ法

#元データ作成：正規乱数20サンプル
n <- 20
samples <- rnorm(n)
samples_mean <- mean(samples)
samples_sd <- sd(samples)

#リサンプリング1000回
iteration <- 1000
resamples <- NULL
for(i in 1:iteration){
  sample_number <- floor(runif(10,min = 1, max = n+1 ))
  each_samples <- s_data[sample_number]
  resamples <- c(resamples, each_samples)
}

#比較
par(mfrow=c(1,2))
hist(samples)
hist(resamples)

mean(samples)
sd(samples)
mean(resamples)
sd(resamples)

t.test(samples)
t.test(resamples)
