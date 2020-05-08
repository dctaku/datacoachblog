## 大数の法則

#二項分布
# コイン投げ設定：10試行を10回繰り返す
size <- 10
p <- 0.5
n <- 10
samples <- coin_sim <- rbinom(n,size,p)
hist(samples)

# コイン投げの標本平均をサンプリング
sample_means <- NULL
sample_ses <- NULL
iteration <- c(10,100,1000,10000)

par(mfrow=c(2,2))
for(i in iteration){
  for(j in 1:i){
    each_samples <- rbinom(n, size, p)
    tmp_mean <- mean(each_samples)
    sample_means <- c(sample_means, tmp_mean)
  }
  hist(sample_means,probability=T,col="gray",main = paste("n = ", i)) 
  temp_se <- sd(sample_means)/sqrt(n)
  sample_ses <- c(sample_ses, temp_se)
}
sample_ses


## 中心極限定理

#混合正規分布をつくる
mean1 <- 2
sd1 <- 1
mean2 <- 10
sd2 <- 3
mean <- (mean1 + 2*mean2)/3

samples1 <- rnorm(100,mean=mean1,sd=sd1)
samples2 <- rnorm(200,mean=mean2,sd=sd2)
samples <- c(samples1, samples2)
par(mfrow=c(1,1))
hist(samples)

#混合正規分布からサンプリング
iteration <- 1000
sample_means<-NULL
for(i in 1:iteration){
  samples1 <- rnorm(100,mean=mean1,sd=sd1)
  samples2 <- rnorm(200,mean=mean2,sd=sd2)
  samples <- c(samples1, samples2)
  tmp_mean<-mean(samples)
  sample_means<-c(sample_means, tmp_mean)
}
sample_ses <- sample_means - mean 
par(mfrow=c(1,2))
hist(sample_means)
hist(sample_ses)