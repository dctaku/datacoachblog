## アンケートのサンプルサイズ

#無限集団
p <- 0.5　#標本比率
delta <- 0.05　#許容誤差
conf.level=0.95　#信頼係数
N <- 200 #有限母集団数

n1 <- qnorm((1-conf.level)/2)^2*p*(1-p)/delta^2
n2 <- N/((delta/qnorm((1-conf.level)/2))^2*((N-1)/(p*(1-p)))+1)
n1
n2
