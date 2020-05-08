#パッケージ読み込み
library(pwr)
#検出力分析実行
pwr.2p2n.test(h=ES.h(0.10, 0.12),
              n1=10000,
              n2=NULL,
              sig.level = 0.05,
              power = 0.8,
              alternative="two.sided")

pwr.2p2n.test(h=0.05,
              n1=10000,
              n2=NULL,
              sig.level = 0.05,
              power = 0.8,
              alternative="two.sided")


ES.h(0.03, 0.04)

pwr.2p.test(h=ES.h(0.03, 0.04),
              n=NULL,
              sig.level = 0.05,
              power = 0.8,
              alternative="two.sided")

pwr.t.test(d=0.05454982,sig.level = 0.05,
           power = 0.8,
           alternative="two.sided")
