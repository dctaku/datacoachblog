## 多重比較法

# Bonferroni法
#データ準備
data("InsectSprays")
str(InsectSprays)
plot(count ~ spray, data = InsectSprays)
InsectSprays_cde <- rbind(InsectSprays[InsectSprays$spray == "C",],InsectSprays[InsectSprays$spray == "D",],InsectSprays[InsectSprays$spray == "E",])
#分散分析
anova(aov(count ~ spray, InsectSprays_cde))

#bonferroni法のt検定
attach(InsectSprays_cde)
pairwise.t.test(count,spray, p.adj="bonferroni")
detach()

#Tukey-Kramer法
TukeyHSD(aov(count~spray,data=InsectSprays_cde))

#Dunnett法
library(multcomp)
summary(glht(aov(count~spray, data = InsectSprays_cde),linfct=mcp(spray="Dunnett")))
