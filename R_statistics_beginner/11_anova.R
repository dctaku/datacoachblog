## test statistic for ANOVA

# 対応のない分散分析
data("InsectSprays")
str(InsectSprays)
plot(count ~ spray, data = InsectSprays)
#boxplot(InsectSprays[,1]~InsectSprays[,2])
anova(aov(count~spray,data=InsectSprays))

# 多群の等分散の検定
bartlett.test(InsectSprays$count, InsectSprays$spray)
bartlett.test(count ~ spray, data = InsectSprays)

InsectSprays_cde <- rbind(InsectSprays[InsectSprays$spray == "C",],InsectSprays[InsectSprays$spray == "D",],InsectSprays[InsectSprays$spray == "E",])
bartlett.test(count ~ spray, data = InsectSprays_cde)

# 対応のある分散分析
#各水準のデータ数確認
library(dplyr)
df <- group_by(InsectSprays_cde, spray)
summarize(df, n=n())
#対応のある情報を付加
InsectSprays_cde["NO"] <- factor(rep(1:12,3))
head(InsectSprays_cde)

#対応のある分散分析実行
#summary(aov(count ~ A + spray, InsectSprays_cde))
anova(aov(count ~ spray + NO , InsectSprays_cde))

#対応のない分散分析実行
anova(aov(count ~ spray, InsectSprays_cde))

# 二元配置分散分析
#サンプルデータ作成
a1 <- c(0,2,8,10)
a2 <- c(6,8,9,13)
A <- factor(c(1,1,1,1,2,2,2,2))
B <- factor(c(1,1,2,2,1,1,2,2))
y <- c(a1,a2)
sample_anova <- data.frame(y,A,B)
sample_anova
#交互作用なし二元配置分散分析実行
summary(aov(y~A+B,data=sample_anova))
#交互作用あり二元配置分散分析実行
summary(aov(y~A*B,data=sample_anova))

