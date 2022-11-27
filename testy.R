library(readr)
library(ggplot2)
ggplot(merged_class2, aes(answer, euclidean)) + geom_boxplot()

merged_class2 = read_csv("data/merged_class2.csv")
merged_class3 = read_csv("data/merged_class3.csv")

# ANOVA
anova2_euc = aov(euclidean~answer, data = merged_class2)
summary(anova2_euc)
anova2_euc_posthoc = TukeyHSD(anova2_euc, which = "answer", conf.level = 0.95)
par(mar = c(4, 25, 4, 0.1))
plot(anova2_euc_posthoc, las = 1)

# Korelacja Spearmana
merged_class2$answer = factor(merged_class2$answer, 
                              levels = c("Brak", "Bardzo małe", "Umiarkowane",
                                         "Bardzo duże", "Pełne"))
merged_class2$answern = as.integer(merged_class2$answer)
cor(merged_class2$answern, merged_class2$wavehedges, method = "spearman")
