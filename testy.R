library(readr)
library(ggplot2)
library(dplyr)

merged_class2 = read_csv("data/merged_class2.csv")
merged_class3 = read_csv("data/merged_class3.csv")

ggplot(merged_class2, aes(answer, euclidean)) + geom_boxplot()

# ANOVA -------------------------------------------------------------------
# anova2_euc = aov(euclidean~answer, data = merged_class2)
# summary(anova2_euc)
# anova2_euc_posthoc = TukeyHSD(anova2_euc, which = "answer", conf.level = 0.95)
# par(mar = c(4, 25, 4, 0.1))
# plot(anova2_euc_posthoc, las = 1)

anova_euc2 = aov(euclidean~answer, data = merged_class2) %>% 
  TukeyHSD(which = "answer", conf.level = 0.95)

anova_jsd2 = aov(jensen.shannon~answer, data = merged_class2) %>% 
  TukeyHSD(which = "answer", conf.level = 0.95)

anova_jac2 = aov(jaccard~answer, data = merged_class2) %>% 
  TukeyHSD(which = "answer", conf.level = 0.95)

anova_wav2 = aov(wavehedges~answer, data = merged_class2) %>% 
  TukeyHSD(which = "answer", conf.level = 0.95)

par(mfrow=c(2,2))
plot(anova_euc2)
plot(anova_jsd2)
plot(anova_jac2)
plot(anova_wav2)

anova_euc3 = aov(euclidean~answer, data = merged_class3) %>% 
  TukeyHSD(which = "answer", conf.level = 0.95)

anova_jsd3 = aov(jensen.shannon~answer, data = merged_class3) %>% 
  TukeyHSD(which = "answer", conf.level = 0.95)

anova_jac3 = aov(jaccard~answer, data = merged_class3) %>% 
  TukeyHSD(which = "answer", conf.level = 0.95)

anova_wav3 = aov(wavehedges~answer, data = merged_class3) %>% 
  TukeyHSD(which = "answer", conf.level = 0.95)


plot(anova2_euc_posthoc)




# Korelacja Spearmana -----------------------------------------------------
merged_class2$answer = factor(merged_class2$answer, 
                              levels = c("Brak", "Bardzo małe", "Umiarkowane",
                                         "Bardzo duże", "Pełne"))
merged_class2$answern = as.integer(merged_class2$answer)

wavecor2 = round(cor(merged_class2$answern, merged_class2$wavehedges, method = "spearman"),2)
jacccor2 = round(cor(merged_class2$answern, merged_class2$jaccard, method = "spearman"),2)
jeshcor2 = round(cor(merged_class2$answern, merged_class2$jensen.shannon, method = "spearman"),2)
euclcor2 = round(cor(merged_class2$answern, merged_class2$euclidean, method = "spearman"),2)


merged_class3$answer = factor(merged_class3$answer, 
                              levels = c("Brak", "Bardzo małe", "Umiarkowane",
                                         "Bardzo duże", "Pełne"))
merged_class3$answern = as.integer(merged_class3$answer)

wavecor3 = round(cor(merged_class3$answern, merged_class3$wavehedges, method = "spearman"),2)
jacccor3 = round(cor(merged_class3$answern, merged_class3$jaccard, method = "spearman"),2)
jeshcor3 = round(cor(merged_class3$answern, merged_class3$jensen.shannon, method = "spearman"),2)
euclcor3 = round(cor(merged_class3$answern, merged_class3$euclidean, method = "spearman"),2)

# tabela z podsumowaniem

table1 = data.frame("miara niepodobieństwa" = c("jensen-shannon", "euclidean", "wavehedges", "jaccard"),
                    "2 klasy" = c(jeshcor2, euclcor2, wavecor2, jacccor2),
                    "3 klasy" = c(jeshcor3, euclcor3, wavecor3, jacccor3))
table1 = arrange(table1, X2.klasy)


library(kableExtra)
kbl(table1, "html", col.names = c("Miara niepodobieństwa", "2 klasy", "3 klasy")) %>%
  add_header_above(c(" ", "Korelacja Spearmana" = 2)) %>% 
  kable_styling(font_size = 30) %>% 
  kable_classic(full_width = F) %>%
  save_kable(file = "plots/table2.png")

