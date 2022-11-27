library(ggplot2)
library(stringr)
library(dplyr)
library(tidyr)
library(ggpubr)

# read survey data --------------------------------------------------------
survey_data = read.csv("data/spcomparison_survey_data.csv", header = FALSE)

colnames(survey_data) = c("userID", "questionID", "answer", "datasetID", "sysTime")

survey_data = survey_data %>%
  subset(questionID != "example_question") %>% 
  subset(select=-c(userID, datasetID, sysTime))

survey_data$answer = factor(survey_data$answer,
                            levels = rev(c("Brak","Bardzo małe","Umiarkowane","Bardzo duże","Pełne")))

survey_data$classes = substr(survey_data$questionID, start = 5, stop = 5)


# read distance dataframes ------------------------------------------------
dist2_class = read.csv("data/2classes_dist.csv", row.names = 1)
dist3_class = read.csv("data/3classes_dist.csv", row.names = 1)

dist_merged = rbind(dist2_class, dist3_class) %>% 
  subset(questionID %in% unique(survey_data$questionID))


# merge distance df with survey data --------------------------------------
merged = merge(survey_data, dist_merged, by = "questionID")
merged$id = as.factor(as.integer(as.factor(merged$questionID)))

merged_class2 = subset(merged, classes == "2")
merged_class3 = subset(merged, classes == "3")

survey_data_grouped = merged %>%
  group_by(questionID, classes, answer, id) %>%
  count(answer)

merged_grouped = merge(survey_data_grouped, dist_merged, by = "questionID")
merged_wide = pivot_wider(merged_grouped, values_from = n, names_from = answer)

merged_grouped_class2 = subset(merged_grouped, classes == "2")
merged_grouped_class3 = subset(merged_grouped, classes == "3")


# boxplot1 ----------------------------------------------------------------
boxplot1 = ggplot(merged, aes(x = answer, y = wavehedges, fill = classes)) +
  geom_boxplot() +
  theme_bw() +
  labs(fill = "Ilość kategorii") +
  scale_fill_manual(values=c(rcartocolor::carto_pal(n = 12, name = "Safe")[c(2,3)])) +
  rremove("xlab")

boxplot2 = ggplot(merged, aes(x = answer, y = jensen.shannon, fill = classes)) +
  geom_boxplot() +
  theme_bw() +
  scale_fill_manual(values=c(rcartocolor::carto_pal(n = 12, name = "Safe")[c(2,3)])) +
  rremove("xlab")

boxplot3 = ggplot(merged, aes(x = answer, y = euclidean, fill = classes)) +
  geom_boxplot() +
  theme_bw() +
  scale_fill_manual(values=c(rcartocolor::carto_pal(n = 12, name = "Safe")[c(2,3)])) +
  rremove("xlab")

boxplot4 = ggplot(merged, aes(x = answer, y = jaccard, fill = classes)) +
  geom_boxplot() +
  theme_bw() +
  scale_fill_manual(values=c(rcartocolor::carto_pal(n = 12, name = "Safe")[c(2,3)])) +
  rremove("xlab")

ggarrange(boxplot1, boxplot2, boxplot3, boxplot4,
          ncol = 2, nrow = 2, common.legend = TRUE, legend="bottom") %>% 
  ggsave(filename = "plots/boxplots1.png", width = 3000, height = 2500, units = "px")



# stacked barplot1 --------------------------------------------------------
library(forcats)

merged_class2_max = merged_grouped_class2 |>
  select(id, n, answer) |>
  group_by(id) |>
  mutate(np = n/sum(n)) |>
  arrange(-np) |>
  filter(answer == "Brak") |>
  pull(id)

diff_classes2 = setdiff(unique(merged_grouped_class2$id), merged_class2_max)
merged_class2_max_all = c(merged_class2_max, diff_classes2)

stackedplot1 = ggplot(merged_grouped_class2, aes(x = factor(id, levels = merged_class2_max_all), y = n, fill = answer)) +
  geom_bar(position = "fill", stat = "identity") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(name = "Odpowiedź:", values=c(rcartocolor::carto_pal(n = 5, name = "Safe"))) +
  ylab("Udział odpowiedzi") +
  xlab("Numer pytania (2 kategorie)") +
  theme_bw()


merged_class3_max = merged_grouped_class3 |>
  select(id, n, answer) |>
  group_by(id) |>
  mutate(np = n/sum(n)) |>
  arrange(-np) |>
  filter(answer == "Brak") |>
  pull(id)

diff_classes3 = setdiff(unique(merged_grouped_class3$id), merged_class3_max)
merged_class3_max_all = c(merged_class3_max, diff_classes3)

stackedplot2 = ggplot(merged_grouped_class3, aes(x = factor(id, levels = merged_class3_max_all), y = n, fill = answer)) +
  geom_bar(position = "fill", stat = "identity") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(name = "Odpowiedź:", values=c(rcartocolor::carto_pal(n = 5, name = "Safe"))) +
  ylab("Udział odpowiedzi") +
  xlab("Numer pytania (3 kategorie)") +
  theme_bw()

ggarrange(stackedplot1, stackedplot2, ncol = 1, nrow = 2, common.legend = TRUE, legend="top") %>% 
  ggsave(filename = "plots/stacked_barplots1.png", width = 3000, height = 2500, units = "px")


# kruskal-wallis test table -----------------------------------------------
library(scales)
jensh_c2 = as.numeric(scientific(kruskal.test(jensen.shannon ~ answer, data = merged_class2)$p.value, digits = 2))
euclidean_c2 = as.numeric(scientific(kruskal.test(euclidean ~ answer, data = merged_class2)$p.value, digits = 2))
wavehedges_c2 = as.numeric(scientific(kruskal.test(wavehedges ~ answer, data = merged_class2)$p.value, digits = 2))
jaccard_c2 = as.numeric(scientific(kruskal.test(jaccard ~ answer, data = merged_class2)$p.value, digits = 2))

jensh_c3 = as.numeric(scientific(kruskal.test(jensen.shannon ~ answer, data = merged_class3)$p.value, digits = 2))
euclidean_c3 = as.numeric(scientific(kruskal.test(euclidean ~ answer, data = merged_class3)$p.value, digits = 2))
wavehedges_c3 = as.numeric(scientific(kruskal.test(wavehedges ~ answer, data = merged_class3)$p.value, digits = 2))
jaccard_c3 = as.numeric(scientific(kruskal.test(jaccard ~ answer, data = merged_class3)$p.value, digits = 2))

table1 = data.frame("miara niepodobieństwa" = c("jensen-shannon", "euclidean", "wavehedges", "jaccard"),
                    "2 klasy" = c(jensh_c2, euclidean_c2, wavehedges_c2, jaccard_c2),
                    "3 klasy" = c(jensh_c3, euclidean_c3, wavehedges_c3, jaccard_c3))
table1 = arrange(table1, X2.klasy)

library(kableExtra)
kbl(table1, "html", col.names = c("Miara niepodobieństwa", "2 klasy", "3 klasy")) %>%
  add_header_above(c(" ", "Wartość p-value" = 2)) %>% 
  kable_styling(font_size = 30) %>% 
  kable_classic(full_width = F) %>%
  save_kable(file = "plots/table1.png")


# answer count plots ------------------------------------------------------

barplot1 = ggplot(survey_data, aes(x = answer, fill = classes)) +
  geom_bar(position = position_dodge()) +
  scale_fill_manual(name = "Liczba kategorii:",
                    values=c(rcartocolor::carto_pal(n = 12, name = "Safe")[c(2,3)])) +
  ylab("Liczba odpowiedzi") +
  theme_bw() +
  theme(legend.position = "top") +
  rremove("xlab")

ggsave(barplot1, filename = "plots/barplot1.png",
       width = 1600, height = 1200, units = "px")

