library(ggplot2)
library(stringr)
library(dplyr)


# likert scale to numbers -------------------------------------------------
# ???????????????????????????????????
# Brak = 1
# Bardzo małe = 0.75
# Umiarkowane = 0.5 
# Bardzo duże = 0.25
# Pełne = 0

# read survey data --------------------------------------------------------
survey_data = read.csv("data/spcomparison_survey_data.csv", header = FALSE)

colnames(survey_data) = c("userID", "questionID", "answer", "datasetID", "sysTime")

survey_data = survey_data %>%
  subset(questionID != "example_question") %>% 
  subset(select=-c(userID, datasetID, sysTime))

survey_data$answer = factor(survey_data$answer,
                            levels = rev(c("Brak","Bardzo małe","Umiarkowane","Bardzo duże","Pełne")))

survey_data$classes = substr(survey_data$questionID, start = 5, stop = 5)

survey_data = survey_data %>% 
  group_by(questionID, classes, answer) %>% 
  count(answer)

# survey_data$answer = recode(survey_data$answer,"Brak"=1,"Bardzo małe"=2,
#                             "Umiarkowane"=3,"Bardzo duże"=4,"Pełne"=5)

survey_data_wide = pivot_wider(survey_data, values_from = n, names_from = answer)


# read distance dataframes ------------------------------------------------
dist2_class = read.csv("data/2classes_dist.csv", row.names = 1)
dist3_class = read.csv("data/3classes_dist.csv", row.names = 1)

dist_merged = rbind(dist2_class, dist3_class) %>% 
  subset(questionID %in% unique(survey_data$questionID))


# merge distance df with survey data --------------------------------------
merged = merge(survey_data, dist_merged, by = "questionID")
merged_wide = merge(survey_data_wide, dist_merged, by = "questionID")


# scatter plot ------------------------------------------------------------
wykres1 = ggplot(merged, aes(x = answer, y = wavehedges, fill = classes)) +
  geom_boxplot()

wykres1


wykres2 = ggplot(merged[merged$classes == "2",], aes(x = answer, y = n)) +
  geom_col()
wykres2

wykres2 = ggplot(merged[merged$classes == "3",], aes(x = answer, y = n)) +
  geom_col()
wykres2

wykres3 = ggplot(merged_wide[merged_wide$classes == "2",], aes(x = euclidean)) +
  geom_histogram(bins = 5)
wykres3

#nie działa
library(forcats)
merged$id = as.factor(as.integer(as.factor(merged$questionID)))
merged_class2 = subset(merged, classes == "2")
ggplot(merged_class2, aes(x = fct_reorder(id, "Brak"), y = n, fill = answer)) +
  geom_bar(position = "fill", stat = "identity") +
  scale_y_continuous(labels = scales::percent) +
  # scale_fill_brewer(name = "Pain Groups:", type = "div", palette = "Spectral", direction = -1) +
  # geom_text(aes(y = -0.02, label = Surgeries), size = 2, color = "gray40") +
  ylab("Proportion") + 
  theme_bw() +
  theme(legend.position = "top")



kruskal.test(jensen.shannon ~ answer, data = merged_class2)



