library(ggplot2)
library(stringr)
library(dplyr)


# read survey data --------------------------------------------------------
survey_data = read.csv("data/spcomparison_survey_data.csv", header = FALSE)

colnames(survey_data) = c("userID", "questionID", "answer", "datasetID", "sysTime")

survey_data = survey_data %>%
  subset(questionID != "example_question") %>% 
  subset(select=-c(userID, datasetID, sysTime))

survey_data = survey_data %>% 
  group_by(questionID, answer) %>% 
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
wykres1 = ggplot(merged, aes(x = answer, y = euclidean)) +
  geom_jitter()

wykres1

