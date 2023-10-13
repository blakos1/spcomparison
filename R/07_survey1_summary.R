library(tidyverse)
library(scales)
library(ggplot2)
library(gridExtra)

# read survey data --------------------------------------------------------
survey_data = read.csv("data/spcomparison_survey_data.csv", header = FALSE)

colnames(survey_data) = c("userID", "questionID", "answer", "datasetID", "sysTime")

survey_data = survey_data %>%
  subset(questionID != "example_question") %>% 
  subset(select=-c(userID, datasetID, sysTime))

survey_data$answer = survey_data$answer %>% 
  factor(levels = rev(c("Brak","Bardzo małe","Umiarkowane","Bardzo duże","Pełne"))) %>% 
  as.integer()

survey_data$classes = substr(survey_data$questionID, start = 5, stop = 5)

# group data to get mode of answers
survey_data_grouped = survey_data %>%
  group_by(questionID, classes, answer) %>%
  count(answer)


survey_data_wide = pivot_wider(survey_data_grouped, values_from = n, names_from = answer)


survey_data_wide$mode = as.integer(colnames(survey_data_wide)[apply(survey_data_wide,1,which.max)])

df = survey_data_wide %>% 
  subset(select=-c(`1`,`2`,`3`,`4`,`5`))

remove(survey_data, survey_data_grouped, survey_data_wide)


# read distance dataframes ------------------------------------------------
data2 = read.csv("data/2classes_dist.csv")
data3 = read.csv("data/3classes_dist.csv")

merged_dist = rbind(data2, data3)

merged_dist[sapply(merged_dist, is.infinite)] = NA

merged_dist = merged_dist %>% 
  subset(select = -c(X, minkowski)) %>% 
  subset(questionID %in% unique(df$questionID)) %>% 
  na.omit()
  # select(c("euclidean", "questionID"))

methods = colnames(merged_dist)[colnames(merged_dist) != "questionID"]

corr_df = data.frame(method = methods)

# data22$euclidean = rescale(data22$euclidean) #scale the values to 0-1 range


# calculate correlation ---------------------------------------------------
merged = merge(df, merged_dist, by = "questionID")

remove(data2, data3, merged_dist, df)

calculate_correlations = function(method_name, df){
  output = cor(df$mode, df[method_name])
  round(output[1], 2)
}

merged2 = merged %>% subset(classes == 2)
merged3 = merged %>% subset(classes == 3)

# calculate_correlations("euclidean", merged)
corr_df$corr_all = unlist(map(corr_df$method, calculate_correlations, merged))
corr_df$corr_2cat = unlist(map(corr_df$method, calculate_correlations, merged2))
corr_df$corr_3cat = unlist(map(corr_df$method, calculate_correlations, merged3))


# plots -------------------------------------------------------------------
corr_df[which.min(corr_df$corr_2cat),] #method most correlated with answers
corr_df[which.max(corr_df$corr_2cat),] #method least correlated with answers


high_corr = ggplot(merged2, aes(fidelity, mode)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  theme(aspect.ratio = 1)
high_corr

low_corr = ggplot(merged2, aes(wavehedges, mode)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  theme(aspect.ratio = 1)
low_corr

p3 = ggplot(merged2, aes(euclidean, mode)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  theme(aspect.ratio = 1)

p4 = ggplot(merged2, aes(neyman, mode)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  theme(aspect.ratio = 1)


grid.arrange(high_corr, low_corr, p3, p4, ncol = 2, nrow = 2)


