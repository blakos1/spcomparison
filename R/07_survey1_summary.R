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
  factor(levels = c("Brak","Bardzo małe","Umiarkowane","Bardzo duże","Pełne"))

survey_data$classes = substr(survey_data$questionID, start = 5, stop = 5)


# sum of each type of answer ----------------------------------------------
totals_df = survey_data %>%
  count(classes, answer) %>% 
  pivot_wider(names_from = classes, values_from = n) %>% 
  mutate(total = `2` + `3`) %>% 
  bind_rows(summarise(., across(where(is.numeric), sum),
                      across(where(is.factor), ~'Suma'))) %>% 
  mutate(percent2 = round((`2` / (sum(`2`) - max(`2`)))*100, 1),
         percent3 = round((`3` / (sum(`3`) - max(`3`)))*100, 1),
         percent_total = round((total / (sum(total) - max(total)))*100, 1))

dir.create("tables")
write.csv(totals_df, file = "tables/tab-totals_df.csv")

# total answers for each question -----------------------------------------
total_answers = survey_data %>% 
  count(questionID) %>% 
  rename(total_answers = n) %>% 
  mutate(row1 = substr(questionID, 17,17),
         row2 = substr(questionID, 40,40),
         same_row = (row1 == row2),
         col1 = substr(questionID, 22,22),
         col2 = substr(questionID, 45,45),
         same_col = (col1 == col2),
         question_group = case_when(
           same_col == TRUE & same_row == FALSE ~ "same col/ent diff row/rmi",
           same_col == FALSE & same_row == TRUE ~ "diff col/ent same row/rmi",
           same_col == FALSE & same_row == FALSE ~ "diff col/ent diff row/rmi"
         )
         ) %>% 
  subset(select = -c(row1, row2, col1, col2, same_row, same_col))


# group data to get mode of answers ---------------------------------------
survey_data$answer = survey_data$answer %>% 
  factor(levels = c("Brak","Bardzo małe","Umiarkowane","Bardzo duże","Pełne")) %>% 
  as.integer()

survey_data_wide = survey_data %>%
  count(questionID, answer, classes) %>% 
  pivot_wider(values_from = n, names_from = answer)

survey_data_wide$mode = as.integer(colnames(survey_data_wide)[apply(survey_data_wide, 1, which.max)]) #finds which answer was chosen most
survey_data_wide$mode_n = apply(survey_data_wide[,4:ncol(survey_data_wide)-1], 1, max, na.rm=TRUE) #finds how many times most answer was chosen

#questions with most agreement
agree_df = survey_data_wide %>% 
  merge(total_answers, by = "questionID") %>% 
  mutate(agree_perc = round(mode_n / total_answers, 2)) %>% 
  arrange(desc(agree_perc)) %>% 
  subset(select=-c(`1`,`2`,`3`,`4`,`5`))

#agreement by question type
qtype_agree_df1 = agree_df %>% 
  group_by(question_group) %>% 
  summarize(sum_mode_n = sum(mode_n),
            sum_total_answers = sum(total_answers)) %>% 
  bind_rows(summarise(., across(where(is.numeric), sum),
                      across(where(is.character), ~'Łącznie'))) %>% 
  mutate(agree_perc = round(sum_mode_n / sum_total_answers, 2))

qtype_agree_df2 = agree_df %>% 
  group_by(classes) %>% 
  summarize(sum_mode_n = sum(mode_n),
            sum_total_answers = sum(total_answers)) %>% 
  bind_rows(summarise(., across(where(is.numeric), sum),
                      across(where(is.character), ~'Łącznie'))) %>% 
  mutate(agree_perc = round(sum_mode_n / sum_total_answers, 2))

qtype_agree_df3 = agree_df %>% 
  group_by(question_group, classes) %>% 
  summarize(sum_mode_n = sum(mode_n),
            sum_total_answers = sum(total_answers)) %>% 
  bind_rows(summarise(., across(where(is.numeric), sum),
                      across(where(is.character), ~'Łącznie'))) %>% 
  mutate(agree_perc = round(sum_mode_n / sum_total_answers, 2)) %>% 
  arrange(question_group, classes)

write.csv(qtype_agree_df1, file = "tables/tab-qtype_agree_df1.csv")
write.csv(qtype_agree_df2, file = "tables/tab-qtype_agree_df2.csv")
write.csv(qtype_agree_df3, file = "tables/tab-qtype_agree_df3.csv")



agree_df %>% 
  ggplot(aes(fct_reorder(questionID, agree_perc),agree_perc, fill = question_group)) +
  geom_col()

agree_df %>% 
  filter(classes == 2) %>% 
  ggplot(aes(fct_reorder(questionID, agree_perc),agree_perc, fill = question_group)) +
  geom_col()

agree_df %>% 
  filter(classes == 3) %>% 
  ggplot(aes(fct_reorder(questionID, agree_perc),agree_perc, fill = question_group)) +
  geom_col()


df = survey_data_wide %>% 
  subset(select=-c(`1`,`2`,`3`,`4`,`5`))

remove(survey_data, survey_data_wide)


# read distance dataframes ------------------------------------------------
data2 = read.csv("data/2classes_dist.csv")
data3 = read.csv("data/3classes_dist.csv")

merged_dist = rbind(data2, data3)

merged_dist[sapply(merged_dist, is.infinite)] = NA

merged_dist = merged_dist %>% 
  subset(select = -c(X, minkowski)) %>% 
  subset(questionID %in% unique(df$questionID))
  # select(c("euclidean", "questionID"))

methods = colnames(merged_dist)[colnames(merged_dist) != "questionID"]

corr_df = data.frame(method = methods)

# data22$euclidean = rescale(data22$euclidean) #scale the values to 0-1 range


# calculate correlation ---------------------------------------------------
merged = merge(df, merged_dist, by = "questionID")

remove(data2, data3)

calculate_correlations = function(method_name, df){
  output = cor(df$mode, df[method_name], method = "spearman")
  round(output[1], 2)
}

# dowód na to że średnia z korelacji grup != korelacji z całego zbioru
# cor(merged$mode, merged$wavehedges, method = "spearman") %>% round(2)
# cor(merged2$mode, merged2$wavehedges, method = "spearman") %>% round(2)
# cor(merged3$mode, merged3$wavehedges, method = "spearman") %>% round(2)
# cor.test(merged3$mode, merged3$wavehedges, method = "spearman")


test = mtcars %>% 
  filter(cyl %in% c("4", "8"))

test = test[-c(2, 3, 6),]

test4 = test %>% 
  filter(cyl == 4)

test8 = test %>% 
  filter(cyl == 8)

cor(test$carb, test$qsec, method = "spearman") %>% round(2)
cor(test4$carb, test4$qsec, method = "spearman") %>% round(2)
cor(test8$carb, test8$qsec, method = "spearman") %>% round(2)
# 

merged2 = merged %>% subset(classes == 2)
merged3 = merged %>% subset(classes == 3)

# calculate_correlations("euclidean", merged)
corr_df$corr_all = unlist(map(corr_df$method, calculate_correlations, merged))
corr_df$corr_2cat = unlist(map(corr_df$method, calculate_correlations, merged2))
corr_df$corr_3cat = unlist(map(corr_df$method, calculate_correlations, merged3))

corr_df$corr_rank = abs(corr_df$corr_all)

corr_df$test = round((corr_df$corr_2cat + corr_df$corr_3cat)/2,2)
corr_df$test2 = corr_df$corr_all - corr_df$test

corr_df = corr_df %>% 
  arrange(desc(corr_rank))

write.csv(corr_df, file = "tables/tab-corr_df.csv")


# plots -------------------------------------------------------------------
corr_df[which.max(abs(corr_df$corr_2cat)),] #method most correlated with answers
corr_df[which.min(abs(corr_df$corr_2cat)),] #method least correlated with answers

corr_df %>% 
  ggplot(aes(corr_2cat, corr_3cat, color = corr_all > 0)) +
  geom_point() +
  stat_ellipse()


high_corr = ggplot(merged2, aes(fidelity, mode)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  theme(aspect.ratio = 1)
high_corr

low_corr = ggplot(merged2, aes(clark, mode)) +
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




# top 4 measures plot -----------------------------------------------------
merged_both = merged %>% 
  mutate(classes = "Łącznie") %>% 
  rbind(merged) %>% 
  rename(Kategorie = classes)

merged2long = select(merged_both, questionID, Kategorie, mode, euclidean:avg) %>% 
  pivot_longer(euclidean:avg)

merged2longsel = filter(merged2long, name %in% c("canberra", "clark", "euclidean", "divergence"))

top4measures = ggplot(merged2longsel, aes(value,mode, color = Kategorie)) +
  geom_point() +
  ylim(1, 5) +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~name, scales = "free_x") +
  theme_bw() +
  labs(x = "Wartość niepodobieństwa", y = "Najczęstsza odpowiedź")
top4measures

ggsave(top4measures, filename = "plots/fig-top4measures.png", width = 1800, height = 1600, units = "px")
