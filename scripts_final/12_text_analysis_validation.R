###
# script: validation.R
# description: Validation of text analysis
###

# If you have trouble loading some of the packages make sure to update your installed packages, especially tidyverse
if (!require("pacman")) install.packages("pacman")
packages <- c("tidyverse", "caret", "xtable", "irr", "psych")
pacman::p_load(char = packages)

# Filter sample validation: ChatGPT ---------------------------------------



load("data/final/all_s_gpt.RData")

all_s <- all_s_gpt

cm <- confusionMatrix(all_s$german_economy3, all_s$German2, mode = "everything")

xtable(cm$table) %>% print(type = "latex", file = "results/tables/confusion_matrix_chatgpt.tex", include.rownames = TRUE)

stats1 <- cm$overall
stats2 <- cm$byClass
stats <- tibble(Statistic = c(names(stats1), names(stats2)),
                Value = c(stats1, stats2) %>% round(3)) %>% 
  filter(Statistic %in% c("Accuracy", "Precision", "Recall", "F1"))

xtable(stats) %>% print(type = "latex", file = "results/tables/text_classification_stats_chatgpt.tex", include.rownames = FALSE)



all_s_gpt %>% 
  select(german_economy3, German2) %>%
  as.matrix() %>% 
  psych::cohen.kappa()


all_s_gpt %>% 
  select(german_economy3, German2) %>% 
  as.matrix() %>% 
  t() %>% 
  kripp.alpha("nominal")




# Filter sample validation: llama3.1 -------------------------------------------------

load("data/final/sample_results600.RData")
ge2 <- purrr::map(all_s$german4, jsonlite::fromJSON)
ge2 <- bind_rows(ge2)

all_s <- all_s %>% 
  mutate(german_economy3 = ge2$german_economy)
table(all_s$German2)
table(all_s$german_economy3)
all_s$German2 <- factor(all_s$German2, levels = c("Ja", "Nein"))
all_s$german_economy3 <- factor(all_s$german_economy3, levels = c("Ja", "Nein"))

cm <- confusionMatrix(all_s$german_economy3, all_s$German2, mode = "everything")

xtable(cm$table) %>% print(type = "latex", file = "results/tables/confusion_matrix.tex", include.rownames = TRUE)

stats1 <- cm$overall
stats2 <- cm$byClass
stats <- tibble(Statistic = c(names(stats1), names(stats2)),
                Value = c(stats1, stats2) %>% round(3)) %>% 
  filter(Statistic %in% c("Accuracy", "Precision", "Recall", "F1"))

xtable(stats) %>% print(type = "latex", file = "results/tables/text_classification_stats.tex", include.rownames = FALSE)

all_s %>% 
  select(german_economy3, German2) %>%
  as.matrix() %>% 
  psych::cohen.kappa()


all_s %>% 
  select(german_economy3, German2) %>% 
  as.matrix() %>% 
  t() %>% 
  kripp.alpha("nominal")




# Sentiment sample validation: llama3.1 -----------------------------------


load("data/final/vali_best.RData")

vali %>% 
  select(sentiment_c, prop_scale_c) %>% 
  cohen.kappa()

vali %>% 
  select(sentiment_c, prop_scale_c) %>% 
  as.matrix() %>% 
  t() %>% 
  kripp.alpha("ordinal")

vali %>% 
  select(ends_with("scale"), sentiment_n) %>% 
  cor(use = "pairwise.complete.obs",
      method = "pearson")
cor.test(vali$prop_scale, vali$sentiment_n)

vali %>% 
  select(ends_with("_c")) %>% 
  mutate(across(everything(),
                ~ as.numeric(.))) %>% 
  cor(use = "pairwise.complete.obs",
      method = "kendall")

cm <- confusionMatrix(vali$sentiment_c, factor(vali$prop_scale_c), mode = "everything")


xtable(cm$table) %>% print(type = "latex", file = "results/tables/confusion_matrix_sentiment.tex", include.rownames = TRUE)

stats1 <- cm$overall
stats2 <- cm$byClass

stats2 <- stats2 %>% 
  as.data.frame() %>% 
  select(Precision, Recall, F1)
# stats <- tibble(Statistic = c(names(stats1), names(stats2)),
#                 Value = c(stats1, stats2) %>% round(3))

xtable(stats2) %>% print(type = "latex", file = "results/tables/text_sentiment_stats.tex", include.rownames = TRUE)
xtable(as.data.frame(stats1)) %>% print(type = "latex", file = "results/tables/text_sentiment_stats2.tex", include.rownames = TRUE)




# sentiment sample validation: ChatGPT ------------------------------------


load("data/final/sentiment_gpt_gpt.RData")
all_s <- sentiment_gpt

all_s %>% 
  select(ends_with("scale"), sentiment_n) %>% 
  cor(use = "pairwise.complete.obs",
      method = "pearson")

all_s %>% 
  select(ends_with("_c")) %>% 
  mutate(across(everything(),
                ~ as.numeric(.))) %>% 
  cor(use = "pairwise.complete.obs",
      method = "kendall")

table(all_s$sentiment_c)
table(all_s$prop_scale_c)

with(all_s, table(prop_scale_c, sentiment_c))

#all_s <- all_s %>% 
#  filter(relevant == "Ja")

cm <- confusionMatrix(all_s$sentiment_c, factor(all_s$prop_scale_c), mode = "everything")



xtable(cm$table) %>% print(type = "latex", file = "results/tables/confusion_matrix_sentiment_chatgpt.tex", include.rownames = TRUE)

stats1 <- cm$overall
stats2 <- cm$byClass
stats2 <- stats2 %>% 
  as.data.frame() %>% 
  select(Precision, Recall, F1)

xtable(stats2) %>% print(type = "latex", file = "results/tables/text_sentiment_stats_chatgpt.tex", include.rownames = TRUE)




all_s %>% 
  select(sentiment_c, prop_scale_c) %>% 
  as.matrix() %>% 
  t() %>% 
  kripp.alpha("ordinal")

