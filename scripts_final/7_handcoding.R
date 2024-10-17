library(tidyverse)
library(readr)
library(caret)
library(xtable)
library(knitr)
library(psych)
library(irr)


options(scipen = 999)
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


load("data/final/final_results2.RData")




all_s <- all_s %>% 
  mutate(is_german = str_trim(german3, side = "both")) %>% 
  filter(
    substr(is_german, 1, 18) == '{"german_economy":' # 100-200 had a completely false answer
    )

table(all_s$is_german)

ge2 <- purrr::map(all_s$is_german, jsonlite::fromJSON)
ge2 <- bind_rows(ge2)

all_s <- all_s %>%
  mutate(relevant_article = ge2$german_economy)

all_r <- all_s %>% 
  filter(relevant_article == "Ja") %>% 
  select(url, date, description, keywords, body, title, total_articles, newspaper, relevant_article)


set.seed(72890)
s <- sample(1:nrow(all_r), 250)
save(s, file = "data/final/relevant_sample250.RData")

all_r[s, ] %>% 
  write_csv("data/final/relevant_articles250.csv")


relevant_sample <- all_r[s, ]
save(relevant_sample, file = "data/final/relevant_sample250.RData")

str_match_all(tolower(relevant_sample$body), "spd|cdu|csu|grüne|linke|fdp|afd")


for (i in 1:nrow(relevant_sample)) {
  # Split the body of the article into sentences marked by either a ".", "!" or a "?"
  sentences <- unlist(strsplit(relevant_sample$body[i], "(?<=[\\.\\?\\!])\\s?(?=[«\"A-Za-z])", perl=T))
  
  # Convert the sentences to a data frame
  sentences_df <- data.frame(sentence = sentences, stringsAsFactors = FALSE)
  
  # Write the sentences to a CSV file
  write_excel_csv(sentences_df, paste0("data/final/article_docs/", "doc_", i, ".csv"))
}







# articles per day --------------------------------------------------------

load("data/final/scrape_data/welt.RData")
load("data/final/scrape_data/welt2.RData")

welt <- welt[ , -c(1:2)]
welt_df$date <- ymd_hms(welt_df$date, tz = "UTC")
welt_df$date <- as.Date(welt_df$date)
welt$paywall <- as.character(welt$paywall)

welt_full <- bind_rows(welt, welt_df)

welt_full <- welt_full %>% 
  group_by(body) %>% 
  mutate(temp = row_number()) %>% 
  filter(temp < 2)

welt_perdate <- welt_full %>% 
  filter(date > "2018-03-13") %>% 
  group_by(date) %>% 
  summarise(n_articles = n())

welt_perdate$newspaper <- "welt"

rm(welt)
rm(welt_df)

load("data/final/scrape_data/zeit.RData")
load("data/final/scrape_data/zeit2.RData")
table(zeit_full$paywall, exclude= NULL) %>% prop.table()
table(welt_full$paywall, exclude= NULL) %>% prop.table()
zeit_full <- bind_rows(zeit, zeit2) %>% 
  group_by(body) %>% 
  mutate(temp = row_number()) %>% 
  filter(temp < 2)
table(zeit_full$paywall)
zeit_full$date <- as.Date(zeit_full$date)

zeit_perdate <- zeit_full %>% 
  filter(date > "2018-03-13") %>% 
  group_by(date) %>% 
  summarise(n_articles = n())

zeit_perdate$newspaper <- "zeit"


all_r <- all_r %>% 
  left_join(zeit_perdate,
            by = c("date", "newspaper"))

all_r <- all_r %>% 
  left_join(welt_perdate,
            by = c("date", "newspaper"))

all_r <- all_r %>% 
  mutate(n_articles = coalesce(n_articles.x, n_articles.y)) %>% 
  select(-n_articles.x, -n_articles.y)

save(all_r, file = "data/final/all_r.RData")

load("data/final/all_r.RData")
nchar(all_r$body) %>% sum()
