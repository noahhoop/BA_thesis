library(httr)
library(tidyverse)
library(openai)
library(jsonlite)
library(caret)
library(xtable)
library(psych)
library(irr)

load("data/vali_sample.RData")

all_s <- vali_sample


my_API <- "XXX"

#The "hey_chatGPT function will help you access the API and prompt GPT 
hey_chatGPT <- function(answer_my_question) {
  chat_GPT_answer <- POST(
    url = "https://api.openai.com/v1/chat/completions",
    add_headers(Authorization = paste("Bearer", my_API)),
    content_type_json(),
    encode = "json",
    body = list(
      model = "gpt-4o",
      temperature = 0, 
      messages = list(
        list(
          role = "user",
          content = answer_my_question
        )
      )
    )
  )
  str_trim(content(chat_GPT_answer)$choices[[1]]$message$content)
}


all_s$sentiment <- NA

for (i in 1:nrow(all_s)) {
  print(i)
  prmpt <- paste0('Sie sind ein Experte für die Analyse von Nachrichtenartikeln.
                  Du erhältst einen Artikel, in dem es um verschiedene wirtschaftliche Themen geht. Deine Aufgabe ist es, ausschließlich die Abschnitte zu analysieren, die sich auf Entwicklungen bei der Arbeitslosigkeit, der Inflation oder dem Wirtschaftswachstum/Bruttoinlandsprodukt (BIP) in Deutschland beziehen. Ignoriere alle anderen Themen und auch Berichte über spezifische Regionen in Deutschland, die EU oder die Weltwirtschaft.

                  Ordne zuerst ein, ob der Artikel überhaupt relevante Informationen hat, wie in den Kriterien oben beschrieben.
Gib dann eine Bewertung ab, ob der Artikel positiv, neutral, negativ oder ungewiss über die Entwicklung in Deutschland berichtet. Begründe deine Bewertung und gib eine Bewertung auf einer Skala von 0 bis 10 ab, wobei 0 vollständig negativ und 10 vollständig positiv bedeutet.

Formatiere deine Antwort im JSON-Format wie folgt:
                  Geben Sie Ihre Antwort im JSON-Format, wie folgt:
                  {"relevant": "Ja/Nein","sentiment": "Negativ/Neutral/Positiv/Ungewiss", "sentiment_score": "0-10", "justification": "Begründen Sie Ihre Antwort"}
            
                  Artikeltext: ', 
                  all_s$body[i])
  
  
  result <- hey_chatGPT(prmpt)
  #Sys.sleep(1)
  while(length(result) == 0){
    result <- hey_chatGPT(prmpt)
    print(result)
    Sys.sleep(1)
  }
  print(result)
  all_s$sentiment[i] <- result
}

t <- all_s

all_s <- all_s %>% 
  mutate(sentiment = gsub("```json|```", "", all_s$sentiment))
ge2 <- purrr::map(all_s$sentiment, jsonlite::fromJSON)
ge2 <- bind_rows(ge2)

all_s <- all_s %>% 
  mutate(sentiment_c = ifelse(ge2$sentiment %in% c("Negativ", "Neutral", "Positiv"), ge2$sentiment, NA) %>% factor(ordered = TRUE),
         sentiment_n = as.numeric(ge2$sentiment_score),
         relevant = ge2$relevant)



all_s <- all_s %>% 
  mutate(across(ends_with("scale"),
                ~ cut(., breaks = c(-1, -0.1, 0.1, 1),
                      labels = c("Negativ", "Neutral", "Positiv"),
                      include.lowest = TRUE),
                .names = "{.col}_c")) %>% 
  mutate(logit_scale_c = case_when(n_rel == 0 ~ NA,
                                   logit_scale < 0 ~ "Negativ",
                                   logit_scale == 0 ~ "Neutral",
                                   logit_scale > 0 ~ "Positiv")) %>% 
  mutate(across(ends_with("scale_c"),
                ~ factor(., ordered = TRUE)))

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

sentiment_gpt <- all_s
save(sentiment_gpt, file = "data/final/sentiment_gpt_gpt.RData")
load("data/final/sentiment_gpt_gpt.RData")

all_s %>% 
  select(sentiment_c, prop_scale_c) %>% 
  cohen.kappa()

all_s %>% 
  select(sentiment_c, prop_scale_c) %>% 
  as.matrix() %>% 
  t() %>% 
  kripp.alpha("ordinal")
