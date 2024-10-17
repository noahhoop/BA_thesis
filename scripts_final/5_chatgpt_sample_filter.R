library(httr)
library(tidyverse)
library(openai)
library(jsonlite)
library(caret)
library(xtable)
library(irr)

all_s <- read_delim("data/final/sample_post600.csv", delim = ";")


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


all_s$german3 <- NA

for (i in 70:nrow(all_s)) {
  print(i)
  prmpt <- paste0('Sie sind ein Experte für die Analyse von Nachrichtenartikeln.
                  Die Aufgabe besteht darin, Artikel zu identifizieren, die entweder um Inflation, Arbeitslosigkeit oder Wirtschaftswachstum/ Bruttoinlandsprodukt (BIP) in Deutschland gehen.
                  Ich benötige nur Artikel, die um Deutschland als Ganzes gehen und nicht nur um eine bestimmte Region oder Stadt, und um eines der gennanten Themen gehen.
                  Der Artikel sollte hauptsächlich um dieses Theme gehen, sonst zählt es nicht.
                  Wenn der Artikel über Europa oder nur die EZB handelt, zählt er nicht, auch wenn Deutschland ein Teil davon ist. Es sollen nur Artikel sein, wo diese Themen im deutschlandweiten Kontext thematisiert werden.
                  Wenn der Artikel über Deutschland handelt, aber keines der genannten Themen erwähnt, zählt er nicht.
                  Wenn der Artikel eines dieser Themen erwähnt, aber nicht im deutschen Kontext, zählt er nicht.
                  Bei der Inflation zählt es, wenn der Artikel über die Veränderung der Inflationsrate schreibt. Es zählt nicht, wenn nur irgendwo Inflation erwähnt wird. Sie zählt auch nicht, wenn das Thema Lohnverhandlungen oder Inflationsausgleichsprämien ist.
                  Bei der Arbeitslosigkeit zählt es, wenn es um die Arbeitslosenquote geht oder darum, wie viele Menschen arbeitslos in Gesamtdeutschland sind.
                  Für das Wirtschaftswachstum/ Bruttoinlandsprodukt (BIP) zählt es, wenn es um die Leistung der deutschen Wirtschaft geht und ob sie wächst oder nicht. Es zählt nicht, wenn das BIP nur irgendwo erwähnt wird oder es nur um das Wachstum einzelner Unternehmen oder Branchen geht.
                  
                  Geht es bei folgenden Artikel um die Inflation, die Arbeitslosigkeit oder das Wirtschaftswachstum/BIP in Deutschland allgemein? Wenn der Artikel nur um ein Bundesland in Deutschland geht, antworte mit "Nein".
                  
                  Geben Sie Ihre Antwort im JSON-Format, wie folgt:
                  {"german_economy": "Ja/Nein/Ungewiss"}
                  Optionen:
                  - Ja
                  - Nein
                  - Ungewiss
                  
                  Achten Sie bei Ihrer Analyse auf Genauigkeit und Klarheit und stützen Sie sich bei Ihrer Bewertung auf den angegebenen Kontext und Ihr Fachwissen. Wenn Sie bei der Einstufung unsicher sind, wählen Sie "Ungewiss".
                  Achten Sie genau auf die oben genannten Ausschlusskriterien. Antworten Sie mit "Nein", wenn die Kriterien nicht genau so zutreffen.
                  
                  Artikeltext: ', 
                  all_s$body[i], 
                  "
                  
                  Bei der Aufgabe geht es um Leben und Tod, also achte darauf, alles richtig zu machen!")
  result <- hey_chatGPT(prmpt)
  #Sys.sleep(1)
  while(length(result) == 0){
    result <- hey_chatGPT(prmpt)
    print(result)
    Sys.sleep(1)
  }
  print(result)
  all_s$german3[i] <- result
}


#load("data/final/all_s_gpt.RData")
#all_s <- all_s_gpt

all_s <- all_s %>% 
  mutate(german4 = paste0("{", str_extract(all_s$german3, "(?<=\\{).*(?=\\})"), "}"))
ge2 <- purrr::map(all_s$german4, jsonlite::fromJSON)
ge2 <- bind_rows(ge2)

all_s <- all_s %>% 
  mutate(german_economy3 = ge2$german_economy)
table(all_s$German2)
table(all_s$german_economy3)
all_s$German2 <- factor(all_s$German2, levels = c("Ja", "Nein"))
all_s$german_economy3 <- factor(all_s$german_economy3, levels = c("Ja", "Nein"))

cm <- confusionMatrix(all_s$german_economy3, all_s$German2, mode = "everything")

xtable(cm$table) %>% print(type = "latex", file = "results/tables/confusion_matrix_chatgpt.tex", include.rownames = TRUE)

stats1 <- cm$overall
stats2 <- cm$byClass
stats <- tibble(Statistic = c(names(stats1), names(stats2)),
                Value = c(stats1, stats2) %>% round(3)) %>% 
  filter(Statistic %in% c("Accuracy", "Precision", "Recall", "F1"))

xtable(stats) %>% print(type = "latex", file = "results/tables/text_classification_stats_chatgpt.tex", include.rownames = FALSE)

all_s_gpt <- all_s
save(all_s_gpt, file = "data/final/all_s_gpt.RData")

load("data/final/all_s_gpt.RData")

all_s_gpt %>% 
  select(german_economy3, German2) %>%
  as.matrix() %>% 
  psych::cohen.kappa()


all_s_gpt %>% 
  select(german_economy3, German2) %>% 
  as.matrix() %>% 
  t() %>% 
  kripp.alpha("nominal")
