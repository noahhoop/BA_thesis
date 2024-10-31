# library(tidyverse)
library(rollama)
library(readr)
library(magrittr)
library(dplyr)
library(stringr)
library(jsonlite)
library(R.utils)
library(purrr)
#library(caret)
rm(list=ls())

#pull_model("llama3.1")

# [[]]
# { } 
# \ 

# alt gr + 7-0 Klammern
# alt gr + < ->| 



set.seed(59128)
load("data/all_r.RData")

all_s <- all_r


all_s$sentiment <- NA


for (i in 1:nrow(all_s)) {
  #for (i in 1:10) {
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
  
  
  #Sys.sleep(10)
  result <- tryCatch({
    withTimeout({
      temp <- query(prmpt, 
                    model = "llama3.1", 
                    model_params = list(
                      seed = 27803, 
                      temperature = 0,
                      num_ctx = 20000)
                    ,
                    format = "json")
    }, timeout = 100, onTimeout = "silent") 
  }, TimeoutException = function(ex) {
    message("Iteration ", i, " took too long, skipping...")
    return(NA)  # Or return some other placeholder
  }, error = function(e) {
    message("An error occurred: ", e$message)
    return(NA)  # Return NA in case of an error
  })
  
  if (is.list(temp)) {
    all_s$sentiment[i] <- temp$message$content
    
    
  } else {
    all_s$sentiment[i] <- NA
  }
  temp <- NA
}

all_sentiment <- all_s

save(all_sentiment, file = "data/all_sentiment.RData")