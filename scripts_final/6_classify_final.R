# library(tidyverse)
library(rollama)
library(readr)
library(magrittr)
library(dplyr)
library(stringr)
library(jsonlite)
library(R.utils)
library(purrr)
rm(list=ls())

#pull_model("llama3.1")

# [[]]
# { } 
# \ 

# alt gr + 7-0 Klammern
# alt gr + < ->| 



set.seed(59128)
all_s <- read_csv("data/full_2.csv")
#all_s <- read_delim("data/sample_post.csv", delim = ";")


# in german
all_s$german3 <- NA


for (i in 1:nrow(all_s)) {
  #for (i in 1:10) {
  print(i)
  
  prmpt <- paste0('Sie sind Assistent in einem wissenschaftlichen Projekt und unterstützen dabei relevante Zeitungsartikel zu erkennen.
                  
                  Ich suche nach Artikeln, die ganz explizit darum gehen, dass über Änderungen der Inflation, Arbeitslosenzahlen oder des Wirtschaftswachstums/Bruttoinlandsprodukt (BIP) in Deutschland berichtet wird.
                  Es sind nur Artikel relevant, die um Deutschland als Ganzes gehen. Artikel, die um Europa oder die EU/EZB oder nur einzelne Regionen oder Bundesländer Deutschlands gehen, sollten ausgeschlossen werden.
                  Es sind nur Artikel relevant, wo es um die Veränderung dieser Statistiken geht. Sollten sie in einem anderen Kontext genannt werden, sind die Artikel irrelevant.
                  Es geht immer nur um den bundesweiten Kontext Deutschlands. Einzelne Industrien oder Unternehmen sind auch nicht relevant.
                  Sollte die bundesweite Statistik und einzelner Bundesländer oder der EU/EZB gleichzeitig genannt werden, ist der Artikel relevant. Wenn nur über ein Bundesland oder die EU/EZB berichtet wird, dann ist der Artikel nicht relevant.
                  
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
  
  
  #Sys.sleep(10)
  result <- tryCatch({
    withTimeout({
      temp <- query(prmpt, 
                    model = "llama3.1", 
                    model_params = list(
                      seed = 439578, 
                      temperature = 0,
                      num_ctx = 80000)
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
    all_s$german3[i] <- temp$message$content
    
    
  } else {
    all_s$german3[i] <- NA
  }
  temp <- NA
}



# ge2 <- purrr::map(all_s$german3, jsonlite::fromJSON)
# ge2 <- bind_rows(ge2)
# 
# all_s <- all_s %>% 
#   mutate(german_economy2 = ge2$german_economy)


save(all_s, file = "data/final_results2.RData")

# 
#write_csv(all_s, "data/final_results2.csv")


