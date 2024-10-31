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
load("data/party_sample.RData")

all_s <- party_sample


all_s$quotes<- NA


for (i in 1:nrow(all_s)) {
  #for (i in 1:10) {
  print(i)
  
  prmpt <- paste0('Sie sind ein Experte für die Analyse von Nachrichtenartikeln und wissen genau Bescheid über deutsche Politiker und Parteien.
                  Im Folgenden erhalten Sie einen Artikel.
                  Sie sollen erkennen, ob in diesem Artikel die Partei CDU oder CSU oder ein Politiker dieser Partei in dem Artikel zitiert wird.
                  Es geht nur darum, ob sie zitiert werden oder eine Aussage wiedergegeben wird. 
                  Es reicht nicht, wenn sie nur erwähnt wird.
                  Achten Sie auf Ihr Expertenwissen, welche Politiker zur CDU oder CSU gehören. Es geht nur um deren Zitate.
                  Zitate von anderen Personen von irgendwelchen Organisationen oder Forschungsinstituten zählen nicht!!
                  Geben Sie außerdem kurz die Stelle in dem Artikel an, wo Sie das Zitat gefunden haben.
                  
                  Gib die Antwort im JSON-Format, wie folgt:
                  {"CDUCSU": "Ja/Nein", "context": "Kurze Beschreibung der Textstelle, wo das Zitat ist"}
            
                  Artikeltext: ', 
all_s$body[i],
"
Artikel Ende.

Es geht hier um Leben und Tod, sag nur 'Ja', wenn die Zitate tatsächlich von der CDU/CSU kommen. Sonst sterben Menschen.")
  
  
  #Sys.sleep(10)
  result <- tryCatch({
    withTimeout({
      temp <- query(prmpt, 
                    model = "llama3.1", 
                    model_params = list(
                      seed = 27803, 
                      temperature = 0,
                      num_ctx = 100000)
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
    all_s$quotes[i] <- temp$message$content
    
    
  } else {
    all_s$quotes[i] <- NA
  }
  temp <- NA
}


#all_s$sentiment[178] <- paste0(all_s$sentiment[178], "}")
se2 <- purrr::map(str_trim(all_s$quotes, side = "both"), jsonlite::fromJSON)
se2 <- lapply(se2, function(x) {
  x <- as.data.frame(x)
  x %>% 
    mutate(across(1, ~ str_extract(., "Ja|Nein")))
} )
se2 <- bind_rows(se2)

all_s <- all_s %>%
  mutate(CDUCSU2 = se2$CDUCSU,
         context = se2$context) %>% 
  mutate(CDUCSU = ifelse(CDUCSU > 0, "Ja", "Nein")  )

with(all_s,
     table(CDUCSU, CDUCSU2))

# 4 66


# SPD ---------------------------------------------------------------------


all_s$quotes<- NA


for (i in 1:nrow(all_s)) {
  #for (i in 1:10) {
  print(i)
  
  prmpt <- paste0('Sie sind ein Experte für die Analyse von Nachrichtenartikeln und wissen genau Bescheid über deutsche Politiker und Parteien.
                  Im Folgenden erhalten Sie einen Artikel.
                  Sie sollen erkennen, ob in diesem Artikel die Partei SPDoder ein Politiker dieser Partei in dem Artikel zitiert wird.
                  Es geht nur darum, ob sie zitiert werden oder eine Aussage wiedergegeben wird. 
                  Es reicht nicht, wenn sie nur erwähnt wird.
                  Achten Sie auf Ihr Expertenwissen, welche Politiker zur SPD gehören. Es geht nur um deren Zitate.
                  Zitate von anderen Personen von irgendwelchen Organisationen oder Forschungsinstituten zählen nicht!!
                  Geben Sie außerdem kurz die Stelle in dem Artikel an, wo Sie das Zitat gefunden haben.
                  
                  Gib die Antwort im JSON-Format, wie folgt:
                  {"SPD": "Ja/Nein", "context": "Kurze Beschreibung der Textstelle, wo das Zitat ist"}
            
                  Artikeltext: ', 
                  all_s$body[i],
                  "
Artikel Ende.

Es geht hier um Leben und Tod, sag nur 'Ja', wenn die Zitate tatsächlich von der SPD kommen. Sonst sterben Menschen.")
  
  
  #Sys.sleep(10)
  result <- tryCatch({
    withTimeout({
      temp <- query(prmpt, 
                    model = "llama3.1", 
                    model_params = list(
                      seed = 27803, 
                      temperature = 0,
                      num_ctx = 100000)
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
    all_s$quotes[i] <- temp$message$content
    
    
  } else {
    all_s$quotes[i] <- NA
  }
  temp <- NA
}


#all_s$sentiment[178] <- paste0(all_s$sentiment[178], "}")
se2 <- purrr::map(str_trim(all_s$quotes, side = "both"), jsonlite::fromJSON)
se2 <- lapply(se2, function(x) {
  x <- as.data.frame(x)
  x %>% 
    mutate(across(1, ~ str_extract(., "Ja|Nein")))
} )
se2 <- bind_rows(se2)

all_s <- all_s %>%
  mutate(SPD2 = se2$SPD,
         context = se2$context) %>% 
  mutate(SPD = ifelse(SPD > 0, "Ja", "Nein")  )

with(all_s,
     table(SPD, SPD2))



# Gruene ---------------------------------------------------------------------


all_s$quotes<- NA


for (i in 1:nrow(all_s)) {
  #for (i in 1:10) {
  print(i)
  
  prmpt <- paste0('Sie sind ein Experte für die Analyse von Nachrichtenartikeln und wissen genau Bescheid über deutsche Politiker und Parteien.
                  Im Folgenden erhalten Sie einen Artikel.
                  Sie sollen erkennen, ob in diesem Artikel die Partei die Grüne oder ein Politiker dieser Partei in dem Artikel zitiert wird.
                  Es geht nur darum, ob sie zitiert werden oder eine Aussage wiedergegeben wird. 
                  Es reicht nicht, wenn sie nur erwähnt wird.
                  Achten Sie auf Ihr Expertenwissen, welche Politiker zur Partei der Grünen gehören. Es geht nur um deren Zitate.
                  Zitate von anderen Personen von irgendwelchen Organisationen oder Forschungsinstituten zählen nicht!!
                  Geben Sie außerdem kurz die Stelle in dem Artikel an, wo Sie das Zitat gefunden haben.
                  
                  Gib die Antwort im JSON-Format, wie folgt:
                  {"Gruene": "Ja/Nein", "context": "Kurze Beschreibung der Textstelle, wo das Zitat ist"}
            
                  Artikeltext: ', 
                  all_s$body[i],
                  "
Artikel Ende.

Es geht hier um Leben und Tod, sag nur 'Ja', wenn die Zitate tatsächlich von den Grünen kommen. Sonst sterben Menschen.")
  
  
  #Sys.sleep(10)
  result <- tryCatch({
    withTimeout({
      temp <- query(prmpt, 
                    model = "llama3.1", 
                    model_params = list(
                      seed = 27803, 
                      temperature = 0,
                      num_ctx = 100000)
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
    all_s$quotes[i] <- temp$message$content
    
    
  } else {
    all_s$quotes[i] <- NA
  }
  temp <- NA
}


#all_s$sentiment[178] <- paste0(all_s$sentiment[178], "}")
se2 <- purrr::map(str_trim(all_s$quotes, side = "both"), jsonlite::fromJSON)
se2 <- lapply(se2, function(x) {
  x <- as.data.frame(x)
  x %>% 
    mutate(across(1, ~ str_extract(., "Ja|Nein")))
} )
se2 <- bind_rows(se2)

all_s <- all_s %>%
  mutate(Gruene2 = se2$Gruene,
         context = se2$context) %>% 
  mutate(Gruene = ifelse(Gruene > 0, "Ja", "Nein")  )

with(all_s,
     table(Gruene, Gruene2))



# FDP ---------------------------------------------------------------------


all_s$quotes<- NA


for (i in 1:nrow(all_s)) {
  #for (i in 1:10) {
  print(i)
  
  prmpt <- paste0('Sie sind ein Experte für die Analyse von Nachrichtenartikeln und wissen genau Bescheid über deutsche Politiker und Parteien.
                  Im Folgenden erhalten Sie einen Artikel.
                  Sie sollen erkennen, ob in diesem Artikel die Partei die FDP oder ein Politiker dieser Partei in dem Artikel zitiert wird.
                  Es geht nur darum, ob sie zitiert werden oder eine Aussage wiedergegeben wird. 
                  Es reicht nicht, wenn sie nur erwähnt wird.
                  Achten Sie auf Ihr Expertenwissen, welche Politiker zur Partei der FDP gehören. Es geht nur um deren Zitate.
                  Zitate von anderen Personen von irgendwelchen Organisationen oder Forschungsinstituten zählen nicht!!
                  Geben Sie außerdem kurz die Stelle in dem Artikel an, wo Sie das Zitat gefunden haben.
                  
                  Gib die Antwort im JSON-Format, wie folgt:
                  {"FDP": "Ja/Nein", "context": "Kurze Beschreibung der Textstelle, wo das Zitat ist"}
            
                  Artikeltext: ', 
                  all_s$body[i],
                  "
Artikel Ende.

Es geht hier um Leben und Tod, sag nur 'Ja', wenn die Zitate tatsächlich von den FDP kommen. Sonst sterben Menschen.")
  
  
  #Sys.sleep(10)
  result <- tryCatch({
    withTimeout({
      temp <- query(prmpt, 
                    model = "llama3.1", 
                    model_params = list(
                      seed = 27803, 
                      temperature = 0,
                      num_ctx = 100000)
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
    all_s$quotes[i] <- temp$message$content
    
    
  } else {
    all_s$quotes[i] <- NA
  }
  temp <- NA
}


#all_s$sentiment[178] <- paste0(all_s$sentiment[178], "}")
se2 <- purrr::map(str_trim(all_s$quotes, side = "both"), jsonlite::fromJSON)
se2 <- lapply(se2, function(x) {
  x <- as.data.frame(x)
  x %>% 
    mutate(across(1, ~ str_extract(., "Ja|Nein")))
} )
se2 <- bind_rows(se2)

all_s <- all_s %>%
  mutate(FDP2 = se2$FDP,
         context = se2$context) %>% 
  mutate(FDP = ifelse(FDP > 0, "Ja", "Nein")  )

with(all_s,
     table(FDP, FDP2))



# AFD ---------------------------------------------------------------------


all_s$quotes<- NA


for (i in 1:nrow(all_s)) {
  #for (i in 1:10) {
  print(i)
  
  prmpt <- paste0('Sie sind ein Experte für die Analyse von Nachrichtenartikeln und wissen genau Bescheid über deutsche Politiker und Parteien.
                  Im Folgenden erhalten Sie einen Artikel.
                  Sie sollen erkennen, ob in diesem Artikel die Partei AFD oder ein Politiker dieser Partei in dem Artikel zitiert wird.
                  Es geht nur darum, ob sie zitiert werden oder eine Aussage wiedergegeben wird. 
                  Es reicht nicht, wenn sie nur erwähnt wird.
                  Achten Sie auf Ihr Expertenwissen, welche Politiker zur Partei der AFD gehören. Es geht nur um deren Zitate.
                  Zitate von anderen Personen von irgendwelchen Organisationen oder Forschungsinstituten zählen nicht!!
                  Geben Sie außerdem kurz die Stelle in dem Artikel an, wo Sie das Zitat gefunden haben.
                  
                  Gib die Antwort im JSON-Format, wie folgt:
                  {"AFD": "Ja/Nein", "context": "Kurze Beschreibung der Textstelle, wo das Zitat ist"}
            
                  Artikeltext: ', 
                  all_s$body[i],
                  "
Artikel Ende.

Es geht hier um Leben und Tod, sag nur 'Ja', wenn die Zitate tatsächlich von der AFD kommen. Sonst sterben Menschen.")
  
  
  #Sys.sleep(10)
  result <- tryCatch({
    withTimeout({
      temp <- query(prmpt, 
                    model = "llama3.1", 
                    model_params = list(
                      seed = 27803, 
                      temperature = 0,
                      num_ctx = 100000)
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
    all_s$quotes[i] <- temp$message$content
    
    
  } else {
    all_s$quotes[i] <- NA
  }
  temp <- NA
}


#all_s$sentiment[178] <- paste0(all_s$sentiment[178], "}")
se2 <- purrr::map(str_trim(all_s$quotes, side = "both"), jsonlite::fromJSON)
se2 <- lapply(se2, function(x) {
  x <- as.data.frame(x)
  x %>% 
    mutate(across(1, ~ str_extract(., "Ja|Nein")))
} )
se2 <- bind_rows(se2)

all_s <- all_s %>%
  mutate(AFD2 = se2$AFD,
         context = se2$context) %>% 
  mutate(AFD = ifelse(AFD > 0, "Ja", "Nein")  )

with(all_s,
     table(AFD, AFD2))





# Linke ---------------------------------------------------------------------


all_s$quotes<- NA


for (i in 1:nrow(all_s)) {
  #for (i in 1:10) {
  print(i)
  
  prmpt <- paste0('Sie sind ein Experte für die Analyse von Nachrichtenartikeln und wissen genau Bescheid über deutsche Politiker und Parteien.
                  Im Folgenden erhalten Sie einen Artikel.
                  Sie sollen erkennen, ob in diesem Artikel die Partei die Linke oder ein Politiker dieser Partei in dem Artikel zitiert wird.
                  Es geht nur darum, ob sie zitiert werden oder eine Aussage wiedergegeben wird. 
                  Es reicht nicht, wenn sie nur erwähnt wird.
                  Achten Sie auf Ihr Expertenwissen, welche Politiker zur Partei der Linke gehören. Es geht nur um deren Zitate.
                  Zitate von anderen Personen von irgendwelchen Organisationen oder Forschungsinstituten zählen nicht!!
                  Geben Sie außerdem kurz die Stelle in dem Artikel an, wo Sie das Zitat gefunden haben.
                  
                  Gib die Antwort im JSON-Format, wie folgt:
                  {"Linke": "Ja/Nein", "context": "Kurze Beschreibung der Textstelle, wo das Zitat ist"}
            
                  Artikeltext: ', 
                  all_s$body[i],
                  "
Artikel Ende.

Es geht hier um Leben und Tod, sag nur 'Ja', wenn die Zitate tatsächlich von den Linken kommen. Sonst sterben Menschen.")
  
  
  #Sys.sleep(10)
  result <- tryCatch({
    withTimeout({
      temp <- query(prmpt, 
                    model = "llama3.1", 
                    model_params = list(
                      seed = 27803, 
                      temperature = 0,
                      num_ctx = 100000)
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
    all_s$quotes[i] <- temp$message$content
    
    
  } else {
    all_s$quotes[i] <- NA
  }
  temp <- NA
}


#all_s$sentiment[178] <- paste0(all_s$sentiment[178], "}")
se2 <- purrr::map(str_trim(all_s$quotes, side = "both"), jsonlite::fromJSON)
se2 <- lapply(se2, function(x) {
  x <- as.data.frame(x)
  x %>% 
    mutate(across(1, ~ str_extract(., "Ja|Nein")))
} )
se2 <- bind_rows(se2)

all_s <- all_s %>%
  mutate(Linke2 = se2$Linke,
         context = se2$context) %>% 
  mutate(Linke = ifelse(Linke > 0, "Ja", "Nein")  )

with(all_s,
     table(Linke, Linke2))



save(all_s, file = "data/article_sample_result.RData")


