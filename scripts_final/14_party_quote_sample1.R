library(tidyverse)
library(readr)
library(caret)
library(xtable)
library(knitr)
library(corrr)
library(deeplr)
library(httr)
library(psych)
library(modelsummary)
library(sjPlot)
library(irr)
library(ggeffects)
library(marginaleffects)
options(scipen = 999)


welt_hex <- "#003a5a"
zeit_hex <- "#b91109"
colors <- c("welt" = welt_hex, "zeit" = zeit_hex)

CDU_hex <- "#000000"
SPD_hex <- "#e3000f"
colors2 <- c("Merkel" = CDU_hex,
             "Scholz" = SPD_hex)

party_sample <- data.frame()


docs <- list.files(path = "data/final/articles_coded_party/", full.names = TRUE)


numeric_values <- as.numeric(str_extract(docs, "(?<=doc_)\\d+"))

# Get the order of numeric values
new_order <- order(numeric_values)

# Sort files based on the Windows order
docs <- docs[new_order]



for (i in 1:length(docs)) {
  
  temp <- read_delim(docs[i], delim = ";")
  
  
  
  temp_df <- data.frame(doc = as.numeric(str_extract(docs[i], "(?<=doc_)\\d+")),
                        #body = paste0(temp$sentence, collapse = " "),
                        CDUCSU = sum(temp$CDUCSU == "1", na.rm = TRUE),
                        SPD = sum(temp$SPD == "1", na.rm = TRUE),
                        Gruene = sum(temp$Gruene == "1", na.rm = TRUE),
                        FDP = sum(temp$FDP == "1", na.rm = TRUE),
                        AFD = sum(temp$AFD == "1", na.rm = TRUE),
                        Linke = sum(temp$Linke == "1", na.rm = TRUE))
  
  party_sample <- bind_rows(party_sample, temp_df)
}

load("data/final/relevant_sample250.RData")
relevant_sample <- relevant_sample[1:200, ]
relevant_sample$doc <- 1:200

party_sample <- relevant_sample %>% 
  left_join(party_sample,
            by = "doc") %>% 
  mutate(across(11:16,
                ~ replace_na(., 0)))

save(party_sample, file = "data/final/party_sample.RData")
