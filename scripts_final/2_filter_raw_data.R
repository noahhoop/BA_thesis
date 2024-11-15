library(tidyverse)
library(rollama)
library(arrow)
library(furrr)
library(progressr)
library(purrr)
library(rvest)
library(xml2)
library(dplyr)
library(tidyr)
library(stringr)


rm(list=ls())

bild <- read_csv("data/bild_full.csv")

faz <- read_csv("data/faz.csv")
faz2 <- read_csv("data/faz.csv")

spiegel <- read_csv("data/data/spiegel.csv")
spiegel2 <- read_csv("data/data/spiegel2.csv")

sz <- read_csv("data/sz.csv")
sz2 <- read_csv("data/sz2.csv")

taz <- read_csv("data/data/taz.csv")
taz2 <- read_csv("data/data/taz2.csv")

welt <- read_csv("data/data/welt_full.csv")

zeit <- read_csv("data/data/zeit.csv")
zeit2 <- read_csv("data/zeit2.csv")

save(bild, file = "data/bild.RData")

save(faz, file = "data/faz.RData")
save(faz2, file = "data/faz2.RData")

save(spiegel, file = "data/spiegel.RData")
save(spiegel2, file = "data/spiegel2.RData")

save(sz, file = "data/sz.RData")
save(sz2, file = "data/sz2.RData")

save(zeit, file = "data/zeit.RData")
save(zeit2, file = "data/zeit2.RData")

save(welt, file = "data/welt.RData")

save(taz, file = "data/taz.RData")
save(taz2, file = "data/taz2.RData")


load("data/bild.RData")
load("data/sz.RData")
load("data/sz2.RData")
load("data/zeit.RData")
load("data/zeit2.RData")
load("data/spiegel.RData")
load("data/spiegel2.RData")
load("data/welt.RData")
load("data/taz.RData")
load("data/taz2.RData")
load("data/faz.RData")
load("data/faz2.RData")


all <- c("spiegel", "spiegel2","sz", "sz2", "taz", "taz2", "welt", "zeit", "zeit2","bild", "faz", "faz2")

# spiegel <- spiegel %>% 
#   filter(str_detect(url, "https://www.spiegel.de/wirtschaft") == TRUE | str_detect(url, "https://www.spiegel.de/politik") == TRUE,
#          !is.na(body)) %>% 
#   filter(body != "")
# sz <- sz %>% 
#   filter(str_detect(url, "https://www.sueddeutsche.de/politik") == TRUE | str_detect(url, "https://www.sueddeutsche.de/wirtschaft") == TRUE,
#          !is.na(body)) %>% 
#   filter(body != "")
# 
# welt <- welt %>% 
#   filter(str_detect(url, "https://www.welt.de/politik") == TRUE | str_detect(url, "https://www.welt.de/wirtschaft") == TRUE,
#          !is.na(body)) %>% 
#   filter(body != "")
# 
# zeit <- zeit %>% 
#   filter(str_detect(url, "https://www.zeit.de/politik") == TRUE | str_detect(url, "https://www.zeit.de/wirtschaft") == TRUE | str_detect(url, "https://www.zeit.de/news") == TRUE,
#       !is.na(body)) %>% 
#   filter(body != "")
#   
# taz <- taz %>% 
#   filter(!is.na(body)) %>% 
#   filter(body != "")

all_df <- mget(all)

all_df <- lapply(all_df, function(df) {
  df %>%
    mutate(
      wirtschaft = str_detect(tolower(str_remove_all(body, "-")), "inflation|bip|arbeitslos|bruttoinlandsprodukt") |
        str_detect(tolower(str_remove_all(description, "-")), "inflation|bip|arbeitslos|bruttoinlandsprodukt")
    )
})



# save(all_df, file = "all_df.RData")


all_df <- lapply(all_df, function(df) {
  df %>%
    filter(!is.na(body) & body != "") %>%
    mutate(
      total_articles = n()
    ) %>%
    filter(wirtschaft == TRUE)
})

all_filtered <- all_df

save(all_filtered, file = "data/all_filtered.RData")

