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


load("data/all_filtered.RData")




all_filtered$taz2$date <- ymd_hms(all_filtered$taz2$date, tz = "UTC")
all_filtered$taz$paywall <- NA

all_filtered$welt2$date <- ymd_hms(all_filtered$welt2$date, tz = "UTC")
all_filtered <- lapply(all_filtered, function(df) {
  df %>%
    mutate(paywall = as.character(paywall))
})


all_filtered_df <- all_filtered |> 
  bind_rows() |>
  tibble()

all_filtered_df <- all_filtered_df %>%
  filter(str_detect(tolower(str_remove_all(body, "-")), "inflation|arbeitslos|bruttoinlandsprodukt") == TRUE | 
           str_detect(body, "BIP") == TRUE)

all_filtered_df <- all_filtered_df %>% 
  mutate(date = as.Date(date)) %>% 
  filter(date > "2018-03-13") # only after start of Merkel cabinet


# all_filtered_df %>% 
#   ggplot() +
#   geom_bar(aes(x = date))

all_filtered_df <- all_filtered_df %>% 
  mutate(newspaper = str_extract(url, "(?<=https://www\\.)[^\\.]+")) %>% 
  mutate(newspaper = replace_na(newspaper, "taz")) # only taz doesn't have www.

table(all_filtered_df$newspaper, exclude = NULL)

all2 <- all_filtered_df %>% 
  filter(newspaper %in% c("welt", "zeit")) %>% 
  select(-1, -2)

# removing parts of the text that doesn't belong to the article
all2 <- all2 %>% 
  mutate(
    body = str_remove_all(body, "Hauptnavigation: Nutzen Sie die Tabulatortaste, um durch die Menüpunkte zu navigieren. Öffnen Sie Untermenüs mit der Leertaste. Schließen Sie Untermenüs mit der Escape-Taste. Hauptnavigation: Nutzen Sie die Tabulatortaste, um durch die Menüpunkte zu navigieren. Öffnen Sie Untermenüs mit der Leertaste.")
    ) %>% 
  mutate(
    body = str_remove_all(body, "Worum geht es Klicken Sie auf die Bullets, um zum entsprechenden Abschnitt im Text zu gelangen")
  )

save(all2, file = "data/final/full_2.RData")

# write_csv(all2, "data/final/full_2.csv")

# load("data/final/full_2.RData")
set.seed(48913)
s <- sample(1:nrow(all2), 600)
save(s, file = "data/final/sample600.RData")

all2[s, ] %>% 
  write_csv("data/final/sample_pre600.csv")

# final <- read_csv("data/final/sample_results2.csv")

