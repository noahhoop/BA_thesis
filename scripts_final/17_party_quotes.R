###
# script: final_data.R
# description: Validation of party quote detection and analysis of party and institute quotes
# author: Noah Badenhoop
###

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


load("data/final/article_sample_result.RData")

cm_CDU <- confusionMatrix(factor(all_s$CDUCSU2), factor(all_s$CDUCSU), mode = "everything")
cm_SPD <- confusionMatrix(factor(all_s$SPD2), factor(all_s$SPD), mode = "everything")
cm_Grüne <- confusionMatrix(factor(all_s$Gruene2), factor(all_s$Gruene), mode = "everything")
cm_FDP <- confusionMatrix(factor(all_s$FDP2), factor(all_s$FDP), mode = "everything")
cm_AfD <- confusionMatrix(factor(all_s$AFD2), factor(all_s$AFD), mode = "everything")
cm_Linke <- confusionMatrix(factor(all_s$Linke2), factor(all_s$Linke), mode = "everything")

cms <- ls(pattern = "^cm_")

all_cm <- data.frame(Party = character(),
                     Precision = numeric(),
                     Recall = numeric(),
                     F1 = numeric())

for (c in cms) {
  tcm <- get(c)
  ts <- tcm$byClass %>% 
    as.data.frame() %>% 
    t() %>% 
    as.data.frame() %>% 
    mutate(Party = substr(c, 4, nchar(c))) %>% 
    select(Party, Precision, Recall, F1)
  
  all_cm <- bind_rows(all_cm, ts)
}

xtable(all_cm) %>% print(type = "latex", file = "results/tables/party_quotes_validation.tex", include.rownames = FALSE)



all_s %>% 
  select(starts_with("CDU")) %>% 
  as.matrix() %>% 
   t() %>% 
   kripp.alpha("nominal")

all_s %>% 
  select(starts_with("SPD")) %>% 
  as.matrix() %>% 
  t() %>% 
  kripp.alpha("nominal")

all_s %>% 
  select(starts_with("Gruene")) %>% 
  as.matrix() %>% 
  t() %>% 
  kripp.alpha("nominal")

all_s %>% 
  select(starts_with("FDP")) %>% 
  as.matrix() %>% 
  t() %>% 
  kripp.alpha("nominal")

all_s %>% 
  select(starts_with("AFD")) %>% 
  as.matrix() %>% 
  t() %>% 
  kripp.alpha("nominal")

all_s %>% 
  select(starts_with("Linke")) %>% 
  as.matrix() %>% 
  t() %>% 
  kripp.alpha("nominal")



load("data/final/party_quotes_results.RData")

se6 <- purrr::map(str_trim(party_quotes_results$quotes6, side = "both"), jsonlite::fromJSON)
se6 <- lapply(se6, function(x) {
  x <- as.data.frame(x)
  x %>% 
    mutate(across(1, ~ str_extract(., "Ja|Nein")))
} )
se6 <- bind_rows(se6)

party_quotes_results <- party_quotes_results %>%
  mutate(Linke = se6$Linke)


party_quotes_results$CDUCSU <- str_match(party_quotes_results$quotes, "(?<=:).*,") %>% str_extract("Ja|Nein")
party_quotes_results$SPD <- str_match(party_quotes_results$quotes2, "(?<=:).*,") %>% str_extract("Ja|Nein")
party_quotes_results$Gruene <- str_match(party_quotes_results$quotes3, "(?<=:).*,") %>% str_extract("Ja|Nein")
party_quotes_results$FDP <- str_match(party_quotes_results$quotes4, "(?<=:).*,") %>% str_extract("Ja|Nein")
party_quotes_results$AFD <- str_match(party_quotes_results$quotes5, "(?<=:).*,") %>% str_extract("Ja|Nein")


load("data/final/all_sentiment_f.RData")

final <- party_quotes_results %>% 
  select(url, Linke, CDUCSU, SPD, FDP, Gruene, AFD) %>% 
  right_join(all_sentiment_f,
             by = "url") %>% 
  mutate(across(c(CDUCSU, SPD, Gruene, FDP, AFD, Linke),
                ~ fct_relevel(., "Nein")))


table(final$Linke)
table(final$CDUCSU)
table(final$SPD)
table(final$FDP)
table(final$Gruene)
table(final$AFD)


with(final,
     table(newspaper, CDUCSU, government)) %>% 
  prop.table(margin = c(1,3)) %>% 
  round(3)*100

with(final,
     table(newspaper, SPD, government)) %>% 
  prop.table(margin = c(1,3)) %>% 
  round(3)*100

with(final,
     table(newspaper, Gruene, government)) %>% 
  prop.table(margin = c(1,3)) %>% 
  round(3)*100

with(final,
     table(newspaper, FDP, government)) %>% 
  prop.table(margin = c(1,3)) %>% 
  round(3)*100

with(final,
     table(newspaper, AFD, government)) %>% 
  prop.table(margin = c(1,3)) %>% 
  round(3)*100

with(final,
     table(newspaper, Linke, government)) %>% 
  prop.table(margin = c(1,3)) %>% 
  round(3)*100

final <- final %>% 
  mutate(quote_any = case_when(
    CDUCSU == "Ja" | SPD == "Ja" | Gruene == "Ja" |
      FDP == "Ja" | AFD == "Ja" | Linke == "Ja" ~ 1,
    .default = 0
  ))



table(final$newspaper ,final$quote_any, final$government) %>% 
  prop.table(margin = c(1, 3)) %>% round(3)*100





# Party models ------------------------------------------------------------
final$ifo <- str_detect(tolower(final$body), "ifo")
final$diw <- str_detect(tolower(final$body), "diw")
final$ifo <- factor(final$ifo, labels = c("Nein", "Ja"))
final$diw <- factor(final$diw, labels = c("Nein", "Ja"))
welt <- final %>% 
  filter(newspaper == "welt")

zeit <- final %>% 
  filter(newspaper == "zeit")



welt %>% 
  with(table(sentiment_c, CDUCSU, government)) %>% 
  prop.table(margin = c(2, 3)) %>% round(3)*100

zeit %>% 
  with(table(sentiment_c, CDUCSU, government)) %>% 
  prop.table(margin = c(2, 3)) %>% round(3)*100


party_models <- list()

### CDUCSU ###
party_models[["CDUCSU"]] <- glm(CDUCSU ~ sentiment_n*government*newspaper, data = final, 
          family = binomial("logit"))

summary(party_models[["CDUCSU"]])

plot_model(party_models[["CDUCSU"]], type = "int",
           colors = colors2,
           legend.title = "Government",
           axis.title = c("Sentiment Score",
                          "Probability of quoting CDU/CSU"),
           title = "")

ggsave("results/graphs/quotes_final_CDUCSU.pdf", width = 8, height = 5)

# SPD
party_models[["SPD"]] <- glm(SPD ~ sentiment_n*government*newspaper, data = final, 
                               family = binomial("logit"))

summary(party_models[["SPD"]])

plot_model(party_models[["SPD"]], type = "int",
           colors = colors2,
           legend.title = "Government",
           axis.title = c("Sentiment Score",
                          "Probability of quoting SPD"),
           title = "")

ggsave("results/graphs/quotes_final_SPD.pdf", width = 8, height = 5)

# Grüne

party_models[["Gruene"]] <- glm(Gruene ~ sentiment_n*government*newspaper, data = final, 
                               family = binomial("logit"))

summary(party_models[["Gruene"]])

plot_model(party_models[["Gruene"]], type = "int",
           colors = colors2,
           legend.title = "Government",
           axis.title = c("Sentiment Score",
                          "Probability of quoting Greens"),
           title = "")

ggsave("results/graphs/quotes_final_Gruene.pdf", width = 8, height = 5)



# FDP
party_models[["FDP"]] <- glm(FDP ~ sentiment_n*government*newspaper, data = final, 
                            family = binomial("logit"))

summary(party_models[["FDP"]])

plot_model(party_models[["FDP"]], type = "int",
           colors = colors2,
           legend.title = "Government",
           axis.title = c("Sentiment Score",
                          "Probability of quoting FDP"),
           title = "")

ggsave("results/graphs/quotes_final_FDP.pdf", width = 8, height = 5)

# AFD
party_models[["AFD"]] <- glm(AFD ~ sentiment_n*government*newspaper, data = final, 
                            family = binomial("logit"))

summary(party_models[["AFD"]])

plot_model(party_models[["AFD"]], type = "int",
           colors = colors2,
           legend.title = "Government",
           axis.title = c("Sentiment Score",
                          "Probability of quoting AFD"),
           title = "")

ggsave("results/graphs/quotes_final_AFD.pdf", width = 8, height = 5)


# Linke
party_models[["Linke"]] <- glm(Linke ~ sentiment_n*government*newspaper, data = final, 
                            family = binomial("logit"))

summary(party_models[["Linke"]])

plot_model(party_models[["Linke"]], type = "int",
           colors = colors2,
           legend.title = "Government",
           axis.title = c("Sentiment Score",
                          "Probability of quoting Linke"),
           title = "")

ggsave("results/graphs/quotes_final_Linke.pdf", width = 8, height = 5)


# Quote government or opposition ------------------------------------------


final <- final %>% 
  mutate(
    q_opposition = case_when(
      government == "Merkel" & Gruene == "Ja" ~ 1,
      government == "Merkel" & FDP == "Ja" ~ 1,
      government == "Merkel" & AFD == "Ja" ~ 1,
      government == "Merkel" & Linke == "Ja" ~ 1,
      government == "Scholz" & CDUCSU == "Ja" ~ 1,
      government == "Scholz" & AFD == "Ja" ~ 1,
      government == "Scholz" & Linke == "Ja" ~ 1,
      .default = 0
    ),
    q_government = case_when(
      government == "Merkel" & SPD == "Ja" ~ 1,
      government == "Merkel" & CDUCSU == "Ja" ~ 1,
      government == "Scholz" & SPD == "Ja" ~ 1,
      government == "Scholz" & Gruene == "Ja" ~ 1,
      government == "Scholz" & FDP == "Ja" ~ 1,
      .default = 0
    )
  )

table(final$q_government)
table(final$q_opposition)


models_quotes <- list()
models_quotes[["Opposition"]] <- glm(q_opposition ~ newspaper*government,
          data = final,
          family = binomial("logit"))

summary(models_quotes[["Opposition"]])


plot_model(models_quotes[["Opposition"]], type = "int",
           colors = colors2,
           legend.title = "Government",
           axis.title = c("",
                          "Probability of quoting the government"),
           title = "")


ame_o <- avg_slopes(models_quotes[["Opposition"]],
                   newdata = "marginalmeans",
                   variables = "government",
                   by = "newspaper",
                   type = "response"
)




predict_response(models_quotes[["Opposition"]],
                 terms = c("government", "newspaper"),
                 #vcov_fun = cluster,
                 margin = "marginaleffect"
)


ame_o %>%
  ggplot(aes(x = newspaper,
             y = estimate,
             ymin = conf.low,
             ymax = conf.high,
             color = newspaper)) +
  geom_point() +
  geom_pointrange() +
  labs(x = "",
       y = "AMEs (Scholz cabinet with ref. Merkel cabinet)") +
  geom_hline(yintercept = 0, color = "red") +
  scale_x_discrete(labels = c("Welt", "Zeit")) +
  scale_color_manual(values = colors) +
  theme_light()

ggsave("results/graphs/quotes_opposition.pdf", width = 7, height = 5)

### government
models_quotes[["Government"]] <- glm(q_government ~ newspaper*government,
                                     data = final,
                                     family = binomial("logit"))

summary(models_quotes[["Government"]])


ame_g <- avg_slopes(models_quotes[["Government"]],
                    newdata = "marginalmeans",
                    variables = "government",
                    by = "newspaper",
                    type = "response"
)




predict_response(models_quotes[["Government"]],
                 terms = c("government", "newspaper"),
                 #vcov_fun = cluster,
                 margin = "marginaleffect"
)


ame_g %>%
  ggplot(aes(x = newspaper,
             y = estimate,
             ymin = conf.low,
             ymax = conf.high,
             color = newspaper)) +
  geom_point() +
  geom_pointrange() +
  labs(x = "",
       y = "AMEs (Scholz cabinet with ref. Merkel cabinet)") +
  geom_hline(yintercept = 0, color = "red") +
  scale_x_discrete(labels = c("Welt", "Zeit")) +
  scale_color_manual(values = colors) +
  theme_light()

ggsave("results/graphs/quotes_government.pdf", width = 7, height = 5)




# Institutes --------------------------------------------------------------


final %>% 
  with(table(ifo, sentiment_c, government, newspaper)) %>% 
  prop.table(margin = c("sentiment_c", "government", "newspaper"))

final %>% 
  with(table(diw, sentiment_c, government, newspaper)) %>% 
  prop.table(margin = c("sentiment_c", "government", "newspaper"))



# ifo
party_models[["ifo"]] <- glm(ifo ~ sentiment_n*government*newspaper, data = final, 
                               family = binomial("logit"))

summary(party_models[["ifo"]])

plot_model(party_models[["ifo"]], type = "int",
           colors = colors2,
           legend.title = "Government",
           axis.title = c("Sentiment Score",
                          "Probability of quoting ifo institute"),
           title = "")

ggsave("results/graphs/quotes_final_ifo.pdf", width = 8, height = 5)

# diw
party_models[["diw"]] <- glm(diw ~ sentiment_n*government*newspaper, data = final, 
                            family = binomial("logit"))

summary(party_models[["diw"]])

plot_model(party_models[["diw"]], type = "int",
           colors = colors2,
           legend.title = "Government",
           axis.title = c("Sentiment Score",
                          "Probability of quoting DIW"),
           title = "")

ggsave("results/graphs/quotes_final_diw.pdf", width = 8, height = 5)

