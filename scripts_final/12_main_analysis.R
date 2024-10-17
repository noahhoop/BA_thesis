###
# script: final_data.R
# description: Data preparation and Analysis for Bachelor thesis
# author: Noah Badenhoop
###
# packages ----------------------------------------------------------------


library(tidyverse)
library(rio)
library(fixest)
library(Hmisc)
library(scales)
library(plm)
library(pglm)
library(labelled)
library(modelsummary)
library(flextable)
library(texreg)
library(stargazer)
library(sandwich)
library(lmtest)
library(xtable)
library(estimatr)
library(marginaleffects)
library(ggeffects)
library(margins)
library(car)
library(corrr)
library(emmeans)
library(miceadds)
library(sjPlot)
library(haven)
library(foreign)
library(boot)
library(nortest)
library(imputeTS)
library(future.apply)
library(knitr)
library(ggfortify)
library(jtools)
library(khb)


options(scipen = 999)
rm(list=ls())





# prepare data ------------------------------------------------------------


load("data/final/all_sentiment.RData")

# pb <- read_dta("data/politbarometer/pb.dta")
# save(pb, file = "data/politbarometer/pb.RData")
load("data/politbarometer/pb.RData")

pbb <- import("data/politbarometer/pbb.xlsx", skip = 7) %>%
  mutate(month = month(date),
         year = year(date)) %>%
  group_by(year, month) %>%
  summarise(approval = mean(Regierung, na.rm = TRUE)) %>%
  filter(year > 2022)

pb2 <- pb %>%
  filter(v4 > 2017) %>%
  filter(!(v4 == 2018 & v3 < 3)) %>%
  select(v3, v4, v15) %>%
  rename(month = v3,
         year = v4,
         approval = v15) %>%
  mutate(approval = ifelse( approval %in% c(0, 99), NA, approval)) %>%
  mutate(approval = approval - 6)

pb3 <- pb2 %>%
  group_by(year, month) %>%
  summarise(approval = mean(approval, na.rm = TRUE))


politbarometer <- bind_rows(pb3, pbb) %>% 
  mutate(date = as.Date(paste(year, month, "1", 
                              sep = "-"), format = "%Y-%m-%d"),
         government = ifelse(date > "2021-12-08", "Scholz", "Merkel"))



mp <- read_dta("data/final/mp_data.dta") %>% 
  filter(countryname == "Germany" & edate > "2017-01-01") %>% 
  select(coderyear, partyname, partyabbrev, absseat, totseats, rile)

mp <- mp %>% 
  mutate(gov = case_when(coderyear == 2017 & partyabbrev %in% c("CDU/CSU", "SPD") ~ 1,
                         coderyear == 2021 & partyabbrev %in% c("SPD", "FDP", "90/Greens") ~ 1,
                         .default = 0))

mp <- mp %>% 
  filter(gov == 1) %>% 
  mutate(share_seat = absseat/totseats,
         rile_weighted = rile*share_seat)

mp %>% 
  group_by(coderyear) %>% 
  summarise(mrile = mean(rile_weighted))

mp %>% 
  group_by(coderyear) %>% 
  summarise(mrile = mean(rile))

# convert json data from AI -----------------------------------------------


#all_sentiment$sentiment[178] <- paste0(all_sentiment$sentiment[178], "}")
se2 <- purrr::map(str_trim(all_sentiment$sentiment, side = "both") %>% 
                    ifelse(substr(., nchar(.), nchar(.)) != "}",
                           paste0(., "}"),
                           # in case it doesn't end on "}"
                           .), 
                  jsonlite::fromJSON)
se2 <- lapply(se2, function(x) {
  x <- as.data.frame(x)
  x %>% 
    mutate(sentiment_score = as.character(sentiment_score))
} )
se2 <- bind_rows(se2)

all_sentiment <- all_sentiment %>%
  mutate(sentiment_c = ifelse(se2$sentiment %in% c("Negativ", "Neutral", "Positiv"), se2$sentiment, NA) %>% factor(ordered = TRUE),
         sentiment_n = as.numeric(se2$sentiment_score),
         justification = se2$justification, 
         relevant = se2$relevant) %>% 
  select(-sentiment)

table(all_sentiment$sentiment_c)
hist(all_sentiment$sentiment_n)
table(all_sentiment$relevant)


# official government formation: 2021-12-08

all_sentiment <- all_sentiment %>% 
  mutate(government = ifelse(date < "2021-12-08", "Merkel", "Scholz"),
         week = week(date),
         month = month(date),
         year = year(date),
         fe_week = paste0(year, week),
         fe_month = paste0(year, month))


# add econ data -----------------------------------------------------------
infl <- import("data/econ/inflation.xlsx", skip = 3) %>% 
  drop_na(Verbraucherpreisindex) %>% 
  fill(Jahr,  .direction = "down") %>% 
  dplyr::rename(year = Jahr,
                month_name = Monat,
                vpi = Verbraucherpreisindex,
                infl_change_year = "Veränderung zum Vorjahresmonat",
                infl_change_month = "Veränderung zum Vormonat") %>%
  mutate(infl_change_year = ifelse(infl_change_year == "-", 0, infl_change_year),
         infl_change_month = ifelse(infl_change_month == "-", 0, infl_change_month)) %>% 
  mutate(infl_change_year = as.numeric(infl_change_year),
         infl_change_month = as.numeric(infl_change_month)) %>%
  mutate(
    infl_change_in_change = infl_change_year - dplyr::lag(infl_change_year),
    year = as.numeric(year))


german_months <- c("Januar", "Februar", "März", "April", "Mai", "Juni",
                   "Juli", "August", "September", "Oktober", "November", "Dezember")

# get numeric month value
infl$month <- match(infl$month_name, german_months)

all_sentiment <- all_sentiment %>% 
  left_join(infl,
            by = c("year", "month"))


unempl <- import("data/econ/unemployment.xlsx", skip = 2) %>% 
  fill(year, .direction = "down") %>% 
  mutate(unemployment_rate = as.numeric(unemployment_rate)) %>% 
  mutate(year = as.numeric(year),
         change_unemployment_rate = unemployment_rate - dplyr::lag(unemployment_rate),
         ychange_unemployment_rate = unemployment_rate - dplyr::lag(unemployment_rate, n = 12),
         month = match(month_name, german_months)) %>% 
  select(-month_name)

all_sentiment <- all_sentiment %>% 
  left_join(unempl,
            by = c("year", "month"))


gdp <- import("data/econ/gdp.xlsx") %>% 
  fill(year, .direction = "down") %>% 
  mutate(quarter = substr(quarter, 1, 1) %>% as.numeric(),
         qchange_gdp_raw = ((gdp_raw/dplyr::lag(gdp_raw)) - 1)*100,
         qchange_gdp_pa = ((gdp_pa/dplyr::lag(gdp_pa)) - 1)*100)

all_sentiment <- all_sentiment %>% 
  mutate(quarter = case_when(month %in% c(1:3) ~ 1,
                             month %in% c(4:6) ~ 2,
                             month %in% c(7:9) ~ 3,
                             month %in% c(10:12) ~ 4),
         fe_quarter = paste0(year, quarter),
         fe_2month = paste0(year, ceiling(month/2))) %>% 
  left_join(gdp,
            by = c("year", "quarter"))

all_sentiment <- all_sentiment %>% 
  mutate(positive_b = ifelse(sentiment_c == "Positiv", 1, 0),
         negative_b = ifelse(sentiment_c == "Negativ", 1, 0))


with(all_sentiment, table(sentiment_c, positive_b))
with(all_sentiment, table(sentiment_c, negative_b))


# all_sentiment <- all_sentiment %>% 
#   left_join(politbarometer,
#             by = c("year", "month"))


# linear time fixed effect
all_sentiment$day_count <- as.numeric(all_sentiment$date - min(all_sentiment$date)) + 1


colSums(is.na(all_sentiment))

all_sentiment <- all_sentiment %>% 
  select(-title, -keywords) %>% 
  #filter(date < "2024-04-01") %>% 
  drop_na()

all_sentiment_f <- all_sentiment
save(all_sentiment_f, file = "data/final/all_sentiment_f.RData")



# Descriptives  ------------------------------------------------------------

welt_hex <- "#003a5a"
zeit_hex <- "#b91109"
colors <- c("welt" = welt_hex, "zeit" = zeit_hex)

CDU_hex <- "#000000"
SPD_hex <- "#e3000f"
colors2 <- c("Merkel" = CDU_hex,
             "Scholz" = SPD_hex)

mdf <- all_sentiment %>% 
  group_by(year, month) %>% 
  dplyr::summarise(m_sent = mean(sentiment_n, na.rm = TRUE),
                   date,
                   government)

mdf2 <- mdf %>% 
  select(-date) %>% 
  distinct() %>% 
  left_join(politbarometer,
            by = c("year", "month"))

cor.test(mdf2$approval, mdf2$m_sent)
cor.test(dplyr::lag(mdf2$approval), mdf2$m_sent)
cor.test(dplyr::lag(mdf2$approval, n = 2), mdf2$m_sent)



politbarometer %>% 
  ggplot(aes(x = date, y = approval, color = government, linetype = "Approval")) +
  geom_line() +
  geom_line(data = mdf, 
            aes(x = date, y = m_sent, color = government, linetype = "Sentiment")) +
  scale_color_manual(values = colors2, name = "Government") +
  scale_linetype_manual(values = c("Approval" = "dashed", "Sentiment" = "solid"),
                        name = "Variable") +
  labs(x = "", 
       y = "Government Approval (-5 to 5) and sentiment score (0 - 10)") +
  theme_light()


ggsave("results/graphs/demand.pdf", width = 8, height = 5)

all_sentiment %>% 
  group_by(newspaper, year, month) %>% 
  dplyr::summarise(n = n(),
            date) %>% 
  ggplot(aes(x = date, y = n, color = newspaper)) +
  geom_line(size = 1) +
  scale_color_manual(values = colors) +
  labs(x = "",
       y = "Number of articles") +
  theme_light()

ggsave("results/graphs/desc_narticles.pdf", width = 7, height = 5)


all_sentiment %>% 
  group_by(newspaper, year, month) %>% 
  dplyr::summarise(m_sent = mean(sentiment_n, na.rm = TRUE),
            date) %>% 
  ggplot(aes(x = date, y = m_sent, color = newspaper)) + 
  geom_line(size = 1) +
  scale_color_manual(values = colors) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  geom_vline(aes(xintercept = as.Date("2021-12-08"), linetype = "Government change"), color = "gray6", size = 0.85) +
  scale_linetype_manual(name = "Government change", values = "solid") +
  labs(x = "",
       y = "Mean sentiment score") +
  theme_light()

ggsave("results/graphs/desc_sentiment.pdf", width = 7, height = 5)


econ_labels <- as_labeller(c(
  `infl_change_year` = "Yearly inflation change",
  `unemployment_rate` = "Unemployment Rate",
  `qchange_gdp_pa` = "Quarterly GDP change (price adjusted)",
  `infl_change_month` = "Monthly inflation change",
  `change_unemployment_rate` = "Change in unemployment Rate",
  `ychange_gdp_pa` = "Yearly GDP change (price adjusted)"
))

all_sentiment %>% 
  select(fe_month, date, infl_change_year, unemployment_rate, qchange_gdp_pa,
         infl_change_month, change_unemployment_rate, ychange_gdp_pa) %>% 
  pivot_longer(cols = c(infl_change_year, unemployment_rate, qchange_gdp_pa,
                        infl_change_month, change_unemployment_rate, ychange_gdp_pa),
               names_to = "variable", 
               values_to = "value") %>% 
  ggplot(aes(x = date, y = value)) +
  geom_line() +
  geom_hline(yintercept = 0, color = "red3") +
  geom_vline(aes(xintercept = as.Date("2021-12-08")), color = "gray6") +
  facet_wrap(~ factor(variable, c("infl_change_year", "unemployment_rate", "qchange_gdp_pa",
                                  "infl_change_month", "change_unemployment_rate", "ychange_gdp_pa")),
             labeller= econ_labels) +
  labs(x = "",
       y = "Value in %") +
  theme_light()

ggsave("results/graphs/desc_econ.pdf", width = 7, height = 5)


all_sentiment %>% 
  group_by(newspaper, government) %>% 
  dplyr::summarise(m = mean(sentiment_n, na.rm = TRUE)) %>% 
  rename(Newspaper = newspaper,
         Government = government,
         "Mean Sentiment Score" = m) %>% 
  xtable() %>% 
  print(type = "latex", file = "results/tables/desc_sentiment_n.tex", include.rownames = FALSE)

with(all_sentiment,
     table(sentiment_c, government, newspaper)) %>% 
  prop.table(margin = c(2,3)) %>% round(3)*100

all_sentiment %>% 
  group_by(newspaper, government, sentiment_c) %>% 
  count() %>% 
  group_by(newspaper, government) %>% 
  dplyr::summarise(perc = n/sum(n)*100,
            sentiment_c) %>% 
  ggplot(aes(x = government, y = perc, fill = sentiment_c)) +
  geom_col() +
  geom_text(aes(label = paste0(round(perc, 1), "%"), 
                y = perc),
            position = position_stack(vjust = 0.5),
            color = "white", size = 3.5) +
  scale_fill_manual(values = c("Negativ" = "red3",
                               "Neutral" = "orange1",
                               "Positiv" = "green4"),
                    name = "Sentiment") +
  labs(x = "", y = "Percentage") +
  facet_wrap(~ newspaper,
             labeller = as_labeller(c(
               `zeit` = "Zeit",
               `welt` = "Welt"))) +
  theme_light()

ggsave("results/graphs/desc_sentiment_c.pdf", width = 7, height = 5)
  

controls_infl <- c("vpi", "infl_change_year", "infl_change_month")

controls_unemp <- c("unemployment_rate", 
                    "change_unemployment_rate", "ychange_unemployment_rate")

controls_gdp <- c("gdp_pa", 
                  "ychange_gdp_pa", "qchange_gdp_pa")

controls_all <- c(controls_infl, controls_unemp, controls_gdp)


cor_control <- all_sentiment %>% 
  select(sentiment_n, controls_all) %>% 
  correlate() %>% 
  shave()


# Models on Negative ------------------------------------------------------------------
all_sentiment <- all_sentiment %>% 
  mutate(newspaper = factor(newspaper),
         government = factor(government))

var_labels <- c("newspaperzeit" = "Newspaper: Zeit (ref. Welt)",
          "governmentScholz" = "Government: Scholz (ref. Merkel)",
          "newspaperzeit x governmentScholz" = "Interaction: Zeit x Scholz",
          "newspaperzeit:governmentScholz" = "Interaction: Zeit x Scholz",
          "infl_change_year" = "Yearly inflation",
          "unemployment_rate" = "Unemployment rate",
          "qchange_gdp_pa" = "Quarterly GDP change (price adjusted)",
          "fe_2month" = "Bimonthly fixed effects",
          "sentiment_n" = "Numeric sentiment score (0-10)",
          "positive_b" = "Article is positive",
          "negative_b" = "Article is negative",
          "infl_change_month" = "Monthly Inflation Change",
          "ychange_gdp_pa" = "Yearly GDP change (price adjusted)",
          "ychange_unemployment_rate" = "Yearly Change of unemployment rate")

models <- list()

#### Base model ####
models[["Model 1"]] <- feglm(negative_b ~ newspaper*government, data = all_sentiment, family = "binomial")

summary(models[["Model 1"]])



predict_response(models[["Model 1"]],
                 terms = c("government", "newspaper")
)


ame1 <- avg_slopes(models[["Model 1"]],
                   newdata = "marginalmeans",
                   variables = "government",
                   by = "newspaper",
                   type = "response",
                   #hypothesis = "pairwise"
)
summary(ame1)

ame1 %>%
  ggplot(aes(x = newspaper,
             y = estimate,
             ymin = conf.low,
             ymax = conf.high,
             color = newspaper)) +
  geom_point() +
  geom_pointrange() +
  labs(x = "Newspaper",
       y = "AMEs (Scholz cabinet with ref. Merkel cabinet)") +
  geom_hline(yintercept = 0, color = "red") +
  scale_x_discrete(labels = c("Welt", "Zeit")) +
  scale_color_manual(values = colors) +
  theme_light()

ggsave("results/graphs/negative_ame1.pdf", width = 7, height = 5)


#### Controls ####

models[["Model 2"]] <- feglm(negative_b ~ newspaper*government + infl_change_year + 
                               unemployment_rate + qchange_gdp_pa +
                               infl_change_month + ychange_gdp_pa + ychange_unemployment_rate, data = all_sentiment, family = "binomial")

summary(models[["Model 2"]])

ame2 <- avg_slopes(models[["Model 2"]],
           newdata = "marginalmeans",
           variables = "government",
           by = "newspaper",
           type = "response",
           #hypothesis = "pairwise"
)

predict_response(models[["Model 2"]],
                 terms = c("government", "newspaper"),
                 # vcov_fun = cluster,
                 margin = "marginalmeans"
)

ame2 %>%
  ggplot(aes(x = newspaper,
             y = estimate,
             ymin = conf.low,
             ymax = conf.high,
             color = newspaper)) +
  geom_point() +
  geom_pointrange() +
  labs(x = "Newspaper",
       y = "AMEs (Scholz cabinet with ref. Merkel cabinet)") +
  geom_hline(yintercept = 0, color = "red") +
  scale_x_discrete(labels = c("Welt", "Zeit")) +
  scale_color_manual(values = colors) +
  theme_light()

ggsave("results/graphs/negative_ame2.pdf", width = 7, height = 5)

cors <- all_sentiment %>% 
  select(controls_infl, controls_unemp, controls_gdp) %>% 
  correlate() %>% 
  shave()

combs <- expand.grid(controls_infl, controls_unemp, controls_gdp)


all_combs <- list()


for (i in 1:length(controls_all)) {
  combn_list <- combn(controls_all, i, simplify = FALSE)
  all_combs <- c(all_combs, combn_list)
}


all_possible_models <- function(i) {
  
  fmla <- as.formula(paste0("negative_b ~ newspaper*government + ",
                            paste(all_combs[[i]], collapse = " + ")))

  
  model <- feglm(fmla, data = all_sentiment, family = "binomial")
  
  tame <- avg_slopes(model, newdata = "marginalmeans",
                     variables = "government", by = "newspaper",
                     type = "response")
  
  # Extract the coefficient and p-value for both 'zeit' and 'welt'
  zeit <- summary(tame)[2, c(4, 7)]  # Coefficient and p-value for 'zeit'
  welt <- summary(tame)[1, c(4, 7)]  # Coefficient and p-value for 'welt'
  
  both <- data.frame(estimate_welt = numeric(), pvalue_welt = numeric(),
                     estimate_zeit = numeric(), pvalue_zeit = numeric())
  both[1, c(1, 2)] <- welt
  both[1, c(3, 4)] <- zeit
  
  return(both)  # Combine 'welt' and 'zeit' results
}


plan(multisession)

results_n <- future_lapply(1:length(all_combs), function(i) {
  all_possible_models(i)
})

results <- do.call(rbind, results_n)


save(results, file = "results/model3_sensitivity_negative.RData")
#load("results/model3_sensitivity3.RData")


results_l <- results %>% 
  select(1:4) %>% 
  pivot_longer(  cols = 1:4, 
                 names_to = c(".value", "newspaper"),
                 names_sep = "_")

results_l %>% 
  ggplot(aes(x = estimate, color = newspaper, fill = newspaper)) +
  geom_density(alpha = .2) +
  scale_color_manual(values = colors) +
  scale_fill_manual(values = colors) +
  labs(x = "Estimate as probability",
       y = "Density") +
  scale_x_continuous(limits = c(0, 0.15)) + 
  theme_light()

ggsave("results/graphs/negative_sens_estimate.pdf", width = 7, height = 5)

results_l %>% 
  ggplot(aes(x = pvalue, color = newspaper, fill = newspaper)) +
  geom_density(alpha = .2) +
  scale_color_manual(values = colors) +
  scale_fill_manual(values = colors) +
  labs(x = "P-value",
       y = "Density") +
  scale_x_continuous(limits = c(0, 0.2),
                     breaks = c(seq(0, 0.2, by = 0.05))) + 
  theme_light()

ggsave("results/graphs/negative_sens_pvalue.pdf", width = 7, height = 5)



etable(models, dict = var_labels,
       se.row = TRUE,
       signif.code = c("***"=0.001, "**"=0.01, "*"=0.05, "."=0.10),
       tex = TRUE,
       file = "results/tables/negative_models.tex",
       replace = TRUE
)


# linear model ------------------------------------------------------------



lm_models <- list()

#### Base model ####
lm_models[["Model 1"]] <- feols(sentiment_n ~ newspaper*government, data = all_sentiment,
                                vcov = "hetero"
                                    )

summary(lm_models[["Model 1"]])



ht1 <- hypothesis_test(lm_models[["Model 1"]], c("newspaper", "government"))




ht1 %>% 
  filter(newspaper %in% c("welt-welt", "zeit-zeit")) %>% 
  mutate(newspaper = substr(newspaper, 1, 4)) %>% 
  ggplot(aes(x = newspaper, 
             y = Contrast*-1,
             ymin = conf.low*-1,
             ymax = conf.high*-1,
             color = newspaper)) +
  geom_point() +
  geom_pointrange() +
  labs(x = "Newspaper",
       y = "Estimates (Scholz cabinet with ref. Merkel cabinet)") +
  geom_hline(yintercept = 0, color = "red") +
  scale_x_discrete(labels = c("Welt", "Zeit")) +
  scale_color_manual(values = colors) +
  theme_light()

ggsave("results/graphs/linear_estimate1.pdf", width = 7, height = 5)

predict_response(lm_models[["Model 1"]],
                 terms = c("government", "newspaper")
)




#### Controls ####


lm_models[["Model 2"]] <- feols(sentiment_n ~ newspaper*government + infl_change_year + 
                                  unemployment_rate + qchange_gdp_pa +
                                  infl_change_month + ychange_gdp_pa + ychange_unemployment_rate, data = all_sentiment,
                                vcov = "hetero"
                             )

summary(lm_models[["Model 2"]])






ht2 <- hypothesis_test(lm_models[["Model 2"]], c("newspaper", "government"))



ht2 %>% 
  filter(newspaper %in% c("welt-welt", "zeit-zeit")) %>% 
  mutate(newspaper = substr(newspaper, 1, 4)) %>% 
  ggplot(aes(x = newspaper, 
             y = Contrast*-1,
             ymin = conf.low*-1,
             ymax = conf.high*-1,
             color = newspaper)) +
  geom_point() +
  geom_pointrange() +
  labs(x = "Newspaper",
       y = "Estimates (Scholz cabinet with ref. Merkel cabinet)") +
  geom_hline(yintercept = 0, color = "red") +
  scale_x_discrete(labels = c("Welt", "Zeit")) +
  scale_color_manual(values = colors) +
  theme_light()

ggsave("results/graphs/linear_estimate2.pdf", width = 7, height = 5)


emmeans(lm_models[["Model 2"]], "government", by = "newspaper") %>% 
  summary() %>% 
  ggplot(aes(x = newspaper,
             y = emmean,
             ymin = lower.CL,
             ymax = upper.CL,
             color = government)) +
  geom_point(position = position_dodge(width = 0.5)) +
  geom_pointrange(position = position_dodge(width = 0.5)) +
  scale_color_manual(values = colors2,
                     name = "Government") +
  labs(x = "Newspaper",
       y = "Estimated Marginal Means") +
  scale_x_discrete(labels = c("Welt", "Zeit")) +
  theme_light()

ggsave("results/graphs/linear_emms.pdf", width = 7, height = 5)


all_possible_models_l <- function(i) {
  
  fmla <- as.formula(paste0("sentiment_n ~ newspaper*government + ",
                            paste(all_combs[[i]], collapse = " + ")))
  
  
  model <- feols(fmla, data = all_sentiment,
                              vcov = "hetero")
  
  ht <- hypothesis_test(model, c("newspaper", "government")) %>% 
    filter(newspaper %in% c("welt-welt", "zeit-zeit")) %>% 
    mutate(newspaper = substr(newspaper, 1, 4),
           Contrast = Contrast *-1)
  
  zeit <- ht[2, c(3, 6)]
  welt <- ht[1, c(3, 6)]
  
  both <- data.frame(estimate_welt = numeric(), pvalue_welt = numeric(),
                     estimate_zeit = numeric(), pvalue_zeit = numeric())
  both[1, c(1, 2)] <- welt
  both[1, c(3, 4)] <- zeit
  
  return(both)  # Combine 'welt' and 'zeit' results
}

#results <- matrix(NA, nrow = length(all_combs), ncol = 4)

plan(multisession)

results_n <- future_lapply(1:length(all_combs), function(i) {
  all_possible_models_l(i)
})

results <- do.call(rbind, results_n)





save(results, file = "results/lm_model3_sensitivity3.RData")
#load("results/lm_model3_sensitivity3.RData")



results_l <- results %>% 
  select(1:4) %>% 
  pivot_longer(  cols = 1:4,  # or you could specify specific columns
                 names_to = c(".value", "newspaper"),
                 names_sep = "_")

results_l %>% 
  ggplot(aes(x = estimate, color = newspaper, fill = newspaper)) +
  geom_density(alpha = .2) +
  scale_color_manual(values = colors) +
  scale_fill_manual(values = colors) +
  labs(x = "Estimate",
       y = "Density") +
  scale_x_continuous(limits = c(-1.5, 0)) + 
  theme_light()

ggsave("results/graphs/linear_sens_estimate.pdf", width = 7, height = 5)

results_l %>% 
  ggplot(aes(x = pvalue, color = newspaper, fill = newspaper)) +
  geom_density(alpha = .2) +
  scale_color_manual(values = colors) +
  scale_fill_manual(values = colors) +
  labs(x = "P-value",
       y = "Density") +
  scale_x_continuous(limits = c(0, 0.2)) + 
  theme_light()

ggsave("results/graphs/linear_sens_pvalue.pdf", width = 7, height = 5)


etable(lm_models, dict = var_labels,
       se.row = TRUE,
       signif.code = c("***"=0.001, "**"=0.01, "*"=0.05, "."=0.10),
       tex = TRUE,
       file = "results/tables/linear_models.tex",
       replace = TRUE)

# Models on positive  -----------------------------------------------------




pmodels <- list()

#### Base model ####
pmodels[["Model 1"]] <- feglm(positive_b ~ newspaper*government, data = all_sentiment, family = "binomial")

summary(pmodels[["Model 1"]])

#cluster <- vcovCL(pmodels[["Model 1"]], cluster = ~newspaper)





pame2 <- avg_slopes(pmodels[["Model 1"]],
                    newdata = "marginalmeans",
                    variables = "government",
                    by = "newspaper",
                    type = "response",
                    #hypothesis = "pairwise"
)

predict_response(pmodels[["Model 1"]],
                 terms = c("government", "newspaper"),
                 # vcov_fun = cluster,
                 margin = "marginalmeans"
)

pame2 %>%
  ggplot(aes(x = newspaper,
             y = estimate,
             ymin = conf.low,
             ymax = conf.high,
             color = newspaper)) +
  geom_point() +
  geom_pointrange() +
  labs(x = "Newspaper",
       y = "AMEs (Scholz cabinet with ref. Merkel cabinet)") +
  geom_hline(yintercept = 0, color = "red") +
  scale_x_discrete(labels = c("Welt", "Zeit")) +
  scale_color_manual(values = colors) +
  theme_light()

ggsave("results/graphs/positive_ame1.pdf", width = 7, height = 5)

#### Controls ####




pmodels[["Model 2"]] <- feglm(positive_b ~ newspaper*government + infl_change_year + 
                                unemployment_rate + qchange_gdp_pa +
                                infl_change_month + ychange_gdp_pa + ychange_unemployment_rate, 
                              data = all_sentiment, family = "binomial",
                             )

summary(pmodels[["Model 2"]])

pame2 <- avg_slopes(pmodels[["Model 2"]],
           newdata = "marginalmeans",
           variables = "government",
           by = "newspaper",
           type = "response",
           #hypothesis = "pairwise"
)

predict_response(pmodels[["Model 2"]],
                 terms = c("government", "newspaper"),
                 # vcov_fun = cluster,
                 margin = "marginalmeans"
)

pame2 %>%
  ggplot(aes(x = newspaper,
             y = estimate,
             ymin = conf.low,
             ymax = conf.high,
             color = newspaper)) +
  geom_point() +
  geom_pointrange() +
  labs(x = "Newspaper",
       y = "AMEs (Scholz cabinet with ref. Merkel cabinet)") +
  geom_hline(yintercept = 0, color = "red") +
  scale_x_discrete(labels = c("Welt", "Zeit")) +
  scale_color_manual(values = colors) +
  theme_light()

ggsave("results/graphs/positive_ame2.pdf", width = 7, height = 5)



all_possible_models_p <- function(i) {
  
  fmla <- as.formula(paste0("positive_b ~ newspaper*government + ",
                            paste(all_combs[[i]], collapse = " + ")))
  
  
  model <- feglm(fmla, data = all_sentiment, family = "binomial")
  
  tame <- avg_slopes(model, newdata = "marginalmeans",
                     variables = "government", by = "newspaper",
                     type = "response")
  
  # Extract the coefficient and p-value for both 'zeit' and 'welt'
  zeit <- summary(tame)[2, c(4, 7)]  # Coefficient and p-value for 'zeit'
  welt <- summary(tame)[1, c(4, 7)]  # Coefficient and p-value for 'welt'
  
  both <- data.frame(estimate_welt = numeric(), pvalue_welt = numeric(),
                     estimate_zeit = numeric(), pvalue_zeit = numeric())
  both[1, c(1, 2)] <- welt
  both[1, c(3, 4)] <- zeit
  
  return(both)  # Combine 'welt' and 'zeit' results
}


plan(multisession)

results_n <- future_lapply(1:length(all_combs), function(i) {
  all_possible_models_p(i)
})

results <- do.call(rbind, results_n)



save(results, file = "results/model3_sensitivity_positiv.RData")




results_l <- results %>% 
  select(1:4) %>% 
  pivot_longer(  cols = 1:4,  # or you could specify specific columns
                 names_to = c(".value", "newspaper"),
                 names_sep = "_") %>% 
  mutate(pvalue = round(pvalue, 5),
         estimate = round(estimate, 5))



results_l %>% 
  ggplot(aes(x = estimate, color = newspaper, fill = newspaper)) +
  geom_density(alpha = .2) +
  scale_color_manual(values = colors) +
  scale_fill_manual(values = colors) +
  labs(x = "Estimate as probability",
       y = "Density") +
  #scale_x_continuous(limits = c(0, 0.15)) + 
  theme_light()

ggsave("results/graphs/positive_sens_estimate.pdf", width = 7, height = 5)

results_l %>% 
  ggplot(aes(x = pvalue, color = newspaper, fill = newspaper)) +
  geom_density(alpha = .2) +
  scale_color_manual(values = colors) +
  scale_fill_manual(values = colors) +
  labs(x = "P-value",
       y = "Density") +
  scale_x_continuous(limits = c(0, 1),
                     breaks = c(seq(0, 1, by = 0.1))) + 
  theme_light()

ggsave("results/graphs/positive_sens_pvalue.pdf", width = 7, height = 5)




etable(pmodels, dict = var_labels,
       se.row = TRUE,
       signif.code = c("***"=0.001, "**"=0.01, "*"=0.05, "."=0.10),
       tex = TRUE,
       file = "results/tables/positive_models.tex",
       replace = TRUE
       )





# Robustness: shorter time frames -----------------------------------------

min(all_sentiment$date)
max(all_sentiment$date)
all_sentiment_robust1 <- all_sentiment %>% 
  filter(date > "2019-03-14" & date < "2023-06-29")

models_robust1 <- list()

#### Base model ####
models_robust1[["Model 1"]] <- feglm(negative_b ~ newspaper*government, data = all_sentiment_robust1, family = "binomial")

summary(models_robust1[["Model 1"]])

#cluster <- vcovCL(models_robust1[["Model 1"]], cluster = ~newspaper)


predict_response(models_robust1[["Model 1"]],
                 terms = c("government", "newspaper"),
                 #  vcov_fun = cluster
)


ame1 <- avg_slopes(models_robust1[["Model 1"]],
                   newdata = "marginalmeans",
                   variables = "government",
                   by = "newspaper",
                   type = "response",
                   #hypothesis = "pairwise"
)
summary(ame1)

ame1 %>%
  ggplot(aes(x = newspaper,
             y = estimate,
             ymin = conf.low,
             ymax = conf.high,
             color = newspaper)) +
  geom_point() +
  geom_pointrange() +
  labs(x = "Newspaper",
       y = "AMEs (Scholz cabinet with ref. Merkel cabinet)") +
  geom_hline(yintercept = 0, color = "red") +
  scale_x_discrete(labels = c("Welt", "Zeit")) +
  scale_color_manual(values = colors) +
  theme_light()

ggsave("results/graphs/robustness1_negative_ame1.pdf", width = 7, height = 5)



#### Controls ####


models_robust1[["Model 2"]] <- feglm(negative_b ~ newspaper*government + infl_change_year + 
                               unemployment_rate + qchange_gdp_pa +
                               infl_change_month + ychange_gdp_pa + ychange_unemployment_rate, data = all_sentiment_robust1, family = "binomial")

summary(models_robust1[["Model 2"]])

ame2 <- avg_slopes(models_robust1[["Model 2"]],
                   newdata = "marginalmeans",
                   variables = "government",
                   by = "newspaper",
                   type = "response",
                   #hypothesis = "pairwise"
)

predict_response(models_robust1[["Model 2"]],
                 terms = c("government", "newspaper"),
                 # vcov_fun = cluster,
                 margin = "marginalmeans"
)

ame2 %>%
  ggplot(aes(x = newspaper,
             y = estimate,
             ymin = conf.low,
             ymax = conf.high,
             color = newspaper)) +
  geom_point() +
  geom_pointrange() +
  labs(x = "Newspaper",
       y = "AMEs (Scholz cabinet with ref. Merkel cabinet)") +
  geom_hline(yintercept = 0, color = "red") +
  scale_x_discrete(labels = c("Welt", "Zeit")) +
  scale_color_manual(values = colors) +
  theme_light()

ggsave("results/graphs/robustness1_negative_ame2.pdf", width = 7, height = 5)




all_possible_models_robust1 <- function(i) {
  
  fmla <- as.formula(paste0("negative_b ~ newspaper*government + ",
                            paste(all_combs[[i]], collapse = " + ")))
  
  
  model <- feglm(fmla, data = all_sentiment_robust1, 
                 family = "binomial")
  
  tame <- avg_slopes(model, newdata = "marginalmeans",
                     variables = "government", by = "newspaper",
                     type = "response")
  
  # Extract the coefficient and p-value for both 'zeit' and 'welt'
  zeit <- summary(tame)[2, c(4, 7)]  # Coefficient and p-value for 'zeit'
  welt <- summary(tame)[1, c(4, 7)]  # Coefficient and p-value for 'welt'
  
  both <- data.frame(estimate_welt = numeric(), pvalue_welt = numeric(),
                     estimate_zeit = numeric(), pvalue_zeit = numeric())
  both[1, c(1, 2)] <- welt
  both[1, c(3, 4)] <- zeit
  
  return(both)  # Combine 'welt' and 'zeit' results
}

#results <- matrix(NA, nrow = length(all_combs), ncol = 4)

plan(multisession)

results_n <- future_lapply(1:length(all_combs), function(i) {
  all_possible_models_robust1(i)
})

results <- do.call(rbind, results_n)


save(results, file = "results/robustness1_model3_sensitivity_negative.RData")
#load("results/model3_sensitivity3.RData")


results_l <- results %>% 
  select(1:4) %>% 
  pivot_longer(  cols = 1:4,  # or you could specify specific columns
                 names_to = c(".value", "newspaper"),
                 names_sep = "_")

results_l %>% 
  ggplot(aes(x = estimate, color = newspaper, fill = newspaper)) +
  geom_density(alpha = .2) +
  scale_color_manual(values = colors) +
  scale_fill_manual(values = colors) +
  labs(x = "Estimate as probability",
       y = "Density") +
 # scale_x_continuous(limits = c(0, 0.15)) + 
  theme_light()

ggsave("results/graphs/robustness1_negative_sens_estimate.pdf", width = 7, height = 5)

results_l %>% 
  ggplot(aes(x = pvalue, color = newspaper, fill = newspaper)) +
  geom_density(alpha = .2) +
  scale_color_manual(values = colors) +
  scale_fill_manual(values = colors) +
  labs(x = "P-value",
       y = "Density") +
  scale_x_continuous(limits = c(0, 0.2),
                     breaks = c(seq(0, 0.2, by = 0.05))) + 
  theme_light()

ggsave("results/graphs/robustness1_negative_sens_pvalue.pdf", width = 7, height = 5)

#etable(models_robust1)

etable(models_robust1, dict = var_labels,
       se.row = TRUE,
       signif.code = c("***"=0.001, "**"=0.01, "*"=0.05, "."=0.10),
       tex = TRUE,
       file = "results/tables/robustness1_negative_models_robust1.tex",
       replace = TRUE
)


# linear model ------------------------------------------------------------


lm_models_robust1 <- list()

#### Base model ####
lm_models_robust1[["Model 1"]] <- feols(sentiment_n ~ newspaper*government, data = all_sentiment_robust1,
                                vcov = "hetero"
)

summary(lm_models_robust1[["Model 1"]])

#cluster <- vcovCL(lm_models_robust1[["Model 1"]], cluster = ~newspaper)



ht1 <- hypothesis_test(lm_models_robust1[["Model 1"]], c("newspaper", "government"))




ht1 %>% 
  filter(newspaper %in% c("welt-welt", "zeit-zeit")) %>% 
  mutate(newspaper = substr(newspaper, 1, 4)) %>% 
  ggplot(aes(x = newspaper, 
             y = Contrast*-1,
             ymin = conf.low*-1,
             ymax = conf.high*-1,
             color = newspaper)) +
  geom_point() +
  geom_pointrange() +
  labs(x = "Newspaper",
       y = "Estimates (Scholz cabinet with ref. Merkel cabinet)") +
  geom_hline(yintercept = 0, color = "red") +
  scale_x_discrete(labels = c("Welt", "Zeit")) +
  scale_color_manual(values = colors) +
  theme_light()

ggsave("results/graphs/robustness1_linear_estimate1.pdf", width = 7, height = 5)

predict_response(lm_models_robust1[["Model 1"]],
                 terms = c("government", "newspaper")
)




#### Controls ####


lm_models_robust1[["Model 2"]] <- feols(sentiment_n ~ newspaper*government + infl_change_year + 
                                  unemployment_rate + qchange_gdp_pa +
                                  infl_change_month + ychange_gdp_pa + ychange_unemployment_rate, data = all_sentiment_robust1,
                                vcov = "hetero"
)

summary(lm_models_robust1[["Model 2"]])



ht2 <- hypothesis_test(lm_models_robust1[["Model 2"]], c("newspaper", "government"))



ht2 %>% 
  filter(newspaper %in% c("welt-welt", "zeit-zeit")) %>% 
  mutate(newspaper = substr(newspaper, 1, 4)) %>% 
  ggplot(aes(x = newspaper, 
             y = Contrast*-1,
             ymin = conf.low*-1,
             ymax = conf.high*-1,
             color = newspaper)) +
  geom_point() +
  geom_pointrange() +
  labs(x = "Newspaper",
       y = "Estimates (Scholz cabinet with ref. Merkel cabinet)") +
  geom_hline(yintercept = 0, color = "red") +
  scale_x_discrete(labels = c("Welt", "Zeit")) +
  scale_color_manual(values = colors) +
  theme_light()

ggsave("results/graphs/robustness1_linear_estimate2.pdf", width = 7, height = 5)

all_possible_models_robust1_l <- function(i) {
  
  fmla <- as.formula(paste0("sentiment_n ~ newspaper*government + ",
                            paste(all_combs[[i]], collapse = " + ")))
  
  
  model <- feols(fmla, data = all_sentiment_robust1,
                 vcov = "hetero")
  
  ht <- hypothesis_test(model, c("newspaper", "government")) %>% 
    filter(newspaper %in% c("welt-welt", "zeit-zeit")) %>% 
    mutate(newspaper = substr(newspaper, 1, 4),
           Contrast = Contrast *-1)
  
  zeit <- ht[2, c(3, 6)]
  welt <- ht[1, c(3, 6)]
  
  both <- data.frame(estimate_welt = numeric(), pvalue_welt = numeric(),
                     estimate_zeit = numeric(), pvalue_zeit = numeric())
  both[1, c(1, 2)] <- welt
  both[1, c(3, 4)] <- zeit
  
  return(both)  # Combine 'welt' and 'zeit' results
}


plan(multisession)

results_n <- future_lapply(1:length(all_combs), function(i) {
  all_possible_models_robust1_l(i)
})

results <- do.call(rbind, results_n)


save(results, file = "results/lm_model3_sensitivity3.RData")
#load("results/lm_model3_sensitivity3.RData")



results_l <- results %>% 
  select(1:4) %>% 
  pivot_longer(  cols = 1:4,  # or you could specify specific columns
                 names_to = c(".value", "newspaper"),
                 names_sep = "_")

results_l %>% 
  ggplot(aes(x = estimate, color = newspaper, fill = newspaper)) +
  geom_density(alpha = .2) +
  scale_color_manual(values = colors) +
  scale_fill_manual(values = colors) +
  labs(x = "Estimate",
       y = "Density") +
  #scale_x_continuous(limits = c(0, 0.15)) + 
  theme_light()

ggsave("results/graphs/robustness1_linear_sens_estimate.pdf", width = 7, height = 5)

results_l %>% 
  ggplot(aes(x = pvalue, color = newspaper, fill = newspaper)) +
  geom_density(alpha = .2) +
  scale_color_manual(values = colors) +
  scale_fill_manual(values = colors) +
  labs(x = "P-value",
       y = "Density") +
  scale_x_continuous(limits = c(0, 0.5)) + 
  theme_light()

ggsave("results/graphs/robustness1_linear_sens_pvalue.pdf", width = 7, height = 5)


etable(lm_models_robust1, dict = var_labels,
       se.row = TRUE,
       signif.code = c("***"=0.001, "**"=0.01, "*"=0.05, "."=0.10),
       tex = TRUE,
       file = "results/tables/robustness1_linear_models_robust1.tex",
       replace = TRUE)

# models_robust1 on positive  -----------------------------------------------------


pmodels_robust1 <- list()

#### Base model ####
pmodels_robust1[["Model 1"]] <- feglm(positive_b ~ newspaper*government, data = all_sentiment_robust1, family = "binomial")

summary(pmodels_robust1[["Model 1"]])


pame1 <- avg_slopes(pmodels_robust1[["Model 1"]],
                    newdata = "marginalmeans",
                    variables = "government",
                    by = "newspaper",
                    type = "response",
)

predict_response(pmodels_robust1[["Model 1"]],
                 terms = c("government", "newspaper"),
                 margin = "marginalmeans"
)

pame1 %>%
  ggplot(aes(x = newspaper,
             y = estimate,
             ymin = conf.low,
             ymax = conf.high,
             color = newspaper)) +
  geom_point() +
  geom_pointrange() +
  labs(x = "Newspaper",
       y = "AMEs (Scholz cabinet with ref. Merkel cabinet)") +
  geom_hline(yintercept = 0, color = "red") +
  scale_x_discrete(labels = c("Welt", "Zeit")) +
  scale_color_manual(values = colors) +
  theme_light()

ggsave("results/graphs/robustness1_positive_ame1.pdf", width = 7, height = 5)

#### Controls ####

pmodels_robust1[["Model 2"]] <- glm(positive_b ~ newspaper*government + infl_change_year + 
                              unemployment_rate + qchange_gdp_pa +
                              infl_change_month + ychange_gdp_pa + ychange_unemployment_rate, 
                            data = all_sentiment_robust1, family = "binomial",
)

summary(pmodels_robust1[["Model 2"]])

pame2 <- avg_slopes(pmodels_robust1[["Model 2"]],
                    newdata = "marginalmeans",
                    variables = "government",
                    by = "newspaper",
                    type = "response",
)

predict_response(pmodels_robust1[["Model 2"]],
                 terms = c("government", "newspaper"),
                 margin = "marginalmeans"
)

pame2 %>%
  ggplot(aes(x = newspaper,
             y = estimate,
             ymin = conf.low,
             ymax = conf.high,
             color = newspaper)) +
  geom_point() +
  geom_pointrange() +
  labs(x = "Newspaper",
       y = "AMEs (Scholz cabinet with ref. Merkel cabinet)") +
  geom_hline(yintercept = 0, color = "red") +
  scale_x_discrete(labels = c("Welt", "Zeit")) +
  scale_color_manual(values = colors) +
  theme_light()

ggsave("results/graphs/robustness1_positive_ame2.pdf", width = 7, height = 5)


all_possible_models_robust1_p <- function(i) {
  
  fmla <- as.formula(paste0("positive_b ~ newspaper*government + ",
                            paste(all_combs[[i]], collapse = " + ")))
  
  
  model <- feglm(fmla, data = all_sentiment_robust1, 
                 family = "binomial")
  
  tame <- avg_slopes(model, newdata = "marginalmeans",
                     variables = "government", by = "newspaper",
                     type = "response")
  
  # Extract the coefficient and p-value for both 'zeit' and 'welt'
  zeit <- summary(tame)[2, c(4, 7)]  # Coefficient and p-value for 'zeit'
  welt <- summary(tame)[1, c(4, 7)]  # Coefficient and p-value for 'welt'
  
  both <- data.frame(estimate_welt = numeric(), pvalue_welt = numeric(),
                     estimate_zeit = numeric(), pvalue_zeit = numeric())
  both[1, c(1, 2)] <- welt
  both[1, c(3, 4)] <- zeit
  
  return(both)  # Combine 'welt' and 'zeit' results
}


plan(multisession)

results_n <- future_lapply(1:length(all_combs), function(i) {
  all_possible_models_robust1_p(i)
})

results <- do.call(rbind, results_n)



save(results, file = "results/model3_sensitivity_positiv.RData")




results_l <- results %>% 
  select(1:4) %>% 
  pivot_longer(  cols = 1:4,  # or you could specify specific columns
                 names_to = c(".value", "newspaper"),
                 names_sep = "_") %>% 
  mutate(pvalue = round(pvalue, 5),
         estimate = round(estimate, 5))



results_l %>% 
  ggplot(aes(x = estimate, color = newspaper, fill = newspaper)) +
  geom_density(alpha = .2) +
  scale_color_manual(values = colors) +
  scale_fill_manual(values = colors) +
  labs(x = "Estimate as probability",
       y = "Density") +
  #scale_x_continuous(limits = c(0, 0.15)) + 
  theme_light()

ggsave("results/graphs/robustness1_positive_sens_estimate.pdf", width = 7, height = 5)

results_l %>% 
  ggplot(aes(x = pvalue, color = newspaper, fill = newspaper)) +
  geom_density(alpha = .2) +
  scale_color_manual(values = colors) +
  scale_fill_manual(values = colors) +
  labs(x = "P-value",
       y = "Density") +
  scale_x_continuous(limits = c(0, 1),
                     breaks = c(seq(0, 1, by = 0.1))) + 
  theme_light()

ggsave("results/graphs/robustness1_positive_sens_pvalue.pdf", width = 7, height = 5)




etable(pmodels_robust1, dict = var_labels,
       se.row = TRUE,
       signif.code = c("***"=0.001, "**"=0.01, "*"=0.05, "."=0.10),
       tex = TRUE,
       file = "results/tables/robustness1_positive_models_robust1.tex",
       replace = TRUE
)



# Robustness 2: shorter time frames -----------------------------------------

min(all_sentiment$date)
max(all_sentiment$date)
all_sentiment_robust2 <- all_sentiment %>% 
  filter(date > "2020-12-08" & date < "2022-12-08")

models_robust2 <- list()

#### Base model ####
models_robust2[["Model 1"]] <- feglm(negative_b ~ newspaper*government, data = all_sentiment_robust2, family = "binomial")

summary(models_robust2[["Model 1"]])

predict_response(models_robust2[["Model 1"]],
                 terms = c("government", "newspaper")
)


ame1 <- avg_slopes(models_robust2[["Model 1"]],
                   newdata = "marginalmeans",
                   variables = "government",
                   by = "newspaper",
                   type = "response"
)
summary(ame1)

ame1 %>%
  ggplot(aes(x = newspaper,
             y = estimate,
             ymin = conf.low,
             ymax = conf.high,
             color = newspaper)) +
  geom_point() +
  geom_pointrange() +
  labs(x = "Newspaper",
       y = "AMEs (Scholz cabinet with ref. Merkel cabinet)") +
  geom_hline(yintercept = 0, color = "red") +
  scale_x_discrete(labels = c("Welt", "Zeit")) +
  scale_color_manual(values = colors) +
  theme_light()

ggsave("results/graphs/robustness2_negative_ame1.pdf", width = 7, height = 5)



#### Controls ####


models_robust2[["Model 2"]] <- feglm(negative_b ~ newspaper*government + infl_change_year + 
                                       unemployment_rate + qchange_gdp_pa +
                                       infl_change_month + ychange_gdp_pa + ychange_unemployment_rate, data = all_sentiment_robust2, family = "binomial")

summary(models_robust2[["Model 2"]])

ame2 <- avg_slopes(models_robust2[["Model 2"]],
                   newdata = "marginalmeans",
                   variables = "government",
                   by = "newspaper",
                   type = "response",
)

predict_response(models_robust2[["Model 2"]],
                 terms = c("government", "newspaper"),
                 margin = "marginalmeans"
)

ame2 %>%
  ggplot(aes(x = newspaper,
             y = estimate,
             ymin = conf.low,
             ymax = conf.high,
             color = newspaper)) +
  geom_point() +
  geom_pointrange() +
  labs(x = "Newspaper",
       y = "AMEs (Scholz cabinet with ref. Merkel cabinet)") +
  geom_hline(yintercept = 0, color = "red") +
  scale_x_discrete(labels = c("Welt", "Zeit")) +
  scale_color_manual(values = colors) +
  theme_light()

ggsave("results/graphs/robustness2_negative_ame2.pdf", width = 7, height = 5)



all_possible_models_robust2 <- function(i) {
  
  fmla <- as.formula(paste0("negative_b ~ newspaper*government + ",
                            paste(all_combs[[i]], collapse = " + ")))
  
  
  model <- feglm(fmla, data = all_sentiment_robust2, 
                 family = "binomial"
                 )
  
  tame <- avg_slopes(model, newdata = "marginalmeans",
                     variables = "government", by = "newspaper",
                     type = "response")
  
  # Extract the coefficient and p-value for both 'zeit' and 'welt'
  zeit <- summary(tame)[2, c(4, 7)]  # Coefficient and p-value for 'zeit'
  welt <- summary(tame)[1, c(4, 7)]  # Coefficient and p-value for 'welt'
  
  both <- data.frame(estimate_welt = numeric(), pvalue_welt = numeric(),
                     estimate_zeit = numeric(), pvalue_zeit = numeric())
  both[1, c(1, 2)] <- welt
  both[1, c(3, 4)] <- zeit
  
  return(both)  # Combine 'welt' and 'zeit' results
}

plan(multisession)

results_n <- future_lapply(1:length(all_combs), function(i) {
  all_possible_models_robust2(i)
})

results <- do.call(rbind, results_n)


save(results, file = "results/robustness2_model3_sensitivity_negative.RData")
#load("results/model3_sensitivity3.RData")


results_l <- results %>% 
  select(1:4) %>% 
  pivot_longer(  cols = 1:4,  # or you could specify specific columns
                 names_to = c(".value", "newspaper"),
                 names_sep = "_")

results_l %>% 
  ggplot(aes(x = estimate, color = newspaper, fill = newspaper)) +
  geom_density(alpha = .2) +
  scale_color_manual(values = colors) +
  scale_fill_manual(values = colors) +
  labs(x = "Estimate as probability",
       y = "Density") +
  # scale_x_continuous(limits = c(0, 0.15)) + 
  theme_light()

ggsave("results/graphs/robustness2_negative_sens_estimate.pdf", width = 7, height = 5)

results_l %>% 
  ggplot(aes(x = pvalue, color = newspaper, fill = newspaper)) +
  geom_density(alpha = .2) +
  scale_color_manual(values = colors) +
  scale_fill_manual(values = colors) +
  labs(x = "P-value",
       y = "Density") +
  scale_x_continuous(limits = c(0, 0.2),
                     breaks = c(seq(0, 0.2, by = 0.05))) + 
  theme_light()

ggsave("results/graphs/robustness2_negative_sens_pvalue.pdf", width = 7, height = 5)

etable(models_robust2)

etable(models_robust2, dict = var_labels,
       se.row = TRUE,
       signif.code = c("***"=0.001, "**"=0.01, "*"=0.05, "."=0.10),
       tex = TRUE,
       file = "results/tables/robustness2_negative_models_robust2.tex",
       replace = TRUE
)


# linear model ------------------------------------------------------------


lm_models_robust2 <- list()

#### Base model ####
lm_models_robust2[["Model 1"]] <- feols(sentiment_n ~ newspaper*government, data = all_sentiment_robust2,
                                        vcov = "hetero"
)

summary(lm_models_robust2[["Model 1"]])


ht1 <- hypothesis_test(lm_models_robust2[["Model 1"]], c("newspaper", "government"))




ht1 %>% 
  filter(newspaper %in% c("welt-welt", "zeit-zeit")) %>% 
  mutate(newspaper = substr(newspaper, 1, 4)) %>% 
  ggplot(aes(x = newspaper, 
             y = Contrast*-1,
             ymin = conf.low*-1,
             ymax = conf.high*-1,
             color = newspaper)) +
  geom_point() +
  geom_pointrange() +
  labs(x = "Newspaper",
       y = "Estimates (Scholz cabinet with ref. Merkel cabinet)") +
  geom_hline(yintercept = 0, color = "red") +
  scale_x_discrete(labels = c("Welt", "Zeit")) +
  scale_color_manual(values = colors) +
  theme_light()

ggsave("results/graphs/robustness2_linear_estimate1.pdf", width = 7, height = 5)

predict_response(lm_models_robust2[["Model 1"]],
                 terms = c("government", "newspaper")
)




#### Controls ####


lm_models_robust2[["Model 2"]] <- feols(sentiment_n ~ newspaper*government + infl_change_year + 
                                          unemployment_rate + qchange_gdp_pa +
                                          infl_change_month + ychange_gdp_pa + ychange_unemployment_rate, data = all_sentiment_robust2,
                                        vcov = "hetero"
)

summary(lm_models_robust2[["Model 2"]])



ht2 <- hypothesis_test(lm_models_robust2[["Model 2"]], c("newspaper", "government"))



ht2 %>% 
  filter(newspaper %in% c("welt-welt", "zeit-zeit")) %>% 
  mutate(newspaper = substr(newspaper, 1, 4)) %>% 
  ggplot(aes(x = newspaper, 
             y = Contrast*-1,
             ymin = conf.low*-1,
             ymax = conf.high*-1,
             color = newspaper)) +
  geom_point() +
  geom_pointrange() +
  labs(x = "Newspaper",
       y = "Estimates (Scholz cabinet with ref. Merkel cabinet)") +
  geom_hline(yintercept = 0, color = "red") +
  scale_x_discrete(labels = c("Welt", "Zeit")) +
  scale_color_manual(values = colors) +
  theme_light()

ggsave("results/graphs/robustness2_linear_estimate2.pdf", width = 7, height = 5)



all_possible_models_robust2_l <- function(i) {
  
  fmla <- as.formula(paste0("sentiment_n ~ newspaper*government + ",
                            paste(all_combs[[i]], collapse = " + ")))
  
  
  model <- feols(fmla, data = all_sentiment_robust2,
                 vcov = "hetero")
  
  ht <- hypothesis_test(model, c("newspaper", "government")) %>% 
    filter(newspaper %in% c("welt-welt", "zeit-zeit")) %>% 
    mutate(newspaper = substr(newspaper, 1, 4),
           Contrast = Contrast *-1)
  
  zeit <- ht[2, c(3, 6)]
  welt <- ht[1, c(3, 6)]
  
  both <- data.frame(estimate_welt = numeric(), pvalue_welt = numeric(),
                     estimate_zeit = numeric(), pvalue_zeit = numeric())
  both[1, c(1, 2)] <- welt
  both[1, c(3, 4)] <- zeit
  
  return(both)  # Combine 'welt' and 'zeit' results
}

#results <- matrix(NA, nrow = length(all_combs), ncol = 4)

plan(multisession)

results_n <- future_lapply(1:length(all_combs), function(i) {
  all_possible_models_robust2_l(i)
})

results <- do.call(rbind, results_n)





save(results, file = "results/lm_model3_sensitivity3.RData")
#load("results/lm_model3_sensitivity3.RData")



results_l <- results %>% 
  select(1:4) %>% 
  pivot_longer(  cols = 1:4,  # or you could specify specific columns
                 names_to = c(".value", "newspaper"),
                 names_sep = "_")

results_l %>% 
  ggplot(aes(x = estimate, color = newspaper, fill = newspaper)) +
  geom_density(alpha = .2) +
  scale_color_manual(values = colors) +
  scale_fill_manual(values = colors) +
  labs(x = "Estimate",
       y = "Density") +
  #scale_x_continuous(limits = c(0, 0.15)) + 
  theme_light()

ggsave("results/graphs/robustness2_linear_sens_estimate.pdf", width = 7, height = 5)

results_l %>% 
  ggplot(aes(x = pvalue, color = newspaper, fill = newspaper)) +
  geom_density(alpha = .2) +
  scale_color_manual(values = colors) +
  scale_fill_manual(values = colors) +
  labs(x = "P-value",
       y = "Density") +
  scale_x_continuous(limits = c(0, 0.5)) + 
  theme_light()

ggsave("results/graphs/robustness2_linear_sens_pvalue.pdf", width = 7, height = 5)


etable(lm_models_robust2, dict = var_labels,
       se.row = TRUE,
       signif.code = c("***"=0.001, "**"=0.01, "*"=0.05, "."=0.10),
       tex = TRUE,
       file = "results/tables/robustness2_linear_models_robust2.tex",
       replace = TRUE)

# models_robust2 on positive  -----------------------------------------------------




pmodels_robust2 <- list()

#### Base model ####
pmodels_robust2[["Model 1"]] <- feglm(positive_b ~ newspaper*government, data = all_sentiment_robust2, family = "binomial")

summary(pmodels_robust2[["Model 1"]])



pame2 <- avg_slopes(pmodels_robust2[["Model 1"]],
                    newdata = "marginalmeans",
                    variables = "government",
                    by = "newspaper",
                    type = "response"
)

predict_response(pmodels_robust2[["Model 1"]],
                 terms = c("government", "newspaper"),
                 margin = "marginalmeans"
)

pame2 %>%
  ggplot(aes(x = newspaper,
             y = estimate,
             ymin = conf.low,
             ymax = conf.high,
             color = newspaper)) +
  geom_point() +
  geom_pointrange() +
  labs(x = "Newspaper",
       y = "AMEs (Scholz cabinet with ref. Merkel cabinet)") +
  geom_hline(yintercept = 0, color = "red") +
  scale_x_discrete(labels = c("Welt", "Zeit")) +
  scale_color_manual(values = colors) +
  theme_light()

ggsave("results/graphs/robustness2_positive_ame1.pdf", width = 7, height = 5)

#### Controls ####


pmodels_robust2[["Model 2"]] <- glm(positive_b ~ newspaper*government + infl_change_year + 
                                      unemployment_rate + qchange_gdp_pa +
                                      infl_change_month + ychange_gdp_pa + ychange_unemployment_rate, 
                                    data = all_sentiment_robust2, family = "binomial",
)

summary(pmodels_robust2[["Model 2"]])

pame2 <- avg_slopes(pmodels_robust2[["Model 2"]],
                    newdata = "marginalmeans",
                    variables = "government",
                    by = "newspaper",
                    type = "response"
)

predict_response(pmodels_robust2[["Model 2"]],
                 terms = c("government", "newspaper"),
                 margin = "marginalmeans"
)

pame2 %>%
  ggplot(aes(x = newspaper,
             y = estimate,
             ymin = conf.low,
             ymax = conf.high,
             color = newspaper)) +
  geom_point() +
  geom_pointrange() +
  labs(x = "Newspaper",
       y = "AMEs (Scholz cabinet with ref. Merkel cabinet)") +
  geom_hline(yintercept = 0, color = "red") +
  scale_x_discrete(labels = c("Welt", "Zeit")) +
  scale_color_manual(values = colors) +
  theme_light()

ggsave("results/graphs/robustness2_positive_ame2.pdf", width = 7, height = 5)


all_possible_models_robust2_p <- function(i) {
  
  fmla <- as.formula(paste0("positive_b ~ newspaper*government + ",
                            paste(all_combs[[i]], collapse = " + ")))
  
  
  model <- feglm(fmla, data = all_sentiment_robust2, 
                 family = "binomial")
  
  tame <- avg_slopes(model, newdata = "marginalmeans",
                     variables = "government", by = "newspaper",
                     type = "response")
  
  # Extract the coefficient and p-value for both 'zeit' and 'welt'
  zeit <- summary(tame)[2, c(4, 7)]  # Coefficient and p-value for 'zeit'
  welt <- summary(tame)[1, c(4, 7)]  # Coefficient and p-value for 'welt'
  
  both <- data.frame(estimate_welt = numeric(), pvalue_welt = numeric(),
                     estimate_zeit = numeric(), pvalue_zeit = numeric())
  both[1, c(1, 2)] <- welt
  both[1, c(3, 4)] <- zeit
  
  return(both)  # Combine 'welt' and 'zeit' results
}

#results <- matrix(NA, nrow = length(all_combs), ncol = 4)

plan(multisession)

results_n <- future_lapply(1:length(all_combs), function(i) {
  all_possible_models_robust2_p(i)
})

results <- do.call(rbind, results_n)



save(results, file = "results/model3_sensitivity_positiv.RData")




results_l <- results %>% 
  select(1:4) %>% 
  pivot_longer(  cols = 1:4,  # or you could specify specific columns
                 names_to = c(".value", "newspaper"),
                 names_sep = "_") %>% 
  mutate(pvalue = round(pvalue, 5),
         estimate = round(estimate, 5))



results_l %>% 
  ggplot(aes(x = estimate, color = newspaper, fill = newspaper)) +
  geom_density(alpha = .2) +
  scale_color_manual(values = colors) +
  scale_fill_manual(values = colors) +
  labs(x = "Estimate as probability",
       y = "Density") +
  #scale_x_continuous(limits = c(0, 0.15)) + 
  theme_light()

ggsave("results/graphs/robustness2_positive_sens_estimate.pdf", width = 7, height = 5)

results_l %>% 
  ggplot(aes(x = pvalue, color = newspaper, fill = newspaper)) +
  geom_density(alpha = .2) +
  scale_color_manual(values = colors) +
  scale_fill_manual(values = colors) +
  labs(x = "P-value",
       y = "Density") +
  scale_x_continuous(limits = c(0, 1),
                     breaks = c(seq(0, 1, by = 0.1))) + 
  theme_light()

ggsave("results/graphs/robustness2_positive_sens_pvalue.pdf", width = 7, height = 5)




etable(pmodels_robust2, dict = var_labels,
       se.row = TRUE,
       signif.code = c("***"=0.001, "**"=0.01, "*"=0.05, "."=0.10),
       tex = TRUE,
       file = "results/tables/robustness2_positive_models_robust2.tex",
       replace = TRUE
)





# Robustness 3: without post election period -----------------------------------------

min(all_sentiment$date)
max(all_sentiment$date)
all_sentiment_robust3 <- all_sentiment %>% 
  filter(date < "2021-08-26" | date > "2022-01-08")

models_robust3 <- list()

#### Base model ####
models_robust3[["Model 1"]] <- feglm(negative_b ~ newspaper*government, data = all_sentiment_robust3, family = "binomial")

summary(models_robust3[["Model 1"]])


predict_response(models_robust3[["Model 1"]],
                 terms = c("government", "newspaper")
)


ame1 <- avg_slopes(models_robust3[["Model 1"]],
                   newdata = "marginalmeans",
                   variables = "government",
                   by = "newspaper",
                   type = "response"
)
summary(ame1)

ame1 %>%
  ggplot(aes(x = newspaper,
             y = estimate,
             ymin = conf.low,
             ymax = conf.high,
             color = newspaper)) +
  geom_point() +
  geom_pointrange() +
  labs(x = "Newspaper",
       y = "AMEs (Scholz cabinet with ref. Merkel cabinet)") +
  geom_hline(yintercept = 0, color = "red") +
  scale_x_discrete(labels = c("Welt", "Zeit")) +
  scale_color_manual(values = colors) +
  theme_light()

ggsave("results/graphs/robustness3_negative_ame1.pdf", width = 7, height = 5)



#### Controls ####


models_robust3[["Model 2"]] <- feglm(negative_b ~ newspaper*government + infl_change_year + 
                                       unemployment_rate + qchange_gdp_pa +
                                       infl_change_month + ychange_gdp_pa + ychange_unemployment_rate, data = all_sentiment_robust3, family = "binomial")

summary(models_robust3[["Model 2"]])

ame2 <- avg_slopes(models_robust3[["Model 2"]],
                   newdata = "marginalmeans",
                   variables = "government",
                   by = "newspaper",
                   type = "response"
)

predict_response(models_robust3[["Model 2"]],
                 terms = c("government", "newspaper"),
                 margin = "marginalmeans"
)

ame2 %>%
  ggplot(aes(x = newspaper,
             y = estimate,
             ymin = conf.low,
             ymax = conf.high,
             color = newspaper)) +
  geom_point() +
  geom_pointrange() +
  labs(x = "Newspaper",
       y = "AMEs (Scholz cabinet with ref. Merkel cabinet)") +
  geom_hline(yintercept = 0, color = "red") +
  scale_x_discrete(labels = c("Welt", "Zeit")) +
  scale_color_manual(values = colors) +
  theme_light()

ggsave("results/graphs/robustness3_negative_ame2.pdf", width = 7, height = 5)



all_possible_models_robust3 <- function(i) {
  
  fmla <- as.formula(paste0("negative_b ~ newspaper*government + ",
                            paste(all_combs[[i]], collapse = " + ")))
  
  
  model <- feglm(fmla, data = all_sentiment_robust3,
                 family = "binomial"
  )
  
  tame <- avg_slopes(model, newdata = "marginalmeans",
                     variables = "government", by = "newspaper",
                     type = "response")
  
  # Extract the coefficient and p-value for both 'zeit' and 'welt'
  zeit <- summary(tame)[2, c(4, 7)]  # Coefficient and p-value for 'zeit'
  welt <- summary(tame)[1, c(4, 7)]  # Coefficient and p-value for 'welt'
  
  both <- data.frame(estimate_welt = numeric(), pvalue_welt = numeric(),
                     estimate_zeit = numeric(), pvalue_zeit = numeric())
  both[1, c(1, 2)] <- welt
  both[1, c(3, 4)] <- zeit
  
  return(both)  # Combine 'welt' and 'zeit' results
}

#results <- matrix(NA, nrow = length(all_combs), ncol = 4)

plan(multisession)

results_n <- future_lapply(1:length(all_combs), function(i) {
  all_possible_models_robust3(i)
})

results <- do.call(rbind, results_n)


save(results, file = "results/robustness3_model3_sensitivity_negative.RData")
#load("results/model3_sensitivity3.RData")


results_l <- results %>% 
  select(1:4) %>% 
  pivot_longer(  cols = 1:4,  # or you could specify specific columns
                 names_to = c(".value", "newspaper"),
                 names_sep = "_")

results_l %>% 
  ggplot(aes(x = estimate, color = newspaper, fill = newspaper)) +
  geom_density(alpha = .2) +
  scale_color_manual(values = colors) +
  scale_fill_manual(values = colors) +
  labs(x = "Estimate as probability",
       y = "Density") +
  # scale_x_continuous(limits = c(0, 0.15)) + 
  theme_light()

ggsave("results/graphs/robustness3_negative_sens_estimate.pdf", width = 7, height = 5)

results_l %>% 
  ggplot(aes(x = pvalue, color = newspaper, fill = newspaper)) +
  geom_density(alpha = .2) +
  scale_color_manual(values = colors) +
  scale_fill_manual(values = colors) +
  labs(x = "P-value",
       y = "Density") +
  scale_x_continuous(limits = c(0, 0.2),
                     breaks = c(seq(0, 0.2, by = 0.05))) + 
  theme_light()

ggsave("results/graphs/robustness3_negative_sens_pvalue.pdf", width = 7, height = 5)

etable(models_robust3)

etable(models_robust3, dict = var_labels,
       se.row = TRUE,
       signif.code = c("***"=0.001, "**"=0.01, "*"=0.05, "."=0.10),
       tex = TRUE,
       file = "results/tables/robustness3_negative_models_robust3.tex",
       replace = TRUE
)


# linear model ------------------------------------------------------------

lillie.test(all_sentiment_robust3$sentiment_n)
ad.test(all_sentiment_robust3$sentiment_n)


all_sentiment_robust3 %>% 
  ggplot() +
  geom_density(aes(x = sentiment_n)) +
  scale_x_continuous(limits = c(0, 10),
                     breaks = c(seq(0, 10, by = 1))) +
  theme_light()




lm_models_robust3 <- list()

#### Base model ####
lm_models_robust3[["Model 1"]] <- feols(sentiment_n ~ newspaper*government, data = all_sentiment_robust3,
                                        vcov = "hetero"
)

summary(lm_models_robust3[["Model 1"]])


ht1 <- hypothesis_test(lm_models_robust3[["Model 1"]], c("newspaper", "government"))




ht1 %>% 
  filter(newspaper %in% c("welt-welt", "zeit-zeit")) %>% 
  mutate(newspaper = substr(newspaper, 1, 4)) %>% 
  ggplot(aes(x = newspaper, 
             y = Contrast*-1,
             ymin = conf.low*-1,
             ymax = conf.high*-1,
             color = newspaper)) +
  geom_point() +
  geom_pointrange() +
  labs(x = "Newspaper",
       y = "Estimates (Scholz cabinet with ref. Merkel cabinet)") +
  geom_hline(yintercept = 0, color = "red") +
  scale_x_discrete(labels = c("Welt", "Zeit")) +
  scale_color_manual(values = colors) +
  theme_light()

ggsave("results/graphs/robustness3_linear_estimate1.pdf", width = 7, height = 5)

predict_response(lm_models_robust3[["Model 1"]],
                 terms = c("government", "newspaper")
)




#### Controls ####

lm_models_robust3[["Model 2"]] <- feols(sentiment_n ~ newspaper*government + infl_change_year + 
                                          unemployment_rate + qchange_gdp_pa +
                                          infl_change_month + ychange_gdp_pa + ychange_unemployment_rate, data = all_sentiment_robust3,
                                        vcov = "hetero"
)

summary(lm_models_robust3[["Model 2"]])


ht2 <- hypothesis_test(lm_models_robust3[["Model 2"]], c("newspaper", "government"))



ht2 %>% 
  filter(newspaper %in% c("welt-welt", "zeit-zeit")) %>% 
  mutate(newspaper = substr(newspaper, 1, 4)) %>% 
  ggplot(aes(x = newspaper, 
             y = Contrast*-1,
             ymin = conf.low*-1,
             ymax = conf.high*-1,
             color = newspaper)) +
  geom_point() +
  geom_pointrange() +
  labs(x = "Newspaper",
       y = "Estimates (Scholz cabinet with ref. Merkel cabinet)") +
  geom_hline(yintercept = 0, color = "red") +
  scale_x_discrete(labels = c("Welt", "Zeit")) +
  scale_color_manual(values = colors) +
  theme_light()

ggsave("results/graphs/robustness3_linear_estimate2.pdf", width = 7, height = 5)



all_possible_models_robust3_l <- function(i) {
  
  fmla <- as.formula(paste0("sentiment_n ~ newspaper*government + ",
                            paste(all_combs[[i]], collapse = " + ")))
  
  
  model <- feols(fmla, data = all_sentiment_robust3,
                 vcov = "hetero")
  
  ht <- hypothesis_test(model, c("newspaper", "government")) %>% 
    filter(newspaper %in% c("welt-welt", "zeit-zeit")) %>% 
    mutate(newspaper = substr(newspaper, 1, 4),
           Contrast = Contrast *-1)
  
  zeit <- ht[2, c(3, 6)]
  welt <- ht[1, c(3, 6)]
  
  both <- data.frame(estimate_welt = numeric(), pvalue_welt = numeric(),
                     estimate_zeit = numeric(), pvalue_zeit = numeric())
  both[1, c(1, 2)] <- welt
  both[1, c(3, 4)] <- zeit
  
  return(both)  # Combine 'welt' and 'zeit' results
}


plan(multisession)

results_n <- future_lapply(1:length(all_combs), function(i) {
  all_possible_models_robust3_l(i)
})

results <- do.call(rbind, results_n)





save(results, file = "results/lm_model3_sensitivity3.RData")
#load("results/lm_model3_sensitivity3.RData")



results_l <- results %>% 
  select(1:4) %>% 
  pivot_longer(  cols = 1:4,  # or you could specify specific columns
                 names_to = c(".value", "newspaper"),
                 names_sep = "_")

results_l %>% 
  ggplot(aes(x = estimate, color = newspaper, fill = newspaper)) +
  geom_density(alpha = .2) +
  scale_color_manual(values = colors) +
  scale_fill_manual(values = colors) +
  labs(x = "Estimate",
       y = "Density") +
  #scale_x_continuous(limits = c(0, 0.15)) + 
  theme_light()

ggsave("results/graphs/robustness3_linear_sens_estimate.pdf", width = 7, height = 5)

results_l %>% 
  ggplot(aes(x = pvalue, color = newspaper, fill = newspaper)) +
  geom_density(alpha = .2) +
  scale_color_manual(values = colors) +
  scale_fill_manual(values = colors) +
  labs(x = "P-value",
       y = "Density") +
  scale_x_continuous(limits = c(0, 0.5)) + 
  theme_light()

ggsave("results/graphs/robustness3_linear_sens_pvalue.pdf", width = 7, height = 5)


etable(lm_models_robust3, dict = var_labels,
       se.row = TRUE,
       signif.code = c("***"=0.001, "**"=0.01, "*"=0.05, "."=0.10),
       tex = TRUE,
       file = "results/tables/robustness3_linear_models_robust3.tex",
       replace = TRUE)

# models_robust3 on positive  -----------------------------------------------------




pmodels_robust3 <- list()

#### Base model ####
pmodels_robust3[["Model 1"]] <- feglm(positive_b ~ newspaper*government, data = all_sentiment_robust3, family = "binomial")

summary(pmodels_robust3[["Model 1"]])


pame2 <- avg_slopes(pmodels_robust3[["Model 1"]],
                    newdata = "marginalmeans",
                    variables = "government",
                    by = "newspaper",
                    type = "response",
                    #hypothesis = "pairwise"
)

predict_response(pmodels_robust3[["Model 1"]],
                 terms = c("government", "newspaper"),
                 margin = "marginalmeans"
)

pame2 %>%
  ggplot(aes(x = newspaper,
             y = estimate,
             ymin = conf.low,
             ymax = conf.high,
             color = newspaper)) +
  geom_point() +
  geom_pointrange() +
  labs(x = "Newspaper",
       y = "AMEs (Scholz cabinet with ref. Merkel cabinet)") +
  geom_hline(yintercept = 0, color = "red") +
  scale_x_discrete(labels = c("Welt", "Zeit")) +
  scale_color_manual(values = colors) +
  theme_light()

ggsave("results/graphs/robustness3_positive_ame1.pdf", width = 7, height = 5)

#### Controls ####

pmodels_robust3[["Model 2"]] <- glm(positive_b ~ newspaper*government + infl_change_year + 
                                      unemployment_rate + qchange_gdp_pa +
                                      infl_change_month + ychange_gdp_pa + ychange_unemployment_rate, 
                                    data = all_sentiment_robust3, family = "binomial",
)

summary(pmodels_robust3[["Model 2"]])

pame2 <- avg_slopes(pmodels_robust3[["Model 2"]],
                    newdata = "marginalmeans",
                    variables = "government",
                    by = "newspaper",
                    type = "response",
                    #hypothesis = "pairwise"
)

predict_response(pmodels_robust3[["Model 2"]],
                 terms = c("government", "newspaper"),
                 margin = "marginalmeans"
)

pame2 %>%
  ggplot(aes(x = newspaper,
             y = estimate,
             ymin = conf.low,
             ymax = conf.high,
             color = newspaper)) +
  geom_point() +
  geom_pointrange() +
  labs(x = "Newspaper",
       y = "AMEs (Scholz cabinet with ref. Merkel cabinet)") +
  geom_hline(yintercept = 0, color = "red") +
  scale_x_discrete(labels = c("Welt", "Zeit")) +
  scale_color_manual(values = colors) +
  theme_light()

ggsave("results/graphs/robustness3_positive_ame2.pdf", width = 7, height = 5)



all_possible_models_robust3_p <- function(i) {
  
  fmla <- as.formula(paste0("positive_b ~ newspaper*government + ",
                            paste(all_combs[[i]], collapse = " + ")))
  
  
  model <- feglm(fmla, data = all_sentiment_robust3,
                 family = "binomial")
  
  tame <- avg_slopes(model, newdata = "marginalmeans",
                     variables = "government", by = "newspaper",
                     type = "response")
  
  # Extract the coefficient and p-value for both 'zeit' and 'welt'
  zeit <- summary(tame)[2, c(4, 7)]  # Coefficient and p-value for 'zeit'
  welt <- summary(tame)[1, c(4, 7)]  # Coefficient and p-value for 'welt'
  
  both <- data.frame(estimate_welt = numeric(), pvalue_welt = numeric(),
                     estimate_zeit = numeric(), pvalue_zeit = numeric())
  both[1, c(1, 2)] <- welt
  both[1, c(3, 4)] <- zeit
  
  return(both)  # Combine 'welt' and 'zeit' results
}

#results <- matrix(NA, nrow = length(all_combs), ncol = 4)

plan(multisession)

results_n <- future_lapply(1:length(all_combs), function(i) {
  all_possible_models_robust3_p(i)
})

results <- do.call(rbind, results_n)



save(results, file = "results/model3_sensitivity_positiv.RData")




results_l <- results %>% 
  select(1:4) %>% 
  pivot_longer(  cols = 1:4,  # or you could specify specific columns
                 names_to = c(".value", "newspaper"),
                 names_sep = "_") %>% 
  mutate(pvalue = round(pvalue, 5),
         estimate = round(estimate, 5))



results_l %>% 
  ggplot(aes(x = estimate, color = newspaper, fill = newspaper)) +
  geom_density(alpha = .2) +
  scale_color_manual(values = colors) +
  scale_fill_manual(values = colors) +
  labs(x = "Estimate as probability",
       y = "Density") +
  #scale_x_continuous(limits = c(0, 0.15)) + 
  theme_light()

ggsave("results/graphs/robustness3_positive_sens_estimate.pdf", width = 7, height = 5)

results_l %>% 
  ggplot(aes(x = pvalue, color = newspaper, fill = newspaper)) +
  geom_density(alpha = .2) +
  scale_color_manual(values = colors) +
  scale_fill_manual(values = colors) +
  labs(x = "P-value",
       y = "Density") +
  scale_x_continuous(limits = c(0, 1),
                     breaks = c(seq(0, 1, by = 0.1))) + 
  theme_light()

ggsave("results/graphs/robustness3_positive_sens_pvalue.pdf", width = 7, height = 5)




etable(pmodels_robust3, dict = var_labels,
       se.row = TRUE,
       signif.code = c("***"=0.001, "**"=0.01, "*"=0.05, "."=0.10),
       tex = TRUE,
       file = "results/tables/robustness3_positive_models_robust3.tex",
       replace = TRUE
)



# Other Explanation -------------------------------------------------------


other_expl <- list()
## inflation
other_expl[["Inflation"]] <- glm(negative_b ~ infl_change_year*newspaper*government
          + unemployment_rate 
          + ychange_gdp_pa,
          data = all_sentiment,
          family = binomial("logit"))

summary(other_expl[["Inflation"]])


ame_oe <- avg_slopes(other_expl[["Inflation"]],
                    newdata = "marginalmeans",
                    variables = c("infl_change_year"),
                    #by = c("government", "newspaper"),
                    by = c("newspaper", "government"),
                    type = "response",
                    hypothesis = "pairwise"
)
ame_oe

plot_model(other_expl[["Inflation"]], 
           type = "pred", 
           terms = c("infl_change_year", "newspaper", "government"),
           colors = colors) +
  labs(x = "Yearly inflation change",
       y = "Probability of a negative article",
       title = "")

ggsave("results/graphs/other_exp_perc_inflation.pdf", width = 8, height = 5)


## gdp
other_expl[["GDP"]] <- glm(negative_b ~ ychange_gdp_pa*newspaper*government
                                 + unemployment_rate 
                                 + infl_change_year,
                                 data = all_sentiment,
                                 family = binomial("logit"))

summary(other_expl[["GDP"]])


ame_oe12 <- avg_slopes(other_expl[["GDP"]],
                     newdata = "marginalmeans",
                     variables = c("ychange_gdp_pa"),
                     #by = c("government", "newspaper"),
                     by = c("newspaper", "government"),
                     type = "response",
                     hypothesis = "pairwise"
)
ame_oe12

plot_model(other_expl[["GDP"]], 
           type = "pred", 
           terms = c("ychange_gdp_pa", "newspaper", "government"),
           colors = colors) +
  labs(x = "Yearly GDP change (price adjusted)",
       y = "Probability of a negative article",
       title = "")

ggsave("results/graphs/other_exp_perc_gdp.pdf", width = 8, height = 5)

## Time Fixed Effect

other_expl[["Time Fixed Effect"]] <- lm_robust(sentiment_n ~ newspaper*government
                                 + infl_change_year
                                 + unemployment_rate 
                                 + ychange_gdp_pa
                                 + day_count,
                                 data = all_sentiment,
                                # family = binomial("logit")
                                 )

summary(other_expl[["Time Fixed Effect"]])

ht_oe <- hypothesis_test(other_expl[["Time Fixed Effect"]], c("newspaper", "government"))


ht_oe %>% 
  filter(newspaper %in% c("welt-welt", "zeit-zeit")) %>% 
  mutate(newspaper = substr(newspaper, 1, 4)) %>% 
  ggplot(aes(x = newspaper, 
             y = Contrast*-1,
             ymin = conf.low*-1,
             ymax = conf.high*-1,
             color = newspaper)) +
  geom_point() +
  geom_pointrange() +
  labs(x = "Newspaper",
       y = "Estimates (Scholz cabinet with ref. Merkel cabinet)") +
  geom_hline(yintercept = 0, color = "red") +
  scale_x_discrete(labels = c("Welt", "Zeit")) +
  scale_color_manual(values = colors) +
  theme_light()

ggsave("results/graphs/other_expl_timefixed.pdf", width = 8, height = 6)

all_sentiment_pd <- all_sentiment %>% 
  group_by(date, newspaper) %>% 
  mutate(n_negative = sum(negative_b, na.rm = TRUE),
         number_articles = n()) %>% 
  select(n_negative, government, newspaper, date, controls_all, number_articles) %>% 
  distinct()

## inflation
other_expl[["Number Inflation"]] <- lm_robust(number_articles ~ infl_change_year*newspaper*government
                                         + change_unemployment_rate 
                                         + ychange_gdp_pa,
                                         data = all_sentiment_pd)

summary(other_expl[["Number Inflation"]])

ame_oe2 <- avg_slopes(other_expl[["Number Inflation"]],
                     newdata = "marginalmeans",
                     variables = c("infl_change_year"),
                     #by = c("government", "newspaper"),
                     by = c("newspaper", "government"),
                     type = "response",
                     hypothesis = "pairwise"
)
ame_oe2

plot_model(other_expl[["Number Inflation"]], 
           type = "pred", 
           terms = c("infl_change_year", "newspaper", "government"),
           colors = colors) +
  labs(x = "Yearly inflation change",
       y = "Number of articles per day",
       title = "")

ggsave("results/graphs/other_exp_n_inflation.pdf", width = 8, height = 5)

## gdp
other_expl[["Number GDP"]] <- lm_robust(number_articles ~ ychange_gdp_pa*newspaper*government
                                              + change_unemployment_rate 
                                              + infl_change_year,
                                              data = all_sentiment_pd)

summary(other_expl[["Number GDP"]])


avg_slopes(other_expl[["Number GDP"]],
           newdata = "marginalmeans",
           variables = c("ychange_gdp_pa"),
           #by = c("government", "newspaper"),
           by = c("newspaper", "government"),
           type = "response",
          # hypothesis = "pairwise"
)
ame_oe3 <- avg_slopes(other_expl[["Number GDP"]],
                      newdata = "marginalmeans",
                      variables = c("ychange_gdp_pa"),
                      #by = c("government", "newspaper"),
                      by = c("newspaper", "government"),
                      type = "response",
                      hypothesis = "pairwise"
)
ame_oe3

plot_model(other_expl[["Number GDP"]], 
           type = "pred", 
           terms = c("ychange_gdp_pa", "newspaper", "government"),
           colors = colors) +
  labs(x = "Yearly GDP change (price adjusted)",
       y = "Number of articles per day",
       title = "")

ggsave("results/graphs/other_exp_n_gdp.pdf", width = 8, height = 5)

## unemployment
other_expl[["Number Unemp"]] <- lm_robust(number_articles ~ change_unemployment_rate*newspaper*government
                                        + ychange_gdp_pa 
                                        + infl_change_year,
                                        data = all_sentiment_pd)

summary(other_expl[["Number Unemp"]])

plot_model(other_expl[["Number Unemp"]], 
           type = "pred", 
           terms = c("change_unemployment_rate", "newspaper", "government"),
           colors = colors) +
  labs(x = "Monthly change in unemployment rate",
       y = "Number of negative articles per day",
       title = "")

ggsave("results/graphs/other_exp_n_unempl.pdf", width = 8, height = 5)


# Diagnostics for linear models -------------------------------------------
lillie.test(all_sentiment$sentiment_n)
ad.test(all_sentiment$sentiment_n)

all_sentiment %>% 
  ggplot() +
  geom_density(aes(x = sentiment_n)) +
  scale_x_continuous(limits = c(0, 10),
                     breaks = c(seq(0, 10, by = 1))) +
  theme_light()




diagm <- stats::lm(sentiment_n ~ newspaper*government + infl_change_year + 
                                  unemployment_rate + qchange_gdp_pa +
                                  infl_change_month + ychange_gdp_pa + ychange_unemployment_rate,
                   data = all_sentiment)
summary(diagm)
slm <- summary(diagm)
# error term normally distributed?
res <- resid(diagm)
qqnorm(res)
qqline(res) 
bptest(diagm)

pdf(file = "results/graphs/diag_error_distrib.pdf", width = 8, height = 6)
density(res) %>% plot()
dev.off()
hist(res)
plot(fitted(diagm), res)
lillie.test(res)
# doesn't look too bad but errors are very significantly not normally distributed

# other diagnostics (outliers)
pdf(file = "results/graphs/diag_plots.pdf", width = 8, height = 8,
    pointsize = 26)
autoplot(diagm)

dev.off()
# looking at outliers
tt <- all_sentiment[c(190, 2538), ] # extraordinary detected

# variance influence factor
vif(diagm)


