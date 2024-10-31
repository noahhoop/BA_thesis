library(tidyverse)
library(readr)
library(caret)
library(xtable)
library(knitr)
library(corrr)
library(deeplr)
library(httr)
library(psych)
library(irr)
options(scipen = 999)



vali_sample <- data.frame()


docs <- list.files(path = "data/final/article_docs_coded", full.names = TRUE)


numeric_values <- as.numeric(str_extract(docs, "(?<=doc_)\\d+"))

# Get the order of numeric values
new_order <- order(numeric_values)

# Sort files based on the Windows order
docs <- docs[new_order]



for (i in 1:length(docs)) {

  temp <- read_delim(docs[i], delim = ";")
  

  
  temp_df <- data.frame(doc = i,
                        body = paste0(temp$sentence, collapse = " "),
                        n_pos = sum(temp$tone == "1", na.rm = TRUE),
                        n_neg = sum(temp$tone == "-1", na.rm = TRUE),
                        n_neu = sum(temp$tone == "0", na.rm = TRUE),
                        n_irr = sum(temp$tone == "I", na.rm = TRUE)) %>% 
    rowwise() %>% 
    mutate(n_tot = sum(c_across(starts_with("n_"))),
           n_rel = n_tot - n_irr)
  
  vali_sample <- bind_rows(vali_sample, temp_df)
}



vali_sample <- vali_sample %>% 
  mutate(prop_scale = n_pos/n_rel - n_neg/n_rel,
         ratio_scale = (n_pos - n_neg)/(n_pos + n_neg),
         logit_scale = log(n_pos + 0.5) - log(n_neg + 0.5))


vali_sample %>% 
  select(ends_with("scale")) %>% 
  correlate() %>% 
  shave()

hist(vali_sample$prop_scale)
hist(vali_sample$ratio_scale)
hist(vali_sample$logit_scale)



vali_sample <- vali_sample %>% 
  mutate(ratio_scale = ifelse(n_neu == n_rel & n_rel > 0, 0, ratio_scale)) %>% 
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

table(vali_sample$logit_scale_c, exclude = NULL)
table(vali_sample$ratio_scale_c, exclude = NULL)
table(vali_sample$prop_scale_c, exclude = NULL)


vali_sample %>% 
  select(ends_with("scale_c")) %>% 
  mutate(across(everything(),
                ~ as.numeric(.))) %>% 
  correlate(method = "kendall") %>% 
  shave()

plot(vali_sample$prop_scale_c)
plot(vali_sample$ratio_scale_c)
plot(vali_sample$logit_scale_c)




save(vali_sample, file = "data/final/vali_sample.RData")

nchar(vali_sample$body) %>% sum()



