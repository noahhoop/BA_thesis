### Master file ###
# Bachelor thesis scripts by Noah Badenhoop

# Download only analysis data here (scripts 12, 13, 17): https://drive.google.com/file/d/1xla26n4H3VtglMcmasxV3UcDI4kAjDlt/view?usp=drive_link

# If you have trouble loading some of the packages make sure to update your installed packages, especially tidyverse
if (!require("pacman")) install.packages("pacman")
packages <- c(
  "tidyverse", "rollama", "arrow", "furrr", "progressr", "purrr", "rvest", "xml2", "dplyr", 
  "tidyr", "stringr", "readr", "httr", "openai", "jsonlite", "caret", "xtable", "irr", 
  "magrittr", "R.utils", "knitr", "corrr", "deeplr", "psych", "rio", "fixest", "Hmisc", 
  "scales", "plm", "pglm", "labelled", "modelsummary", "flextable", "texreg", "stargazer", 
  "sandwich", "lmtest", "estimatr", "marginaleffects", "ggeffects", "margins", "car", 
  "emmeans", "miceadds", "sjPlot", "haven", "foreign", "boot", "nortest", "imputeTS", 
  "future.apply", "ggfortify", "jtools"
)

pacman::p_load(char = packages)

# 1. Webscraping ----------------------------------------------------------

### These scripts download all articles from the newswebsites
## Attention: this takes many hours
scrape_scripts <- list.files("scripts_final/1_scrape_articles", full.names = TRUE)

for (s in scrape_scripts) {
  source(s)
}


# 2. Clean raw data -------------------------------------------------------

### This script combines the data from all newspapers and does a first filter for relevant keywords

source("scripts_final/2_filter_raw_data.R")


# 3. Clean data -----------------------------------------------------------

### This script further cleans the data and creates the first sample for text classification

source("scripts_final/3_clean_newspapers.R")

# After this script I manually coded the sample

# 4. Classify sample with LLM ---------------------------------------------

### This script runs the LLM to classify the sample

## Attention: this needs the ollama software installed and then the script would take hours on a normal computer

source("scripts_final/4_classify_sample.R")


# 5. ChatGPT classify sample ----------------------------------------------

### This script uses the ChatGPT API to classify the sample

## Attention: only usable with an API Key

source("scripts_final/5_chatgpt_sample_filter.R")

# 6. Final classification -------------------------------------------------

### This script runs the classification on the full data

## Attention: this needs the ollama software installed and then the script would take days/weeks on a normal computer

source("scripts_final/6_classify_final.R")


# 7. Validation and handcoding preparation --------------------------------

### This script has the validation of the classification on the sample
### and then saves another sample of documents for the next handcoding

source("scripts_final/7_handcoding.R")

# After this script I handcoded the single documents

# 8. Sample sentiment analysis -----------------------------------------------

### This script runs the sentiment analysis on the sample

## Attention: this needs the ollama software installed and then the script would take hours on a normal computer

source("scripts_final/8_classify_sentiment_sample_best.R")

# 9. Validation of sentiment sample ---------------------------------------

### This script calculates the sentiment index from the handcoded data and then runs the validation

source("scripts_final/9_validation_tonality.R")

# 10. Final sentiment analysis --------------------------------------------

### This script runs the sentiment analysis on the full data

## Attention: this needs the ollama software installed and then the script would take days/weeks on a normal computer

source("scripts_final/10_classify_sentiment_final.R")


# 11. ChatGPT sentiment analysis ------------------------------------------

### This script runs the sentiment analysis on the sample with ChatGPT

## Attention: only usable with an API Key

source("scripts_final/11_chatgpt_sentiment.R")


# 12. Validation of text analysis -------------------------------------------------------

### This script contains the validation of the text analysis of filtering the relevant articles and the sentiment analysis

source("scripts_final/12_text_analysis_validation.R")


# 13. Main analysis -------------------------------------------------------

### This script prepares the data and contains the main analysis

source("scripts_final/13_main_analysis.R")


# 14. Party quotes sample -------------------------------------------------

### This script creates the sample for the party quote detection

source("scripts_final/14_party_quote_sample1.R")

# After this script I handcoded the party quotes


# 15. Party quote detection sample ----------------------------------------

### This script runs the party quote detection on the sample

source("scripts_final/15_party_quote_sample2.R")


# 16. Party quote detection final -----------------------------------------

### This script runs the party quote detection on the full sample

## Attention: this needs the ollama software installed and then the script would take days/weeks on a normal computer

source("scripts_final/16_party_quote_final.R")


# 17. Party quote validation and analysis ---------------------------------

### This script contains the validation of the party quote detection and runs the analysis

source("scripts_final/17_party_quotes.R")

