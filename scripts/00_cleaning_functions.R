library(tibble)
library(dplyr)
library(lubridate)
library(MASS)
library(janitor)

mock_survey_data <- readRDS("C:/Users/otool/Desktop/r_directory/survey_research_ops/data/simulated/mock_survey_data.rds")

# A function to deduplicate by email, taking the earliest response
deduplicate_by_email <- function(raw_data, date_var, email_var) {
  
  raw_data %>%
    filter(!is.na({{ email_var }})) %>%
    arrange({{ email_var }}, {{ date_var }}) %>%
    distinct({{ email_var }}, .keep_all = TRUE) %>% 
    clean_names()
  
}

deduplicated_data <- deduplicate_by_email(mock_survey_data, timestamp, q_email)