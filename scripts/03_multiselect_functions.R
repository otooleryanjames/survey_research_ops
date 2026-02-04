library(tibble)
library(dplyr)
library(lubridate)
library(janitor)
library(tidyr)
library(here)
library(ggplot2)
library(scales)
library(forcats)
library(purrr)

qa_data <- readRDS(
  here("data", "processed", "cleaned_qaflag_survey_data.rds")
)

dictionary <- read.csv(
  here("documents", "data_dictionary.csv")
)

multi_select_vars <- dictionary %>% 
  filter(type == "multi_select") %>% 
  mutate(variable = sub("_.*", "", variable))
