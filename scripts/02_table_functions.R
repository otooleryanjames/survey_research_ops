library(tibble)
library(dplyr)
library(lubridate)
library(janitor)
library(tidyr)
library(here)

qa_data <- readRDS(
  here("data", "processed", "cleaned_qaflag_survey_data.rds")
)

dictionary <- read.csv(
  here("documents", "data_dictionary.csv")
)

battery_table <- dictionary %>%
  dplyr::filter(type == "likert") %>%
  dplyr::select(variable, scale_group)

satisfaction_vars <- battery_table %>%
  filter(scale_group == "satisfaction") %>%
  pull(variable)

satisfaction_data <- dplyr::select(qa_data, all_of(satisfaction_vars))

satisfaction_percent <- satisfaction_data %>%
  pivot_longer(cols = everything(), names_to = "item", values_to = "response") %>%
  group_by(item, response) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(item) %>%
  mutate(percent = n / sum(n) * 100) %>%
  dplyr::select(-n) %>%
  pivot_wider(names_from = item, values_from = percent, values_fill = 0)

satisfaction_percent