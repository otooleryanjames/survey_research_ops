library(tibble)
library(dplyr)
library(lubridate)
library(MASS)
library(janitor)
library(tidyr)
library(here)

qa_data <- readRDS(
  here("data", "processed", "cleaned_qaflag_survey_data.rds")
)

dictionary <- read.csv(
  here("documents", "data_dictionary.csv")
)

summarize_scales <- function(data, dictionary){
  
  battery_table <- dictionary %>%
    filter(type == "likert") %>%
    dplyr::select(variable, scale_group)
  
  battery_names <- unique(battery_table$scale_group)

  for (b in battery_names) {
    
    items <- battery_table %>%
      filter(scale_group == b) %>%
      pull(variable)
    
    scale_data <- data %>% 
      select(all_of(items))
  }
  
  return(battery_table)
  return(scale_data)
}

summarize_scales(qa_data, dictionary)
