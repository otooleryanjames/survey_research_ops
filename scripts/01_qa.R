library(tibble)
library(dplyr)
library(lubridate)
library(MASS)
library(janitor)
library(tidyr)
library(here)


clean_data <- readRDS(
  here("data", "processed", "cleaned_survey_data.rds")
)

dictionary <- read.csv(
  here("documents", "data_dictionary.csv")
)

### Time-based flags
speed_time <- median(clean_data$time_to_complete) - 3 * mad(clean_data$time_to_complete)

#but what does this mean for people who were just in the survey for a short time...?
clean_data_time_flag <- clean_data %>% 
  mutate(
    flag_speeder_time = case_when(
      time_to_complete < speed_time ~ 1,
      TRUE ~ 0
    )
  )

### Attention-based flags
## Function with variable, and required response?

flag_attention_checks <- function(data, dictionary) {
  
  # Get all attention check questions
  attention_table <- dictionary %>%
    filter(qa_role == "attention_check") %>%
    mutate(
      # Handle comma-separated levels
      levels = gsub(",(?=\\s*\\d+\\s*=)", "|DELIM|", levels, perl = TRUE)
    ) %>%
    separate_rows(levels, sep = "\\|DELIM\\|") %>%
    separate(levels, into = c("value", "choice"), sep = " = ") %>%
    mutate(value = trimws(value), choice = trimws(choice))
  
  flagged_data <- data
  
  # Loop through each attention check question
  for (q in unique(attention_table$variable)) {
    
    # Get the correct response (usually the one the instructions say to pick)
    correct_response <- attention_table %>%
      filter(variable == q) %>%
      slice(1) %>%  # assume first listed value is the correct one
      pull(choice)
    
    # Create a new column name for the flag
    flag_col <- paste0(q, "_fail")
    
    # Flag failures: 0 = correct, 1 = incorrect
    flagged_data <- flagged_data %>%
      mutate(
        !!flag_col := ifelse(.data[[q]] == correct_response, 0, 1)
      )
  }
  
  return(flagged_data)
}


survey_data_flagged <- flag_attention_checks(clean_data_time_flag, dictionary)



###
