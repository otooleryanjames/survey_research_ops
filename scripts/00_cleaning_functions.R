library(tibble)
library(dplyr)
library(lubridate)
library(MASS)
library(janitor)
library(tidyr)

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

dictionary <- read.csv("C:/Users/otool/Desktop/r_directory/survey_research_ops/documents/data_dictionary.csv")














# Create lookup table
likert_lookup <- dictionary %>%
  filter(type == "likert") %>%
  mutate(
    # Replace commas separating choices with a temporary delimiter
    levels = gsub(",(?=\\s*\\d+\\s*=)", "|DELIM|", levels, perl = TRUE)
  ) %>%
  separate_rows(levels, sep = "\\|DELIM\\|") %>%
  separate(levels, into = c("value", "choice"), sep = " = ") %>%
  mutate(value = trimws(value), choice = trimws(choice))



convert_likert_to_factor <- function(data, question_name, lookup) {
  
  # Extract mapping for this question
  mapping <- lookup %>%
    filter(variable == question_name) %>%
    arrange(as.numeric(value))
  
  # Map numeric code (stored as character) to the text choice
  data[[question_name]] <- mapping$choice[match(data[[question_name]], mapping$value)]
  
  # Convert to ordered factor
  data[[question_name]] <- factor(data[[question_name]],
                                  levels = mapping$choice,
                                  ordered = TRUE)
  
  return(data)
}


# All Likert variable names
likert_vars <- dictionary %>%
  filter(type == "likert") %>%
  pull(variable)

# Loop through all Likert columns
for (q in likert_vars) {
  mock_survey_data <- convert_likert_to_factor(mock_survey_data, q, likert_lookup)
}



# 1. Create a lookup table for single_select questions
single_select_lookup <- dictionary %>%
  filter(type == "single_select") %>%
  mutate(
    # Replace commas separating choices with a temporary delimiter
    levels = gsub(",(?=\\s*\\d+\\s*=)", "|DELIM|", levels, perl = TRUE)
  ) %>%
  separate_rows(levels, sep = "\\|DELIM\\|") %>%
  separate(levels, into = c("value", "choice"), sep = " = ") %>%
  mutate(value = trimws(value), choice = trimws(choice))

# 2. Function to convert single_select column to factor
convert_single_select_to_factor <- function(data, question_name, lookup) {
  
  mapping <- lookup %>% filter(variable == question_name)
  
  # Map numeric codes (stored as character) to text choices
  data[[question_name]] <- mapping$choice[match(data[[question_name]], mapping$value)]
  
  # Convert to factor (not ordered)
  data[[question_name]] <- factor(data[[question_name]],
                                  levels = mapping$choice)
  
  return(data)
}

# 3. All single_select variable names
single_select_vars <- dictionary %>%
  filter(type == "single_select") %>%
  pull(variable)

# 4. Apply conversion to all single_select columns
for (q in single_select_vars) {
  mock_survey_data <- convert_single_select_to_factor(mock_survey_data, q, single_select_lookup)
}

# Function to enforce variable types based on dictionary
# enforce_types <- function(data, dict) {
#   
#   for (i in seq_len(nrow(dict))) {
#     var <- dict$variable[i]
#     type <- dict$type[i]
#     
#     if (!var %in% names(data)) next  # skip if variable not in data
#     
#     # Force types
#     if (type == "numeric") {
#       data[[var]] <- as.numeric(data[[var]])
#     } else if (type %in% c("character", "multi_select")) {
#       data[[var]] <- as.character(data[[var]])
#     } else if (type %in% c("single_select", "likert")) {
#       # For factors, leave conversion to your existing functions
#       # or convert to factor here if you want
#       data[[var]] <- as.character(data[[var]])  # keep as char for now for later factor conversion
#     }
#   }
#   
#   return(data)
# }

# Apply
mock_survey_data <- enforce_types(mock_survey_data, dictionary)