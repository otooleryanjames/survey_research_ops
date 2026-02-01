library(tibble)
library(dplyr)
library(lubridate)
library(MASS)
library(janitor)
library(tidyr)
library(here)

survey_data <- readRDS(
  here("data", "simulated", "mock_survey_data.rds")
)

dictionary <- read.csv(
  here("documents", "data_dictionary.csv")
)


# A function to deduplicate by email, taking the earliest response
deduplicate_by_email <- function(raw_data, date_var, email_var) {
  
  raw_data %>%
    filter(!is.na({{ email_var }})) %>%
    arrange({{ email_var }}, {{ date_var }}) %>%
    distinct({{ email_var }}, .keep_all = TRUE) %>% 
    clean_names()
  
}

deduplicated_data <- deduplicate_by_email(survey_data, timestamp, q_email)













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
survey_data_recoded1 <- deduplicated_data
for (q in likert_vars) {
  survey_data_recoded1 <- convert_likert_to_factor(survey_data_recoded1, q, likert_lookup)
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
  
  # Extract mapping for this question
  mapping <- lookup %>% filter(variable == question_name)
  
  # If all values in the column are numeric codes (as character), map to choice text
  if (all(data[[question_name]] %in% mapping$value | is.na(data[[question_name]]))) {
    data[[question_name]] <- mapping$choice[match(data[[question_name]], mapping$value)]
  } else {
    # Otherwise, assume data already contains text values
    missing_vals <- setdiff(unique(data[[question_name]]), mapping$choice)
    if (length(missing_vals) > 0) {
      warning(
        paste0(
          "The following values in '", question_name,
          "' are not in the lookup and will be kept as-is: ",
          paste(missing_vals, collapse = ", ")
        )
      )
    }
  }
  
  # Convert to factor with levels in dictionary order
  data[[question_name]] <- factor(data[[question_name]], levels = mapping$choice)
  
  return(data)
}

# 3. All single_select variable names
single_select_vars <- dictionary %>%
  filter(type == "single_select") %>%
  pull(variable)

# 4. Apply conversion to all single_select columns
survey_data_recoded2 <- survey_data_recoded1

for (q in single_select_vars) {
  survey_data_recoded2 <- convert_single_select_to_factor(
    survey_data_recoded2,  # note: pass the *updated* data
    q,
    single_select_lookup
  )
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
# mock_survey_data <- enforce_types(mock_survey_data, dictionary)




cleaned_data <- survey_data_recoded2


# Create folders if they don't exist
if(!dir.exists("data/processed")) dir.create("data/processed", recursive = TRUE)

# Save files in the processed data folder
saveRDS(cleaned_data, "data/processed/cleaned_survey_data.rds")
write.csv(cleaned_data, "data/processed/cleaned_survey_data.csv", row.names = FALSE)