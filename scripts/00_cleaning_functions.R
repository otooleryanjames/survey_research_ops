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


library(dplyr)
library(tidyr)
library(janitor)

clean_survey_data <- function(raw_data, dictionary, date_var, email_var) {
  
  # --- 1️⃣ Deduplicate by email ---
  data <- raw_data %>%
    filter(!is.na({{ email_var }})) %>%
    arrange({{ email_var }}, {{ date_var }}) %>%
    distinct({{ email_var }}, .keep_all = TRUE) %>%
    clean_names()
  
  # --- 2️⃣ Likert recoding ---
  likert_lookup <- dictionary %>%
    filter(type == "likert") %>%
    mutate(levels = gsub(",(?=\\s*\\d+\\s*=)", "|DELIM|", levels, perl = TRUE)) %>%
    separate_rows(levels, sep = "\\|DELIM\\|") %>%
    separate(levels, into = c("value", "choice"), sep = " = ") %>%
    mutate(value = trimws(value), choice = trimws(choice))
  
  convert_likert_to_factor <- function(data, question_name, lookup) {
    mapping <- lookup %>% filter(variable == question_name) %>% arrange(as.numeric(value))
    data[[question_name]] <- mapping$choice[match(data[[question_name]], mapping$value)]
    data[[question_name]] <- factor(data[[question_name]],
                                    levels = mapping$choice,
                                    ordered = TRUE)
    data
  }
  
  likert_vars <- dictionary %>% filter(type == "likert") %>% pull(variable)
  for (q in likert_vars) {
    data <- convert_likert_to_factor(data, q, likert_lookup)
  }
  
  # --- 3️⃣ Single-select recoding ---
  single_select_lookup <- dictionary %>%
    filter(type == "single_select") %>%
    mutate(levels = gsub(",(?=\\s*\\d+\\s*=)", "|DELIM|", levels, perl = TRUE)) %>%
    separate_rows(levels, sep = "\\|DELIM\\|") %>%
    separate(levels, into = c("value", "choice"), sep = " = ") %>%
    mutate(value = trimws(value), choice = trimws(choice))
  
  convert_single_select_to_factor <- function(data, question_name, lookup) {
    mapping <- lookup %>% filter(variable == question_name)
    
    if (all(data[[question_name]] %in% mapping$value | is.na(data[[question_name]]))) {
      data[[question_name]] <- mapping$choice[match(data[[question_name]], mapping$value)]
    } else {
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
    
    data[[question_name]] <- factor(data[[question_name]], levels = mapping$choice)
    data
  }
  
  single_select_vars <- dictionary %>% filter(type == "single_select") %>% pull(variable)
  for (q in single_select_vars) {
    data <- convert_single_select_to_factor(data, q, single_select_lookup)
  }
  
  return(data)
}


cleaned_data <- clean_survey_data(
  raw_data = survey_data,
  dictionary = dictionary,
  date_var = timestamp,
  email_var = q_email
)


# Create folders if they don't exist
if(!dir.exists("data/processed")) dir.create("data/processed", recursive = TRUE)

# Save files in the processed data folder
saveRDS(cleaned_data, "data/processed/cleaned_survey_data.rds")
write.csv(cleaned_data, "data/processed/cleaned_survey_data.csv", row.names = FALSE)