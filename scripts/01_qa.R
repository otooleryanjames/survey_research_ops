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

run_survey_QA <- function(data, dictionary) {
  
  ### 1️⃣ Time-based flag: weighted speeder
  speed_time <- median(data$time_to_complete, na.rm = TRUE) - 3 * mad(data$time_to_complete, na.rm = TRUE)
  
  data <- data %>%
    mutate(
      flag_speeder_time_weighted = case_when(
        time_to_complete >= speed_time ~ 0,
        TRUE ~ pmin(1, ((speed_time - time_to_complete) / speed_time)^2) * 2
      )
    )
  
  ### 2️⃣ Attention check flags
  attention_table <- dictionary %>%
    filter(qa_role == "attention_check") %>%
    mutate(
      # Handle comma-separated levels
      levels = gsub(",(?=\\s*\\d+\\s*=)", "|DELIM|", levels, perl = TRUE)
    ) %>%
    separate_rows(levels, sep = "\\|DELIM\\|") %>%
    separate(levels, into = c("value", "choice"), sep = " = ") %>%
    mutate(value = trimws(value), choice = trimws(choice))
  
  for (q in unique(attention_table$variable)) {
    correct_response <- attention_table %>%
      filter(variable == q) %>%
      slice(1) %>% # assume first listed value is correct
      pull(choice)
    
    flag_col <- paste0(q, "_attention_fail")
    data[[flag_col]] <- ifelse(data[[q]] == correct_response, 0, 2)
  }
  
  ### 3️⃣ Straightliner check across all batteries
  battery_table <- dictionary %>%
    filter(qa_role == "scale") %>%
    dplyr::select(variable, scale_group)
  
  battery_names <- unique(battery_table$scale_group)
  n <- nrow(data)
  pct_straightlined <- numeric(n)
  
  for (b in battery_names) {
    items <- battery_table %>%
      filter(scale_group == b) %>%
      pull(variable)
    
    items <- intersect(items, names(data))
    if (length(items) < 3) next
    
    battery_flags <- apply(
      data[items],
      1,
      function(row) {
        row <- row[!is.na(row)]
        if (length(row) < 3) return(0)
        as.integer(length(unique(row)) == 1)
      }
    )
    
    pct_straightlined <- pct_straightlined + battery_flags
  }
  
  pct_straightlined <- pct_straightlined / length(battery_names)
  data$pct_batteries_straightlined <- pct_straightlined
  
  ### 4️⃣ QA summary score
  attention_cols <- grep("_attention_fail$", names(data), value = TRUE)
  
  data <- data %>%
    rowwise() %>%
    mutate(
      qa_sum = sum(
        c_across(c(flag_speeder_time_weighted, pct_batteries_straightlined, all_of(attention_cols))),
        na.rm = TRUE
      )
    ) %>%
    ungroup()
  
  return(data)
}


qa_data <- run_survey_QA(clean_data, dictionary)


# Save files in the processed data folder
saveRDS(cleaned_data, "data/processed/cleaned_qaflag_survey_data.rds")
write.csv(cleaned_data, "data/processed/cleaned_qaflag_survey_data.csv", row.names = FALSE)
