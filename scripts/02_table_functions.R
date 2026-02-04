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



# # axis labels
# # arrange by top three (tiebreaker for very, then agree)
# # turn into a plotting function for batteries



table_output <- function(data, vars) {
  
  data %>% 
    dplyr::select(all_of(vars)) %>% 
    tidyr::pivot_longer(
      cols = everything(),
      names_to = "item",
      values_to = "response"
    ) %>%
    dplyr::filter(!is.na(response)) %>%
    dplyr::group_by(item, response) %>%
    dplyr::summarise(n = n(), .groups = "drop") %>%
    tidyr::complete(item, response, fill = list(n = 0)) %>%
    dplyr::group_by(item) %>%
    dplyr::mutate(percent = n / sum(n) * 100) %>%
    dplyr::ungroup()
}

battery_output <- function(dictionary, data) {
  
  battery_vars <- dictionary %>%
    dplyr::filter(type == "likert", !is.na(scale_group)) %>%
    dplyr::select(variable, scale_group, question_text)
  
  battery_items <- unique(battery_vars$scale_group)
  
  output <- vector("list", length(battery_items))
  names(output) <- battery_items
  
  for (b in battery_items) {
    
    vars_df <- battery_vars %>%
      dplyr::filter(scale_group == b)
    
    output[[b]] <- table_output(data, vars_df$variable) %>%
      dplyr::left_join(
        vars_df,
        by = c("item" = "variable")
      ) %>%
      dplyr::mutate(
        question_text = factor(
          question_text,
          levels = unique(question_text)
        ),
        scale_group = b
      )
  }
  
  output
}


plot_battery <- function(df) {
  ggplot(
    df,
    aes(
      y = forcats::fct_rev(question_text),
      x = percent,
      fill = forcats::fct_rev(response)
    )
  ) +
    geom_col(width = 0.7) +
    scale_x_continuous(
      labels = function(x) paste0(round(x), "%"),
      expand = expansion(mult = c(0, 0.02))
    ) +
    scale_fill_manual(
      values = c(
        "#4575b4",
        "#91bfdb",
        "#e0e0e0",
        "#fc8d59",
        "#d73027"
      ),
      guide = guide_legend(reverse = TRUE)
    ) +
    labs(
      x = "Percent of respondents",
      y = NULL,
      fill = NULL
    ) +
    theme_minimal(base_size = 12) +
    theme(
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank(),
      legend.position = "bottom"
    ) +
    geom_text(
      aes(label = ifelse(percent >= 5, paste0(round(percent), "%"), "")),
      position = position_stack(vjust = 0.5),
      color = "black",
      size = 3
    )
}


make_readable_table <- function(df) {
  df %>%
    dplyr::select(question_text, response, percent) %>%
    tidyr::pivot_wider(
      names_from = question_text,
      values_from = percent,
      values_fill = 0
    )
}


battery_list <- battery_output(dictionary, qa_data)

plots <- purrr::map(battery_list, plot_battery)

readable_tables <- purrr::map(battery_list, make_readable_table)
