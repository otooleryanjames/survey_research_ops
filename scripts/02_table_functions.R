library(tibble)
library(dplyr)
library(lubridate)
library(janitor)
library(tidyr)
library(here)
library(ggplot2)
library(scales)
library(forcats)

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

satisfaction_long <- satisfaction_data %>%
  pivot_longer(
    cols = everything(),
    names_to = "item",
    values_to = "response"
  ) %>%
  filter(!is.na(response)) %>%   # optional, but usually wise
  count(item, response) %>%
  group_by(item) %>%
  mutate(percent = n / sum(n) * 100) %>%
  ungroup()

satisfaction_long

ggplot(
  satisfaction_long,
  aes(
    y = item,
    x = percent,
    fill = fct_rev(response)  # reverse factor for bars
  )
) +
  geom_col(width = 0.7) +  # horizontal bars, width = thickness
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
    guide = guide_legend(reverse = TRUE)  # <-- flip legend order
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


# axis labels
# arrange by top three (tiebreaker for very, then agree)
# turn into a plotting function for batteries



       