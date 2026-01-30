# survey_research_ops
Reproducible R workflow for transforming messy Qualtrics survey exports into 
analysis-ready datasets with built-in quality assurance and documentation.


# Simulated Survey Data Project

## Overview
This repository contains simulated survey data and accompanying R code for generating, cleaning, and summarizing responses. The data is designed to resemble real-world survey data, including:

- Multiple question types: single-select, multi-select, Likert scale, numeric, and open-ended text.
- Correlated batteries (e.g., satisfaction and trust questions).
- Skewed responses for attention checks and Likert questions.
- Partial survey completion (dropouts) to simulate real-world response patterns.
- Randomized email identifiers, some duplicates, and outlier responses.

The goal of this project is to create a realistic dataset for testing data cleaning functions, summary outputs, and visualization pipelines.

---

## Contents

/data
/raw # Optional: placeholder for original survey files
/simulated # Generated simulated survey data (CSV/RDS)
/R
generate_mock_data.R # Script for generating the simulated survey dataset
clean_functions.R # Functions for cleaning and structuring survey data
summary_functions.R # Functions for generating tables and plots
README.md


---

## Simulated Dataset

The main dataset `mock_data` includes:

- **Metadata**: `respondent_id`, `timestamp`, `time_to_complete`, `q_email`, `q_age`, `q_gender`, `q_education`.
- **Satisfaction Battery**: `q1_satisfaction` … `q5_satisfaction` (5-point Likert, correlated).
- **Trust Battery**: `q6_trust` … `q10_trust` (5-point Likert, correlated).
- **Multi-select Question**: `q11_multiselect_*` (select 0–3 brands from a set of running shoes).
- **Skewed Likert Question**: `q12_likert`.
- **Attention Check Question**: `q13_attention`.
- **Open-ended Text Responses**: `q8_text` / `q14_text`.
- **Numeric Input**: `q15_miles_per_week` (with random NAs and extreme outliers).
- **Dropouts**: 20% of respondents stop partway through the survey to simulate partial completion.

---

## Generating the Data

The simulated dataset is generated using `generate_mock_data.R`. Key features include:

1. **Randomized timestamps** within the last 30 days.
2. **Time to complete** with realistic variance and outliers.
3. **Correlated batteries** for satisfaction and trust.
4. **Skewed distributions** for Likert and attention questions.
5. **Partial completion** for 20% of respondents.
6. **Randomized multi-select choices** for running shoe brands.
7. **Randomly inserted NAs and extreme outliers** in numeric questions.

To regenerate the mock dataset:

```r
source("R/generate_mock_data.R")
