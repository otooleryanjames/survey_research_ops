library(tibble)
library(dplyr)
library(lubridate)
library(MASS)
library(stringi)

set.seed(455)

n <- 500  # sample size

### --- Simulate survey time --- ###
time_base <- rnorm(n, mean = 7, sd = 1.5)
fast_indices <- sample(1:n, 15)
slow_indices <- sample(1:n, 5)
time_base[fast_indices] <- runif(15, 0, 3)
time_base[slow_indices] <- runif(5, 20, 60)
time_to_complete <- round(pmax(time_base, 0), 1)

### --- Unique emails --- ###
emails <- paste0("user", 1:n, "@example.com")
dup_indices <- sample(1:n, 10)
emails[dup_indices] <- emails[sample(1:n, 10)]

### --- Initialize mock_data --- ###
mock_data <- tibble(
  respondent_id = 1:n,
  q_email = emails,
  q_age = sample(18:75, n, replace = TRUE),
  q_gender = sample(c("Male","Female","Non-binary", "Prefer to self-describe:", "Prefer not to say", NA), n, replace = TRUE, prob=c(0.45,0.45,0.03,0.01,0.01,0.05)),
  q_education = sample(c("Less than high school", "High school diploma", "Some college","Bachelor's degree","Master's degree","Doctorate degree", "Prefer not to say", NA), n, replace = TRUE, prob = c(0.098, 0.294, 0.196, 0.245, 0.098, 0.039, 0.01, 0.02)),
  time_to_complete = time_to_complete,
  timestamp = now() - runif(n, 0, 3600*24*30)  # last 30 days
)


### --- New column: q_gender_text for self-described gender --- ###
# Example self-describe responses
self_describe_options <- c(
  "Agender",
  "Genderqueer",
  "Transgender male",
  "Transgender female"
)

# Initialize column with NA
mock_data$q_gender_text <- NA_character_

# Fill in self-describe responses
self_describe_indices <- which(mock_data$q_gender == "Prefer to self-describe:")
mock_data$q_gender_text[self_describe_indices] <- sample(
  self_describe_options,
  length(self_describe_indices),
  replace = TRUE
)


### --- Correlated Satisfaction & Trust Batteries --- ###
n_satisfaction <- 5
n_trust <- 5
rho <- 0.75

# Satisfaction q1–q5
cor_matrix_sat <- matrix(rho, n_satisfaction, n_satisfaction)
diag(cor_matrix_sat) <- 1
latent_sat <- mvrnorm(n, mu=rep(3,n_satisfaction), Sigma=cor_matrix_sat)
battery_sat <- apply(latent_sat, 2, function(x) pmin(pmax(round(x),1),5))
for(i in 1:n_satisfaction){
  mock_data[[paste0("q", i, "_satisfaction")]] <- battery_sat[, i]
}

# Trust q6–q10
cor_matrix_trust <- matrix(rho, n_trust, n_trust)
diag(cor_matrix_trust) <- 1
latent_trust <- mvrnorm(n, mu=rep(3,n_trust), Sigma=cor_matrix_trust)
battery_trust <- apply(latent_trust, 2, function(x) pmin(pmax(round(x),1),5))
for(i in 1:n_trust){
  mock_data[[paste0("q", n_satisfaction + i, "_trust")]] <- battery_trust[, i]
}



### --- Multiselect Running Shoes q11 --- ###
brands <- c("Salomon","Topo","Saucony","Brooks","Asics","Hoka","A different brand, not listed here")
for(b in brands){
  mock_data[[paste0("q11_multiselect_",b)]] <- NA
}

for(row in 1:n){
  n_select <- sample(0:3,1)
  if(n_select>0){
    chosen <- sample(brands,n_select)
    for(b in chosen){
      col_name <- paste0("q11_multiselect_",b)
      mock_data[[col_name]][row] <- b
    }
  }
}

### --- Skewed Likert q12 --- ###
likert_levels <- c("5", "4", "3", "2", "1", NA)
prob_skewed <- c(0,0.06,0.21,0.4,0.3,0.03)
mock_data$q12_likert <- sample(likert_levels, n, replace=TRUE, prob=prob_skewed)

### --- Attention Check q13 --- ###
attention_levels <- c("Yes, I'm paying attention","Just clicking this button",
                      "I'm definitely not paying attention","Just here for the vibes",NA)
prob_attention <- c(0.94,0.01,0.01,0.01,0.03)
mock_data$q13_attention <- sample(attention_levels,n,replace=TRUE,prob=prob_attention)

### --- Open-ended text q14 --- ###
text_real <- c(
  "I really liked this experience",
  "Could use some improvement",
  "No strong opinion",
  "It was confusing at times",
  "I learned a lot from this",
  "Not what I expected",
  "Well organized and clear"
)
gibberish <- c("asdf qwer zxcv","lorem ipsum 123","blah blah blah","qwerty uiop 789","zzzz aaa bbb")
mock_data$q14_text <- sample(c(text_real,gibberish), n, replace=TRUE,
                             prob=c(rep(0.9/length(text_real),length(text_real)),
                                    rep(0.1/length(gibberish),length(gibberish))))
mock_data$q14_text <- paste0(mock_data$q14_text, sample(c("","!","."),n,replace=TRUE))



### --- Numeric: Miles per Week q15 --- ###
mock_data$q15_miles_per_week <- round(rnorm(n, mean=35, sd=10))
mock_data$q15_miles_per_week[sample(1:n,20)] <- NA
mock_data$q15_miles_per_week[sample(1:n,5)] <- sample(c(250,450,500),5,replace=TRUE)
mock_data$q15_miles_per_week <- pmax(mock_data$q15_miles_per_week,0)

### --- Optional: Dropout (partial survey completion) --- ###
dropout_rate <- 0.2
n_dropout <- round(n*dropout_rate)
dropout_indices <- sample(1:n, n_dropout)

survey_cols <- setdiff(names(mock_data), c("respondent_id","timestamp","time_to_complete","q_email","q_age","q_gender","q_education"))

for(i in dropout_indices){
  completed <- sample(3:length(survey_cols),1)
  if(completed < length(survey_cols)){
    mock_data[i, survey_cols[(completed+1):length(survey_cols)]] <- NA
  }
}


### --- Reorder columns: Metadata → Satisfaction → Trust → Multiselect → Other --- ###
demographics_cols <- c("respondent_id","timestamp","time_to_complete","q_email","q_age","q_gender","q_gender_text","q_education")
satisfaction_cols <- paste0("q",1:n_satisfaction,"_satisfaction")      # q1–q5
trust_cols <- paste0("q",6:10,"_trust")                                # q6–q10
multiselect_cols <- paste0("q11_multiselect_",brands)                  # q11
other_cols <- c("q12_likert","q13_attention","q14_text","q15_miles_per_week")
mock_data <- mock_data[, c(demographics_cols, satisfaction_cols, trust_cols, multiselect_cols, other_cols)]


### --- Quick checks --- ###
summary(mock_data$time_to_complete)
sum(duplicated(mock_data$q_email))
cor(mock_data[, satisfaction_cols], use="complete.obs")
cor(mock_data[, trust_cols], use="complete.obs")

### --- Optional: subset of dropouts --- ###
mock_dropout_data <- mock_data %>% slice(dropout_indices)
cat("Dropout respondents:", n_dropout, "\n")




# --- Convert Satisfaction and Trust columns to character ---
mock_data <- mock_data %>%
  mutate(across(all_of(c(satisfaction_cols, trust_cols)), as.character))



# Create folders if they don't exist
if(!dir.exists("data/raw")) dir.create("data/raw", recursive = TRUE)

# Save files in the raw data folder
saveRDS(mock_data, "data/raw/mock_survey_data.rds")
write.csv(mock_data, "data/raw/mock_survey_data.csv", row.names = FALSE)

# Create folders if they don't exist
if(!dir.exists("data/simulated")) dir.create("data/simulated", recursive = TRUE)

# Save files in the simulated data folder
saveRDS(mock_data, "data/simulated/mock_survey_data.rds")
write.csv(mock_data, "data/simulated/mock_survey_data.csv", row.names = FALSE)
