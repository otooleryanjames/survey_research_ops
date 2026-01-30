library(tibble)
library(dplyr)
library(lubridate)
library(MASS)

set.seed(455)

n <- 500

### ---------------- Time to Complete ---------------- ###
time_base <- rnorm(n, mean = 7, sd = 1.5)
fast_indices <- sample(1:n, 15)
slow_indices <- sample(1:n, 5)
time_base[fast_indices] <- runif(15, 0, 3)
time_base[slow_indices] <- runif(5, 20, 60)
time_to_complete <- round(pmax(time_base, 0), 1)

### ---------------- Emails & Demographics ---------------- ###
emails <- paste0("user", 1:n, "@example.com")
dup_indices <- sample(1:n, 10)
emails[dup_indices] <- emails[sample(1:n, 10)]

mock_data <- tibble(
  respondent_id = 1:n,
  q_email = emails,
  q_age = sample(18:75, n, replace = TRUE),
  q_gender = sample(c("Male","Female","Other",NA), n, replace = TRUE, prob=c(0.45,0.45,0.05,0.05)),
  q_education = sample(c("High school","Some college","Bachelor's","Master's","Doctorate"), n, replace = TRUE),
  time_to_complete = time_to_complete,
  timestamp = now() - runif(n, 0, 3600*24*30)
)

### ---------------- Correlated Batteries ---------------- ###
make_battery <- function(n_items, rho=0.75, n_respondents=n){
  Sigma <- matrix(rho, n_items, n_items); diag(Sigma) <- 1
  latent <- mvrnorm(n_respondents, mu=rep(3, n_items), Sigma=Sigma)
  apply(latent, 2, function(x) pmin(pmax(round(x),1),5))
}

# Satisfaction and Trust
battery_sat <- make_battery(5)
battery_trust <- make_battery(5)

for(i in 1:5){
  mock_data[[paste0("q", i, "_satisfaction")]] <- battery_sat[,i]
  mock_data[[paste0("q", i, "_trust")]] <- battery_trust[,i]
}

### ---------------- Multiselect Question ---------------- ###
brands <- c("Salomon","Topo","Saucony","Brooks","Asics","Hoka","New Balance")
for(b in brands) mock_data[[paste0("q_multiselect_",b)]] <- NA
for(i in 1:n){
  n_sel <- sample(0:3,1)
  if(n_sel>0){
    chosen <- sample(brands, n_sel)
    for(b in chosen) mock_data[[paste0("q_multiselect_",b)]][i] <- b
  }
}

### ---------------- Skewed Likert Questions ---------------- ###
likert_levels <- c("Strongly disagree","Disagree","Neutral","Agree","Strongly agree",NA)
mock_data$q9_likert_skewed <- sample(likert_levels, n, replace=TRUE, prob=c(0,0.06,0.21,0.4,0.3,0.03))

attention_levels <- c("Yes, I'm paying attention","Just clicking this button","I'm definitely not paying attention",
                      "Just here for the vibes",NA)
mock_data$q_attention_skewed <- sample(attention_levels, n, replace=TRUE, prob=c(0.94,0.01,0.01,0.01,0.03))

mock_data$q7_likert <- sample(likert_levels, n, replace=TRUE)

### ---------------- Open-Text Responses ---------------- ###
text_responses <- c("I really liked this experience","Could use some improvement","No strong opinion",
                    "It was confusing at times","I learned a lot from this","Not what I expected",
                    "Well organized and clear")
gibberish <- c("asdf qwer zxcv","lorem ipsum 123","blah blah blah","qwerty uiop 789","zzzz aaa bbb")
mock_data$q8_text <- sample(c(text_responses,gibberish), n, replace=TRUE,
                            prob=c(rep(0.9/length(text_responses),length(text_responses)),
                                   rep(0.1/length(gibberish),length(gibberish))))
mock_data$q8_text <- paste0(mock_data$q8_text, sample(c("", "!", ".", "!!"), n, replace=TRUE))

### ---------------- Numeric Input ---------------- ###
mock_data$q_miles_per_week <- round(rnorm(n, mean=35, sd=10))
mock_data$q_miles_per_week[sample(1:n, 20)] <- NA
mock_data$q_miles_per_week[sample(1:n, 5)] <- sample(c(250,450,500),5,replace=TRUE)
mock_data$q_miles_per_week <- pmax(mock_data$q_miles_per_week,0)

### ---------------- Reorder Columns ---------------- ###
all_metadata <- c("respondent_id","timestamp","time_to_complete","q_email","q_age","q_gender","q_education")
question_cols <- setdiff(names(mock_data), all_metadata)
mock_data <- mock_data[, c(all_metadata, question_cols)]

survey_cols <- question_cols  # for dropout reference

### ---------------- Simulate Dropout ---------------- ###
dropout_rate <- 0.2
n_dropout <- round(n*dropout_rate)
dropout_indices <- sample(1:n, n_dropout)

for(i in dropout_indices){
  completed <- sample(3:length(survey_cols),1)
  if(completed < length(survey_cols)){
    mock_data[i, survey_cols[(completed+1):length(survey_cols)]] <- NA
  }
}

cat("Dropout rate:", dropout_rate, "\n")
cat("Number of respondents who dropped out:", n_dropout, "\n")
cat("Indices of dropout respondents:\n")
print(dropout_indices)

mock_dropout_data <- mock_data %>% slice(dropout_indices)

### ---------------- Quick Checks ---------------- ###
summary(mock_data$time_to_complete)
sum(duplicated(mock_data$q_email))
cor(mock_data[, grep("_satisfaction$",names(mock_data))], use="complete.obs")
cor(mock_data[, grep("_trust$",names(mock_data))], use="complete.obs")