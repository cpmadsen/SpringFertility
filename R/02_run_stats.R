# These stats come to us from Lara. They look brilliant!

# Load in data
df = readr::read_rds("data/clean_data/cld_2022.rds") |> 
  dplyr::rename(pregnant = pregnant_rev) |> 
  dplyr::mutate(pregnant = ifelse(pregnant == 'yes', 1, 0))


############################################################
# IVF INSURANCE MODELING SCRIPT
# Purpose: Build patient-level pregnancy probabilities
# and convert to multi-cycle insurance risk
############################################################

##############################
# 0. SETUP
##############################

# Install packages if needed
# install.packages(c("dplyr", "ggplot2", "VGAM"))

library(dplyr)
library(ggplot2)
library(VGAM)

# Assumes dataset "df" with:
# pregnant = 1/0 outcome
# age = numeric
# attempt = cycle number (1,2,3,...)
# patient_id = unique patient identifier



##############################
# 1. CREATE AGE GROUPS
##############################

df <- df %>%
  mutate(age_group = case_when(
    age < 35 ~ "<35",
    age >= 35 & age <= 37 ~ "35-37",
    age >= 38 & age <= 40 ~ "38-40",
    age >= 41 ~ "41+"
  ))

df$age_group <- as.factor(df$age_group)



########################################
# 2. STEP 1: DESCRIPTIVE PREGNANCY RATES
########################################
# Calculate historical pregnancy rates per cycle by age group

pregnancy_rates <- df %>%
  group_by(age_group) %>%
  summarize(
    cycles = n(),
    pregnancies = sum(pregnant),
    pregnancy_rate = pregnancies / cycles
  )

print(pregnancy_rates)



###################################
# 3. STEP 2: BINOMIAL (LOGISTIC) MODEL
###################################
# Estimate probability of pregnancy per cycle using age group

logit_model <- glm(pregnant ~ age_group,
                   family = binomial(link = "logit"),
                   data = df)

summary(logit_model)



##############################
# 4. PREDICTED PROBABILITIES
##############################
# Convert model outputs into probabilities (p)

predicted_probs <- df %>%
  distinct(age_group) %>%
  mutate(predicted_p = predict(logit_model, newdata = ., type = "response"))

print(predicted_probs)



##################################################
# 5. STEP 3: MULTI-CYCLE SUCCESS (INSURANCE VIEW)
##################################################
# Convert per-cycle probability into probability of success
# across N covered cycles

N <- 3 # number of covered cycles

multi_cycle <- predicted_probs %>%
  mutate(
    success_N_cycles = 1 - (1 - predicted_p)^N,
    failure_N_cycles = (1 - predicted_p)^N
  )

print(multi_cycle)



###################################
# 6. ADD ATTEMPT-LEVEL MODELING
###################################
# Allow probability to vary by cycle number (attempt)

# Chris: I think we might need to recalculate attempt number within 2022.
# If we take 'attempt' column at face value, it can have a higher number than
# we have rows for a patient (e.g. look at patient 'C81763': 'attempt' col says 18
# is their highest attempt #, but that patient only has 16 rows, 4 of which say attempt = 1)

df_attempt_recalc = df |> 
  dplyr::group_by(pat_id) |> 
  dplyr::arrange(treatment_start_date) |> 
  dplyr::mutate(attempt_num = dplyr::row_number()) |> 
  dplyr::ungroup()

attempt_model <- glm(pregnant ~ age_group + attempt_num,
                     family = binomial,
                     data = df_attempt_recalc)

summary(attempt_model)



###################################
# 7. PREDICT PROBABILITY BY ATTEMPT
###################################
# Generate p1, p2, p3 for each age group

attempt_predictions <- expand.grid(
  age_group = levels(df$age_group),
  attempt_num = 1:3
)

attempt_predictions$predicted_p <- predict(
  attempt_model,
  newdata = attempt_predictions,
  type = "response"
)
  
attempt_predictions |> 
  dplyr::arrange(age_group, attempt_num)
