############################################################
# IVF INSURANCE PRICING MODEL — AGE × CYCLE PRODUCTS
# Purpose:
# - Price 1-cycle, 2-cycle, and 3-cycle offerings
# - separately for each age group using model outputs
#
# REQUIRED INPUTS:
# - 1. attempt_predictions (from Step 7 model output)
# - 2. cost + pricing assumptions
############################################################

##############################
# 0. REQUIRED DATA INPUT
##############################

# - Bring in previous output:
# Columns required:
# - age_group | attempt_num | predicted_p

# Example structure (REPLACE WITH REAL MODEL OUTPUT)
# attempt_predictions <- data.frame(
#   age_group = rep(c("<35","35-37","38-40","41+"), each = 3),
#   attempt_num = rep(1:3, times = 4),
#   predicted_p = c(
#     0.45, 0.40, 0.35,   # <35
#     0.40, 0.35, 0.30,   # 35-37
#     0.30, 0.25, 0.20,   # 38-40
#     0.15, 0.12, 0.10    # 41+
#   )
# )

# IMPORTANT:
# Replace this with actual model output:
attempt_predictions <- readr::read_csv("outputs/attempt_probabilities_by_age_long.csv")


##############################
# 1. FINANCIAL INPUTS
##############################

# Source: Clinic / finance team - can vary
cost_per_cycle <- 12000

# Source: TBD
payout_amount <- 25000

# Source: Actuarial / finance
risk_margin <- 0.20
admin_load <- 0.10


##############################
# 2. PRICING FUNCTION
##############################

# This function prices a product covering N cycles
price_product <- function(p_vec, N, cost_per_cycle, payout_amount, risk_margin, admin_load) {
  
  # Select probabilities up to N cycles
  p <- p_vec[1:N]
  
  # --- FAILURE PROBABILITY (payout risk) ---
  failure_prob <- prod(1 - p)
  success_prob <- 1 - failure_prob
  
  # --- PROBABILITY OF SUCCESS AT EACH CYCLE ---
  prob_success_each <- numeric(N)
  
  for (i in 1:N) {
    if (i == 1) {
      prob_success_each[i] <- p[i]
    } else {
      prob_success_each[i] <- prod(1 - p[1:(i-1)]) * p[i]
    }
  }
  
  # --- EXPECTED CYCLES USED ---
  expected_cycles <- sum((1:N) * prob_success_each) + N * failure_prob
  
  # --- COST CALCULATIONS ---
  expected_treatment_cost <- expected_cycles * cost_per_cycle
  expected_payout_cost <- failure_prob * payout_amount
  
  total_expected_cost <- expected_treatment_cost + expected_payout_cost
  
  # --- FINAL PREMIUM ---
  premium <- total_expected_cost * (1 + risk_margin + admin_load)
  
  return(data.frame(
    cycles_covered = N,
    success_prob = success_prob,
    failure_prob = failure_prob,
    expected_cycles = expected_cycles,
    expected_treatment_cost = expected_treatment_cost,
    expected_payout_cost = expected_payout_cost,
    total_expected_cost = total_expected_cost,
    premium = premium
  ))
}


##############################
# 3. RUN MODEL BY AGE GROUP
##############################

library(dplyr)

# Group probabilities by age
age_results <- attempt_predictions %>%
  group_by(age_group) %>%
  arrange(attempt_num) %>%
  summarize(
    p_vec = list(predicted_p),
    .groups = "drop"
  )


##############################
# 4. GENERATE PRICING TABLE
##############################

results_list <- list()

for (i in 1:nrow(age_results)) {
  
  age <- age_results$age_group[i]
  p_vec <- unlist(age_results$p_vec[i])
  
  # Price each product option
  pricing_1 <- price_product(p_vec, 1, cost_per_cycle, payout_amount, risk_margin, admin_load)
  pricing_2 <- price_product(p_vec, 2, cost_per_cycle, payout_amount, risk_margin, admin_load)
  pricing_3 <- price_product(p_vec, 3, cost_per_cycle, payout_amount, risk_margin, admin_load)
  
  # Combine and add age label
  age_table <- rbind(pricing_1, pricing_2, pricing_3)
  age_table$age_group <- age
  
  results_list[[i]] <- age_table
}

final_pricing_table <- bind_rows(results_list)



##############################
# 5. TABLE
##############################

presentation_table <- final_pricing_table %>%
  mutate(
    success_prob = round(success_prob, 2),
    failure_prob = round(failure_prob, 2),
    expected_cycles = round(expected_cycles, 2),
    expected_treatment_cost = round(expected_treatment_cost, 0),
    expected_payout_cost = round(expected_payout_cost, 0),
    total_expected_cost = round(total_expected_cost, 0),
    premium = round(premium, 0)
  ) %>%
  select(
    age_group,
    cycles_covered,
    success_prob,
    failure_prob,
    expected_cycles,
    expected_treatment_cost,
    expected_payout_cost,
    premium
  ) %>%
  arrange(age_group, cycles_covered)

print(presentation_table)



##############################
# 6.  EXPORT TO CSV
##############################

write.csv(presentation_table, "outputs/ivf_pricing_by_age_and_cycles.csv", row.names = FALSE)
