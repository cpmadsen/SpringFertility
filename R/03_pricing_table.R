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

library(purrr)

# IMPORTANT:
# Replace this with actual model output:
attempt_predictions <- readr::read_csv("outputs/attempt_probabilities_by_age_long.csv")

##############################
# 1. FINANCIAL INPUTS
##############################

cost_per_cycle <- 22000
payout_amount <- 22000
risk_margin <- 0.20
admin_load <- 0.10

##############################
# 2. PRICING FUNCTION
##############################

price_product <- function(p_vec, N, cost_per_cycle, payout_amount, risk_margin, admin_load) {
  
  if (length(p_vec) < N) {
    stop(paste("Not enough attempt probabilities supplied for N =", N))
  }
  
  p <- p_vec[1:N]
  
  failure_prob <- prod(1 - p)
  success_prob <- 1 - failure_prob
  
  prob_success_each <- numeric(N)
  
  for (i in seq_len(N)) {
    if (i == 1) {
      prob_success_each[i] <- p[i]
    } else {
      prob_success_each[i] <- prod(1 - p[1:(i - 1)]) * p[i]
    }
  }
  
  expected_cycles <- sum(seq_len(N) * prob_success_each) + N * failure_prob
  
  expected_treatment_cost <- expected_cycles * cost_per_cycle
  expected_payout_cost <- failure_prob * payout_amount
  total_expected_cost <- expected_treatment_cost + expected_payout_cost
  premium <- total_expected_cost * (1 + risk_margin + admin_load)
  
  tibble(
    cycles_covered = N,
    success_prob = success_prob,
    failure_prob = failure_prob,
    expected_cycles = expected_cycles,
    expected_treatment_cost = expected_treatment_cost,
    expected_payout_cost = expected_payout_cost,
    total_expected_cost = total_expected_cost,
    premium = premium
  )
}

##############################
# 3. PREP DATA BY AGE GROUP
##############################

attempt_predictions <- attempt_predictions %>%
  mutate(
    age_group = factor(age_group, levels = c("<35", "35-37", "38-40", "41+"))
  )

age_results <- attempt_predictions %>%
  arrange(age_group, attempt_num) %>%
  group_by(age_group) %>%
  summarise(
    p_vec = list(predicted_p_mean),
    .groups = "drop"
  )

##############################
# 4. GENERATE PRICING TABLE
##############################

final_pricing_table <- age_results %>%
  mutate(
    pricing = map(
      p_vec,
      ~ map_dfr(
        1:3,
        \(n) price_product(
          p_vec = .x,
          N = n,
          cost_per_cycle = cost_per_cycle,
          payout_amount = payout_amount,
          risk_margin = risk_margin,
          admin_load = admin_load
        )
      )
    )
  ) %>%
  dplyr::select(age_group, pricing) %>%
  tidyr::unnest(pricing) %>%
  arrange(age_group, cycles_covered)

##############################
# 5. PRESENTATION TABLE
##############################

presentation_table <- final_pricing_table %>%
  dplyr::mutate(
    success_prob = round(success_prob, 3),
    failure_prob = round(failure_prob, 3),
    expected_cycles = round(expected_cycles, 2),
    expected_treatment_cost = round(expected_treatment_cost, 0),
    expected_payout_cost = round(expected_payout_cost, 0),
    total_expected_cost = round(total_expected_cost, 0),
    premium = round(premium, 0)
  ) %>%
  dplyr::select(
    age_group,
    cycles_covered,
    success_prob,
    failure_prob,
    expected_cycles,
    expected_treatment_cost,
    expected_payout_cost,
    total_expected_cost,
    premium
  ) %>%
  dplyr::arrange(age_group, cycles_covered)

print(presentation_table)

##############################
# 6. EXPORT TO CSV
##############################

readr::write_csv(
  presentation_table,
  "outputs/ivf_pricing_by_age_and_cycles.csv"
)

