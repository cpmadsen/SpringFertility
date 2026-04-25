############################################################
# MASTER ACTUARIAL PRICING LAYER (ALL MODELS)
# Updated on April 24, 2026
############################################################
# PURPOSE:
# Convert model outputs into patient-level premiums
# using:
#   - Monte Carlo simulations (egg models)
#   - flexible refund structures
#
# IMPORTANT:
# This section takes the following model outputs already exist:
#   euploid_embryo_multicycle_probabilities
#   livebirth_multicycle_probabilities

############################################################

library(dplyr)
library(readr)
library(purrr)

############################################################
# 0. MODEL INPUTS FROM PREVIOUS SCRIPT
############################################################

# File paths
euploid_model_input_path <- "outputs/euploid_embryo_multicycle_probabilities.csv"
live_birth_model_input_path <- "outputs/livebirth_multicycle_probabilities.csv"

# Read in files
euploid_probs <- readr::read_csv(
  euploid_model_input_path,
  show_col_types = FALSE
)

livebirth_probs <- readr::read_csv(
  live_birth_model_input_path,
  show_col_types = FALSE
)

############################################################
# 1. SPRING PRICING INPUTS (!! MUST BE UPDATED !!)
############################################################

# Clinic pricing
cost_per_cycle <- 12000

# Product design
payout_live_birth <- 25000
payout_euploid <- 20000
payout_egg <- 15000

# Product design (set to 1.0 for full refund)
refund_percent <- 1.0

# Actuarial / finance
risk_margin <- 0.20
admin_load <- 0.10

# !!  NOTE !!:
# Changing ANY of the above values will directly change premiums.
# These should NOT be hardcoded in production — they should be inputs.

############################################################
# 2. PRICING FUNCTION
############################################################

pricing_outputs <- pricing_inputs |>
  dplyr::mutate(
    cumulative_failure_prob = failure_3,
    cumulative_success_prob = cycle_3,
    expected_cycles = 1 + failure_1 + failure_2,
    
    expected_treatment_cost = expected_cycles * cost_per_cycle,
    expected_payout_cost = cumulative_failure_prob * payout_euploid * refund_percent,
    total_expected_cost = expected_treatment_cost + expected_payout_cost,
    premium = total_expected_cost * (1 + risk_margin + admin_load)
  )


############################################################
# 4. CORE PRICING FUNCTION
############################################################

price_product <- function(p_failure,
                          expected_cycles,
                          cost_per_cycle,
                          payout_amount,
                          refund_percent,
                          risk_margin,
                          admin_load) {
  
  ##########################################################
  # COST COMPONENTS
  ##########################################################
  
  expected_treatment_cost <- expected_cycles * cost_per_cycle
  
  expected_payout_cost <- p_failure * payout_amount * refund_percent
  
  total_expected_cost <- expected_treatment_cost + expected_payout_cost
  
  ##########################################################
  # FINAL PREMIUM
  ##########################################################
  
  premium <- total_expected_cost * (1 + risk_margin + admin_load)
  
  return(data.frame(
    failure_prob = round(p_failure, 3),
    expected_cycles = round(expected_cycles, 2),
    expected_treatment_cost = round(expected_treatment_cost, 0),
    expected_payout_cost = round(expected_payout_cost, 0),
    premium = round(premium, 0)
  ))
}


############################################################
# 5. LIVE BIRTH MODEL (MULTI-CYCLE)
############################################################

# !! NOTE !!: Replace with model-generated probabilities
p_vec_live_birth <- c(
  p_live_birth_cycle1,
  p_live_birth_cycle2,
  p_live_birth_cycle3
)

multi_lb <- compute_multi_cycle(p_vec_live_birth)

expected_cycles_lb <- expected_cycles_used(p_vec_live_birth)

pricing_live_birth <- price_product(
  p_failure = multi_lb$failure,
  expected_cycles = expected_cycles_lb,
  cost_per_cycle = cost_per_cycle,
  payout_amount = payout_live_birth,
  refund_percent = refund_percent,
  risk_margin = risk_margin,
  admin_load = admin_load
)


############################################################
# 6. EUPLOID MODEL (MULTI-CYCLE)
############################################################

# !! NOTE !!: Replace with model-generated probabilities
p_vec_euploid <- c(
  euploid_probs$cycle_1,
  euploid_probs$cycle_2,
  euploid_probs$cycle_3
)

multi_eu <- compute_multi_cycle(p_vec_euploid)

expected_cycles_eu <- expected_cycles_used(p_vec_euploid)

pricing_euploid <- price_product(
  p_failure = multi_eu$failure,
  expected_cycles = expected_cycles_eu,
  cost_per_cycle = cost_per_cycle,
  payout_amount = payout_euploid,
  refund_percent = refund_percent,
  risk_margin = risk_margin,
  admin_load = admin_load
)


############################################################
# 7. EGG MODEL — 1 CYCLE (MONTE CARLO)
############################################################

# !! NOTE !!: Monte Carlo simulation output
sims_1 <- simulate_eggs(new_patient)

# 90% threshold
threshold_90 <- quantile(sims_1, 0.10)

# 95% threshold
threshold_95 <- quantile(sims_1, 0.05)

# Failure probabilities
failure_90 <- mean(sims_1 < threshold_90)
failure_95 <- mean(sims_1 < threshold_95)

# Expected cycles = 1 (single cycle)
expected_cycles_1 <- 1

pricing_egg_90 <- price_product(
  p_failure = failure_90,
  expected_cycles = expected_cycles_1,
  cost_per_cycle = cost_per_cycle,
  payout_amount = payout_egg,
  refund_percent = refund_percent,
  risk_margin = risk_margin,
  admin_load = admin_load
)

pricing_egg_95 <- price_product(
  p_failure = failure_95,
  expected_cycles = expected_cycles_1,
  cost_per_cycle = cost_per_cycle,
  payout_amount = payout_egg,
  refund_percent = refund_percent,
  risk_margin = risk_margin,
  admin_load = admin_load
)


############################################################
# 8. EGG MODEL — 2 CYCLES (MONTE CARLO + POOLING)
############################################################

# !! NOTE !!: Ensure cycle 1 information is included in simulation
sims_2 <- simulate_eggs2(new_patient)

# 90% threshold
threshold_90_2 <- quantile(sims_2, 0.10)

# 95% threshold
threshold_95_2 <- quantile(sims_2, 0.05)

# Failure probabilities
failure_90_2 <- mean(sims_2 < threshold_90_2)
failure_95_2 <- mean(sims_2 < threshold_95_2)

# Expected cycles for 2-cycle product
expected_cycles_2 <- 2

pricing_egg2_90 <- price_product(
  p_failure = failure_90_2,
  expected_cycles = expected_cycles_2,
  cost_per_cycle = cost_per_cycle,
  payout_amount = payout_egg,
  refund_percent = refund_percent,
  risk_margin = risk_margin,
  admin_load = admin_load
)

pricing_egg2_95 <- price_product(
  p_failure = failure_95_2,
  expected_cycles = expected_cycles_2,
  cost_per_cycle = cost_per_cycle,
  payout_amount = payout_egg,
  refund_percent = refund_percent,
  risk_margin = risk_margin,
  admin_load = admin_load
)


############################################################
# 9. FINAL OUTPUT TABLE
############################################################

final_pricing <- rbind(
  cbind(model="Live Birth (3-cycle pooled)", pricing_live_birth),
  cbind(model="Euploid (3-cycle pooled)", pricing_euploid),
  cbind(model="Egg 1-cycle (90%)", pricing_egg_90),
  cbind(model="Egg 1-cycle (95%)", pricing_egg_95),
  cbind(model="Egg 2-cycle (90%)", pricing_egg2_90),
  cbind(model="Egg 2-cycle (95%)", pricing_egg2_95)
)

print(final_pricing)



# From Lara originally:


############################################################
# 2. MULTI-CYCLE POOLING FUNCTION
############################################################
# Converts per-cycle probabilities into pooled risk
# 
# compute_multi_cycle <- function(p_vec) {
#   
#   success_prob <- 1 - prod(1 - p_vec)
#   failure_prob <- prod(1 - p_vec)
#   
#   return(list(
#     success = success_prob,
#     failure = failure_prob
#   ))
# }


############################################################
# 3. EXPECTED CYCLES USED (IMPORTANT FOR COST)
############################################################

# expected_cycles_used <- function(p_vec) {
#   
#   N <- length(p_vec)
#   
#   prob_success_each <- numeric(N)
#   
#   for (i in 1:N) {
#     if (i == 1) {
#       prob_success_each[i] <- p_vec[i]
# #     } else {
# #       prob_success_each[i] <- prod(1 - p_vec[1:(i-1)]) * p_vec[i]
# #     }
# #   }
# #   
# #   failure_prob <- prod(1 - p_vec)
# #   
# #   expected_cycles <- sum((1:N) * prob_success_each) +
# #     N * failure_prob
# #   
# #   return(expected_cycles)
# # }
# 
# # This version assumes cumulative probs in input
# expected_cycles_used_cumulative <- function(p_cum_vec) {
#   
#   N <- length(p_cum_vec)
#   
#   # Probability of first success in each cycle
#   prob_success_each <- c(
#     p_cum_vec[1],
#     diff(p_cum_vec)
#   )
#   
#   # Probability of no success by final covered cycle
#   failure_prob <- 1 - p_cum_vec[N]
#   
#   expected_cycles <- sum((1:N) * prob_success_each) +
#     N * failure_prob
#   
#   return(expected_cycles)
# }
# 
