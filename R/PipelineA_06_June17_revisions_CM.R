# ============================================================
# 06 -- INELIGIBLE PATIENT PROFILES & WORST-PERFORMER SUMMARY
# Products: IVF (live birth) and Euploid embryo
# Cohort:   Spring 2025 validation data
#
# RUN ORDER: run AFTER PipelineA_05. This script sources 05 to
# obtain the 2025 cohort and its calibrated euploid predictions,
# then adds a live-birth probability and computes ineligibility
# (premium cap) and worst-performer (bottom-decile) summaries.
#
# NOTE ON CONSISTENCY: this uses the floored deployed models
# (age_model) via 05, and the calibration constants saved by 01.
# It assumes you applied the 05 fix so p_cal_eup uses
# eu_cal_int_refit / eu_cal_slope_refit (not the old hardcoded
# 0.0152 / 0.8508).
# ============================================================

library(dplyr)

# ── Parameters ───────────────────────────────────────────────
# Keep these aligned with the location + margins you use in
# PipelineA_03 (the Excel model). Defaults below = NY location,
# risk_margin 0.20, admin_load 0.10.
age_floor      <- 32
cost_livebirth <- 21460   # PipelineA_03 NY live_birth cost (C-column in Excel)
cost_euploid   <- 18704   # PipelineA_03 NY euploid cost
risk_margin    <- 0.20
admin_load     <- 0.10
combined_load  <- (1 + risk_margin) * (1 + admin_load)   # = 1.32
cap_pct        <- 0.50    # 50% premium cap -- do not change

# ── Calibration + model needed for the live-birth product ────
# (05 already loads euploid_model and computes p_cal_eup; the
#  live-birth leg is added here.)
lb_cal_int_refit   <- readr::read_rds("data/models/lb_cal_int_refit.rds")
lb_cal_slope_refit <- readr::read_rds("data/models/lb_cal_slope_refit.rds")
livebirth_model    <- readr::read_rds("data/models/livebirth_model_revised_modeling_statistics_end.rds")

# ── Pull the 2025 cohort + euploid predictions from script 05 ─
# 05 leaves in the environment:
#   df_embryo : embryo-guarantee cohort, with age_model + p_cal_eup
#   df_egg    : egg-freezing cohort
source("R/PipelineA_05_compare_2025_v2.R")

logit <- function(p) log(p / (1 - p))

stopifnot(exists("df_embryo"), "p_cal_eup" %in% names(df_embryo))

# ============================================================
# PART 1: IVF / LIVE BIRTH
# ------------------------------------------------------------
# The live-birth model is conditional on a euploid embryo, so the
# UNCONDITIONAL probability of a live birth is:
#     P(live birth) = P(>=1 euploid) * P(live birth | euploid)
# We already have P(>=1 euploid) = p_cal_eup from 05; here we add
# the calibrated P(live birth | euploid) and multiply.
# ============================================================

df_livebirth <- df_embryo %>%
  mutate(
    p_raw_lb_given_eu = predict(livebirth_model, newdata = ., type = "response"),
    p_cal_lb_given_eu = plogis(lb_cal_int_refit + lb_cal_slope_refit * logit(p_raw_lb_given_eu)),
    p_livebirth       = p_cal_eup * p_cal_lb_given_eu,     # unconditional live-birth prob
    failure_prob_lb   = 1 - p_livebirth,
    uncapped_premium_lb = failure_prob_lb * cost_livebirth * combined_load,
    cap_level_lb      = cap_pct * cost_livebirth,
    cap_binds_lb      = uncapped_premium_lb > cap_level_lb,
    ineligible_lb     = cap_binds_lb,
    p10_cutoff_lb     = quantile(p_livebirth, 0.10, na.rm = TRUE),
    worst_performer_lb = p_livebirth <= p10_cutoff_lb
  )

# Overall ineligibility summary -- IVF
cat("=== IVF / LIVE BIRTH -- INELIGIBILITY SUMMARY ===\n")
df_livebirth %>%
  summarize(
    n_total        = n(),
    n_ineligible   = sum(ineligible_lb, na.rm = TRUE),
    pct_ineligible = mean(ineligible_lb, na.rm = TRUE),
    mean_failure_prob_ineligible = mean(failure_prob_lb[ineligible_lb], na.rm = TRUE),
    mean_age_ineligible = mean(age[ineligible_lb], na.rm = TRUE),
    mean_amh_ineligible = mean(amh[ineligible_lb], na.rm = TRUE),
    mean_afc_ineligible = mean(afc[ineligible_lb], na.rm = TRUE)
  ) %>%
  print()

# Ineligible profile breakdown -- IVF
cat("\n--- IVF Ineligible Patient Profile ---\n")
df_livebirth %>%
  filter(ineligible_lb) %>%
  summarize(
    n = n(),
    mean_age = mean(age, na.rm = TRUE),  median_age = median(age, na.rm = TRUE),
    min_age = min(age, na.rm = TRUE),    max_age = max(age, na.rm = TRUE),
    mean_amh = mean(amh, na.rm = TRUE),  median_amh = median(amh, na.rm = TRUE),
    mean_afc = mean(afc, na.rm = TRUE),  median_afc = median(afc, na.rm = TRUE),
    mean_success_prob = mean(p_livebirth, na.rm = TRUE),
    median_success_prob = median(p_livebirth, na.rm = TRUE),
    mean_failure_prob = mean(failure_prob_lb, na.rm = TRUE),
    mean_uncapped_prem = mean(uncapped_premium_lb, na.rm = TRUE)
  ) %>%
  print()

# Worst performers -- IVF (bottom 10% success probability)
cat("\n--- IVF Worst Performers (bottom 10% success probability) ---\n")
cat("10th percentile success-probability cutoff:",
    quantile(df_livebirth$p_livebirth, 0.10, na.rm = TRUE), "\n\n")

ivf_worst <- df_livebirth %>%
  filter(worst_performer_lb) %>%
  transmute(
    patient_id, age, amh, afc,
    success_prob     = p_livebirth,
    failure_prob     = failure_prob_lb,
    uncapped_premium = uncapped_premium_lb,
    cap_binds        = cap_binds_lb,
    ineligible       = ineligible_lb
  ) %>%
  arrange(success_prob)

# print(ivf_worst, n = Inf)

# ============================================================
# PART 2: EUPLOID EMBRYO
# ------------------------------------------------------------
# Uses the calibrated euploid probability (p_cal_eup) already on
# df_embryo from script 05 -- no separate model needed.
# ============================================================

df_euploid <- df_embryo %>%
  mutate(
    failure_prob_eu     = 1 - p_cal_eup,
    uncapped_premium_eu = failure_prob_eu * cost_euploid * combined_load,
    cap_level_eu        = cap_pct * cost_euploid,
    cap_binds_eu        = uncapped_premium_eu > cap_level_eu,
    ineligible_eu       = cap_binds_eu,
    p10_cutoff_eu       = quantile(p_cal_eup, 0.10, na.rm = TRUE),
    worst_performer_eu  = p_cal_eup <= p10_cutoff_eu
  )

# Overall ineligibility summary -- euploid
cat("\n=== EUPLOID EMBRYO -- INELIGIBILITY SUMMARY ===\n")
df_euploid %>%
  summarize(
    n_total        = n(),
    n_ineligible   = sum(ineligible_eu, na.rm = TRUE),
    pct_ineligible = mean(ineligible_eu, na.rm = TRUE),
    mean_failure_prob_ineligible = mean(failure_prob_eu[ineligible_eu], na.rm = TRUE),
    mean_age_ineligible = mean(age[ineligible_eu], na.rm = TRUE),
    mean_amh_ineligible = mean(amh[ineligible_eu], na.rm = TRUE),
    mean_afc_ineligible = mean(afc[ineligible_eu], na.rm = TRUE)
  ) %>%
  print()

# Ineligible profile breakdown -- euploid
cat("\n--- Euploid Ineligible Patient Profile ---\n")
df_euploid %>%
  filter(ineligible_eu) %>%
  summarize(
    n = n(),
    mean_age = mean(age, na.rm = TRUE),  median_age = median(age, na.rm = TRUE),
    min_age = min(age, na.rm = TRUE),    max_age = max(age, na.rm = TRUE),
    mean_amh = mean(amh, na.rm = TRUE),  median_amh = median(amh, na.rm = TRUE),
    mean_afc = mean(afc, na.rm = TRUE),  median_afc = median(afc, na.rm = TRUE),
    mean_success_prob = mean(p_cal_eup, na.rm = TRUE),
    median_success_prob = median(p_cal_eup, na.rm = TRUE),
    mean_failure_prob = mean(failure_prob_eu, na.rm = TRUE),
    mean_uncapped_prem = mean(uncapped_premium_eu, na.rm = TRUE)
  ) %>%
  print()

# Worst performers -- euploid (bottom 10% success probability)
cat("\n--- Euploid Worst Performers (bottom 10% success probability) ---\n")
cat("10th percentile success-probability cutoff:",
    quantile(df_euploid$p_cal_eup, 0.10, na.rm = TRUE), "\n\n")

euploid_worst <- df_euploid %>%
  filter(worst_performer_eu) %>%
  transmute(
    patient_id, age, amh, afc,
    success_prob     = p_cal_eup,
    failure_prob     = failure_prob_eu,
    uncapped_premium = uncapped_premium_eu,
    cap_binds        = cap_binds_eu,
    ineligible       = ineligible_eu
  ) %>%
  arrange(success_prob)

print(euploid_worst, n = Inf)

# ── Write patient-level worst-performer lists as deliverables ─
dir.create("outputs", showWarnings = FALSE)
readr::write_csv(ivf_worst,     "outputs/ivf_worst_performers.csv")
readr::write_csv(euploid_worst, "outputs/euploid_worst_performers.csv")

cat("\nSaved: outputs/ivf_worst_performers.csv, outputs/euploid_worst_performers.csv\n")
