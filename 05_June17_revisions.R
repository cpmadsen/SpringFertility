# ============================================================
# INELIGIBLE PATIENT PROFILES & WORST PERFORMER SUMMARY
# IVF (Live Birth) and Euploid Embryo products only
# ============================================================

library(dplyr)

# ── Parameters ───────────────────────────────────────────────

cycle_cost_ivf     <- 31555    # replace with actual IVF cycle cost from Excel C24
cycle_cost_euploid <- 21689    # replace with actual euploid cycle cost from Excel
combined_load      <- 1.38     # replace with actual combined load factor
cap_pct            <- 0.50     # 50% premium cap — do not change

# ============================================================
# PART 1: IVF / LIVE BIRTH
# ============================================================

df_livebirth <- df_livebirth %>%
  mutate(
    failure_prob_lb      = 1 - p_cal_lb,
    uncapped_premium_lb  = failure_prob_lb * cycle_cost_ivf * combined_load,
    cap_level_lb         = cap_pct * cycle_cost_ivf,
    cap_binds_lb         = uncapped_premium_lb > cap_level_lb,
    ineligible_lb        = cap_binds_lb,
    p10_cutoff_lb        = quantile(p_cal_lb, 0.10, na.rm=TRUE),
    worst_performer_lb   = p_cal_lb <= p10_cutoff_lb
  )

# Overall ineligibility summary — IVF
cat("=== IVF / LIVE BIRTH — INELIGIBILITY SUMMARY ===\n")
df_livebirth %>%
  summarize(
    n_total              = n(),
    n_ineligible         = sum(ineligible_lb, na.rm=TRUE),
    pct_ineligible       = mean(ineligible_lb, na.rm=TRUE),
    mean_failure_prob_ineligible = mean(failure_prob_lb[ineligible_lb], na.rm=TRUE),
    mean_age_ineligible  = mean(age_model[ineligible_lb], na.rm=TRUE),
    mean_amh_ineligible  = mean(amh[ineligible_lb], na.rm=TRUE),
    mean_afc_ineligible  = mean(afc[ineligible_lb], na.rm=TRUE)
  ) %>%
  print()

# Ineligible profile breakdown — IVF
cat("\n--- IVF Ineligible Patient Profile ---\n")
df_livebirth %>%
  filter(ineligible_lb) %>%
  summarize(
    n                    = n(),
    mean_age             = mean(age_model, na.rm=TRUE),
    median_age           = median(age_model, na.rm=TRUE),
    min_age              = min(age_model, na.rm=TRUE),
    max_age              = max(age_model, na.rm=TRUE),
    mean_amh             = mean(amh, na.rm=TRUE),
    median_amh           = median(amh, na.rm=TRUE),
    mean_afc             = mean(afc, na.rm=TRUE),
    median_afc           = median(afc, na.rm=TRUE),
    mean_success_prob    = mean(p_cal_lb, na.rm=TRUE),
    median_success_prob  = median(p_cal_lb, na.rm=TRUE),
    mean_failure_prob    = mean(failure_prob_lb, na.rm=TRUE),
    mean_uncapped_prem   = mean(uncapped_premium_lb, na.rm=TRUE),
    median_uncapped_prem = median(uncapped_premium_lb, na.rm=TRUE)
  ) %>%
  print()

# Worst performer summary — IVF
cat("\n--- IVF Worst Performers (bottom 10% success probability) — Summary ---\n")
cat("10th percentile success probability cutoff:",
    quantile(df_livebirth$p_cal_lb, 0.10, na.rm=TRUE), "\n\n")

df_livebirth %>%
  filter(worst_performer_lb) %>%
  summarize(
    n                    = n(),
    mean_age             = mean(age_model, na.rm=TRUE),
    median_age           = median(age_model, na.rm=TRUE),
    min_age              = min(age_model, na.rm=TRUE),
    max_age              = max(age_model, na.rm=TRUE),
    mean_amh             = mean(amh, na.rm=TRUE),
    median_amh           = median(amh, na.rm=TRUE),
    mean_afc             = mean(afc, na.rm=TRUE),
    median_afc           = median(afc, na.rm=TRUE),
    mean_success_prob    = mean(p_cal_lb, na.rm=TRUE),
    median_success_prob  = median(p_cal_lb, na.rm=TRUE),
    mean_failure_prob    = mean(failure_prob_lb, na.rm=TRUE),
    mean_uncapped_prem   = mean(uncapped_premium_lb, na.rm=TRUE),
    pct_also_ineligible  = mean(ineligible_lb, na.rm=TRUE)
  ) %>%
  print()

# Worst performer patient-level list — IVF
cat("\n--- IVF Worst Performers (bottom 10% success probability) — Patient List ---\n")
df_livebirth %>%
  filter(worst_performer_lb) %>%
  select(
    patient_id,
    age          = age_model,
    amh,
    afc,
    success_prob = p_cal_lb,
    failure_prob = failure_prob_lb,
    uncapped_premium = uncapped_premium_lb,
    cap_binds    = cap_binds_lb,
    ineligible   = ineligible_lb
  ) %>%
  arrange(success_prob) %>%
  print(n = Inf)

# ============================================================
# PART 2: EUPLOID EMBRYO
# ============================================================

df_euploid <- df_euploid %>%
  mutate(
    failure_prob_eu      = 1 - p_cal_euploid,
    uncapped_premium_eu  = failure_prob_eu * cycle_cost_euploid * combined_load,
    cap_level_eu         = cap_pct * cycle_cost_euploid,
    cap_binds_eu         = uncapped_premium_eu > cap_level_eu,
    ineligible_eu        = cap_binds_eu,
    p10_cutoff_eu        = quantile(p_cal_euploid, 0.10, na.rm=TRUE),
    worst_performer_eu   = p_cal_euploid <= p10_cutoff_eu
  )

# Overall ineligibility summary — euploid
cat("\n=== EUPLOID EMBRYO — INELIGIBILITY SUMMARY ===\n")
df_euploid %>%
  summarize(
    n_total              = n(),
    n_ineligible         = sum(ineligible_eu, na.rm=TRUE),
    pct_ineligible       = mean(ineligible_eu, na.rm=TRUE),
    mean_failure_prob_ineligible = mean(failure_prob_eu[ineligible_eu], na.rm=TRUE),
    mean_age_ineligible  = mean(age_model[ineligible_eu], na.rm=TRUE),
    mean_amh_ineligible  = mean(amh[ineligible_eu], na.rm=TRUE),
    mean_afc_ineligible  = mean(afc[ineligible_eu], na.rm=TRUE)
  ) %>%
  print()

# Ineligible profile breakdown — euploid
cat("\n--- Euploid Ineligible Patient Profile ---\n")
df_euploid %>%
  filter(ineligible_eu) %>%
  summarize(
    n                    = n(),
    mean_age             = mean(age_model, na.rm=TRUE),
    median_age           = median(age_model, na.rm=TRUE),
    min_age              = min(age_model, na.rm=TRUE),
    max_age              = max(age_model, na.rm=TRUE),
    mean_amh             = mean(amh, na.rm=TRUE),
    median_amh           = median(amh, na.rm=TRUE),
    mean_afc             = mean(afc, na.rm=TRUE),
    median_afc           = median(afc, na.rm=TRUE),
    mean_success_prob    = mean(p_cal_euploid, na.rm=TRUE),
    median_success_prob  = median(p_cal_euploid, na.rm=TRUE),
    mean_failure_prob    = mean(failure_prob_eu, na.rm=TRUE),
    mean_uncapped_prem   = mean(uncapped_premium_eu, na.rm=TRUE),
    median_uncapped_prem = median(uncapped_premium_eu, na.rm=TRUE)
  ) %>%
  print()

# Worst performer summary — euploid
cat("\n--- Euploid Worst Performers (bottom 10% success probability) — Summary ---\n")
cat("10th percentile success probability cutoff:",
    quantile(df_euploid$p_cal_euploid, 0.10, na.rm=TRUE), "\n\n")

df_euploid %>%
  filter(worst_performer_eu) %>%
  summarize(
    n                    = n(),
    mean_age             = mean(age_model, na.rm=TRUE),
    median_age           = median(age_model, na.rm=TRUE),
    min_age              = min(age_model, na.rm=TRUE),
    max_age              = max(age_model, na.rm=TRUE),
    mean_amh             = mean(amh, na.rm=TRUE),
    median_amh           = median(amh, na.rm=TRUE),
    mean_afc             = mean(afc, na.rm=TRUE),
    median_afc           = median(afc, na.rm=TRUE),
    mean_success_prob    = mean(p_cal_euploid, na.rm=TRUE),
    median_success_prob  = median(p_cal_euploid, na.rm=TRUE),
    mean_failure_prob    = mean(failure_prob_eu, na.rm=TRUE),
    mean_uncapped_prem   = mean(uncapped_premium_eu, na.rm=TRUE),
    pct_also_ineligible  = mean(ineligible_eu, na.rm=TRUE)
  ) %>%
  print()

# Worst performer patient-level list — euploid
cat("\n--- Euploid Worst Performers (bottom 10% success probability) — Patient List ---\n")
df_euploid %>%
  filter(worst_performer_eu) %>%
  select(
    patient_id,
    age          = age_model,
    amh,
    afc,
    success_prob = p_cal_euploid,
    failure_prob = failure_prob_eu,
    uncapped_premium = uncapped_premium_eu,
    cap_binds    = cap_binds_eu,
    ineligible   = ineligible_eu
  ) %>%
  arrange(success_prob) %>%
  print(n = Inf)