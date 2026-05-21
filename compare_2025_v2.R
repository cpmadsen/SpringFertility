library(dplyr)
library(magrittr)
library(tidyr)
library(purrr)

output_log = list()

# Assumes the 2025 data is loaded as df_2025
df_2025 = openxlsx::read.xlsx("data/spring_2025/2025 outcomes for risk premium validation.xlsx")

# and the fitted models are livebirth_model, euploid_model, egg1_model
livebirth_model = readr::read_rds("data/models/livebirth_model_revised_modeling_statistics_end.rds")
euploid_model = readr::read_rds("data/models/euploid_model_revised_modeling_statistics_end.rds")
egg1_model = readr::read_rds("data/models/egg_1_model_revised_modeling_statistics_end.rds")
egg2_model = readr::read_rds("data/models/egg_2_model_revised_modeling_statistics_end.rds")

# Step 0: Align datatype between models and Spring's 2025 data.
df_2025 = df_2025 |> 
  dplyr::mutate(amh = as.numeric(amh),
                afc = as.numeric(afc),
                num_euploid_embryos = as.numeric(num_euploid_embryos),
                num_blast = as.numeric(num_blast),
                num_m2_eggs = as.numeric(num_m2_eggs)) |> 
  dplyr::rename(patient_id = id)

# ══════════════════════════════════════════════════════════════════════════════
# EGG FREEZING — Oocyte Cryopreservation cases
# ══════════════════════════════════════════════════════════════════════════════

output_log = output_log |> append(list("=== EGG FREEZING ANALYSIS ==="))

df_egg = df_2025 |> 
  dplyr::filter(stringr::str_detect(treatment, 'Oocyte Cryopreservation')) |> 
  tidyr::as_tibble()

cat("\n[Egg Freezing] N cases:", nrow(df_egg), "\n")
output_log = output_log |> append(list(paste0("[Egg Freezing] N cases: ", nrow(df_egg))))

df_egg <- df_egg |>
  mutate(euploid_success_bool = as.integer(num_euploid_embryos > 0))

df_egg |> 
  dplyr::count(euploid_success_bool)

# Population shift check
df_egg %>%
  summarize(
    mean_age = mean(age, na.rm=T), median_age = median(age, na.rm=T),
    mean_amh = mean(amh, na.rm=T), median_amh = median(amh, na.rm=T),
    mean_afc = mean(afc, na.rm=T), median_afc = median(afc, na.rm=T),
    mean_m2  = mean(num_m2_eggs, na.rm=T),
    n = n()
  ) %>%
  print()

output_log = output_log |> append(list(df_egg %>%
                                         summarize(
                                           mean_age = mean(age, na.rm=T), median_age = median(age, na.rm=T),
                                           mean_amh = mean(amh, na.rm=T), median_amh = median(amh, na.rm=T),
                                           mean_afc = mean(afc, na.rm=T), median_afc = median(afc, na.rm=T),
                                           mean_m2  = mean(num_m2_eggs, na.rm=T),
                                           n = n()
                                         )))

# Generate payout review list for egg freezing
payout_review_egg <- df_egg %>%
  mutate(
    mu_hat = predict(egg1_model, newdata=., type="response"),
    
    threshold_90 = map_dbl(row_number(), ~{
      floor(qnbinom(0.10, mu=mu_hat[.x], size=egg1_model$theta))
    }),
    
    triggers_payout = (num_m2_eggs < threshold_90),
    model_predicted_low_risk = (mu_hat >= 8),
    surprise_failure = triggers_payout & model_predicted_low_risk
    
  ) %>%
  filter(triggers_payout) %>%
  dplyr::select(patient_id, age, amh, afc, mu_hat, threshold_90,
                num_m2_eggs, triggers_payout, surprise_failure) %>%
  arrange(desc(surprise_failure), mu_hat) |> 
  tidyr::as_tibble()

print(payout_review_egg)
output_log = output_log |> append(list(payout_review_egg))

cat("\n[Egg Freezing] Total payout cases:", nrow(payout_review_egg))
cat("\n[Egg Freezing] Surprise failures:", sum(payout_review_egg$surprise_failure))
output_log = output_log |> append(list(paste0("[Egg Freezing] Total payout cases: ", nrow(payout_review_egg))))
output_log = output_log |> append(list(paste0("[Egg Freezing] Surprise failures: ", sum(payout_review_egg$surprise_failure))))

# ══════════════════════════════════════════════════════════════════════════════
# EMBRYO GUARANTEE — Dual Stim / ICSI / Embryo Cryo
# ══════════════════════════════════════════════════════════════════════════════

output_log = output_log |> append(list("=== EMBRYO GUARANTEE ANALYSIS ==="))

df_embryo = df_2025 |> 
  dplyr::filter(stringr::str_detect(treatment, 'Dual [Ss]tim|ICSI|Embryo [Cc]ryo'))

cat("\n[Embryo Guarantee] N cases:", nrow(df_embryo), "\n")
output_log = output_log |> append(list(paste0("[Embryo Guarantee] N cases: ", nrow(df_embryo))))

# Step 1: Get predictions on embryo data
df_embryo$p_raw_eup <- predict(euploid_model, newdata=df_embryo, type="response")

# Calibrate using April 2026 parameters
logit <- function(p) log(p/(1-p))
df_embryo$p_cal_eup <- plogis(0.0152 + 0.8508 * logit(df_embryo$p_raw_eup))

# Step 2: Check overall calibration
cat("\n[Embryo - Euploid] Predicted mean:", mean(df_embryo$p_cal_eup, na.rm=TRUE))
cat("\n[Embryo - Euploid] Observed mean:", mean(df_embryo$num_euploid_embryos, na.rm=TRUE))
output_log = output_log |> append(list(paste0("[Embryo - Euploid] Predicted mean: ", mean(df_embryo$p_cal_eup, na.rm=TRUE))))
output_log = output_log |> append(list(paste0("[Embryo - Euploid] Observed mean: ", mean(df_embryo$num_euploid_embryos, na.rm=TRUE))))

# Step 3: Calibration by decile
df_embryo$decile <- ntile(df_embryo$p_cal_eup, 10)
calib_check_embryo <- df_embryo %>%
  group_by(decile) %>%
  summarize(
    mean_predicted = mean(p_cal_eup, na.rm=T),
    mean_observed_euploid = mean(num_euploid_embryos, na.rm=T),
    mean_observed_blasts  = mean(num_blast, na.rm=T),
    n = n()
  )
print(calib_check_embryo)
output_log = output_log |> append(list(calib_check_embryo))

# Step 4: AUC
library(pROC)
roc_embryo <- roc(as.integer(df_embryo$num_euploid_embryos >= 1), df_embryo$p_cal_eup, quiet=TRUE)
cat("\n[Embryo - Euploid] AUC:", auc(roc_embryo))
output_log = output_log |> append(list(paste0("[Embryo - Euploid] AUC: ", auc(roc_embryo))))

# Step 5: Population shift check
df_embryo %>%
  summarize(
    mean_age = mean(age, na.rm=T), median_age = median(age, na.rm=T),
    mean_amh = mean(amh, na.rm=T), median_amh = median(amh, na.rm=T),
    mean_afc = mean(afc, na.rm=T), median_afc = median(afc, na.rm=T),
    mean_blasts  = mean(num_blast, na.rm=T),
    mean_euploid = mean(num_euploid_embryos, na.rm=T),
    n = n()
  ) %>% print()

output_log = output_log |> append(list(df_embryo %>%
                                         summarize(
                                           mean_age = mean(age, na.rm=T), median_age = median(age, na.rm=T),
                                           mean_amh = mean(amh, na.rm=T), median_amh = median(amh, na.rm=T),
                                           mean_afc = mean(afc, na.rm=T), median_afc = median(afc, na.rm=T),
                                           mean_blasts  = mean(num_blast, na.rm=T),
                                           mean_euploid = mean(num_euploid_embryos, na.rm=T),
                                           n = n()
                                         )))


# Additional checks etc. from Lara
output_log = output_log |> append(list("=== NEW SECTION - ADDITIONAL CHECKS FROM LARA ==="))

# ============================================================
# PART 1: EUPLOID MODEL - COMPARISON
# ============================================================

# Fix observed outcome to binary (did they get at least one euploid embryo?)
df_embryo <- df_embryo %>%
  mutate(euploid_success_bool = as.integer(num_euploid_embryos > 0))

# Overall calibration
cat("=== EUPLOID MODEL CALIBRATION ===\n")
cat("Predicted mean (probability):", mean(df_embryo$p_cal_eup, na.rm=TRUE), "\n")
cat("Observed mean (proportion with >=1 euploid):", mean(df_embryo$euploid_success_bool, na.rm=TRUE), "\n")

output_log = output_log |> append(list("=== EUPLOID MODEL CALIBRATION ===\n"))
output_log = output_log |> append(list(paste0("Predicted mean (probability):", mean(df_embryo$p_cal_eup, na.rm=TRUE), "\n")))
output_log = output_log |> append(list(paste0("Observed mean (proportion with >=1 euploid):", mean(df_embryo$euploid_success_bool, na.rm=TRUE), "\n")))

# Calibration by decile
df_embryo$decile <- ntile(df_embryo$p_cal_eup, 10)
euploid_calib <- df_embryo %>%
  group_by(decile) %>%
  summarize(
    mean_predicted = mean(p_cal_eup, na.rm=TRUE),
    mean_observed = mean(euploid_success_bool, na.rm=TRUE),
    n = n()
  )
print(euploid_calib)

output_log = output_log |> append(list(euploid_calib))

# AUC
library(pROC)
roc_euploid <- roc(df_embryo$euploid_success_bool, df_embryo$p_cal_eup)
cat("AUC (euploid model):", auc(roc_euploid), "\n")

output_log = output_log |> append(list(paste0("AUC (euploid model):", auc(roc_euploid), "\n")))

# ============================================================
# PART 2: EGG MODEL - THRESHOLD ACCURACY AND SURPRISE PAYOUTS
# ============================================================

cat("\n=== EGG MODEL THRESHOLD VALIDATION ===\n")

output_log = output_log |> append(list("\n=== EGG MODEL THRESHOLD VALIDATION ===\n"))

df_egg <- df_egg %>%
  mutate(
    mu_hat = predict(egg1_model, newdata=., type="response"),
    threshold_90 = floor(qnbinom(0.10,
                                 mu = mu_hat,
                                 size = egg1_model$theta)),
    threshold_met = (num_m2_eggs >= threshold_90),
    triggers_payout = !threshold_met,
    model_predicted_low_risk = (mu_hat >= 8),
    surprise_failure = triggers_payout & model_predicted_low_risk
  )

# Overall threshold accuracy
cat("\n--- Threshold Accuracy ---\n")
output_log = output_log |> append(list("\n--- Threshold Accuracy ---\n"))

df_egg %>%
  summarize(
    n = n(),
    n_met_threshold = sum(threshold_met, na.rm=TRUE),
    n_payout = sum(triggers_payout, na.rm=TRUE),
    pct_met_threshold = mean(threshold_met, na.rm=TRUE),
    pct_payout = mean(triggers_payout, na.rm=TRUE)
  ) %>%
  print()

output_log = output_log |> append(list(
  df_egg %>%
    summarize(
      n = n(),
      n_met_threshold = sum(threshold_met, na.rm=TRUE),
      n_payout = sum(triggers_payout, na.rm=TRUE),
      pct_met_threshold = mean(threshold_met, na.rm=TRUE),
      pct_payout = mean(triggers_payout, na.rm=TRUE)
    )
))

# Threshold accuracy by decile
cat("\n--- Threshold Accuracy by Decile ---\n")
output_log = output_log |> append(list("\n--- Threshold Accuracy by Decile ---\n"))

df_egg$decile <- ntile(df_egg$mu_hat, 10)
egg_calib <- df_egg %>%
  group_by(decile) %>%
  summarize(
    mean_mu_hat = mean(mu_hat, na.rm=TRUE),
    mean_observed_eggs = mean(num_m2_eggs, na.rm=TRUE),
    mean_threshold_90 = mean(threshold_90, na.rm=TRUE),
    pct_met_threshold = mean(threshold_met, na.rm=TRUE),
    n_payout = sum(triggers_payout, na.rm=TRUE),
    n = n()
  )
print(egg_calib)

output_log = output_log |> append(list(egg_calib))

# Surprise payout review list
cat("\n--- Surprise Payout Cases ---\n")
output_log = output_log |> append(list("\n--- Surprise Payout Cases ---\n"))

payout_review <- df_egg %>%
  filter(triggers_payout) %>%
  select(patient_id, age, amh, afc, mu_hat, threshold_90,
         num_m2_eggs, triggers_payout, surprise_failure) %>%
  arrange(desc(surprise_failure), mu_hat)

print(payout_review)
output_log = output_log |> append(list(payout_review))


cat("\nTotal payout cases:", nrow(payout_review), "\n")
output_log = output_log |> append(list(paste0("\nTotal payout cases:", nrow(payout_review), "\n")))

cat("Surprise failures (model expected good outcome):",
    sum(payout_review$surprise_failure), "\n")

output_log = output_log |> append(list(paste0("Surprise failures (model expected good outcome):",
                                              sum(payout_review$surprise_failure), "\n")))

cat("Payout rate:", mean(df_egg$triggers_payout, na.rm=TRUE), "\n")
output_log = output_log |> append(list(paste0("Payout rate:", mean(df_egg$triggers_payout, na.rm=TRUE), "\n")))

# ── Write output log ───────────────────────────────────────────────────────────

file_name <- "outputs/comparison_models_to_spring_2025_data.txt"
cat("", file = file_name)

for (i in 1:length(output_log)) {
  item <- output_log[[i]]
  cat('\n', file = file_name, append = TRUE, sep = "\n")
  
  if (is.data.frame(item) || is.matrix(item)) {
    write.table(item, file = file_name, append = TRUE, sep = "\t", 
                row.names = FALSE, quote = FALSE)
  } else {
    cat(item, file = file_name, append = TRUE, sep = "\n")
  }
}

