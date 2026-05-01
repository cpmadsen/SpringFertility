############################################################
# DATA PREPARATION, EVENT CHECKING, AGE POOLING,
# TRAIN/VALIDATION SPLIT, REFIT & RECALIBRATION
#
# Version: 1.0 | April 30, 2026
#
# PURPOSE:
# This script should be run BEFORE the master pricing script
# (fertility_master_v4.R). It prepares the data correctly,
# checks that event rates are sufficient, implements the
# age pooling decision, splits data for honest calibration,
# and produces a summary table of what changed and why.
#
# RUN ORDER:
#   1. This script  --> produces cleaned, split data frames
#   2. fertility_master_v4.R --> fits models and builds Excel
#
# !!!! NOTE: Read every section header before running.
# Sections marked !! DECISION REQUIRED !! need your input
# before proceeding. Do not skip them.
############################################################

library(dplyr)
library(tidyr)
library(caret)      # for createDataPartition (stratified split)

############################################################
# 0.  INPUTS
# ══════════════════════════════════════════════════════════
# !!!! NOTE: Set your data frame names and outcome column names
# here. These are the only values you should change in this
# script. Everything else flows from them automatically.
############################################################

# ── 0A. Raw data frames ───────────────────────────────────
# !!!! NOTE: Replace with your actual data frame names.
# These must already be loaded in your R environment.

raw_livebirth <- your_livebirth_df
raw_euploid   <- your_euploid_df
raw_egg1      <- your_egg1_df
raw_egg2      <- your_egg2_df

# ── 0B. Column names ──────────────────────────────────────
# !!!! NOTE: Confirm these match your actual column names exactly.
# 

col_outcome_lb  <- "live_birth"         # binary 0/1
col_outcome_eu  <- "euploid_success"    # binary 0/1
col_outcome_e1  <- "eggs_retrieved"     # integer count
col_outcome_e2  <- "total_eggs_2_cycles" # integer count
col_age         <- "age"
col_amh         <- "amh"
col_afc         <- "afc"
col_eggs_cy1    <- "eggs_cycle1"        # egg2 model only

# !! DECISION REQUIRED !!
# -- 0C. Age floor---------------------------------------------
# !!!! NOTE: Set the minimum age below which patients are pooled.
# Recommended: 28 (n=37 at age 28, clinically distinct from 30)
# Conservative: 30 (n=125, stronger statistical stability)
# Reasoning: ages below this threshold have too few observations
# to support the interaction terms in the model. All patients
# below age_floor will be assigned age_floor for model purposes.
# Their TRUE age is preserved in a separate column for audit.

age_floor <- 28

# !! DECISION REQUIRED !!
# -- 0D. Train/validation split---------------------------------
# !!!! NOTE: Set the proportion of data used for TRAINING.
# The remainder becomes the validation set for calibration.
# 0.80 = 80% train / 20% validation (recommended for N~5500)
# If event counts in Section 1 show <100 events in validation,
# switch to cross-validation (cv_mode <- TRUE below).

train_pct <- 0.80
cv_mode   <- FALSE   # set TRUE to use 5-fold CV instead of split
cv_folds  <- 5       # only used if cv_mode = TRUE
random_seed <- 42    # fix for reproducibility — do not change between runs

############################################################
# 1. EVENT RATE CHECK
# ==========================================================
# PURPOSE:
# Before splitting or fitting anything, we need to know:
#   (a) How many events (outcome = 1) exist in each dataset
#   (b) Whether a train/validation split will leave enough events
#       in the validation set for stable calibration
#
# WHAT TO LOOK FOR:
#   events_total     : must be > 200 to support interaction models
#   events_in_val    : must be >= 100 for stable calibration slope
#   event_rate       : very low rates (<10%) need extra caution
#
# IF events_in_val < 100:
#   -> Set cv_mode <- TRUE in Section 0D above
#   -> Cross-validation uses all data for both training and
#     validation across folds, wasting nothing
#
# IF events_total < 200:
#   -> The model may be overfit regardless of approach
#   -> Flag to clinical team — more data needed before production, or other assumptions
############################################################

message("\n================================================")
message("SECTION 1: EVENT RATE CHECK")
message("==================================================")

check_events <- function(df, outcome_col, model_name, train_p) {
  outcome <- df[[outcome_col]]
  n_total       <- nrow(df)
  events_total  <- sum(outcome, na.rm = TRUE)
  event_rate    <- mean(outcome, na.rm = TRUE)
  n_val         <- round(n_total * (1 - train_p))
  events_in_val <- round(events_total * (1 - train_p))
  
  flag_total <- if (events_total < 200) "!! LOW — consider more data" else "OK"
  flag_val   <- if (events_in_val < 100) "!! LOW — consider cv_mode=TRUE" else "OK"
  
  data.frame(
    Model          = model_name,
    N_total        = n_total,
    Events_total   = events_total,
    Event_rate_pct = round(event_rate * 100, 1),
    N_validation   = n_val,
    Events_in_val  = events_in_val,
    Flag_total     = flag_total,
    Flag_val       = flag_val,
    stringsAsFactors = FALSE
  )
}

event_check_table <- bind_rows(
  check_events(raw_livebirth, col_outcome_lb, "Live birth",   train_pct),
  check_events(raw_euploid,   col_outcome_eu, "Euploid",      train_pct),
  # Egg models are count outcomes — no binary event check needed
  # but we report N for completeness
  data.frame(
    Model="Egg 1-cycle", N_total=nrow(raw_egg1),
    Events_total=NA, Event_rate_pct=NA,
    N_validation=round(nrow(raw_egg1)*(1-train_pct)),
    Events_in_val=NA,
    Flag_total="Count model — no binary event check",
    Flag_val="N/A",
    stringsAsFactors=FALSE
  ),
  data.frame(
    Model="Egg 2-cycle", N_total=nrow(raw_egg2),
    Events_total=NA, Event_rate_pct=NA,
    N_validation=round(nrow(raw_egg2)*(1-train_pct)),
    Events_in_val=NA,
    Flag_total="Count model — no binary event check",
    Flag_val="N/A",
    stringsAsFactors=FALSE
  )
)

message("\nEvent rate summary:")
print(event_check_table, row.names = FALSE)

# -- Age-specific event check -------------------------------
# !!! NOTE: This table shows event rates by age band.
# Pay attention to the <28 group specifically.
# If the event rate there is very different from ages 28-30,
# that confirms the pooling decision is appropriate.
# If it is similar, pooling is still justified by sample size alone.

message("\nEvent rates by age band (live birth):")
age_band_check_lb <- raw_livebirth %>%
  mutate(age_band = case_when(
    .data[[col_age]] < 28  ~ "22-27 (pooled)",
    .data[[col_age]] < 31  ~ "28-30",
    .data[[col_age]] < 35  ~ "31-34",
    .data[[col_age]] < 39  ~ "35-38",
    .data[[col_age]] < 43  ~ "39-42",
    TRUE                    ~ "43+"
  )) %>%
  group_by(age_band) %>%
  summarize(
    n            = n(),
    events       = sum(.data[[col_outcome_lb]], na.rm = TRUE),
    event_rate   = round(mean(.data[[col_outcome_lb]], na.rm = TRUE) * 100, 1),
    .groups = "drop"
  ) %>%
  mutate(
    flag = case_when(
      n < 20  ~ "!! Very sparse — unreliable estimates",
      n < 100 ~ "! Thin — treat with caution",
      TRUE    ~ "OK"
    )
  )

print(age_band_check_lb, row.names = FALSE)

message("\nEvent rates by age band (euploid):")
age_band_check_eu <- raw_euploid %>%
  mutate(age_band = case_when(
    .data[[col_age]] < 28  ~ "22-27 (pooled)",
    .data[[col_age]] < 31  ~ "28-30",
    .data[[col_age]] < 31  ~ "31-34",
    .data[[col_age]] < 35  ~ "35-38",
    .data[[col_age]] < 39  ~ "39-42",
    TRUE                    ~ "43+"
  )) %>%
  group_by(age_band) %>%
  summarize(
    n          = n(),
    events     = sum(.data[[col_outcome_eu]], na.rm = TRUE),
    event_rate = round(mean(.data[[col_outcome_eu]], na.rm = TRUE) * 100, 1),
    .groups = "drop"
  ) %>%
  mutate(
    flag = case_when(
      n < 20  ~ "!! Very sparse — unreliable estimates",
      n < 100 ~ "! Thin — treat with caution",
      TRUE    ~ "OK"
    )
  )

print(age_band_check_eu, row.names = FALSE)

# --- Egg model count check --------------------------------------
# !!! NOTE: For egg models, instead of event rates we check
# whether the observed egg count distribution looks reasonable
# (not zero-inflated, not implausibly wide).

message("\nEgg 1-cycle count distribution:")
print(summary(raw_egg1[[col_outcome_e1]]))
message(sprintf("  Zero egg retrievals: %d (%.1f%%)",
                sum(raw_egg1[[col_outcome_e1]] == 0, na.rm=TRUE),
                mean(raw_egg1[[col_outcome_e1]] == 0, na.rm=TRUE)*100))

message("\nEgg 2-cycle count distribution:")
print(summary(raw_egg2[[col_outcome_e2]]))
message(sprintf("  Zero total eggs (2 cycles): %d (%.1f%%)",
                sum(raw_egg2[[col_outcome_e2]] == 0, na.rm=TRUE),
                mean(raw_egg2[[col_outcome_e2]] == 0, na.rm=TRUE)*100))

############################################################
# 2. AGE POOLING
# =========================================================
# PURPOSE:
# Patients below age_floor are assigned age_floor for all
# model computations. Their true age is retained in age_true
# for audit and reporting purposes.
#
# HOW THIS WORKS IN THE MODEL:
# The model formula uses age_model (not age directly).
# age_model is continuous for all patients >= age_floor.
# For patients < age_floor, age_model = age_floor (constant).
# This means the model interpolates normally above the floor
# and assigns floor-level predictions below it.
#
# IN THE EXCEL PRICING TOOL:
# The patient input cell uses =MAX(entered_age, age_floor)
# before passing age to the probability formula.
# This is already implemented in fertility_master_v4.R.
#
# WHAT TO LOOK FOR IN THE OUTPUT:
# n_pooled: number of patients affected by the floor.
# If this is > 5% of your total N, consider whether the
# floor is set too high and clinical justification holds.
############################################################

message("\n============================================")
message("SECTION 2: AGE POOLING")
message("==============================================")

apply_age_floor <- function(df, age_col, floor_val) {
  df %>%
    mutate(
      age_true  = .data[[age_col]],           # preserve original
      age_model = pmax(.data[[age_col]], floor_val)  # floored version
    )
}

df_livebirth <- apply_age_floor(raw_livebirth, col_age, age_floor)
df_euploid   <- apply_age_floor(raw_euploid,   col_age, age_floor)
df_egg1      <- apply_age_floor(raw_egg1,      col_age, age_floor)
df_egg2      <- apply_age_floor(raw_egg2,      col_age, age_floor)

# Report how many patients were affected
pooling_summary <- bind_rows(
  data.frame(Model="Live birth", stringsAsFactors=FALSE),
  data.frame(Model="Euploid",    stringsAsFactors=FALSE),
  data.frame(Model="Egg 1",      stringsAsFactors=FALSE),
  data.frame(Model="Egg 2",      stringsAsFactors=FALSE)
) %>%
  mutate(
    N_total   = c(nrow(df_livebirth), nrow(df_euploid),
                  nrow(df_egg1),      nrow(df_egg2)),
    N_pooled  = c(
      sum(df_livebirth$age_true < age_floor),
      sum(df_euploid$age_true   < age_floor),
      sum(df_egg1$age_true      < age_floor),
      sum(df_egg2$age_true      < age_floor)
    )
  ) %>%
  mutate(
    Pct_pooled = round(N_pooled / N_total * 100, 1),
    Flag = ifelse(Pct_pooled > 5,
                  "! >5% pooled — review age_floor clinical justification",
                  "OK")
  )

message(sprintf("\nAge floor applied: %d", age_floor))
message("Patients affected by age pooling:")
print(pooling_summary, row.names = FALSE)

############################################################
# 3. TRAIN / VALIDATION SPLIT
# =========================================================
# PURPOSE:
# Split each data frame into training and validation sets
# BEFORE fitting any models. The training set is used to
# fit the glm. The validation set is used ONLY for
# calibration — the model has never seen these patients,
# so calibration slope estimates are honest.
#
# WHY THIS MATTERS FOR PRICING:
# Calibration slope on training data is always biased toward
# 1.0 (the model looks better calibrated than it truly is).
# A slope on validation data is what you actually get when
# pricing a new patient who was not in your training data.
# Premiums based on training-data calibration are mispriced.
#
# STRATIFICATION:
# We stratify on the outcome (live_birth, euploid_success)
# so both train and validation sets have similar event rates.
# For count outcomes (egg models) we stratify on a binned
# version of the count.
#
# IF cv_mode = TRUE:
# Skip the split and use 5-fold cross-validation instead.
# Each fold acts as a mini validation set. Calibration is
# assessed on out-of-fold predictions pooled across all folds.
# Recommended when events_in_val < 100.
############################################################

message("\n================================================")
message("SECTION 3: TRAIN / VALIDATION SPLIT")
message("==================================================")

set.seed(random_seed)

if (!cv_mode) {
  
  message(sprintf("Split mode: %.0f%% train / %.0f%% validation",
                  train_pct*100, (1-train_pct)*100))
  
  # ── Binary outcome models (stratify on outcome) ──────────
  split_binary <- function(df, outcome_col, train_p) {
    idx <- caret::createDataPartition(
      df[[outcome_col]],
      p    = train_p,
      list = FALSE
    )
    list(train = df[ idx, ], val = df[-idx, ])
  }
  
  lb_split <- split_binary(df_livebirth, col_outcome_lb, train_pct)
  eu_split <- split_binary(df_euploid,   col_outcome_eu, train_pct)
  
  df_lb_train <- lb_split$train
  df_lb_val   <- lb_split$val
  df_eu_train <- eu_split$train
  df_eu_val   <- eu_split$val
  
  # -- Count outcome models (stratify on binned count) --------------
  # !!! NOTE: We bin the egg count into quartiles for stratification.
  # This ensures both train and val sets cover the full range of
  # egg counts, not just high or low retrievers.
  
  split_count <- function(df, outcome_col, train_p) {
    df <- df %>%
      mutate(.strat = ntile(.data[[outcome_col]], 4))
    idx <- caret::createDataPartition(df$.strat, p=train_p, list=FALSE)
    list(
      train = df[ idx, ] %>% select(-.strat),
      val   = df[-idx, ] %>% select(-.strat)
    )
  }
  
  e1_split <- split_count(df_egg1, col_outcome_e1, train_pct)
  e2_split <- split_count(df_egg2, col_outcome_e2, train_pct)
  
  df_e1_train <- e1_split$train
  df_e1_val   <- e1_split$val
  df_e2_train <- e2_split$train
  df_e2_val   <- e2_split$val
  
  # -- Confirm split balance ---------------------------------------
  # !!! NOTE: Check that event rates are similar across train/val.
  # Large differences (>3 percentage points) suggest the
  # stratification didn't balance well — rare with createDataPartition
  # but possible with very small samples.
  
  split_check <- data.frame(
    Model = c("Live birth — train", "Live birth — val",
              "Euploid — train",    "Euploid — val"),
    N = c(nrow(df_lb_train), nrow(df_lb_val),
          nrow(df_eu_train), nrow(df_eu_val)),
    Events = c(
      sum(df_lb_train[[col_outcome_lb]]),
      sum(df_lb_val[[col_outcome_lb]]),
      sum(df_eu_train[[col_outcome_eu]]),
      sum(df_eu_val[[col_outcome_eu]])
    ),
    stringsAsFactors = FALSE
  ) %>%
    mutate(
      Event_rate_pct = round(Events / N * 100, 1),
      Flag = ifelse(N < 50, "!! Very small set", "OK")
    )
  
  message("\nSplit balance check:")
  print(split_check, row.names = FALSE)
  
  # ~~!!!! IMPORTANT - DECISION POINT - SPOT CHECK !!!!~~ ##
  # !!! NOTE: If any Flag shows "!! Very small set", go back to
  # Section 0D and set cv_mode <- TRUE, then re-run this script.
  
} else {
  
  # -- Cross-validation mode ------------------------------------
  # !!! NOTE: In CV mode we do not create a fixed train/val split.
  # Instead, the refitting and calibration in Section 4 will use
  # createFolds() to generate out-of-fold predictions.
  # The full data frames (df_livebirth, df_euploid, etc.) are
  # passed to Section 4 directly.
  
  message("Cross-validation mode: 5-fold, stratified on outcome.")
  message("No fixed train/val split created.")
  message("Out-of-fold predictions will be used for calibration.")
  
  df_lb_train <- df_livebirth
  df_lb_val   <- NULL
  df_eu_train <- df_euploid
  df_eu_val   <- NULL
  df_e1_train <- df_egg1
  df_e1_val   <- NULL
  df_e2_train <- df_egg2
  df_e2_val   <- NULL
}

############################################################
# 4. REFIT MODELS AND RECALIBRATE
# ==========================================================
# PURPOSE:
# Fit models on training data only, then generate predictions
# on validation data only, then fit calibration models on
# those validation predictions.
#
# This is the correct order of operations for honest pricing:
#   fit on train -> predict on val -> calibrate on val
#
# For count models (egg1, egg2): we refit glm.nb and report
# the mean absolute error on validation as a calibration check.
# There is no logistic calibration for count models — instead
# we compare predicted mean vs. observed mean by age band.
#
# WHAT THE CALIBRATION OUTPUT MEANS:
# cal_intercept near 0   -> predicted probabilities are well centered
# cal_slope near 1       -> predicted probabilities scale correctly
# cal_slope > 1          -> model under-dispersed (probs too compressed)
# cal_slope < 1          -> model over-dispersed (probs too wide)
# Acceptable range for pricing: intercept (-0.2, 0.2), slope (0.8, 1.2)
############################################################

message("\n================================================")
message("SECTION 4: REFIT AND RECALIBRATE")
message("==================================================")

library(MASS)
library(pROC)

if (!cv_mode) {
  
  # -- Refit on training data --------------------------------------
  message("\nFitting models on training data...")
  
  # !!! NOTE: Model formulas use age_model (the floored version).
  # Do not change age_model back to age here.
  
  livebirth_model_refit <- glm(
    live_birth ~ age_model * amh * afc,
    data   = df_lb_train,
    family = binomial
  )
  
  euploid_model_refit <- glm(
    euploid_success ~ age_model + amh + afc + amh:afc,
    data   = df_eu_train,
    family = binomial
  )
  
  egg1_model_refit <- glm.nb(
    eggs_retrieved ~ age_model + amh + afc + age_model:afc,
    data = df_e1_train
  )
  
  egg2_model_refit <- glm.nb(
    total_eggs_2_cycles ~ age_model + amh + afc + eggs_cycle1 + amh:afc,
    data = df_e2_train
  )
  
  message("    Done.")
  
  # -- Predict on validation data -------------------------------------
  message("\nGenerating out-of-sample predictions on validation set...")
  
  df_lb_val$pred_raw <- predict(livebirth_model_refit,
                                newdata = df_lb_val,
                                type    = "response")
  df_eu_val$pred_raw <- predict(euploid_model_refit,
                                newdata = df_eu_val,
                                type    = "response")
  df_e1_val$pred_mu  <- predict(egg1_model_refit,
                                newdata = df_e1_val,
                                type    = "response")
  df_e2_val$pred_mu  <- predict(egg2_model_refit,
                                newdata = df_e2_val,
                                type    = "response")
  
  # -- Calibration — binary models -----------------------------------
  # !!! NOTE: These calibration models are fit on VALIDATION
  # predictions only. This is the honest calibration.
  # The intercept and slope extracted here go directly into
  # the Excel pricing formulas. They replace the in-sample
  # values from the previous version of the script.
  
  message("\nFitting calibration models on validation data...")
  
  cal_lb_refit <- glm(
    live_birth ~ log(pred_raw / (1 - pred_raw)),
    data   = df_lb_val,
    family = binomial
  )
  
  cal_eu_refit <- glm(
    euploid_success ~ log(pred_raw / (1 - pred_raw)),
    data   = df_eu_val,
    family = binomial
  )
  
  lb_cal_int_refit   <- round(coef(cal_lb_refit)[1], 8)
  lb_cal_slope_refit <- round(coef(cal_lb_refit)[2], 8)
  eu_cal_int_refit   <- round(coef(cal_eu_refit)[1], 8)
  eu_cal_slope_refit <- round(coef(cal_eu_refit)[2], 8)
  
  # -- Calibration — egg count models -----------------------------
  # !!! NOTE: For count models we report mean predicted vs.
  # observed egg counts by age band on the validation set.
  # There is no calibration intercept/slope for these models.
  # Look for abs_error < 1.0 across all age bands as acceptable.
  # Errors > 2.0 in any band indicate systematic miscalibration
  # for that patient group — flag for clinical review.
  
  egg_cal_check <- function(df_val, pred_col, outcome_col, age_col) {
    df_val %>%
      mutate(age_band = case_when(
        .data[[age_col]] < 28  ~ "22-27",
        .data[[age_col]] < 31  ~ "28-30",
        .data[[age_col]] < 35  ~ "31-34",
        .data[[age_col]] < 39  ~ "35-38",
        .data[[age_col]] < 43  ~ "39-42",
        TRUE                    ~ "43+"
      )) %>%
      group_by(age_band) %>%
      summarize(
        n              = n(),
        observed_mean  = round(mean(.data[[outcome_col]], na.rm=TRUE), 2),
        predicted_mean = round(mean(.data[[pred_col]],   na.rm=TRUE), 2),
        abs_error      = round(abs(observed_mean - predicted_mean), 2),
        flag           = ifelse(abs_error > 2.0,
                                "!! Miscalibrated — review",
                                ifelse(abs_error > 1.0, "! Check", "OK")),
        .groups = "drop"
      )
  }
  
  message("\nEgg 1-cycle calibration check (validation set):")
  egg1_cal_table <- egg_cal_check(df_e1_val, "pred_mu",
                                  col_outcome_e1, "age_model")
  print(egg1_cal_table, row.names = FALSE)
  
  message("\nEgg 2-cycle calibration check (validation set):")
  egg2_cal_table <- egg_cal_check(df_e2_val, "pred_mu",
                                  col_outcome_e2, "age_model")
  print(egg2_cal_table, row.names = FALSE)
  
} else {
  
  # -- Cross-validation calibration --------------------------------
  # !!! NOTE: In CV mode, we generate out-of-fold predictions
  # for live birth and euploid by fitting on k-1 folds and
  # predicting on the held-out fold, then pooling all
  # out-of-fold predictions for calibration.
  
  message("\nRunning 5-fold cross-validation for calibration...")
  
  run_cv_calibration <- function(df, outcome_col, formula_str, family, k=5) {
    folds <- caret::createFolds(df[[outcome_col]], k=k, list=TRUE)
    oof_pred <- numeric(nrow(df))
    
    for (i in seq_along(folds)) {
      val_idx   <- folds[[i]]
      train_df  <- df[-val_idx, ]
      val_df    <- df[ val_idx, ]
      
      fold_model <- glm(as.formula(formula_str), data=train_df, family=family)
      oof_pred[val_idx] <- predict(fold_model, newdata=val_df, type="response")
    }
    
    # Clip to avoid log(0) in calibration
    oof_pred <- pmax(pmin(oof_pred, 0.9999), 0.0001)
    df$oof_pred <- oof_pred
    
    cal_model <- glm(
      as.formula(paste0(outcome_col, " ~ log(oof_pred / (1 - oof_pred))")),
      data   = df,
      family = binomial
    )
    list(
      intercept = round(coef(cal_model)[1], 8),
      slope     = round(coef(cal_model)[2], 8),
      oof_pred  = oof_pred
    )
  }
  
  lb_cv <- run_cv_calibration(
    df_livebirth, col_outcome_lb,
    "live_birth ~ age_model * amh * afc",
    binomial
  )
  eu_cv <- run_cv_calibration(
    df_euploid, col_outcome_eu,
    "euploid_success ~ age_model + amh + afc + amh:afc",
    binomial
  )
  
  lb_cal_int_refit   <- lb_cv$intercept
  lb_cal_slope_refit <- lb_cv$slope
  eu_cal_int_refit   <- eu_cv$intercept
  eu_cal_slope_refit <- eu_cv$slope
  
  # In CV mode, refit final model on full data for production
  livebirth_model_refit <- glm(
    live_birth ~ age_model * amh * afc,
    data   = df_livebirth,
    family = binomial
  )
  euploid_model_refit <- glm(
    euploid_success ~ age_model + amh + afc + amh:afc,
    data   = df_euploid,
    family = binomial
  )
  egg1_model_refit <- glm.nb(
    eggs_retrieved ~ age_model + amh + afc + age_model:afc,
    data = df_egg1
  )
  egg2_model_refit <- glm.nb(
    total_eggs_2_cycles ~ age_model + amh + afc + eggs_cycle1 + amh:afc,
    data = df_egg2
  )
  
  message("    CV calibration complete.")
}

############################################################
# 5. SUMMARY OUTCOME TABLE
# ==========================================================
# PURPOSE:
# Produces a single consolidated table comparing:
#   - Pre vs. post recalibration (in-sample vs. honest)
#   - AUC, Brier score, calibration intercept and slope
#
# WHAT TO LOOK FOR:
#
# AUC:
#   Should be similar pre/post — recalibration does not
#   change discrimination. If it changes by >0.01, something
#   went wrong in the split or formula.
#
# Brier score:
#   Honest (validation) Brier will be WORSE (higher) than
#   training Brier. This is expected and correct. The gap
#   tells you how optimistic your in-sample evaluation was.
#   Gap > 0.05 suggests overfitting — review model complexity.
#
# Calibration intercept (post):
#   Target: between -0.20 and +0.20
#   If outside this range: model is systematically over or
#   under-predicting — add or remove predictors
#
# Calibration slope (post):
#   Target: between 0.80 and 1.20
#   If > 1.20: predictions are too compressed (common with
#              in-sample calibration — confirms split was needed)
#   If < 0.80: predictions are too spread out — check for
#              data leakage or overfitting
#
# Pricing implication:
#   If slope is far from 1.0, your premiums are systematically
#   wrong for high-risk and low-risk patients in opposite directions.
#   A slope of 5.0 (as seen before this fix) means you were
#   dramatically overestimating the probability range —
#   likely underpricing high-risk patients and overpricing low-risk.
############################################################

message("\n================================================")
message("SECTION 5: SUMMARY OUTCOME TABLE")
message("==================================================")

# -- In-sample metrics (for comparison baseline) ----------------
get_metrics_insample <- function(model, df, outcome_col) {
  preds   <- predict(model, type="response")
  outcome <- df[[outcome_col]]
  auc_val <- as.numeric(pROC::roc(outcome, preds, quiet=TRUE)$auc)
  brier   <- mean((preds - outcome)^2, na.rm=TRUE)
  # In-sample calibration (biased — shown for comparison only)
  preds_clip <- pmax(pmin(preds, 0.9999), 0.0001)
  cal_m  <- glm(outcome ~ log(preds_clip/(1-preds_clip)), family=binomial)
  list(auc   = round(auc_val, 4),
       brier = round(brier, 4),
       int   = round(coef(cal_m)[1], 4),
       slope = round(coef(cal_m)[2], 4))
}

# -- Out-of-sample metrics (honest) -----------------------------
get_metrics_oos <- function(model, df_val, outcome_col,
                            cal_int, cal_slope) {
  if (is.null(df_val)) return(list(auc=NA, brier=NA,
                                   int=cal_int, slope=cal_slope))
  preds   <- predict(model, newdata=df_val, type="response")
  outcome <- df_val[[outcome_col]]
  auc_val <- as.numeric(pROC::roc(outcome, preds, quiet=TRUE)$auc)
  brier   <- mean((preds - outcome)^2, na.rm=TRUE)
  list(auc   = round(auc_val, 4),
       brier = round(brier, 4),
       int   = cal_int,
       slope = cal_slope)
}

lb_in  <- get_metrics_insample(livebirth_model_refit, df_lb_train, col_outcome_lb)
lb_out <- get_metrics_oos(livebirth_model_refit, df_lb_val,
                          col_outcome_lb, lb_cal_int_refit, lb_cal_slope_refit)

eu_in  <- get_metrics_insample(euploid_model_refit, df_eu_train, col_outcome_eu)
eu_out <- get_metrics_oos(euploid_model_refit, df_eu_val,
                          col_outcome_eu, eu_cal_int_refit, eu_cal_slope_refit)

summary_table <- data.frame(
  Model = c("Live birth — in-sample (training)",
            "Live birth — out-of-sample (validation)",
            "Euploid — in-sample (training)",
            "Euploid — out-of-sample (validation)"),
  AUC = c(lb_in$auc,  lb_out$auc,
          eu_in$auc,  eu_out$auc),
  Brier = c(lb_in$brier,  lb_out$brier,
            eu_in$brier,  eu_out$brier),
  Cal_Intercept = c(lb_in$int,  lb_out$int,
                    eu_in$int,  eu_out$int),
  Cal_Slope = c(lb_in$slope,  lb_out$slope,
                eu_in$slope,  eu_out$slope),
  stringsAsFactors = FALSE
) %>%
  mutate(
    Intercept_flag = case_when(
      is.na(Cal_Intercept)              ~ "",
      abs(Cal_Intercept) <= 0.20        ~ "OK",
      abs(Cal_Intercept) <= 0.50        ~ "! Review",
      TRUE                              ~ "!! Action needed"
    ),
    Slope_flag = case_when(
      is.na(Cal_Slope)                  ~ "",
      Cal_Slope >= 0.80 & Cal_Slope <= 1.20 ~ "OK",
      Cal_Slope >= 0.60 & Cal_Slope <= 1.50 ~ "! Review",
      TRUE                              ~ "!! Action needed"
    )
  )

message("\nFinal summary table:")
print(summary_table, row.names = FALSE)

# -- Pricing impact statement -------------------------------
# !!! NOTE: This prints a plain-language interpretation of
# whether the recalibration materially changes the pricing.
# Read this before updating the master pricing script.

message("\n-- PRICING IMPACT ASSESSMENT -----------------------")

slope_change_lb <- lb_out$slope - lb_in$slope
slope_change_eu <- eu_out$slope - eu_in$slope

message(sprintf(
  "Live birth calibration slope: in-sample=%.3f → validation=%.3f (change: %+.3f)",
  lb_in$slope, lb_out$slope, slope_change_lb
))
message(sprintf(
  "Euploid calibration slope:    in-sample=%.3f → validation=%.3f (change: %+.3f)",
  eu_in$slope, eu_out$slope, slope_change_eu
))

if (abs(slope_change_lb) > 0.5 | abs(slope_change_eu) > 0.5) {
  message("\n!! MATERIAL CHANGE DETECTED")
  message("   Calibration slope shifted by more than 0.5 between")
  message("   in-sample and validation. This means in-sample premiums")
  message("   were materially mispriced. Update master script with")
  message("   new calibration parameters before any pricing is issued.")
} else {
  message("\n   Calibration shift is modest. In-sample pricing was")
  message("   approximately correct. Validation parameters are still")
  message("   preferred and should be used going forward.")
}

############################################################
# 6. EXPORT CLEAN DATA FRAMES FOR MASTER SCRIPT
# =========================================================
# PURPOSE:
# The master pricing script (fertility_master_v4.R) expects
# data frames named df_livebirth, df_euploid, df_egg1, df_egg2.
# We export the TRAINING versions here (with age_model column)
# so the master script refits on the correct data.
#
# The calibration parameters (intercepts and slopes) are
# printed below — copy these into Section 0 of
# fertility_master_v4.R to override the in-sample estimates.
#
#
#
## ----- !!!   IMPORTANT READ THIS   !!! -------------###
#
# !!! NOTE: After running this script, update these four
# values in fertility_master_v4.R before the next run:
#   lb_cal_intercept  <- from lb_cal_int_refit
#   lb_cal_slope      <- from lb_cal_slope_refit
#   eu_cal_intercept  <- from eu_cal_int_refit
#   eu_cal_slope      <- from eu_cal_slope_refit
############################################################

message("\n================================================")
message("SECTION 6: EXPORT AND HANDOFF")
message("==================================================")

# Rename training frames to names expected by master script
df_livebirth <- df_lb_train
df_euploid   <- df_eu_train
df_egg1      <- df_e1_train
df_egg2      <- df_e2_train

message("\n--COPY THESE INTO fertility_master_v4.R --------------")
message("   (replace the placeholder calibration values)\n")
message(sprintf("lb_cal_intercept <- %.8f", lb_cal_int_refit))
message(sprintf("lb_cal_slope     <- %.8f", lb_cal_slope_refit))
message(sprintf("eu_cal_intercept <- %.8f", eu_cal_int_refit))
message(sprintf("eu_cal_slope     <- %.8f", eu_cal_slope_refit))

message("\n-- DATA FRAMES READY FOR MASTER SCRIPT -----------------")
message(sprintf("   df_livebirth : %d rows (training)", nrow(df_livebirth)))
message(sprintf("   df_euploid   : %d rows (training)", nrow(df_euploid)))
message(sprintf("   df_egg1      : %d rows (training)", nrow(df_egg1)))
message(sprintf("   df_egg2      : %d rows (training)", nrow(df_egg2)))
message("\n   All data frames contain 'age_model' column (floored at",
        age_floor, ")")
message("   and 'age_true' column (original age preserved for audit).")

message("\n=================================================")
message("PREPARATION SCRIPT COMPLETE")
message("Next step: run fertility_master_v4.R")
message("=================================================\n")