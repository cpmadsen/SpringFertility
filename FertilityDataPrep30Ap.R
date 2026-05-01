############################################################
# DATA PREP + AGE POOLING + TRAIN/VALIDATION CALIBRATION
# Updated to match Insurance Modeling Analysis Rmd
############################################################

library(dplyr)
library(tidyr)
library(caret)
library(MASS)
library(pROC)

set.seed(42)

############################################################
# 0. INPUTS
############################################################

raw_livebirth <- m1df_eup
raw_euploid   <- m1df
raw_egg1      <- df_no_na
raw_egg2      <- df_2

col_outcome_lb <- "birth_bool"
col_outcome_eu <- "euploid_success"
col_outcome_e1 <- "num_eggs_collected"
col_outcome_e2 <- "eggs_2cycle_total"

col_age      <- "age"
col_amh      <- "amh"
col_afc      <- "afc"
col_eggs_cy1 <- "eggs_cycle1"

age_floor   <- 30
train_pct   <- 0.80
cv_mode     <- FALSE
cv_folds    <- 5
random_seed <- 42

############################################################
# 1. EVENT RATE CHECK
############################################################

message("\n================================================")
message("SECTION 1: EVENT RATE CHECK")
message("================================================")

check_events <- function(df, outcome_col, model_name, train_p) {
  outcome <- df[[outcome_col]]
  
  n_total       <- nrow(df)
  events_total  <- sum(outcome, na.rm = TRUE)
  event_rate    <- mean(outcome, na.rm = TRUE)
  n_val         <- round(n_total * (1 - train_p))
  events_in_val <- round(events_total * (1 - train_p))
  
  data.frame(
    Model          = model_name,
    N_total        = n_total,
    Events_total   = events_total,
    Event_rate_pct = round(event_rate * 100, 1),
    N_validation   = n_val,
    Events_in_val  = events_in_val,
    Flag_total     = ifelse(events_total < 200, "!! LOW — consider more data", "OK"),
    Flag_val       = ifelse(events_in_val < 100, "!! LOW — consider cv_mode=TRUE", "OK"),
    stringsAsFactors = FALSE
  )
}

event_check_table <- bind_rows(
  check_events(raw_livebirth, col_outcome_lb, "Live birth", train_pct),
  check_events(raw_euploid,   col_outcome_eu, "Euploid",    train_pct),
  data.frame(
    Model = "Egg 1-cycle",
    N_total = nrow(raw_egg1),
    Events_total = NA,
    Event_rate_pct = NA,
    N_validation = round(nrow(raw_egg1) * (1 - train_pct)),
    Events_in_val = NA,
    Flag_total = "Count model — no binary event check",
    Flag_val = "N/A"
  ),
  data.frame(
    Model = "Egg 2-cycle",
    N_total = nrow(raw_egg2),
    Events_total = NA,
    Event_rate_pct = NA,
    N_validation = round(nrow(raw_egg2) * (1 - train_pct)),
    Events_in_val = NA,
    Flag_total = "Count model — no binary event check",
    Flag_val = "N/A"
  )
)

print(event_check_table, row.names = FALSE)

age_band_summary <- function(df, outcome_col, label) {
  message(paste0("\nEvent rates by age band: ", label))
  
  out <- df %>%
    mutate(
      age_band = case_when(
        #.data[[col_age]] < 28 ~ "22-27 pooled",
        .data[[col_age]] < 31 ~ "22-30 pooled",
        .data[[col_age]] < 35 ~ "31-34",
        .data[[col_age]] < 39 ~ "35-38",
        .data[[col_age]] < 43 ~ "39-42",
        TRUE                  ~ "43+"
      )
    ) %>%
    group_by(age_band) %>%
    summarize(
      n          = n(),
      events     = sum(.data[[outcome_col]], na.rm = TRUE),
      event_rate = round(mean(.data[[outcome_col]], na.rm = TRUE) * 100, 1),
      .groups = "drop"
    ) %>%
    mutate(
      flag = case_when(
        n < 20  ~ "!! Very sparse",
        n < 100 ~ "! Thin",
        TRUE    ~ "OK"
      )
    )
  
  print(out, row.names = FALSE)
  out
}

age_band_check_lb <- age_band_summary(raw_livebirth, col_outcome_lb, "live birth")
age_band_check_eu <- age_band_summary(raw_euploid,   col_outcome_eu, "euploid")

message("\nEgg 1-cycle count distribution:")
print(summary(raw_egg1[[col_outcome_e1]]))

message(sprintf(
  "Zero egg retrievals: %d (%.1f%%)",
  sum(raw_egg1[[col_outcome_e1]] == 0, na.rm = TRUE),
  mean(raw_egg1[[col_outcome_e1]] == 0, na.rm = TRUE) * 100
))

message("\nEgg 2-cycle count distribution:")
print(summary(raw_egg2[[col_outcome_e2]]))

message(sprintf(
  "Zero total eggs across 2 cycles: %d (%.1f%%)",
  sum(raw_egg2[[col_outcome_e2]] == 0, na.rm = TRUE),
  mean(raw_egg2[[col_outcome_e2]] == 0, na.rm = TRUE) * 100
))

############################################################
# 2. AGE POOLING
############################################################

apply_age_floor <- function(df, age_col, floor_val) {
  df %>%
    mutate(
      age_true  = .data[[age_col]],
      age_model = pmax(.data[[age_col]], floor_val)
    )
}

df_livebirth <- apply_age_floor(raw_livebirth, col_age, age_floor)
df_euploid   <- apply_age_floor(raw_euploid,   col_age, age_floor)
df_egg1      <- apply_age_floor(raw_egg1,      col_age, age_floor)
df_egg2      <- apply_age_floor(raw_egg2,      col_age, age_floor)

pooling_summary <- data.frame(
  Model = c("Live birth", "Euploid", "Egg 1", "Egg 2"),
  N_total = c(
    nrow(df_livebirth),
    nrow(df_euploid),
    nrow(df_egg1),
    nrow(df_egg2)
  ),
  N_pooled = c(
    sum(df_livebirth$age_true < age_floor, na.rm = TRUE),
    sum(df_euploid$age_true   < age_floor, na.rm = TRUE),
    sum(df_egg1$age_true      < age_floor, na.rm = TRUE),
    sum(df_egg2$age_true      < age_floor, na.rm = TRUE)
  )
) %>%
  mutate(
    Pct_pooled = round(N_pooled / N_total * 100, 1),
    Flag = ifelse(
      Pct_pooled > 5,
      "! >5% pooled — review age_floor",
      "OK"
    )
  )

print(pooling_summary, row.names = FALSE)

############################################################
# 3. TRAIN / VALIDATION SPLIT
############################################################

set.seed(random_seed)

split_binary <- function(df, outcome_col, train_p) {
  idx <- caret::createDataPartition(
    df[[outcome_col]],
    p = train_p,
    list = FALSE
  )
  
  list(
    train = df[idx, ],
    val   = df[-idx, ]
  )
}

split_count <- function(df, outcome_col, train_p) {
  df <- df %>%
    mutate(.strat = ntile(.data[[outcome_col]], 4))
  
  idx <- caret::createDataPartition(
    df$.strat,
    p = train_p,
    list = FALSE
  )
  
  list(
    train = df[idx, ] %>% dplyr::select(-.strat),
    val   = df[-idx, ] %>% dplyr::select(-.strat)
  )
}

if (!cv_mode) {
  
  lb_split <- split_binary(df_livebirth, col_outcome_lb, train_pct)
  eu_split <- split_binary(df_euploid,   col_outcome_eu, train_pct)
  e1_split <- split_count(df_egg1,       col_outcome_e1, train_pct)
  e2_split <- split_count(df_egg2,       col_outcome_e2, train_pct)
  
  df_lb_train <- lb_split$train
  df_lb_val   <- lb_split$val
  
  df_eu_train <- eu_split$train
  df_eu_val   <- eu_split$val
  
  df_e1_train <- e1_split$train
  df_e1_val   <- e1_split$val
  
  df_e2_train <- e2_split$train
  df_e2_val   <- e2_split$val
  
} else {
  
  df_lb_train <- df_livebirth
  df_lb_val   <- NULL
  
  df_eu_train <- df_euploid
  df_eu_val   <- NULL
  
  df_e1_train <- df_egg1
  df_e1_val   <- NULL
  
  df_e2_train <- df_egg2
  df_e2_val   <- NULL
}

split_check <- data.frame(
  Model = c(
    "Live birth — train",
    "Live birth — val",
    "Euploid — train",
    "Euploid — val"
  ),
  N = c(
    nrow(df_lb_train),
    nrow(df_lb_val),
    nrow(df_eu_train),
    nrow(df_eu_val)
  ),
  Events = c(
    sum(df_lb_train[[col_outcome_lb]], na.rm = TRUE),
    sum(df_lb_val[[col_outcome_lb]], na.rm = TRUE),
    sum(df_eu_train[[col_outcome_eu]], na.rm = TRUE),
    sum(df_eu_val[[col_outcome_eu]], na.rm = TRUE)
  )
) %>%
  mutate(
    Event_rate_pct = round(Events / N * 100, 1),
    Flag = ifelse(N < 50, "!! Very small set", "OK")
  )

print(split_check, row.names = FALSE)

############################################################
# 4. REFIT MODELS + VALIDATION CALIBRATION
############################################################

clip_prob <- function(x, eps = 0.0001) {
  pmax(pmin(x, 1 - eps), eps)
}

if (!cv_mode) {
  
  livebirth_model_refit <- glm(
    birth_bool ~ age_model * amh * afc,
    data = df_lb_train,
    family = binomial
  )
  
  euploid_model_refit <- glm(
    euploid_success ~ age_model + amh + afc + amh:afc,
    data = df_eu_train,
    family = binomial
  )
  
  egg1_model_refit <- MASS::glm.nb(
    num_eggs_collected ~ age_model + amh + afc + age_model:afc,
    data = df_e1_train
  )
  
  egg2_model_refit <- MASS::glm.nb(
    eggs_2cycle_total ~ age_model + amh + afc + eggs_cycle1 + amh:afc,
    data = df_e2_train
  )
  
  df_lb_val <- df_lb_val %>%
    mutate(pred_raw = clip_prob(predict(livebirth_model_refit, newdata = ., type = "response")))
  
  df_eu_val <- df_eu_val %>%
    mutate(pred_raw = clip_prob(predict(euploid_model_refit, newdata = ., type = "response")))
  
  df_e1_val <- df_e1_val %>%
    mutate(pred_mu = predict(egg1_model_refit, newdata = ., type = "response"))
  
  df_e2_val <- df_e2_val %>%
    mutate(pred_mu = predict(egg2_model_refit, newdata = ., type = "response"))
  
  cal_lb_refit <- glm(
    birth_bool ~ log(pred_raw / (1 - pred_raw)),
    data = df_lb_val,
    family = binomial
  )
  
  cal_eu_refit <- glm(
    euploid_success ~ log(pred_raw / (1 - pred_raw)),
    data = df_eu_val,
    family = binomial
  )
  
  lb_cal_int_refit   <- round(coef(cal_lb_refit)[1], 8)
  lb_cal_slope_refit <- round(coef(cal_lb_refit)[2], 8)
  
  eu_cal_int_refit   <- round(coef(cal_eu_refit)[1], 8)
  eu_cal_slope_refit <- round(coef(cal_eu_refit)[2], 8)
}

############################################################
# 5. EGG COUNT CALIBRATION CHECK
############################################################

egg_cal_check <- function(df_val, pred_col, outcome_col, age_col = "age_model") {
  df_val %>%
    mutate(
      age_band = case_when(
        .data[[age_col]] < 28 ~ "22-27",
        .data[[age_col]] < 31 ~ "28-30",
        .data[[age_col]] < 35 ~ "31-34",
        .data[[age_col]] < 39 ~ "35-38",
        .data[[age_col]] < 43 ~ "39-42",
        TRUE                  ~ "43+"
      )
    ) %>%
    group_by(age_band) %>%
    summarize(
      n              = n(),
      observed_mean  = round(mean(.data[[outcome_col]], na.rm = TRUE), 2),
      predicted_mean = round(mean(.data[[pred_col]], na.rm = TRUE), 2),
      abs_error      = round(abs(observed_mean - predicted_mean), 2),
      flag = case_when(
        abs_error > 2 ~ "!! Miscalibrated — review",
        abs_error > 1 ~ "! Check",
        TRUE          ~ "OK"
      ),
      .groups = "drop"
    )
}

egg1_cal_table <- egg_cal_check(df_e1_val, "pred_mu", col_outcome_e1)
egg2_cal_table <- egg_cal_check(df_e2_val, "pred_mu", col_outcome_e2)

print(egg1_cal_table, row.names = FALSE)
print(egg2_cal_table, row.names = FALSE)

############################################################
# 6. SUMMARY METRICS
############################################################

get_metrics_insample <- function(model, df, outcome_col) {
  preds <- clip_prob(predict(model, type = "response"))
  y     <- df[[outcome_col]]
  
  auc_val <- as.numeric(pROC::roc(y, preds, quiet = TRUE)$auc)
  brier   <- mean((preds - y)^2, na.rm = TRUE)
  
  cal_m <- glm(
    y ~ log(preds / (1 - preds)),
    family = binomial
  )
  
  list(
    auc   = round(auc_val, 4),
    brier = round(brier, 4),
    int   = round(coef(cal_m)[1], 4),
    slope = round(coef(cal_m)[2], 4)
  )
}

get_metrics_oos <- function(model, df_val, outcome_col, cal_int, cal_slope) {
  preds <- clip_prob(predict(model, newdata = df_val, type = "response"))
  y     <- df_val[[outcome_col]]
  
  auc_val <- as.numeric(pROC::roc(y, preds, quiet = TRUE)$auc)
  brier   <- mean((preds - y)^2, na.rm = TRUE)
  
  list(
    auc   = round(auc_val, 4),
    brier = round(brier, 4),
    int   = round(cal_int, 4),
    slope = round(cal_slope, 4)
  )
}

lb_in  <- get_metrics_insample(livebirth_model_refit, df_lb_train, col_outcome_lb)
lb_out <- get_metrics_oos(livebirth_model_refit, df_lb_val, col_outcome_lb,
                          lb_cal_int_refit, lb_cal_slope_refit)

eu_in  <- get_metrics_insample(euploid_model_refit, df_eu_train, col_outcome_eu)
eu_out <- get_metrics_oos(euploid_model_refit, df_eu_val, col_outcome_eu,
                          eu_cal_int_refit, eu_cal_slope_refit)

summary_table <- data.frame(
  Model = c(
    "Live birth — in-sample training",
    "Live birth — out-of-sample validation",
    "Euploid — in-sample training",
    "Euploid — out-of-sample validation"
  ),
  AUC = c(lb_in$auc, lb_out$auc, eu_in$auc, eu_out$auc),
  Brier = c(lb_in$brier, lb_out$brier, eu_in$brier, eu_out$brier),
  Cal_Intercept = c(lb_in$int, lb_out$int, eu_in$int, eu_out$int),
  Cal_Slope = c(lb_in$slope, lb_out$slope, eu_in$slope, eu_out$slope)
) %>%
  mutate(
    Intercept_flag = case_when(
      abs(Cal_Intercept) <= 0.20 ~ "OK",
      abs(Cal_Intercept) <= 0.50 ~ "! Review",
      TRUE                       ~ "!! Action needed"
    ),
    Slope_flag = case_when(
      Cal_Slope >= 0.80 & Cal_Slope <= 1.20 ~ "OK",
      Cal_Slope >= 0.60 & Cal_Slope <= 1.50 ~ "! Review",
      TRUE                                  ~ "!! Action needed"
    )
  )

print(summary_table, row.names = FALSE)

############################################################
# 7. EXPORT / HANDOFF OBJECTS
############################################################

df_livebirth <- df_lb_train
df_euploid   <- df_eu_train
df_egg1      <- df_e1_train
df_egg2      <- df_e2_train

message("\n-- COPY THESE INTO MASTER SCRIPT ----------------")
message(sprintf("lb_cal_intercept <- %.8f", lb_cal_int_refit))
message(sprintf("lb_cal_slope     <- %.8f", lb_cal_slope_refit))
message(sprintf("eu_cal_intercept <- %.8f", eu_cal_int_refit))
message(sprintf("eu_cal_slope     <- %.8f", eu_cal_slope_refit))

message("\n-- DATA FRAMES READY ----------------------------")
message(sprintf("df_livebirth : %d rows", nrow(df_livebirth)))
message(sprintf("df_euploid   : %d rows", nrow(df_euploid)))
message(sprintf("df_egg1      : %d rows", nrow(df_egg1)))
message(sprintf("df_egg2      : %d rows", nrow(df_egg2)))
