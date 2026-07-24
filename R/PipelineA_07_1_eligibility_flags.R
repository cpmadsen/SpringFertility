############################################################
# FERTILITY ACTUARIAL PRICING -- ELIGIBILITY FLAGS
# Version: 2.0 | July 2026
#
# CHANGES FROM v1.0:
#   [1] Egg freezing poor responder exclusion added:
#       AFC <= 6 AND AMH <= 2.0 flagged as ineligible for
#       BOTH 1-cycle and 2-cycle egg freezing products.
#       Clinically aligned with Bologna POR criteria.
#       Replaces prior T1 < 1 egg floor check.
#   [2] flag_threshold_below_1egg removed; replaced with
#       flag_poor_responder throughout
#   [3] clinical_note updated to surface POR exclusion reason
#   [4] Combined Summary sheet updated to reflect new flag
#
# PURPOSE:
#   For each of the three product models (Live Birth, Euploid,
#   Egg Freezing), identify patients who:
#     (a) Would generate a premium exceeding 50% of total
#         cycle cost (premium cap flag -- ineligible for
#         full refund products)
#     (b) Have a calibrated success probability below 15%,
#         10%, or 5% (IVF and Euploid models only)
#     (c) Meet poor ovarian reserve criteria: AFC <= 6 AND
#         AMH <= 2.0 (Egg models only -- ineligible for both
#         1-cycle and 2-cycle egg freezing products)
#
#   Output: per-patient flag rows + summary tables by age
#   band, AMH quartile, AFC quartile. Exported as both
#   Excel workbook and CSV files.
#
# RUN ORDER:
#   Run AFTER PipelineA_03_fertility_master_v7.R
#   Requires: fitted models and calibration params in
#   environment, OR load from saved RDS files (Section 0).
#
# ============================================================
# !!NOTE!!: THERE ARE EXACTLY 4 PLACES YOU MUST EDIT.
# Every one is marked: vvvvvvvvv !!NOTE!! CHANGE HERE
# ============================================================
############################################################

library(dplyr)
library(tidyr)
library(openxlsx)
library(MASS)
library(readr)


############################################################
# SECTION 0 -- INPUTS
############################################################

# ============================================================
# vvvvvvvvv !!NOTE!! CHANGE HERE [1 of 4] -- DATA
# ============================================================
# Full patient-level scoring datasets (all eligible patients,
# not just training data). Must contain age_model (floored
# at 32), amh (capped at 12), afc (capped at 40).
#
# If running interactively after PipelineA_03_v7, the fitted
# models and calibration params are already in your environment
# and the RDS loads below can be skipped. Otherwise load them.
# ------------------------------------------------------------
# 
# df_livebirth <- readr::read_rds("data/models/df_lb_train.rds")
# df_euploid   <- readr::read_rds("data/models/df_eu_train.rds")
# df_egg1      <- readr::read_rds("data/models/df_e1_train.rds")
# 
# # Fitted models -- skip if already in environment from v7 run
# # livebirth_model <- readr::read_rds("data/models/livebirth_model_revised_modeling_statistics_end.rds")
# # euploid_model   <- readr::read_rds("data/models/euploid_model.rds")
# # egg1_model      <- readr::read_rds("data/models/egg1_model.rds")
# 
# # Calibration parameters -- skip if already in environment
# # lb_cal_intercept <- readr::read_rds("data/models/lb_cal_int_refit.rds")
# # lb_cal_slope     <- readr::read_rds("data/models/lb_cal_slope_refit.rds")
# eu_cal_intercept <- 0.015
# eu_cal_slope     <- 0.853


# ============================================================
# vvvvvvvvv !!NOTE!! CHANGE HERE [2 of 4] -- COSTS
# ============================================================
# Keep aligned with PipelineA_03_v7 Section 0 [5 of 7].
# ------------------------------------------------------------

cost_livebirth <- 21460   # NY total cycle cost
cost_euploid   <- 18704   # NY total cycle cost
cost_egg1      <- 10700   # NY total cycle cost
cost_egg2      <- 9630    # NY total cycle cost (= 0.9 * egg1)
variable_cost  <- 6000    # free cycle variable cost basis

# Egg mu calibration scalar -- must match PipelineA_03_v7.1
# Scales predicted mu so observed 1-cycle fail rate = 0.10
# on training data. Derived from Spec 2 refit July 2026.
egg_mu_k <- 1.095380


# ============================================================
# vvvvvvvvv !!NOTE!! CHANGE HERE [3 of 4] -- MARGINS & CAP
# ============================================================

risk_margin     <- 0.20
admin_load      <- 0.10
pool_weight     <- 0.30
premium_cap_pct <- 0.50   # cap ceiling = 50% of total cycle cost


# ============================================================
# vvvvvvvvv !!NOTE!! CHANGE HERE [4 of 4] -- OUTPUT PATHS
# ============================================================

output_excel   <- "outputs/Fertility_Eligibility_Flags.xlsx"
output_dir_csv <- "outputs/eligibility_flags/"


# ============================================================
# POOR RESPONDER EXCLUSION CRITERIA (do not edit)
# ============================================================
# Clinically aligned with Bologna POR criteria.
# Applies to BOTH egg 1-cycle and 2-cycle products.
# Patients meeting this rule are ineligible for all egg
# freezing guarantee products.
# Data validation confirmed these profiles produce T1 < 3
# (clinically uninformative guarantee thresholds) and
# effective fail rates as low as 2.7% vs nominal 10%.

POR_AFC_THRESHOLD <- 6    # AFC <= this value
POR_AMH_THRESHOLD <- 2.0  # AND AMH <= this value


############################################################
# SECTION 1 -- SHARED HELPERS
############################################################

dir.create(output_dir_csv, showWarnings = FALSE, recursive = TRUE)

combined_load <- (1 + risk_margin) * (1 + admin_load)

# Calibration function (Platt scaling)
apply_calibration <- function(pred_raw, intercept, slope) {
  logit_raw <- log(pmax(pmin(pred_raw, 0.9999), 0.0001) /
                     (1 - pmax(pmin(pred_raw, 0.9999), 0.0001)))
  1 / (1 + exp(-(intercept + slope * logit_raw)))
}

# Blended uncapped premium (1-cycle full refund basis for cap comparison)
# Cap ceiling = n_cycles * cost * 50% (no combined_load in ceiling)
compute_uncapped_premium <- function(p_fail_i, p_fail_pool, cost, n_cycles) {
  refund_amt <- n_cycles * cost
  blended    <- (1 - pool_weight) * p_fail_i * refund_amt +
                pool_weight       * p_fail_pool * refund_amt
  blended * combined_load
}

cap_ceiling <- function(cost, n_cycles) {
  n_cycles * cost * premium_cap_pct
}

# Age banding
age_band <- function(age) {
  cut(age,
      breaks = c(-Inf, 34, 37, 40, 43, Inf),
      labels = c("<=34", "35-37", "38-40", "41-43", "44+"),
      right  = TRUE)
}

# Summary helper: flag rate by grouping variable
summarise_flags <- function(df, group_col, flag_cols) {
  df %>%
    group_by(across(all_of(group_col))) %>%
    summarise(
      n_patients = n(),
      across(all_of(flag_cols), ~ sum(.x, na.rm = TRUE),
             .names = "n_{.col}"),
      across(all_of(flag_cols), ~ round(mean(.x, na.rm = TRUE) * 100, 1),
             .names = "pct_{.col}"),
      .groups = "drop"
    )
}


############################################################
# SECTION 2 -- LIVE BIRTH MODEL FLAGS
############################################################

message("[ 1/3 ] Processing Live Birth model...")

e1_theta <- egg1_model$theta   # needed for egg sections

df_lb <- df_livebirth %>%
  mutate(
    age_model = pmax(age_model, 32),
    amh       = pmin(amh, 12),
    afc       = pmin(afc, 40),

    p_raw     = predict(livebirth_model, newdata = ., type = "response"),
    p_success = apply_calibration(p_raw, lb_cal_intercept, lb_cal_slope),
    p_fail    = 1 - p_success,

    pool_fail_lb = mean(p_fail, na.rm = TRUE),

    uncapped_premium_1cy = compute_uncapped_premium(
      p_fail, pool_fail_lb, cost_livebirth, 1),
    cap_ceiling_1cy = cap_ceiling(cost_livebirth, 1),

    flag_cap_exceeded = uncapped_premium_1cy > cap_ceiling_1cy,
    flag_below_15pct  = p_success < 0.15,
    flag_below_10pct  = p_success < 0.10,
    flag_below_5pct   = p_success < 0.05,

    age_band     = age_band(age_model),
    amh_quartile = paste0("Q", ntile(amh, 4)),
    afc_quartile = paste0("Q", ntile(afc, 4))
  ) %>%
  dplyr::select(-pool_fail_lb)

flag_cols_lb <- c("flag_cap_exceeded", "flag_below_15pct",
                  "flag_below_10pct",  "flag_below_5pct")

lb_summary_age <- summarise_flags(df_lb, "age_band",     flag_cols_lb) %>%
  mutate(breakdown = "Age Band")
lb_summary_amh <- summarise_flags(df_lb, "amh_quartile", flag_cols_lb) %>%
  mutate(breakdown = "AMH Quartile")
lb_summary_afc <- summarise_flags(df_lb, "afc_quartile", flag_cols_lb) %>%
  mutate(breakdown = "AFC Quartile")
lb_summary_overall <- df_lb %>%
  summarise(
    group      = "All patients",
    n_patients = n(),
    across(all_of(flag_cols_lb), ~ sum(.x, na.rm = TRUE),
           .names = "n_{.col}"),
    across(all_of(flag_cols_lb), ~ round(mean(.x, na.rm = TRUE) * 100, 1),
           .names = "pct_{.col}")
  ) %>%
  mutate(breakdown = "Overall")

lb_summary <- bind_rows(
  lb_summary_overall %>% rename(group = group),
  lb_summary_age     %>% rename(group = age_band),
  lb_summary_amh     %>% rename(group = amh_quartile),
  lb_summary_afc     %>% rename(group = afc_quartile)
) %>%
  dplyr::select(breakdown, group, n_patients, everything())

message(sprintf("    LB: %d patients | %d cap-exceeded | %d below 10pct success",
                nrow(df_lb),
                sum(df_lb$flag_cap_exceeded, na.rm = TRUE),
                sum(df_lb$flag_below_10pct,  na.rm = TRUE)))


############################################################
# SECTION 3 -- EUPLOID MODEL FLAGS
############################################################

message("[ 2/3 ] Processing Euploid model...")

df_eu <- df_euploid %>%
  mutate(
    age_model = pmax(age_model, 32),
    amh       = pmin(amh, 12),
    afc       = pmin(afc, 40),

    p_raw     = predict(euploid_model, newdata = ., type = "response"),
    p_success = apply_calibration(p_raw, eu_cal_intercept, eu_cal_slope),
    p_fail    = 1 - p_success,

    pool_fail_eu = mean(p_fail, na.rm = TRUE),

    uncapped_premium_1cy = compute_uncapped_premium(
      p_fail, pool_fail_eu, cost_euploid, 1),
    cap_ceiling_1cy = cap_ceiling(cost_euploid, 1),

    flag_cap_exceeded = uncapped_premium_1cy > cap_ceiling_1cy,
    flag_below_15pct  = p_success < 0.15,
    flag_below_10pct  = p_success < 0.10,
    flag_below_5pct   = p_success < 0.05,

    age_band     = age_band(age_model),
    amh_quartile = paste0("Q", ntile(amh, 4)),
    afc_quartile = paste0("Q", ntile(afc, 4))
  ) %>%
  dplyr::select(-pool_fail_eu)

flag_cols_eu <- flag_cols_lb

eu_summary_age <- summarise_flags(df_eu, "age_band",     flag_cols_eu) %>%
  mutate(breakdown = "Age Band")
eu_summary_amh <- summarise_flags(df_eu, "amh_quartile", flag_cols_eu) %>%
  mutate(breakdown = "AMH Quartile")
eu_summary_afc <- summarise_flags(df_eu, "afc_quartile", flag_cols_eu) %>%
  mutate(breakdown = "AFC Quartile")
eu_summary_overall <- df_eu %>%
  summarise(
    group      = "All patients",
    n_patients = n(),
    across(all_of(flag_cols_eu), ~ sum(.x, na.rm = TRUE),
           .names = "n_{.col}"),
    across(all_of(flag_cols_eu), ~ round(mean(.x, na.rm = TRUE) * 100, 1),
           .names = "pct_{.col}")
  ) %>%
  mutate(breakdown = "Overall")

eu_summary <- bind_rows(
  eu_summary_overall %>% rename(group = group),
  eu_summary_age     %>% rename(group = age_band),
  eu_summary_amh     %>% rename(group = amh_quartile),
  eu_summary_afc     %>% rename(group = afc_quartile)
) %>%
  dplyr::select(breakdown, group, n_patients, everything())

message(sprintf("    EU: %d patients | %d cap-exceeded | %d below 10pct success",
                nrow(df_eu),
                sum(df_eu$flag_cap_exceeded, na.rm = TRUE),
                sum(df_eu$flag_below_10pct,  na.rm = TRUE)))


############################################################
# SECTION 4 -- EGG FREEZING FLAGS
# ============================================================
# Two ineligibility criteria apply to egg freezing products:
#
# (1) POOR RESPONDER EXCLUSION (flag_poor_responder)
#     AFC <= 6 AND AMH <= 2.0
#     Applies to BOTH 1-cycle and 2-cycle products.
#     These profiles produce T1 < 3 (clinically uninformative
#     guarantee thresholds) with effective fail rates as low
#     as 2.7% vs the nominal 10% design target.
#     Clinically aligned with Bologna POR criteria.
#
# (2) PREMIUM CAP (flag_cap_exceeded)
#     Uncapped premium > 50% of total egg cycle cost.
#     Applies to full refund products only. Patient remains
#     eligible for free cycle products.
#
# Additional columns for clinical context:
#   mu_1cy       : expected eggs per cycle from model
#   T1_threshold : personalized 90th pct guarantee (1-cycle)
#   T_cumulative : personalized 90th pct guarantee (2-cycle)
#   T_increment  : additional eggs guaranteed by Cycle 2
#   clinical_note: plain-language description of patient status
############################################################

message("[ 3/3 ] Processing Egg Freezing model...")

df_egg <- df_egg1 %>%
  mutate(
    age_model = pmax(age_model, 32),
    amh       = pmin(amh, 12),
    afc       = pmin(afc, 40),

    # Predicted mean egg yield per cycle
    mu_1cy = egg_mu_k * predict(egg1_model, newdata = ., type = "response"),

    # 90th pct guarantee threshold (1-cycle)
    T1_threshold = qnbinom(0.10, size = e1_theta, mu = mu_1cy),

    # Cumulative 2-cycle threshold (analytic NegBin convolution)
    T_cumulative = qnbinom(0.10, size = e1_theta, mu = 2 * mu_1cy),

    # Incremental eggs guaranteed by adding Cycle 2
    T_increment = T_cumulative - T1_threshold,

    # Poor responder exclusion flag
    # AFC <= 6 AND AMH <= 2.0 -- ineligible for all egg products
    flag_poor_responder = (afc <= POR_AFC_THRESHOLD &
                           amh <= POR_AMH_THRESHOLD),

    # Premium cap flag
    pool_fail_egg        = 0.10,
    uncapped_premium_1cy = compute_uncapped_premium(
      0.10, pool_fail_egg, cost_egg1, 1),
    cap_ceiling_1cy      = cap_ceiling(cost_egg1, 1),
    flag_cap_exceeded    = uncapped_premium_1cy > cap_ceiling_1cy,

    # Combined ineligibility: either flag means ineligible for
    # full refund egg products. Poor responder also ineligible
    # for free cycle egg products.
    flag_any_egg_ineligible = flag_poor_responder | flag_cap_exceeded,

    # Plain-language clinical note
    clinical_note = case_when(
      flag_poor_responder & flag_cap_exceeded ~
        paste0("Ineligible (all egg products): AFC <= ", POR_AFC_THRESHOLD,
               " and AMH <= ", POR_AMH_THRESHOLD,
               " (poor ovarian reserve) AND premium cap exceeded"),
      flag_poor_responder ~
        paste0("Ineligible (all egg products): AFC <= ", POR_AFC_THRESHOLD,
               " and AMH <= ", POR_AMH_THRESHOLD,
               " -- poor ovarian reserve (Bologna POR criteria). ",
               "Expected yield: ", round(mu_1cy, 1), " eggs/cycle. ",
               "1-cycle threshold: ", T1_threshold, " eggs"),
      flag_cap_exceeded ~
        paste0("Ineligible (full refund only): premium cap exceeded. ",
               "Eligible for free cycle products. ",
               "Expected yield: ", round(mu_1cy, 1), " eggs/cycle. ",
               "1-cycle threshold: ", T1_threshold, " eggs"),
      T1_threshold >= 8 ~
        paste0("Eligible: strong responder. ",
               round(mu_1cy, 1), " eggs expected. ",
               "Guaranteed >= ", T1_threshold, " eggs (1-cycle), ",
               ">= ", T_cumulative, " eggs (2-cycle cumulative)"),
      TRUE ~
        paste0("Eligible. ", round(mu_1cy, 1), " eggs expected. ",
               "Guaranteed >= ", T1_threshold, " eggs (1-cycle), ",
               ">= ", T_cumulative, " eggs (2-cycle cumulative)")
    ),

    age_band     = age_band(age_model),
    amh_quartile = paste0("Q", ntile(amh, 4)),
    afc_quartile = paste0("Q", ntile(afc, 4))
  ) %>%
  dplyr::select(-pool_fail_egg)

flag_cols_egg <- c("flag_poor_responder", "flag_cap_exceeded",
                   "flag_any_egg_ineligible")

egg_summary_age <- summarise_flags(df_egg, "age_band",     flag_cols_egg) %>%
  mutate(breakdown = "Age Band")
egg_summary_amh <- summarise_flags(df_egg, "amh_quartile", flag_cols_egg) %>%
  mutate(breakdown = "AMH Quartile")
egg_summary_afc <- summarise_flags(df_egg, "afc_quartile", flag_cols_egg) %>%
  mutate(breakdown = "AFC Quartile")
egg_summary_overall <- df_egg %>%
  summarise(
    group      = "All patients",
    n_patients = n(),
    across(all_of(flag_cols_egg), ~ sum(.x, na.rm = TRUE),
           .names = "n_{.col}"),
    across(all_of(flag_cols_egg), ~ round(mean(.x, na.rm = TRUE) * 100, 1),
           .names = "pct_{.col}")
  ) %>%
  mutate(breakdown = "Overall")

egg_summary <- bind_rows(
  egg_summary_overall %>% rename(group = group),
  egg_summary_age     %>% rename(group = age_band),
  egg_summary_amh     %>% rename(group = amh_quartile),
  egg_summary_afc     %>% rename(group = afc_quartile)
) %>%
  dplyr::select(breakdown, group, n_patients, everything())

message(sprintf(
  "    Egg: %d patients | %d poor responder | %d cap-exceeded | %d any-ineligible",
  nrow(df_egg),
  sum(df_egg$flag_poor_responder,      na.rm = TRUE),
  sum(df_egg$flag_cap_exceeded,        na.rm = TRUE),
  sum(df_egg$flag_any_egg_ineligible,  na.rm = TRUE)))


############################################################
# SECTION 5 -- CSV EXPORT
############################################################

message("Exporting CSVs...")

readr::write_csv(df_lb,
  file.path(output_dir_csv, "livebirth_patient_flags.csv"))
readr::write_csv(df_eu,
  file.path(output_dir_csv, "euploid_patient_flags.csv"))
readr::write_csv(df_egg,
  file.path(output_dir_csv, "egg_patient_flags.csv"))

readr::write_csv(lb_summary,
  file.path(output_dir_csv, "livebirth_flag_summary.csv"))
readr::write_csv(eu_summary,
  file.path(output_dir_csv, "euploid_flag_summary.csv"))
readr::write_csv(egg_summary,
  file.path(output_dir_csv, "egg_flag_summary.csv"))

message("    CSVs written to: ", output_dir_csv)


############################################################
# SECTION 6 -- EXCEL EXPORT
############################################################

message("Building Excel workbook...")

NAVY   <- "#0E2841"; BLUE   <- "#005DAE"; PURPLE <- "#8C65FF"
TEAL   <- "#2EA7CD"; GREEN  <- "#43AA49"; DPURP  <- "#503F84"
WHITE  <- "#FFFFFF"; LGREY  <- "#F5F5F5"; MGREY  <- "#D9DCE1"
AMBER  <- "#FFF3CD"; RED_LT <- "#FDECEA"

brd <- function() {
  s <- Side(style = "thin", color = MGREY)
  Border(top = s, bottom = s, left = s, right = s)
}

s_title <- createStyle(fontName = "Arial", fontSize = 13,
                       fontColour = WHITE, fgFill = NAVY,
                       textDecoration = "bold", halign = "left")
s_hdr   <- createStyle(fontName = "Arial", fontSize = 9,
                       fontColour = WHITE, fgFill = DPURP,
                       textDecoration = "bold", halign = "center",
                       border = "TopBottomLeftRight",
                       borderColour = WHITE, wrapText = TRUE)
s_sub   <- createStyle(fontName = "Arial", fontSize = 9,
                       fontColour = WHITE, fgFill = BLUE,
                       textDecoration = "bold", halign = "left",
                       border = "TopBottomLeftRight",
                       borderColour = WHITE)
s_dat   <- createStyle(fontName = "Arial", fontSize = 9,
                       border = "TopBottomLeftRight",
                       borderColour = MGREY)
s_shade <- createStyle(fontName = "Arial", fontSize = 9,
                       fgFill = LGREY,
                       border = "TopBottomLeftRight",
                       borderColour = MGREY)
s_flag  <- createStyle(fontName = "Arial", fontSize = 9,
                       fgFill = RED_LT, fontColour = "#C0392B",
                       textDecoration = "bold",
                       border = "TopBottomLeftRight",
                       borderColour = MGREY)
s_warn  <- createStyle(fontName = "Arial", fontSize = 9,
                       fgFill = AMBER,
                       border = "TopBottomLeftRight",
                       borderColour = MGREY)
s_pct   <- createStyle(fontName = "Arial", fontSize = 9,
                       numFmt = "0.0%",
                       border = "TopBottomLeftRight",
                       borderColour = MGREY)
s_curr  <- createStyle(fontName = "Arial", fontSize = 9,
                       numFmt = "$#,##0",
                       border = "TopBottomLeftRight",
                       borderColour = MGREY)
s_num   <- createStyle(fontName = "Arial", fontSize = 9,
                       numFmt = "#,##0",
                       border = "TopBottomLeftRight",
                       borderColour = MGREY)

wb <- createWorkbook()

write_section_hdr <- function(ws, row, col, label, ncols, clr = BLUE) {
  mergeCells(wb, ws, cols = col:(col + ncols - 1), rows = row)
  writeData(wb, ws, label, startRow = row, startCol = col, colNames = FALSE)
  addStyle(wb, ws,
           createStyle(fontName = "Arial", fontSize = 9,
                       fontColour = WHITE, fgFill = clr,
                       textDecoration = "bold", halign = "left",
                       border = "TopBottomLeftRight",
                       borderColour = WHITE),
           rows = row, cols = col:(col + ncols - 1), gridExpand = TRUE)
}

write_patient_table <- function(ws, df, start_row,
                                flag_cols_highlight,
                                curr_cols = NULL,
                                pct_cols  = NULL,
                                num_cols  = NULL) {
  writeData(wb, ws, df, startRow = start_row, colNames = TRUE)
  n_cols <- ncol(df)
  n_rows <- nrow(df)

  addStyle(wb, ws, s_hdr,
           rows = start_row, cols = 1:n_cols, gridExpand = TRUE)

  if (n_rows == 0) return(start_row + n_rows + 1)

  # ---- Classify every row once (vectorized), no per-row loop ----
  # Flagged if any highlight flag column is TRUE for that row.
  flag_mat <- vapply(flag_cols_highlight,
                     function(fc) isTRUE_vec(df[[fc]]),
                     logical(n_rows))
  if (is.null(dim(flag_mat))) flag_mat <- matrix(flag_mat, nrow = n_rows)
  row_flagged <- rowSums(flag_mat) > 0

  shade <- (seq_len(n_rows) %% 2 == 0)

  # Absolute sheet-row indices for each visual category.
  flag_rows  <- start_row + which(row_flagged)
  shade_rows <- start_row + which(!row_flagged & shade)
  plain_rows <- start_row + which(!row_flagged & !shade)

  # ---- Pre-built number-format styles per background (built once) ----
  mk <- function(fmt, fill)
    createStyle(fontName = "Arial", fontSize = 9,
                numFmt = fmt, fgFill = fill,
                border = "TopBottomLeftRight", borderColour = MGREY)
  bg <- list(flag = "#FDECEA", shade = LGREY, plain = WHITE)

  # ---- Apply base row style to each category in one call each ----
  apply_block <- function(rows, base_style, key) {
    if (length(rows) == 0) return(invisible())
    addStyle(wb, ws, base_style, rows = rows, cols = 1:n_cols,
             gridExpand = TRUE)
    if (!is.null(curr_cols))
      addStyle(wb, ws, mk("$#,##0", bg[[key]]),
               rows = rows, cols = curr_cols, gridExpand = TRUE)
    if (!is.null(pct_cols))
      addStyle(wb, ws, mk("0.0%", bg[[key]]),
               rows = rows, cols = pct_cols, gridExpand = TRUE)
    if (!is.null(num_cols))
      addStyle(wb, ws, mk("#,##0", bg[[key]]),
               rows = rows, cols = num_cols, gridExpand = TRUE)
  }

  apply_block(flag_rows,  s_flag,  "flag")
  apply_block(shade_rows, s_shade, "shade")
  apply_block(plain_rows, s_dat,   "plain")

  # ---- All body-row heights in a single call ----
  setRowHeights(wb, ws, rows = start_row + seq_len(n_rows), heights = 15)
  setRowHeights(wb, ws, rows = start_row, heights = 28)

  return(start_row + n_rows + 1)
}

# NA-safe elementwise TRUE test for a logical/flag column.
isTRUE_vec <- function(x) {
  x <- as.logical(x)
  !is.na(x) & x
}

write_summary_block <- function(ws, summary_df, start_row, title) {
  write_section_hdr(ws, start_row, 1, title,
                    ncols = ncol(summary_df), clr = NAVY)
  start_row <- start_row + 1
  writeData(wb, ws, summary_df, startRow = start_row, colNames = TRUE)
  addStyle(wb, ws, s_hdr,
           rows = start_row, cols = 1:ncol(summary_df), gridExpand = TRUE)
  setRowHeights(wb, ws, rows = start_row, heights = 28)
  n <- nrow(summary_df)
  for (i in seq_len(n)) {
    r <- start_row + i
    addStyle(wb, ws, if (i%%2==0) s_shade else s_dat,
             rows = r, cols = 1:ncol(summary_df), gridExpand = TRUE)
    setRowHeights(wb, ws, rows = r, heights = 15)
  }
  return(start_row + n + 2)
}


# ===========================================================
# SHEET 1: LIVE BIRTH
# ===========================================================

addWorksheet(wb, "Live Birth — IVF")
ws <- "Live Birth — IVF"
setColWidths(wb, ws, cols = 1:14,
             widths = c(10,10,10,12,12,12,12,10,10,10,10,14,14,14))

mergeCells(wb, ws, cols = 1:14, rows = 1)
writeData(wb, ws, "LIVE BIRTH (IVF) — ELIGIBILITY FLAGS",
          startRow = 1, startCol = 1, colNames = FALSE)
addStyle(wb, ws, s_title, rows = 1, cols = 1:14, gridExpand = TRUE)
setRowHeights(wb, ws, rows = 1, heights = 22)

mergeCells(wb, ws, cols = 1:14, rows = 2)
writeData(wb, ws,
  paste0("Premium cap: 50% of $", format(cost_livebirth, big.mark=","),
         " = $", format(cost_livebirth * 0.5, big.mark=","),
         " | Combined load: ", round(combined_load, 2), "x",
         " | Prob thresholds: 15%, 10%, 5% calibrated success",
         " | Red rows = any flag active"),
  startRow = 2, startCol = 1, colNames = FALSE)
addStyle(wb, ws,
  createStyle(fontName = "Arial", fontSize = 9, textDecoration = "italic",
              fontColour = NAVY, fgFill = "#E8EEF4"),
  rows = 2, cols = 1:14, gridExpand = TRUE)
setRowHeights(wb, ws, rows = 2, heights = 14)

write_section_hdr(ws, 4, 1,
  "PATIENT-LEVEL FLAGS (all patients, sorted by success probability ascending)",
  ncols = 14)

lb_out <- df_lb %>%
  dplyr::select(age_model, amh, afc,
                p_success, p_fail,
                uncapped_premium_1cy, cap_ceiling_1cy,
                flag_cap_exceeded, flag_below_15pct,
                flag_below_10pct,  flag_below_5pct,
                age_band, amh_quartile, afc_quartile) %>%
  arrange(p_success)

next_row <- write_patient_table(ws, lb_out, start_row = 5,
  flag_cols_highlight = c("flag_cap_exceeded", "flag_below_10pct"),
  curr_cols = 6:7, pct_cols = 4:5)

next_row <- next_row + 1
next_row <- write_summary_block(ws, lb_summary, next_row,
  "Live Birth — Flag Summary by Age Band / AMH Quartile / AFC Quartile")


# ===========================================================
# SHEET 2: EUPLOID EMBRYO
# ===========================================================

addWorksheet(wb, "Euploid Embryo")
ws <- "Euploid Embryo"
setColWidths(wb, ws, cols = 1:14,
             widths = c(10,10,10,12,12,12,12,10,10,10,10,14,14,14))

mergeCells(wb, ws, cols = 1:14, rows = 1)
writeData(wb, ws, "EUPLOID EMBRYO — ELIGIBILITY FLAGS",
          startRow = 1, startCol = 1, colNames = FALSE)
addStyle(wb, ws, s_title, rows = 1, cols = 1:14, gridExpand = TRUE)
setRowHeights(wb, ws, rows = 1, heights = 22)

mergeCells(wb, ws, cols = 1:14, rows = 2)
writeData(wb, ws,
  paste0("Premium cap: 50% of $", format(cost_euploid, big.mark=","),
         " = $", format(cost_euploid * 0.5, big.mark=","),
         " | Combined load: ", round(combined_load, 2), "x",
         " | Prob thresholds: 15%, 10%, 5% calibrated success",
         " | Red rows = any flag active"),
  startRow = 2, startCol = 1, colNames = FALSE)
addStyle(wb, ws,
  createStyle(fontName = "Arial", fontSize = 9, textDecoration = "italic",
              fontColour = NAVY, fgFill = "#E8EEF4"),
  rows = 2, cols = 1:14, gridExpand = TRUE)
setRowHeights(wb, ws, rows = 2, heights = 14)

write_section_hdr(ws, 4, 1,
  "PATIENT-LEVEL FLAGS (all patients, sorted by success probability ascending)",
  ncols = 14)

eu_out <- df_eu %>%
  dplyr::select(age_model, amh, afc,
                p_success, p_fail,
                uncapped_premium_1cy, cap_ceiling_1cy,
                flag_cap_exceeded, flag_below_15pct,
                flag_below_10pct,  flag_below_5pct,
                age_band, amh_quartile, afc_quartile) %>%
  arrange(p_success)

next_row <- write_patient_table(ws, eu_out, start_row = 5,
  flag_cols_highlight = c("flag_cap_exceeded", "flag_below_10pct"),
  curr_cols = 6:7, pct_cols = 4:5)

next_row <- next_row + 1
next_row <- write_summary_block(ws, eu_summary, next_row,
  "Euploid Embryo — Flag Summary by Age Band / AMH Quartile / AFC Quartile")


# ===========================================================
# SHEET 3: EGG FREEZING
# ===========================================================

addWorksheet(wb, "Egg Freezing")
ws <- "Egg Freezing"
setColWidths(wb, ws, cols = 1:16,
             widths = c(10,10,10,10,10,10,10,12,12,10,10,10,14,14,14,40))

mergeCells(wb, ws, cols = 1:16, rows = 1)
writeData(wb, ws, "EGG FREEZING — ELIGIBILITY FLAGS",
          startRow = 1, startCol = 1, colNames = FALSE)
addStyle(wb, ws, s_title, rows = 1, cols = 1:16, gridExpand = TRUE)
setRowHeights(wb, ws, rows = 1, heights = 22)

mergeCells(wb, ws, cols = 1:16, rows = 2)
writeData(wb, ws,
  paste0("Poor responder exclusion: AFC <= ", POR_AFC_THRESHOLD,
         " AND AMH <= ", POR_AMH_THRESHOLD,
         " (Bologna POR criteria) -- ineligible for ALL egg products. ",
         "Premium cap: 50% of $", format(cost_egg1, big.mark=","),
         " = $", format(cost_egg1 * 0.5, big.mark=","),
         " -- ineligible for full refund only. Red rows = any flag active."),
  startRow = 2, startCol = 1, colNames = FALSE)
addStyle(wb, ws,
  createStyle(fontName = "Arial", fontSize = 9, textDecoration = "italic",
              fontColour = NAVY, fgFill = "#E8EEF4"),
  rows = 2, cols = 1:16, gridExpand = TRUE)
setRowHeights(wb, ws, rows = 2, heights = 14)

write_section_hdr(ws, 4, 1,
  "PATIENT-LEVEL FLAGS (all patients, sorted by expected egg yield ascending)",
  ncols = 16)

egg_out <- df_egg %>%
  dplyr::select(age_model, amh, afc,
                mu_1cy, T1_threshold, T_cumulative, T_increment,
                uncapped_premium_1cy, cap_ceiling_1cy,
                flag_poor_responder, flag_cap_exceeded,
                flag_any_egg_ineligible,
                age_band, amh_quartile, afc_quartile,
                clinical_note) %>%
  arrange(mu_1cy)

next_row <- write_patient_table(ws, egg_out, start_row = 5,
  flag_cols_highlight = c("flag_poor_responder",
                          "flag_cap_exceeded",
                          "flag_any_egg_ineligible"),
  curr_cols = 8:9, num_cols = 4:7)

next_row <- next_row + 1
next_row <- write_summary_block(ws, egg_summary, next_row,
  "Egg Freezing — Flag Summary by Age Band / AMH Quartile / AFC Quartile")


# ===========================================================
# SHEET 4: COMBINED SUMMARY
# ===========================================================

addWorksheet(wb, "Combined Summary")
ws <- "Combined Summary"
setColWidths(wb, ws, cols = 1:10,
             widths = c(22,18,14,14,14,14,14,14,14,14))

mergeCells(wb, ws, cols = 1:10, rows = 1)
writeData(wb, ws, "COMBINED ELIGIBILITY FLAG SUMMARY — ALL MODELS",
          startRow = 1, startCol = 1, colNames = FALSE)
addStyle(wb, ws, s_title, rows = 1, cols = 1:10, gridExpand = TRUE)
setRowHeights(wb, ws, rows = 1, heights = 22)

mergeCells(wb, ws, cols = 1:10, rows = 2)
writeData(wb, ws,
  paste0("Premium cap = 50% of total cycle cost (no combined load in ceiling). ",
         "IVF/Euploid prob flags at 15%/10%/5% calibrated success. ",
         "Egg poor responder = AFC <= ", POR_AFC_THRESHOLD,
         " AND AMH <= ", POR_AMH_THRESHOLD, " (Bologna POR). Red = flagged."),
  startRow = 2, startCol = 1, colNames = FALSE)
addStyle(wb, ws,
  createStyle(fontName = "Arial", fontSize = 9, textDecoration = "italic",
              fontColour = NAVY, fgFill = "#E8EEF4"),
  rows = 2, cols = 1:10, gridExpand = TRUE)
setRowHeights(wb, ws, rows = 2, heights = 14)

overall_summary <- data.frame(
  Model = c("Live Birth (IVF)", "Euploid Embryo", "Egg Freezing"),
  N_Patients = c(nrow(df_lb), nrow(df_eu), nrow(df_egg)),
  N_Cap_Exceeded = c(
    sum(df_lb$flag_cap_exceeded,  na.rm = TRUE),
    sum(df_eu$flag_cap_exceeded,  na.rm = TRUE),
    sum(df_egg$flag_cap_exceeded, na.rm = TRUE)
  ),
  Pct_Cap_Exceeded = c(
    round(mean(df_lb$flag_cap_exceeded,  na.rm = TRUE)*100, 1),
    round(mean(df_eu$flag_cap_exceeded,  na.rm = TRUE)*100, 1),
    round(mean(df_egg$flag_cap_exceeded, na.rm = TRUE)*100, 1)
  ),
  N_Below_15pct = c(
    sum(df_lb$flag_below_15pct, na.rm = TRUE),
    sum(df_eu$flag_below_15pct, na.rm = TRUE),
    NA_integer_
  ),
  N_Below_10pct = c(
    sum(df_lb$flag_below_10pct, na.rm = TRUE),
    sum(df_eu$flag_below_10pct, na.rm = TRUE),
    NA_integer_
  ),
  N_Below_5pct = c(
    sum(df_lb$flag_below_5pct, na.rm = TRUE),
    sum(df_eu$flag_below_5pct, na.rm = TRUE),
    NA_integer_
  ),
  N_Egg_Poor_Responder = c(
    NA_integer_, NA_integer_,
    sum(df_egg$flag_poor_responder, na.rm = TRUE)
  ),
  N_Egg_Any_Ineligible = c(
    NA_integer_, NA_integer_,
    sum(df_egg$flag_any_egg_ineligible, na.rm = TRUE)
  ),
  stringsAsFactors = FALSE
)

write_section_hdr(ws, 4, 1, "OVERALL FLAG COUNTS BY MODEL",
                  ncols = ncol(overall_summary), clr = NAVY)
writeData(wb, ws, overall_summary, startRow = 5, colNames = TRUE)
addStyle(wb, ws, s_hdr,
         rows = 5, cols = 1:ncol(overall_summary), gridExpand = TRUE)
setRowHeights(wb, ws, rows = 5, heights = 28)
for (i in seq_len(nrow(overall_summary))) {
  r <- 5 + i
  addStyle(wb, ws, if (i%%2==0) s_shade else s_dat,
           rows = r, cols = 1:ncol(overall_summary), gridExpand = TRUE)
  setRowHeights(wb, ws, rows = r, heights = 15)
}

cur_row <- 5 + nrow(overall_summary) + 3

age_stack <- bind_rows(
  lb_summary  %>% filter(breakdown == "Age Band") %>%
    mutate(model = "Live Birth (IVF)"),
  eu_summary  %>% filter(breakdown == "Age Band") %>%
    mutate(model = "Euploid Embryo"),
  egg_summary %>% filter(breakdown == "Age Band") %>%
    mutate(model = "Egg Freezing")
) %>%
  dplyr::select(model, everything())

cur_row <- write_summary_block(ws, age_stack, cur_row,
  "FLAG SUMMARY BY AGE BAND — ALL MODELS")


############################################################
# SAVE
############################################################

message("Saving workbook...")
saveWorkbook(wb, output_excel, overwrite = TRUE)
message("Done.")

message(paste0(
  "\n+--------------------------------------------------+\n",
  "| DONE\n",
  "| Excel: ", output_excel, "\n",
  "| CSVs:  ", output_dir_csv, "\n",
  "+--------------------------------------------------+\n",
  "\nSUMMARY:\n",
  "  Live Birth   | N=", nrow(df_lb),
  " | Cap exceeded: ", sum(df_lb$flag_cap_exceeded, na.rm=TRUE),
  " (", round(mean(df_lb$flag_cap_exceeded, na.rm=TRUE)*100,1), "%)",
  " | Below 10%: ", sum(df_lb$flag_below_10pct, na.rm=TRUE), "\n",
  "  Euploid      | N=", nrow(df_eu),
  " | Cap exceeded: ", sum(df_eu$flag_cap_exceeded, na.rm=TRUE),
  " (", round(mean(df_eu$flag_cap_exceeded, na.rm=TRUE)*100,1), "%)",
  " | Below 10%: ", sum(df_eu$flag_below_10pct, na.rm=TRUE), "\n",
  "  Egg Freezing | N=", nrow(df_egg),
  " | Poor responder (AFC<=", POR_AFC_THRESHOLD,
  " & AMH<=", POR_AMH_THRESHOLD, "): ",
  sum(df_egg$flag_poor_responder, na.rm=TRUE),
  " (", round(mean(df_egg$flag_poor_responder, na.rm=TRUE)*100,1), "%)",
  " | Cap exceeded: ", sum(df_egg$flag_cap_exceeded, na.rm=TRUE),
  " | Any ineligible: ", sum(df_egg$flag_any_egg_ineligible, na.rm=TRUE), "\n"
))
