############################################################
# FERTILITY ACTUARIAL PRICING -- MASTER SCRIPT
# Version: 6.1 | May 2026
# Author: Charm Economics
#
# CHANGES FROM v6.0:
#   [1] Egg2 age 43+ bias correction added (Section 0 + Section 7)
#   [2] simulate_eggs2() updated with correction
#   [3] Egg2 grid simulation applies correction
#   [4] Winsorization removed; replaced with data validation
#   [5] Calibration parameters updated from validation run
#   [6] Premium formula corrected: premium = fail_prob x payout x load
#       (cycle costs excluded -- patient pays clinic directly)
#   [7] Excel rebuilt: Client Price Sheet, Model Coefficients,
#       Inputs & Key Outputs (patient calculator), Pricing,
#       EggLookup -- all 4 models included
#
# RUN ORDER:
#   STEP 1 -> fertility_data_prep.R   (split + calibration)
#   STEP 2 -> THIS SCRIPT             (models + Excel output)
#
# ============================================================
# !!NOTE!!: THERE ARE EXACTLY 9 PLACES YOU MUST EDIT.
# Every one is marked: vvvvvvvvv !!NOTE!! CHANGE HERE
# Search this string to find all 9. Do not edit anything else.
# ============================================================
############################################################


library(dplyr)
library(MASS)
library(openxlsx)
library(pROC)
library(readr)
library(tidyr)


############################################################
# SECTION 0 -- !!NOTE!! INPUTS
# This is the ONLY section you edit between runs.
############################################################


# ============================================================
# vvvvvvvvv !!NOTE!! CHANGE HERE [1 of 9] -- DATA FRAMES
# ============================================================
# These must be the TRAINING frames from fertility_data_prep.R
# They must contain age_model (floored at 28), NOT raw age.
# df_livebirth must contain ONLY euploid_success = TRUE patients.
#
# Required columns:
#   df_livebirth : birth_bool (0/1), age_model, amh, afc
#   df_euploid   : euploid_success (0/1), age_model, amh, afc
#   df_egg1      : num_eggs_collected (int), age_model, amh, afc
#   df_egg2      : eggs_2cycle_total (int), age_model, amh,
#                  afc, eggs_cycle1 (int)
# ------------------------------------------------------------

# Read in dataset training thingies
df_lb_train = readr::read_rds("data/models/df_lb_train.rds")
df_eu_train = readr::read_rds("data/models/df_eu_train.rds")
df_e1_train = readr::read_rds("data/models/df_e1_train.rds")
df_e2_train = readr::read_rds("data/models/df_e2_train.rds")

df_livebirth <- df_lb_train     # euploid-conditioned live birth
df_euploid   <- df_eu_train
df_egg1      <- df_e1_train
df_egg2      <- df_e2_train


# ============================================================
# vvvvvvvvv !!NOTE!! CHANGE HERE [2 of 9] -- AGE FLOOR
# ============================================================
# Must match age_floor in fertility_data_prep.R. Default: 28.
# ------------------------------------------------------------


age_floor <- 28


# ============================================================
# vvvvvvvvv !!NOTE!! CHANGE HERE [3 of 9] -- ELIGIBILITY CAPS
# ============================================================
# AMH > 12 and AFC > 40 patients were removed in
# fertility_data_prep.R. These values are used here only as
# guardrails for the data validation check below and for the
# Excel MIN/MAX formula caps on the patient input cells.
# Do not change unless clinical guidance changes.
# ------------------------------------------------------------


amh_cap <- 12
afc_cap <- 40


# ============================================================
# vvvvvvvvv !!NOTE!! CHANGE HERE [4 of 9] -- CALIBRATION PARAMS
# ============================================================
# Copy EXACTLY from the console output at the end of
# fertility_data_prep.R. Use full 8-decimal precision.
#
# From April 2026 validation run (post AMH/AFC/egg cleaning):
#   LB  intercept=0.0791  slope=0.9699  -- GOOD (slope near 1)
#   EU  intercept=0.0623  slope=0.8449  -- ACCEPTABLE
#
# The script will STOP if these are left as NA.
# ------------------------------------------------------------


lb_cal_intercept <- 0.07910000
lb_cal_slope     <- 0.96990000
eu_cal_intercept <- 0.06230000
eu_cal_slope     <- 0.84490000


# Safety stop
if (any(is.na(c(lb_cal_intercept, lb_cal_slope,
                eu_cal_intercept, eu_cal_slope)))) {
  stop(paste0(
    "\n\nSTOPPED: Calibration parameters not set.\n",
    "Run fertility_data_prep.R first.\n",
    "Copy the 4 output lines into Section 0 [4/9]."
  ))
}


# ============================================================
# vvvvvvvvv !!NOTE!! CHANGE HERE [5 of 9] -- EGG2 CORRECTION
# ============================================================
# Egg2 validation shows miscalibration at age 43+:
#   observed mean = 14.75, predicted mean = 17.32 (N=44)
#   abs_error = 2.57 -- model over-predicts for older patients
#   Direction of error: thresholds set too HIGH -> liability
#   underpriced for age 43+ -> dangerous for pricing
#
# Correction = observed / predicted = 14.75 / 17.32 = 0.851
# Applied to mu before NB simulation for patients age >= 43.
#
# !!NOTE!!: Update if model is refit and validation changes.
# Document in actuarial file as post-hoc bias correction.
# ------------------------------------------------------------


egg2_age43_obs        <- 14.75
egg2_age43_pred       <- 17.32
egg2_age43_correction <- egg2_age43_obs / egg2_age43_pred
egg2_age43_threshold  <- 43


message(sprintf("Egg2 age 43+ correction factor: %.4f (obs=%.2f / pred=%.2f)",
                egg2_age43_correction,
                egg2_age43_obs,
                egg2_age43_pred))


# ============================================================
# vvvvvvvvv !!NOTE!! CHANGE HERE [6 of 9] -- LOCATION COSTS
# ============================================================
# Update if clinic quotes change. Egg 2-cycle = 0.9 x Egg 1.
# selected_location drives the pool statistics baked into R.
# The Excel location toggle updates individual premiums live.
# Options for selected_location: "NY", "CA", "PDX", "Blended"
# ------------------------------------------------------------


location_costs <- data.frame(
  location   = c("NY",    "CA",    "PDX"),
  euploid    = c(18704,   20376,   16043),
  live_birth = c(21460,   23378,   18404),
  egg1       = c(10700,   10500,    9188),
  stringsAsFactors = FALSE
) %>%
  dplyr::mutate(egg2 = round(egg1 * 0.9, 0))


# Add blended row
blended_row <- data.frame(
  location   = "Blended",
  euploid    = round(mean(location_costs$euploid)),
  live_birth = round(mean(location_costs$live_birth)),
  egg1       = round(mean(location_costs$egg1)),
  egg2       = round(mean(location_costs$egg2))
)
location_costs <- bind_rows(location_costs, blended_row)


selected_location <- "NY"


loc_row        <- location_costs[location_costs$location == selected_location, ]
cost_euploid   <- loc_row$euploid
cost_livebirth <- loc_row$live_birth
cost_egg1      <- loc_row$egg1
cost_egg2      <- loc_row$egg2


# ============================================================
# vvvvvvvvv !!NOTE!! CHANGE HERE [7 of 9] -- MARGINS
# ============================================================
# Combined load = (1 + risk_margin) * (1 + admin_load)
# Current: (1.20)*(1.10) = 32% -- review before production.
# ------------------------------------------------------------


risk_margin  <- 0.20
admin_load   <- 0.10
pool_weight  <- 0.30
premium_cap_pct <- 0.50


# ============================================================
# vvvvvvvvv !!NOTE!! CHANGE HERE [8 of 9] -- MONTE CARLO N_SIM
# ============================================================
# 5000 for development. Set to 10000 for production.
# ------------------------------------------------------------


n_sim <- 5000


# ============================================================
# vvvvvvvvv !!NOTE!! CHANGE HERE [9 of 9] -- OUTPUT PATHS
# ============================================================


output_excel   <- "Fertility_Pricing_Model.xlsx"
output_dir_csv <- "outputs/"


# ---- Fixed product assumptions (do not edit) ----------------
variable_cost <- 6000   # free cycle basis


refund_tiers <- c(
  "Free Cycle"  = 0.00,
  "25% Refund"  = 0.25,
  "50% Refund"  = 0.50,
  "75% Refund"  = 0.75,
  "Full Refund" = 1.00
)


# Monte Carlo grid -- starts at age_floor, AMH/AFC capped
grid_ages     <- c(28,30,32,34,35,36,37,38,39,40,41,42,43)
grid_amh      <- pmin(c(0.3,0.5,1.0,1.5,2.0,3.0,4.0,6.0,10.0,12.0), amh_cap)
grid_afc      <- pmin(c(3,6,9,12,15,20,25,30,40), afc_cap)
grid_cy1_eggs <- c(3,5,7,9,11,14,18)


############################################################
# SECTION 1 -- DATA VALIDATION
# ============================================================
# Replaces winsorization. AMH/AFC outliers were removed in
# fertility_data_prep.R. This confirms clean data before
# fitting. Script stops with a clear message if violated.
############################################################


message("[ 0/7 ] Validating input data ranges...")


stopifnot(
  "AMH > 12 in df_livebirth -- run fertility_data_prep.R first" =
    max(df_livebirth$amh, na.rm = TRUE) <= amh_cap,
  "AMH > 12 in df_euploid -- run fertility_data_prep.R first" =
    max(df_euploid$amh,   na.rm = TRUE) <= amh_cap,
  "AMH > 12 in df_egg1 -- run fertility_data_prep.R first" =
    max(df_egg1$amh,      na.rm = TRUE) <= amh_cap,
  "AMH > 12 in df_egg2 -- run fertility_data_prep.R first" =
    max(df_egg2$amh,      na.rm = TRUE) <= amh_cap,
  "AFC > 40 in df_livebirth -- run fertility_data_prep.R first" =
    max(df_livebirth$afc, na.rm = TRUE) <= afc_cap,
  "AFC > 40 in df_euploid -- run fertility_data_prep.R first" =
    max(df_euploid$afc,   na.rm = TRUE) <= afc_cap,
  "AFC > 40 in df_egg1 -- run fertility_data_prep.R first" =
    max(df_egg1$afc,      na.rm = TRUE) <= afc_cap,
  "AFC > 40 in df_egg2 -- run fertility_data_prep.R first" =
    max(df_egg2$afc,      na.rm = TRUE) <= afc_cap
)


message(sprintf("    Passed: AMH <= %g, AFC <= %g across all frames.",
                amh_cap, afc_cap))


############################################################
# SECTION 2 -- SIMULATION HELPER FUNCTIONS
# Self-contained -- no external source() dependency.
############################################################


simulate_eggs <- function(newdata, n = n_sim, model = egg1_model) {
  mu    <- predict(model, newdata = newdata, type = "response")
  theta <- model$theta
  rnbinom(n, mu = mu, size = theta)
}


# CHANGE [2]: egg2 simulation now applies age 43+ correction
simulate_eggs2 <- function(newdata, n = n_sim, model = egg2_model) {
  mu <- predict(model, newdata = newdata, type = "response")
  # Apply bias correction for patients aged 43+.
  # Validation showed over-prediction at 43+ (obs 14.75, pred 17.32).
  # Correction pulls mu toward observed before drawing from NB.
  # Documented as post-hoc calibration correction in actuarial file.
  if (!is.null(newdata$age_model) &&
      any(newdata$age_model >= egg2_age43_threshold)) {
    mu <- mu * egg2_age43_correction
  }
  theta <- model$theta
  rnbinom(n, mu = mu, size = theta)
}


############################################################
# SECTION 3 -- FIT MODELS
# ============================================================
# All formulas use age_model (floored at 28).
# Column names match actual data from Rmd:
#   birth_bool          (live birth outcome)
#   num_eggs_collected  (egg1 outcome)
#   eggs_2cycle_total   (egg2 outcome)
#
# Model specifications from April 2026 selection:
#   Live birth : birth_bool ~ age_model * amh * afc
#   Euploid    : euploid_success ~ age_model + amh + afc + amh:afc
#   Egg1       : num_eggs_collected ~ age_model + amh + afc + age_model:afc
#   Egg2       : eggs_2cycle_total ~ age_model + amh + afc + eggs_cycle1 + amh:afc
############################################################


message("[ 1/7 ] Fitting models on training data...")


# LIVE BIRTH -- anchored on euploid (df_livebirth = euploid patients only)
livebirth_model <- glm(
  birth_bool ~ age_model * amh * afc,
  data   = df_livebirth,
  family = binomial
)
# Coefficient order (8 terms):
# B2=intercept B3=age_model B4=amh B5=afc
# B6=age_model:amh B7=age_model:afc B8=amh:afc B9=age_model:amh:afc


# EUPLOID -- amh:afc interaction
euploid_model <- glm(
  euploid_success ~ age_model + amh + afc + amh:afc,
  data   = df_euploid,
  family = binomial
)
# Coefficient order (5 terms):
# B2=intercept B3=age_model B4=amh B5=afc B6=amh:afc


# EGG 1 CYCLE -- age_model:afc interaction, negative binomial
egg1_model <- glm.nb(
  num_eggs_collected ~ age_model + amh + afc + age_model:afc,
  data = df_egg1
)
# Coefficient order (5 terms):
# B2=intercept B3=age_model B4=amh B5=afc B6=age_model:afc


# EGG 2 CYCLE -- amh:afc interaction, negative binomial
egg2_model <- glm.nb(
  eggs_2cycle_total ~ age_model + amh + afc + eggs_cycle1 + amh:afc,
  data = df_egg2
)
# Coefficient order (6 terms):
# B2=intercept B3=age_model B4=amh B5=afc B6=eggs_cycle1 B7=amh:afc


message("    Models fitted.")


# Export coefficients
dir.create(output_dir_csv, showWarnings = FALSE, recursive = TRUE)


export_coefs <- function(model, model_name, path) {
  coef(model) %>%
    as.data.frame() %>%
    tibble::rownames_to_column("term") %>%
    dplyr::rename(estimate = 2) %>%
    dplyr::mutate(model = model_name) %>%
    readr::write_csv(path)
}


export_coefs(livebirth_model, "live_birth",
             file.path(output_dir_csv, "live_birth_model_coefficients.csv"))
export_coefs(euploid_model, "euploid",
             file.path(output_dir_csv, "euploid_model_coefficients.csv"))
export_coefs(egg1_model, "egg1_cycle",
             file.path(output_dir_csv, "egg1_model_coefficients.csv"))
export_coefs(egg2_model, "egg2_cycle",
             file.path(output_dir_csv, "egg2_model_coefficients.csv"))


message("    Coefficients exported to: ", output_dir_csv)


############################################################
# SECTION 4 -- APPLY CALIBRATION
# ============================================================
# Uses hardcoded validation-set parameters from Section 0.
# Does NOT refit calibration inline.
# Sanity warnings if slopes look unusual.
############################################################


message("[ 2/7 ] Applying validation calibration parameters...")


apply_calibration <- function(pred_raw, intercept, slope) {
  logit_raw <- log(pmax(pmin(pred_raw, 0.9999), 0.0001) /
                     (1 - pmax(pmin(pred_raw, 0.9999), 0.0001)))
  1 / (1 + exp(-(intercept + slope * logit_raw)))
}


df_livebirth$pred_raw <- predict(livebirth_model, type = "response")
df_euploid$pred_raw   <- predict(euploid_model,   type = "response")


df_livebirth$p_cal <- apply_calibration(
  df_livebirth$pred_raw, lb_cal_intercept, lb_cal_slope)
df_euploid$p_cal   <- apply_calibration(
  df_euploid$pred_raw, eu_cal_intercept, eu_cal_slope)


message(sprintf("    LB: intercept=%.4f slope=%.4f",
                lb_cal_intercept, lb_cal_slope))
message(sprintf("    EU: intercept=%.4f slope=%.4f",
                eu_cal_intercept, eu_cal_slope))


if (abs(lb_cal_intercept) > 0.5 | abs(lb_cal_slope - 1) > 0.5)
  warning("LB calibration params look unusual. Confirm from prep script.")
if (abs(eu_cal_intercept) > 0.5 | abs(eu_cal_slope - 1) > 0.5)
  warning("EU calibration params look unusual. Confirm from prep script.")


############################################################
# SECTION 5 -- EXTRACT MODEL PARAMETERS
############################################################


lb_coefs <- coef(livebirth_model)
eu_coefs <- coef(euploid_model)
e1_coefs <- coef(egg1_model)
e2_coefs <- coef(egg2_model)
e1_theta <- egg1_model$theta
e2_theta <- egg2_model$theta


############################################################
# SECTION 6 -- POOL-LEVEL STATISTICS
############################################################


message("[ 3/7 ] Computing pool statistics...")


pool_p_lb <- mean(df_livebirth$p_cal, na.rm = TRUE)
pool_p_eu <- mean(df_euploid$p_cal,   na.rm = TRUE)
pool_n_lb <- nrow(df_livebirth)
pool_n_eu <- nrow(df_euploid)


pool_fail_lb <- sapply(1:3, function(n)
  mean((1 - df_livebirth$p_cal)^n, na.rm = TRUE))
pool_fail_eu <- sapply(1:3, function(n)
  mean((1 - df_euploid$p_cal)^n,   na.rm = TRUE))


exp_cycles_vec <- function(p_vec, max_cy) {
  mean(sapply(p_vec, function(p) {
    e <- 0; remain <- 1
    for (cy in seq_len(max_cy)) {
      e      <- e + cy * remain * p
      remain <- remain * (1 - p)
    }
    e + max_cy * remain
  }), na.rm = TRUE)
}


pool_exp_cy_lb <- sapply(1:3, function(n)
  exp_cycles_vec(df_livebirth$p_cal, n))
pool_exp_cy_eu <- sapply(1:3, function(n)
  exp_cycles_vec(df_euploid$p_cal, n))


message("    Pool statistics computed.")
message(sprintf("    LB: pool_p=%.4f  fail_1cy=%.4f  fail_2cy=%.4f  fail_3cy=%.4f",
                pool_p_lb, pool_fail_lb[1], pool_fail_lb[2], pool_fail_lb[3]))
message(sprintf("    EU: pool_p=%.4f  fail_1cy=%.4f  fail_2cy=%.4f  fail_3cy=%.4f",
                pool_p_eu, pool_fail_eu[1], pool_fail_eu[2], pool_fail_eu[3]))


############################################################
# SECTION 7 -- MONTE CARLO SIMULATION (EGG THRESHOLD GRID)
# ============================================================
# CHANGE [3]: egg2 grid now applies age 43+ correction.
# mu_raw column preserved for audit.
############################################################


message("[ 4/7 ] Running Monte Carlo simulations...")


set.seed(42)  # Fixed -- do not change between runs


# EGG 1 GRID
egg1_grid <- expand.grid(
  age_model = grid_ages,
  amh       = grid_amh,
  afc       = grid_afc,
  stringsAsFactors = FALSE
) %>%
  rowwise() %>%
  mutate(
    mu      = predict(egg1_model,
                      newdata = data.frame(age_model = age_model,
                                           amh = amh, afc = afc),
                      type = "response"),
    sims    = list(rnbinom(n_sim, mu = mu, size = e1_theta)),
    t90_int = as.integer(floor(quantile(sims[[1]], 0.10))),
    t95_int = as.integer(floor(quantile(sims[[1]], 0.05))),
    mu_rnd  = round(mu, 2)
  ) %>%
  ungroup() %>%
  dplyr::select(age_model, amh, afc, mu_rnd, t90_int, t95_int)


names(egg1_grid) <- c("Age", "AMH", "AFC",
                      "Expected_Eggs_Mean",
                      "Threshold_90pct", "Threshold_95pct")


message(sprintf("    Egg1 done: %d profiles.", nrow(egg1_grid)))


# EGG 2 GRID -- CHANGE [3]: correction applied to mu before simulation
egg2_grid <- expand.grid(
  age_model   = grid_ages,
  amh         = grid_amh,
  afc         = grid_afc,
  eggs_cycle1 = grid_cy1_eggs,
  stringsAsFactors = FALSE
) %>%
  rowwise() %>%
  mutate(
    mu_raw  = predict(egg2_model,
                      newdata = data.frame(age_model   = age_model,
                                           amh         = amh,
                                           afc         = afc,
                                           eggs_cycle1 = eggs_cycle1),
                      type = "response"),
    # Apply age 43+ bias correction before simulation
    # Documented: obs/pred = 14.75/17.32 = 0.851 from validation
    mu      = ifelse(age_model >= egg2_age43_threshold,
                     mu_raw * egg2_age43_correction,
                     mu_raw),
    sims    = list(rnbinom(n_sim, mu = mu, size = e2_theta)),
    t90_int = as.integer(floor(quantile(sims[[1]], 0.10))),
    t95_int = as.integer(floor(quantile(sims[[1]], 0.05))),
    mu_rnd  = round(mu, 2),
    mu_raw_rnd = round(mu_raw, 2),
    corrected = age_model >= egg2_age43_threshold
  ) %>%
  ungroup() %>%
  dplyr::select(age_model, amh, afc, eggs_cycle1,
         mu_rnd, mu_raw_rnd, corrected, t90_int, t95_int)


names(egg2_grid) <- c("Age", "AMH", "AFC", "Eggs_Cycle1",
                      "Expected_Eggs_Mean", "Expected_Eggs_Uncorrected",
                      "Age43_Correction_Applied",
                      "Threshold_90pct", "Threshold_95pct")


message(sprintf("    Egg2 done: %d profiles (%d with age43+ correction).",
                nrow(egg2_grid),
                sum(egg2_grid$Age43_Correction_Applied)))


# Export lookup grids
readr::write_csv(egg1_grid,
                 file.path(output_dir_csv, "egg1_mc_threshold_lookup.csv"))
readr::write_csv(egg2_grid,
                 file.path(output_dir_csv, "egg2_mc_threshold_lookup.csv"))


message("    Monte Carlo complete.")


############################################################
# SECTION 8 -- PREMIUM CALCULATION
# ============================================================
# CHANGE [6]: Corrected formula.
# Patient pays clinic directly for cycles.
# Insurance only covers the refund on failure.
#
# Premium = P(fail) * refund_amount * (1 + risk) * (1 + admin)
#
# Where:
#   refund_amount = payout_pct * cost_per_cycle (full refund)
#                  OR variable_cost (free cycle tier)
#   P(fail) = pool-blended calibrated failure probability
#
# Old formula (REMOVED):
#   premium = (fail*refund + exp_cycles*cost) * load
#
# New formula:
#   premium = fail * refund * load
#   (cycle cost is not in the premium -- patient pays it direct)
############################################################


compute_premium <- function(fail_prob_i, fail_prob_p,
                            cost_cy,     n_cycles,
                            tier_pct,
                            pool_wt  = pool_weight,
                            risk_mgn = risk_margin,
                            adm_load = admin_load,
                            cap_pct  = premium_cap_pct) {
  
  
  # Refund amount per triggered claim
  # Free cycle: only variable cost (meds + monitoring + retrieval)
  # Other tiers: percentage of n_cycles * cost_per_cycle
  refund_amt <- if (tier_pct == 0) {
    variable_cost
  } else {
    tier_pct * n_cycles * cost_cy
  }
  
  
  # Individual expected payout (pure premium)
  indiv_pure <- fail_prob_i * refund_amt
  
  
  # Pool-average expected payout
  pool_pure  <- fail_prob_p * refund_amt
  
  
  # Risk-pooled blend
  blended    <- (1 - pool_wt) * indiv_pure + pool_wt * pool_pure
  
  
  # Cap: premium cannot exceed premium_cap_pct of treatment cost
  cap_value  <- n_cycles * cost_cy * cap_pct
  capped     <- min(blended, cap_value)
  
  
  # Apply loads
  loaded     <- capped * (1 + risk_mgn) * (1 + adm_load)
  
  
  return(round(loaded, 2))
}


############################################################
# SECTION 9 -- BUILD EXCEL WORKBOOK
# ============================================================
# Tab structure (mirrors reference file + our extensions):
#   1. Client Price Sheet  -- clinic costs reference
#   2. Model Coefficients  -- fitted R coefficients, all 4 models
#   3. Inputs & Key Outputs -- patient-level live calculator
#   4. Pricing             -- all products x tiers x cycles
#   5. EggLookup           -- Monte Carlo grid (hidden)
#   6. Pool Stats          -- pool-level diagnostics
############################################################


message("[ 5/7 ] Building Excel workbook...")


wb <- createWorkbook()


# -- Shared styles ------------------------------------------
NAVY   <- "#1F3864"; BLUE   <- "#2F5496"; LBLUE  <- "#BDD7EE"
LLBLUE <- "#DEEAF1"; WHITE  <- "#FFFFFF"; LGRAY  <- "#F2F2F2"
DGRAY  <- "#595959"; GOLD   <- "#FFF2CC"; GOLD_D <- "#7B5800"
GREEN_D<- "#375623"; RED_D  <- "#9C0006"


myfill  <- function(h) PatternFill("solid", start_color=h, fgColor=h)
mybdr   <- function(c="BFBFBF") {
  s <- Side(style="thin", color=c)
  Border(left=s, right=s, top=s, bottom=s)
}


s_hdr  <- createStyle(textDecoration="bold", fgFill=BLUE,
                      fontColour=WHITE, border="TopBottomLeftRight",
                      halign="center", wrapText=TRUE)
s_inp  <- createStyle(fgFill="#FFF9E6", fontColour="#0000FF",
                      border="TopBottomLeftRight")
s_frm  <- createStyle(fgFill=LGRAY, fontColour="#000000",
                      border="TopBottomLeftRight", halign="right")
s_lbl  <- createStyle(textDecoration="bold")
s_note <- createStyle(fontColour=DGRAY, wrapText=TRUE)
s_prem <- createStyle(fgFill="#E2EFDA", fontColour=GREEN_D,
                      textDecoration="bold",
                      border="TopBottomLeftRight",
                      numFmt="$#,##0.00")
s_pkg  <- createStyle(fgFill=GOLD, fontColour=GOLD_D,
                      textDecoration="bold",
                      border="TopBottomLeftRight",
                      numFmt="$#,##0.00")


CURR  <- "$#,##0"
CURR2 <- "$#,##0.00"
PCT2  <- "0.0000"
PCT   <- "0.0%"
DEC2  <- "0.00"
DEC4  <- "0.0000"


############################################################
# TAB 1: CLIENT PRICE SHEET
############################################################


addWorksheet(wb, "Client Price Sheet")
ws_cp <- wb$worksheets[[length(wb$worksheets)]]
setColWidths(wb, "Client Price Sheet",
             cols = 1:6, widths = c(3,60,14,14,14,20))


writeData(wb, "Client Price Sheet",
          "CLIENT PRICE SHEET -- Shared by Fertility Client",
          startRow=1, colNames=FALSE)
addStyle(wb, "Client Price Sheet",
         createStyle(textDecoration="bold", fontSize=13,
                     fgFill=NAVY, fontColour=WHITE),
         rows=1, cols=1)
setRowHeights(wb, "Client Price Sheet", rows=1, heights=30)


# Location cost summary (top section -- what we use)
writeData(wb, "Client Price Sheet",
          "COSTS USED IN PRICING MODEL",
          startRow=3, startCol=2, colNames=FALSE)
addStyle(wb, "Client Price Sheet", s_lbl, rows=3, cols=2)


hdr_data <- data.frame(
  Location = c("CA","NY","PDX","Blended (avg)"),
  Euploid  = location_costs$euploid,
  Live_Birth = location_costs$live_birth,
  Egg_1cycle = location_costs$egg1,
  Egg_2cycle_90pct_off = location_costs$egg2
)
writeData(wb, "Client Price Sheet", hdr_data, startRow=4, colNames=TRUE)
addStyle(wb, "Client Price Sheet", s_hdr, rows=4, cols=1:5, gridExpand=TRUE)
addStyle(wb, "Client Price Sheet",
         createStyle(fgFill="#FFF9E6", fontColour="#0000FF",
                     border="TopBottomLeftRight", numFmt=CURR),
         rows=5:8, cols=2:5, gridExpand=TRUE)
setRowHeights(wb, "Client Price Sheet", rows=4:8, heights=rep(18,5))


writeData(wb, "Client Price Sheet",
          "Note: Egg 2-cycle cost = 0.9 x Egg 1-cycle (10% discount for second cycle).",
          startRow=9, startCol=2, colNames=FALSE)
addStyle(wb, "Client Price Sheet", s_note, rows=9, cols=2)


############################################################
# TAB 2: MODEL COEFFICIENTS
############################################################


addWorksheet(wb, "Model Coefficients")
setColWidths(wb, "Model Coefficients",
             cols=1:5, widths=c(3,28,22,16,40))


writeData(wb, "Model Coefficients",
          "MODEL COEFFICIENTS -- Fitted in R, pasted here for reference",
          startRow=1, colNames=FALSE)
addStyle(wb, "Model Coefficients",
         createStyle(textDecoration="bold", fontSize=13,
                     fgFill=NAVY, fontColour=WHITE),
         rows=1, cols=1)
setRowHeights(wb, "Model Coefficients", rows=1, heights=30)


writeData(wb, "Model Coefficients",
          paste0("Generated by fertility_master_v6_final.R. ",
                 "Coefficients use age_model (floored at 28), amh (capped at 12), ",
                 "afc (capped at 40). Live birth model conditioned on euploid success."),
          startRow=2, startCol=2, colNames=FALSE)
addStyle(wb, "Model Coefficients", s_note, rows=2, cols=2)
setRowHeights(wb, "Model Coefficients", rows=2, heights=20)


# Write all 4 model coefficient tables
coef_start_rows <- c(4, 12, 20, 28)
model_names_list <- list(
  list(name="EUPLOID MODEL",    coefs=eu_coefs,
       family="Binomial logistic | outcome: euploid_success",
       note="amh:afc interaction term"),
  list(name="LIVE BIRTH MODEL", coefs=lb_coefs,
       family="Binomial logistic | outcome: birth_bool (conditional on euploid)",
       note="Full 3-way interaction age_model * amh * afc"),
  list(name="EGG 1-CYCLE MODEL",coefs=e1_coefs,
       family="Negative binomial | outcome: num_eggs_collected",
       note=paste0("age_model:afc interaction | theta=",round(e1_theta,4))),
  list(name="EGG 2-CYCLE MODEL",coefs=e2_coefs,
       family="Negative binomial | outcome: eggs_2cycle_total",
       note=paste0("amh:afc interaction | theta=",round(e2_theta,4),
                   " | age43+ correction=",round(egg2_age43_correction,4)))
)


r <- 4
for (m in model_names_list) {
  writeData(wb, "Model Coefficients", m$name, startRow=r, startCol=2, colNames=FALSE)
  addStyle(wb, "Model Coefficients",
           createStyle(textDecoration="bold", fgFill=NAVY, fontColour=WHITE,
                       border="TopBottomLeftRight"),
           rows=r, cols=2:5, gridExpand=TRUE)
  setRowHeights(wb, "Model Coefficients", rows=r, heights=18)
  r <- r + 1
  
  
  coef_df <- data.frame(
    Model  = names(m$coefs),
    Term   = names(m$coefs),
    Estimate = round(unname(m$coefs), 8),
    Notes  = ""
  )
  coef_df$Notes[1] <- m$family
  if (nrow(coef_df) > 1) coef_df$Notes[2] <- m$note
  
  
  writeData(wb, "Model Coefficients", coef_df, startRow=r, colNames=TRUE)
  addStyle(wb, "Model Coefficients", s_hdr, rows=r, cols=1:4, gridExpand=TRUE)
  n_coef <- nrow(coef_df)
  addStyle(wb, "Model Coefficients",
           createStyle(fgFill=LLBLUE, border="TopBottomLeftRight"),
           rows=(r+1):(r+n_coef), cols=1:4, gridExpand=TRUE)
  setRowHeights(wb, "Model Coefficients",
                rows=r:(r+n_coef), heights=rep(16, n_coef+1))
  r <- r + n_coef + 2
}


# Calibration summary
writeData(wb, "Model Coefficients",
          "CALIBRATION PARAMETERS (from out-of-sample validation)",
          startRow=r, startCol=2, colNames=FALSE)
addStyle(wb, "Model Coefficients",
         createStyle(textDecoration="bold", fgFill=NAVY, fontColour=WHITE,
                     border="TopBottomLeftRight"),
         rows=r, cols=2:5, gridExpand=TRUE)
setRowHeights(wb, "Model Coefficients", rows=r, heights=18)
r <- r + 1


cal_df <- data.frame(
  Model     = c("Live Birth","Euploid"),
  Intercept = c(lb_cal_intercept, eu_cal_intercept),
  Slope     = c(lb_cal_slope,     eu_cal_slope),
  Status    = c(
    ifelse(abs(lb_cal_slope-1) < 0.20, "OK -- slope near 1.0", "REVIEW"),
    ifelse(abs(eu_cal_slope-1) < 0.20, "OK -- slope near 1.0", "REVIEW")
  )
)
writeData(wb, "Model Coefficients", cal_df, startRow=r, colNames=TRUE)
addStyle(wb, "Model Coefficients", s_hdr, rows=r, cols=1:4, gridExpand=TRUE)
addStyle(wb, "Model Coefficients",
         createStyle(fgFill=LLBLUE, border="TopBottomLeftRight"),
         rows=(r+1):(r+2), cols=1:4, gridExpand=TRUE)
setRowHeights(wb, "Model Coefficients", rows=r:(r+2), heights=rep(18,3))


############################################################
# TAB 3: INPUTS & KEY OUTPUTS (PATIENT-LEVEL CALCULATOR)
# ============================================================
# Mirrors the reference file layout.
# Left panel: patient inputs (yellow cells)
# Right panel: computed outputs per model, per cycle count
#
# LAYOUT:
#   A1:C1  = Patient/Clinic Inputs header
#   A2:C9  = Input cells (age, AMH, AFC, location)
#   A10:C15 = Actuarial assumptions (risk margin etc.)
#   A16:C20 = Cost inputs (lookup from Client Price Sheet)
#
#   E1:H20  = Euploid outputs
#   J1:M20  = Live Birth outputs
#   O1:R20  = Egg Model outputs
#
# FORMULA APPROACH:
#   Raw probability = logistic(b0 + b1*eff_age + b2*eff_amh + ...)
#   Calibrated prob = logistic(intercept + slope * logit(p_raw))
#   Failure probs   = (1 - p_cal)^N
#   Expected cycles = geometric stopping rule
#   Expected payout = P(fail) * refund_amount
#   Premium         = P(fail) * refund_amount * load_factor
#   Total package   = expected_treatment_cost + premium
############################################################


addWorksheet(wb, "Inputs & Key Outputs")
setColWidths(wb, "Inputs & Key Outputs",
             cols=1:18, widths=c(3,28,16,3,
                                 24,16,3,
                                 24,16,3,
                                 24,16,3,
                                 24,16,3,
                                 3,3))


# ---- Main title ----
ws_io <- "Inputs & Key Outputs"


writeData(wb, ws_io,
          "FERTILITY ACTUARIAL PRICING -- PATIENT CALCULATOR",
          startRow=1, startCol=1, colNames=FALSE)
addStyle(wb, ws_io,
         createStyle(textDecoration="bold", fontSize=14,
                     fgFill=NAVY, fontColour=WHITE),
         rows=1, cols=1)
setRowHeights(wb, ws_io, rows=1, heights=34)


writeData(wb, ws_io,
          "Enter patient values in YELLOW cells. All outputs update automatically.",
          startRow=2, startCol=2, colNames=FALSE)
addStyle(wb, ws_io, s_note, rows=2, cols=2)
setRowHeights(wb, ws_io, rows=2, heights=16)
setRowHeights(wb, ws_io, rows=3, heights=8)


# ---- LEFT PANEL: Patient inputs ----
writeData(wb, ws_io, "PATIENT / CLINIC INPUTS",
          startRow=4, startCol=2, colNames=FALSE)
addStyle(wb, ws_io,
         createStyle(textDecoration="bold", fgFill=NAVY, fontColour=WHITE,
                     border="TopBottomLeftRight"),
         rows=4, cols=2:3, gridExpand=TRUE)
setRowHeights(wb, ws_io, rows=4, heights=18)


input_rows <- list(
  list(r=5,  lbl="Age (years)",       val=35,    nf="0"),
  list(r=6,  lbl="AMH (ng/mL)",       val=2.0,   nf="0.0"),
  list(r=7,  lbl="AFC",               val=12,    nf="0"),
  list(r=8,  lbl="Clinic location",   val="NY",  nf=NULL)
)
for (inp_row in input_rows) {
  writeData(wb, ws_io, inp_row$lbl,
            startRow=inp_row$r, startCol=2, colNames=FALSE)
  writeData(wb, ws_io, inp_row$val,
            startRow=inp_row$r, startCol=3, colNames=FALSE)
  addStyle(wb, ws_io, s_inp, rows=inp_row$r, cols=3)
  if (!is.null(inp_row$nf))
    addStyle(wb, ws_io,
             createStyle(fgFill="#FFF9E6", fontColour="#0000FF",
                         border="TopBottomLeftRight",
                         numFmt=inp_row$nf),
             rows=inp_row$r, cols=3)
  setRowHeights(wb, ws_io, rows=inp_row$r, heights=20)
}
# !!NOTE!! (Excel user): B5=Age B6=AMH B7=AFC B8=location (NY/CA/PDX)


# Effective inputs (age floor + caps)
setRowHeights(wb, ws_io, rows=9, heights=8)
writeData(wb, ws_io, "EFFECTIVE INPUTS (auto-applied)",
          startRow=10, startCol=2, colNames=FALSE)
addStyle(wb, ws_io,
         createStyle(textDecoration="bold", fgFill=LBLUE, fontColour=NAVY,
                     border="TopBottomLeftRight"),
         rows=10, cols=2:3, gridExpand=TRUE)
setRowHeights(wb, ws_io, rows=10, heights=18)


eff_rows <- list(
  list(r=11, lbl=paste0("Effective age (floor=",age_floor,")"),
       formula=paste0("=MAX(C5,",age_floor,")")),
  list(r=12, lbl=paste0("Effective AMH (cap=",amh_cap,")"),
       formula=paste0("=MIN(C6,",amh_cap,")")),
  list(r=13, lbl=paste0("Effective AFC (cap=",afc_cap,")"),
       formula=paste0("=MIN(C7,",afc_cap,")"))
)
for (er in eff_rows) {
  writeData(wb, ws_io, er$lbl,
            startRow=er$r, startCol=2, colNames=FALSE)
  writeFormula(wb, ws_io, x=er$formula,
               startRow=er$r, startCol=3)
  addStyle(wb, ws_io, s_frm, rows=er$r, cols=3)
  setRowHeights(wb, ws_io, rows=er$r, heights=18)
}
# C11=eff_age  C12=eff_amh  C13=eff_afc


# Actuarial assumptions
setRowHeights(wb, ws_io, rows=14, heights=8)
writeData(wb, ws_io, "ACTUARIAL ASSUMPTIONS",
          startRow=15, startCol=2, colNames=FALSE)
addStyle(wb, ws_io,
         createStyle(textDecoration="bold", fgFill=NAVY, fontColour=WHITE,
                     border="TopBottomLeftRight"),
         rows=15, cols=2:3, gridExpand=TRUE)
setRowHeights(wb, ws_io, rows=15, heights=18)


assm_rows <- list(
  list(r=16, lbl="Risk margin",     val=risk_margin,  nf=PCT),
  list(r=17, lbl="Admin load",      val=admin_load,   nf=PCT),
  list(r=18, lbl="Combined load",   val=NULL,         nf=DEC4),
  list(r=19, lbl="Payout scenario", val="Full Refund",nf=NULL),
  list(r=20, lbl="Refund %",        val=1.0,          nf=PCT)
)
for (ar in assm_rows) {
  writeData(wb, ws_io, ar$lbl,
            startRow=ar$r, startCol=2, colNames=FALSE)
  if (is.null(ar$val)) {
    writeFormula(wb, ws_io,
                 x=paste0("=(1+C16)*(1+C17)"),
                 startRow=ar$r, startCol=3)
    addStyle(wb, ws_io, s_frm, rows=ar$r, cols=3)
    if (!is.null(ar$nf))
      addStyle(wb, ws_io,
               createStyle(fgFill=LGRAY, fontColour="#000000",
                           border="TopBottomLeftRight", numFmt=ar$nf),
               rows=ar$r, cols=3)
  } else {
    writeData(wb, ws_io, ar$val, startRow=ar$r, startCol=3, colNames=FALSE)
    addStyle(wb, ws_io, s_inp, rows=ar$r, cols=3)
    if (!is.null(ar$nf))
      addStyle(wb, ws_io,
               createStyle(fgFill="#FFF9E6", fontColour="#0000FF",
                           border="TopBottomLeftRight", numFmt=ar$nf),
               rows=ar$r, cols=3)
  }
  setRowHeights(wb, ws_io, rows=ar$r, heights=18)
}
# C16=risk_margin C17=admin_load C18=combined_load
# C19=payout_scenario C20=refund_pct


# Cost inputs from Client Price Sheet
setRowHeights(wb, ws_io, rows=21, heights=8)
writeData(wb, ws_io, "COST INPUTS (from Client Price Sheet, based on location C8)",
          startRow=22, startCol=2, colNames=FALSE)
addStyle(wb, ws_io,
         createStyle(textDecoration="bold", fgFill=NAVY, fontColour=WHITE,
                     border="TopBottomLeftRight"),
         rows=22, cols=2:3, gridExpand=TRUE)
setRowHeights(wb, ws_io, rows=22, heights=18)


# VLOOKUP on location_costs written to Client Price Sheet rows 5-8
# Columns on Client Price Sheet: A=Location, B=Euploid, C=Live_Birth, D=Egg1, E=Egg2
cost_lookup_rows <- list(
  list(r=23, lbl="Cost per cycle (Euploid)",
       formula="=VLOOKUP(C8,'Client Price Sheet'!B5:F8,2,FALSE)"),
  list(r=24, lbl="Cost per cycle (Live birth)",
       formula="=VLOOKUP(C8,'Client Price Sheet'!B5:F8,3,FALSE)"),
  list(r=25, lbl="Cost per cycle (Egg 1-cycle)",
       formula="=VLOOKUP(C8,'Client Price Sheet'!B5:F8,4,FALSE)"),
  list(r=26, lbl="Cost per cycle (Egg 2-cycle)",
       formula="=VLOOKUP(C8,'Client Price Sheet'!B5:F8,5,FALSE)")
)
for (cr in cost_lookup_rows) {
  writeData(wb, ws_io, cr$lbl,
            startRow=cr$r, startCol=2, colNames=FALSE)
  writeFormula(wb, ws_io, x=cr$formula,
               startRow=cr$r, startCol=3)
  addStyle(wb, ws_io,
           createStyle(fgFill=LGRAY, fontColour="#006400",
                       border="TopBottomLeftRight", numFmt=CURR),
           rows=cr$r, cols=3)
  setRowHeights(wb, ws_io, rows=cr$r, heights=18)
}
# C23=cost_euploid C24=cost_lb C25=cost_egg1 C26=cost_egg2


# ---- RIGHT PANELS: Model outputs ----
# Column layout:
#   E-F = Euploid outputs
#   H-I = Live Birth outputs
#   K-L = Egg 1-cycle outputs
#   N-O = Egg 2-cycle outputs


models_out <- list(
  list(name="EUPLOID EMBRYO MODEL",
       start_col=5,
       p_raw_formula=paste0(
         "=1/(1+EXP(-(", paste(
           paste0(round(eu_coefs["(Intercept)"],8)),
           paste0(round(eu_coefs["age_model"],8),"*C11"),
           paste0(round(eu_coefs["amh"],8),"*C12"),
           paste0(round(eu_coefs["afc"],8),"*C13"),
           paste0(round(eu_coefs["amh:afc"],8),"*C12*C13"),
           sep="+"
         ), ")))"),
       cal_int_val=eu_cal_intercept,
       cal_slope_val=eu_cal_slope,
       cost_cell="C23",
       pf_pool_1cy=pool_fail_eu[1],
       pf_pool_2cy=pool_fail_eu[2],
       pf_pool_3cy=pool_fail_eu[3]
  ),
  list(name="LIVE BIRTH MODEL",
       start_col=8,
       p_raw_formula=paste0(
         "=1/(1+EXP(-(", paste(
           paste0(round(lb_coefs["(Intercept)"],8)),
           paste0(round(lb_coefs["age_model"],8),"*C11"),
           paste0(round(lb_coefs["amh"],8),"*C12"),
           paste0(round(lb_coefs["afc"],8),"*C13"),
           paste0(round(lb_coefs["age_model:amh"],8),"*C11*C12"),
           paste0(round(lb_coefs["age_model:afc"],8),"*C11*C13"),
           paste0(round(lb_coefs["amh:afc"],8),"*C12*C13"),
           paste0(round(lb_coefs["age_model:amh:afc"],8),"*C11*C12*C13"),
           sep="+"
         ), ")))"),
       cal_int_val=lb_cal_intercept,
       cal_slope_val=lb_cal_slope,
       cost_cell="C24",
       pf_pool_1cy=pool_fail_lb[1],
       pf_pool_2cy=pool_fail_lb[2],
       pf_pool_3cy=pool_fail_lb[3]
  )
)

get_column_letter = function(num){
  LETTERS[num]
}

# For each binary model, build the output panel
for (m_out in models_out) {
  sc <- m_out$start_col
  lc <- sc + 1
  
  
  # Panel header
  writeData(wb, ws_io, m_out$name,
            startRow=4, startCol=sc, colNames=FALSE)
  addStyle(wb, ws_io,
           createStyle(textDecoration="bold", fgFill=NAVY, fontColour=WHITE,
                       border="TopBottomLeftRight"),
           rows=4, cols=sc:lc, gridExpand=TRUE)
  setRowHeights(wb, ws_io, rows=4, heights=18)
  
  
  # Sub-headers
  for (col_i in sc:lc) {
    writeData(wb, ws_io,
              c("Variable","Value")[col_i - sc + 1],
              startRow=5, startCol=col_i, colNames=FALSE)
  }
  addStyle(wb, ws_io, s_hdr, rows=5, cols=sc:lc, gridExpand=TRUE)
  setRowHeights(wb, ws_io, rows=5, heights=18)
  
  
  # p_raw
  writeData(wb, ws_io, "Raw logit",
            startRow=6, startCol=sc, colNames=FALSE)
  p_raw_cell <- paste0(get_column_letter(lc), "6")
  writeFormula(wb, ws_io,
               x=paste0("=LN(", gsub("^=1/\\(1\\+EXP\\(-\\(", "", m_out$p_raw_formula),
                        "/(1-", gsub("^=1/\\(1\\+EXP\\(-\\(", "1/(1+EXP(-(", m_out$p_raw_formula), "))"),
               startRow=6, startCol=lc)
  writeData(wb, ws_io, "Raw probability p_raw",
            startRow=7, startCol=sc, colNames=FALSE)
  writeFormula(wb, ws_io,
               x=m_out$p_raw_formula,
               startRow=7, startCol=lc)
  addStyle(wb, ws_io, s_frm,  rows=6:7, cols=lc, gridExpand=TRUE)
  addStyle(wb, ws_io,
           createStyle(fgFill=LGRAY, fontColour="#000000",
                       border="TopBottomLeftRight", numFmt=DEC4),
           rows=7, cols=lc)
  
  
  p_raw_ref <- paste0(get_column_letter(lc), "7")
  
  
  # p_calibrated
  writeData(wb, ws_io, "Calibrated probability p_cal",
            startRow=8, startCol=sc, colNames=FALSE)
  writeFormula(wb, ws_io,
               x=paste0("=1/(1+EXP(-(",
                        round(m_out$cal_int_val,8), "+",
                        round(m_out$cal_slope_val,8), "*LN(",
                        p_raw_ref, "/(1-", p_raw_ref, "))))"),
               startRow=8, startCol=lc)
  addStyle(wb, ws_io,
           createStyle(fgFill=LGRAY, fontColour="#000000",
                       border="TopBottomLeftRight", numFmt=DEC4),
           rows=8, cols=lc)
  p_cal_ref <- paste0(get_column_letter(lc), "8")
  
  
  # Success/failure probs by cycle
  prob_rows <- list(
    list(r=9,  lbl="1-cycle success prob",     f=paste0("=",p_cal_ref)),
    list(r=10, lbl="2-cycle success prob",     f=paste0("=1-(1-",p_cal_ref,")^2")),
    list(r=11, lbl="3-cycle success prob",     f=paste0("=1-(1-",p_cal_ref,")^3")),
    list(r=12, lbl="1-cycle failure prob",     f=paste0("=(1-",p_cal_ref,")^1")),
    list(r=13, lbl="2-cycle failure prob",     f=paste0("=(1-",p_cal_ref,")^2")),
    list(r=14, lbl="3-cycle failure prob",     f=paste0("=(1-",p_cal_ref,")^3")),
    list(r=15, lbl="No. expected cycles (2cy)",
         f=paste0("=",p_cal_ref,"*1+(1-",p_cal_ref,")*",p_cal_ref,"*2+(1-",p_cal_ref,")^2*2")),
    list(r=16, lbl="Expected treatment cost (2cy)",
         f=paste0("=",get_column_letter(lc),"15*",m_out$cost_cell))
  )
  for (pr in prob_rows) {
    writeData(wb, ws_io, pr$lbl,
              startRow=pr$r, startCol=sc, colNames=FALSE)
    writeFormula(wb, ws_io, x=pr$f,
                 startRow=pr$r, startCol=lc)
    nf_val <- if (pr$r <= 14) DEC4 else if (pr$r == 15) DEC2 else CURR2
    addStyle(wb, ws_io,
             createStyle(fgFill=LGRAY, fontColour="#000000",
                         border="TopBottomLeftRight", numFmt=nf_val),
             rows=pr$r, cols=lc)
    setRowHeights(wb, ws_io, rows=pr$r, heights=18)
  }
  
  
  # Pricing scenarios (1cy, 2cy, 3cy)
  setRowHeights(wb, ws_io, rows=17, heights=8)
  for (cy in 1:3) {
    sc_r <- 17 + (cy-1)*4
    pf_pool <- c(m_out$pf_pool_1cy, m_out$pf_pool_2cy, m_out$pf_pool_3cy)[cy]
    fail_ref <- paste0("=(1-",p_cal_ref,")^",cy)
    
    
    writeData(wb, ws_io, paste0(cy, "-CYCLE SCENARIO"),
              startRow=sc_r, startCol=sc, colNames=FALSE)
    addStyle(wb, ws_io,
             createStyle(textDecoration="bold", fgFill=BLUE, fontColour=WHITE,
                         border="TopBottomLeftRight"),
             rows=sc_r, cols=sc:lc, gridExpand=TRUE)
    setRowHeights(wb, ws_io, rows=sc_r, heights=18)
    
    
    fail_r   <- sc_r + 1
    payout_r <- sc_r + 2
    prem_r   <- sc_r + 3
    
    
    writeData(wb, ws_io, "Failure probability",
              startRow=fail_r, startCol=sc, colNames=FALSE)
    writeFormula(wb, ws_io, x=fail_ref,
                 startRow=fail_r, startCol=lc)
    addStyle(wb, ws_io, s_frm, rows=fail_r, cols=lc)
    addStyle(wb, ws_io,
             createStyle(fgFill=LGRAY, numFmt=DEC4, border="TopBottomLeftRight"),
             rows=fail_r, cols=lc)
    
    
    writeData(wb, ws_io, "Expected payout cost",
              startRow=payout_r, startCol=sc, colNames=FALSE)
    # Expected payout = fail_prob * refund_amount
    # refund_amount = refund_pct * cy * cost (cell C20 = refund_pct)
    writeFormula(wb, ws_io,
                 x=paste0("=",get_column_letter(lc),fail_r,
                          "*C20*",cy,"*",m_out$cost_cell),
                 startRow=payout_r, startCol=lc)
    addStyle(wb, ws_io,
             createStyle(fgFill=LGRAY, numFmt=CURR2, border="TopBottomLeftRight"),
             rows=payout_r, cols=lc)
    
    
    writeData(wb, ws_io, paste0(cy, "-cycle guarantee premium"),
              startRow=prem_r, startCol=sc, colNames=FALSE)
    # Premium = blended_fail * refund * load
    # blended = (1-pool_w)*individual + pool_w*pool_average
    writeFormula(wb, ws_io,
                 x=paste0("=MIN(",
                          "((1-",pool_weight,")*",get_column_letter(lc),fail_r,
                          "+",pool_weight,"*",round(pf_pool,6),")",
                          "*C20*",cy,"*",m_out$cost_cell,
                          "*C18,",
                          cy,"*",m_out$cost_cell,"*",premium_cap_pct,"*C18)"),
                 startRow=prem_r, startCol=lc)
    addStyle(wb, ws_io, s_prem, rows=prem_r, cols=lc)
    setRowHeights(wb, ws_io, rows=c(fail_r,payout_r,prem_r), heights=rep(18,3))
  }
  
  
  # Total package cost rows (treatment + premium)
  tp_start <- 30
  for (cy in 1:3) {
    tp_r <- tp_start + (cy-1)
    prem_r_ref <- paste0(get_column_letter(lc), 17 + (cy-1)*4 + 3)
    cost_n_cy  <- paste0(cy,"*",m_out$cost_cell)
    writeData(wb, ws_io, paste0("Total package cost (",cy,"cy)"),
              startRow=tp_r, startCol=sc, colNames=FALSE)
    writeFormula(wb, ws_io,
                 x=paste0("=",cost_n_cy,"+",prem_r_ref),
                 startRow=tp_r, startCol=lc)
    addStyle(wb, ws_io, s_pkg, rows=tp_r, cols=lc)
    setRowHeights(wb, ws_io, rows=tp_r, heights=18)
  }
}


# Egg model panels (cols 11-12 = Egg1, cols 14-15 = Egg2)
egg_models_out <- list(
  list(name="EGG 1-CYCLE MODEL", start_col=11, cy=1,
       cost_cell="C25",
       coefs=e1_coefs, theta=e1_theta,
       pf_pool=0.10,
       note="Threshold from EggLookup tab via INDEX/MATCH"),
  list(name="EGG 2-CYCLE MODEL", start_col=14, cy=2,
       cost_cell="C26",
       coefs=e2_coefs, theta=e2_theta,
       pf_pool=0.10,
       note=paste0("Age 43+ correction applied (factor=",
                   round(egg2_age43_correction,4),")"))
)


for (em in egg_models_out) {
  sc <- em$start_col
  lc <- sc + 1
  
  
  writeData(wb, ws_io, em$name,
            startRow=4, startCol=sc, colNames=FALSE)
  addStyle(wb, ws_io,
           createStyle(textDecoration="bold", fgFill=NAVY, fontColour=WHITE,
                       border="TopBottomLeftRight"),
           rows=4, cols=sc:lc, gridExpand=TRUE)
  
  
  for (col_i in sc:lc)
    writeData(wb, ws_io,
              c("Variable","Value")[col_i - sc + 1],
              startRow=5, startCol=col_i, colNames=FALSE)
  addStyle(wb, ws_io, s_hdr, rows=5, cols=sc:lc, gridExpand=TRUE)
  
  
  # Expected eggs (mu = exp(linear predictor))
  writeData(wb, ws_io, "Expected eggs (mean mu)",
            startRow=6, startCol=sc, colNames=FALSE)
  if (em$cy == 1) {
    mu_formula <- paste0("=EXP(",
                         round(em$coefs["(Intercept)"],8), "+",
                         round(em$coefs["age_model"],8),    "*C11+",
                         round(em$coefs["amh"],8),          "*C12+",
                         round(em$coefs["afc"],8),          "*C13+",
                         round(em$coefs["age_model:afc"],8),"*C11*C13)")
  } else {
    mu_formula <- paste0("=EXP(",
                         round(em$coefs["(Intercept)"],8),  "+",
                         round(em$coefs["age_model"],8),    "*C11+",
                         round(em$coefs["amh"],8),          "*C12+",
                         round(em$coefs["afc"],8),          "*C13+",
                         round(em$coefs["eggs_cycle1"],8),  "*C7+",
                         round(em$coefs["amh:afc"],8),      "*C12*C13)")
  }
  writeFormula(wb, ws_io, x=mu_formula, startRow=6, startCol=lc)
  addStyle(wb, ws_io,
           createStyle(fgFill=LGRAY, numFmt=DEC2, border="TopBottomLeftRight"),
           rows=6, cols=lc)
  mu_ref <- paste0(get_column_letter(lc), "6")
  
  
  # Threshold from EggLookup
  writeData(wb, ws_io, "90% guarantee threshold (eggs)",
            startRow=7, startCol=sc, colNames=FALSE)
  # INDEX/MATCH on EggLookup
  if (em$cy == 1) {
    lookup_range_age <- paste0("EggLookup!$A$2:$A$", 1+nrow(egg1_grid))
    lookup_range_amh <- paste0("EggLookup!$B$2:$B$", 1+nrow(egg1_grid))
    lookup_range_afc <- paste0("EggLookup!$C$2:$C$", 1+nrow(egg1_grid))
    lookup_range_t90 <- paste0("EggLookup!$E$2:$E$", 1+nrow(egg1_grid))
    match_f <- paste0(
      "=INDEX(", lookup_range_t90, ",MATCH(MIN(",
      "ABS(",lookup_range_age,"-C11)+",
      "ABS(",lookup_range_amh,"-C12)+",
      "ABS(",lookup_range_afc,"-C13)",
      "),ABS(",lookup_range_age,"-C11)+",
      "ABS(",lookup_range_amh,"-C12)+",
      "ABS(",lookup_range_afc,"-C13),0))"
    )
  } else {
    lookup_range_age <- paste0("EggLookup2!$A$2:$A$", 1+nrow(egg2_grid))
    lookup_range_amh <- paste0("EggLookup2!$B$2:$B$", 1+nrow(egg2_grid))
    lookup_range_afc <- paste0("EggLookup2!$C$2:$C$", 1+nrow(egg2_grid))
    lookup_range_cy1 <- paste0("EggLookup2!$D$2:$D$", 1+nrow(egg2_grid))
    lookup_range_t90 <- paste0("EggLookup2!$H$2:$H$", 1+nrow(egg2_grid))
    match_f <- paste0(
      "=INDEX(", lookup_range_t90, ",MATCH(MIN(",
      "ABS(",lookup_range_age,"-C11)+",
      "ABS(",lookup_range_amh,"-C12)+",
      "ABS(",lookup_range_afc,"-C13)+",
      "ABS(",lookup_range_cy1,"-C7)",
      "),ABS(",lookup_range_age,"-C11)+",
      "ABS(",lookup_range_amh,"-C12)+",
      "ABS(",lookup_range_afc,"-C13)+",
      "ABS(",lookup_range_cy1,"-C7),0))"
    )
  }
  writeFormula(wb, ws_io, x=match_f, startRow=7, startCol=lc)
  addStyle(wb, ws_io,
           createStyle(fgFill=LGRAY, numFmt="0", border="TopBottomLeftRight"),
           rows=7, cols=lc)
  t90_ref <- paste0(get_column_letter(lc), "7")
  
  
  # Failure probability (by definition = 10% for 90th pct threshold)
  writeData(wb, ws_io, "Failure probability (definitional)",
            startRow=8, startCol=sc, colNames=FALSE)
  writeData(wb, ws_io, 0.10, startRow=8, startCol=lc, colNames=FALSE)
  addStyle(wb, ws_io,
           createStyle(fgFill=LGRAY, numFmt=DEC4, border="TopBottomLeftRight"),
           rows=8, cols=lc)
  
  
  # Expected payout
  writeData(wb, ws_io, "Expected payout cost",
            startRow=9, startCol=sc, colNames=FALSE)
  # Free cycle: 0.10 * variable_cost
  # Full refund: 0.10 * cy * cost
  writeFormula(wb, ws_io,
               x=paste0("=0.10*C20*",em$cy,"*",em$cost_cell),
               startRow=9, startCol=lc)
  addStyle(wb, ws_io,
           createStyle(fgFill=LGRAY, numFmt=CURR2, border="TopBottomLeftRight"),
           rows=9, cols=lc)
  
  
  # Premium
  writeData(wb, ws_io, paste0(em$cy,"-cycle guarantee premium"),
            startRow=10, startCol=sc, colNames=FALSE)
  writeFormula(wb, ws_io,
               x=paste0("=MIN(",
                        "((1-",pool_weight,")*0.10+",pool_weight,"*",em$pf_pool,")",
                        "*C20*",em$cy,"*",em$cost_cell,"*C18,",
                        em$cy,"*",em$cost_cell,"*",premium_cap_pct,"*C18)"),
               startRow=10, startCol=lc)
  addStyle(wb, ws_io, s_prem, rows=10, cols=lc)
  
  
  # Total package
  writeData(wb, ws_io, "Total package cost",
            startRow=11, startCol=sc, colNames=FALSE)
  writeFormula(wb, ws_io,
               x=paste0("=",em$cy,"*",em$cost_cell,"+",
                        get_column_letter(lc),"10"),
               startRow=11, startCol=lc)
  addStyle(wb, ws_io, s_pkg, rows=11, cols=lc)
  
  
  writeData(wb, ws_io, em$note,
            startRow=12, startCol=sc, colNames=FALSE)
  addStyle(wb, ws_io, s_note, rows=12, cols=sc)
  
  
  for (rr in 6:12)
    setRowHeights(wb, ws_io, rows=rr, heights=20)
}


############################################################
# TAB 4: PRICING (ALL PRODUCTS x TIERS x CYCLES)
############################################################


addWorksheet(wb, "Pricing")
setColWidths(wb, "Pricing",
             cols=1:10, widths=c(3,30,12,12,12,12,12,12,12,3))


writeData(wb, "Pricing",
          "PRICING SCHEDULE -- ALL PRODUCTS x REFUND TIERS x CYCLE COUNTS",
          startRow=1, colNames=FALSE)
addStyle(wb, "Pricing",
         createStyle(textDecoration="bold", fontSize=14,
                     fgFill=NAVY, fontColour=WHITE),
         rows=1, cols=1)
setRowHeights(wb, "Pricing", rows=1, heights=32)


writeData(wb, "Pricing",
          paste0("Location: ", selected_location,
                 " | Pool weight: ", pool_weight*100, "% pool",
                 " | Load: ", round((1+risk_margin)*(1+admin_load)-1,3)*100, "%",
                 " | Premium = P(fail) x refund x load (patient pays cycles direct)"),
          startRow=2, startCol=2, colNames=FALSE)
addStyle(wb, "Pricing", s_note, rows=2, cols=2)
setRowHeights(wb, "Pricing", rows=2:3, heights=c(16,6))


pr_row <- 4
products_pricing <- list(
  list(name="LIVE BIRTH GUARANTEE", cost=cost_livebirth,
       pf_pool=pool_fail_lb, type="binary"),
  list(name="EUPLOID EMBRYO GUARANTEE", cost=cost_euploid,
       pf_pool=pool_fail_eu, type="binary"),
  list(name="EGG RETRIEVAL GUARANTEE -- 1 cycle (90% threshold)",
       cost=cost_egg1, pf_pool=c(0.10,0.10,0.10), type="egg"),
  list(name="EGG RETRIEVAL GUARANTEE -- 2 cycles (90% threshold)",
       cost=cost_egg2, pf_pool=c(0.10,0.10,0.10), type="egg")
)


for (prod in products_pricing) {
  # Section header
  writeData(wb, "Pricing", paste0("  ", prod$name),
            startRow=pr_row, startCol=1, colNames=FALSE)
  addStyle(wb, "Pricing",
           createStyle(textDecoration="bold", fgFill=NAVY, fontColour=WHITE,
                       border="TopBottomLeftRight"),
           rows=pr_row, cols=1:9, gridExpand=TRUE)
  setRowHeights(wb, "Pricing", rows=pr_row, heights=18)
  pr_row <- pr_row + 1
  
  
  # Column headers
  writeData(wb, "Pricing",
            data.frame(
              Tier          = "Refund Tier",
              Payout_1cy    = "1cy Refund",
              Prem_1cy      = "1cy Premium",
              Pkg_1cy       = "1cy Total Package",
              Payout_2cy    = "2cy Refund",
              Prem_2cy      = "2cy Premium",
              Pkg_2cy       = "2cy Total Package",
              Payout_3cy    = "3cy Refund",
              Prem_3cy      = "3cy Premium"
            ),
            startRow=pr_row, colNames=FALSE)
  addStyle(wb, "Pricing", s_hdr,
           rows=pr_row, cols=1:9, gridExpand=TRUE)
  setRowHeights(wb, "Pricing", rows=pr_row, heights=24)
  pr_row <- pr_row + 1
  
  
  for (ti in seq_along(refund_tiers)) {
    tier_name <- names(refund_tiers)[ti]
    tier_pct  <- unname(refund_tiers)[ti]
    bg <- if (ti %% 2 == 0) LLBLUE else WHITE
    
    
    writeData(wb, "Pricing", tier_name,
              startRow=pr_row, startCol=1, colNames=FALSE)
    addStyle(wb, "Pricing",
             createStyle(fgFill=bg, border="TopBottomLeftRight", halign="left"),
             rows=pr_row, cols=1)
    
    
    col_idx <- 2
    for (cy in 1:3) {
      pf_pool   <- prod$pf_pool[min(cy, length(prod$pf_pool))]
      cost_cy   <- prod$cost
      refund_pp <- if (tier_pct == 0) variable_cost else tier_pct * cy * cost_cy
      prem      <- compute_premium(pf_pool, pf_pool, cost_cy, cy,
                                   tier_pct, pool_weight, risk_margin,
                                   admin_load, premium_cap_pct)
      pkg       <- cy * cost_cy + prem
      
      
      writeData(wb, "Pricing", round(refund_pp, 0),
                startRow=pr_row, startCol=col_idx, colNames=FALSE)
      addStyle(wb, "Pricing",
               createStyle(fgFill=bg, border="TopBottomLeftRight",
                           numFmt=CURR, halign="right"),
               rows=pr_row, cols=col_idx)
      
      
      writeData(wb, "Pricing", prem,
                startRow=pr_row, startCol=col_idx+1, colNames=FALSE)
      addStyle(wb, "Pricing", s_prem, rows=pr_row, cols=col_idx+1)
      addStyle(wb, "Pricing",
               createStyle(fgFill="#E2EFDA", fontColour=GREEN_D,
                           textDecoration="bold",
                           border="TopBottomLeftRight", numFmt=CURR2),
               rows=pr_row, cols=col_idx+1)
      
      
      if (cy <= 2) {
        writeData(wb, "Pricing", pkg,
                  startRow=pr_row, startCol=col_idx+2, colNames=FALSE)
        addStyle(wb, "Pricing", s_pkg, rows=pr_row, cols=col_idx+2)
      }
      
      
      col_idx <- col_idx + 3
    }
    
    
    setRowHeights(wb, "Pricing", rows=pr_row, heights=18)
    pr_row <- pr_row + 1
  }
  pr_row <- pr_row + 1
}


############################################################
# TAB 5: EGGLOOKUP (hidden) + EGGLOOKUP2 (hidden)
############################################################


addWorksheet(wb, "EggLookup")
setColWidths(wb, "EggLookup", cols=1:7, widths=c(10,10,10,18,18,3,3))


writeData(wb, "EggLookup",
          paste0("EGG 1-CYCLE THRESHOLD LOOKUP | MC n_sim=",n_sim,
                 " | seed=42 | age_floor=",age_floor,
                 " | amh_cap=",amh_cap," | afc_cap=",afc_cap),
          startRow=1, colNames=FALSE)
addStyle(wb, "EggLookup",
         createStyle(textDecoration="bold", fgFill=NAVY, fontColour=WHITE),
         rows=1, cols=1)
writeData(wb, "EggLookup",
          "Do not edit. Re-run R script to refresh.",
          startRow=1, startCol=6, colNames=FALSE)


writeData(wb, "EggLookup", egg1_grid, startRow=2, colNames=TRUE)
addStyle(wb, "EggLookup", s_hdr,
         rows=2, cols=1:ncol(egg1_grid), gridExpand=TRUE)
addStyle(wb, "EggLookup",
         createStyle(border="TopBottomLeftRight"),
         rows=3:(2+nrow(egg1_grid)), cols=1:ncol(egg1_grid), gridExpand=TRUE)


addWorksheet(wb, "EggLookup2")
setColWidths(wb, "EggLookup2",
             cols=1:10, widths=c(10,10,10,14,18,22,22,14,14,3))


writeData(wb, "EggLookup2",
          paste0("EGG 2-CYCLE THRESHOLD LOOKUP | age43+ correction=",
                 round(egg2_age43_correction,4)),
          startRow=1, colNames=FALSE)
addStyle(wb, "EggLookup2",
         createStyle(textDecoration="bold", fgFill=NAVY, fontColour=WHITE),
         rows=1, cols=1)
writeData(wb, "EggLookup2",
          "Do not edit. Age43_Correction_Applied = TRUE for patients >= 43.",
          startRow=1, startCol=9, colNames=FALSE)


writeData(wb, "EggLookup2", egg2_grid, startRow=2, colNames=TRUE)
addStyle(wb, "EggLookup2", s_hdr,
         rows=2, cols=1:ncol(egg2_grid), gridExpand=TRUE)
addStyle(wb, "EggLookup2",
         createStyle(border="TopBottomLeftRight"),
         rows=3:(2+nrow(egg2_grid)), cols=1:ncol(egg2_grid), gridExpand=TRUE)


# Make lookup tabs hidden -- !!NOTE !! use Inputs & Key Outputs
# worksheetOrder(wb) <- c(1,2,3,4,5,6,7)
# Note: use sheetsVisibility to hide in production
# sheetsVisibility(wb)["EggLookup"]  <- "hidden"
# sheetsVisibility(wb)["EggLookup2"] <- "hidden"


############################################################
# TAB 6: POOL STATS
############################################################


addWorksheet(wb, "Pool Stats")
setColWidths(wb, "Pool Stats", cols=1:3, widths=c(3,44,20))


writeData(wb, "Pool Stats",
          "POOL-LEVEL STATISTICS -- from training data",
          startRow=1, colNames=FALSE)
addStyle(wb, "Pool Stats",
         createStyle(textDecoration="bold", fontSize=12,
                     fgFill=NAVY, fontColour=WHITE),
         rows=1, cols=1)


pool_stats_df <- data.frame(
  Metric = c(
    "Age floor", "AMH cap", "AFC cap",
    "N live birth (training, euploid-anchored)", "N euploid (training)",
    "Pool mean P(LB/cycle) -- calibrated",
    "Pool mean P(EU/cycle) -- calibrated",
    "LB cal intercept (validation)", "LB cal slope (validation)",
    "EU cal intercept (validation)", "EU cal slope (validation)",
    "Pool P(fail LB 1cy)", "Pool P(fail LB 2cy)", "Pool P(fail LB 3cy)",
    "Pool P(fail EU 1cy)", "Pool P(fail EU 2cy)", "Pool P(fail EU 3cy)",
    "Pool E[cycles LB max1]","Pool E[cycles LB max2]","Pool E[cycles LB max3]",
    "Pool E[cycles EU max1]","Pool E[cycles EU max2]","Pool E[cycles EU max3]",
    "Egg1 MC profiles", "Egg2 MC profiles", "MC n_sim",
    "Egg2 age43+ correction factor"
  ),
  Value = round(c(
    age_floor, amh_cap, afc_cap,
    pool_n_lb, pool_n_eu,
    pool_p_lb, pool_p_eu,
    lb_cal_intercept, lb_cal_slope,
    eu_cal_intercept, eu_cal_slope,
    pool_fail_lb, pool_fail_eu,
    pool_exp_cy_lb, pool_exp_cy_eu,
    nrow(egg1_grid), nrow(egg2_grid), n_sim,
    egg2_age43_correction
  ), 6),
  stringsAsFactors = FALSE
)
writeData(wb, "Pool Stats", pool_stats_df, startRow=3, colNames=TRUE)
addStyle(wb, "Pool Stats", s_hdr, rows=3, cols=1:2, gridExpand=TRUE)
addStyle(wb, "Pool Stats",
         createStyle(fgFill=LLBLUE, border="TopBottomLeftRight"),
         rows=4:(3+nrow(pool_stats_df)), cols=1:2, gridExpand=TRUE)


############################################################
# SECTION 10 -- SAVE
############################################################


message("[ 6/7 ] Saving workbook...")
saveWorkbook(wb, output_excel, overwrite = TRUE)
message("[ 7/7 ] Done.")


message(paste0(
  "\n+--------------------------------------------------+\n",
  "| DONE -- saved to: ", output_excel, "\n",
  "+--------------------------------------------------+\n",
  "\n CHECKLIST:\n",
  "  [1/9] Data frames from fertility_data_prep.R\n",
  "  [2/9] age_floor = ", age_floor, "\n",
  "  [3/9] amh_cap = ", amh_cap, " | afc_cap = ", afc_cap, "\n",
  "  [4/9] LB cal: intercept=", lb_cal_intercept,
  " slope=", lb_cal_slope, "\n",
  "        EU cal: intercept=", eu_cal_intercept,
  " slope=", eu_cal_slope, "\n",
  "  [5/9] Egg2 age43+ correction = ",
  round(egg2_age43_correction, 4), "\n",
  "  [6/9] Location=", selected_location,
  " | egg2_age43_correction applied to grid\n",
  "  [7/9] Risk=", risk_margin, " Admin=", admin_load,
  " Combined=", round((1+risk_margin)*(1+admin_load)-1,3)*100, "%\n",
  "  [8/9] n_sim=", n_sim, " (set to 10000 for production)\n",
  "  [9/9] Output: ", output_excel, "\n",
  "\nTEST: Open Excel, go to Inputs & Key Outputs.\n",
  "Enter age=36, AMH=2.0, AFC=12, location=NY.\n",
  "Premiums should appear in green cells.\n",
  "Total package cost should appear in gold cells.\n"
))
