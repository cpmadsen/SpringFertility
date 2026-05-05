############################################################
# SPRING FERTILITY ACTUARIAL PRICING -- MASTER SCRIPT
# Version: 5.1 | May 5, 2026
# Authors: Charm Economics 
# UPDATES:
#   - Column names aligned to actual data (birth_bool,
#     num_eggs_collected, eggs_2cycle_total)
#   - Live birth model now correctly conditioned on euploid
#     success (anchored model), matching the Rmd
#   - Real cost inputs from Rmd (cost_per_cycle = 22014,
#     payout_amount = 17611)
#   - simulate_eggs() and simulate_eggs2() defined inline
#     so script is fully self-contained (no external source)
#   - Model coefficient CSVs exported to outputs/
#   - Multi-cycle probability tables exported to outputs/
#   - age_model (floored) applied consistently throughout
#   - AMH and AFC winsorization applied before model fitting
#
# RUN ORDER:
#   STEP 1 -> fertility_data_prep.R   (split + calibration)
#   STEP 2 -> THIS SCRIPT             (models + Excel output)
#
# +===============================================================+
# |  !!NOTE!!: THERE ARE EXACTLY 9 PLACES YOU MUST EDIT.          |
# |  Every one is marked with:  vvvvvvvvv !!NOTE!! CHANGE HERE    |
# |  Search the document for that string to find them all.        |
# |  Do not edit anything that is not marked.                     |
# +===============================================================+
############################################################

library(dplyr)
library(MASS)
library(openxlsx)
library(pROC)
library(readr)
library(purrr)
library(tidyr)

############################################################
# SECTION 0 --  INPUTS
# ==========================================================
# This is the ONLY section you edit between runs.
############################################################

# ========================================================
# vvvvvvvvv !!NOTE!! CHANGE HERE  [1 of 9] -- DATA FRAMES
# ========================================================
# These must be the TRAINING frames output by
# fertility_data_prep.R -- NOT your raw data.
# They must contain the age_model column (floored at 28).
#
# Required columns:
#   df_livebirth : birth_bool (0/1), age_model, amh, afc
#                  NOTE: this frame must contain ONLY patients
#                  with euploid_success = TRUE (anchored model)
#   df_euploid   : euploid_success (0/1), age_model, amh, afc
#   df_egg1      : num_eggs_collected (int), age_model, amh, afc
#   df_egg2      : eggs_2cycle_total (int), age_model, amh,
#                  afc, eggs_cycle1 (int)
# --------------------------------------------------------

df_livebirth <- df_lb_train     # euploid-conditioned live birth
df_euploid   <- df_eu_train
df_egg1      <- df_e1_train
df_egg2      <- df_e2_train

# ========================================================
# vvvvvvvvv !!NOTE!! CHANGE HERE  [2 of 9] -- AGE FLOOR
# ========================================================
# Must match the age_floor used in fertility_data_prep.R
# --------------------------------------------------------

age_floor <- 30

# ========================================================
# vvvvvvvvv !!NOTE!! CHANGE HERE  [3 of 9] -- WINSORIZATION CAPS
# ========================================================
# Values above these caps are replaced with the cap value
# before model fitting. Based on clinical review:
#   AMH > 12 ng/mL : biologically implausible, likely PCOS
#                    or lab/unit error
#   AFC > 40        : extreme PCOS territory; outside normal
#                    IVF candidate range
# Run these lines first to check how many patients are affected:
#   sum(df_egg1$amh > 12, na.rm = TRUE)
#   sum(df_egg1$afc > 40, na.rm = TRUE)
# If counts are large (>100), consider eligibility exclusion
# rather than winsorization and document the decision.
# --------------------------------------------------------

amh_cap <- 12    # ng/mL -- patients above this are winsorized
afc_cap <- 40    # antral follicle count cap

# ========================================================
# vvvvvvvvv !!NOTE!! CHANGE HERE  [4 of 9] -- CALIBRATION PARAMS
# ========================================================
# Copy EXACTLY from the console output at the end of
# fertility_data_prep.R. These are the VALIDATION-SET
# (out-of-sample) calibration values.
#
# GOOD VALUES: intercept ? (-0.20, 0.20), slope ? (0.80, 1.20)
# From your last prep run:
#   LB  intercept=0.1736  slope=0.7253  (slope flagged -- monitor)
#   EU  intercept=0.0562  slope=0.8816  (acceptable)
#
# Use full 8-decimal precision from your console, not rounded.
# --------------------------------------------------------

lb_cal_intercept <- lb_cal_int_refit   # <- PASTE from prep script output
lb_cal_slope     <- lb_cal_slope_refit   # <- PASTE from prep script output
eu_cal_intercept <- eu_cal_int_refit   # <- PASTE from prep script output
eu_cal_slope     <- eu_cal_slope_refit   # <- PASTE from prep script output

# Safety stop -- script halts immediately if these are not filled
if (any(is.na(c(lb_cal_intercept, lb_cal_slope,
                eu_cal_intercept, eu_cal_slope)))) {
  stop(paste0(
    "\n\n",
    "+==================================================+\n",
    "|  STOPPED: Calibration parameters not set.       |\n",
    "|  Run fertility_data_prep.R first.               |\n",
    "|  Copy the 4 output lines into Section 0 [4/9].  |\n",
    "+==================================================+"
  ))
}

# ========================================================
# vvvvvvvvv !!NOTE!! CHANGE HERE  [5 of 9] -- COST ASSUMPTIONS
# ========================================================
# Costs are defined per location and per product.
# Egg 2-cycle cost is automatically computed as 0.9 x egg1 cost.
# The selected_location variable controls which costs flow into
# the R-side pool statistics and premium calculations.
# The Excel Pricing tab has a live location toggle so you
# can switch between locations without re-running R.
#
# !!NOTE!!: Update costs here if clinic quotes change.
# Do not change the structure -- only update the dollar values.
# --------------------------------------------------------

location_costs <- data.frame(
  location    = c("NY",     "CA",     "PDX"),
  euploid     = c(18704,    20376,    16043),
  live_birth  = c(21460,    23378,    18404),
  egg1        = c(10700,    10500,     9188),
  stringsAsFactors = FALSE
) %>%
  dplyr::mutate(egg2 = round(egg1 * 0.9, 0))

# !!NOTE!!: Set which location is used for R-side pool statistics.
# This does NOT affect the Excel toggle -- it only controls which
# cost is used when computing pool_fail and pool_exp_cy values
# baked into the premium formulas at script run time.
# Recommended: set to the location with the largest expected enrollment.
# Options: "NY", "CA", "PDX"

selected_location <- "NY"

# Extract costs for the selected location (used in pool stats below)
loc_row <- location_costs[location_costs$location == selected_location, ]
cost_euploid   <- loc_row$euploid
cost_livebirth <- loc_row$live_birth
cost_egg1      <- loc_row$egg1
cost_egg2      <- loc_row$egg2

# For legacy compatibility with pool stat functions below,
# define cost_per_cycle_1/2/3 as the live birth cost
# (live birth is the highest cost and most conservative basis)
cost_per_cycle_1 <- cost_livebirth
cost_per_cycle_2 <- cost_livebirth
cost_per_cycle_3 <- cost_livebirth

variable_cost    <- 6000    # free cycle guarantee basis (variable costs only)

# ========================================================
# vvvvvvvvv !!NOTE!! CHANGE HERE  [6 of 9] -- MARGINS
# ========================================================
# From your Rmd: risk_margin=0.20, admin_load=0.10
# Combined load = (1.20)*(1.10) = 32% -- above Sunfish/Gaia
# benchmark of 15-25%. Review before production.
# --------------------------------------------------------

risk_margin <- 0.20   # from Rmd -- review before production
admin_load  <- 0.10   # from Rmd -- review before production

# ========================================================
# vvvvvvvvv !!NOTE!! CHANGE HERE  [7 of 9] -- POOL WEIGHT
# ========================================================

pool_weight <- 0.30   # 70% individual, 30% pool average

# ========================================================
# vvvvvvvvv !!NOTE!! CHANGE HERE  [8 of 9] -- MONTE CARLO N_SIM
# ========================================================
# 5000 for development. Set to 10000 for production run.

n_sim <- 5000

# ========================================================
# vvvvvvvvv !!NOTE!! CHANGE HERE  [9 of 9] -- OUTPUT PATHS
# ========================================================
# Use forward slashes on Windows too: "C:/Users/name/file.xlsx"

output_excel   <- "Fertility_Pricing_Model.xlsx"
output_dir_csv <- "outputs/"   # must exist; CSVs written here

# -- Fixed product assumptions (do not edit) ---------------
refund_tiers <- c(
  "Free Cycle"  = 0.00,
  "25% Refund"  = 0.25,
  "50% Refund"  = 0.50,
  "75% Refund"  = 0.75,
  "Full Refund" = 1.00
)
premium_cap_pct <- 0.50

# Monte Carlo lookup grid -- starts at age_floor
grid_ages     <- c(28,30,32,34,35,36,37,38,39,40,41,42,43)
grid_amh      <- c(0.3,0.5,1.0,1.5,2.0,3.0,4.0,6.0,10.0,12.0)
grid_afc      <- c(3,6,9,12,15,20,25,30,40)
grid_cy1_eggs <- c(3,5,7,9,11,14,18)

############################################################
# 1. WINSORIZE INPUTS
# ==========================================================
# Cap extreme AMH and AFC values before fitting.
# Preserves all patients in the dataset but prevents
# implausible values from distorting model coefficients.
# This is applied to all four data frames consistently.
############################################################

message("[ 0/7 ] Applying winsorization caps...")

winsorize <- function(df, amh_cap, afc_cap) {
  n_amh <- sum(df$amh > amh_cap, na.rm = TRUE)
  n_afc <- sum(df$afc > afc_cap, na.rm = TRUE)
  if (n_amh > 0)
    message(sprintf("    %d AMH values > %g capped at %g",
                    n_amh, amh_cap, amh_cap))
  if (n_afc > 0)
    message(sprintf("    %d AFC values > %g capped at %g",
                    n_afc, afc_cap, afc_cap))
  df %>%
    mutate(
      amh = pmin(amh, amh_cap),
      afc = pmin(afc, afc_cap)
    )
}

df_livebirth <- winsorize(df_livebirth, amh_cap, afc_cap)
df_euploid   <- winsorize(df_euploid,   amh_cap, afc_cap)
df_egg1      <- winsorize(df_egg1,      amh_cap, afc_cap)
df_egg2      <- winsorize(df_egg2,      amh_cap, afc_cap)

message("    Winsorization complete.")

############################################################
# 2. SIMULATION HELPER FUNCTIONS
# ==========================================================
# Defined inline so script is fully self-contained.
# These match the functions in modeling_stats_utility_functions.R
############################################################

simulate_eggs <- function(newdata, n = n_sim, model = egg1_model) {
  mu    <- predict(model, newdata = newdata, type = "response")
  theta <- model$theta
  rnbinom(n, mu = mu, size = theta)
}

simulate_eggs2 <- function(newdata, n = n_sim, model = egg2_model) {
  mu    <- predict(model, newdata = newdata, type = "response")
  theta <- model$theta
  rnbinom(n, mu = mu, size = theta)
}

############################################################
# 3. FIT MODELS
# ==========================================================
# Column names match your actual data from the Rmd:
#   birth_bool        (live birth outcome)
#   num_eggs_collected (egg1 outcome)
#   eggs_2cycle_total  (egg2 outcome)
#   euploid_success    (euploid outcome)
#
# Live birth model is anchored: fit only on patients with
# euploid_success = TRUE, matching your Rmd approach.
# df_livebirth must already be this filtered subset
# (produced by fertility_data_prep.R).
#
# All formulas use age_model (the floored variable).
#
# Model specifications from April 2026 selection:
#   Live birth : birth_bool ~ age_model * amh * afc
#   Euploid    : euploid_success ~ age_model + amh + afc + amh:afc
#   Egg1       : num_eggs_collected ~ age_model + amh + afc + age_model:afc
#   Egg2       : eggs_2cycle_total ~ age_model + amh + afc + eggs_cycle1 + amh:afc
############################################################

message("[ 1/7 ] Fitting models on training data...")

# LIVE BIRTH -- anchored on euploid, full 3-way interaction
# df_livebirth must contain only euploid-success patients
livebirth_model <- glm(
  birth_bool ~ age_model * amh * afc,
  data   = df_livebirth,
  family = binomial
)
# Coefficient order (8 terms):
#   B2=intercept  B3=age_model  B4=amh  B5=afc
#   B6=age_model:amh  B7=age_model:afc
#   B8=amh:afc  B9=age_model:amh:afc

# EUPLOID -- amh:afc interaction
euploid_model <- glm(
  euploid_success ~ age_model + amh + afc + amh:afc,
  data   = df_euploid,
  family = binomial
)
# Coefficient order (5 terms):
#   B2=intercept  B3=age_model  B4=amh  B5=afc  B6=amh:afc

# EGG 1 CYCLE -- negative binomial, age_model:afc interaction
# IMPORTANT: uses num_eggs_collected (matches your Rmd column name)
egg1_model <- glm.nb(
  num_eggs_collected ~ age_model + amh + afc + age_model:afc,
  data = df_egg1
)
# Coefficient order (5 terms):
#   B2=intercept  B3=age_model  B4=amh  B5=afc  B6=age_model:afc

# EGG 2 CYCLE -- negative binomial, amh:afc interaction
# IMPORTANT: uses eggs_2cycle_total (matches your Rmd column name)
egg2_model <- glm.nb(
  eggs_2cycle_total ~ age_model + amh + afc + eggs_cycle1 + amh:afc,
  data = df_egg2
)
# Coefficient order (6 terms):
#   B2=intercept  B3=age_model  B4=amh  B5=afc
#   B6=eggs_cycle1  B7=amh:afc

message("    Models fitted.")

# -- Export coefficients to CSV ----------------------------
# Matching your Rmd export pattern (lines 250-269)

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
export_coefs(euploid_model,   "euploid",
  file.path(output_dir_csv, "euploid_model_coefficients.csv"))
export_coefs(egg1_model,      "egg1_cycle",
  file.path(output_dir_csv, "egg1_model_coefficients.csv"))
export_coefs(egg2_model,      "egg2_cycle",
  file.path(output_dir_csv, "egg2_model_coefficients.csv"))

message("    Coefficients exported to: ", output_dir_csv)

############################################################
# 4. APPLY CALIBRATION PARAMETERS
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

message(sprintf("    LB  calibration: intercept=%.4f  slope=%.4f",
                lb_cal_intercept, lb_cal_slope))
message(sprintf("    EU  calibration: intercept=%.4f  slope=%.4f",
                eu_cal_intercept, eu_cal_slope))

# Sanity warnings
if (abs(lb_cal_intercept) > 0.5 | abs(lb_cal_slope - 1) > 0.5)
  warning("LB calibration params look unusual. Confirm from prep script.")
if (abs(eu_cal_intercept) > 0.5 | abs(eu_cal_slope - 1) > 0.5)
  warning("EU calibration params look unusual. Confirm from prep script.")

############################################################
# 5. MULTI-CYCLE PROBABILITY TABLES
# ==========================================================
# Matching your Rmd export (lines 347-364, 405-424)
# Computes per-patient failure and success probabilities
# across 1, 2, and 3 cycles using calibrated p_cal.
############################################################

message("[ 3/7 ] Computing multi-cycle probability tables...")

livebirth_multi_cycle <- df_livebirth %>%
  dplyr::group_by(pat_id) %>%
  dplyr::summarise(p1 = first(p_cal), .groups = "drop") %>%
  dplyr::mutate(
    one_cycle_success    = 1 - (1 - p1)^1,
    two_cycles_success   = 1 - (1 - p1)^2,
    three_cycles_success = 1 - (1 - p1)^3,
    one_cycle_fail       = (1 - p1)^1,
    two_cycles_fail      = (1 - p1)^2,
    three_cycles_fail    = (1 - p1)^3
  )

euploid_multi_cycle <- df_euploid %>%
  dplyr::group_by(pat_id) %>%
  dplyr::summarise(p1 = first(p_cal), .groups = "drop") %>%
  dplyr::mutate(
    one_cycle_success    = 1 - (1 - p1)^1,
    two_cycles_success   = 1 - (1 - p1)^2,
    three_cycles_success = 1 - (1 - p1)^3,
    one_cycle_fail       = (1 - p1)^1,
    two_cycles_fail      = (1 - p1)^2,
    three_cycles_fail    = (1 - p1)^3
  )

readr::write_csv(livebirth_multi_cycle,
  file.path(output_dir_csv, "livebirth_multicycle_probabilities.csv"))
readr::write_csv(euploid_multi_cycle,
  file.path(output_dir_csv, "euploid_embryo_multicycle_probabilities.csv"))

message("    Multi-cycle tables exported.")

############################################################
# 6. POOL-LEVEL STATISTICS
############################################################

message("[ 4/7 ] Computing pool statistics...")

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

############################################################
# 7. MONTE CARLO SIMULATION -- EGG THRESHOLD LOOKUP TABLE
############################################################

message("[ 5/7 ] Running Monte Carlo simulations...")
message(sprintf("    Egg1: %d profiles x %d sims",
  length(grid_ages)*length(grid_amh)*length(grid_afc), n_sim))

set.seed(42)  # fixed seed -- do not change between runs

# Winsorize grid values to match training caps
grid_amh_capped <- pmin(grid_amh, amh_cap)
grid_afc_capped <- pmin(grid_afc, afc_cap)

egg1_grid <- expand.grid(
  age_model = grid_ages,
  amh       = grid_amh_capped,
  afc       = grid_afc_capped,
  stringsAsFactors = FALSE
) %>%
  rowwise() %>%
  mutate(
    mu      = predict(egg1_model,
                      newdata = data.frame(age_model=age_model,
                                           amh=amh, afc=afc),
                      type = "response"),
    sims    = list(rnbinom(n_sim, mu=mu, size=egg1_model$theta)),
    t90_int = as.integer(floor(quantile(sims[[1]], 0.10))),
    t95_int = as.integer(floor(quantile(sims[[1]], 0.05))),
    mu_rnd  = round(mu, 2)
  ) %>%
  ungroup() %>%
  dplyr::select(age_model, amh, afc, mu_rnd, t90_int, t95_int)

names(egg1_grid) <- c("Age","AMH","AFC",
                       "Expected_Eggs_Mean",
                       "Threshold_90pct","Threshold_95pct")

message(sprintf("    Egg1 done: %d profiles.", nrow(egg1_grid)))

egg2_grid <- expand.grid(
  age_model   = grid_ages,
  amh         = grid_amh_capped,
  afc         = grid_afc_capped,
  eggs_cycle1 = grid_cy1_eggs,
  stringsAsFactors = FALSE
) %>%
  rowwise() %>%
  mutate(
    mu      = predict(egg2_model,
                      newdata = data.frame(age_model=age_model,
                                           amh=amh, afc=afc,
                                           eggs_cycle1=eggs_cycle1),
                      type = "response"),
    sims    = list(rnbinom(n_sim, mu=mu, size=egg2_model$theta)),
    t90_int = as.integer(floor(quantile(sims[[1]], 0.10))),
    t95_int = as.integer(floor(quantile(sims[[1]], 0.05))),
    mu_rnd  = round(mu, 2)
  ) %>%
  ungroup() %>%
  dplyr::select(age_model, amh, afc, eggs_cycle1, mu_rnd, t90_int, t95_int)

names(egg2_grid) <- c("Age","AMH","AFC","Eggs_Cycle1",
                       "Expected_Eggs_Mean",
                       "Threshold_90pct","Threshold_95pct")

message(sprintf("    Egg2 done: %d profiles.", nrow(egg2_grid)))
message("    Monte Carlo complete.")

# Export lookup grids
readr::write_csv(egg1_grid,
  file.path(output_dir_csv, "egg1_mc_threshold_lookup.csv"))
readr::write_csv(egg2_grid,
  file.path(output_dir_csv, "egg2_mc_threshold_lookup.csv"))

############################################################
# 8. BUILD EXCEL WORKBOOK
############################################################

message("[ 6/7 ] Building Excel workbook...")

wb <- createWorkbook()

# -- Shared styles -----------------------------------------
s_hdr  <- createStyle(textDecoration="bold", fgFill="#D6E4F0",
                      border="TopBottomLeftRight", borderColour="#2F5496",
                      halign="center")
s_inp  <- createStyle(fgFill="#FFF9E6", fontColour="#0000FF",
                      border="TopBottomLeftRight", borderColour="#C9A400")
s_frm  <- createStyle(fgFill="#F2F2F2",
                      border="TopBottomLeftRight", halign="right")
s_frm4 <- createStyle(fgFill="#F2F2F2", numFmt="0.0000",
                      border="TopBottomLeftRight", halign="right")
s_frmp <- createStyle(fgFill="#F2F2F2", numFmt="0.00%",
                      border="TopBottomLeftRight", halign="right")
s_frm2 <- createStyle(fgFill="#F2F2F2", numFmt="0.00",
                      border="TopBottomLeftRight", halign="right")
s_lbl  <- createStyle(textDecoration="bold")
s_assm <- createStyle(fgFill="#FCE4D6", fontColour="#833C00",
                      border="TopBottomLeftRight")
s_prem <- createStyle(fgFill="#E2EFDA", fontColour="#375623",
                      textDecoration="bold",
                      border="TopBottomLeftRight", borderColour="#70AD47",
                      numFmt="$#,##0.00")
s_cap  <- createStyle(fgFill="#FFF2CC", fontColour="#7B5800",
                      border="TopBottomLeftRight", numFmt="$#,##0.00")
s_note <- createStyle(fontColour="#595959", textDecoration = "italic", wrapText=TRUE)

############################################################
# TAB: Instructions
############################################################

addWorksheet(wb, "Instructions")
setColWidths(wb, "Instructions", cols=1:3, widths=c(3,36,62))

writeData(wb, "Instructions",
  "FERTILITY ACTUARIAL PRICING TOOL v5.1 --  GUIDE",
  startRow=1, colNames=FALSE)
addStyle(wb, "Instructions",
  createStyle(textDecoration="bold", fontSize=14), rows=1, cols=1)

guide_rows <- list(
  list(r=3,  a="COLOR KEY", b=""),
  list(r=4,  a="Yellow",    b="PATIENT INPUT -- enter here each run"),
  list(r=5,  a="Orange",    b="ASSUMPTIONS -- from R, display only"),
  list(r=6,  a="Grey",      b="FORMULA OUTPUT -- never edit"),
  list(r=7,  a="Green",     b="FINAL PREMIUM -- read results here"),
  list(r=9,  a="KEY NOTES", b=""),
  list(r=10, a="Age floor",
       b=paste0("Patients under ",age_floor," priced at age ",age_floor,
                ". Enter true age -- MAX() applied automatically.")),
  list(r=11, a="AMH cap",
       b=paste0("AMH above ",amh_cap," ng/mL winsorized to ",amh_cap,
                ". Enter true value -- MIN() applied automatically.")),
  list(r=12, a="AFC cap",
       b=paste0("AFC above ",afc_cap," winsorized to ",afc_cap,
                ". Enter true value -- MIN() applied automatically.")),
  list(r=13, a="Live birth model",
       b="Anchored on euploid success -- conditional probability only."),
  list(r=14, a="Calibration",
       b=paste0("LB slope=",round(lb_cal_slope,4),
                " (monitor -- moderate compression). ",
                "EU slope=",round(eu_cal_slope,4)," (acceptable).")),
  list(r=16, a="WORKFLOW", b=""),
  list(r=17, a="Step 1", b="Enter patient values in YELLOW cells on Pricing tab"),
  list(r=18, a="Step 2", b="Read premiums from GREEN cells -- all update automatically"),
  list(r=19, a="Step 3", b="Cap columns show 50% treatment cost ceiling"),
  list(r=21, a="CELL MAP", b=""),
  list(r=22, a="p_cal live birth",  b="LiveBirth!B27"),
  list(r=23, a="p_cal euploid",     b="Euploid!B24"),
  list(r=24, a="Egg1 mu",           b="Egg1!B20"),
  list(r=25, a="Egg2 mu",           b="Egg2!B22"),
  list(r=26, a="All premiums",      b="Pricing tab -- green cells"),
  list(r=27, a="Egg thresholds",    b="EggLookup tab -- do not edit")
)

for (g in guide_rows) {
  writeData(wb, "Instructions", g$a, startRow=g$r, startCol=2, colNames=FALSE)
  if (nchar(g$b) > 0)
    writeData(wb, "Instructions", g$b, startRow=g$r, startCol=3, colNames=FALSE)
  if (g$a %in% c("COLOR KEY","KEY NOTES","WORKFLOW","CELL MAP"))
    addStyle(wb, "Instructions", s_lbl, rows=g$r, cols=2)
}

############################################################
# TAB: Assumptions
############################################################

addWorksheet(wb, "Assumptions")
setColWidths(wb, "Assumptions", cols=1:4, widths=c(3,36,18,46))

writeData(wb, "Assumptions",
  "PRICING ASSUMPTIONS -- all from R Section 0; re-run script to update",
  startRow=1, colNames=FALSE)
addStyle(wb, "Assumptions",
  createStyle(textDecoration="bold", fontSize=12), rows=1, cols=1)

assm_df <- data.frame(
  Parameter = c(
    "Age floor","AMH cap (winsorize)","AFC cap (winsorize)",
    "Cost/cycle -- 1-cycle","Cost/cycle -- 2-cycle","Cost/cycle -- 3-cycle",
    "Variable cost (free cycle basis)","Payout amount (cash refund cap)",
    "Risk margin","Admin load","Combined load factor",
    "Pool weight","Premium cap",
    "Refund: Free Cycle","Refund: 25%","Refund: 50%",
    "Refund: 75%","Refund: Full",
    "Monte Carlo n_sim",
    "LB cal intercept (validation)","LB cal slope (validation)",
    "EU cal intercept (validation)","EU cal slope (validation)",
    "Pool N -- live birth (training)","Pool N -- euploid (training)",
    "Pool mean P(LB/cycle)","Pool mean P(EU/cycle)",
    "Pool P(fail LB 1cy)","Pool P(fail LB 2cy)","Pool P(fail LB 3cy)",
    "Pool P(fail EU 1cy)","Pool P(fail EU 2cy)","Pool P(fail EU 3cy)"
  ),
  Value = round(c(
    age_floor, amh_cap, afc_cap,
    cost_per_cycle_1, cost_per_cycle_2, cost_per_cycle_3,
    variable_cost, payout_amount,
    risk_margin, admin_load,
    (1+risk_margin)*(1+admin_load),
    pool_weight, premium_cap_pct,
    refund_tiers,
    n_sim,
    lb_cal_intercept, lb_cal_slope,
    eu_cal_intercept, eu_cal_slope,
    pool_n_lb, pool_n_eu,
    pool_p_lb, pool_p_eu,
    pool_fail_lb, pool_fail_eu
  ), 6),
  Notes = c(
    "Patients below floored to this age","AMH winsorized above this value",
    "AFC winsorized above this value",
    "From clinic intake form (Rmd line 335)",
    "Update if bundle discount applies",
    "Update if bundle discount applies",
    "Meds + monitoring + retrieval only",
    "Cash refund cap (Rmd line 336)",
    "Review -- 32% combined above benchmark",
    "","(1+risk)*(1+admin)",
    "0=individual 1=pooled","Applied before loading",
    "Free cycle -- variable cost basis only",
    "","","","",
    "Set to 10000 for production",
    "From validation set","From validation set -- monitor (0.73)",
    "From validation set","From validation set",
    "Training N","Training N",
    "Calibrated mean","Calibrated mean",
    rep("Training population",6)
  ),
  stringsAsFactors=FALSE
)

writeData(wb, "Assumptions", assm_df, startRow=3, colNames=TRUE)
addStyle(wb, "Assumptions", s_hdr, rows=3, cols=1:4, gridExpand=TRUE)
addStyle(wb, "Assumptions", s_assm,
  rows=4:(3+nrow(assm_df)), cols=2, gridExpand=TRUE)

############################################################
# TAB: LiveBirth
# ==========================================================
# KEY: outcome column is birth_bool (not live_birth)
#      age input wrapped in MAX(B17, age_floor)
#      amh input wrapped in MIN(B18, amh_cap)
#      afc input wrapped in MIN(B19, afc_cap)
############################################################

addWorksheet(wb, "LiveBirth")
setColWidths(wb, "LiveBirth", cols=1:3, widths=c(3,42,22))

lb_coef_df <- data.frame(
  Variable    = names(coef(livebirth_model)),
  Coefficient = round(unname(coef(livebirth_model)), 8),
  stringsAsFactors=FALSE
)
writeData(wb, "LiveBirth", lb_coef_df, startRow=1, colNames=TRUE)
addStyle(wb, "LiveBirth", s_hdr, rows=1, cols=1:2, gridExpand=TRUE)
# CELL MAP: B2=intercept B3=age_model B4=amh B5=afc
#           B6=age_model:amh B7=age_model:afc B8=amh:afc B9=age_model:amh:afc
# NOTE: outcome is birth_bool (anchored on euploid_success=TRUE patients)

writeData(wb, "LiveBirth",
  "CALIBRATION PARAMETERS -- validation set (fertility_data_prep.R)",
  startRow=11, colNames=FALSE)
addStyle(wb, "LiveBirth", s_lbl, rows=11, cols=1)
writeData(wb, "LiveBirth",
  data.frame(Cal_Intercept=lb_cal_intercept, Cal_Slope=lb_cal_slope),
  startRow=12, colNames=TRUE)
addStyle(wb, "LiveBirth", s_hdr, rows=12, cols=1:2, gridExpand=TRUE)
# CELL MAP: A13=cal_intercept  B13=cal_slope
# NOTE: slope=0.73 is flagged; premiums for extreme profiles
# will be compressed. Monitor on first book of business.

writeData(wb, "LiveBirth", "PATIENT INPUTS (enter true values -- caps applied automatically)",
  startRow=15, colNames=FALSE)
addStyle(wb, "LiveBirth", s_lbl, rows=15, cols=1)
writeData(wb, "LiveBirth",
  data.frame(Input=c("Age (years)","AMH (ng/mL)","AFC"),
             Value=c(NA_real_,NA_real_,NA_real_)),
  startRow=16, colNames=TRUE)
addStyle(wb, "LiveBirth", s_hdr, rows=16, cols=1:2, gridExpand=TRUE)
addStyle(wb, "LiveBirth", s_inp, rows=17:19, cols=2, gridExpand=TRUE)
# !!NOTE!! (Excel user): B17=Age  B18=AMH  B19=AFC

# Effective inputs (floored/capped)
writeData(wb, "LiveBirth", "EFFECTIVE INPUTS (after age floor + AMH/AFC caps)",
  startRow=20, colNames=FALSE)
addStyle(wb, "LiveBirth", s_lbl, rows=20, cols=1)
writeData(wb, "LiveBirth",
  data.frame(Input=c("Effective age","Effective AMH","Effective AFC"),
             Value=NA),
  startRow=21, colNames=TRUE)
addStyle(wb, "LiveBirth", s_hdr, rows=21, cols=1:2, gridExpand=TRUE)
writeFormula(wb, "LiveBirth",
  x=paste0("=MAX(B17,",age_floor,")"), startRow=22, startCol=2)
writeFormula(wb, "LiveBirth",
  x=paste0("=MIN(B18,",amh_cap,")"),  startRow=23, startCol=2)
writeFormula(wb, "LiveBirth",
  x=paste0("=MIN(B19,",afc_cap,")"),  startRow=24, startCol=2)
for (r in 22:24)
  addStyle(wb, "LiveBirth", s_frm2, rows=r, cols=2)
# CELL MAP: B22=eff_age  B23=eff_amh  B24=eff_afc

writeData(wb, "LiveBirth", "RAW PROBABILITY  p_raw", startRow=26, colNames=FALSE)
addStyle(wb, "LiveBirth", s_lbl, rows=26, cols=1)
writeFormula(wb, "LiveBirth",
  # Uses effective inputs: B22=age  B23=amh  B24=afc
  # Coefficients: B2=intercept B3=age B4=amh B5=afc
  #               B6=age:amh B7=age:afc B8=amh:afc B9=age:amh:afc
  x=paste0("=1/(1+EXP(-(B2",
           "+B3*B22",
           "+B4*B23",
           "+B5*B24",
           "+B6*B22*B23",
           "+B7*B22*B24",
           "+B8*B23*B24",
           "+B9*B22*B23*B24",
           ")))"),
  startRow=27, startCol=2)
addStyle(wb, "LiveBirth", s_frm4, rows=27, cols=2)
# CELL MAP: B27 = p_raw

writeData(wb, "LiveBirth", "CALIBRATED PROBABILITY  p_calibrated",
  startRow=29, colNames=FALSE)
addStyle(wb, "LiveBirth", s_lbl, rows=29, cols=1)
writeFormula(wb, "LiveBirth",
  x="=1/(1+EXP(-(A13+B13*LN(B27/(1-B27)))))",
  startRow=30, startCol=2)
addStyle(wb, "LiveBirth", s_frm4, rows=30, cols=2)
# CELL MAP: B30 = p_calibrated  <- Pricing tab reads this

writeData(wb, "LiveBirth", "FAILURE PROBABILITY BY CYCLE COUNT",
  startRow=32, colNames=FALSE)
addStyle(wb, "LiveBirth", s_lbl, rows=32, cols=1)
writeData(wb, "LiveBirth",
  data.frame(Cycles=c("1 cycle","2 cycles","3 cycles"), P_failure=NA),
  startRow=33, colNames=TRUE)
addStyle(wb, "LiveBirth", s_hdr, rows=33, cols=1:2, gridExpand=TRUE)
for (cy in 1:3) {
  writeFormula(wb, "LiveBirth",
    x=paste0("=(1-B30)^",cy), startRow=33+cy, startCol=2)
  addStyle(wb, "LiveBirth", s_frmp, rows=33+cy, cols=2)
}
# CELL MAP: B34=fail_1cy  B35=fail_2cy  B36=fail_3cy

writeData(wb, "LiveBirth", "ACTUARIAL EXPECTED CYCLES",
  startRow=38, colNames=FALSE)
addStyle(wb, "LiveBirth", s_lbl, rows=38, cols=1)
writeData(wb, "LiveBirth",
  data.frame(Cycles=c("1 cycle","2 cycles","3 cycles"), Exp_Cycles=NA),
  startRow=39, colNames=TRUE)
addStyle(wb, "LiveBirth", s_hdr, rows=39, cols=1:2, gridExpand=TRUE)
writeFormula(wb, "LiveBirth", x="=1", startRow=40, startCol=2)
writeFormula(wb, "LiveBirth",
  x="=B30*1+(1-B30)*B30*2+(1-B30)^2*2", startRow=41, startCol=2)
writeFormula(wb, "LiveBirth",
  x="=B30*1+(1-B30)*B30*2+(1-B30)^2*B30*3+(1-B30)^3*3",
  startRow=42, startCol=2)
for (r in 40:42)
  addStyle(wb, "LiveBirth", s_frm2, rows=r, cols=2)
# CELL MAP: B40=exp_cy1  B41=exp_cy2  B42=exp_cy3

############################################################
# TAB: Euploid
############################################################

addWorksheet(wb, "Euploid")
setColWidths(wb, "Euploid", cols=1:3, widths=c(3,42,22))

eu_coef_df <- data.frame(
  Variable    = names(coef(euploid_model)),
  Coefficient = round(unname(coef(euploid_model)), 8),
  stringsAsFactors=FALSE
)
writeData(wb, "Euploid", eu_coef_df, startRow=1, colNames=TRUE)
addStyle(wb, "Euploid", s_hdr, rows=1, cols=1:2, gridExpand=TRUE)
# CELL MAP: B2=intercept B3=age_model B4=amh B5=afc B6=amh:afc

writeData(wb, "Euploid",
  "CALIBRATION PARAMETERS -- validation set",
  startRow=8, colNames=FALSE)
addStyle(wb, "Euploid", s_lbl, rows=8, cols=1)
writeData(wb, "Euploid",
  data.frame(Cal_Intercept=eu_cal_intercept, Cal_Slope=eu_cal_slope),
  startRow=9, colNames=TRUE)
addStyle(wb, "Euploid", s_hdr, rows=9, cols=1:2, gridExpand=TRUE)
# CELL MAP: A10=cal_intercept  B10=cal_slope

writeData(wb, "Euploid",
  "PATIENT INPUTS (enter true values -- caps applied automatically)",
  startRow=12, colNames=FALSE)
addStyle(wb, "Euploid", s_lbl, rows=12, cols=1)
writeData(wb, "Euploid",
  data.frame(Input=c("Age (years)","AMH (ng/mL)","AFC"),
             Value=c(NA,NA,NA)),
  startRow=13, colNames=TRUE)
addStyle(wb, "Euploid", s_hdr, rows=13, cols=1:2, gridExpand=TRUE)
addStyle(wb, "Euploid", s_inp, rows=14:16, cols=2, gridExpand=TRUE)
# !!NOTE!!: B14=Age  B15=AMH  B16=AFC

writeData(wb, "Euploid", "EFFECTIVE INPUTS", startRow=17, colNames=FALSE)
addStyle(wb, "Euploid", s_lbl, rows=17, cols=1)
writeData(wb, "Euploid",
  data.frame(Input=c("Effective age","Effective AMH","Effective AFC"), Value=NA),
  startRow=18, colNames=TRUE)
addStyle(wb, "Euploid", s_hdr, rows=18, cols=1:2, gridExpand=TRUE)
writeFormula(wb, "Euploid",
  x=paste0("=MAX(B14,",age_floor,")"), startRow=19, startCol=2)
writeFormula(wb, "Euploid",
  x=paste0("=MIN(B15,",amh_cap,")"),  startRow=20, startCol=2)
writeFormula(wb, "Euploid",
  x=paste0("=MIN(B16,",afc_cap,")"),  startRow=21, startCol=2)
for (r in 19:21)
  addStyle(wb, "Euploid", s_frm2, rows=r, cols=2)
# CELL MAP: B19=eff_age  B20=eff_amh  B21=eff_afc

writeData(wb, "Euploid", "RAW PROBABILITY  p_raw", startRow=23, colNames=FALSE)
addStyle(wb, "Euploid", s_lbl, rows=23, cols=1)
writeFormula(wb, "Euploid",
  # B2=intercept B3=age B4=amh B5=afc B6=amh:afc
  # B19=eff_age  B20=eff_amh  B21=eff_afc
  x="=1/(1+EXP(-(B2+B3*B19+B4*B20+B5*B21+B6*B20*B21)))",
  startRow=24, startCol=2)
addStyle(wb, "Euploid", s_frm4, rows=24, cols=2)
# CELL MAP: B24 = p_raw

writeData(wb, "Euploid", "CALIBRATED PROBABILITY  p_calibrated",
  startRow=26, colNames=FALSE)
addStyle(wb, "Euploid", s_lbl, rows=26, cols=1)
writeFormula(wb, "Euploid",
  x="=1/(1+EXP(-(A10+B10*LN(B24/(1-B24)))))",
  startRow=27, startCol=2)
addStyle(wb, "Euploid", s_frm4, rows=27, cols=2)
# CELL MAP: B27 = p_calibrated  <- Pricing tab reads this

writeData(wb, "Euploid", "FAILURE PROBABILITY BY CYCLE COUNT",
  startRow=29, colNames=FALSE)
addStyle(wb, "Euploid", s_lbl, rows=29, cols=1)
writeData(wb, "Euploid",
  data.frame(Cycles=c("1 cycle","2 cycles","3 cycles"), P_failure=NA),
  startRow=30, colNames=TRUE)
addStyle(wb, "Euploid", s_hdr, rows=30, cols=1:2, gridExpand=TRUE)
for (cy in 1:3) {
  writeFormula(wb, "Euploid",
    x=paste0("=(1-B27)^",cy), startRow=30+cy, startCol=2)
  addStyle(wb, "Euploid", s_frmp, rows=30+cy, cols=2)
}
# CELL MAP: B31=fail_1cy  B32=fail_2cy  B33=fail_3cy

writeData(wb, "Euploid", "ACTUARIAL EXPECTED CYCLES", startRow=35, colNames=FALSE)
addStyle(wb, "Euploid", s_lbl, rows=35, cols=1)
writeData(wb, "Euploid",
  data.frame(Cycles=c("1 cycle","2 cycles","3 cycles"), Exp_Cycles=NA),
  startRow=36, colNames=TRUE)
addStyle(wb, "Euploid", s_hdr, rows=36, cols=1:2, gridExpand=TRUE)
writeFormula(wb, "Euploid", x="=1", startRow=37, startCol=2)
writeFormula(wb, "Euploid",
  x="=B27*1+(1-B27)*B27*2+(1-B27)^2*2", startRow=38, startCol=2)
writeFormula(wb, "Euploid",
  x="=B27*1+(1-B27)*B27*2+(1-B27)^2*B27*3+(1-B27)^3*3",
  startRow=39, startCol=2)
for (r in 37:39)
  addStyle(wb, "Euploid", s_frm2, rows=r, cols=2)
# CELL MAP: B37=exp_cy1  B38=exp_cy2  B39=exp_cy3

############################################################
# TAB: Egg1
# ==========================================================
# KEY: outcome column is num_eggs_collected (from Rmd)
############################################################

addWorksheet(wb, "Egg1")
setColWidths(wb, "Egg1", cols=1:3, widths=c(3,42,22))

e1_coef_df <- data.frame(
  Variable    = names(coef(egg1_model)),
  Coefficient = round(unname(coef(egg1_model)), 8),
  stringsAsFactors=FALSE
)
writeData(wb, "Egg1", e1_coef_df, startRow=1, colNames=TRUE)
addStyle(wb, "Egg1", s_hdr, rows=1, cols=1:2, gridExpand=TRUE)
# CELL MAP: B2=intercept B3=age_model B4=amh B5=afc B6=age_model:afc
# NOTE: outcome is num_eggs_collected

writeData(wb, "Egg1",
  data.frame(Parameter="Theta -- NB dispersion",
             Value=round(egg1_model$theta,6)),
  startRow=8, colNames=TRUE)
addStyle(wb, "Egg1", s_hdr, rows=8, cols=1:2, gridExpand=TRUE)
# CELL MAP: B9=theta

writeData(wb, "Egg1",
  "PATIENT INPUTS (enter true values -- caps applied automatically)",
  startRow=11, colNames=FALSE)
addStyle(wb, "Egg1", s_lbl, rows=11, cols=1)
writeData(wb, "Egg1",
  data.frame(Input=c("Age (years)","AMH (ng/mL)","AFC"),
             Value=c(NA,NA,NA)),
  startRow=12, colNames=TRUE)
addStyle(wb, "Egg1", s_hdr, rows=12, cols=1:2, gridExpand=TRUE)
addStyle(wb, "Egg1", s_inp, rows=13:15, cols=2, gridExpand=TRUE)
# !!NOTE!!: B13=Age  B14=AMH  B15=AFC

writeData(wb, "Egg1", "EFFECTIVE INPUTS", startRow=16, colNames=FALSE)
addStyle(wb, "Egg1", s_lbl, rows=16, cols=1)
writeData(wb, "Egg1",
  data.frame(Input=c("Effective age","Effective AMH","Effective AFC"), Value=NA),
  startRow=17, colNames=TRUE)
addStyle(wb, "Egg1", s_hdr, rows=17, cols=1:2, gridExpand=TRUE)
writeFormula(wb, "Egg1",
  x=paste0("=MAX(B13,",age_floor,")"), startRow=18, startCol=2)
writeFormula(wb, "Egg1",
  x=paste0("=MIN(B14,",amh_cap,")"),  startRow=19, startCol=2)
writeFormula(wb, "Egg1",
  x=paste0("=MIN(B15,",afc_cap,")"),  startRow=20, startCol=2)
for (r in 18:20)
  addStyle(wb, "Egg1", s_frm2, rows=r, cols=2)
# CELL MAP: B18=eff_age  B19=eff_amh  B20=eff_afc

writeData(wb, "Egg1", "EXPECTED EGGS -- mean (mu)", startRow=22, colNames=FALSE)
addStyle(wb, "Egg1", s_lbl, rows=22, cols=1)
writeFormula(wb, "Egg1",
  # B18=eff_age  B19=eff_amh  B20=eff_afc
  # B6=age_model:afc interaction
  x="=EXP(B2+B3*B18+B4*B19+B5*B20+B6*B18*B20)",
  startRow=23, startCol=2)
addStyle(wb, "Egg1",
  createStyle(fgFill="#F2F2F2", numFmt="0.0", border="TopBottomLeftRight"),
  rows=23, cols=2)
# CELL MAP: B23 = mu (expected eggs)

writeData(wb, "Egg1",
  "Thresholds retrieved from EggLookup via INDEX/MATCH on Pricing tab.",
  startRow=25, colNames=FALSE)
addStyle(wb, "Egg1", s_note, rows=25, cols=1)

############################################################
# TAB: Egg2
############################################################

addWorksheet(wb, "Egg2")
setColWidths(wb, "Egg2", cols=1:3, widths=c(3,42,22))

e2_coef_df <- data.frame(
  Variable    = names(coef(egg2_model)),
  Coefficient = round(unname(coef(egg2_model)), 8),
  stringsAsFactors=FALSE
)
writeData(wb, "Egg2", e2_coef_df, startRow=1, colNames=TRUE)
addStyle(wb, "Egg2", s_hdr, rows=1, cols=1:2, gridExpand=TRUE)
# CELL MAP: B2=intercept B3=age_model B4=amh B5=afc B6=eggs_cycle1 B7=amh:afc
# NOTE: outcome is eggs_2cycle_total

writeData(wb, "Egg2",
  data.frame(Parameter="Theta -- NB dispersion",
             Value=round(egg2_model$theta,6)),
  startRow=9, colNames=TRUE)
addStyle(wb, "Egg2", s_hdr, rows=9, cols=1:2, gridExpand=TRUE)
# CELL MAP: B10=theta

writeData(wb, "Egg2",
  "PATIENT INPUTS (enter true values -- caps applied automatically)",
  startRow=12, colNames=FALSE)
addStyle(wb, "Egg2", s_lbl, rows=12, cols=1)
writeData(wb, "Egg2",
  data.frame(Input=c("Age (years)","AMH (ng/mL)","AFC",
                     "Eggs cycle 1 (actual count)"),
             Value=c(NA,NA,NA,NA)),
  startRow=13, colNames=TRUE)
addStyle(wb, "Egg2", s_hdr, rows=13, cols=1:2, gridExpand=TRUE)
addStyle(wb, "Egg2", s_inp, rows=14:17, cols=2, gridExpand=TRUE)
# !!NOTE!!: B14=Age  B15=AMH  B16=AFC  B17=eggs_cycle1

writeData(wb, "Egg2", "EFFECTIVE INPUTS", startRow=18, colNames=FALSE)
addStyle(wb, "Egg2", s_lbl, rows=18, cols=1)
writeData(wb, "Egg2",
  data.frame(Input=c("Effective age","Effective AMH","Effective AFC"), Value=NA),
  startRow=19, colNames=TRUE)
addStyle(wb, "Egg2", s_hdr, rows=19, cols=1:2, gridExpand=TRUE)
writeFormula(wb, "Egg2",
  x=paste0("=MAX(B14,",age_floor,")"), startRow=20, startCol=2)
writeFormula(wb, "Egg2",
  x=paste0("=MIN(B15,",amh_cap,")"),  startRow=21, startCol=2)
writeFormula(wb, "Egg2",
  x=paste0("=MIN(B16,",afc_cap,")"),  startRow=22, startCol=2)
for (r in 20:22)
  addStyle(wb, "Egg2", s_frm2, rows=r, cols=2)
# CELL MAP: B20=eff_age  B21=eff_amh  B22=eff_afc

writeData(wb, "Egg2", "EXPECTED EGGS -- 2 cycles (mu)", startRow=24, colNames=FALSE)
addStyle(wb, "Egg2", s_lbl, rows=24, cols=1)
writeFormula(wb, "Egg2",
  # B20=eff_age B21=eff_amh B22=eff_afc B17=eggs_cycle1
  # B7=amh:afc interaction
  x="=EXP(B2+B3*B20+B4*B21+B5*B22+B6*B17+B7*B21*B22)",
  startRow=25, startCol=2)
addStyle(wb, "Egg2",
  createStyle(fgFill="#F2F2F2", numFmt="0.0", border="TopBottomLeftRight"),
  rows=25, cols=2)
# CELL MAP: B25 = mu

writeData(wb, "Egg2",
  "Thresholds retrieved from EggLookup via INDEX/MATCH on Pricing tab.",
  startRow=27, colNames=FALSE)
addStyle(wb, "Egg2", s_note, rows=27, cols=1)

############################################################
# TAB: EggLookup
############################################################

addWorksheet(wb, "EggLookup")
setColWidths(wb, "EggLookup", cols=1:8, widths=c(3,10,10,10,14,14,14,3))

writeData(wb, "EggLookup",
  paste0("EGG THRESHOLD LOOKUP -- MC (n_sim=",n_sim,
         ", seed=42, AMH cap=",amh_cap,", AFC cap=",afc_cap,")"),
  startRow=1, colNames=FALSE)
addStyle(wb, "EggLookup",
  createStyle(textDecoration="bold", fontSize=12), rows=1, cols=1)
writeData(wb, "EggLookup",
  "Do not edit. All values from Monte Carlo simulation. Re-run R to refresh.",
  startRow=2, colNames=FALSE)
addStyle(wb, "EggLookup", s_note, rows=2, cols=1)

writeData(wb, "EggLookup", "EGG 1-CYCLE -- num_eggs_collected model",
  startRow=4, colNames=FALSE)
addStyle(wb, "EggLookup", s_lbl, rows=4, cols=1)
writeData(wb, "EggLookup", egg1_grid, startRow=5, colNames=TRUE)
addStyle(wb, "EggLookup", s_hdr,
  rows=5, cols=1:ncol(egg1_grid), gridExpand=TRUE)
egg1_lookup_start <- 6
egg1_lookup_end   <- 5 + nrow(egg1_grid)
addStyle(wb, "EggLookup",
  createStyle(border="TopBottomLeftRight"),
  rows=egg1_lookup_start:egg1_lookup_end,
  cols=1:ncol(egg1_grid), gridExpand=TRUE)

egg2_start_row <- egg1_lookup_end + 3
writeData(wb, "EggLookup", "EGG 2-CYCLE -- eggs_2cycle_total model",
  startRow=egg2_start_row, colNames=FALSE)
addStyle(wb, "EggLookup", s_lbl, rows=egg2_start_row, cols=1)
writeData(wb, "EggLookup", egg2_grid, startRow=egg2_start_row+1, colNames=TRUE)
addStyle(wb, "EggLookup", s_hdr,
  rows=egg2_start_row+1, cols=1:ncol(egg2_grid), gridExpand=TRUE)
egg2_lookup_start <- egg2_start_row + 2
egg2_lookup_end   <- egg2_start_row + 1 + nrow(egg2_grid)
addStyle(wb, "EggLookup",
  createStyle(border="TopBottomLeftRight"),
  rows=egg2_lookup_start:egg2_lookup_end,
  cols=1:ncol(egg2_grid), gridExpand=TRUE)

############################################################
# TAB: Pricing
# ==========================================================
# All cross-sheet references updated for new row positions:
#   LiveBirth: p_cal = B30 (was B27 in v5.0)
#   Euploid:   p_cal = B27 (was B24 in v5.0)
#   Egg1:      mu    = B23 (was B20)
#   Egg2:      mu    = B25 (was B22)
# All effective inputs (age/AMH/AFC) have MAX/MIN applied
# inside the INDEX/MATCH formula -- no raw values used
############################################################

addWorksheet(wb, "Pricing")
setColWidths(wb, "Pricing", cols=1:9,
  widths=c(3,28,16,16,16,16,16,16,3))

writeData(wb, "Pricing",
  "FERTILITY PRICING CALCULATOR -- INDIVIDUALIZED PREMIUMS",
  startRow=1, colNames=FALSE)
addStyle(wb, "Pricing",
  createStyle(textDecoration="bold", fontSize=14), rows=1, cols=1)
writeData(wb, "Pricing",
  paste0("Enter patient values and select location in YELLOW cells. ",
         "Age floor=",age_floor,", AMH cap=",amh_cap,
         ", AFC cap=",afc_cap," applied automatically."),
  startRow=2, colNames=FALSE)
addStyle(wb, "Pricing", s_note, rows=2, cols=1)

# -- Location selector --------------------------------------
# Write the full cost table to Costs tab (see below) and
# reference it here via INDEX/MATCH on the location selector.
# !!NOTE!! change B4 to switch all costs instantly.

writeData(wb, "Pricing", "LOCATION SELECTOR", startRow=4, colNames=FALSE)
addStyle(wb, "Pricing", s_lbl, rows=4, cols=1)
writeData(wb, "Pricing",
  data.frame(
    Input = "Location (enter: NY, CA, or PDX)",
    Value = selected_location
  ),
  startRow=5, colNames=TRUE)
addStyle(wb, "Pricing", s_hdr, rows=5, cols=1:2, gridExpand=TRUE)
addStyle(wb, "Pricing", s_inp, rows=6, cols=2, gridExpand=TRUE)
# !!NOTE!! (Excel user): B6 = location code -- type NY, CA, or PDX
# All cost cells below update automatically via VLOOKUP on Costs tab

# Live cost lookups from Costs tab
# Costs tab columns: A=location, B=euploid, C=live_birth, D=egg1, E=egg2
writeData(wb, "Pricing", "COSTS FOR SELECTED LOCATION (auto)", startRow=8, colNames=FALSE)
addStyle(wb, "Pricing", s_lbl, rows=8, cols=1)
writeData(wb, "Pricing",
  data.frame(
    Cost_Item = c("Euploid cost/cycle",
                  "Live birth cost/cycle",
                  "Egg 1-cycle cost",
                  "Egg 2-cycle cost (= 0.9 x egg1)"),
    Value = NA
  ),
  startRow=9, colNames=TRUE)
addStyle(wb, "Pricing", s_hdr, rows=9, cols=1:2, gridExpand=TRUE)
# These are formula cells that look up from the Costs tab
writeFormula(wb, "Pricing",
  x="=VLOOKUP(B6,Costs!$A:$E,2,FALSE)", startRow=10, startCol=2)
writeFormula(wb, "Pricing",
  x="=VLOOKUP(B6,Costs!$A:$E,3,FALSE)", startRow=11, startCol=2)
writeFormula(wb, "Pricing",
  x="=VLOOKUP(B6,Costs!$A:$E,4,FALSE)", startRow=12, startCol=2)
writeFormula(wb, "Pricing",
  x="=VLOOKUP(B6,Costs!$A:$E,5,FALSE)", startRow=13, startCol=2)
for (r in 10:13) {
  addStyle(wb, "Pricing",
    createStyle(fgFill="#F2F2F2", numFmt="$#,##0",
                border="TopBottomLeftRight", halign="right"),
    rows=r, cols=2)
}
# CELL MAP: B10=cost_euploid  B11=cost_livebirth
#           B12=cost_egg1     B13=cost_egg2

# -- Patient inputs --------------------------------------
writeData(wb, "Pricing", "PATIENT INPUTS", startRow=15, colNames=FALSE)
addStyle(wb, "Pricing", s_lbl, rows=15, cols=1)
writeData(wb, "Pricing",
  data.frame(
    Input=c(paste0("Age (floor=",age_floor," applied)"),
            paste0("AMH ng/mL (cap=",amh_cap," applied)"),
            paste0("AFC (cap=",afc_cap," applied)"),
            "Eggs cycle 1 -- actual (egg2 only)"),
    Value=c(NA,NA,NA,NA)
  ),
  startRow=16, colNames=TRUE)
addStyle(wb, "Pricing", s_hdr, rows=16, cols=1:2, gridExpand=TRUE)
addStyle(wb, "Pricing", s_inp, rows=17:20, cols=2, gridExpand=TRUE)
# !!NOTE!!: B17=Age  B18=AMH  B19=AFC  B20=eggs_cycle1

# Effective inputs
writeData(wb, "Pricing", "EFFECTIVE INPUTS (auto-applied)", startRow=22, colNames=FALSE)
addStyle(wb, "Pricing", s_lbl, rows=22, cols=1)
writeData(wb, "Pricing",
  data.frame(Input=c("Eff. age","Eff. AMH","Eff. AFC"), Value=NA),
  startRow=23, colNames=TRUE)
addStyle(wb, "Pricing", s_hdr, rows=23, cols=1:2, gridExpand=TRUE)
writeFormula(wb, "Pricing",
  x=paste0("=MAX(B17,",age_floor,")"), startRow=24, startCol=2)
writeFormula(wb, "Pricing",
  x=paste0("=MIN(B18,",amh_cap,")"),  startRow=25, startCol=2)
writeFormula(wb, "Pricing",
  x=paste0("=MIN(B19,",afc_cap,")"),  startRow=26, startCol=2)
for (r in 24:26)
  addStyle(wb, "Pricing", s_frm2, rows=r, cols=2)
# CELL MAP: B24=eff_age  B25=eff_amh  B26=eff_afc

# Model outputs
writeData(wb, "Pricing",
  "RECALIBRATED MODEL OUTPUTS (update when patient inputs change)",
  startRow=28, colNames=FALSE)
addStyle(wb, "Pricing", s_lbl, rows=28, cols=1)
writeData(wb, "Pricing",
  data.frame(Output=c("P(live birth) -- calibrated",
                      "P(euploid) -- calibrated",
                      "Expected eggs -- 1 cycle (mu)",
                      "Expected eggs -- 2 cycles (mu)"),
             Value=NA),
  startRow=29, colNames=TRUE)
addStyle(wb, "Pricing", s_hdr, rows=29, cols=1:2, gridExpand=TRUE)
writeFormula(wb, "Pricing", x="=LiveBirth!B30", startRow=30, startCol=2)
writeFormula(wb, "Pricing", x="=Euploid!B27",   startRow=31, startCol=2)
writeFormula(wb, "Pricing", x="=Egg1!B23",       startRow=32, startCol=2)
writeFormula(wb, "Pricing", x="=Egg2!B25",       startRow=33, startCol=2)
addStyle(wb, "Pricing", s_frm4, rows=30:33, cols=2, gridExpand=TRUE)

# Egg threshold lookups
writeData(wb, "Pricing",
  "EGG THRESHOLDS -- Monte Carlo lookup (nearest profile by effective inputs)",
  startRow=35, colNames=FALSE)
addStyle(wb, "Pricing", s_lbl, rows=35, cols=1)
writeData(wb, "Pricing",
  data.frame(Threshold=c("Egg1 90% guarantee","Egg2 90% guarantee"),
             Min_Eggs=NA),
  startRow=36, colNames=TRUE)
addStyle(wb, "Pricing", s_hdr, rows=36, cols=1:2, gridExpand=TRUE)

egg1_ra <- paste0("EggLookup!A",egg1_lookup_start,":A",egg1_lookup_end)
egg1_rb <- paste0("EggLookup!B",egg1_lookup_start,":B",egg1_lookup_end)
egg1_rc <- paste0("EggLookup!C",egg1_lookup_start,":C",egg1_lookup_end)
egg1_re <- paste0("EggLookup!E",egg1_lookup_start,":E",egg1_lookup_end)

writeFormula(wb, "Pricing",
  x=paste0("=INDEX(",egg1_re,",MATCH(MIN(",
    "ABS(",egg1_ra,"-B24)+ABS(",egg1_rb,"-B25)+ABS(",egg1_rc,"-B26)",
    "),ABS(",egg1_ra,"-B24)+ABS(",egg1_rb,"-B25)+ABS(",egg1_rc,"-B26),0))"),
  startRow=37, startCol=2)

egg2_ra <- paste0("EggLookup!A",egg2_lookup_start,":A",egg2_lookup_end)
egg2_rb <- paste0("EggLookup!B",egg2_lookup_start,":B",egg2_lookup_end)
egg2_rc <- paste0("EggLookup!C",egg2_lookup_start,":C",egg2_lookup_end)
egg2_rd <- paste0("EggLookup!D",egg2_lookup_start,":D",egg2_lookup_end)
egg2_rf <- paste0("EggLookup!F",egg2_lookup_start,":F",egg2_lookup_end)

writeFormula(wb, "Pricing",
  x=paste0("=INDEX(",egg2_rf,",MATCH(MIN(",
    "ABS(",egg2_ra,"-B24)+ABS(",egg2_rb,"-B25)+",
    "ABS(",egg2_rc,"-B26)+ABS(",egg2_rd,"-B20)",
    "),ABS(",egg2_ra,"-B24)+ABS(",egg2_rb,"-B25)+",
    "ABS(",egg2_rc,"-B26)+ABS(",egg2_rd,"-B20),0))"),
  startRow=38, startCol=2)

addStyle(wb, "Pricing", s_frm, rows=37:38, cols=2, gridExpand=TRUE)

# -- Premium blocks --------------------------------------
# IMPORTANT DESIGN NOTE:
# Because costs now come from a live Excel VLOOKUP (B10:B13),
# the pool_pure_r component (pool average) must still be
# baked in from R at script write time using the selected_location
# costs. The individual component uses live Excel cell references
# so it updates when someone changes location.
# This means pool statistics reflect selected_location -- if
# someone toggles to a different location in Excel, the individual
# premium updates but the pool blend component stays at selected_location.
# For full location-aware pooling, re-run the R script with
# the target selected_location and regenerate the Excel.

pr_col_hdrs <- c("","Refund Tier","1 Cycle","2 Cycles","3 Cycles",
                 "Cap (1cy)","Cap (2cy)","Cap (3cy)")

write_pricing_block <- function(wb, label, start_row,
                                p_cal_ref, pool_fails, pool_exp_cys,
                                r_costs,          # R-side costs for pool calc
                                excel_cost_cell,  # Excel cell ref for live cost
                                is_egg=FALSE, egg_max_cycles=1) {

  mergeCells(wb, "Pricing", cols=1:8, rows=start_row)
  writeData(wb, "Pricing", label, startRow=start_row, startCol=1, colNames=FALSE)
  addStyle(wb, "Pricing",
    createStyle(textDecoration="bold", fgFill="#1F3864", fontColour="#FFFFFF",
                border="TopBottomLeftRight"),
    rows=start_row, cols=1:8, gridExpand=TRUE)

  writeData(wb, "Pricing",
    data.frame(t(pr_col_hdrs)),
    startRow=start_row+1, startCol=1, colNames=FALSE)
  addStyle(wb, "Pricing", s_hdr, rows=start_row+1, cols=1:8, gridExpand=TRUE)

  tier_names <- names(refund_tiers)
  tier_pcts  <- unname(refund_tiers)

  for (ti in seq_along(tier_names)) {
    r <- start_row + 1 + ti
    writeData(wb, "Pricing", tier_names[ti],
              startRow=r, startCol=2, colNames=FALSE)

    for (cy in 1:3) {
      col_idx <- 2 + cy
      tier_p  <- tier_pcts[ti]

      # R-side cost (selected_location) used for pool calc
      cost_cy_r  <- r_costs[cy]
      total_r    <- cy * cost_cy_r
      refund_r   <- if (tier_p == 0) variable_cost else tier_p * total_r
      pool_pure_r <- pool_fails[cy] * refund_r + pool_exp_cys[cy] * cost_cy_r

      # Excel-side: individual component uses live cost from B10/B11/B12/B13
      # total_cost in Excel = cy * excel_cost_cell
      # cap = cy * excel_cost_cell * 0.50 * load_factor
      excel_total <- paste0(cy,"*",excel_cost_cell)
      refund_excel <- if (tier_p == 0) {
        as.character(variable_cost)
      } else {
        paste0(round(tier_p, 2),"*",excel_total)
      }
      cap_excel_loaded <- paste0(
        excel_total,"*",premium_cap_pct,
        "*(1+",risk_margin,")*(1+",admin_load,")"
      )

      if (is_egg) {
        indiv_f <- paste0(
          "(0.10*",refund_excel,"+",egg_max_cycles,"*",excel_cost_cell,")"
        )
      } else {
        fail_f <- paste0("(1-",p_cal_ref,")^",cy)
        exp_cy_f <- switch(as.character(cy),
          "1" = "1",
          "2" = paste0("(",p_cal_ref,"+(1-",p_cal_ref,")*",p_cal_ref,
                        "*2+(1-",p_cal_ref,")^2*2)"),
          "3" = paste0("(",p_cal_ref,"+(1-",p_cal_ref,")*",p_cal_ref,
                        "*2+(1-",p_cal_ref,")^2*",p_cal_ref,
                        "*3+(1-",p_cal_ref,")^3*3)")
        )
        indiv_f <- paste0(
          "(",fail_f,"*",refund_excel,"+",exp_cy_f,"*",excel_cost_cell,")"
        )
      }

      prem_f <- paste0(
        "=MIN(",
          "((1-",pool_weight,")*",indiv_f,
          "+",pool_weight,"*",round(pool_pure_r,2),")",
          "*(1+",risk_margin,")*(1+",admin_load,"),",
          cap_excel_loaded,
        ")"
      )

      writeFormula(wb, "Pricing", x=prem_f, startRow=r, startCol=col_idx)
      addStyle(wb, "Pricing", s_prem, rows=r, cols=col_idx)

      # Cap column (also live from Excel cost)
      cap_f <- paste0("=",excel_total,"*",premium_cap_pct,
                      "*(1+",risk_margin,")*(1+",admin_load,")")
      writeFormula(wb, "Pricing", x=cap_f, startRow=r, startCol=5+cy)
      addStyle(wb, "Pricing", s_cap, rows=r, cols=5+cy)
    }
  }
  return(start_row + 1 + length(tier_names) + 2)
}

# Product A: Live Birth
# Excel cost cell: B11 (live birth cost from VLOOKUP)
# R costs: cost_livebirth repeated for 1/2/3 cycles
next_row <- write_pricing_block(wb, "A  |  LIVE BIRTH GUARANTEE",
  40, "LiveBirth!B30",
  pool_fail_lb, pool_exp_cy_lb,
  r_costs        = rep(cost_livebirth, 3),
  excel_cost_cell = "B11")

# Product B: Euploid
# Excel cost cell: B10
next_row <- write_pricing_block(wb, "B  |  EUPLOID EMBRYO GUARANTEE",
  next_row, "Euploid!B27",
  pool_fail_eu, pool_exp_cy_eu,
  r_costs        = rep(cost_euploid, 3),
  excel_cost_cell = "B10")

# Product C: Egg 1-cycle
# Excel cost cell: B12
next_row <- write_pricing_block(wb,
  "C  |  EGG RETRIEVAL GUARANTEE -- 1 cycle, 90% threshold",
  next_row, NULL,
  c(0.10,0.10,0.10), c(1,1,1),
  r_costs        = rep(cost_egg1, 3),
  excel_cost_cell = "B12",
  is_egg=TRUE, egg_max_cycles=1)

# Product D: Egg 2-cycle
# Excel cost cell: B13 (egg2 = 0.9 x egg1, auto-computed)
next_row <- write_pricing_block(wb,
  "D  |  EGG RETRIEVAL GUARANTEE -- 2 cycles, 90% threshold",
  next_row, NULL,
  c(0.10,0.10,0.10), c(2,2,2),
  r_costs        = rep(cost_egg2, 3),
  excel_cost_cell = "B13",
  is_egg=TRUE, egg_max_cycles=2)

writeData(wb, "Pricing",
  paste0(
    "Location toggle: type NY, CA, or PDX in cell B6 to switch all costs. ",
    "Pool blend uses ", selected_location, " costs (re-run R to change pool basis). ",
    "Free cycle tier uses $",format(variable_cost,big.mark=",")," variable cost basis. ",
    "Green=premium, Yellow=50% cap."
  ),
  startRow=next_row+1, colNames=FALSE)
addStyle(wb, "Pricing", s_note, rows=next_row+1, cols=1)

############################################################
# TAB: Costs
# ==========================================================
# This tab is the VLOOKUP source for the Pricing tab.
# If you type NY, CA, or PDX in Pricing!B6 and all
# cost cells update via VLOOKUP on this table.
# To update costs: edit the location_costs data frame in
# Section 0 [5/9] in R and re-run the script.
############################################################

addWorksheet(wb, "Costs")
setColWidths(wb, "Costs", cols=1:6,
  widths=c(3,14,18,18,18,20))

writeData(wb, "Costs",
  "LOCATION COST TABLE -- source for Pricing tab VLOOKUP",
  startRow=1, colNames=FALSE)
addStyle(wb, "Costs",
  createStyle(textDecoration="bold", fontSize=12), rows=1, cols=1)
writeData(wb, "Costs",
  "Do not edit this tab manually. Update location_costs in R Section 0 [5/9] and re-run script.",
  startRow=2, colNames=FALSE)
addStyle(wb, "Costs", s_note, rows=2, cols=1)

cost_export <- location_costs %>%
  dplyr::select(location, euploid, live_birth, egg1, egg2) %>%
  dplyr::rename(
    Location        = location,
    Euploid         = euploid,
    Live_Birth      = live_birth,
    Egg_1_Cycle     = egg1,
    Egg_2_Cycle_90pct = egg2
  )

writeData(wb, "Costs", cost_export, startRow=4, colNames=TRUE)
addStyle(wb, "Costs", s_hdr, rows=4, cols=1:5, gridExpand=TRUE)
addStyle(wb, "Costs", s_assm, rows=5:7, cols=1:5, gridExpand=TRUE)

# Format cost columns as currency
for (col in 2:5)
  addStyle(wb, "Costs",
    createStyle(fgFill="#FCE4D6", fontColour="#833C00",
                border="TopBottomLeftRight", numFmt="$#,##0"),
    rows=5:7, cols=col, gridExpand=TRUE)

# Add notes column
writeData(wb, "Costs", "Notes", startRow=4, startCol=6, colNames=FALSE)
addStyle(wb, "Costs", s_hdr, rows=4, cols=6, gridExpand=TRUE)
writeData(wb, "Costs",
  data.frame(Notes=c(
    "New York clinics",
    "California clinics",
    "Portland OR clinics -- Egg 2-cycle = 0.9 x Egg 1-cycle for all locations"
  )),
  startRow=5, startCol=6, colNames=FALSE)
addStyle(wb, "Costs", s_note, rows=5:7, cols=6, gridExpand=TRUE)

writeData(wb, "Costs",
  paste0("Selected location for R pool statistics: ", selected_location,
         " (pool blend component uses this location's costs).",
         " To change, update selected_location in R and re-run."),
  startRow=9, colNames=FALSE)
addStyle(wb, "Costs", s_note, rows=9, cols=1)

############################################################
# TAB: PoolStats
############################################################

addWorksheet(wb, "PoolStats")
setColWidths(wb, "PoolStats", cols=1:3, widths=c(3,46,20))

writeData(wb, "PoolStats",
  "POOL-LEVEL STATISTICS -- from training data",
  startRow=1, colNames=FALSE)
addStyle(wb, "PoolStats",
  createStyle(textDecoration="bold", fontSize=12), rows=1, cols=1)

pool_df <- data.frame(
  Metric=c(
    "Age floor","AMH cap","AFC cap",
    "N live birth (training, euploid-anchored)",
    "N euploid (training)",
    "Pool mean P(LB/cycle) calibrated",
    "Pool mean P(EU/cycle) calibrated",
    "LB cal intercept (validation)",
    "LB cal slope (validation) -- MONITOR",
    "EU cal intercept (validation)",
    "EU cal slope (validation)",
    "Pool P(fail LB 1cy)","Pool P(fail LB 2cy)","Pool P(fail LB 3cy)",
    "Pool P(fail EU 1cy)","Pool P(fail EU 2cy)","Pool P(fail EU 3cy)",
    "Pool E[cycles LB max1]","Pool E[cycles LB max2]","Pool E[cycles LB max3]",
    "Pool E[cycles EU max1]","Pool E[cycles EU max2]","Pool E[cycles EU max3]",
    "Egg1 MC profiles","Egg2 MC profiles","MC n_sim","Variable cost basis"
  ),
  Value=round(c(
    age_floor, amh_cap, afc_cap,
    pool_n_lb, pool_n_eu,
    pool_p_lb, pool_p_eu,
    lb_cal_intercept, lb_cal_slope,
    eu_cal_intercept, eu_cal_slope,
    pool_fail_lb, pool_fail_eu,
    pool_exp_cy_lb, pool_exp_cy_eu,
    nrow(egg1_grid), nrow(egg2_grid), n_sim,
    variable_cost
  ), 6),
  stringsAsFactors=FALSE
)

writeData(wb, "PoolStats", pool_df, startRow=3, colNames=TRUE)
addStyle(wb, "PoolStats", s_hdr, rows=3, cols=1:2, gridExpand=TRUE)
addStyle(wb, "PoolStats", s_assm,
  rows=4:(3+nrow(pool_df)), cols=2, gridExpand=TRUE)

writeData(wb, "PoolStats",
  paste0(
    "NOTES:\n",
    "1. LB calibration slope of ~0.73 indicates moderate probability compression.\n",
    "   Premiums for extreme-risk patients will be less differentiated than true biology.\n",
    "   Monitor actual vs. expected claims on first book of business.\n",
    "2. Live birth model is anchored on euploid success patients only.\n",
    "3. AMH and AFC winsorized before fitting -- outliers do not distort coefficients.\n",
    "4. Free cycle tier priced on variable cost ($",format(variable_cost,big.mark=","),
    ") not full cycle cost ($",format(cost_per_cycle_1,big.mark=","),").\n",
    "5. Re-run this script whenever: costs change, new patient data available,\n",
    "   or calibration is refreshed on new validation data."
  ),
  startRow=3+nrow(pool_df)+2, colNames=FALSE)
addStyle(wb, "PoolStats", s_note, rows=3+nrow(pool_df)+2, cols=1)

############################################################
# 9. SAVE
############################################################

message("[ 7/7 ] Saving workbook...")
saveWorkbook(wb, output_excel, overwrite=TRUE)

message(paste0(
"\n+======================================================+\n",
"|  DONE  DONE                                            |\n",
"|  Excel : ", output_excel,
paste0(rep(" ", max(0,44-nchar(output_excel))),"|\n",collapse=""),
"|  CSVs  : ", output_dir_csv,
paste0(rep(" ", max(0,44-nchar(output_dir_csv))),"|\n",collapse=""),
"+======================================================+\n",
"\n-- !!NOTE!! CHECKLIST ------------------------------------\n",
"  [1/9] Data frames loaded from fertility_data_prep.R\n",
"  [2/9] age_floor = ", age_floor, "\n",
"  [3/9] AMH cap = ", amh_cap, " | AFC cap = ", afc_cap, "\n",
"  [4/9] Calibration params filled (not NA):\n",
"        LB intercept=",lb_cal_intercept,"  slope=",lb_cal_slope,"\n",
"        EU intercept=",eu_cal_intercept,"  slope=",eu_cal_slope,"\n",
"  [5/9] Cost/cycle = $",format(cost_per_cycle_1,big.mark=","),
         " | Variable cost = $",format(variable_cost,big.mark=","),"\n",
"  [6/9] Margins: risk=",risk_margin," admin=",admin_load,
         " combined=",round((1+risk_margin)*(1+admin_load)-1,3)*100,"%\n",
"  [7/9] Pool weight = ", pool_weight, "\n",
"  [8/9] n_sim = ", n_sim, " (set to 10000 for production)\n",
"  [9/9] Output paths correct\n",
"\n-- TEST ON OPEN -----------------------------------------\n",
"  Enter age=36, AMH=2.0, AFC=12 on Pricing tab.\n",
"  Premiums should be positive in green cells.\n",
"  Cap cells (yellow) should be higher than premiums.\n",
"  Effective inputs should show: age=36, AMH=2.0, AFC=12\n",
"  (no clipping at these values -- they are within range)\n",
"\n-- OUTPUTS WRITTEN --------------------------------------\n",
"  outputs/live_birth_model_coefficients.csv\n",
"  outputs/euploid_model_coefficients.csv\n",
"  outputs/egg1_model_coefficients.csv\n",
"  outputs/egg2_model_coefficients.csv\n",
"  outputs/livebirth_multicycle_probabilities.csv\n",
"  outputs/euploid_embryo_multicycle_probabilities.csv\n",
"  outputs/egg1_mc_threshold_lookup.csv\n",
"  outputs/egg2_mc_threshold_lookup.csv\n"
))