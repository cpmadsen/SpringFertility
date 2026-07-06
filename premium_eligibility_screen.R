############################################################
# PREMIUM ELIGIBILITY SCREEN
# Run AFTER fertility pricing master script
############################################################

library(dplyr)
library(readr)
library(openxlsx)

# Choose which product population to screen.
# Recommended default: "euploid"
#   - "euploid" uses df_euploid, which is the broader patient population.
#   - "live_birth" uses df_livebirth, which is euploid-conditioned only.
product_to_screen <- "euploid"

# Product design to test
n_cycles <- 2
refund_tier_name <- "Full Refund"
tier_pct <- refund_tiers[[refund_tier_name]]

# Eligibility threshold:
# ineligible if premium exceeds 50% of total treatment cycle cost
eligibility_cap_pct <- 0.50

# Output paths
output_dir <- "outputs"
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

patient_level_output <- file.path(output_dir, "premium_eligibility_patient_flags.csv")
summary_csv_output   <- file.path(output_dir, "premium_eligibility_summary.csv")
summary_xlsx_output  <- file.path(output_dir, "premium_eligibility_one_page_summary.xlsx")


############################################################
# CHECK REQUIRED OBJECTS EXIST
############################################################

required_objects <- c(
  "df_euploid", "df_livebirth",
  "pool_fail_eu", "pool_fail_lb",
  "cost_euploid", "cost_livebirth",
  "compute_premium",
  "refund_tiers",
  "selected_location",
  "risk_margin", "admin_load", "pool_weight"
)

missing_objects <- required_objects[!sapply(required_objects, exists)]

if (length(missing_objects) > 0) {
  stop(
    paste0(
      "Missing objects from pricing master script: ",
      paste(missing_objects, collapse = ", "),
      "\nRun/source the pricing master script first, then run this script."
    )
  )
}


############################################################
# SELECT POPULATION + PRODUCT ASSUMPTIONS
############################################################

if (product_to_screen == "euploid") {
  
  pop_df <- df_euploid %>%
    mutate(
      product = "Euploid Embryo Guarantee",
      success_prob = p_cal,
      fail_prob_i = (1 - p_cal)^n_cycles
    )
  
  cost_cy <- cost_euploid
  fail_prob_pool <- pool_fail_eu[n_cycles]
  
} else if (product_to_screen == "live_birth") {
  
  pop_df <- df_livebirth %>%
    mutate(
      product = "Live Birth Guarantee",
      success_prob = p_cal,
      fail_prob_i = (1 - p_cal)^n_cycles
    )
  
  cost_cy <- cost_livebirth
  fail_prob_pool <- pool_fail_lb[n_cycles]
  
} else {
  stop("product_to_screen must be either 'euploid' or 'live_birth'.")
}


############################################################
# PATIENT-LEVEL PREMIUM CALCULATION
############################################################

total_cycle_cost <- n_cycles * cost_cy
premium_threshold <- eligibility_cap_pct * total_cycle_cost

screened_df <- pop_df %>%
  mutate(
    total_cycle_cost = total_cycle_cost,
    premium_threshold_50pct = premium_threshold,
    
    premium = mapply(
      compute_premium,
      fail_prob_i = fail_prob_i,
      MoreArgs = list(
        fail_prob_p = fail_prob_pool,
        cost_cy = cost_cy,
        n_cycles = n_cycles,
        tier_pct = tier_pct
      )
    ),
    
    premium_pct_of_cycle_cost = premium / total_cycle_cost,
    ineligible_flag = premium > premium_threshold,
    
    age_band = case_when(
      age_model < 30 ~ "<30",
      age_model >= 30 & age_model <= 34 ~ "30-34",
      age_model >= 35 & age_model <= 37 ~ "35-37",
      age_model >= 38 & age_model <= 40 ~ "38-40",
      age_model >= 41 ~ "41+",
      TRUE ~ NA_character_
    )
  )


############################################################
# CREATE AMH + AFC QUARTILES
############################################################

screened_df <- screened_df %>%
  mutate(
    amh_quartile = ntile(amh, 4),
    afc_quartile = ntile(afc, 4),
    
    amh_quartile = paste0("AMH Q", amh_quartile),
    afc_quartile = paste0("AFC Q", afc_quartile)
  )


############################################################
# OVERALL IMPACT
############################################################

total_enrollment <- nrow(screened_df)
total_ineligible <- sum(screened_df$ineligible_flag, na.rm = TRUE)
total_eligible   <- total_enrollment - total_ineligible

overall_summary <- tibble(
  summary_section = "Overall",
  segment = "Total Population",
  enrolled_n = total_enrollment,
  ineligible_n = total_ineligible,
  eligible_pool_n = total_eligible,
  pct_total_enrollment_affected = total_ineligible / total_enrollment,
  implied_eligible_pool_pct = total_eligible / total_enrollment,
  avg_premium = mean(screened_df$premium, na.rm = TRUE),
  avg_premium_pct_of_cycle_cost = mean(screened_df$premium_pct_of_cycle_cost, na.rm = TRUE)
)


############################################################
# ONE-PAGE SUMMARY BY AGE BAND, AMH QUARTILE, AFC QUARTILE
############################################################

summarize_screenout <- function(data, group_var, section_name) {
  
  data %>%
    group_by({{ group_var }}) %>%
    summarise(
      enrolled_n = n(),
      ineligible_n = sum(ineligible_flag, na.rm = TRUE),
      eligible_pool_n = enrolled_n - ineligible_n,
      pct_total_enrollment_affected = ineligible_n / total_enrollment,
      implied_eligible_pool_pct = eligible_pool_n / total_enrollment,
      avg_premium = mean(premium, na.rm = TRUE),
      avg_premium_pct_of_cycle_cost = mean(premium_pct_of_cycle_cost, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    rename(segment = {{ group_var }}) %>%
    mutate(summary_section = section_name) %>%
    select(
      summary_section,
      segment,
      enrolled_n,
      ineligible_n,
      eligible_pool_n,
      pct_total_enrollment_affected,
      implied_eligible_pool_pct,
      avg_premium,
      avg_premium_pct_of_cycle_cost
    )
}

age_summary <- summarize_screenout(screened_df, age_band, "Age Band")
amh_summary <- summarize_screenout(screened_df, amh_quartile, "AMH Quartile")
afc_summary <- summarize_screenout(screened_df, afc_quartile, "AFC Quartile")

one_page_summary <- bind_rows(
  overall_summary,
  age_summary,
  amh_summary,
  afc_summary
) %>%
  mutate(
    pct_total_enrollment_affected = round(100 * pct_total_enrollment_affected, 1),
    implied_eligible_pool_pct = round(100 * implied_eligible_pool_pct, 1),
    avg_premium = round(avg_premium, 0),
    avg_premium_pct_of_cycle_cost = round(100 * avg_premium_pct_of_cycle_cost, 1)
  )


############################################################
# OPTIONAL: MORE DETAILED INTERSECTION TABLE
# Age band x AMH quartile x AFC quartile
############################################################

intersection_summary <- screened_df %>%
  group_by(age_band, amh_quartile, afc_quartile) %>%
  summarise(
    enrolled_n = n(),
    ineligible_n = sum(ineligible_flag, na.rm = TRUE),
    eligible_pool_n = enrolled_n - ineligible_n,
    pct_total_enrollment_affected = ineligible_n / total_enrollment,
    implied_eligible_pool_pct = eligible_pool_n / total_enrollment,
    avg_premium = mean(premium, na.rm = TRUE),
    avg_premium_pct_of_cycle_cost = mean(premium_pct_of_cycle_cost, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    pct_total_enrollment_affected = round(100 * pct_total_enrollment_affected, 1),
    implied_eligible_pool_pct = round(100 * implied_eligible_pool_pct, 1),
    avg_premium = round(avg_premium, 0),
    avg_premium_pct_of_cycle_cost = round(100 * avg_premium_pct_of_cycle_cost, 1)
  ) %>%
  arrange(age_band, amh_quartile, afc_quartile)


############################################################
# EXPORT OUTPUTS
############################################################

write_csv(screened_df, patient_level_output)
write_csv(one_page_summary, summary_csv_output)

wb <- createWorkbook()

addWorksheet(wb, "One Page Summary")
writeData(wb, "One Page Summary", one_page_summary)

addWorksheet(wb, "Detailed Cross Tab")
writeData(wb, "Detailed Cross Tab", intersection_summary)

addWorksheet(wb, "Patient Flags")
writeData(wb, "Patient Flags", screened_df)

# Formatting
header_style <- createStyle(
  textDecoration = "bold",
  fgFill = "#1F3864",
  fontColour = "#FFFFFF",
  halign = "center",
  border = "TopBottomLeftRight"
)

pct_style <- createStyle(numFmt = "0.0")
currency_style <- createStyle(numFmt = "$#,##0")

for (sheet in names(wb)) {
  addStyle(wb, sheet, header_style, rows = 1, cols = 1:20, gridExpand = TRUE)
  freezePane(wb, sheet, firstRow = TRUE)
  setColWidths(wb, sheet, cols = 1:20, widths = "auto")
}

saveWorkbook(wb, summary_xlsx_output, overwrite = TRUE)


############################################################
# PRINT CONSOLE SUMMARY
############################################################

message("\nPremium eligibility screen complete.")
message("Product screened: ", unique(screened_df$product))
message("Location: ", selected_location)
message("Cycles: ", n_cycles)
message("Refund tier: ", refund_tier_name)
message("Total enrollment: ", total_enrollment)
message("Ineligible records: ", total_ineligible)
message("Eligible pool size after screen: ", total_eligible)
message(
  "Percent of total enrollment affected: ",
  round(100 * total_ineligible / total_enrollment, 1), "%"
)

message("\nFiles written:")
message("  Patient-level flags: ", patient_level_output)
message("  One-page summary CSV: ", summary_csv_output)
message("  Excel summary: ", summary_xlsx_output)