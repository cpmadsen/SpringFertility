# ============================================================
# Egg Retrieval Models - June 15, 2026
# LOW AMH / LOW AFC PATIENT PROFILE CHECK
# Cycle 1 vs Cycle 2 comparison
# Run on the cleaned egg training data (post exclusion criteria)
# ============================================================

low_amh_cutoff <- 0.5
low_afc_cutoff <- 3

# ── Cycle 1 low reserve patients ────────────────────────────
cat("=== CYCLE 1 — LOW RESERVE PATIENTS ===\n")

df_no_na = readr::read_csv("data/prepped_data/df_egg1.csv")

out1 = df_no_na |>
  filter(amh <= low_amh_cutoff | afc <= low_afc_cutoff) |>
  summarize(
    n               = n(),
    mean_age        = mean(age_model, na.rm=TRUE),
    median_age      = median(age_model, na.rm=TRUE),
    min_age         = min(age_model, na.rm=TRUE),
    max_age         = max(age_model, na.rm=TRUE),
    mean_amh        = mean(amh, na.rm=TRUE),
    median_amh      = median(amh, na.rm=TRUE),
    mean_afc        = mean(afc, na.rm=TRUE),
    median_afc      = median(afc, na.rm=TRUE),
    mean_eggs       = mean(num_eggs_collected, na.rm=TRUE),
    median_eggs     = median(num_eggs_collected, na.rm=TRUE)
  ) |> 
  print()

# ── Cycle 2 low reserve patients ────────────────────────────
cat("\n=== CYCLE 2 — LOW RESERVE PATIENTS ===\n")

df_egg2 = readr::read_csv("data/prepped_data/df_egg2.csv")

out2 = df_egg2 |>
  filter(amh <= low_amh_cutoff | afc <= low_afc_cutoff) |>
  summarize(
    n               = n(),
    mean_age        = mean(age_model, na.rm=TRUE),
    median_age      = median(age_model, na.rm=TRUE),
    min_age         = min(age_model, na.rm=TRUE),
    max_age         = max(age_model, na.rm=TRUE),
    mean_amh        = mean(amh, na.rm=TRUE),
    median_amh      = median(amh, na.rm=TRUE),
    mean_afc        = mean(afc, na.rm=TRUE),
    median_afc      = median(afc, na.rm=TRUE),
    mean_eggs_cy1   = mean(eggs_cycle1, na.rm=TRUE),
    mean_eggs_cy2   = mean(eggs_2cycle_total, na.rm=TRUE),
    median_eggs_cy2 = median(eggs_2cycle_total, na.rm=TRUE)
  ) |>
  print()

# ── Overall N comparison ─────────────────────────────────────
cat("\n=== OVERALL SAMPLE SIZE: CYCLE 1 vs CYCLE 2 ===\n")
cat("Cycle 1 total N:", nrow(df_no_na), "\n")
cat("Cycle 2 total N:", nrow(df_egg2), "\n")
out3 = cat("Cycle 1 low reserve N:",
    nrow(filter(df_no_na, amh <= low_amh_cutoff | afc <= low_afc_cutoff)), "\n") |> 
  capture.output()
out4 = cat("Cycle 2 low reserve N:",
    nrow(filter(df_egg2, amh <= low_amh_cutoff | afc <= low_afc_cutoff)), "\n") |> 
  capture.output()

out1
out2
out3
out4
