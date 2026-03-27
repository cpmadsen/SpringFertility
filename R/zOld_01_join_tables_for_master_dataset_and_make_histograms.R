# ==========================================
# Author: Chris Madsen, Brandon Kwon
# Script purpose: create 'master dataset' for running models based on feedback from 
# Adam Walter re: how to join tables.
# 
# Steps for joins:
# 1. Treatment table cleaned and structured by Adam.
# 2. Structured egg table left_joined onto treatment table using pat_id and tx_id (one-to-many join as one pat_id can have many egg_ids, row count increases)
# 3. Raw embryo table left_joined onto result of #2 table (one-to-one join as both seem to be one-row-per-egg-journey, row count remains the same)
# The above steps keep the 'universe' restricted to the one we need (whatever that is ^^) because we started with the treatment table already filtered etc.
#
#
# Plots to be made:
# Histograms (single and also facetted by age bin) for each variable of interest
# ==========================================

library(tidyverse)

# Import dataset.
treat = readr::read_csv("Data/Branch Care/Data/treatment_table_20260203 Adam Walter.csv") |> 
  dplyr::select(-pregnant)
str_egg = readr::read_csv("Data/Branch Care/Data/structured_egg_table_20260130 Adam Walter.csv")
raw_embryo_tbl = readr::read_csv("Data/Branch Care/Data/MLData_Nov25 Adam Walter/Embryo.csv")


# Remove extraneous columns in common with treatment table.
str_egg = str_egg |>
  dplyr::select(-c(attempt, age))


# Narrow columns of raw_embryo_tbl
raw_embryo_tbl = raw_embryo_tbl |> 
  dplyr::select(pat_id, egg_id, pregnant, donated, egg_maturity, pronuclei, thawed_oocyte, blast_rating, live_birth_Derive)


# Join structured egg tbl on with pat_id and tx_id
treat_w_egg = treat |> 
  dplyr::left_join(str_egg, by = dplyr::join_by(pat_id, tx_id))

treat_w_egg_and_embryo = treat_w_egg |> 
  dplyr::left_join(raw_embryo_tbl, by = dplyr::join_by(pat_id, egg_id))

# Remove "test" rows
treat_w_egg_and_embryo = treat_w_egg_and_embryo |> 
  dplyr::filter(pat_id != "C100")

# Check that we now have 'pregnant' column.
treat_w_egg_and_embryo |> 
  dplyr::count(pregnant)

# # Check what levels we have in treatment table.
# treat_by_pat_tx_cycle_ids |> 
#   dplyr::filter(live_birth_Derive == 'Outcome Live birth') |> 
#   dplyr::count(live_birth_Derive, pregnant, sort = T)

treat_w_egg_and_embryo |> 
  dplyr::filter(treatment == 'ICSI') |> 
  dplyr::count(pregnant)

# 12K cycles, 5K clinical pregnancies

# grouping_vars = c('pat_id','tx_id')
# TEST: what if we include cycle_id in the grouping vars?
grouping_vars = c('pat_id','tx_id','tx_cycle')

# Add re-calculated number_eggs_collected, etc.
treat_by_pat_tx_cycle_ids = treat_w_egg_and_embryo |>
  dplyr::filter(pregnant %in% c('Biochemical','Clinical','No Scan','Pending')) |> 
  # Question: Are these the right levels for the 'pregnant' column? Could a 'No Scan' in fact be a case of someone achieving pregnancy later?
  #dplyr::filter(pregnant %in% c('Biochemical','Clinical','No Scan','Pending')) |> 
  # Add a categorical binned version of age.
  dplyr::mutate(age_bin = dplyr::case_when(
    age > 42 ~ ">42",
    age > 40 ~ "41-42",
    age > 37 ~ "38-40",
    age > 34 ~ "35-37",
    age < 35 ~ "<35",
    T ~ "Unknown"
  )) |> 
  dplyr::mutate(age_bin = factor(age_bin, levels = c("Unknown","<35","35-37","38-40","41-42",">42")))
  
  

cycle_level <- treat_by_pat_tx_cycle_ids %>%
  mutate(
    amh = coalesce(amh_start, amh),
    afc = coalesce(afc_first_baseline, afc_other)
  ) %>%
  group_by(pat_id, tx_id) %>%
  summarise(
    across(-c(pregnant, live_birth_Derive), first),
    pregnant_rev = as.integer(any(grepl("Clinical", pregnant, ignore.case = TRUE), na.rm = TRUE)),
    birth_rev = as.integer(any(grepl("Outcome Live Birth", live_birth_Derive, ignore.case = TRUE), na.rm = TRUE)),
    .groups = "drop"
  )


test <- cycle_level %>%
  select(dplyr::all_of(grouping_vars),
       treatment_start_date,
       treatment,
       primary_infertility_diagnosis,
       secondary_infertility_diagnosis,
       # number_eggs_collected,
       # number_m2_eggs,
       # number_2pn_eggs,
       # number_euploid_embryos,
       num_eggs_collected,
       total_fertilized,
       num_m2_eggs,
       num_2pn_from_fresh_eggs,
       num_2pn_from_frozen_eggs,
       num_total_blast,
       num_pgds_done,
       num_euploid_blast,
       age,
       age_bin,
       bmi,
       amh,
       afc,
       pregnant_rev,
       birth_rev)
