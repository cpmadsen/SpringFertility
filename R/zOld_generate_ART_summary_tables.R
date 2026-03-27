# =====================================
# Title:
# Author: 
# Date: 
# 
# This script: 
# 
# =====================================

# Load in data

dat = readr::read_csv("data/clean_data/treatments_by_pat_id_tx_id_cycle_id.csv")

# 
clinic_summary_incomplete = dat |> 
  dplyr::filter(lubridate::year(treatment_start_date) == 2022) |> 
  dplyr::reframe(total_cycles = length(unique(tx_cycle)),
                 pregnancies = sum(pregnant_bool[number_euploid_embryos > 1]))

clinic_summary_incomplete

