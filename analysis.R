table(treat_by_pat_tx_cycle_ids$ahm)

Total Treatment Cycles, n	Unique Patients, n	Mean Age, years	Mean BMI, kg/m²	Mean AMH, ng/mL	Mean Baseline AFC	Mean Oocytes Retrieved	Live Birth Rate 1	Clinical Pregnancy Rate 2


test <- raw_embryo_tbl |> 
  dplyr::filter(col_fate == 'Cryopreservation') %>%
  dplyr::filter(preg_derive == 'Outcome Live birth')


table(test$preg_derive)

treat_by_pat_tx_cycle_ids %>%
  summarise(n_unique_pat_id = n_distinct(tx_id))

library(dplyr)

data <- treat_by_pat_tx_cycle_ids %>%
  summarise(
    mean_age = mean(age, na.rm = TRUE),
    mean_bmi = mean(bmi, na.rm = TRUE),
    mean_amh = mean(amh, na.rm = TRUE),
    mean_afc = mean(afc, na.rm = TRUE),
    mean_oocytes = mean(num_eggs_collected, na.rm = TRUE),
    live_birth_rate = mean(live_birth_derive, na.rm = TRUE),
    clinical_preg_rate = mean(pregnant_bool, na.rm = TRUE)
  )

test <- treat_by_pat_tx_cycle_ids %>%
  group_by(age_bin)

summary_table_age <- treat_by_pat_tx_cycle_ids %>%
  group_by(age_bin) %>%
  summarise(
    `Total Cycles, n` = n(),
    `Unique Patients, n` = n_distinct(pat_id),
    `Mean AMH` = mean(amh, na.rm = TRUE),
    `Mean Oocytes Retrieved` = mean(num_eggs_collected, na.rm = TRUE),
    
    `Fertilization Rate` = sum(total_fertilized, na.rm = TRUE) /
      sum(num_eggs_collected, na.rm = TRUE),
    
    `Blastulation Rate` = sum(num_total_blast, na.rm = TRUE) /
      sum(num_eggs_collected, na.rm = TRUE),
    
    `Number of Clinical Pregnancies` = sum(pregnant_bool, na.rm = TRUE),
    `Clinical Pregnancy Rate` = mean(pregnant_bool, na.rm = TRUE),
    
    `Number of Live Births` = sum(live_birth_derive, na.rm = TRUE),
    `Live Birth Rate` = mean(live_birth_derive, na.rm = TRUE),
    
    `PGT Cycles, n` = sum(num_pgds_done > 0, na.rm = TRUE),
    `Mean Euploid Blastocysts` = mean(num_euploid_blast, na.rm = TRUE),
    
    `Euploid Rate` = sum(num_euploid_blast, na.rm = TRUE) /
      sum(num_pgds_done, na.rm = TRUE),
    
    `Percentage of ART cycles that resulted in live-birth delivery` = mean(live_birth_derive, na.rm = TRUE),
    
    .groups = "drop"
  )






treat_by_pat_tx_cycle_ids |> 
  dplyr::distinct(pat_id, ) |> 
  dplyr::count(pregnant)

table(treat_by_pat_tx_cycle_ids$pregnant)
