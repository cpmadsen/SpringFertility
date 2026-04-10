var_summary <- df %>%
  mutate(
    total_2pn_eggs = num_2pn_from_fresh_eggs + num_2pn_from_frozen_eggs
  ) %>%
  group_by(age_group) %>%
  summarise(
    n_cycles = n(),
    
    # Euploid embryos
    mean_euploid    = mean(num_euploid_blast, na.rm = TRUE),
    iqr_euploid_l   = quantile(num_euploid_blast, 0.25, na.rm = TRUE),
    iqr_euploid_u   = quantile(num_euploid_blast, 0.75, na.rm = TRUE),
    pct_ge1_euploid = mean(coalesce(num_euploid_blast, 0) >= 1) * 100,
    
    # Eggs retrieved
    mean_eggs       = mean(num_eggs_collected, na.rm = TRUE),
    iqr_eggs_l      = quantile(num_eggs_collected, 0.25, na.rm = TRUE),
    iqr_eggs_u      = quantile(num_eggs_collected, 0.75, na.rm = TRUE),
    
    # MII eggs
    mean_m2         = mean(num_m2_eggs, na.rm = TRUE),
    iqr_m2_l        = quantile(num_m2_eggs, 0.25, na.rm = TRUE),
    iqr_m2_u        = quantile(num_m2_eggs, 0.75, na.rm = TRUE),
    
    # Fertilized (2PN)
    mean_2pn        = mean(total_2pn_eggs, na.rm = TRUE),
    iqr_2pn_l       = quantile(total_2pn_eggs, 0.25, na.rm = TRUE),
    iqr_2pn_u       = quantile(total_2pn_eggs, 0.75, na.rm = TRUE),
    
    # AMH
    mean_amh        = mean(amh, na.rm = TRUE),
    iqr_amh_l       = quantile(amh, 0.25, na.rm = TRUE),
    iqr_amh_u       = quantile(amh, 0.75, na.rm = TRUE),
    
    # AFC
    mean_afc        = mean(afc, na.rm = TRUE),
    iqr_afc_l       = quantile(afc, 0.25, na.rm = TRUE),
    iqr_afc_u       = quantile(afc, 0.75, na.rm = TRUE),
    
    .groups = "drop"
  )


var_summary_fmt <- var_summary %>%
  mutate(
    eggs_fmt = sprintf("%.1f (%.1f–%.1f)", mean_eggs, iqr_eggs_l, iqr_eggs_u),
    m2_fmt   = sprintf("%.1f (%.1f–%.1f)", mean_m2, iqr_m2_l, iqr_m2_u),
    p2n_fmt  = sprintf("%.1f (%.1f–%.1f)", mean_2pn, iqr_2pn_l, iqr_2pn_u),
    amh_fmt  = sprintf("%.2f (%.2f–%.2f)", mean_amh, iqr_amh_l, iqr_amh_u),
    afc_fmt  = sprintf("%.1f (%.1f–%.1f)", mean_afc, iqr_afc_l, iqr_afc_u),
    euploid_fmt = sprintf("%.1f (%.1f–%.1f)", mean_euploid, iqr_euploid_l, iqr_euploid_u),
    euploid_pct = sprintf("%.1f%%", pct_ge1_euploid)
  )





euploid_summary <- df %>%
  group_by(age_group) %>%
  summarise(
    n_ge1_euploid = sum(coalesce(num_euploid_blast, 0) >= 1),
    total_cycles = n()
  )
