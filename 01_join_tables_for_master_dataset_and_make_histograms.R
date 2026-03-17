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

treat = readr::read_csv("Data/Branch Care/Data/treatment_table_20260203 Adam Walter.csv") |> 
  dplyr::select(-pregnant)

# Get row count
nrow(treat)

length(unique(treat$pat_id))
length(unique(treat$tx_id))

# Inner join with structured egg table on pat_id and tx_id

str_egg = readr::read_csv("Data/Branch Care/Data/structured_egg_table_20260130 Adam Walter.csv")

# Remove extraneous columns in commmon with treatment table.
str_egg = str_egg |> 
  dplyr::select(-c(attempt, age))

# Bring in raw embryo treatment
raw_embryo_tbl = readr::read_csv("Data/Branch Care/Data/MLData_Nov25 Adam Walter/Embryo.csv")

# Narrow columns of raw_embryo_tbl
raw_embryo_tbl = raw_embryo_tbl |> 
  dplyr::select(pat_id, egg_id, pregnant)

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

# Check what levels we have in treatment table.
treat_w_egg_and_embryo |> 
  dplyr::filter(pregnant == 'Clinical') |> 
  dplyr::count(treatment, pregnant, sort = T)

treat_w_egg_and_embryo |> 
  dplyr::filter(treatment == 'IVF') |> 
  dplyr::count(pregnant)

# 12K cycles, 5K clinical pregnancies

grouping_vars = c('pat_id','tx_id')

# Add re-calculated number_eggs_collected, etc.
treat_by_pat_tx_ids = treat_w_egg_and_embryo |>
  # Question: Are these the right levels for the 'pregnant' column? Could a 'No Scan' in fact be a case of someone achieving pregnancy later?
  dplyr::filter(pregnant %in% c('Biochemical','Clinical','No Scan','Pending')) |> 
  # Add a categorical binned version of age.
  dplyr::mutate(age_bin = dplyr::case_when(
    age > 42 ~ ">42",
    age > 40 ~ "41-42",
    age > 37 ~ "38-40",
    age > 34 ~ "35-37",
    age < 35 ~ "<35",
    T ~ "Unknown"
  )) |> 
  dplyr::mutate(age_bin = factor(age_bin, levels = c("Unknown","<35","35-37","38-40","41-42",">42"))) |> 
  dplyr::group_by(dplyr::across(dplyr::all_of(grouping_vars))) |> 
  # Clean up 'pregnant' column: if a combo of pat_id and tx_id ever has 'Pregnant',
  # Apply that to all other rows for that pat_id/tx_id combo.
  dplyr::mutate(pregnant = ifelse(sum(pregnant == 'Clinical') > 1, 'Clinical', pregnant)) |> 
  dplyr::mutate(number_eggs_collected = dplyr::n()) |> 
  dplyr::group_by(dplyr::across(dplyr::all_of(c(grouping_vars,'tx_cycle')))) |> 
  dplyr::mutate(num_eggs_per_cycle = dplyr::n()) |> 
  # Question for Adam / Lara: should we group by attempt as well as pat_id and tx_id?
  dplyr::group_by(dplyr::across(dplyr::all_of(grouping_vars))) |> 
  # In-line filter below needs debugging.
  dplyr::mutate(number_m2_eggs = sum(collection_maturity_day_0 == "MII",na.rm=T)) |> 
  # TO ADD : Get 2pn fertilization (could filter by pronuclei_day_1 == 2) 
  # Question for Adam: if this column is greater than 2, include or not?
  dplyr::mutate(number_2pn_eggs = sum(pronuclei_day_1 == 2,na.rm=T)) |> 
  # Calculate euploid embryos
  dplyr::mutate(number_euploid_embryos = sum(pgd_clinresult %in% c("Euploid", "Normal"),na.rm=T)) |> 
  # Coalesce AMH and AFC
  dplyr::mutate(amh = dplyr::coalesce(amh_start, amh)) |> 
  dplyr::mutate(afc = dplyr::coalesce(afc_first_baseline, afc_other)) |> 
  dplyr::ungroup() |> 
  dplyr::mutate(pregnant_bool = as.numeric(pregnant == 'Clinical')) |> 
  # Narrow down columns to get one row per patient.
  dplyr::select(pat_id,
                tx_id,
                number_eggs_collected,
                number_m2_eggs,
                number_2pn_eggs,
                number_euploid_embryos,
                age,
                age_bin,
                amh,
                afc,
                pregnant,
                pregnant_bool) |> 
  dplyr::distinct()

treat_by_pat_tx_ids |> 
  dplyr::filter(pat_id == 'C100151')

treat_by_pat_tx_ids |> 
  dplyr::count(pat_id, sort = T)

treat_by_pat_tx_ids |> 
  dplyr::count(age, sort = T)

# The above table should have 1 row per patient/treatment_id combo, with all the goodies recalculated from the expanded one-row-per-egg table.

vars_to_explore = names(treat_by_pat_tx_ids |> dplyr::select(-c(pat_id, tx_id, age_bin, pregnant)))

if(!dir.exists('outputs')) dir.create('outputs')

# Cycle through variables to explore for distributions, saving plots to outputs folder.
vars_to_explore |> 
  purrr::iwalk(~{

    distinct_data = treat_by_pat_tx_ids |> 
      dplyr::select(dplyr::all_of(c('pat_id', 'tx_id', !!.x)))
    
    norm_test = shapiro.test(distinct_data[[.x]]) |> 
      broom::tidy() |> 
      dplyr::mutate(data_normal = ifelse(p.value >= 0.05, T, F))

    # Is the variable a character? If so, set stat to count for histogram.
    if(is.character(treat_by_pat_tx_ids[[.x]])){
      p = ggplot(treat_by_pat_tx_ids) + 
        geom_histogram(aes(!!rlang::sym(.x)), stat = "count")
    } else {
      # Variable is numeric, praise be!
      p = ggplot(treat_by_pat_tx_ids) + 
        geom_histogram(aes(!!rlang::sym(.x)))
    }
    
    norm_test_label = paste0("Data is ",ifelse(norm_test$p.value>0.05,"","NOT "),"normally distributed (Shapiro-Wilks p-value ~ ",round(norm_test$p.value,3),")")
    
    p = p  + 
      theme_minimal() + 
      labs(title = paste0(.x," count by pat_id and tx_id"),
           caption = norm_test_label)
    
    if(.x == 'pregnant_bool') p = p + scale_x_continuous(breaks = c(0, 1), labels = c("FALSE","TRUE"))
    
    ggsave(filename = paste0("outputs/",.x,"_histogram.jpg"),
           plot = p, width = 6, height = 5)
    
    # Make a version of this histogram that's also facetted by age bin
    p_age_split = p + 
      facet_wrap(~age_bin, ncol = 1) + 
      labs(title = paste0(.x," count by pat_id, tx_id, and age grouping"))
    
    ggsave(filename = paste0("outputs/",.x,"_histogram_split_by_age_grp.jpg"),
           plot = p_age_split, width = 6, height = 8)
    
  },.progress = T)
