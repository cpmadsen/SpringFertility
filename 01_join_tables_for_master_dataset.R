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
# ==========================================

<test test test>





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

# Check that we now have 'pregnant' column.
treat_w_egg_and_embryo |> 
  dplyr::count(pregnant)


# 12K cycles, 5K clinical pregnancies

# ===== Code below is an unfinished draft ===== # 

# Coalesce various columns, collapse some columns into 'summarized' columns like 'normal and euploid'.
treat_w_egg_and_embryo |> 
  dplyr::mutate(healthy_embryo = )

# Add re-calculated number_eggs_collected, etc.
treat_w_egg_and_embryo |>
  dplyr::group_by(pat_id, tx_id) |> 
  dplyr::mutate(number_eggs_collected = dplyr::n()) |> 
  dplyr::group_by(pat_id, tx_id, )
  dplyr::add_count(pat_id, tx_id, name = 'number_eggs_collected') |> 
  dplyr::add_count()
