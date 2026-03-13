
treat = readr::read_csv("Data/Branch Care/Data/treatment_table_20260203 Adam Walter.csv") |> 
  dplyr::select(-pregnant)

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

# Coalesce various columns, collapse some columns into 'summarized' columns like 'normal and euploid'.
treat_w_egg_and_embryo |> 
  dplyr::mutate(healthy_embryo = )

# Add re-calculated number_eggs_collected, etc.
treat_w_egg_and_embryo |>
  dplyr::group_by(pat_id, tx_id) |> 
  dplyr::mutate(number_eggs_collected = dplyr::n()) |> 
  dplyr::group_by(pat_id, tx_id, )
  dplyr::add_count(pat_id, tx_id, name = 'number_eggs_collected') |> 
  dplyr::add_count
