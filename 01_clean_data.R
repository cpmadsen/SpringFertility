# This script defines a cleaning function for the key variables present in the 
# two cleaned datasets: "treatment_table" and "structured_egg_table"

library(data.table)
library(tidyverse)

treatdf = fread("data/Branch Care/Data/treatment_table_20260203 Adam Walter.csv")
eggdf = fread("data/Branch Care/Data/structured_egg_table_20260130 Adam Walter.csv")

# Get colnames.
names(treatdf)

# Check levels of pre-cleaned data.

# How many unique pat_ids?
length(unique(treatdf$pat_id))
# 7,426 unique pat_ids.

length(unique(treatdf$tx_id))
# Before cleaning up eggs that are used in treatment n, frozen, then thawed and used for treatment n+1, there are
# 12,495 unique treatment IDs.

treatdf |> dplyr::count(pat_id, tx_id, sort = T)
treatdf |> dplyr::count(pat_id, tx_cycle, sort = T)
# Single row per tx_id and tx_cycle. Good to know the table structure.

treatments_per_pat = treatdf |> dplyr::count(pat_id, tx_id, sort = T) |> dplyr::count(pat_id, sort = T)

ggplot(treatments_per_pat) + geom_histogram(aes(n))

treatments_per_pat |> dplyr::count(n)
# Most pats have a single treatment ID (4,634 of them), number of treatments per pat falls off pretty quickly. 

treatdf[,.N,by=treatment]
# Looks clean

treatdf[,.N,by=pregnant]
# Looks clean, but only 194 with level 'Clinical', 7 'Pending', 41 'No Scan', most are 'No Data', thousands of 'Negative'

treatdf[,.N,by=preg_derive]

treatdf[!is.na(preg_derive),][,.N,by=.(pregnant,preg_derive)]
# A bunch of HCG levels (e.g. 6.37, 178.60, 0.25, 0.80, 2.20)
# Only 63 'Positive Pregnancy Test', most NA
# Maybe we can lump 'Positive Pregnancy Test', 'Clinical Pregnancy Date', 'Outcome Live birth', and 'Estimated Delivery Date'
# into a bucket of 'Positive Pregnancy Test'

treatdf[, preg_derive := ifelse(
  preg_derive %in% c('Positive Pregnancy Test', 'Clinical Pregnancy Date', 'Outcome Live birth', 'Estimated Delivery Date'),
  'Positive Pregnancy Test',
  preg_derive
  )]
treatdf[,.N,by=preg_derive]
# If we apply that 'cleaning' step, we're up to 262 Positive Pregnancy tests!







treatdf[,.N,by=live_birth]
# 12K NA, only 73 true - looks like the column 'live_birth_derive' is cleaner and we should use that one.

treatdf[,.N,by=live_birth_derive]
# 12K NA, 78 of 'Outcome Live birth'

# Double check table where pregnant is No Data
treatdf[pregnant == 'No Data',] |> View()

treatdf[pregnant == 'No Data',][,.N,by=preg_derive]
# 19 Positive Pregnancy Test, 1 Estimated Delivery Date - could change 'pregnant' to perhaps 'Clinical'?
 
