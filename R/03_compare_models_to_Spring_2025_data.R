
library(dplyr)
library(magrittr)
library(tidyr)
library(purrr)

output_log = list()

# Assumes the 2025 data is loaded as df_2025
df_2025 = openxlsx::read.xlsx("data/spring_2025/2025 outcomes for risk premium validation.xlsx")

# and the fitted models are livebirth_model, euploid_model, egg1_model
livebirth_model = readr::read_rds("data/models/livebirth_model_revised_modeling_statistics_end.rds")
euploid_model = readr::read_rds("data/models/euploid_model_revised_modeling_statistics_end.rds")
egg1_model = readr::read_rds("data/models/egg_1_model_revised_modeling_statistics_end.rds")
egg2_model = readr::read_rds("data/models/egg_2_model_revised_modeling_statistics_end.rds")

# Step 0: Align datatype between models and Spring's 2025 data.
df_2025 = df_2025 |> 
  dplyr::mutate(amh = as.numeric(amh),
                afc = as.numeric(afc),
                num_euploid_embryos = as.numeric(num_euploid_embryos)) |> 
  dplyr::rename(patient_id = id)

# Just look at ICSI rows?
df_2025 = df_2025 |> 
  dplyr::filter(stringr::str_detect(treatment, 'ICSI'))

# df_2025 = df_2025 |> 
#   dplyr::filter(!is.na(amh) & !is.na(afc))

# Step 1: Get predictions on 2025 data
df_2025$p_raw_lb <- predict(euploid_model, newdata=df_2025, type="response")

# Calibrate using your April 2026 parameters
logit <- function(p) log(p/(1-p))
df_2025$p_cal_lb <- plogis(0.0152 + 0.8508 * logit(df_2025$p_raw_lb))

# Step 2: Check overall calibration
cat("Predicted mean:", mean(df_2025$p_cal_lb, na.rm=TRUE))

output_log = output_log |> append(list(paste0("Predicted mean:", mean(df_2025$p_cal_lb, na.rm=TRUE))))

cat("Observed mean:", mean(df_2025$num_euploid_embryos, na.rm=TRUE))

output_log = output_log |> append(list(paste0("Observed mean:", mean(df_2025$num_euploid_embryos, na.rm=TRUE))))

# Step 3: Calibration by decile
df_2025$decile <- ntile(df_2025$p_cal_lb, 10)
calib_check <- df_2025 %>%
  group_by(decile) %>%
  summarize(
    mean_predicted = mean(p_cal_lb,na.rm=T),
    mean_observed  = mean(num_euploid_embryos,na.rm=T),
    n = n()
  )

print(calib_check)
output_log = output_log |> append(list(calib_check))

# Step 4: AUC
library(pROC)
roc_2025 <- roc(df_2025$num_euploid_embryos, df_2025$p_cal_lb)
cat("AUC on 2025 data:", auc(roc_2025))
output_log = output_log |> append(list(paste0("AUC on 2025 data:", auc(roc_2025))))

# Step 5: Population shift check
df_2025 %>%
  summarize(
    mean_age = mean(age), median_age = median(age),
    mean_amh = mean(amh),       median_amh = median(amh),
    mean_afc = mean(afc),       median_afc = median(afc),
    n = n()
  ) %>%
  print()

output_log = output_log |> append(list(df_2025 %>%
                                         summarize(
                                           mean_age = mean(age), median_age = median(age),
                                           mean_amh = mean(amh),       median_amh = median(amh),
                                           mean_afc = mean(afc),       median_afc = median(afc),
                                           n = n()
                                         )))

# Generate payout review list for egg freezing
payout_review <- df_2025 %>%
  mutate(
    # Get model prediction
    mu_hat = predict(egg1_model, newdata=., type="response"),
    
    # Look up guarantee threshold (or compute from simulation)
    threshold_90 = map_dbl(row_number(), ~{
      # simplified lookup - replace with actual EggLookup INDEX/MATCH
      floor(qnbinom(0.10, mu=mu_hat[.x], size=egg1_model$theta))
    }),
    
    # Flag payout cases
    triggers_payout = (num_m2_eggs < threshold_90),
    
    # Flag where model expected good outcome but got bad
    model_predicted_low_risk = (mu_hat >= 8),  # expected ≥8 eggs
    surprise_failure = triggers_payout & model_predicted_low_risk
    
  ) %>%
  filter(triggers_payout) %>%
  dplyr::select(patient_id, age, amh, afc, mu_hat, threshold_90,
                num_m2_eggs, triggers_payout, surprise_failure) %>%
  arrange(desc(surprise_failure), mu_hat) |> 
  tidyr::as_tibble()

payout_review %>%
  mutate(has_amh = !is.na(amh)) %>%
  group_by(has_amh) %>%
  summarise(
    n = n(),
    has_mu_hat = sum(!is.na(mu_hat)),
    mean_mu_hat = mean(mu_hat, na.rm=TRUE),
    mean_afc = mean(afc, na.rm=TRUE),
    mean_age = mean(age, na.rm=TRUE)
  )

print(payout_review)
output_log = output_log |> append(list(payout_review))

cat("\nTotal payout cases:", nrow(payout_review))
cat("\nSurprise failures (model expected good outcome):",
    sum(payout_review$surprise_failure))

output_log = output_log |> append(list(paste0("\nTotal payout cases:", nrow(payout_review))))
output_log = output_log |> append(list(paste0("\nSurprise failures (model expected good outcome):",
                                              sum(payout_review$surprise_failure))))

file.remove("outputs/comparison_models_to_spring_2025_data.txt")

# output_log |> 
#   purrr::iwalk(~{
#     if(length(.x) == 1){
#       writeLines(text = .x, con = "outputs/comparison_models_to_spring_2025_data.txt")
#     } else {
#       .x |> 
#         group_by(dplyr::row_number()) |> 
#         group_split() |> 
#         lapply(\(y){
#           writeLines(text = y, con = "outputs/comparison_models_to_spring_2025_data.txt")
#         })
#     }
#   },.progress=T)

file_name <- "outputs/comparison_models_to_spring_2025_data.txt"

# Clear the file if it already exists
cat("", file = file_name) 

for (i in 1:length(output_log)) {
  # Write the name of the list element
  
  item <- output_log[[i]]
  cat('\n', file = file_name, append = TRUE, sep = "\n")
  
  if (is.data.frame(item) || is.matrix(item)) {
    # Save tables with headers and tab separation
    write.table(item, file = file_name, append = TRUE, sep = "\t", 
                row.names = FALSE, quote = FALSE)
  } else {
    # Save simple text or vectors
    cat(item, file = file_name, append = TRUE, sep = "\n")
  }
}
