############################################################
# RECALIBRATION MODULE — Apply AFTER Model Predictions
############################################################

# IMPORTANT:
# You MUST update the following variables for EACH model:

# # ---- CHANGE THESE ----
# actual_outcome <- df_model$OUTCOME_VARIABLE
# predicted_prob <- df_model$predicted_prob
# model_name <- "REPLACE_WITH_MODEL_NAME"
# # ---------------------

# Example replacements:
# OUTCOME_VARIABLE:
# Live birth model → df_live$live_birth
# Euploid model → df_euploid$euploid_success
# Egg model → df_egg$egg_success_binary

# predicted_prob:
# Must match prediction column created earlier

##############################
# FIT CALIBRATION MODEL
##############################

fit_and_apply_calibration_model = function(actual_outcome, predicted_prob){

  cal_model <- glm(actual_outcome ~ predicted_prob, family = binomial)
  
  cal_intercept <- coef(cal_model)[1]
  cal_slope <- coef(cal_model)[2]
  
  predicted_prob_calibrated <- plogis(
    cal_intercept + cal_slope * predicted_prob
  )
  
  return(
    list(
      'preds' = predicted_prob_calibrated |> as.vector() |> as.numeric(),
      'intercept' = cal_intercept,
      'slope' = cal_slope,
      'model' = cal_model)
  )
}

##############################
# INTERPRETATION GUIDE
##############################

# Ideal:
#   Intercept ≈ 0
#   Slope ≈ 1

# If slope < 1:
#   -->Model predictions too extreme (overfitting)
#   --> Calibration shrinks them toward mean

# If slope > 1:
#   --> Predictions too conservative

# ACTION:
# Always use:
#   predicted_prob_calibrated

############################################################
# MODEL VALIDATION MODULE
############################################################

library(pROC)

##############################
# AUC / ROC
##############################

get_auc_of_post_calibrated_model = function(actual_outcome, predicted_prob_calibrated){
  roc_obj <- roc(actual_outcome, predicted_prob_calibrated)
  auc_val <- auc(roc_obj)
  cat("AUC:", auc_val, "\n")
  print(plot(roc_obj, main = paste("ROC Curve -", model_name)))
  
  brier <- mean((predicted_prob_calibrated - actual_outcome)^2)
  cat("Brier Score:", brier, "\n")
  
  dispersion <- deviance(cal_model) / df.residual(cal_model)
  cat("Dispersion:", dispersion, "\n")
  
  cal_table <- data.frame(actual_outcome, pred = predicted_prob_calibrated) %>%
    dplyr::mutate(bin = ntile(pred, 10)) %>%
    dplyr::group_by(bin) %>%
    dplyr::reframe(
      pred_mean = mean(pred),
      actual_mean = mean(actual_outcome)
    )
  
  return(list(
    'auc' = auc_val,
    'brier' = brier,
    'dispersion' = dispersion,
    'cal_table' = cal_table
  ))
}

##############################
# Save Results to Master Table
##############################

# validation_results <- rbind(
#   validation_results,
#   data.frame(
#     Model = model_name,
#     AUC = as.numeric(auc_val),
#     Brier = brier,
#     Calibration_Intercept = cal_intercept,
#     Calibration_Slope = cal_slope,
#     Dispersion = dispersion
#   )
# )

# summarise_
#   summary_table <- df_model %>%
#   summarize(
#     Model = model_name,
#     Avg_Prob = mean(predicted_prob_calibrated),
#     Avg_Payout = mean(expected_payout_full),
#     Avg_Premium = mean(premium_final)
#   )
# 
# print(summary_table)