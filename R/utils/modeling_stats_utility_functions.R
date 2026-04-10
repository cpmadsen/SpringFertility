summarise_to_age_classes = function(dat, var_to_mean){
  dat |> 
    mutate(age_group = case_when(
      age < 35 ~ "<35",
      age >= 35 & age <= 37 ~ "35-37",
      age >= 38 & age <= 40 ~ "38-40",
      age >= 41 ~ "41+"
    )) |> 
    dplyr::mutate(age_group = factor(age_group, levels = c("<35","35-37","38-40","41+"))) |> 
    dplyr::group_by(age_group) |> 
    dplyr::reframe(
      !!paste0(var_to_mean, "_mean") := mean(!!rlang::sym(var_to_mean), na.rm = TRUE),
      !!paste0(var_to_mean, "_median") := median(!!rlang::sym(var_to_mean), na.rm = TRUE)
    ) |> 
    dplyr::arrange(age_group)
}

run_binomial_model_with_various_distributions = function(dat, formula){
  
  formula <- as.formula(
    formula
  )
  
  binom_mod = glm(
    # cbind(successes, attempts - successes) ~ age,
    formula = formula,
    family = binomial,
    data = patient_summary
  )
  
  validation_res = validate_model(binom_mod, data = patient_summary, outcome_var = 'successes') |> 
    # Get console output as make into a list
    capture.output() |> 
    suppressWarnings()
  
  # snag the dispersion.
  dispersion_amount = validation_res[2] |>
    stringr::str_extract('(?<=: ).*(?=\")') |> 
    as.numeric() |> 
    round(4)
  
  if(dispersion_amount > 1.5){
    cat(paste0("\nDispersion amount was too high for binomial distribution (",dispersion_amount,") - now trying quasibinomial distribution."))
    binom_mod = glm(
      cbind(successes, attempts - successes) ~ age,
      family = quasibinomial,
      data = patient_summary
    )
    validation_res = validate_model(binom_mod, data = patient_summary, outcome_var = 'successes') |> 
      # Get console output as make into a list
      capture.output() |> 
      suppressWarnings()
    # snag the dispersion.
    dispersion_amount = validation_res[2] |>
      stringr::str_extract('(?<=: ).*(?=\")') |> 
      as.numeric() |> 
      round(4)
    if(dispersion_amount > 1.5){
      cat(paste0("\nDispersion amount was too high for quasibinomial distribution (",dispersion_amount,") - now trying betabinomial distribution."))
      binom_mod = glm(
        cbind(successes, attempts - successes) ~ age,
        family = betabinomialff,
        data = patient_summary
      )
      validation_res = validate_model(binom_mod, data = patient_summary, outcome_var = 'successes') |> 
        # Get console output as make into a list
        capture.output() |> 
        suppressWarnings()
      # snag the dispersion.
      dispersion_amount = validation_res[2] |>
        stringr::str_extract('(?<=: ).*(?=\")') |> 
        as.numeric() |> 
        round(4)
    }
  }
  return(binom_mod)
}


############################################################
# VALIDATION FUNCTION
############################################################

validate_model <- function(model, data, outcome_var) {

  pred <- predict(model, type = "response")
  
  # AUC
  # Are there more than two levels of the outcome var?
  lvls_outcome = length(unique(data[[outcome_var]]))
  
  if(lvls_outcome == 2){
    roc_obj <- roc(data[[outcome_var]], pred)
  } else {
    roc_obj <- multiclass.roc(data[[outcome_var]], pred) |> suppressMessages()
  }
  
  auc_val <- auc(roc_obj)
  
  # Calibration
  data$pred_bin <- cut(pred, breaks = 10)
  calib <- data %>%
    group_by(pred_bin) %>%
    summarize(predicted = mean(pred),
              observed = mean(.data[[outcome_var]]))
  
  # Dispersion
  dispersion <- sum(residuals(model, type = "pearson")^2) / model$df.residual
  
  print(paste("AUC:", auc_val))
  print(paste("Dispersion:", dispersion))
  # print(calib)
  return(calib)
}

############################################################
# FUNCTION: MONTE CARLO PRICING ENGINE
############################################################

run_simulation <- function(p_vec, cost, payout, n_sim) {
  
  sim_costs <- replicate(n_sim, {
    
    p <- sample(p_vec, 1)
    outcome <- rbinom(1, 1, p)
    
    payout_cost <- ifelse(outcome == 1, 0, payout)
    total_cost <- cost + payout_cost
    
    return(total_cost)
  })
}

# ############################################################
# # FUNCTION: FINAL PRICING
# ############################################################
# 
# price_from_sim <- function(sim_df, risk_margin, admin_load) {
# 
#   expected_cost <- mean(sim_df$total_cost)
# 
#   # Risk metrics
#   var_95 <- quantile(sim_df$total_cost, 0.95)
# 
#   premium <- expected_cost * (1 + risk_margin + admin_load)
# 
#   return(data.frame(
#     expected_cost = expected_cost,
#     var_95 = var_95,
#     premium = premium
#   ))
# }