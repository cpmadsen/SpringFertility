## Euploid Embryo Model (Binomial)

```{r}
############################################################
# 1. EUPLOID EMBRYO MODEL (BINOMIAL)
############################################################

# Aggregate structured egg table to patient-level counts
euploid_summary <- egg_df |>
  dplyr::filter(!frozen_egg) |>
  dplyr::mutate(
    success = pgd_clinresult %in% c("Euploid", "Normal") & !is.na(pgd_clinresult)
  ) |>
  dplyr::group_by(pat_id) |>
  dplyr::reframe(
    successes = sum(success),
    number_of_eggs_attempted = dplyr::n(),
    age = median(age, na.rm = TRUE),
    age_group = dplyr::first(age_group)
  )

# Fit binomial model for per-egg euploid probability as a function of age
euploid_binom_model <- glm(
  cbind(successes, number_of_eggs_attempted - successes) ~ age,
  family = binomial,
  data = euploid_summary
)

# Save coefficient summary
euploid_model_summary <- coef(summary(euploid_binom_model)) |>
  as.data.frame()

# Add predicted probabilities
euploid_summary <- euploid_summary |>
  dplyr::mutate(
    pred_p = predict(euploid_binom_model, newdata = euploid_summary, type = "response"),
    prob_ge1_euploid = 1 - (1 - pred_p)^number_of_eggs_attempted
  )

# Simulate outcomes per patient
set.seed(123)

euploid_sim_results <- euploid_summary |>
  dplyr::rowwise() |>
  dplyr::mutate(
    success_sim = rbinom(1, size = number_of_eggs_attempted, prob = pred_p),
    ge1_euploid_sim = success_sim >= 1
  ) |>
  dplyr::ungroup()

# Plot predicted per-egg probabilities from the model
euploid_summary |>
  ggplot(aes(x = pred_p)) +
  geom_histogram(bins = 30) +
  labs(
    x = "Predicted Per-Egg Euploid Probability",
    title = "Distribution of Predicted Probabilities"
  ) +
  theme_minimal()

print("The following table represents simulated successes or failures at the unique pat_id level based on the euploid binomial model.")

euploid_sim_results |>
  DT::datatable()
```

### Simulated Probabilities averaged by Age Group
```{r}
euploid_summary |> 
  dplyr::group_by(age_group) |> 
  dplyr::reframe(
    prob_ge1_mean = mean(prob_ge1_euploid, na.rm = TRUE),
    prob_ge1_median = median(prob_ge1_euploid, na.rm = TRUE)
  ) |> 
  dplyr::arrange(age_group) |> 
  knitr::kable()
```

### Model with Primary Infertility Diagnosis Added
```{r}
euploid_summary = egg_df |>
  dplyr::mutate(success = pgd_clinresult %in% c("Euploid","Normal") & !is.na(pgd_clinresult)) |> 
  group_by(pat_id) |>
  summarise(
    successes = sum(success),
    number_of_eggs_attempted = n(),
    age = first(age),
    age_group = first(age_group),
    .groups = "drop"
  ) |> 
  dplyr::left_join(df |> dplyr::select(pat_id, primary_infertility_diagnosis_cl), by = dplyr::join_by(pat_id))

euploid_binom_model = glm(
  cbind(successes, number_of_eggs_attempted - successes) ~ age + primary_infertility_diagnosis_cl,
  family = binomial,
  data = euploid_summary
)

validate_model(euploid_binom_model, euploid_summary, 'successes')

euploid_model_summary = coef(summary(euploid_binom_model)) |>
  as.data.frame()

coef_euploid <- coef(euploid_binom_model)

alpha <- exp(coef_euploid[1])
beta <- exp(coef_euploid[2])

validate_model(euploid_binom_model, euploid_summary, "successes")

# Add probabilities to egg_df

euploid_p = rbeta(n_sim, alpha, beta)

euploid_costs <- run_simulation(euploid_p, cost_per_cycle, payout_amount, n_sim)

data.frame(pred_probs = euploid_p) |>
  ggplot() +
  geom_histogram(aes(pred_probs)) + 
  labs(x = "Simulated Probability", title = paste0("Simulated Probabilities with Monte-Carlo: ",n_sim," simulation runs")) + 
  theme_minimal()

# Simulate results with egg dataframe.
euploid_sim_results <- euploid_summary %>%
  rowwise() %>%
  mutate(
    simulated_p = rbeta(1, shape1 = 2, shape2 = 5), # example params
    success_sim = rbinom(1, size = number_of_eggs_attempted, prob = simulated_p)
  )

print("The following table represents predicted successes or failures at the unique pat_id level based on the euploid binomial model.")

euploid_sim_results |> 
  DT::datatable()


```

### Simulated Probabilities averaged by Age Group with Primary Inf Diagnosis
```{r}
euploid_sim_results |> 
  dplyr::group_by(age_group) |> 
  dplyr::reframe(
    simulated_p_mean = mean(simulated_p, na.rm = TRUE),
    simulated_p_median = median(simulated_p, na.rm = TRUE)
  ) |> 
  dplyr::arrange(age_group) |> 
  knitr::kable()
```