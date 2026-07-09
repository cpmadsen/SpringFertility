############################################################
# CUMULATIVE SUCCESS PROBABILITY PLOTS
# Generates patient-profile-specific cumulative probability
# curves for live birth and euploid models.
#
# Three plot types:
#   1. Cumulative prob by age for fixed AMH/AFC profiles
#   2. Cumulative prob by AMH for fixed age profiles
#   3. Heatmap of 2-cycle cumulative prob by age x AMH
#
# Run AFTER fertility_master_v6_1.R so models are in memory.
# Or source this file standalone -- paste your coefficients
# into Section 0 below.
############################################################


library(ggplot2)
library(dplyr)
library(tidyr)
library(patchwork)   # install if needed: install.packages("patchwork")


# ── 0. MODEL COEFFICIENTS ──────────────────────────────
# !! NOTE !!: paste your fitted coefficients here.
# These come from coef(livebirth_model) and coef(euploid_model)
# printed at the end of fertility_master_v6_1.R.
# If models are already in your R session, skip this block
# and the functions below will use the existing objects.


# Calibration parameters (from April 2026 validated run)

lb_cal_intercept <- readr::read_rds("data/models/lb_cal_int_refit.rds")
lb_cal_slope     <- readr::read_rds("data/models/lb_cal_slope_refit.rds")
eu_cal_intercept <- readr::read_rds("data/models/eu_cal_int_refit.rds")
eu_cal_slope     <- readr::read_rds("data/models/eu_cal_slope_refit.rds")

livebirth_model <- readr::read_rds("data/models/livebirth_model_revised_modeling_statistics_end.rds")
euploid_model   <- readr::read_rds("data/models/euploid_model_revised_modeling_statistics_end.rds")

# Age floor
age_floor <- 32


# ── 1. PREDICTION FUNCTIONS ────────────────────────────


apply_cal <- function(p_raw, intercept, slope) {
  logit_raw <- log(pmax(pmin(p_raw, 0.9999), 0.0001) /
                     (1 - pmax(pmin(p_raw, 0.9999), 0.0001)))
  1 / (1 + exp(-(intercept + slope * logit_raw)))
}


# p_cal for live birth (conditional on euploid)
p_cal_lb <- function(age, amh, afc,
                     model = livebirth_model) {
  age_m <- pmax(age, age_floor)
  nd <- data.frame(age_model = age_m, amh = amh, afc = afc)
  p_raw <- predict(model, newdata = nd, type = "response")
  apply_cal(p_raw, lb_cal_intercept, lb_cal_slope)
}


# p_cal for euploid
p_cal_eu <- function(age, amh, afc,
                     model = euploid_model) {
  age_m <- pmax(age, age_floor)
  nd <- data.frame(age_model = age_m, amh = amh, afc = afc)
  p_raw <- predict(model, newdata = nd, type = "response")
  apply_cal(p_raw, eu_cal_intercept, eu_cal_slope)
}


# Cumulative success prob over N cycles
cum_success <- function(p_per_cycle, n_cycles) {
  1 - (1 - p_per_cycle)^n_cycles
}


# ── 2. PATIENT PROFILES ────────────────────────────────
# Define representative profiles varying one dimension
# while holding others at typical values


ages     <- 32:43
amh_vals <- c(0.3, 0.5, 1.0, 2.0, 4.0, 8.0)
afc_vals <- c(3, 6, 10, 15, 20, 30)
n_cycles <- 1:3


# Typical fixed values
typical_amh <- 2.0
typical_afc <- 12
typical_age <- 36


# AMH profile labels
amh_labels <- c(
  "0.3" = "AMH 0.3 (very low)",
  "0.5" = "AMH 0.5 (low)",
  "1"   = "AMH 1.0 (below avg)",
  "2"   = "AMH 2.0 (average)",
  "4"   = "AMH 4.0 (good)",
  "8"   = "AMH 8.0 (high)"
)


# ── 3. FIGURE 1: CUMULATIVE PROB BY AGE x AMH PROFILE ──
# For each AMH level, plot cumulative P(success) over
# ages 28-43 for 1, 2, and 3 cycle products.
# Two panels: live birth (left) and euploid (right).


df_age <- expand.grid(
  age    = ages,
  amh    = amh_vals,
  cycles = n_cycles
) |>
  tidyr::as_tibble() |> 
  dplyr::mutate(afc = typical_afc) %>% 
  dplyr::group_by(age, amh, afc) |> 
  dplyr::group_split() |> 
  lapply(\(x) {
    x |> 
      dplyr::mutate(
    p_lb   = p_cal_lb(x$age, x$amh, x$afc),
    p_eu   = p_cal_eu(x$age, x$amh, x$afc)
      )
  }) |> 
  dplyr::bind_rows() |> 
  dplyr::mutate(
    cum_lb = cum_success(p_lb, cycles),
    cum_eu = cum_success(p_eu, cycles),
    amh_label = factor(
      paste0("AMH ", amh),
      levels = paste0("AMH ", amh_vals)
    ),
    cycle_label = factor(
      paste0(cycles, "-cycle"),
      levels = c("1-cycle","2-cycle","3-cycle")
    )
  )


palette_amh <- c(
  "#D62728","#FF7F0E","#BCBD22",
  "#17BECF","#1F77B4","#2CA02C"
)


p1_lb <- ggplot(df_age,
                aes(x = age, y = cum_lb,
                    color = amh_label,
                    linetype = cycle_label)) +
  geom_line(linewidth = 0.9) +
  scale_color_manual(values = palette_amh,
                     name   = "AMH Profile") +
  scale_linetype_manual(
    values = c("solid","dashed","dotted"),
    name   = "Cycles") +
  scale_y_continuous(labels = scales::percent_format(accuracy=1),
                     limits = c(0, 1)) +
  scale_x_continuous(breaks = seq(28, 43, 2)) +
  labs(
    title    = "Live Birth Guarantee",
    subtitle = paste0("AFC held at ", typical_afc,
                      " | Calibrated model (slope=",
                      lb_cal_slope, ")"),
    x = "Age",
    y = "Cumulative P(success)"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title    = element_text(face = "bold", size = 12),
    plot.subtitle = element_text(color = "grey50", size = 9),
    legend.position = "right",
    panel.grid.minor = element_blank()
  )


p1_eu <- ggplot(df_age,
                aes(x = age, y = cum_eu,
                    color = amh_label,
                    linetype = cycle_label)) +
  geom_line(linewidth = 0.9) +
  scale_color_manual(values = palette_amh,
                     name   = "AMH Profile") +
  scale_linetype_manual(
    values = c("solid","dashed","dotted"),
    name   = "Cycles") +
  scale_y_continuous(labels = scales::percent_format(accuracy=1),
                     limits = c(0, 1)) +
  scale_x_continuous(breaks = seq(28, 43, 2)) +
  labs(
    title    = "Euploid Embryo Guarantee",
    subtitle = paste0("AFC held at ", typical_afc,
                      " | Calibrated model (slope=",
                      eu_cal_slope, ")"),
    x = "Age",
    y = "Cumulative P(success)"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title    = element_text(face = "bold", size = 12),
    plot.subtitle = element_text(color = "grey50", size = 9),
    legend.position = "right",
    panel.grid.minor = element_blank()
  )


fig1 <- p1_lb + p1_eu +
  plot_annotation(
    title   = "Cumulative Success Probability by Age, AMH Profile, and Cycle Count",
    caption = paste0("Based on calibrated April 2026 model. AFC=", typical_afc,
                     ". Individual pricing uses patient-specific AMH and AFC."),
    theme   = theme(
      plot.title   = element_text(face="bold", size=13),
      plot.caption = element_text(color="grey50", size=8)
    )
  )


ggsave("outputs/fig1_cumulative_prob_by_age_amh.png",
       fig1, width = 14, height = 6, dpi = 150)
message("Figure 1 saved: outputs/fig1_cumulative_prob_by_age_amh.png")


# ── 4. FIGURE 2: CUMULATIVE PROB BY AMH x AFC PROFILE ──
# For fixed age, show how cumulative prob varies across
# AMH and AFC combinations. 2-cycle product shown.


amh_grid <- seq(0.3, 10, length.out = 50)
afc_grid <- c(5, 10, 15, 20, 30)


df_amh <- expand.grid(
  amh = amh_grid,
  afc = afc_grid
) %>%
  mutate(
    age       = typical_age,
    p_lb      = mapply(p_cal_lb, age, amh, afc),
    p_eu      = mapply(p_cal_eu, age, amh, afc),
    cum_lb_2  = cum_success(p_lb, 2),
    cum_eu_2  = cum_success(p_eu, 2),
    afc_label = factor(
      paste0("AFC ", afc),
      levels = paste0("AFC ", sort(afc_grid))
    )
  )


palette_afc <- c("#E41A1C","#FF7F00","#4DAF4A","#377EB8","#984EA3")


p2_lb <- ggplot(df_amh,
                aes(x = amh, y = cum_lb_2,
                    color = afc_label)) +
  geom_line(linewidth = 1.0) +
  scale_color_manual(values = palette_afc, name = "AFC") +
  scale_y_continuous(labels = scales::percent_format(accuracy=1),
                     limits = c(0, 1)) +
  scale_x_continuous(breaks = c(0,2,4,6,8,10)) +
  labs(
    title    = "Live Birth -- 2-Cycle",
    subtitle = paste0("Age held at ", typical_age),
    x = "AMH (ng/mL)",
    y = "Cumulative P(success, 2cy)"
  ) +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face="bold"),
        panel.grid.minor = element_blank())


p2_eu <- ggplot(df_amh,
                aes(x = amh, y = cum_eu_2,
                    color = afc_label)) +
  geom_line(linewidth = 1.0) +
  scale_color_manual(values = palette_afc, name = "AFC") +
  scale_y_continuous(labels = scales::percent_format(accuracy=1),
                     limits = c(0, 1)) +
  scale_x_continuous(breaks = c(0,2,4,6,8,10)) +
  labs(
    title    = "Euploid -- 2-Cycle",
    subtitle = paste0("Age held at ", typical_age),
    x = "AMH (ng/mL)",
    y = "Cumulative P(success, 2cy)"
  ) +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face="bold"),
        panel.grid.minor = element_blank())


fig2 <- p2_lb + p2_eu +
  plot_annotation(
    title   = "Cumulative Success Probability by AMH and AFC (2-cycle product)",
    caption = paste0("Age fixed at ", typical_age,
                     ". Higher AMH and AFC = higher success probability."),
    theme   = theme(
      plot.title   = element_text(face="bold", size=13),
      plot.caption = element_text(color="grey50", size=8)
    )
  )


ggsave("outputs/fig2_cumulative_prob_by_amh_afc.png",
       fig2, width = 12, height = 5, dpi = 150)
message("Figure 2 saved: outputs/fig2_cumulative_prob_by_amh_afc.png")


# ── 5. FIGURE 3: HEATMAP -- 2-CYCLE CUMPROB BY AGE x AMH ──
# Shows profitability landscape: which patient profiles
# are high/low probability of success (and thus high/low
# premium and liability).


amh_heat <- seq(0.3, 10, length.out = 40)
age_heat  <- 28:43


df_heat <- expand.grid(
  age = age_heat,
  amh = amh_heat
) %>%
  mutate(
    afc       = typical_afc,
    p_lb      = mapply(p_cal_lb, age, amh, afc),
    p_eu      = mapply(p_cal_eu, age, amh, afc),
    cum_lb_2  = cum_success(p_lb, 2),
    cum_eu_2  = cum_success(p_eu, 2),
    # Failure probability (insurer's liability driver)
    fail_lb_2 = 1 - cum_lb_2,
    fail_eu_2 = 1 - cum_eu_2
  )


p3_lb <- ggplot(df_heat,
                aes(x = amh, y = age, fill = cum_lb_2)) +
  geom_tile() +
  scale_fill_gradient2(
    low      = "#D62728",   # red = low success (high insurer liability)
    mid      = "#FFFFBF",
    high     = "#1A9641",   # green = high success (low insurer liability)
    midpoint = 0.60,
    labels   = scales::percent_format(accuracy=1),
    name     = "P(success\n2cy)"
  ) +
  scale_x_continuous(breaks = c(0,2,4,6,8,10)) +
  scale_y_continuous(breaks = seq(32,43,2)) +
  labs(
    title    = "Live Birth 2-Cycle",
    subtitle = paste0("Green = profitable patients. Red = high liability. AFC=", typical_afc),
    x = "AMH (ng/mL)",
    y = "Age"
  ) +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face="bold"),
        panel.grid = element_blank())


p3_eu <- ggplot(df_heat,
                aes(x = amh, y = age, fill = cum_eu_2)) +
  geom_tile() +
  scale_fill_gradient2(
    low      = "#D62728",
    mid      = "#FFFFBF",
    high     = "#1A9641",
    midpoint = 0.65,
    labels   = scales::percent_format(accuracy=1),
    name     = "P(success\n2cy)"
  ) +
  scale_x_continuous(breaks = c(0,2,4,6,8,10)) +
  scale_y_continuous(breaks = seq(32,43,2)) +
  labs(
    title    = "Euploid 2-Cycle",
    subtitle = paste0("Green = profitable patients. Red = high liability. AFC=", typical_afc),
    x = "AMH (ng/mL)",
    y = "Age"
  ) +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face="bold"),
        panel.grid = element_blank())


fig3 <- p3_lb + p3_eu +
  plot_annotation(
    title   = "Patient Profile Heatmap: Cumulative P(Success) by Age and AMH",
    caption = paste0(
      "Red = low success probability = high insurer liability = patients who are most likely to claim.\n",
      "Green = high success probability = low insurer liability = most profitable patients.\n",
      "AFC fixed at ", typical_afc, ". Individual pricing accounts for AFC variation."
    ),
    theme   = theme(
      plot.title   = element_text(face="bold", size=13),
      plot.caption = element_text(color="grey50", size=8)
    )
  )


ggsave("outputs/fig3_heatmap_age_amh.png",
       fig3, width = 13, height = 6, dpi = 150)
message("Figure 3 saved: outputs/fig3_heatmap_age_amh.png")


# ── 6. FIGURE 4: INDIVIDUAL PATIENT PROFILES ───────────
# Shows how cumulative prob differs across 1/2/3 cycles
# for a set of specific named patient profiles.
# This is the closest analog to the patient-level
# calculator -- each profile gets its own curve.


profiles <- data.frame(
  label  = c(
    "Young / High reserve\n(age 30, AMH 4.0, AFC 18)",
    "Prime / Average\n(age 35, AMH 2.0, AFC 12)",
    "Mid / Below avg\n(age 38, AMH 1.0, AFC 8)",
    "Older / Low reserve\n(age 40, AMH 0.5, AFC 5)",
    "Older / Very low\n(age 42, AMH 0.3, AFC 3)"
  ),
  age    = c(30, 35, 38, 40, 42),
  amh    = c(4.0, 2.0, 1.0, 0.5, 0.3),
  afc    = c(18,  12,   8,   5,   3)
)


df_profiles <- profiles %>%
  rowwise() %>%
  mutate(
    p_lb   = p_cal_lb(age, amh, afc),
    p_eu   = p_cal_eu(age, amh, afc)
  ) %>%
  ungroup() %>%
  tidyr::crossing(cycles = 1:3) %>%
  mutate(
    cum_lb = cum_success(p_lb, cycles),
    cum_eu = cum_success(p_eu, cycles),
    cycle_label = factor(
      paste0(cycles, " cycle"),
      levels = c("1 cycle","2 cycle","3 cycle")
    )
  )


palette_prof <- c(
  "#2CA02C","#1F77B4","#FF7F0E","#D62728","#9467BD"
)


p4_lb <- ggplot(df_profiles,
                aes(x = cycles, y = cum_lb,
                    color = label, group = label)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  scale_color_manual(values = palette_prof, name = NULL) +
  scale_y_continuous(labels = scales::percent_format(accuracy=1),
                     limits = c(0, 1),
                     breaks = seq(0, 1, 0.1)) +
  scale_x_continuous(breaks = 1:3,
                     labels = c("1cy","2cy","3cy")) +
  geom_label(
    data = df_profiles %>% filter(cycles == 3),
    aes(label = scales::percent(cum_lb, accuracy=1)),
    hjust = -0.1, size = 3, show.legend = FALSE
  ) +
  labs(
    title    = "Live Birth Guarantee",
    subtitle = "Individual patient profiles -- cumulative P(success) by cycle count",
    x = "Number of cycles",
    y = "Cumulative P(success)"
  ) +
  coord_cartesian(xlim = c(0.8, 3.5)) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title    = element_text(face="bold"),
    legend.text   = element_text(size=8),
    panel.grid.minor = element_blank()
  )


p4_eu <- ggplot(df_profiles,
                aes(x = cycles, y = cum_eu,
                    color = label, group = label)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  scale_color_manual(values = palette_prof, name = NULL) +
  scale_y_continuous(labels = scales::percent_format(accuracy=1),
                     limits = c(0, 1),
                     breaks = seq(0, 1, 0.1)) +
  scale_x_continuous(breaks = 1:3,
                     labels = c("1cy","2cy","3cy")) +
  geom_label(
    data = df_profiles %>% filter(cycles == 3),
    aes(label = scales::percent(cum_eu, accuracy=1)),
    hjust = -0.1, size = 3, show.legend = FALSE
  ) +
  labs(
    title    = "Euploid Embryo Guarantee",
    subtitle = "Individual patient profiles -- cumulative P(success) by cycle count",
    x = "Number of cycles",
    y = "Cumulative P(success)"
  ) +
  coord_cartesian(xlim = c(0.8, 3.5)) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title    = element_text(face="bold"),
    legend.text   = element_text(size=8),
    panel.grid.minor = element_blank()
  )


fig4 <- p4_lb + p4_eu +
  plot_annotation(
    title   = "Individual Patient Profile Cumulative Success Probabilities",
    caption = (paste0(
      "Each line = one patient profile. Values at 3-cycle endpoint shown.",
      "\nThese are the p_cal values that drive individualized pricing.",
      "\nSource: Calibrated April 2026 logistic models.")
      ),
    theme   = theme(
      plot.title   = element_text(face="bold", size=13),
      plot.caption = element_text(color="grey50", size=8)
    )
  )


ggsave("outputs/fig4_individual_profile_cumprob.png",
       fig4, width = 14, height = 6, dpi = 150)
message("Figure 4 saved: outputs/fig4_individual_profile_cumprob.png")


# ── 7. SUMMARY TABLE ───────────────────────────────────
# Print profile-level summary for verification


message("\nIndividual profile summary:")
df_profiles %>%
  filter(cycles == 2) %>%
  dplyr::select(label, age, amh, afc, p_lb, p_eu, cum_lb, cum_eu) %>%
  mutate(
    across(c(p_lb, p_eu, cum_lb, cum_eu),
           ~round(., 4))
  ) %>%
  print(n=Inf)


message("\nAll 4 figures saved to outputs/ directory.")
message("Run order: fertility_data_prep.R -> fertility_master_v6_1.R -> this script")
