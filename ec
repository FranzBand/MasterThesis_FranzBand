library(broom)
library(ggplot2)
library(dplyr)

# ============================================================
# PART 1 — Summary of ECM coefficients
# ============================================================
summary(step2_ecm)

# Clean tidy table of all coefficients
ecm_coefs <- tidy(step2_ecm) %>%
  mutate(
    significant = ifelse(p.value < 0.05, "Yes", "No"),
    term_short  = case_when(
      grepl("error_va", term) & !grepl("sector", term) ~ "ΔError VA (main)",
      grepl("error_k",  term) & !grepl("sector", term) ~ "ΔError K (main)",
      grepl("error_va", term) &  grepl("sector", term) ~ paste0("ΔError VA × ", gsub(".*sectorsbi08(\\d+).*", "Sector \\1", term)),
      grepl("error_k",  term) &  grepl("sector", term) ~ paste0("ΔError K × ",  gsub(".*sectorsbi08(\\d+).*", "Sector \\1", term)),
      term == "(Intercept)" ~ "Intercept",
      TRUE ~ term
    )
  )

print(ecm_coefs, n = Inf)

# ============================================================
# PART 2 — Speed of adjustment (gamma)
# ============================================================
# Not directly in step2_ecm — it comes from ECM on dlnl if you added it
# If you have it, extract like this:
if ("ec_term" %in% names(coef(step2_ecm))) {
  gamma <- coef(step2_ecm)["ec_term"]
  cat("\nSpeed of adjustment (gamma):", round(gamma, 4), "\n")
  cat("Half-life of shock (years):", round(log(0.5) / log(1 + gamma), 2), "\n")
}

# ============================================================
# PART 3 — Plot main ECM coefficients (sector interactions)
# ============================================================

# Extract sector-specific error_va coefficients
coef_va <- ecm_coefs %>%
  filter(grepl("error_va", term)) %>%
  mutate(
    sector = ifelse(
      grepl("sectorsbi08", term),
      as.integer(gsub(".*sectorsbi08(\\d+).*", "\\1", term)),
      0   # 0 = main effect
    ),
    label = ifelse(sector == 0, "Main effect", paste0("Sector ", sector))
  )

ggplot(coef_va, aes(x = reorder(label, estimate), y = estimate,
                    fill = significant)) +
  geom_col() +
  geom_errorbar(aes(ymin = estimate - 1.96 * std.error,
                    ymax = estimate + 1.96 * std.error),
                width = 0.3) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
  coord_flip() +
  scale_fill_manual(values = c("Yes" = "steelblue", "No" = "grey70")) +
  labs(
    title = "ECM Coefficients — ΔError VA by Sector",
    x = NULL, y = "Coefficient estimate",
    fill = "Significant (p<0.05)"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

# Same for error_k
coef_k <- ecm_coefs %>%
  filter(grepl("error_k", term)) %>%
  mutate(
    sector = ifelse(
      grepl("sectorsbi08", term),
      as.integer(gsub(".*sectorsbi08(\\d+).*", "\\1", term)),
      0
    ),
    label = ifelse(sector == 0, "Main effect", paste0("Sector ", sector))
  )

ggplot(coef_k, aes(x = reorder(label, estimate), y = estimate,
                   fill = significant)) +
  geom_col() +
  geom_errorbar(aes(ymin = estimate - 1.96 * std.error,
                    ymax = estimate + 1.96 * std.error),
                width = 0.3) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
  coord_flip() +
  scale_fill_manual(values = c("Yes" = "steelblue", "No" = "grey70")) +
  labs(
    title = "ECM Coefficients — ΔError K by Sector",
    x = NULL, y = "Coefficient estimate",
    fill = "Significant (p<0.05)"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

# ============================================================
# PART 4 — Short-run dynamics: fitted vs actual in training period
# ============================================================
df_train$err_y_hat_raw <- predict(step2_ecm, newdata = df_train)

df_dynamics <- df_train %>%
  group_by(jaar) %>%
  summarise(
    actual_error    = mean(error_y,       na.rm = TRUE),
    fitted_error    = mean(err_y_hat_raw, na.rm = TRUE),
    residual        = mean(error_y - err_y_hat_raw, na.rm = TRUE),
    .groups = "drop"
  )

ggplot(df_dynamics, aes(x = jaar)) +
  geom_line(aes(y = actual_error, color = "Actual error_y"),  linewidth = 1) +
  geom_line(aes(y = fitted_error, color = "ECM fitted"),
            linewidth = 1, linetype = "dashed") +
  geom_col(aes(y = residual, fill = "Residual"), alpha = 0.3, width = 0.5) +
  scale_color_manual(values = c("Actual error_y" = "black",
                                "ECM fitted"     = "steelblue")) +
  scale_fill_manual(values  = c("Residual" = "firebrick")) +
  scale_x_continuous(breaks = data_start:data_end) +
  labs(
    title  = "ECM Short-run Dynamics (training period)",
    subtitle = "Average across all sector-occupation cells per year",
    x      = "Year",
    y      = "Equilibrium error",
    color  = "", fill  = ""
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

# ============================================================
# PART 5 — R-squared and model fit diagnostics
# ============================================================
cat("\n=== ECM Model Diagnostics ===\n")
cat("R-squared:        ", round(summary(step2_ecm)$r.squared,      4), "\n")
cat("Adj. R-squared:   ", round(summary(step2_ecm)$adj.r.squared,  4), "\n")
cat("Residual Std Err: ", round(summary(step2_ecm)$sigma,           4), "\n")
cat("N observations:   ", nobs(step2_ecm), "\n")
