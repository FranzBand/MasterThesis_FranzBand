# ============================================================
# STEP 0 — Pre-compute all variables ONCE before the loop
# ============================================================
library(dplyr)

# Lead/lag column names (adjust to match your actual column names)
ll_vars_va <- names(df)[grepl("^lnva_", names(df))]
ll_vars_k  <- names(df)[grepl("^lnk_",  names(df))]

# DOLS and ECM formulas
dols_formula_base <- paste(
  "lnl ~ lnva + lnk "#+,
  # paste(ll_vars_va, collapse = " + "),
  # "+",
  # paste(ll_vars_k,  collapse = " + ")
)

ecm_formula <- as.formula("dlnl ~ ec_lag + dlnl_lag + dlnva + dlnk")

# Pre-compute differenced variables and initialise new columns
df <- df %>%
  arrange(code, jaar) %>%
  group_by(code) %>%
  mutate(
    dlnl     = lnl  - lag(lnl,  1),
    dlnva    = lnva - lag(lnva, 1),
    dlnk     = lnk  - lag(lnk,  1),
    dlnl_lag = lag(dlnl, 1)
  ) %>%
  ungroup() %>%
  mutate(
    ec_resid   = NA_real_,
    lnl_hat_lr = NA_real_,
    dlnl_hat   = NA_real_
  )

ecm_vars     <- c("dlnl", "ec_lag", "dlnl_lag", "dlnva", "dlnk")
df_pred_all  <- data.frame()
segments     <- unique(df$segment)

# ============================================================
# OUTER LOOP — over segments
# ============================================================
for (seg in segments) {
  
  df_seg       <- df %>% filter(segment == seg)
  df_pred_seg  <- data.frame()
  temp_sectors <- unique(df_seg$sector)
  
  # ==========================================================
  # MIDDLE LOOP — over sectors within segment
  # ==========================================================
  for (sec in temp_sectors) {
    
    df_sec   <- df_seg %>% filter(sector == sec)
    df_train <- df_sec %>% filter(!is.na(lnl))
    df_fc    <- df_sec %>% filter(is.na(lnl))
    fc_years <- sort(unique(df_fc$jaar))
    
    # --- Guard: skip if data too sparse or any core var all-NA
    # if (nrow(df_train) < 10       ||
    #     length(fc_years) == 0     ||
    #     all(is.na(df_train$lnva)) ||
    #     all(is.na(df_train$lnk))) {
    #   cat("SKIP (sparse) | seg:", seg, "| sec:", sec, "\n")
    #   rm(df_sec, df_train, df_fc, fc_years)
    #   next
    # }
    
    # --- Step 1: DOLS — dynamic formula if only 1 code ------
    n_codes <- length(unique(df_train$code))
    dols_formula_dyn <- if (n_codes > 1) {
      as.formula(paste(dols_formula_base, "+ factor(code)"))
    } else {
      as.formula(dols_formula_base)
    }
    
    lm_dols <- tryCatch(
      lm(dols_formula_dyn, data = df_train, na.action = na.exclude),
      error = function(e) {
        cat("DOLS FAILED | seg:", seg, "| sec:", sec,
            "|", e$message, "\n")
        NULL
      }
    )
    
    if (is.null(lm_dols)) {
      rm(df_sec, df_train, df_fc, fc_years, n_codes, dols_formula_dyn)
      next
    }
    
    # --- Step 2: ECM residuals + lag -----------------------
    df_train <- df_train %>%
      mutate(ec_resid = residuals(lm_dols)) %>%
      arrange(code, jaar) %>%
      group_by(code) %>%
      mutate(ec_lag = lag(ec_resid, 1)) %>%
      ungroup()
    
    df_ecm_train <- df_train %>%
      filter(complete.cases(across(all_of(ecm_vars))))
    
    cat("ECM rows | seg:", seg, "| sec:", sec,
        "| n =", nrow(df_ecm_train), "\n")
    
    # --- Guard: skip if ECM train too sparse ---------------
    if (nrow(df_ecm_train) < 5) {
      cat("SKIP (ECM sparse) | seg:", seg, "| sec:", sec, "\n")
      rm(df_sec, df_train, df_fc, fc_years, n_codes,
         dols_formula_dyn, df_ecm_train, lm_dols)
      next
    }
    
    # --- Step 3: ECM fit -----------------------------------
    lm_ecm <- tryCatch(
      lm(ecm_formula, data = df_ecm_train, na.action = na.exclude),
      error = function(e) {
        cat("ECM FAILED | seg:", seg, "| sec:", sec,
            "|", e$message, "\n")
        NULL
      }
    )
    
    if (is.null(lm_ecm)) {
      rm(df_sec, df_train, df_fc, fc_years, n_codes,
         dols_formula_dyn, df_ecm_train, lm_dols)
      next
    }
    
    # --- Write training ec_resid back into working df ------
    df_sec_working <- df_sec %>%
      rows_update(
        df_train %>% select(code, jaar, ec_resid),
        by = c("code", "jaar")
      )
    
    # ========================================================
    # INNER LOOP — over forecast years (one-step-ahead)
    # ========================================================
    for (yr in fc_years) {
      
      prev_rows  <- df_sec_working %>% filter(jaar == yr - 1, !is.na(lnl))
      prev2_rows <- df_sec_working %>% filter(jaar == yr - 2, !is.na(lnl))
      curr_rows  <- df_sec_working %>% filter(jaar == yr)
      
      if (nrow(prev_rows) == 0 || nrow(prev2_rows) == 0) {
        cat("SKIP (no anchor) | seg:", seg, "| sec:", sec,
            "| yr:", yr, "\n")
        rm(prev_rows, prev2_rows, curr_rows)
        next
      }
      
      curr_rows <- curr_rows %>%
        left_join(
          prev_rows %>%
            select(code, lnl, lnva, lnk, ec_resid) %>%
            rename(lnl_prev  = lnl,  lnva_prev = lnva,
                   lnk_prev  = lnk,  ec_lag    = ec_resid),
          by = "code"
        ) %>%
        left_join(
          prev2_rows %>%
            select(code, lnl) %>% rename(lnl_prev2 = lnl),
          by = "code"
        ) %>%
        mutate(
          dlnl_lag   = lnl_prev  - lnl_prev2,
          dlnva      = lnva      - lnva_prev,
          dlnk       = lnk       - lnk_prev,
          lnl_hat_lr = predict(lm_dols, newdata = .),
          dlnl_hat   = predict(lm_ecm,  newdata = .),
          lnl        = lnl_prev  + dlnl_hat,
          ec_resid   = lnl - lnl_hat_lr
        )
      
      df_sec_working <- df_sec_working %>%
        rows_update(
          curr_rows %>%
            select(code, jaar, lnl, lnl_hat_lr, dlnl_hat, ec_resid),
          by = c("code", "jaar")
        )
      
      rm(prev_rows, prev2_rows, curr_rows)
      
    } # end inner loop
    
    df_pred_seg <- bind_rows(
      df_pred_seg,
      df_sec_working %>% filter(jaar %in% fc_years)
    )
    
    rm(df_sec, df_train, df_fc, fc_years, n_codes,
       dols_formula_dyn, df_ecm_train, lm_dols, lm_ecm,
       df_sec_working)
    
  } # end middle loop
  
  df_pred_all <- bind_rows(df_pred_all, df_pred_seg)
  rm(df_seg, df_pred_seg, temp_sectors)
  
} # end outer loop