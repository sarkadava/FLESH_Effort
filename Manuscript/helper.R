library(brms)
library(here)
library(tidybayes)
library(dplyr)

# --- Extractors ---
fe <- function(model, param) {
  s <- fixef(model, probs = c(0.025, 0.975))[param, ]
  list(
    pct    = (exp(s["Estimate"]) - 1) * 100,
    pct_lo = (exp(s["Q2.5"])    - 1) * 100,
    pct_hi = (exp(s["Q97.5"])   - 1) * 100
  )
}

fe_contrast <- function(model, param, multiplier = 2) {
  s <- fixef(model, probs = c(0.025, 0.975))[param, ]
  list(
    pct    = (exp(multiplier * s["Estimate"]) - 1) * 100,
    pct_lo = (exp(multiplier * s["Q2.5"])    - 1) * 100,
    pct_hi = (exp(multiplier * s["Q97.5"])   - 1) * 100
  )
}

fe_2sd <- function(model, param) {
  est <- fixef(model)[param, "Estimate"]
  (exp(-4 * est) - 1) * 100
}

# Single re_sd returning ci() named vector
re_sd <- function(fit, group, param = "Intercept") {
  vc <- VarCorr(fit, summary = FALSE)[[group]]$sd
  ci(vc[, param])
}

# Update vpc to use ["med"] instead of $est
vpc <- function(model, groups) {
  sigma   <- posterior_summary(model)["sigma", "Estimate"]
  vars    <- sapply(groups, function(g) re_sd(model, g)["med"]^2)
  var_res <- sigma^2
  total   <- sum(vars) + var_res
  c(setNames(vars / total * 100, names(groups)), resid = var_res / total * 100)
}

re_cor <- function(fit, group, param1 = "Intercept", param2) {
  vc <- VarCorr(fit, summary = FALSE)[[group]]$cor
  ci(vc[, param1, param2])
}

fe_derived_contrast <- function(model, param1, param2, w1 = 1, w2 = 1,
                                probs = c(0.025, 0.975)) {
  s <- fixef(model, probs = probs)
  est <- w1 * s[param1, "Estimate"] + w2 * s[param2, "Estimate"]
  lo  <- w1 * s[param1, "Q2.5"]    + w2 * s[param2, "Q2.5"]
  hi  <- w1 * s[param1, "Q97.5"]   + w2 * s[param2, "Q97.5"]
  list(
    pct    = (exp(est) - 1) * 100,
    pct_lo = (exp(lo)  - 1) * 100,
    pct_hi = (exp(hi)  - 1) * 100
  )
}

# --- Formatters ---
pct     <- function(x)      sprintf("%.1f%%", abs(x))
cri_pct <- function(lo, hi) sprintf("[%.1f%%, %.1f%%]", lo, hi)

fmt_pct <- function(m, lo, hi, d = 3) {
  fmt <- function(x) paste0(formatC(round(x * 100, d), format = "f", digits = d), "%")
  paste0(fmt(m), " (95% CrI: [", fmt(lo), ", ", fmt(hi), "])")
}

fmt_sd  <- function(x)      sprintf("%.1f", x)
cri_sd  <- function(lo, hi) sprintf("[%.1f, %.1f]", lo, hi)
fmt_cor <- function(x)      sprintf("%.1f", x)
cri_cor <- function(lo, hi) sprintf("[%.1f, %.1f]", lo, hi)


fmt_b <- function(m, lo, hi, d = 2) {
  s <- function(x) ifelse(x >= 0, sprintf("+%.*f", d, x), sprintf("%.*f", d, x))
  paste0("\u03B2 = ", s(m), " (95% CrI: [", s(lo), ", ", s(hi), "])")
}

fmt_p <- function(m, lo, hi, d = 2) {
  fmt <- function(x) formatC(round(x, d), format = "f", digits = d)
  paste0(fmt(m), " (95% CrI: [", fmt(lo), ", ", fmt(hi), "])")
}

fmt_fe <- function(fit, param, d = 2) {
  x <- ci(fixef(fit, summary = FALSE)[, param])
  fmt_b(x["med"], x["lo"], x["hi"], d)
}


# Productivity

fmt_dp <- function(x, d=2) ifelse(x>=0, sprintf("+%.*f",d,x), sprintf("%.*f",d,x))
ci     <- function(draws) setNames(quantile(draws, c(.5,.025,.975)), c("med","lo","hi"))

make_nd_base <- function(fit, extra_zero_vars = NULL, extra_factor_refs = NULL) {
  nd <- fit$data |>
    group_by(modality) |>
    slice(1) |>
    ungroup() |>
    mutate(across(c(arm_torque_log_c, envelope_log_c, copc_log_c,
                    expressibility_z, TrialNumber_c,
                    all_of(extra_zero_vars)), \(x) 0),
           Familiarity = mean(fit$data$Familiarity),
           BFI_extra   = mean(fit$data$BFI_extra))
  for (nm in names(extra_factor_refs)) {
    nd[[nm]] <- extra_factor_refs[[nm]]
  }
  nd
}

make_nd_base_in <- function(fit, extra_zero_vars = NULL, extra_factor_refs = NULL) {
  nd <- fit$data |>
    group_by(modality) |>
    slice(1) |>
    ungroup() |>
    mutate(across(c(arm_torque_in_log_c, envelope_in_log_c, copc_in_log_c,
                    expressibility_z, TrialNumber_c,
                    all_of(extra_zero_vars)), \(x) 0),
           Familiarity = mean(fit$data$Familiarity),
           BFI_extra   = mean(fit$data$BFI_extra))
  for (nm in names(extra_factor_refs)) {
    nd[[nm]] <- extra_factor_refs[[nm]]
  }
  nd
}

pred_at <- function(fit, nd_base, modality_lev, effort_var, sd_val) {
  nd <- nd_base |> filter(modality == modality_lev) |>
    mutate(!!effort_var := sd_val * sd(fit$data[[effort_var]], na.rm = TRUE))
  posterior_epred(fit, newdata = nd, re_formula = NA)[, 1]
}

productivity <- function(fit, nd_base, modality_lev, effort_var) {
  hi <- pred_at(fit, nd_base, modality_lev, effort_var,  1)
  lo <- pred_at(fit, nd_base, modality_lev, effort_var, -1)
  ci(hi - lo)
}


# Ordinal posterior predictions: returns draws vector for one category
# epred_ord: result of posterior_epred on ordinal model (draws x nrow x ncats)
# row_idx: which newdata row (modality index)
# cat_idx: which response category (1=c0, 2=c1, 3=c2, 4=never)
ord_cat <- function(epred_ord, row_idx, cat_idx) {
  epred_ord[, row_idx, cat_idx]
}

# Productivity for ordinal: ΔP for a given category
productivity_ord <- function(fit, nd_base, modality_lev, effort_var, cat_idx) {
  make_nd <- function(sd_val) {
    nd_base |> filter(modality == modality_lev) |>
      mutate(!!effort_var := sd_val * sd(fit$data[[effort_var]], na.rm = TRUE))
  }
  hi <- posterior_epred(fit, newdata = make_nd( 1), re_formula = NA)[, 1, cat_idx]
  lo <- posterior_epred(fit, newdata = make_nd(-1), re_formula = NA)[, 1, cat_idx]
  ci(hi - lo)
}

# Add to helper.R



# New: implicit level SD from sum coding
# Computes posterior SD of -(modality1 + modality2) random effects per group
re_sd_implicit_sum <- function(fit, group) {
  draws_df  <- brms::as_draws_df(fit)
  
  # Find columns for this group's modality random effects
  pat1 <- paste0("^r_", group, "\\[.*,modality1\\]$")
  pat2 <- paste0("^r_", group, "\\[.*,modality2\\]$")
  
  cols1 <- grep(pat1, names(draws_df), value = TRUE)
  cols2 <- grep(pat2, names(draws_df), value = TRUE)
  
  if (length(cols1) == 0 || length(cols2) == 0)
    stop("Could not find modality1/modality2 random effects for group: ", group)
  
  # Per draw: compute implicit = -(mod1 + mod2) for each unit, then SD across units
  implicit_sd <- purrr::map_dbl(seq_len(nrow(draws_df)), function(i) {
    mod1 <- as.numeric(draws_df[i, cols1])
    mod2 <- as.numeric(draws_df[i, cols2])
    sd(-(mod1 + mod2))
  })
  
  ci(implicit_sd)
}

re_cor_implicit_sum <- function(fit, group, param1 = "Intercept") {
  draws_df <- brms::as_draws_df(fit)
  
  pat_int <- paste0("^r_", group, "\\[.*,", param1, "\\]$")
  pat1    <- paste0("^r_", group, "\\[.*,modality1\\]$")
  pat2    <- paste0("^r_", group, "\\[.*,modality2\\]$")
  
  cols_int <- grep(pat_int, names(draws_df), value = TRUE)
  cols1    <- grep(pat1,    names(draws_df), value = TRUE)
  cols2    <- grep(pat2,    names(draws_df), value = TRUE)
  
  if (length(cols_int) == 0 || length(cols1) == 0 || length(cols2) == 0)
    stop("Could not find required random effects for group: ", group)
  
  cor_draws <- purrr::map_dbl(seq_len(nrow(draws_df)), function(i) {
    intercepts <- as.numeric(draws_df[i, cols_int])
    implicit   <- -(as.numeric(draws_df[i, cols1]) + as.numeric(draws_df[i, cols2]))
    cor(intercepts, implicit)
  })
  
  ci(cor_draws)
}

# ── Helper: predicted % change at ±N SD of baseline ───────────────────────────
# Assumes model has: correction2M1 + baseline_z + correction2M1:baseline_z
fe_at_baseline <- function(model, correction_param, interaction_param, sd_val) {
  fe  <- fixef(model, summary = FALSE)
  log_change <- fe[, correction_param] + sd_val * fe[, interaction_param]
  x   <- ci(log_change)
  list(
    pct    = (exp(x["med"]) - 1) * 100,
    pct_lo = (exp(x["lo"])  - 1) * 100,
    pct_hi = (exp(x["hi"])  - 1) * 100
  )
}

# Existing re_cor (assumed — adjust if yours differs)
re_cor <- function(fit, group, param1, param2) {
  vc <- VarCorr(fit, summary = FALSE)[[group]]$cor
  ci(vc[, param1, param2])
}

fmt_fe_pct <- function(x) {
  paste0("\u03B2 = ", sprintf("%.1f%%", x$pct),
         " (95% CrI: [", sprintf("%.1f%%", x$pct_lo),
         ", ", sprintf("%.1f%%", x$pct_hi), "])")
}

re_sd_implicit_two <- function(fit, group, param = "modality1") {
  vc <- VarCorr(fit, summary = FALSE)[[group]]$sd
  ci(vc[, param])  # SD is symmetric, negating doesn't change it
}