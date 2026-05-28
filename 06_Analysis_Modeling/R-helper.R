
modality_colors <- c(
  "gesture"    = "#2196F3",
  "vocal"      = "#FF9800",
  "multimodal" = "#4CAF50"
)

theme_effort_plot <- function(base_size = 16) {
  theme_minimal(base_size = base_size) +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(colour = "grey90", linewidth = 0.4),
      axis.ticks = element_line(colour = "grey40", linewidth = 0.5),
      axis.ticks.length = unit(4, "pt"),
      axis.text.x = element_text(size = 15, face = "bold", colour = "grey20"),
      axis.text.y = element_text(size = 13, colour = "grey20"),
      axis.title.x = element_text(size = 15, margin = margin(t = 8)),
      axis.title.y = element_text(size = 15, margin = margin(r = 8)),
      legend.position = "top",
      legend.title = element_blank(),
      legend.text = element_text(size = 14),
      legend.key.size = unit(12, "pt"),
      legend.margin = margin(b = 2),
      plot.margin = margin(8, 12, 8, 8),
      plot.title = element_text(size = 16, face = "bold"),
      plot.subtitle = element_text(size = 12, colour = "grey40")
    )
}


.make_noise_flag <- function(channel_vec, modality_vec, noise_channels) {
  purrr::map2_lgl(
    as.character(channel_vec),
    as.character(modality_vec),
    function(ch, mod) {
      if (ch %in% names(noise_channels)) noise_channels[[ch]] == mod
      else FALSE
    }
  )
}

.make_colour_scale <- function(modality_lvls, nc_colour = "grey70", ...) {
  vals <- c(
    setNames(purrr::map_chr(modality_lvls, ~ modality_colors[[.x]]),
             modality_lvls),
    setNames(rep(nc_colour, length(modality_lvls)),
             paste0(modality_lvls, "_nc"))
  )
  scale_colour_manual(values = vals, ...)
}

.make_fill_scale <- function(modality_lvls, nc_colour = "grey70", ...) {
  vals <- c(
    setNames(purrr::map_chr(modality_lvls, ~ modality_colors[[.x]]),
             modality_lvls),
    setNames(rep(nc_colour, length(modality_lvls)),
             paste0(modality_lvls, "_nc"))
  )
  scale_fill_manual(values = vals, ...)
}

# ══════════════════════════════════════════════════════════════════════════════
#  report_effort_success()
#
#  Unified report for a brms logistic model predicting P(resolved at c0)
#  from effort × modality. Produces three panels:
#
#  A) Effort→success slopes (log-odds scale, via emtrends)
#  B) Predicted P(resolved) at −1SD / Mean / +1SD per channel × modality
#  C) Productivity summary: ΔP(resolved) per SD effort per modality
#
#  Arguments
#  ─────────────────────────────────────────────────────────────────────────────
#  model         Fitted brmsfit (Bernoulli logistic).
#
#  data          Data frame used to fit the model (c0 trials).
#
#  channels      Named list; each element: list(col = "...", label = "...")
#
#  covariates    Named list of columns to hold at fixed values (default = 0).
#
#  modality_col  Name of modality column (default "modality").
#
#  modality_lvls Modality levels to include.
#
#  noise_channels
#                Named list: list("Channel Label" = "modality_value")
#                defining which channel × modality combinations are noise.
#
#  model_label   String used in titles and saved filename.
#
#  output_file   PNG path; NULL to skip saving.
#
#  Globals assumed: modality_colors, theme_effort_plot()
# ══════════════════════════════════════════════════════════════════════════════

report_effort_success <- function(
    model,
    data,
    channels = list(
      arm = list(col = "arm_torque_log_c", label = "Arm Torque"),
      env = list(col = "envelope_log_c",   label = "Envelope"),
      cop = list(col = "copc_log_c",       label = "COP")
    ),
    covariates = list(
      expressibility_z = 0,
      Familiarity      = 0,
      BFI_extra        = 0,
      TrialNumber_c    = 0
    ),
    modality_col   = "modality",
    modality_lvls  = c("gesture", "multimodal", "vocal"),
    noise_channels = list(
      "Envelope"   = "gesture",
      "Arm Torque" = "vocal"
    ),
    model_label = deparse(substitute(model)),
    output_file = paste0("effort_success_", model_label, ".png")
) {
  
  channel_cols   <- purrr::map_chr(channels, "col")
  channel_labels <- purrr::map_chr(channels, "label")
  
  # ── 1. Console: SDs and model summary ───────────────────────────────────────
  cat("═══════════════════════════════════════════════\n")
  cat(sprintf("  REPORT: %s\n", model_label))
  cat("  Outcome: P(resolved at c0) | Bernoulli logit\n")
  cat("═══════════════════════════════════════════════\n\n")
  
  channel_sds <- purrr::map_dbl(channel_cols, ~ sd(data[[.x]], na.rm = TRUE))
  names(channel_sds) <- channel_cols
  
  cat("  Effort variable SDs (anchor scale):\n")
  purrr::walk2(channel_labels, channel_sds,
               ~ cat(sprintf("    %-20s SD = %.3f\n", .x, .y)))
  cat("\n")
  
  cat("  Fixed effects (log-odds scale):\n")
  fe <- fixef(model) |>
    as.data.frame() |>
    rownames_to_column("term") |>
    mutate(credible = (Q2.5 > 0 | Q97.5 < 0))
  print(fe)
  cat("\n")
  
  # ── 2. Slopes via emtrends (log-odds scale) ──────────────────────────────────
  cat("  Effort→success slopes by channel × modality (log-odds / SD):\n\n")
  
  all_slopes <- purrr::map2_dfr(channel_cols, channel_labels, function(col, label) {
    emtrends(model, ~ modality, var = col) |>
      as.data.frame() |>
      dplyr::rename(
        estimate = paste0(col, ".trend"),
        lower    = lower.HPD,
        upper    = upper.HPD
      ) |>
      dplyr::mutate(channel = label)
  }) |>
    dplyr::rename(modality = !!modality_col) |>
    dplyr::mutate(
      channel  = factor(channel, levels = channel_labels),
      modality = factor(modality, levels = modality_lvls),
      credible = lower > 0 | upper < 0,
      noise    = purrr::map2_lgl(
        as.character(channel), as.character(modality),
        function(ch, mod) {
          if (ch %in% names(noise_channels)) noise_channels[[ch]] == mod
          else FALSE
        }
      ),
      colour_var = dplyr::case_when(
        noise     ~ paste0(as.character(modality), "_nc"),
        !credible ~ paste0(as.character(modality), "_nc"),
        TRUE      ~ as.character(modality)
      )
    )
  
  print(all_slopes |> dplyr::select(channel, modality, estimate, lower, upper, credible, noise))
  cat("\n")
  
  # ── 3. Predicted P(resolved) at ±1SD anchors ─────────────────────────────────
  cat("  Computing predicted P(resolved) at ±1 SD anchors...\n")
  
  make_preds <- function(ch_col, anchors, ch_label) {
    zero_cols <- setNames(rep(0, length(channel_cols)), channel_cols)
    
    base <- data |>
      modelr::data_grid(!!modality_col := modality_lvls, .model = model)
    for (col in names(zero_cols))  base[[col]] <- zero_cols[[col]]
    for (col in names(covariates)) base[[col]] <- covariates[[col]]
    
    base |>
      tidyr::crossing(effort_anchor = anchors) |>
      dplyr::mutate(!!ch_col := effort_anchor) |>
      tidybayes::add_epred_draws(model, re_formula = NA) |>
      dplyr::group_by(!!sym(modality_col), effort_anchor) |>
      dplyr::summarise(
        p_resolved = median(.epred),
        lower      = quantile(.epred, 0.025),
        upper      = quantile(.epred, 0.975),
        .groups    = "drop"
      ) |>
      dplyr::mutate(channel = ch_label)
  }
  
  all_preds <- purrr::pmap_dfr(
    list(
      ch_col   = channel_cols,
      anchors  = purrr::map(channel_sds, ~ c(-.x, 0, .x)),
      ch_label = channel_labels
    ),
    make_preds
  ) |>
    dplyr::rename(modality = !!modality_col) |>
    dplyr::mutate(
      channel  = factor(channel, levels = channel_labels),
      modality = factor(modality, levels = modality_lvls),
      effort_label = factor(
        dplyr::case_when(
          effort_anchor < -1e-10 ~ "−1 SD",
          effort_anchor >  1e-10 ~ "+1 SD",
          TRUE                   ~ "Mean"
        ),
        levels = c("−1 SD", "Mean", "+1 SD")
      ),
      noise = purrr::map2_lgl(
        as.character(channel), as.character(modality),
        function(ch, mod) {
          if (ch %in% names(noise_channels)) noise_channels[[ch]] == mod
          else FALSE
        }
      ),
      colour_var = ifelse(noise,
                          paste0(as.character(modality), "_nc"),
                          as.character(modality))
    )
  
  # ── 4. Productivity: ΔP(resolved) between −1SD and +1SD ─────────────────────
  productivity <- all_preds |>
    dplyr::filter(effort_label != "Mean") |>
    dplyr::select(channel, modality, effort_label, p_resolved) |>
    dplyr::group_by(channel, modality) |>
    dplyr::summarise(
      delta_p = p_resolved[effort_label == "+1 SD"] - 
        p_resolved[effort_label == "\u22121 SD"],
      .groups = "drop"
    )
  # ── 5. Colour scale helper ───────────────────────────────────────────────────
  colour_scale <- scale_colour_manual(
    values = c(
      setNames(purrr::map_chr(modality_lvls, ~ modality_colors[[.x]]),
               modality_lvls),
      setNames(rep("grey70", length(modality_lvls)),
               paste0(modality_lvls, "_nc"))
    ),
    guide = "none"
  )
  
  # ── Formatted numerical summary for reporting ─────────────────────────────────
  cat("\n═══════════════════════════════════════════════\n")
  cat("  NUMERICAL SUMMARY FOR REPORTING\n")
  cat("═══════════════════════════════════════════════\n\n")
  
  cat("  Slopes (log-odds per SD effort | 95% HPD):\n")
  all_slopes |>
    dplyr::arrange(channel, modality) |>
    dplyr::mutate(
      cred_flag = ifelse(credible, " *", ""),
      line = sprintf("    %-12s × %-12s  b = %+.3f [%+.3f, %+.3f]%s",
                     as.character(channel), as.character(modality),
                     estimate, lower, upper, cred_flag)
    ) |>
    dplyr::pull(line) |>
    cat(sep = "\n")
  
  cat("\n\n  Predicted P(resolved) at effort anchors (95% CI):\n")
  all_preds |>
    dplyr::arrange(channel, modality, effort_label) |>
    dplyr::mutate(
      line = sprintf("    %-12s × %-12s  %-8s  P = %.3f [%.3f, %.3f]",
                     as.character(channel), as.character(modality),
                     as.character(effort_label),
                     p_resolved, lower, upper)
    ) |>
    dplyr::pull(line) |>
    cat(sep = "\n")
  
  cat("\n\n  Productivity \u0394P (+1SD \u2212 \u22121SD):\n")
  productivity |>
    dplyr::arrange(channel, modality) |>
    dplyr::mutate(
      line = sprintf("    %-12s × %-12s  \u0394P = %+.3f",
                     as.character(channel), as.character(modality), delta_p)
    ) |>
    dplyr::pull(line) |>
    cat(sep = "\n")
  cat("\n\n")
  
  # ── Plot A: slopes (log-odds) ────────────────────────────────────────────────
  p_slopes <- all_slopes |>
    ggplot(aes(x = modality, y = estimate,
               colour = colour_var, shape = noise)) +
    geom_hline(yintercept = 0, linetype = "dashed",
               colour = "grey30", linewidth = 0.8) +
    geom_pointrange(aes(ymin = lower, ymax = upper),
                    linewidth = 1.1, size = 0.9) +
    colour_scale +
    scale_shape_manual(
      values = c("FALSE" = 16, "TRUE" = 4),
      labels = c("FALSE" = "Active channel", "TRUE" = "Noise channel"),
      name   = NULL
    ) +
    facet_wrap(~ channel, nrow = 1) +
    theme_effort_plot() +
    theme(
      legend.position = "bottom",
      strip.text      = element_text(size = 11, face = "bold"),
      axis.text.x     = element_text(size = 8)
    ) +
    labs(
      title    = "Effort \u2192 success slope by channel and modality",
      subtitle = "Log-odds change in P(resolved) per SD effort | 95% HPD | Grey = not credible or noise",
      x = NULL, y = "Slope (log-odds per SD effort)"
    )
  
  # ── Plot B: predicted P(resolved) at anchors ─────────────────────────────────
  p_preds <- all_preds |>
    ggplot(aes(x = effort_label, y = p_resolved,
               colour = colour_var, group = modality)) +
    geom_line(linewidth = 0.8, linetype = "dashed", alpha = 0.5) +
    geom_pointrange(aes(ymin = lower, ymax = upper),
                    size = 0.7, linewidth = 1) +
    colour_scale +
    ggnewscale::new_scale_colour() +
    geom_point(aes(colour = modality), size = 0) +
    scale_colour_manual(values = modality_colors, name = "Modality") +
    facet_wrap(~ channel, nrow = 1) +
    theme_effort_plot() +
    theme(
      legend.position = "bottom",
      strip.text      = element_text(size = 11, face = "bold"),
      axis.text.x     = element_text(size = 9)
    ) +
    labs(
      title    = "Predicted P(resolved) at effort anchors",
      subtitle = "95% CI | Other predictors at 0 | Grey = noise channel",
      x = "Effort level", y = "P(resolved at c0)"
    )
  
  # ── Plot C: productivity (ΔP) ────────────────────────────────────────────────
  p_prod <- productivity |>
    dplyr::mutate(
      modality   = factor(modality, levels = modality_lvls),
      noise      = purrr::map2_lgl(
        as.character(channel), as.character(modality),
        function(ch, mod) {
          if (ch %in% names(noise_channels)) noise_channels[[ch]] == mod
          else FALSE
        }
      ),
      colour_var = ifelse(noise,
                          paste0(as.character(modality), "_nc"),
                          as.character(modality))
    ) |>
    ggplot(aes(x = modality, y = delta_p,
               fill = colour_var, colour = colour_var)) +
    geom_col(width = 0.6) +
    geom_hline(yintercept = 0, linetype = "dashed",
               colour = "grey30", linewidth = 0.8) +
    scale_fill_manual(
      values = c(
        setNames(purrr::map_chr(modality_lvls, ~ modality_colors[[.x]]),
                 modality_lvls),
        setNames(rep("grey70", length(modality_lvls)),
                 paste0(modality_lvls, "_nc"))
      ),
      guide = "none"
    ) +
    scale_colour_manual(
      values = c(
        setNames(purrr::map_chr(modality_lvls, ~ modality_colors[[.x]]),
                 modality_lvls),
        setNames(rep("grey70", length(modality_lvls)),
                 paste0(modality_lvls, "_nc"))
      ),
      guide = "none"
    ) +
    facet_wrap(~ channel, nrow = 1) +
    theme_effort_plot() +
    theme(
      strip.text  = element_text(size = 11, face = "bold"),
      axis.text.x = element_text(size = 8)
    ) +
    labs(
      title    = "Effort productivity: \u0394P(resolved) per SD effort",
      subtitle = "P(resolved | +1 SD) \u2212 P(resolved | \u22121 SD) | Grey = noise channel",
      x = NULL, y = "\u0394P(resolved)"
    )
  
  # ── Combine ──────────────────────────────────────────────────────────────────
  final <- (p_slopes / p_preds / p_prod) +
    patchwork::plot_annotation(
      title    = "Does effort buy communicative success? (c0 trials)",
      subtitle = sprintf("Model: %s | Bernoulli logit | effort \u00d7 modality",
                         model_label),
      theme    = theme(
        plot.title    = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 11, colour = "grey40")
      )
    )
  
  print(final)
  
  if (!is.null(output_file)) {
    ggsave(output_file, final, width = 12, height = 15,
           dpi = 300, bg = "white")
    message("Saved \u2192 ", output_file)
  }
  
  invisible(list(
    slopes       = all_slopes,
    predictions  = all_preds,
    productivity = productivity,
    plots        = list(slopes = p_slopes, preds = p_preds, prod = p_prod)
  ))
}


# ══════════════════════════════════════════════════════════════════════════════
#  Usage
# ══════════════════════════════════════════════════════════════════════════════

# report_effort_success(
#   model       = e3.m1,
#   data        = trials_c0,
#   model_label = "e3.m1"
# )

# ── With different effort columns (e.g. peak-mean variants) ──────────────────
# report_effort_success(
#   model = e3.m1b,
#   data  = trials_c0,
#   channels = list(
#     arm = list(col = "arm_torque_peak_c", label = "Arm Torque"),
#     env = list(col = "envelope_peak_c",   label = "Envelope"),
#     cop = list(col = "copc_peak_c",       label = "COP")
#   ),
#   model_label = "e3.m1b"
# )


# ══════════════════════════════════════════════════════════════════════════════
#  report_effort_similarity()
#
#  Unified report for a brms Beta regression model predicting answer similarity
#  from effort × modality (all correction trials).
#
#  Produces:
#  A) Effort→similarity slopes (Beta scale, via emtrends) with pairwise
#     contrasts printed to console
#  B) Predicted similarity at −1SD / Mean / +1SD per channel × modality
#
#  Arguments
#  ─────────────────────────────────────────────────────────────────────────────
#  model         Fitted brmsfit (Beta family).
#
#  data          Data frame used to fit the model.
#
#  channels      Named list; each element: list(col = "...", label = "...")
#
#  covariates    Named list of columns to hold at fixed values.
#                Must include any extra predictors in the model formula
#                (e.g. answer_prev_dist_z, correction).
#
#  modality_col  Name of the modality column (default "modality").
#
#  modality_lvls Modality levels to include.
#
#  noise_channels
#                Named list: list("Channel Label" = "modality_value").
#
#  model_label   String used in titles and saved filename.
#
#  output_file   PNG path; NULL to skip saving.
#
#  Globals assumed: modality_colors, theme_effort_plot()
# ══════════════════════════════════════════════════════════════════════════════

report_effort_similarity <- function(
    model,
    data,
    channels = list(
      arm = list(col = "arm_torque_log_c", label = "Arm Torque"),
      env = list(col = "envelope_log_c",   label = "Envelope"),
      cop = list(col = "copc_log_c",       label = "COP")
    ),
    covariates = list(
      answer_prev_dist_z = 0,
      correction         = "c1",
      expressibility_z   = 0,
      Familiarity        = 0,
      BFI_extra          = 0,
      TrialNumber_c      = 0
    ),
    modality_col   = "modality",
    modality_lvls  = c("gesture", "multimodal", "vocal"),
    noise_channels = list(
      "Envelope"   = "gesture",
      "Arm Torque" = "vocal"
    ),
    model_label = deparse(substitute(model)),
    output_file = paste0("effort_similarity_", model_label, ".png")
) {
  
  channel_cols   <- purrr::map_chr(channels, "col")
  channel_labels <- purrr::map_chr(channels, "label")
  
  # ── 1. Console header ────────────────────────────────────────────────────────
  cat("═══════════════════════════════════════════════\n")
  cat(sprintf("  REPORT: %s\n", model_label))
  cat("  Outcome: answer similarity (Beta scale)\n")
  cat("═══════════════════════════════════════════════\n\n")
  
  channel_sds <- purrr::map_dbl(channel_cols, ~ sd(data[[.x]], na.rm = TRUE))
  names(channel_sds) <- channel_cols
  
  cat("  Effort variable SDs:\n")
  purrr::walk2(channel_labels, channel_sds,
               ~ cat(sprintf("    %-20s SD = %.3f\n", .x, .y)))
  cat("\n")
  
  # ── 2. Slopes via emtrends + pairwise contrasts ───────────────────────────────
  cat("  EFFORT→SIMILARITY SLOPES BY CHANNEL × MODALITY\n\n")
  
  all_slopes <- purrr::map2_dfr(channel_cols, channel_labels, function(col, lbl) {
    
    cat(sprintf("  %s:\n", lbl))
    slopes <- emtrends(model, ~ modality, var = col)
    print(slopes)
    cat("\n  Pairwise contrasts:\n")
    print(pairs(slopes))
    cat("\n")
    
    slopes |>
      as.data.frame() |>
      dplyr::rename(
        estimate = paste0(col, ".trend"),
        lower    = lower.HPD,
        upper    = upper.HPD
      ) |>
      dplyr::mutate(channel = lbl)
  }) |>
    dplyr::rename(modality = !!modality_col) |>
    dplyr::mutate(
      channel  = factor(channel, levels = channel_labels),
      modality = factor(modality, levels = modality_lvls),
      credible = lower > 0 | upper < 0,
      noise    = .make_noise_flag(channel, modality, noise_channels),
      colour_var = dplyr::case_when(
        noise     ~ paste0(as.character(modality), "_nc"),
        !credible ~ paste0(as.character(modality), "_nc"),
        TRUE      ~ as.character(modality)
      )
    )
  
  # ── 3. Predicted similarity at ±1SD anchors ───────────────────────────────────
  cat("  Computing predicted similarity at ±1 SD anchors...\n")
  
  make_preds <- function(ch_col, anchors, ch_label) {
    zero_cols <- setNames(rep(0, length(channel_cols)), channel_cols)
    
    base <- data |>
      modelr::data_grid(!!modality_col := modality_lvls, .model = model)
    for (col in names(zero_cols))  base[[col]] <- zero_cols[[col]]
    for (col in names(covariates)) base[[col]] <- covariates[[col]]
    
    base |>
      tidyr::crossing(effort_anchor = anchors) |>
      dplyr::mutate(!!ch_col := effort_anchor) |>
      tidybayes::add_epred_draws(model, re_formula = NA) |>
      dplyr::group_by(!!sym(modality_col), effort_anchor) |>
      dplyr::summarise(
        predicted = median(.epred),
        lower     = quantile(.epred, 0.025),
        upper     = quantile(.epred, 0.975),
        .groups   = "drop"
      ) |>
      dplyr::mutate(channel = ch_label)
  }
  
  all_predictions <- purrr::pmap_dfr(
    list(
      ch_col   = channel_cols,
      anchors  = purrr::map(channel_sds, ~ c(-.x, 0, .x)),
      ch_label = channel_labels
    ),
    make_preds
  ) |>
    dplyr::rename(modality = !!modality_col) |>
    dplyr::mutate(
      channel  = factor(channel, levels = channel_labels),
      modality = factor(modality, levels = modality_lvls),
      effort_label = factor(
        dplyr::case_when(
          effort_anchor < -1e-10 ~ "\u22121 SD",
          effort_anchor >  1e-10 ~ "+1 SD",
          TRUE                   ~ "Mean"
        ),
        levels = c("\u22121 SD", "Mean", "+1 SD")
      ),
      noise      = .make_noise_flag(channel, modality, noise_channels),
      colour_var = ifelse(noise,
                          paste0(as.character(modality), "_nc"),
                          as.character(modality))
    )
  
  cat("\n  Predicted similarity by channel / modality / effort level:\n\n")
  all_predictions |>
    dplyr::select(channel, modality, effort_label, predicted, lower, upper) |>
    dplyr::arrange(channel, modality, effort_label) |>
    print(n = Inf)
  
  cat("\n  Productivity \u0394similarity (+1SD \u2212 \u22121SD):\n")
  all_predictions |>
    dplyr::filter(effort_label != "Mean") |>
    dplyr::select(channel, modality, effort_label, predicted) |>
    dplyr::group_by(channel, modality) |>
    dplyr::summarise(
      delta_sim = predicted[effort_label == "+1 SD"] -
        predicted[effort_label == "\u22121 SD"],
      .groups = "drop"
    ) |>
    dplyr::arrange(channel, modality) |>
    dplyr::mutate(
      line = sprintf("    %-12s \u00d7 %-12s  \u0394sim = %+.3f",
                     as.character(channel), as.character(modality), delta_sim)
    ) |>
    dplyr::pull(line) |>
    cat(sep = "\n")
  cat("\n\n")
  
  # ── 4. Plot A: slopes (Beta scale) ───────────────────────────────────────────
  colour_scale <- .make_colour_scale(modality_lvls, guide = "none")
  
  p_slopes <- all_slopes |>
    ggplot(aes(x = modality, y = estimate,
               colour = colour_var, shape = noise)) +
    geom_hline(yintercept = 0, linetype = "dashed",
               colour = "grey30", linewidth = 0.8) +
    geom_pointrange(aes(ymin = lower, ymax = upper),
                    linewidth = 1.1, size = 0.9) +
    colour_scale +
    scale_shape_manual(
      values = c("FALSE" = 16, "TRUE" = 4),
      labels = c("FALSE" = "Active channel", "TRUE" = "Noise channel"),
      name   = NULL
    ) +
    facet_wrap(~ channel, nrow = 1) +
    theme_effort_plot() +
    theme(
      legend.position = "bottom",
      strip.text      = element_text(size = 11, face = "bold"),
      axis.text.x     = element_text(size = 8)
    ) +
    labs(
      title    = "Effort\u2192similarity slope by channel and modality",
      subtitle = "Effect on answer similarity (Beta scale) | 95% HPD\nGrey = not credible | \u00d7 = noise channel",
      x        = NULL,
      y        = "Slope (Beta scale per SD effort)"
    )
  
  # ── 5. Plot B: predicted similarity at anchors ────────────────────────────────
  p_preds <- all_predictions |>
    ggplot(aes(x = effort_label, y = predicted,
               colour = colour_var, group = modality)) +
    geom_line(linewidth = 0.8, linetype = "dashed", alpha = 0.5) +
    geom_pointrange(aes(ymin = lower, ymax = upper),
                    size = 0.7, linewidth = 1) +
    colour_scale +
    ggnewscale::new_scale_colour() +
    geom_point(aes(colour = modality), size = 0) +
    scale_colour_manual(values = modality_colors, name = "Modality") +
    facet_wrap(~ channel, nrow = 1) +
    theme_effort_plot() +
    theme(
      legend.position = "bottom",
      strip.text      = element_text(size = 11, face = "bold"),
      axis.text.x     = element_text(size = 9)
    ) +
    labs(
      title    = "Predicted similarity at effort anchors",
      subtitle = "95% CI | Other predictors at 0 | Grey = noise channel",
      x        = "Effort level",
      y        = "Predicted answer similarity"
    )
  
  # ── 6. Combine and save ───────────────────────────────────────────────────────
  final <- (p_slopes / p_preds) +
    patchwork::plot_annotation(
      title    = "Does effort predict communicative success? (similarity outcome)",
      subtitle = sprintf("Model: %s | Beta regression | effort \u00d7 modality",
                         model_label),
      theme    = theme(
        plot.title    = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 11, colour = "grey40")
      )
    )
  
  print(final)
  
  if (!is.null(output_file)) {
    ggsave(output_file, final, width = 12, height = 10,
           dpi = 300, bg = "white")
    message("Saved \u2192 ", output_file)
  }
  
  invisible(list(
    slopes      = all_slopes,
    predictions = all_predictions,
    plots       = list(slopes = p_slopes, preds = p_preds)
  ))
}


# ══════════════════════════════════════════════════════════════════════════════
#  Usage
# ══════════════════════════════════════════════════════════════════════════════

# report_effort_similarity(
#   model       = e4.m4_att,
#   data        = df_eff_rep,
#   model_label = "e4.m4_att"
# )

# ── With different column names ───────────────────────────────────────────────
# report_effort_similarity(
#   model = e4.m4_att,
#   data  = df_eff_rep,
#   channels = list(
#     arm = list(col = "arm_torque_in_log_c", label = "Arm Torque"),
#     env = list(col = "envelope_log_c",      label = "Envelope"),
#     cop = list(col = "copc_log_c",          label = "COP")
#   ),
#   model_label = "e4.m4_att"
# )

# ── With extra covariates held at non-zero values ─────────────────────────────
# report_effort_similarity(
#   model = e4.m4_att,
#   data  = df_eff_rep,
#   covariates = list(
#     answer_prev_dist_z = 0,
#     correction         = "c1",
#     expressibility_z   = 0,
#     Familiarity        = 0,
#     BFI_extra          = 0,
#     TrialNumber_c      = 0
#   ),
#   model_label = "e4.m4_att"
# )


# ══════════════════════════════════════════════════════════════════════════════
#  report_resolution_by_effort()
#
#  Generates a three-panel report for a brms cumulative("logit") model where
#  effort (in up to 3 channels) × modality predicts an ordinal resolution
#  outcome.
#
#  Panels:
#  A) ΔP slopes for ALL ordinal categories (rows) × channels (cols)
#  B) P(fastest category) pointrange at −1SD / Mean / +1SD
#  C) Stacked category probability bars
#
#  Arguments
#  ─────────────────────────────────────────────────────────────────────────────
#  model         A fitted brmsfit with family cumulative("logit").
#  data          The data frame used to fit the model.
#  channels      Named list; each element: list(col = "...", label = "...")
#  covariates    Named list of columns to hold at fixed values.
#  modality_col  Name of the modality column (default "modality").
#  modality_lvls Character vector of modality levels.
#  noise_channels Named list: list("Channel Label" = "modality_value").
#  model_label   String used in titles and saved filename.
#  output_file   PNG path; NULL to skip saving.
#
#  Globals assumed: modality_colors, theme_effort_plot(),
#                   .make_noise_flag(), .make_colour_scale()
# ══════════════════════════════════════════════════════════════════════════════

report_resolution_by_effort <- function(
    model,
    data,
    channels = list(
      arm = list(col = "arm_torque_log_c", label = "Arm Torque"),
      env = list(col = "envelope_log_c",   label = "Envelope"),
      cop = list(col = "copc_log_c",       label = "COP")
    ),
    covariates = list(
      expressibility_z = 0,
      Familiarity      = 0,
      BFI_extra        = 0,
      TrialNumber_c    = 0
    ),
    modality_col   = "modality",
    modality_lvls  = c("gesture", "vocal", "multimodal"),
    noise_channels = list(
      "Envelope"   = "gesture",
      "Arm Torque" = "vocal"
    ),
    model_label = deparse(substitute(model)),
    output_file = paste0("resolution_by_effort_", model_label, ".png")
) {
  
  channel_cols   <- purrr::map_chr(channels, "col")
  channel_labels <- purrr::map_chr(channels, "label")
  
  # ── 1. Console: model summary ─────────────────────────────────────────────────
  cat("═══════════════════════════════════════════════\n")
  cat(sprintf("  MODEL SUMMARY: %s\n", model_label))
  cat("  Family: cumulative logit\n")
  cat("  Positive b = higher odds of *slower* resolution\n")
  cat("═══════════════════════════════════════════════\n\n")
  print(summary(model))
  
  # ── 2. Fixed effects table ────────────────────────────────────────────────────
  cat("\n  Fixed effects (excl. thresholds):\n")
  fe <- fixef(model) |>
    as.data.frame() |>
    rownames_to_column("term") |>
    filter(!str_starts(term, "Intercept")) |>
    mutate(credible = (Q2.5 > 0 | Q97.5 < 0))
  print(fe)
  
  cat("\n  Ordinal thresholds:\n")
  fixef(model) |>
    as.data.frame() |>
    rownames_to_column("term") |>
    filter(str_starts(term, "Intercept")) |>
    print()
  
  # ── 3. SD-based anchors ───────────────────────────────────────────────────────
  cat("\n  Computing predictions...\n")
  
  channel_sds <- purrr::map_dbl(channel_cols, ~ sd(data[[.x]], na.rm = TRUE))
  names(channel_sds) <- channel_cols
  
  cat("  Effort variable SDs:\n")
  purrr::walk2(channel_labels, channel_sds,
               ~ cat(sprintf("    %-20s SD = %.3f\n", .x, .y)))
  cat("\n")
  
  # ── 4. Inner helper: predictions for one channel ──────────────────────────────
  make_preds <- function(ch_col, anchors, ch_label) {
    zero_cols <- setNames(rep(0, length(channel_cols)), channel_cols)
    
    base <- data |>
      modelr::data_grid(!!modality_col := modality_lvls, .model = model)
    for (col in names(zero_cols))  base[[col]] <- zero_cols[[col]]
    for (col in names(covariates)) base[[col]] <- covariates[[col]]
    
    base |>
      tidyr::crossing(effort_anchor = anchors) |>
      dplyr::mutate(!!ch_col := effort_anchor) |>
      tidybayes::add_epred_draws(model, re_formula = NA) |>
      dplyr::group_by(!!sym(modality_col), effort_anchor, .category) |>
      dplyr::summarise(
        prob  = median(.epred),
        lower = quantile(.epred, 0.055),
        upper = quantile(.epred, 0.945),
        .groups = "drop"
      ) |>
      dplyr::mutate(channel = ch_label)
  }
  
  all_ord <- purrr::pmap_dfr(
    list(
      ch_col   = channel_cols,
      anchors  = purrr::map(channel_sds, ~ c(-.x, 0, .x)),
      ch_label = channel_labels
    ),
    make_preds
  ) |>
    dplyr::rename(modality = !!modality_col) |>
    dplyr::mutate(
      channel  = factor(channel, levels = channel_labels),
      modality = factor(modality, levels = modality_lvls),
      effort_label = factor(
        dplyr::case_when(
          effort_anchor < -1e-10 ~ "\u22121 SD",
          effort_anchor >  1e-10 ~ "+1 SD",
          TRUE                   ~ "Mean"
        ),
        levels = c("\u22121 SD", "Mean", "+1 SD")
      ),
      noise = .make_noise_flag(channel, modality, noise_channels)
    )
  
  fastest_cat <- levels(factor(all_ord$.category))[1]
  
  # ── 5. Console: numerical summary ─────────────────────────────────────────────
  cat("\n═══════════════════════════════════════════════\n")
  cat("  NUMERICAL SUMMARY FOR REPORTING\n")
  cat("═══════════════════════════════════════════════\n\n")
  
  cat("  Predicted P per resolution category at effort anchors (89% CI):\n\n")
  for (cat_level in levels(factor(all_ord$.category))) {
    cat(sprintf("  [%s]\n", cat_level))
    all_ord |>
      dplyr::filter(.category == cat_level) |>
      dplyr::arrange(channel, modality, effort_label) |>
      dplyr::mutate(
        noise_flag = ifelse(noise, " [noise]", ""),
        line = sprintf(
          "    %-12s \u00d7 %-12s  %-8s  P = %.3f [%.3f, %.3f]%s",
          as.character(channel), as.character(modality),
          as.character(effort_label),
          prob, lower, upper, noise_flag
        )
      ) |>
      dplyr::pull(line) |>
      cat(sep = "\n")
    cat("\n")
  }
  
  cat("\n  Productivity \u0394P (+1SD \u2212 \u22121SD) per resolution category:\n\n")
  
  productivity_ord <- all_ord |>
    dplyr::filter(effort_label != "Mean") |>
    dplyr::select(channel, modality, effort_label, .category, prob, noise) |>
    dplyr::group_by(channel, modality, .category, noise) |>
    dplyr::summarise(
      delta_p = prob[effort_label == "+1 SD"] -
        prob[effort_label == "\u22121 SD"],
      .groups = "drop"
    )
  
  for (cat_level in levels(factor(productivity_ord$.category))) {
    cat(sprintf("  [%s]\n", cat_level))
    productivity_ord |>
      dplyr::filter(.category == cat_level) |>
      dplyr::arrange(channel, modality) |>
      dplyr::mutate(
        noise_flag = ifelse(noise, " [noise]", ""),
        line = sprintf(
          "    %-12s \u00d7 %-12s  \u0394P = %+.3f%s",
          as.character(channel), as.character(modality),
          delta_p, noise_flag
        )
      ) |>
      dplyr::pull(line) |>
      cat(sep = "\n")
    cat("\n")
  }
  
  cat("\n  Mean P per category at average effort by modality:\n\n")
  all_ord |>
    dplyr::filter(effort_label == "Mean") |>
    dplyr::group_by(.category, modality) |>
    dplyr::summarise(prob = mean(prob), .groups = "drop") |>
    dplyr::arrange(.category, modality) |>
    dplyr::mutate(
      line = sprintf("    %-6s \u00d7 %-12s  P = %.3f",
                     as.character(.category), as.character(modality), prob)
    ) |>
    dplyr::pull(line) |>
    cat(sep = "\n")
  cat("\n\n")
  
  # ── 6. Draw-level slopes for ALL categories ───────────────────────────────────
  cat("  Computing draw-level slopes for all categories...\n")
  
  slope_draws_all <- purrr::pmap_dfr(
    list(
      ch_col   = channel_cols,
      anchors  = purrr::map(channel_sds, ~ c(-.x, .x)),
      ch_label = channel_labels
    ),
    function(ch_col, anchors, ch_label) {
      zero_cols <- setNames(rep(0, length(channel_cols)), channel_cols)
      
      base <- data |>
        modelr::data_grid(!!modality_col := modality_lvls, .model = model)
      for (col in names(zero_cols))  base[[col]] <- zero_cols[[col]]
      for (col in names(covariates)) base[[col]] <- covariates[[col]]
      
      base |>
        tidyr::crossing(effort_anchor = anchors) |>
        dplyr::mutate(!!ch_col := effort_anchor) |>
        tidybayes::add_epred_draws(model, re_formula = NA) |>
        dplyr::mutate(
          effort_dir = ifelse(effort_anchor < 0, "lo", "hi"),
          channel    = ch_label
        )
    }
  ) |>
    dplyr::rename(modality = !!modality_col) |>
    dplyr::mutate(
      channel  = factor(channel, levels = channel_labels),
      modality = factor(modality, levels = modality_lvls)
    )
  
  slope_summary_all <- slope_draws_all |>
    dplyr::select(.draw, modality, channel, effort_dir, .category, .epred) |>
    tidyr::pivot_wider(
      id_cols     = c(.draw, modality, channel, .category),
      names_from  = effort_dir,
      values_from = .epred
    ) |>
    dplyr::mutate(slope = hi - lo) |>
    dplyr::group_by(channel, modality, .category) |>
    tidybayes::median_hdi(slope) |>
    dplyr::mutate(
      credible   = .lower > 0 | .upper < 0,
      noise      = .make_noise_flag(channel, modality, noise_channels),
      colour_var = dplyr::case_when(
        noise     ~ paste0(as.character(modality), "_nc"),
        !credible ~ paste0(as.character(modality), "_nc"),
        TRUE      ~ as.character(modality)
      ),
      .category = factor(.category,
                         levels = levels(factor(all_ord$.category)))
    )
  
  # ── Plot A: ΔP slopes for all categories ─────────────────────────────────────
  p_slopes_all <- slope_summary_all |>
    ggplot(aes(x = modality, y = slope,
               colour = colour_var, shape = noise)) +
    geom_hline(yintercept = 0, linetype = "dashed",
               colour = "grey30", linewidth = 0.8) +
    geom_pointrange(aes(ymin = .lower, ymax = .upper),
                    linewidth = 1.1, size = 0.8) +
    .make_colour_scale(modality_lvls, guide = "none") +
    scale_shape_manual(
      values = c("FALSE" = 16, "TRUE" = 4),
      labels = c("FALSE" = "Active channel", "TRUE" = "Noise channel"),
      name   = NULL
    ) +
    facet_grid(.category ~ channel, switch = "y") +
    theme_effort_plot() +
    theme(
      legend.position = "bottom",
      strip.text      = element_text(size = 10, face = "bold"),
      strip.placement = "outside",
      axis.text.x     = element_text(size = 8)
    ) +
    labs(
      title    = "Effort \u2192 resolution: \u0394P per SD effort by resolution category",
      subtitle = "Rows: resolution category (fast \u2192 slow) | Grey = not credible or noise | 95% HDI",
      x        = NULL,
      y        = "\u0394P per SD effort"
    )
  
  # ── Plot B: P(fastest) pointrange ────────────────────────────────────────────
  p_fast <- all_ord |>
    dplyr::filter(.category == fastest_cat) |>
    dplyr::mutate(
      colour_var = ifelse(noise,
                          paste0(as.character(modality), "_nc"),
                          as.character(modality))
    ) |>
    ggplot(aes(x = effort_label, y = prob,
               colour = colour_var, group = modality)) +
    geom_line(linewidth = 0.8, linetype = "dashed", alpha = 0.5) +
    geom_pointrange(aes(ymin = lower, ymax = upper),
                    size = 0.7, linewidth = 1) +
    .make_colour_scale(modality_lvls, guide = "none") +
    ggnewscale::new_scale_colour() +
    geom_point(aes(colour = modality), size = 0) +
    scale_colour_manual(values = modality_colors, name = "Modality") +
    facet_wrap(~ channel, nrow = 1) +
    theme_effort_plot() +
    theme(
      legend.position = "bottom",
      strip.text      = element_text(size = 11, face = "bold"),
      axis.text.x     = element_text(size = 9)
    ) +
    labs(
      title    = sprintf("P(resolved at %s) by effort level", fastest_cat),
      subtitle = "89% CI | Grey = noise channel",
      x        = "Effort level",
      y        = sprintf("P(%s)", fastest_cat)
    )
  
  # ── Plot C: stacked category bars ────────────────────────────────────────────
  n_cats  <- nlevels(factor(all_ord$.category))
  cat_pal <- colorRampPalette(
    c("#1B5E20", "#A5D6A7", "#FFF9C4", "#EF9A9A", "#B71C1C")
  )(n_cats)
  names(cat_pal) <- levels(factor(all_ord$.category))
  
  p_stack <- all_ord |>
    ggplot(aes(x = effort_label, y = prob, fill = .category)) +
    geom_col(position = "stack", width = 0.7,
             colour = "white", linewidth = 0.3) +
    scale_fill_manual(
      values = cat_pal,
      name   = "Resolution\ncategory",
      guide  = guide_legend(reverse = TRUE)
    ) +
    facet_grid(modality ~ channel) +
    theme_effort_plot() +
    theme(
      legend.position = "right",
      strip.text      = element_text(size = 10, face = "bold"),
      axis.text.x     = element_text(size = 8)
    ) +
    labs(
      title    = "Effort \u2192 resolution speed: predicted category probabilities",
      subtitle = "Stacked bars | Rows: modality | Cols: effort channel",
      x        = "Effort level",
      y        = "Predicted probability"
    )
  
  # ── 7. Combine and save ───────────────────────────────────────────────────────
  final <- (p_slopes_all / p_fast / p_stack) +
    patchwork::plot_annotation(
      title    = "Does communicative investment predict resolution speed?",
      subtitle = sprintf("Model: %s | cumulative logit | effort \u00d7 modality",
                         model_label),
      theme    = theme(
        plot.title    = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 11, colour = "grey40")
      )
    )
  
  print(final)
  
  if (!is.null(output_file)) {
    ggsave(output_file, final, width = 14, height = 20,
           dpi = 300, bg = "white")
    message("Saved \u2192 ", output_file)
  }
  
  invisible(list(
    predictions   = all_ord,
    slope_summary = slope_summary_all,
    productivity  = productivity_ord,
    plots         = list(
      slopes = p_slopes_all,
      fast   = p_fast,
      stack  = p_stack
    )
  ))
}


# ══════════════════════════════════════════════════════════════════════════════
#  Usage
# ══════════════════════════════════════════════════════════════════════════════

# Default (matches e3.m4_att / e4.m5_att):
# report_resolution_by_effort(
#   model       = e3.m4_att,
#   data        = df_ordinal,
#   model_label = "e3.m4_att"
# )

# With different effort column names:
# report_resolution_by_effort(
#   model = e3.m4b_att,
#   data  = df_ordinal,
#   channels = list(
#     arm = list(col = "arm_torque_in_log_c", label = "Arm Torque"),
#     env = list(col = "envelope_in_log_c",   label = "Envelope"),
#     cop = list(col = "copc_in_log_c",       label = "COP")
#   ),
#   model_label = "e3.m4b_att"
# )

# c0-only model:
# report_resolution_by_effort(
#   model       = e4.m5_att,
#   data        = df_ordinal_c0,
#   model_label = "e4.m5_att"
# )

# Weighted mean model:
# report_resolution_by_effort(
#   model       = e4.m6_att,
#   data        = df_ordinal_weighted,
#   model_label = "e4.m6_att"
# )

#######
compare_convergence <- function(model_list) {
  
  # ── Per-model convergence metrics ───────────────────────────────────────────
  results <- imap_dfr(model_list, function(model, model_name) {
    
    # Total post-warmup draws
    total_draws <- tryCatch(
      (model$fit@sim$iter - model$fit@sim$warmup) * model$fit@sim$chains,
      error = function(e) NA
    )
    
    # Dynamic neff thresholds based on actual draws
    # 1000 = recommended (Bürkner 2017, Kruschke 2015)
    # 400  = critical minimum (Vehtari et al. 2021)
    neff_threshold_bad  <- if (!is.na(total_draws)) 1000 / total_draws else 0.1
    neff_threshold_crit <- if (!is.na(total_draws)) 400  / total_draws else 0.05
    
    # neff and rhat — from full parameter set
    neff_vals <- tryCatch(brms::neff_ratio(model), error = function(e) NULL)
    rhat_vals <- tryCatch(brms::rhat(model),        error = function(e) NULL)
    
    # ESS — from summary (fast, avoids iterating over individual RE levels)
    sum_fixed  <- tryCatch(as.data.frame(summary(model)$fixed),     error = function(e) NULL)
    sum_random <- tryCatch(as.data.frame(summary(model)$random),    error = function(e) NULL)
    sum_spec   <- tryCatch(as.data.frame(summary(model)$spec_pars), error = function(e) NULL)
    all_sum    <- bind_rows(sum_fixed, sum_random, sum_spec)
    
    data.frame(
      model = model_name,
      
      # Draw info
      total_draws    = total_draws,
      neff_thr_bad   = round(neff_threshold_bad,  4),
      neff_thr_crit  = round(neff_threshold_crit, 4),
      
      # Rhat
      max_rhat    = if (!is.null(rhat_vals)) round(max(rhat_vals, na.rm=TRUE), 4) else NA,
      n_rhat_bad  = if (!is.null(rhat_vals)) sum(rhat_vals > 1.01, na.rm=TRUE) else NA,
      
      # neff ratio — with dynamic thresholds
      min_neff      = if (!is.null(neff_vals)) round(min(neff_vals,  na.rm=TRUE), 4) else NA,
      mean_neff     = if (!is.null(neff_vals)) round(mean(neff_vals, na.rm=TRUE), 4) else NA,
      n_neff_bad    = if (!is.null(neff_vals)) sum(neff_vals < neff_threshold_bad,  na.rm=TRUE) else NA,
      n_neff_crit   = if (!is.null(neff_vals)) sum(neff_vals < neff_threshold_crit, na.rm=TRUE) else NA,
      
      # Bulk ESS
      min_bulk_ESS  = if (nrow(all_sum) > 0 && "Bulk_ESS" %in% names(all_sum))
        min(all_sum$Bulk_ESS,  na.rm=TRUE) else NA,
      mean_bulk_ESS = if (nrow(all_sum) > 0 && "Bulk_ESS" %in% names(all_sum))
        round(mean(all_sum$Bulk_ESS, na.rm=TRUE)) else NA,
      n_bulk_bad    = if (nrow(all_sum) > 0 && "Bulk_ESS" %in% names(all_sum))
        sum(all_sum$Bulk_ESS < 1000, na.rm=TRUE) else NA,
      n_bulk_crit   = if (nrow(all_sum) > 0 && "Bulk_ESS" %in% names(all_sum))
        sum(all_sum$Bulk_ESS < 400,  na.rm=TRUE) else NA,
      
      # Tail ESS
      min_tail_ESS  = if (nrow(all_sum) > 0 && "Tail_ESS" %in% names(all_sum))
        min(all_sum$Tail_ESS,  na.rm=TRUE) else NA,
      mean_tail_ESS = if (nrow(all_sum) > 0 && "Tail_ESS" %in% names(all_sum))
        round(mean(all_sum$Tail_ESS, na.rm=TRUE)) else NA,
      n_tail_bad    = if (nrow(all_sum) > 0 && "Tail_ESS" %in% names(all_sum))
        sum(all_sum$Tail_ESS < 1000, na.rm=TRUE) else NA,
      n_tail_crit   = if (nrow(all_sum) > 0 && "Tail_ESS" %in% names(all_sum))
        sum(all_sum$Tail_ESS < 400,  na.rm=TRUE) else NA
    )
  })
  
  # ── Scoring ──────────────────────────────────────────────────────────────────
  # Hard convergence: Rhat + critical ESS threshold (400)
  # Soft scoring: uses recommended threshold (1000) and neff
  results <- results |>
    mutate(
      # Hard pass/fail based on critical thresholds
      converged = max_rhat < 1.01 &
        min_bulk_ESS >= 400 &
        min_tail_ESS >= 400,
      
      # Soft score for ranking converged models
      # neff_bad uses dynamic threshold (equivalent to ESS < 1000)
      # weighted: Rhat > neff > ESS
      score = scale(replace_na(n_rhat_bad,  0))[,1] * 2.0 +
        scale(replace_na(n_neff_bad,  0))[,1] * 1.5 +
        scale(replace_na(n_bulk_bad,  0))[,1] * 0.5 +
        scale(replace_na(n_tail_bad,  0))[,1] * 0.5
    )
  
  # ── Print summary ─────────────────────────────────────────────────────────────
  cat("═══════════════════════════════════════════════════════════════════════════\n")
  cat("  CONVERGENCE COMPARISON\n")
  cat("  Thresholds: Rhat < 1.01 | ESS > 1000 (recommended) | ESS > 400 (critical)\n")
  cat("  neff thresholds dynamic: bad = 1000/draws | crit = 400/draws\n")
  cat("═══════════════════════════════════════════════════════════════════════════\n\n")
  
  cat(sprintf("  %-10s %7s %8s %9s %9s %9s %10s %9s %9s %9s %9s\n",
              "Model", "draws", "max_Rhat", "n_Rhat>1%",
              "min_neff", "neff_thr", "n_neff_bad",
              "min_bulk", "n_bulk<1k", "min_tail", "Converged"))
  cat(paste(rep("─", 120), collapse=""), "\n")
  
  for (i in seq_len(nrow(results))) {
    r <- results[i, ]
    cat(sprintf("  %-10s %7.0f %8.4f %9d %9.4f %9.4f %10d %9.0f %9d %9.0f %9s\n",
                r$model,
                r$total_draws,
                r$max_rhat,
                r$n_rhat_bad,
                r$min_neff,
                r$neff_thr_bad,
                r$n_neff_bad,
                r$min_bulk_ESS,
                r$n_bulk_bad,
                r$min_tail_ESS,
                ifelse(isTRUE(r$converged), "✓", "⚠")))
  }
  
  cat("\n")
  cat("  Legend:\n")
  cat("  n_neff_bad  = parameters below ESS-equivalent neff threshold (< 1000 effective)\n")
  cat("  n_bulk<1k   = parameters with Bulk ESS < 1000 (recommended)\n")
  cat("  Converged   = Rhat < 1.01 AND min bulk/tail ESS >= 400 (critical minimum)\n\n")
  
  # ── Best model ────────────────────────────────────────────────────────────────
  converged_models <- results |> filter(isTRUE(converged))
  
  if (nrow(converged_models) == 0) {
    cat("  ⚠ No model passed all convergence criteria (Rhat + ESS > 400).\n")
    cat("  Best available (lowest composite score):\n")
    best <- results |> slice_min(score, n = 1)
  } else if (nrow(converged_models) == 1) {
    best <- converged_models
  } else {
    best <- converged_models |> slice_min(score, n = 1)
  }
  
  cat(sprintf("  ✓ Best converging model: %s\n", best$model[1]))
  cat(sprintf("    total draws    = %.0f\n",  best$total_draws[1]))
  cat(sprintf("    max Rhat       = %.4f\n",  best$max_rhat[1]))
  cat(sprintf("    n Rhat bad     = %d\n",    best$n_rhat_bad[1]))
  cat(sprintf("    min neff       = %.4f  (threshold = %.4f)\n",
              best$min_neff[1], best$neff_thr_bad[1]))
  cat(sprintf("    n neff bad     = %d  (< 1000 ESS equivalent)\n", best$n_neff_bad[1]))
  cat(sprintf("    n neff crit    = %d  (< 400 ESS equivalent)\n",  best$n_neff_crit[1]))
  cat(sprintf("    min bulk ESS   = %.0f\n",  best$min_bulk_ESS[1]))
  cat(sprintf("    n bulk < 1000  = %d\n",    best$n_bulk_bad[1]))
  cat(sprintf("    n bulk < 400   = %d\n",    best$n_bulk_crit[1]))
  cat(sprintf("    min tail ESS   = %.0f\n",  best$min_tail_ESS[1]))
  cat(sprintf("    n tail < 1000  = %d\n",    best$n_tail_bad[1]))
  cat(sprintf("    n tail < 400   = %d\n",    best$n_tail_crit[1]))
  cat("\n")
  
  invisible(results)
}

######
create_parameter_table <- function(model_list, r2_list = NULL) {
  
  # ── Model info ──────────────────────────────────────────────────────────────
  extract_model_info <- function(model, model_name) {
    data.frame(
      model     = model_name,
      parameter = c("formula", "n_obs"),
      label     = as.character(c(
        as.character(formula(model))[1],
        nobs(model)
      )),
      term_type = "model_info",
      stringsAsFactors = FALSE
    )
  }
  
  # ── R2 ──────────────────────────────────────────────────────────────────────
  extract_r2 <- function(model_name, r2_list) {
    if (is.null(r2_list) || !model_name %in% names(r2_list))
      return(NULL)
    r2 <- r2_list[[model_name]]
    data.frame(
      model     = model_name,
      parameter = "R2",
      label     = paste0(round(r2[, "Estimate"], 3), " [",
                         round(r2[, "Q2.5"],    3), ", ",
                         round(r2[, "Q97.5"],   3), "]"),
      term_type = "model_info",
      stringsAsFactors = FALSE
    )
  }
  
  # ── Fixed effects ────────────────────────────────────────────────────────────
  extract_fixed <- function(model, model_name) {
    as.data.frame(fixef(model)) |>
      tibble::rownames_to_column("parameter") |>
      rename(estimate = Estimate, se = Est.Error,
             ci_low = Q2.5, ci_high = Q97.5) |>
      mutate(
        model     = model_name,
        term_type = "fixed",
        credible  = ci_low > 0 | ci_high < 0,
        label     = paste0(round(estimate, 3), " [", round(ci_low, 3), ", ",
                           round(ci_high, 3), "]",
                           ifelse(credible, " *", ""))
      )
  }
  
  # ── Random SDs ──────────────────────────────────────────────────────────────
  extract_random_sds <- function(model, model_name) {
    vc <- VarCorr(model)
    purrr::imap_dfr(vc, function(group_vc, group_name) {
      as.data.frame(group_vc$sd) |>
        tibble::rownames_to_column("param") |>
        rename(estimate = Estimate, se = Est.Error,
               ci_low = Q2.5, ci_high = Q97.5) |>
        mutate(
          model     = model_name,
          term_type = "random_sd",
          parameter = paste0("sd(", param, ") | ", group_name),
          credible  = ci_low > 0 | ci_high < 0,
          label     = paste0(round(estimate, 3), " [", round(ci_low, 3), ", ",
                             round(ci_high, 3), "]")
        ) |>
        select(-param)
    })
  }
  
  # ── Random correlations ──────────────────────────────────────────────────────
  extract_random_cors <- function(model, model_name) {
    vc <- VarCorr(model)
    purrr::imap_dfr(vc, function(group_vc, group_name) {
      cor_array <- group_vc$cor
      if (is.null(cor_array)) return(NULL)
      param_names <- dimnames(cor_array)[[1]]
      purrr::map_dfr(seq_along(param_names), function(i) {
        purrr::map_dfr(seq_along(param_names), function(j) {
          if (j <= i) return(NULL)
          p1      <- param_names[i]
          p2      <- param_names[j]
          est     <- cor_array[p1, "Estimate", p2]
          ci_low  <- cor_array[p1, "Q2.5",     p2]
          ci_high <- cor_array[p1, "Q97.5",    p2]
          credible <- (!is.na(ci_low) & !is.na(ci_high)) &
            (ci_low > 0 | ci_high < 0)
          data.frame(
            model     = model_name,
            term_type = "random_cor",
            parameter = paste0("cor(", p1, " x ", p2, ") | ", group_name),
            credible  = credible,
            label     = paste0(
              ifelse(is.na(est),     "NA", sprintf("%.3f", est)), " [",
              ifelse(is.na(ci_low),  "NA", sprintf("%.3f", ci_low)), ", ",
              ifelse(is.na(ci_high), "NA", sprintf("%.3f", ci_high)), "]",
              ifelse(isTRUE(credible), " *", "")
            ),
            stringsAsFactors = FALSE
          )
        })
      })
    })
  }
  
  # ── Run across models ────────────────────────────────────────────────────────
  model_names <- names(model_list)
  
  all_rows <- imap_dfr(model_list, function(mod, nm) {
    info <- tryCatch(extract_model_info(mod, nm), error = function(e) NULL)
    r2   <- tryCatch(extract_r2(nm, r2_list),     error = function(e) NULL)
    fe   <- tryCatch(extract_fixed(mod, nm),       error = function(e) NULL)
    sds  <- tryCatch(extract_random_sds(mod, nm),  error = function(e) NULL)
    cors <- tryCatch(extract_random_cors(mod, nm), error = function(e) NULL)
    bind_rows(info, r2, fe, sds, cors) |>
      select(model, term_type, parameter, label) |>
      mutate(label = as.character(label))
  })
  
  # ── Pivot ────────────────────────────────────────────────────────────────────
  wide <- all_rows |>
    mutate(
      term_type = factor(term_type,
                         levels = c("model_info", "fixed",
                                    "random_sd", "random_cor"))
    ) |>
    pivot_wider(
      id_cols     = c(term_type, parameter),
      names_from  = model,
      values_from = label,
      values_fill = "—"
    )
  
  # ── Sort: Intercept first within fixed effects ────────────────────────────── 
  wide |>
    mutate(
      sort_key = case_when(
        term_type == "model_info"  ~ 0,
        term_type == "fixed" & parameter == "Intercept" ~ 1,
        term_type == "fixed"       ~ 2,
        term_type == "random_sd"   ~ 3,
        term_type == "random_cor"  ~ 4,
        TRUE                       ~ 5
      )
    ) |>
    arrange(sort_key, term_type, parameter) |>
    select(-sort_key) |>
    select(term_type, parameter, all_of(model_names))
}


###############
targeted_comparisons <- function(model, type = c("two_modality", "three_modality"),
                                 dv_label = "Effort") {
  
  type <- match.arg(type)
  
  # ── Helper: print formatted result ─────────────────────────────────────────
  print_result <- function(draws, var, label, pct = FALSE, digits = 1) {
    res <- draws |> median_hdi(!!sym(var))
    est <- res[[var]]
    lo  <- res$.lower
    hi  <- res$.upper
    cred <- lo > 0 | hi < 0
    if (pct) {
      cat(sprintf("  %-45s %+.1f%% [%+.1f%%, %+.1f%%]%s\n",
                  label, est, lo, hi, ifelse(cred, "  *", "")))
    } else {
      cat(sprintf("  %-45s %.3f [%.3f, %.3f]%s\n",
                  label, est, lo, hi, ifelse(cred, "  *", "")))
    }
  }
  
  modality_colors <- c(
    "gesture"    = "#2196F3",
    "vocal"      = "#FF9800",
    "multimodal" = "#4CAF50"
  )
  
  expr_palette <- c(
    "−2 SD"   = "#C2185B",
    "−1 SD"   = "#E91E63",
    "Average" = "#795548",
    "+1 SD"   = "#8D6E63",
    "+2 SD"   = "#4E342E"
  )
  
  performer_palette <- c(
    "−2 SD"   = "#37474F",
    "−1 SD"   = "#607D8B",
    "Average" = "#000000",
    "+1 SD"   = "#E53935",
    "+2 SD"   = "#B71C1C"
  )
  
  plots <- list()
  
  # ── 1. Direct modality comparisons ─────────────────────────────────────────
  cat("═══════════════════════════════════════════════\n")
  cat("  1. MODALITY COMPARISONS (main effect)\n")
  cat("═══════════════════════════════════════════════\n\n")
  
  if (type == "two_modality") {
    
    diff_draws <- model |>
      spread_draws(b_modality1) |>
      mutate(
        diff     = 2 * b_modality1,
        diff_pct = (exp(diff) - 1) * 100
      )
    
    print_result(diff_draws, "diff",     "Gesture vs Multimodal (log scale):")
    print_result(diff_draws, "diff_pct", "Gesture vs Multimodal (%):", pct = TRUE)
    
    plots$modality_diff <- diff_draws |>
      ggplot(aes(x = diff_pct, fill = after_stat(x > 0))) +
      stat_halfeye(
        .width = c(0.89, 0.95), point_interval = median_hdi,
        normalize = "none", alpha = 0.85
      ) +
      geom_vline(xintercept = 0, linetype = "dashed",
                 colour = "grey20", linewidth = 0.8) +
      scale_fill_manual(
        values = c("FALSE" = "#4CAF50", "TRUE" = "#2196F3"),
        guide  = "none"
      ) +
      theme_effort_plot() +
      labs(title    = "Gesture vs Multimodal",
           subtitle = "Posterior of % difference",
           x = "% difference", y = NULL)
    
  } else {
    
    diff_draws <- model |>
      spread_draws(b_modality1, b_modality2) |>
      mutate(
        vocal_offset          = -(b_modality1 + b_modality2),
        gesture_vs_multimodal = (exp(b_modality1 - b_modality2)  - 1) * 100,
        gesture_vs_vocal      = (exp(b_modality1 - vocal_offset) - 1) * 100,
        multimodal_vs_vocal   = (exp(b_modality2 - vocal_offset) - 1) * 100
      )
    
    print_result(diff_draws, "gesture_vs_multimodal", "Gesture vs Multimodal (%):", pct = TRUE)
    print_result(diff_draws, "gesture_vs_vocal",      "Gesture vs Vocal (%):",      pct = TRUE)
    print_result(diff_draws, "multimodal_vs_vocal",   "Multimodal vs Vocal (%):",   pct = TRUE)
    
    plots$modality_diff <- diff_draws |>
      select(.draw, gesture_vs_multimodal, gesture_vs_vocal, multimodal_vs_vocal) |>
      tidyr::pivot_longer(
        cols     = c(gesture_vs_multimodal, gesture_vs_vocal, multimodal_vs_vocal),
        names_to = "comparison", values_to = "pct_diff"
      ) |>
      mutate(comparison = factor(comparison,
                                 levels = c("gesture_vs_vocal", "multimodal_vs_vocal", "gesture_vs_multimodal"),
                                 labels = c("Gesture vs Vocal", "Multimodal vs Vocal", "Gesture vs Multimodal")
      )) |>
      ggplot(aes(x = pct_diff, y = comparison, fill = comparison)) +
      stat_halfeye(
        .width = c(0.89, 0.95), point_interval = median_hdi,
        normalize = "groups", scale = 0.7, alpha = 0.85
      ) +
      geom_vline(xintercept = 0, linetype = "dashed",
                 colour = "grey20", linewidth = 0.8) +
      scale_fill_manual(
        values = c("Gesture vs Vocal"      = "#2196F3",
                   "Multimodal vs Vocal"   = "#4CAF50",
                   "Gesture vs Multimodal" = "#9C27B0"),
        guide = "none"
      ) +
      theme_effort_plot() +
      labs(title    = "Pairwise modality comparisons",
           subtitle = "Posterior of % differences",
           x = "% difference", y = NULL)
  }
  
  # ── 2. Effort trajectory by modality across corrections ────────────────────
  cat("\n═══════════════════════════════════════════════\n")
  cat("  2. EFFORT BY MODALITY ACROSS CORRECTIONS\n")
  cat("═══════════════════════════════════════════════\n\n")
  
  if (type == "two_modality") {
    
    mod_corr <- model |>
      spread_draws(b_Intercept, b_correction2M1, b_correction3M2, b_modality1) |>
      mutate(
        gesture_c0      = exp(b_Intercept + 0.5 * b_modality1),
        gesture_c1      = exp(b_Intercept + 0.5 * b_modality1 + b_correction2M1),
        gesture_c2      = exp(b_Intercept + 0.5 * b_modality1 + b_correction2M1 + b_correction3M2),
        multi_c0        = exp(b_Intercept - 0.5 * b_modality1),
        multi_c1        = exp(b_Intercept - 0.5 * b_modality1 + b_correction2M1),
        multi_c2        = exp(b_Intercept - 0.5 * b_modality1 + b_correction2M1 + b_correction3M2),
        gest_abs_c0_c1  = gesture_c1 - gesture_c0,
        gest_abs_c1_c2  = gesture_c2 - gesture_c1,
        multi_abs_c0_c1 = multi_c1   - multi_c0,
        multi_abs_c1_c2 = multi_c2   - multi_c1,
        diff_c0_pct     = (gesture_c0 / multi_c0 - 1) * 100,
        diff_c1_pct     = (gesture_c1 / multi_c1 - 1) * 100,
        diff_c2_pct     = (gesture_c2 / multi_c2 - 1) * 100
      )
    
    cat("  Predicted effort (median [95% HDI]):\n")
    print_result(mod_corr, "gesture_c0", "  Gesture c0:")
    print_result(mod_corr, "gesture_c1", "  Gesture c1:")
    print_result(mod_corr, "gesture_c2", "  Gesture c2:")
    print_result(mod_corr, "multi_c0",   "  Multimodal c0:")
    print_result(mod_corr, "multi_c1",   "  Multimodal c1:")
    print_result(mod_corr, "multi_c2",   "  Multimodal c2:")
    cat("\n  Absolute increase per step:\n")
    print_result(mod_corr, "gest_abs_c0_c1",  "  Gesture c0→c1:")
    print_result(mod_corr, "gest_abs_c1_c2",  "  Gesture c1→c2:")
    print_result(mod_corr, "multi_abs_c0_c1", "  Multimodal c0→c1:")
    print_result(mod_corr, "multi_abs_c1_c2", "  Multimodal c1→c2:")
    cat("\n  Gesture vs multimodal at each step (%):\n")
    print_result(mod_corr, "diff_c0_pct", "  c0:", pct = TRUE)
    print_result(mod_corr, "diff_c1_pct", "  c1:", pct = TRUE)
    print_result(mod_corr, "diff_c2_pct", "  c2:", pct = TRUE)
    
    traj_long <- mod_corr |>
      select(.draw, gesture_c0:multi_c2) |>
      tidyr::pivot_longer(
        cols     = gesture_c0:multi_c2,
        names_to = c("modality", "correction"), names_sep = "_",
        values_to = "effort"
      ) |>
      mutate(
        correction = factor(correction, levels = c("c0", "c1", "c2")),
        modality   = factor(modality, levels = c("gesture", "multi"),
                            labels = c("gesture", "multimodal"))
      )
    
  } else {
    
    mod_corr <- model |>
      spread_draws(b_Intercept, b_correction2M1, b_correction3M2,
                   b_modality1, b_modality2) |>
      mutate(
        vocal_offset    = -(b_modality1 + b_modality2),
        gesture_c0      = exp(b_Intercept + 0.5 * b_modality1),
        gesture_c1      = exp(b_Intercept + 0.5 * b_modality1 + b_correction2M1),
        gesture_c2      = exp(b_Intercept + 0.5 * b_modality1 + b_correction2M1 + b_correction3M2),
        multimodal_c0   = exp(b_Intercept + 0.5 * b_modality2),
        multimodal_c1   = exp(b_Intercept + 0.5 * b_modality2 + b_correction2M1),
        multimodal_c2   = exp(b_Intercept + 0.5 * b_modality2 + b_correction2M1 + b_correction3M2),
        vocal_c0        = exp(b_Intercept + 0.5 * vocal_offset),
        vocal_c1        = exp(b_Intercept + 0.5 * vocal_offset + b_correction2M1),
        vocal_c2        = exp(b_Intercept + 0.5 * vocal_offset + b_correction2M1 + b_correction3M2),
        gm_diff_c0_pct  = (gesture_c0    / multimodal_c0 - 1) * 100,
        gm_diff_c1_pct  = (gesture_c1    / multimodal_c1 - 1) * 100,
        gm_diff_c2_pct  = (gesture_c2    / multimodal_c2 - 1) * 100,
        gv_diff_c0_pct  = (gesture_c0    / vocal_c0      - 1) * 100,
        gv_diff_c1_pct  = (gesture_c1    / vocal_c1      - 1) * 100,
        gv_diff_c2_pct  = (gesture_c2    / vocal_c2      - 1) * 100,
        mv_diff_c0_pct  = (multimodal_c0 / vocal_c0      - 1) * 100,
        mv_diff_c1_pct  = (multimodal_c1 / vocal_c1      - 1) * 100,
        mv_diff_c2_pct  = (multimodal_c2 / vocal_c2      - 1) * 100
      )
    
    cat("  Predicted effort (median [95% HDI]):\n")
    print_result(mod_corr, "gesture_c0",    "  Gesture c0:")
    print_result(mod_corr, "gesture_c1",    "  Gesture c1:")
    print_result(mod_corr, "gesture_c2",    "  Gesture c2:")
    print_result(mod_corr, "multimodal_c0", "  Multimodal c0:")
    print_result(mod_corr, "multimodal_c1", "  Multimodal c1:")
    print_result(mod_corr, "multimodal_c2", "  Multimodal c2:")
    print_result(mod_corr, "vocal_c0",      "  Vocal c0:")
    print_result(mod_corr, "vocal_c1",      "  Vocal c1:")
    print_result(mod_corr, "vocal_c2",      "  Vocal c2:")
    cat("\n  Gesture vs Multimodal at each step (%):\n")
    print_result(mod_corr, "gm_diff_c0_pct", "  c0:", pct = TRUE)
    print_result(mod_corr, "gm_diff_c1_pct", "  c1:", pct = TRUE)
    print_result(mod_corr, "gm_diff_c2_pct", "  c2:", pct = TRUE)
    cat("\n  Gesture vs Vocal at each step (%):\n")
    print_result(mod_corr, "gv_diff_c0_pct", "  c0:", pct = TRUE)
    print_result(mod_corr, "gv_diff_c1_pct", "  c1:", pct = TRUE)
    print_result(mod_corr, "gv_diff_c2_pct", "  c2:", pct = TRUE)
    cat("\n  Multimodal vs Vocal at each step (%):\n")
    print_result(mod_corr, "mv_diff_c0_pct", "  c0:", pct = TRUE)
    print_result(mod_corr, "mv_diff_c1_pct", "  c1:", pct = TRUE)
    print_result(mod_corr, "mv_diff_c2_pct", "  c2:", pct = TRUE)
    
    traj_long <- mod_corr |>
      select(.draw, gesture_c0:vocal_c2) |>
      tidyr::pivot_longer(
        cols     = gesture_c0:vocal_c2,
        names_to = c("modality", "correction"), names_sep = "_",
        values_to = "effort"
      ) |>
      mutate(
        correction = factor(correction, levels = c("c0", "c1", "c2")),
        modality   = factor(modality, levels = c("gesture", "multimodal", "vocal"))
      )
  }
  
  traj_summary <- traj_long |>
    group_by(modality, correction) |>
    median_hdi(effort)
  
  plots$modality_trajectory <- traj_summary |>
    ggplot(aes(x = correction, y = effort,
               colour = modality, group = modality)) +
    geom_ribbon(aes(ymin = .lower, ymax = .upper, fill = modality),
                alpha = 0.15, colour = NA) +
    geom_line(linewidth = 1.2) +
    geom_point(size = 3) +
    scale_colour_manual(values = modality_colors, name = "Modality") +
    scale_fill_manual(values = modality_colors, guide = "none") +
    scale_y_log10(breaks = scales::pretty_breaks(n = 5)) +
    theme_effort_plot() +
    labs(title    = "Effort trajectory by modality",
         subtitle = "Median ± 95% HDI | log y-axis",
         x = "Correction phase", y = paste0(dv_label, " (log scale)"))
  
  # ── 3. Expressibility across corrections ───────────────────────────────────
  cat("\n═══════════════════════════════════════════════\n")
  cat("  3. EXPRESSIBILITY ACROSS CORRECTIONS (±1 SD and ±2 SD)\n")
  cat("═══════════════════════════════════════════════\n\n")
  
  expr_draws <- model |>
    spread_draws(b_Intercept, b_correction2M1, b_correction3M2,
                 b_expressibility_z) |>
    mutate(
      vlow_c0         = exp(b_Intercept + (-2) * b_expressibility_z),
      vlow_c1         = exp(b_Intercept + (-2) * b_expressibility_z + b_correction2M1),
      vlow_c2         = exp(b_Intercept + (-2) * b_expressibility_z + b_correction2M1 + b_correction3M2),
      low_c0          = exp(b_Intercept + (-1) * b_expressibility_z),
      low_c1          = exp(b_Intercept + (-1) * b_expressibility_z + b_correction2M1),
      low_c2          = exp(b_Intercept + (-1) * b_expressibility_z + b_correction2M1 + b_correction3M2),
      avg_c0          = exp(b_Intercept),
      avg_c1          = exp(b_Intercept + b_correction2M1),
      avg_c2          = exp(b_Intercept + b_correction2M1 + b_correction3M2),
      high_c0         = exp(b_Intercept +   1  * b_expressibility_z),
      high_c1         = exp(b_Intercept +   1  * b_expressibility_z + b_correction2M1),
      high_c2         = exp(b_Intercept +   1  * b_expressibility_z + b_correction2M1 + b_correction3M2),
      vhigh_c0        = exp(b_Intercept +   2  * b_expressibility_z),
      vhigh_c1        = exp(b_Intercept +   2  * b_expressibility_z + b_correction2M1),
      vhigh_c2        = exp(b_Intercept +   2  * b_expressibility_z + b_correction2M1 + b_correction3M2),
      diff_1sd_c0_pct = (low_c0  / high_c0  - 1) * 100,
      diff_1sd_c1_pct = (low_c1  / high_c1  - 1) * 100,
      diff_1sd_c2_pct = (low_c2  / high_c2  - 1) * 100,
      diff_2sd_c0_pct = (vlow_c0 / vhigh_c0 - 1) * 100,
      diff_2sd_c1_pct = (vlow_c1 / vhigh_c1 - 1) * 100,
      diff_2sd_c2_pct = (vlow_c2 / vhigh_c2 - 1) * 100,
      low_abs_c0_c1   = low_c1   - low_c0,
      low_abs_c1_c2   = low_c2   - low_c1,
      avg_abs_c0_c1   = avg_c1   - avg_c0,
      avg_abs_c1_c2   = avg_c2   - avg_c1,
      high_abs_c0_c1  = high_c1  - high_c0,
      high_abs_c1_c2  = high_c2  - high_c1
    )
  
  cat("  Predicted effort (median [95% HDI]):\n")
  print_result(expr_draws, "vlow_c0",  "  Very low (-2SD) c0:")
  print_result(expr_draws, "vlow_c1",  "  Very low (-2SD) c1:")
  print_result(expr_draws, "vlow_c2",  "  Very low (-2SD) c2:")
  print_result(expr_draws, "low_c0",   "  Low (-1SD) c0:")
  print_result(expr_draws, "low_c1",   "  Low (-1SD) c1:")
  print_result(expr_draws, "low_c2",   "  Low (-1SD) c2:")
  print_result(expr_draws, "avg_c0",   "  Average c0:")
  print_result(expr_draws, "avg_c1",   "  Average c1:")
  print_result(expr_draws, "avg_c2",   "  Average c2:")
  print_result(expr_draws, "high_c0",  "  High (+1SD) c0:")
  print_result(expr_draws, "high_c1",  "  High (+1SD) c1:")
  print_result(expr_draws, "high_c2",  "  High (+1SD) c2:")
  print_result(expr_draws, "vhigh_c0", "  Very high (+2SD) c0:")
  print_result(expr_draws, "vhigh_c1", "  Very high (+2SD) c1:")
  print_result(expr_draws, "vhigh_c2", "  Very high (+2SD) c2:")
  cat("\n  Absolute increase per step:\n")
  print_result(expr_draws, "low_abs_c0_c1",  "  Low  c0→c1:")
  print_result(expr_draws, "low_abs_c1_c2",  "  Low  c1→c2:")
  print_result(expr_draws, "avg_abs_c0_c1",  "  Avg  c0→c1:")
  print_result(expr_draws, "avg_abs_c1_c2",  "  Avg  c1→c2:")
  print_result(expr_draws, "high_abs_c0_c1", "  High c0→c1:")
  print_result(expr_draws, "high_abs_c1_c2", "  High c1→c2:")
  cat("\n  ±1 SD difference at each step (%):\n")
  print_result(expr_draws, "diff_1sd_c0_pct", "  c0:", pct = TRUE)
  print_result(expr_draws, "diff_1sd_c1_pct", "  c1:", pct = TRUE)
  print_result(expr_draws, "diff_1sd_c2_pct", "  c2:", pct = TRUE)
  cat("\n  ±2 SD difference at each step (%):\n")
  print_result(expr_draws, "diff_2sd_c0_pct", "  c0:", pct = TRUE)
  print_result(expr_draws, "diff_2sd_c1_pct", "  c1:", pct = TRUE)
  print_result(expr_draws, "diff_2sd_c2_pct", "  c2:", pct = TRUE)
  
  expr_traj <- expr_draws |>
    select(.draw, vlow_c0:vhigh_c2) |>
    tidyr::pivot_longer(
      cols     = vlow_c0:vhigh_c2,
      names_to = c("expressibility", "correction"), names_sep = "_",
      values_to = "effort"
    ) |>
    mutate(
      correction     = factor(correction, levels = c("c0", "c1", "c2")),
      expressibility = factor(expressibility,
                              levels = c("vlow", "low", "avg", "high", "vhigh"),
                              labels = c("−2 SD", "−1 SD", "Average", "+1 SD", "+2 SD"))
    )
  
  expr_summary <- expr_traj |>
    group_by(expressibility, correction) |>
    median_hdi(effort)
  
  plots$expressibility_trajectory <- expr_summary |>
    ggplot(aes(x = correction, y = effort,
               colour = expressibility, group = expressibility)) +
    geom_ribbon(aes(ymin = .lower, ymax = .upper, fill = expressibility),
                alpha = 0.12, colour = NA) +
    geom_line(linewidth = 1.2) +
    geom_point(size = 3) +
    scale_colour_manual(values = expr_palette, name = "Expressibility") +
    scale_fill_manual(values = expr_palette, guide = "none") +
    scale_y_log10(breaks = scales::pretty_breaks(n = 5)) +
    theme_effort_plot() +
    labs(title    = "Effort by expressibility across corrections",
         subtitle = "Median ± 95% HDI | log y-axis",
         x = "Correction phase", y = paste0(dv_label, " (log scale)"))
  
  # ── 4. Low vs high effort performers ───────────────────────────────────────
  cat("\n═══════════════════════════════════════════════\n")
  cat("  4. EFFORT PERFORMERS ON CORRECTION (±1 SD and ±2 SD)\n")
  cat("═══════════════════════════════════════════════\n\n")
  
  effort_draws <- model |>
    spread_draws(b_Intercept, b_correction2M1, b_correction3M2,
                 sd_pcn_ID__Intercept) |>
    mutate(
      vhigh_c0            = exp(b_Intercept + 2 * sd_pcn_ID__Intercept),
      vhigh_c1            = exp(b_Intercept + 2 * sd_pcn_ID__Intercept + b_correction2M1),
      vhigh_c2            = exp(b_Intercept + 2 * sd_pcn_ID__Intercept + b_correction2M1 + b_correction3M2),
      high_c0             = exp(b_Intercept + sd_pcn_ID__Intercept),
      high_c1             = exp(b_Intercept + sd_pcn_ID__Intercept + b_correction2M1),
      high_c2             = exp(b_Intercept + sd_pcn_ID__Intercept + b_correction2M1 + b_correction3M2),
      avg_c0              = exp(b_Intercept),
      avg_c1              = exp(b_Intercept + b_correction2M1),
      avg_c2              = exp(b_Intercept + b_correction2M1 + b_correction3M2),
      low_c0              = exp(b_Intercept - sd_pcn_ID__Intercept),
      low_c1              = exp(b_Intercept - sd_pcn_ID__Intercept + b_correction2M1),
      low_c2              = exp(b_Intercept - sd_pcn_ID__Intercept + b_correction2M1 + b_correction3M2),
      vlow_c0             = exp(b_Intercept - 2 * sd_pcn_ID__Intercept),
      vlow_c1             = exp(b_Intercept - 2 * sd_pcn_ID__Intercept + b_correction2M1),
      vlow_c2             = exp(b_Intercept - 2 * sd_pcn_ID__Intercept + b_correction2M1 + b_correction3M2),
      vhigh_abs_c0_c1     = vhigh_c1 - vhigh_c0,
      vhigh_abs_c1_c2     = vhigh_c2 - vhigh_c1,
      high_abs_c0_c1      = high_c1  - high_c0,
      high_abs_c1_c2      = high_c2  - high_c1,
      avg_abs_c0_c1       = avg_c1   - avg_c0,
      avg_abs_c1_c2       = avg_c2   - avg_c1,
      low_abs_c0_c1       = low_c1   - low_c0,
      low_abs_c1_c2       = low_c2   - low_c1,
      vlow_abs_c0_c1      = vlow_c1  - vlow_c0,
      vlow_abs_c1_c2      = vlow_c2  - vlow_c1,
      vhigh_vs_vlow_c0_c1 = vhigh_abs_c0_c1 - vlow_abs_c0_c1,
      vhigh_vs_vlow_c1_c2 = vhigh_abs_c1_c2 - vlow_abs_c1_c2
    )
  
  cat("  Predicted effort at each step (median [95% HDI]):\n")
  print_result(effort_draws, "vlow_c0",  "  Very low (-2SD) c0:")
  print_result(effort_draws, "vlow_c1",  "  Very low (-2SD) c1:")
  print_result(effort_draws, "vlow_c2",  "  Very low (-2SD) c2:")
  print_result(effort_draws, "low_c0",   "  Low (-1SD) c0:")
  print_result(effort_draws, "low_c1",   "  Low (-1SD) c1:")
  print_result(effort_draws, "low_c2",   "  Low (-1SD) c2:")
  print_result(effort_draws, "avg_c0",   "  Average c0:")
  print_result(effort_draws, "avg_c1",   "  Average c1:")
  print_result(effort_draws, "avg_c2",   "  Average c2:")
  print_result(effort_draws, "high_c0",  "  High (+1SD) c0:")
  print_result(effort_draws, "high_c1",  "  High (+1SD) c1:")
  print_result(effort_draws, "high_c2",  "  High (+1SD) c2:")
  print_result(effort_draws, "vhigh_c0", "  Very high (+2SD) c0:")
  print_result(effort_draws, "vhigh_c1", "  Very high (+2SD) c1:")
  print_result(effort_draws, "vhigh_c2", "  Very high (+2SD) c2:")
  cat("\n  Absolute increase per step:\n")
  print_result(effort_draws, "vlow_abs_c0_c1",  "  Very low  c0→c1:")
  print_result(effort_draws, "vlow_abs_c1_c2",  "  Very low  c1→c2:")
  print_result(effort_draws, "low_abs_c0_c1",   "  Low       c0→c1:")
  print_result(effort_draws, "low_abs_c1_c2",   "  Low       c1→c2:")
  print_result(effort_draws, "avg_abs_c0_c1",   "  Avg       c0→c1:")
  print_result(effort_draws, "avg_abs_c1_c2",   "  Avg       c1→c2:")
  print_result(effort_draws, "high_abs_c0_c1",  "  High      c0→c1:")
  print_result(effort_draws, "high_abs_c1_c2",  "  High      c1→c2:")
  print_result(effort_draws, "vhigh_abs_c0_c1", "  Very high c0→c1:")
  print_result(effort_draws, "vhigh_abs_c1_c2", "  Very high c1→c2:")
  cat("\n  Extra absolute effort very high vs very low performer:\n")
  print_result(effort_draws, "vhigh_vs_vlow_c0_c1", "  c0→c1:")
  print_result(effort_draws, "vhigh_vs_vlow_c1_c2", "  c1→c2:")
  
  effort_traj <- effort_draws |>
    select(.draw, vhigh_c0:vlow_c2) |>
    tidyr::pivot_longer(
      cols     = vhigh_c0:vlow_c2,
      names_to = c("group", "correction"), names_sep = "_",
      values_to = "effort"
    ) |>
    mutate(
      correction = factor(correction, levels = c("c0", "c1", "c2")),
      group      = factor(group,
                          levels = c("vlow", "low", "avg", "high", "vhigh"),
                          labels = c("−2 SD", "−1 SD", "Average", "+1 SD", "+2 SD"))
    )
  
  effort_summary <- effort_traj |>
    group_by(group, correction) |>
    median_hdi(effort)
  
  plots$effort_group_trajectory <- effort_summary |>
    ggplot(aes(x = correction, y = effort,
               colour = group, group = group)) +
    geom_ribbon(aes(ymin = .lower, ymax = .upper, fill = group),
                alpha = 0.12, colour = NA) +
    geom_line(linewidth = 1.2) +
    geom_point(size = 3) +
    scale_colour_manual(values = performer_palette, name = "Performer") +
    scale_fill_manual(values = performer_palette, guide = "none") +
    scale_y_log10(breaks = scales::pretty_breaks(n = 5)) +
    theme_effort_plot() +
    labs(title    = "Effort trajectory by performer level",
         subtitle = "Median ± 95% HDI | ±1/2 SD participant intercept | log y-axis",
         x = "Correction phase", y = paste0(dv_label, " (log scale)"))
  
  # ── 5. Variance decomposition ───────────────────────────────────────────────
  cat("\n═══════════════════════════════════════════════\n")
  cat("  5. VARIANCE DECOMPOSITION\n")
  cat("═══════════════════════════════════════════════\n\n")
  
  var_draws <- model |>
    spread_draws(sd_pcn_ID__Intercept,
                 sd_concept__Intercept,
                 sd_SessionID__Intercept,
                 sigma) |>
    mutate(
      var_participant = sd_pcn_ID__Intercept^2,
      var_concept     = sd_concept__Intercept^2,
      var_dyad        = sd_SessionID__Intercept^2,
      var_residual    = sigma^2,
      var_total       = var_participant + var_concept + var_dyad + var_residual,
      pct_participant = var_participant / var_total * 100,
      pct_concept     = var_concept     / var_total * 100,
      pct_dyad        = var_dyad        / var_total * 100,
      pct_residual    = var_residual    / var_total * 100
    )
  
  purrr::walk(
    list(
      list(var = "pct_participant", label = "Participant"),
      list(var = "pct_concept",     label = "Concept    "),
      list(var = "pct_dyad",        label = "Dyad       "),
      list(var = "pct_residual",    label = "Residual   ")
    ),
    function(x) print_result(var_draws, x$var, x$label, pct = TRUE)
  )
  
  var_long <- var_draws |>
    select(.draw, pct_participant, pct_concept, pct_dyad, pct_residual) |>
    tidyr::pivot_longer(
      cols     = pct_participant:pct_residual,
      names_to = "component", values_to = "pct"
    ) |>
    mutate(component = factor(component,
                              levels = c("pct_concept", "pct_dyad", "pct_participant", "pct_residual"),
                              labels = c("Concept", "Dyad", "Participant", "Residual")
    ))
  
  plots$variance_decomp <- var_long |>
    ggplot(aes(x = pct, y = component, fill = component)) +
    stat_halfeye(
      .width = c(0.89, 0.95), point_interval = median_hdi,
      normalize = "groups", scale = 0.7, alpha = 0.85
    ) +
    geom_vline(xintercept = 0, linetype = "dashed",
               colour = "grey20", linewidth = 0.8) +
    scale_fill_manual(
      values = c("Concept"     = "#9C27B0",
                 "Dyad"        = "#E91E63",
                 "Participant" = "#FF9800",
                 "Residual"    = "#607D8B"),
      guide = "none"
    ) +
    scale_x_continuous(limits = c(0, NA),
                       breaks = scales::pretty_breaks(n = 5),
                       labels = function(x) paste0(x, "%")) +
    theme_effort_plot() +
    labs(title    = "Variance decomposition",
         subtitle = "Posterior of % variance by grouping level",
         x = "% of total variance", y = NULL)
  
  # ── Assemble grid ───────────────────────────────────────────────────────────
  top_row    <- plots$modality_diff             | plots$modality_trajectory
  middle_row <- plots$expressibility_trajectory | plots$effort_group_trajectory
  bottom_row <- plot_spacer() | wrap_elements(plots$variance_decomp) | plot_spacer()
  
  final <- (top_row / middle_row / bottom_row) +
    plot_layout(heights = c(0.8, 0.8, 1)) +
    plot_annotation(
      title    = paste0("Targeted comparisons — ", dv_label),
      subtitle = "Median ± 89% and 95% HDI",
      theme    = theme(
        plot.title    = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 11, colour = "grey40")
      )
    )
  
  print(final)
  
  ggsave(paste0("targeted_comparisons_",
                gsub("[^a-zA-Z0-9]", "_", dv_label), ".png"),
         final, width = 14, height = 12, dpi = 300, bg = "white")
  
  invisible(plots)
}


##########

targeted_comparisons_bfi <- function(model, type = c("two_modality", "three_modality"),
                                     dv_label = "Effort") {
  
  type <- match.arg(type)
  
  # ── Helper: print formatted result ─────────────────────────────────────────
  print_result <- function(draws, var, label, pct = FALSE, digits = 1) {
    res  <- draws |> median_hdi(!!sym(var))
    est  <- res[[var]]
    lo   <- res$.lower
    hi   <- res$.upper
    cred <- lo > 0 | hi < 0
    if (pct) {
      cat(sprintf("  %-45s %+.1f%% [%+.1f%%, %+.1f%%]%s\n",
                  label, est, lo, hi, ifelse(cred, "  *", "")))
    } else {
      cat(sprintf("  %-45s %.3f [%.3f, %.3f]%s\n",
                  label, est, lo, hi, ifelse(cred, "  *", "")))
    }
  }
  
  modality_colors <- c(
    "gesture"    = "#2196F3",
    "vocal"      = "#FF9800",
    "multimodal" = "#4CAF50"
  )
  
  bfi_palette <- c(
    "−2 SD"   = "#1A237E",
    "−1 SD"   = "#3949AB",
    "Average" = "#795548",
    "+1 SD"   = "#8D6E63",
    "+2 SD"   = "#4E342E"
  )
  
  performer_palette <- c(
    "−2 SD"   = "#37474F",
    "−1 SD"   = "#607D8B",
    "Average" = "#000000",
    "+1 SD"   = "#E53935",
    "+2 SD"   = "#B71C1C"
  )
  
  plots <- list()
  
  # ── 1. Direct modality comparisons ─────────────────────────────────────────
  cat("═══════════════════════════════════════════════\n")
  cat("  1. MODALITY COMPARISONS (main effect)\n")
  cat("═══════════════════════════════════════════════\n\n")
  
  if (type == "two_modality") {
    
    diff_draws <- model |>
      spread_draws(b_modality1) |>
      mutate(
        diff     = 2 * b_modality1,
        diff_pct = (exp(diff) - 1) * 100
      )
    
    print_result(diff_draws, "diff",     "Gesture vs Multimodal (log scale):")
    print_result(diff_draws, "diff_pct", "Gesture vs Multimodal (%):", pct = TRUE)
    
    plots$modality_diff <- diff_draws |>
      ggplot(aes(x = diff_pct, fill = after_stat(x > 0))) +
      stat_halfeye(
        .width = c(0.89, 0.95), point_interval = median_hdi,
        normalize = "none", alpha = 0.85
      ) +
      geom_vline(xintercept = 0, linetype = "dashed",
                 colour = "grey20", linewidth = 0.8) +
      scale_fill_manual(
        values = c("FALSE" = "#4CAF50", "TRUE" = "#2196F3"),
        guide  = "none"
      ) +
      theme_effort_plot() +
      labs(title    = "Gesture vs Multimodal",
           subtitle = "Posterior of % difference",
           x = "% difference", y = NULL)
    
  } else {
    
    diff_draws <- model |>
      spread_draws(b_modality1, b_modality2) |>
      mutate(
        vocal_offset          = -(b_modality1 + b_modality2),
        gesture_vs_multimodal = (exp(b_modality1 - b_modality2)  - 1) * 100,
        gesture_vs_vocal      = (exp(b_modality1 - vocal_offset) - 1) * 100,
        multimodal_vs_vocal   = (exp(b_modality2 - vocal_offset) - 1) * 100
      )
    
    print_result(diff_draws, "gesture_vs_multimodal", "Gesture vs Multimodal (%):", pct = TRUE)
    print_result(diff_draws, "gesture_vs_vocal",      "Gesture vs Vocal (%):",      pct = TRUE)
    print_result(diff_draws, "multimodal_vs_vocal",   "Multimodal vs Vocal (%):",   pct = TRUE)
    
    plots$modality_diff <- diff_draws |>
      select(.draw, gesture_vs_multimodal, gesture_vs_vocal, multimodal_vs_vocal) |>
      tidyr::pivot_longer(
        cols     = c(gesture_vs_multimodal, gesture_vs_vocal, multimodal_vs_vocal),
        names_to = "comparison", values_to = "pct_diff"
      ) |>
      mutate(comparison = factor(comparison,
                                 levels = c("gesture_vs_vocal", "multimodal_vs_vocal", "gesture_vs_multimodal"),
                                 labels = c("Gesture vs Vocal", "Multimodal vs Vocal", "Gesture vs Multimodal")
      )) |>
      ggplot(aes(x = pct_diff, y = comparison, fill = comparison)) +
      stat_halfeye(
        .width = c(0.89, 0.95), point_interval = median_hdi,
        normalize = "groups", scale = 0.7, alpha = 0.85
      ) +
      geom_vline(xintercept = 0, linetype = "dashed",
                 colour = "grey20", linewidth = 0.8) +
      scale_fill_manual(
        values = c("Gesture vs Vocal"      = "#2196F3",
                   "Multimodal vs Vocal"   = "#4CAF50",
                   "Gesture vs Multimodal" = "#9C27B0"),
        guide = "none"
      ) +
      theme_effort_plot() +
      labs(title    = "Pairwise modality comparisons",
           subtitle = "Posterior of % differences",
           x = "% difference", y = NULL)
  }
  
  # ── 2. Effort trajectory by modality across corrections ────────────────────
  cat("\n═══════════════════════════════════════════════\n")
  cat("  2. EFFORT BY MODALITY ACROSS CORRECTIONS\n")
  cat("═══════════════════════════════════════════════\n\n")
  
  if (type == "two_modality") {
    
    mod_corr <- model |>
      spread_draws(b_Intercept, b_correction2M1, b_correction3M2, b_modality1) |>
      mutate(
        gesture_c0      = exp(b_Intercept + 0.5 * b_modality1),
        gesture_c1      = exp(b_Intercept + 0.5 * b_modality1 + b_correction2M1),
        gesture_c2      = exp(b_Intercept + 0.5 * b_modality1 + b_correction2M1 + b_correction3M2),
        multi_c0        = exp(b_Intercept - 0.5 * b_modality1),
        multi_c1        = exp(b_Intercept - 0.5 * b_modality1 + b_correction2M1),
        multi_c2        = exp(b_Intercept - 0.5 * b_modality1 + b_correction2M1 + b_correction3M2),
        gest_abs_c0_c1  = gesture_c1 - gesture_c0,
        gest_abs_c1_c2  = gesture_c2 - gesture_c1,
        multi_abs_c0_c1 = multi_c1   - multi_c0,
        multi_abs_c1_c2 = multi_c2   - multi_c1,
        diff_c0_pct     = (gesture_c0 / multi_c0 - 1) * 100,
        diff_c1_pct     = (gesture_c1 / multi_c1 - 1) * 100,
        diff_c2_pct     = (gesture_c2 / multi_c2 - 1) * 100
      )
    
    cat("  Predicted effort (median [95% HDI]):\n")
    print_result(mod_corr, "gesture_c0", "  Gesture c0:")
    print_result(mod_corr, "gesture_c1", "  Gesture c1:")
    print_result(mod_corr, "gesture_c2", "  Gesture c2:")
    print_result(mod_corr, "multi_c0",   "  Multimodal c0:")
    print_result(mod_corr, "multi_c1",   "  Multimodal c1:")
    print_result(mod_corr, "multi_c2",   "  Multimodal c2:")
    cat("\n  Absolute increase per step:\n")
    print_result(mod_corr, "gest_abs_c0_c1",  "  Gesture c0→c1:")
    print_result(mod_corr, "gest_abs_c1_c2",  "  Gesture c1→c2:")
    print_result(mod_corr, "multi_abs_c0_c1", "  Multimodal c0→c1:")
    print_result(mod_corr, "multi_abs_c1_c2", "  Multimodal c1→c2:")
    cat("\n  Gesture vs multimodal at each step (%):\n")
    print_result(mod_corr, "diff_c0_pct", "  c0:", pct = TRUE)
    print_result(mod_corr, "diff_c1_pct", "  c1:", pct = TRUE)
    print_result(mod_corr, "diff_c2_pct", "  c2:", pct = TRUE)
    
    traj_long <- mod_corr |>
      select(.draw, gesture_c0:multi_c2) |>
      tidyr::pivot_longer(
        cols     = gesture_c0:multi_c2,
        names_to = c("modality", "correction"), names_sep = "_",
        values_to = "effort"
      ) |>
      mutate(
        correction = factor(correction, levels = c("c0", "c1", "c2")),
        modality   = factor(modality, levels = c("gesture", "multi"),
                            labels = c("gesture", "multimodal"))
      )
    
  } else {
    
    mod_corr <- model |>
      spread_draws(b_Intercept, b_correction2M1, b_correction3M2,
                   b_modality1, b_modality2) |>
      mutate(
        vocal_offset    = -(b_modality1 + b_modality2),
        gesture_c0      = exp(b_Intercept + 0.5 * b_modality1),
        gesture_c1      = exp(b_Intercept + 0.5 * b_modality1 + b_correction2M1),
        gesture_c2      = exp(b_Intercept + 0.5 * b_modality1 + b_correction2M1 + b_correction3M2),
        multimodal_c0   = exp(b_Intercept + 0.5 * b_modality2),
        multimodal_c1   = exp(b_Intercept + 0.5 * b_modality2 + b_correction2M1),
        multimodal_c2   = exp(b_Intercept + 0.5 * b_modality2 + b_correction2M1 + b_correction3M2),
        vocal_c0        = exp(b_Intercept + 0.5 * vocal_offset),
        vocal_c1        = exp(b_Intercept + 0.5 * vocal_offset + b_correction2M1),
        vocal_c2        = exp(b_Intercept + 0.5 * vocal_offset + b_correction2M1 + b_correction3M2),
        gm_diff_c0_pct  = (gesture_c0    / multimodal_c0 - 1) * 100,
        gm_diff_c1_pct  = (gesture_c1    / multimodal_c1 - 1) * 100,
        gm_diff_c2_pct  = (gesture_c2    / multimodal_c2 - 1) * 100,
        gv_diff_c0_pct  = (gesture_c0    / vocal_c0      - 1) * 100,
        gv_diff_c1_pct  = (gesture_c1    / vocal_c1      - 1) * 100,
        gv_diff_c2_pct  = (gesture_c2    / vocal_c2      - 1) * 100,
        mv_diff_c0_pct  = (multimodal_c0 / vocal_c0      - 1) * 100,
        mv_diff_c1_pct  = (multimodal_c1 / vocal_c1      - 1) * 100,
        mv_diff_c2_pct  = (multimodal_c2 / vocal_c2      - 1) * 100
      )
    
    cat("  Predicted effort (median [95% HDI]):\n")
    print_result(mod_corr, "gesture_c0",    "  Gesture c0:")
    print_result(mod_corr, "gesture_c1",    "  Gesture c1:")
    print_result(mod_corr, "gesture_c2",    "  Gesture c2:")
    print_result(mod_corr, "multimodal_c0", "  Multimodal c0:")
    print_result(mod_corr, "multimodal_c1", "  Multimodal c1:")
    print_result(mod_corr, "multimodal_c2", "  Multimodal c2:")
    print_result(mod_corr, "vocal_c0",      "  Vocal c0:")
    print_result(mod_corr, "vocal_c1",      "  Vocal c1:")
    print_result(mod_corr, "vocal_c2",      "  Vocal c2:")
    cat("\n  Gesture vs Multimodal at each step (%):\n")
    print_result(mod_corr, "gm_diff_c0_pct", "  c0:", pct = TRUE)
    print_result(mod_corr, "gm_diff_c1_pct", "  c1:", pct = TRUE)
    print_result(mod_corr, "gm_diff_c2_pct", "  c2:", pct = TRUE)
    cat("\n  Gesture vs Vocal at each step (%):\n")
    print_result(mod_corr, "gv_diff_c0_pct", "  c0:", pct = TRUE)
    print_result(mod_corr, "gv_diff_c1_pct", "  c1:", pct = TRUE)
    print_result(mod_corr, "gv_diff_c2_pct", "  c2:", pct = TRUE)
    cat("\n  Multimodal vs Vocal at each step (%):\n")
    print_result(mod_corr, "mv_diff_c0_pct", "  c0:", pct = TRUE)
    print_result(mod_corr, "mv_diff_c1_pct", "  c1:", pct = TRUE)
    print_result(mod_corr, "mv_diff_c2_pct", "  c2:", pct = TRUE)
    
    traj_long <- mod_corr |>
      select(.draw, gesture_c0:vocal_c2) |>
      tidyr::pivot_longer(
        cols     = gesture_c0:vocal_c2,
        names_to = c("modality", "correction"), names_sep = "_",
        values_to = "effort"
      ) |>
      mutate(
        correction = factor(correction, levels = c("c0", "c1", "c2")),
        modality   = factor(modality, levels = c("gesture", "multimodal", "vocal"))
      )
  }
  
  traj_summary <- traj_long |>
    group_by(modality, correction) |>
    median_hdi(effort)
  
  plots$modality_trajectory <- traj_summary |>
    ggplot(aes(x = correction, y = effort,
               colour = modality, group = modality)) +
    geom_ribbon(aes(ymin = .lower, ymax = .upper, fill = modality),
                alpha = 0.15, colour = NA) +
    geom_line(linewidth = 1.2) +
    geom_point(size = 3) +
    scale_colour_manual(values = modality_colors, name = "Modality") +
    scale_fill_manual(values = modality_colors, guide = "none") +
    scale_y_log10(breaks = scales::pretty_breaks(n = 5)) +
    theme_effort_plot() +
    labs(title    = "Effort trajectory by modality",
         subtitle = "Median ± 95% HDI | log y-axis",
         x = "Correction phase", y = paste0(dv_label, " (log scale)"))
  
  # ── 3. BFI extraversion across corrections ─────────────────────────────────
  cat("\n═══════════════════════════════════════════════\n")
  cat("  3. BFI EXTRAVERSION ACROSS CORRECTIONS (±1 SD and ±2 SD)\n")
  cat("═══════════════════════════════════════════════\n\n")
  
  bfi_draws <- model |>
    spread_draws(b_Intercept, b_correction2M1, b_correction3M2,
                 b_BFI_extra) |>
    mutate(
      vlow_c0         = exp(b_Intercept + (-2) * b_BFI_extra),
      vlow_c1         = exp(b_Intercept + (-2) * b_BFI_extra + b_correction2M1),
      vlow_c2         = exp(b_Intercept + (-2) * b_BFI_extra + b_correction2M1 + b_correction3M2),
      low_c0          = exp(b_Intercept + (-1) * b_BFI_extra),
      low_c1          = exp(b_Intercept + (-1) * b_BFI_extra + b_correction2M1),
      low_c2          = exp(b_Intercept + (-1) * b_BFI_extra + b_correction2M1 + b_correction3M2),
      avg_c0          = exp(b_Intercept),
      avg_c1          = exp(b_Intercept + b_correction2M1),
      avg_c2          = exp(b_Intercept + b_correction2M1 + b_correction3M2),
      high_c0         = exp(b_Intercept +   1  * b_BFI_extra),
      high_c1         = exp(b_Intercept +   1  * b_BFI_extra + b_correction2M1),
      high_c2         = exp(b_Intercept +   1  * b_BFI_extra + b_correction2M1 + b_correction3M2),
      vhigh_c0        = exp(b_Intercept +   2  * b_BFI_extra),
      vhigh_c1        = exp(b_Intercept +   2  * b_BFI_extra + b_correction2M1),
      vhigh_c2        = exp(b_Intercept +   2  * b_BFI_extra + b_correction2M1 + b_correction3M2),
      diff_1sd_c0_pct = (low_c0  / high_c0  - 1) * 100,
      diff_1sd_c1_pct = (low_c1  / high_c1  - 1) * 100,
      diff_1sd_c2_pct = (low_c2  / high_c2  - 1) * 100,
      diff_2sd_c0_pct = (vlow_c0 / vhigh_c0 - 1) * 100,
      diff_2sd_c1_pct = (vlow_c1 / vhigh_c1 - 1) * 100,
      diff_2sd_c2_pct = (vlow_c2 / vhigh_c2 - 1) * 100,
      low_abs_c0_c1   = low_c1   - low_c0,
      low_abs_c1_c2   = low_c2   - low_c1,
      avg_abs_c0_c1   = avg_c1   - avg_c0,
      avg_abs_c1_c2   = avg_c2   - avg_c1,
      high_abs_c0_c1  = high_c1  - high_c0,
      high_abs_c1_c2  = high_c2  - high_c1
    )
  
  cat("  Predicted effort (median [95% HDI]):\n")
  print_result(bfi_draws, "vlow_c0",  "  Very low BFI (-2SD) c0:")
  print_result(bfi_draws, "vlow_c1",  "  Very low BFI (-2SD) c1:")
  print_result(bfi_draws, "vlow_c2",  "  Very low BFI (-2SD) c2:")
  print_result(bfi_draws, "low_c0",   "  Low BFI (-1SD) c0:")
  print_result(bfi_draws, "low_c1",   "  Low BFI (-1SD) c1:")
  print_result(bfi_draws, "low_c2",   "  Low BFI (-1SD) c2:")
  print_result(bfi_draws, "avg_c0",   "  Average BFI c0:")
  print_result(bfi_draws, "avg_c1",   "  Average BFI c1:")
  print_result(bfi_draws, "avg_c2",   "  Average BFI c2:")
  print_result(bfi_draws, "high_c0",  "  High BFI (+1SD) c0:")
  print_result(bfi_draws, "high_c1",  "  High BFI (+1SD) c1:")
  print_result(bfi_draws, "high_c2",  "  High BFI (+1SD) c2:")
  print_result(bfi_draws, "vhigh_c0", "  Very high BFI (+2SD) c0:")
  print_result(bfi_draws, "vhigh_c1", "  Very high BFI (+2SD) c1:")
  print_result(bfi_draws, "vhigh_c2", "  Very high BFI (+2SD) c2:")
  cat("\n  Absolute increase per step:\n")
  print_result(bfi_draws, "low_abs_c0_c1",  "  Low BFI  c0→c1:")
  print_result(bfi_draws, "low_abs_c1_c2",  "  Low BFI  c1→c2:")
  print_result(bfi_draws, "avg_abs_c0_c1",  "  Avg BFI  c0→c1:")
  print_result(bfi_draws, "avg_abs_c1_c2",  "  Avg BFI  c1→c2:")
  print_result(bfi_draws, "high_abs_c0_c1", "  High BFI c0→c1:")
  print_result(bfi_draws, "high_abs_c1_c2", "  High BFI c1→c2:")
  cat("\n  ±1 SD difference at each step (%):\n")
  print_result(bfi_draws, "diff_1sd_c0_pct", "  c0:", pct = TRUE)
  print_result(bfi_draws, "diff_1sd_c1_pct", "  c1:", pct = TRUE)
  print_result(bfi_draws, "diff_1sd_c2_pct", "  c2:", pct = TRUE)
  cat("\n  ±2 SD difference at each step (%):\n")
  print_result(bfi_draws, "diff_2sd_c0_pct", "  c0:", pct = TRUE)
  print_result(bfi_draws, "diff_2sd_c1_pct", "  c1:", pct = TRUE)
  print_result(bfi_draws, "diff_2sd_c2_pct", "  c2:", pct = TRUE)
  
  bfi_traj <- bfi_draws |>
    select(.draw, vlow_c0:vhigh_c2) |>
    tidyr::pivot_longer(
      cols     = vlow_c0:vhigh_c2,
      names_to = c("bfi", "correction"), names_sep = "_",
      values_to = "effort"
    ) |>
    mutate(
      correction = factor(correction, levels = c("c0", "c1", "c2")),
      bfi        = factor(bfi,
                          levels = c("vlow", "low", "avg", "high", "vhigh"),
                          labels = c("−2 SD", "−1 SD", "Average", "+1 SD", "+2 SD"))
    )
  
  bfi_summary <- bfi_traj |>
    group_by(bfi, correction) |>
    median_hdi(effort)
  
  plots$bfi_trajectory <- bfi_summary |>
    ggplot(aes(x = correction, y = effort,
               colour = bfi, group = bfi)) +
    geom_ribbon(aes(ymin = .lower, ymax = .upper, fill = bfi),
                alpha = 0.12, colour = NA) +
    geom_line(linewidth = 1.2) +
    geom_point(size = 3) +
    scale_colour_manual(values = bfi_palette, name = "BFI Extraversion") +
    scale_fill_manual(values = bfi_palette, guide = "none") +
    scale_y_log10(breaks = scales::pretty_breaks(n = 5)) +
    theme_effort_plot() +
    labs(title    = "Effort by BFI Extraversion across corrections",
         subtitle = "Median ± 95% HDI | log y-axis",
         x = "Correction phase", y = paste0(dv_label, " (log scale)"))
  
  # ── 4. Low vs high effort performers ───────────────────────────────────────
  cat("\n═══════════════════════════════════════════════\n")
  cat("  4. EFFORT PERFORMERS ON CORRECTION (±1 SD and ±2 SD)\n")
  cat("═══════════════════════════════════════════════\n\n")
  
  effort_draws <- model |>
    spread_draws(b_Intercept, b_correction2M1, b_correction3M2,
                 sd_pcn_ID__Intercept) |>
    mutate(
      vhigh_c0            = exp(b_Intercept + 2 * sd_pcn_ID__Intercept),
      vhigh_c1            = exp(b_Intercept + 2 * sd_pcn_ID__Intercept + b_correction2M1),
      vhigh_c2            = exp(b_Intercept + 2 * sd_pcn_ID__Intercept + b_correction2M1 + b_correction3M2),
      high_c0             = exp(b_Intercept + sd_pcn_ID__Intercept),
      high_c1             = exp(b_Intercept + sd_pcn_ID__Intercept + b_correction2M1),
      high_c2             = exp(b_Intercept + sd_pcn_ID__Intercept + b_correction2M1 + b_correction3M2),
      avg_c0              = exp(b_Intercept),
      avg_c1              = exp(b_Intercept + b_correction2M1),
      avg_c2              = exp(b_Intercept + b_correction2M1 + b_correction3M2),
      low_c0              = exp(b_Intercept - sd_pcn_ID__Intercept),
      low_c1              = exp(b_Intercept - sd_pcn_ID__Intercept + b_correction2M1),
      low_c2              = exp(b_Intercept - sd_pcn_ID__Intercept + b_correction2M1 + b_correction3M2),
      vlow_c0             = exp(b_Intercept - 2 * sd_pcn_ID__Intercept),
      vlow_c1             = exp(b_Intercept - 2 * sd_pcn_ID__Intercept + b_correction2M1),
      vlow_c2             = exp(b_Intercept - 2 * sd_pcn_ID__Intercept + b_correction2M1 + b_correction3M2),
      vhigh_abs_c0_c1     = vhigh_c1 - vhigh_c0,
      vhigh_abs_c1_c2     = vhigh_c2 - vhigh_c1,
      high_abs_c0_c1      = high_c1  - high_c0,
      high_abs_c1_c2      = high_c2  - high_c1,
      avg_abs_c0_c1       = avg_c1   - avg_c0,
      avg_abs_c1_c2       = avg_c2   - avg_c1,
      low_abs_c0_c1       = low_c1   - low_c0,
      low_abs_c1_c2       = low_c2   - low_c1,
      vlow_abs_c0_c1      = vlow_c1  - vlow_c0,
      vlow_abs_c1_c2      = vlow_c2  - vlow_c1,
      vhigh_vs_vlow_c0_c1 = vhigh_abs_c0_c1 - vlow_abs_c0_c1,
      vhigh_vs_vlow_c1_c2 = vhigh_abs_c1_c2 - vlow_abs_c1_c2
    )
  
  cat("  Predicted effort at each step (median [95% HDI]):\n")
  print_result(effort_draws, "vlow_c0",  "  Very low (-2SD) c0:")
  print_result(effort_draws, "vlow_c1",  "  Very low (-2SD) c1:")
  print_result(effort_draws, "vlow_c2",  "  Very low (-2SD) c2:")
  print_result(effort_draws, "low_c0",   "  Low (-1SD) c0:")
  print_result(effort_draws, "low_c1",   "  Low (-1SD) c1:")
  print_result(effort_draws, "low_c2",   "  Low (-1SD) c2:")
  print_result(effort_draws, "avg_c0",   "  Average c0:")
  print_result(effort_draws, "avg_c1",   "  Average c1:")
  print_result(effort_draws, "avg_c2",   "  Average c2:")
  print_result(effort_draws, "high_c0",  "  High (+1SD) c0:")
  print_result(effort_draws, "high_c1",  "  High (+1SD) c1:")
  print_result(effort_draws, "high_c2",  "  High (+1SD) c2:")
  print_result(effort_draws, "vhigh_c0", "  Very high (+2SD) c0:")
  print_result(effort_draws, "vhigh_c1", "  Very high (+2SD) c1:")
  print_result(effort_draws, "vhigh_c2", "  Very high (+2SD) c2:")
  cat("\n  Absolute increase per step:\n")
  print_result(effort_draws, "vlow_abs_c0_c1",  "  Very low  c0→c1:")
  print_result(effort_draws, "vlow_abs_c1_c2",  "  Very low  c1→c2:")
  print_result(effort_draws, "low_abs_c0_c1",   "  Low       c0→c1:")
  print_result(effort_draws, "low_abs_c1_c2",   "  Low       c1→c2:")
  print_result(effort_draws, "avg_abs_c0_c1",   "  Avg       c0→c1:")
  print_result(effort_draws, "avg_abs_c1_c2",   "  Avg       c1→c2:")
  print_result(effort_draws, "high_abs_c0_c1",  "  High      c0→c1:")
  print_result(effort_draws, "high_abs_c1_c2",  "  High      c1→c2:")
  print_result(effort_draws, "vhigh_abs_c0_c1", "  Very high c0→c1:")
  print_result(effort_draws, "vhigh_abs_c1_c2", "  Very high c1→c2:")
  cat("\n  Extra absolute effort very high vs very low performer:\n")
  print_result(effort_draws, "vhigh_vs_vlow_c0_c1", "  c0→c1:")
  print_result(effort_draws, "vhigh_vs_vlow_c1_c2", "  c1→c2:")
  
  effort_traj <- effort_draws |>
    select(.draw, vhigh_c0:vlow_c2) |>
    tidyr::pivot_longer(
      cols     = vhigh_c0:vlow_c2,
      names_to = c("group", "correction"), names_sep = "_",
      values_to = "effort"
    ) |>
    mutate(
      correction = factor(correction, levels = c("c0", "c1", "c2")),
      group      = factor(group,
                          levels = c("vlow", "low", "avg", "high", "vhigh"),
                          labels = c("−2 SD", "−1 SD", "Average", "+1 SD", "+2 SD"))
    )
  
  effort_summary <- effort_traj |>
    group_by(group, correction) |>
    median_hdi(effort)
  
  plots$effort_group_trajectory <- effort_summary |>
    ggplot(aes(x = correction, y = effort,
               colour = group, group = group)) +
    geom_ribbon(aes(ymin = .lower, ymax = .upper, fill = group),
                alpha = 0.12, colour = NA) +
    geom_line(linewidth = 1.2) +
    geom_point(size = 3) +
    scale_colour_manual(values = performer_palette, name = "Performer") +
    scale_fill_manual(values = performer_palette, guide = "none") +
    scale_y_log10(breaks = scales::pretty_breaks(n = 5)) +
    theme_effort_plot() +
    labs(title    = "Effort trajectory by performer level",
         subtitle = "Median ± 95% HDI | ±1/2 SD participant intercept | log y-axis",
         x = "Correction phase", y = paste0(dv_label, " (log scale)"))
  
  # ── 5. Variance decomposition ───────────────────────────────────────────────
  cat("\n═══════════════════════════════════════════════\n")
  cat("  5. VARIANCE DECOMPOSITION\n")
  cat("═══════════════════════════════════════════════\n\n")
  
  var_draws <- model |>
    spread_draws(sd_pcn_ID__Intercept,
                 sd_concept__Intercept,
                 sd_SessionID__Intercept,
                 sigma) |>
    mutate(
      var_participant = sd_pcn_ID__Intercept^2,
      var_concept     = sd_concept__Intercept^2,
      var_dyad        = sd_SessionID__Intercept^2,
      var_residual    = sigma^2,
      var_total       = var_participant + var_concept + var_dyad + var_residual,
      pct_participant = var_participant / var_total * 100,
      pct_concept     = var_concept     / var_total * 100,
      pct_dyad        = var_dyad        / var_total * 100,
      pct_residual    = var_residual    / var_total * 100
    )
  
  purrr::walk(
    list(
      list(var = "pct_participant", label = "Participant"),
      list(var = "pct_concept",     label = "Concept    "),
      list(var = "pct_dyad",        label = "Dyad       "),
      list(var = "pct_residual",    label = "Residual   ")
    ),
    function(x) print_result(var_draws, x$var, x$label, pct = TRUE)
  )
  
  var_long <- var_draws |>
    select(.draw, pct_participant, pct_concept, pct_dyad, pct_residual) |>
    tidyr::pivot_longer(
      cols     = pct_participant:pct_residual,
      names_to = "component", values_to = "pct"
    ) |>
    mutate(component = factor(component,
                              levels = c("pct_concept", "pct_dyad", "pct_participant", "pct_residual"),
                              labels = c("Concept", "Dyad", "Participant", "Residual")
    ))
  
  plots$variance_decomp <- var_long |>
    ggplot(aes(x = pct, y = component, fill = component)) +
    stat_halfeye(
      .width = c(0.89, 0.95), point_interval = median_hdi,
      normalize = "groups", scale = 0.7, alpha = 0.85
    ) +
    geom_vline(xintercept = 0, linetype = "dashed",
               colour = "grey20", linewidth = 0.8) +
    scale_fill_manual(
      values = c("Concept"     = "#9C27B0",
                 "Dyad"        = "#E91E63",
                 "Participant" = "#FF9800",
                 "Residual"    = "#607D8B"),
      guide = "none"
    ) +
    scale_x_continuous(limits = c(0, NA),
                       breaks = scales::pretty_breaks(n = 5),
                       labels = function(x) paste0(x, "%")) +
    theme_effort_plot() +
    labs(title    = "Variance decomposition",
         subtitle = "Posterior of % variance by grouping level",
         x = "% of total variance", y = NULL)
  
  # ── Assemble grid ───────────────────────────────────────────────────────────
  top_row    <- plots$modality_diff        | plots$modality_trajectory
  middle_row <- plots$bfi_trajectory       | plots$effort_group_trajectory
  bottom_row <- plot_spacer() | wrap_elements(plots$variance_decomp) | plot_spacer()
  
  final <- (top_row / middle_row / bottom_row) +
    plot_layout(heights = c(1, 1, 0.7)) +
    plot_annotation(
      title    = paste0("Targeted comparisons (BFI) — ", dv_label),
      subtitle = "Median ± 89% and 95% HDI",
      theme    = theme(
        plot.title    = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 11, colour = "grey40")
      )
    )
  
  print(final)
  
  ggsave(paste0("targeted_comparisons_bfi_",
                gsub("[^a-zA-Z0-9]", "_", dv_label), ".png"),
         final, width = 14, height = 12, dpi = 300, bg = "white")
  
  invisible(plots)
}

# Function to get % from log estimates
log_to_pct <- function(estimate, ci_low = NULL, ci_high = NULL, digits = 1) {
  pct      <- round((exp(estimate) - 1) * 100, digits)
  
  if (!is.null(ci_low) && !is.null(ci_high)) {
    pct_low  <- round((exp(ci_low)  - 1) * 100, digits)
    pct_high <- round((exp(ci_high) - 1) * 100, digits)
    cat(sprintf("%s%% [%s%%, %s%%]\n", pct, pct_low, pct_high))
    invisible(c(pct = pct, ci_low = pct_low, ci_high = pct_high))
  } else {
    cat(sprintf("%s%%\n", pct))
    invisible(pct)
  }
}

# Function to get implied modality
get_implied_estimate <- function(model, type = c("two_modality", "three_modality"),
                                 digits = 3) {
  # type = "two_modality"   → implied = -modality1
  #        "three_modality" → implied = -(modality1 + modality2)
  
  type <- match.arg(type)
  
  if (type == "two_modality") {
    
    implied_draws <- model |>
      spread_draws(b_modality1) |>
      mutate(implied = -b_modality1)
    
    result <- implied_draws |>
      median_hdi(implied)
    
    cat("Implied modality (= -modality1):\n")
    cat(sprintf("  log scale: %.3f [%.3f, %.3f]\n",
                result$implied, result$.lower, result$.upper))
    cat(sprintf("  pct scale: %.1f%% [%.1f%%, %.1f%%]%s\n",
                (exp(result$implied) - 1) * 100,
                (exp(result$.lower)  - 1) * 100,
                (exp(result$.upper)  - 1) * 100,
                ifelse(result$.lower > 0 | result$.upper < 0, " *", "")))
    
  } else {
    
    implied_draws <- model |>
      spread_draws(b_modality1, b_modality2) |>
      mutate(implied = -(b_modality1 + b_modality2))
    
    result <- implied_draws |>
      median_hdi(implied)
    
    cat("Implied modality (= -(modality1 + modality2)):\n")
    cat(sprintf("  log scale: %.3f [%.3f, %.3f]\n",
                result$implied, result$.lower, result$.upper))
    cat(sprintf("  pct scale: %.1f%% [%.1f%%, %.1f%%]%s\n",
                (exp(result$implied) - 1) * 100,
                (exp(result$.lower)  - 1) * 100,
                (exp(result$.upper)  - 1) * 100,
                ifelse(result$.lower > 0 | result$.upper < 0, " *", "")))
  }
  
  invisible(result)
}

####
create_paper_table <- function(model_list, r2_list = NULL, 
                               dv_labels = NULL,
                               log_transformed = NULL) {
  # model_list:      named list of 6 models, one per DV
  # r2_list:         named list of R2 objects matching model_list names
  # dv_labels:       named vector of readable DV labels for column headers
  #                  e.g. c(torq_cum = "Arm Torque (integral)", ...)
  # log_transformed: named logical vector indicating which DVs are log-transformed
  #                  e.g. c(torq_cum = TRUE, env_cum = TRUE, cop_cum = FALSE, ...)
  
  model_names <- names(model_list)
  
  # ── Helper: back-transform label for log models ──────────────────────────────
  # ── Helper: back-transform label for log models ──────────────────────────────
  make_label <- function(estimate, ci_low, ci_high, credible, is_log, is_intercept) {
    if (is_log && !is_intercept) {
      # Percentage change
      pct      <- round((exp(estimate) - 1) * 100, 1)
      pct_low  <- round((exp(ci_low)   - 1) * 100, 1)
      pct_high <- round((exp(ci_high)  - 1) * 100, 1)
      # Raw log-scale estimate in parentheses
      paste0(pct, "% [", pct_low, "%, ", pct_high, "%]",
             " (β = ", round(estimate, 3), " [", round(ci_low, 3), ", ", round(ci_high, 3), "])",
             ifelse(credible, " *", ""))
    } else if (is_log && is_intercept) {
      # Geometric mean + raw log intercept
      paste0(round(exp(estimate), 1), " [",
             round(exp(ci_low),   1), ", ",
             round(exp(ci_high),  1), "]",
             " (β = ", round(estimate, 3), " [", round(ci_low, 3), ", ", round(ci_high, 3), "])",
             ifelse(credible, " *", ""))
    } else {
      # Non-log: just raw estimate
      paste0(round(estimate, 3), " [", round(ci_low, 3), ", ",
             round(ci_high, 3), "]",
             ifelse(credible, " *", ""))
    }
  }
  
  # ── Model info ──────────────────────────────────────────────────────────────
  extract_model_info <- function(model, model_name) {
    data.frame(
      model     = model_name,
      parameter = c("formula", "n_obs"),
      label     = as.character(c(
        as.character(formula(model))[1],
        nobs(model)
      )),
      term_type = "model_info",
      stringsAsFactors = FALSE
    )
  }
  
  # ── R2 ──────────────────────────────────────────────────────────────────────
  extract_r2 <- function(model_name, r2_list) {
    if (is.null(r2_list) || !model_name %in% names(r2_list)) return(NULL)
    r2 <- r2_list[[model_name]]
    data.frame(
      model     = model_name,
      parameter = "R²",
      label     = paste0(round(r2[, "Estimate"], 3), " [",
                         round(r2[, "Q2.5"],    3), ", ",
                         round(r2[, "Q97.5"],   3), "]"),
      term_type = "model_info",
      stringsAsFactors = FALSE
    )
  }
  
  # ── Fixed effects ────────────────────────────────────────────────────────────
  extract_fixed <- function(model, model_name) {
    is_log <- isTRUE(log_transformed[[model_name]])
    as.data.frame(fixef(model)) |>
      tibble::rownames_to_column("parameter") |>
      rename(estimate = Estimate, se = Est.Error,
             ci_low = Q2.5, ci_high = Q97.5) |>
      mutate(
        model      = model_name,
        term_type  = "fixed",
        credible   = ci_low > 0 | ci_high < 0,
        is_intercept = parameter == "Intercept",
        label      = purrr::pmap_chr(
          list(estimate, ci_low, ci_high, credible, is_intercept),
          ~ make_label(..1, ..2, ..3, ..4, is_log, ..5)
        )
      ) |>
      select(-is_intercept)
  }
  
  # ── Random SDs ──────────────────────────────────────────────────────────────
  extract_random_sds <- function(model, model_name) {
    vc <- VarCorr(model)
    purrr::imap_dfr(vc, function(group_vc, group_name) {
      as.data.frame(group_vc$sd) |>
        tibble::rownames_to_column("param") |>
        rename(estimate = Estimate, se = Est.Error,
               ci_low = Q2.5, ci_high = Q97.5) |>
        mutate(
          model     = model_name,
          term_type = "random_sd",
          parameter = paste0("sd(", param, ") | ", group_name),
          credible  = ci_low > 0 | ci_high < 0,
          label     = paste0(round(estimate, 3), " [", round(ci_low, 3), ", ",
                             round(ci_high, 3), "]")
        ) |>
        select(-param)
    })
  }
  
  # ── Random correlations ──────────────────────────────────────────────────────
  extract_random_cors <- function(model, model_name) {
    vc <- VarCorr(model)
    purrr::imap_dfr(vc, function(group_vc, group_name) {
      cor_array <- group_vc$cor
      if (is.null(cor_array)) return(NULL)
      param_names <- dimnames(cor_array)[[1]]
      purrr::map_dfr(seq_along(param_names), function(i) {
        purrr::map_dfr(seq_along(param_names), function(j) {
          if (j <= i) return(NULL)
          p1      <- param_names[i]
          p2      <- param_names[j]
          est     <- cor_array[p1, "Estimate", p2]
          ci_low  <- cor_array[p1, "Q2.5",     p2]
          ci_high <- cor_array[p1, "Q97.5",    p2]
          credible <- (!is.na(ci_low) & !is.na(ci_high)) &
            (ci_low > 0 | ci_high < 0)
          data.frame(
            model     = model_name,
            term_type = "random_cor",
            parameter = paste0("cor(", p1, " × ", p2, ") | ", group_name),
            credible  = credible,
            label     = paste0(
              ifelse(is.na(est),     "NA", sprintf("%.3f", est)), " [",
              ifelse(is.na(ci_low),  "NA", sprintf("%.3f", ci_low)), ", ",
              ifelse(is.na(ci_high), "NA", sprintf("%.3f", ci_high)), "]",
              ifelse(isTRUE(credible), " *", "")
            ),
            stringsAsFactors = FALSE
          )
        })
      })
    })
  }
  
  # ── Run across all models ────────────────────────────────────────────────────
  all_rows <- imap_dfr(model_list, function(mod, nm) {
    info <- tryCatch(extract_model_info(mod, nm), error = function(e) NULL)
    r2   <- tryCatch(extract_r2(nm, r2_list),     error = function(e) NULL)
    fe   <- tryCatch(extract_fixed(mod, nm),       error = function(e) NULL)
    sds  <- tryCatch(extract_random_sds(mod, nm),  error = function(e) NULL)
    cors <- tryCatch(extract_random_cors(mod, nm), error = function(e) NULL)
    bind_rows(info, r2, fe, sds, cors) |>
      select(model, term_type, parameter, label) |>
      mutate(label = as.character(label))
  })
  
  # ── Pivot ────────────────────────────────────────────────────────────────────
  wide <- all_rows |>
    mutate(
      term_type = factor(term_type,
                         levels = c("model_info", "fixed",
                                    "random_sd", "random_cor"))
    ) |>
    pivot_wider(
      id_cols     = c(term_type, parameter),
      names_from  = model,
      values_from = label,
      values_fill = "—"
    )
  
  # ── Sort: Intercept first within fixed effects ───────────────────────────────
  wide <- wide |>
    mutate(
      sort_key = case_when(
        term_type == "model_info" ~ 0,
        term_type == "fixed" & parameter == "Intercept" ~ 1,
        term_type == "fixed"      ~ 2,
        term_type == "random_sd"  ~ 3,
        term_type == "random_cor" ~ 4,
        TRUE                      ~ 5
      )
    ) |>
    arrange(sort_key, term_type, parameter) |>
    select(-sort_key) |>
    select(term_type, parameter, all_of(model_names))
  
  # ── Rename columns to readable DV labels ────────────────────────────────────
  if (!is.null(dv_labels)) {
    matched <- intersect(names(dv_labels), model_names)
    for (nm in matched) {
      names(wide)[names(wide) == nm] <- dv_labels[[nm]]
    }
  }
  
  # ── Add section separator rows for readability ───────────────────────────────
  section_labels <- tibble::tibble(
    term_type = factor(c("model_info", "fixed", "random_sd", "random_cor"),
                       levels = levels(wide$term_type)),
    parameter = c("— Model Information —",
                  "— Fixed Effects —",
                  "— Random Effect SDs —",
                  "— Random Effect Correlations —")
  )
  
  # Fill separator rows with em-dashes
  for (col in setdiff(names(wide), c("term_type", "parameter"))) {
    section_labels[[col]] <- ""
  }
  
  wide <- bind_rows(section_labels, wide) |>
    arrange(term_type, parameter == "— Model Information —",
            parameter == "— Fixed Effects —",
            parameter == "— Random Effect SDs —",
            parameter == "— Random Effect Correlations —") |>
    # Re-sort properly with separators at top of each section
    group_by(term_type) |>
    arrange(term_type, 
            !startsWith(parameter, "—"),
            parameter == "Intercept",
            parameter) |>
    ungroup()
  
  return(wide)
}


####

plot_correction_grid <- function(model_grid, data, dv_vars) {
  
  modality_colors <- c(
    "gesture"    = "#2196F3",
    "vocal"      = "#FF9800",
    "multimodal" = "#4CAF50"
  )
  
  plots <- purrr::imap(model_grid, function(entry, i) {
    
    mod      <- entry$model
    dv_label <- entry$label
    dv_col   <- dv_vars[[dv_label]]
    col      <- ((i - 1) %% 3) + 1
    show_y   <- col == 1
    show_x   <- entry$row == 2
    show_leg <- i == 3
    
    # ── Modalities in this model's data ──────────────────────────────────────
    mod_names <- entry$data_modalities
    
    cat(sprintf("Panel %d (%s): data modalities = %s\n",
                i, gsub("\n", " ", dv_label),
                paste(mod_names, collapse = ", ")))
    
    # ── Filter data ───────────────────────────────────────────────────────────
    model_data <- data |>
      filter(as.character(modality) %in% mod_names) |>
      mutate(modality = factor(as.character(modality), levels = mod_names))
    
    cat(sprintf("  n rows: %d | levels: %s\n",
                nrow(model_data),
                paste(levels(model_data$modality), collapse = ", ")))
    
    # ── Grid for predictions ──────────────────────────────────────────────────
    pred_data <- model_data |>
      modelr::data_grid(
        correction,
        modality,
        BFI_extra        = 0,
        Familiarity      = 0,
        expressibility_z = 0,
        TrialNumber_c    = 0
      ) |>
      mutate(modality = factor(as.character(modality), levels = mod_names))
    
    pred_draws <- pred_data |>
      add_epred_draws(mod, re_formula = NA)
    
    # ── c0 reference lines ────────────────────────────────────────────────────
    c0_means <- pred_draws |>
      filter(correction == "c0") |>
      group_by(modality) |>
      summarise(mean_val = mean(log(.epred)), .groups = "drop")
    
    # ── Raw data ──────────────────────────────────────────────────────────────
    raw_data <- model_data |>
      mutate(dv_log = log(.data[[dv_col]]))
    
    # ── Plot ──────────────────────────────────────────────────────────────────
    p <- ggplot() +
      
      geom_hline(
        data      = c0_means,
        aes(yintercept = mean_val, colour = modality),
        linetype  = "dashed",
        linewidth = 0.7,
        alpha     = 0.8
      ) +
      
      stat_halfeye(
        data           = pred_draws,
        aes(x = correction, y = log(.epred), fill = modality),
        side            = "right",
        .width          = c(0.89, 0.95),
        point_interval  = median_hdi,
        scale           = 1.2,
        alpha           = 0.7,
        normalize       = "groups",
        position        = position_dodge(width = 0.8),
        point_colour    = NA,
        interval_colour = NA
      ) +
      
      geom_jitter(
        data     = raw_data,
        aes(x = correction, y = dv_log, colour = modality),
        alpha    = 0.2,
        size     = 0.8,
        position = position_jitterdodge(jitter.width = 0.05, dodge.width = 0.8)
      ) +
      
      stat_pointinterval(
        data           = pred_draws,
        aes(x = correction, y = log(.epred), group = modality),
        .width         = c(0.89, 0.95),
        point_interval = median_hdi,
        position       = position_dodge(width = 0.8),
        colour         = "black",
        linewidth      = 1.0,
        point_size     = 2.0
      ) +
      
      scale_fill_manual(
        values = modality_colors,
        guide  = if (show_leg) guide_legend(title = "Modality") else "none"
      ) +
      scale_colour_manual(
        values = modality_colors,
        guide  = if (show_leg) guide_legend(title = "Modality") else "none"
      ) +
      scale_x_discrete(
        labels = c("c0_only" = "c0 only", "c0" = "c0",
                   "c1" = "c1", "c2" = "c2"),
        expand = c(0.1, 0.1)
      ) +
      scale_y_continuous(
        breaks = scales::pretty_breaks(n = 5),
        expand = expansion(mult = c(0.02, 0.05))
      ) +
      theme_effort_plot(base_size = 11) +
      theme(
        axis.text.x     = if (show_x) element_text(size = 9) else element_blank(),
        axis.ticks.x    = if (show_x) element_line() else element_blank(),
        axis.text.y     = if (show_y) element_text(size = 9) else element_blank(),
        axis.title.y    = if (show_y) element_text(size = 9) else element_blank(),
        plot.title      = element_text(size = 10, face = "bold", hjust = 0.5),
        legend.position = if (show_leg) "right" else "none"
      ) +
      labs(
        title = dv_label,
        x     = NULL,
        y     = if (show_y) "log(effort)" else NULL
      )
    
    return(p)
  })
  
  # ── Assemble ─────────────────────────────────────────────────────────────────
  row1 <- plots[[1]] | plots[[2]] | plots[[3]]
  row2 <- plots[[4]] | plots[[5]] | plots[[6]]
  
  (row1 / row2) +
    plot_annotation(
      title    = "Predicted effort by correction phase",
      subtitle = "Posterior mean ± 89% and 95% HDI | raw data overlaid | log y-axis | dashed = c0 reference",
      caption  = "Correction phase",
      theme    = theme(
        plot.title    = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 11, colour = "grey40"),
        plot.caption  = element_text(size = 11, hjust = 0.5,
                                     margin = margin(t = 8))
      )
    )
}

plot_correction_posteriors_grid <- function(model_grid) {
  
  # ── Extract draws ───────────────────────────────────────────────────────────
  all_draws <- purrr::imap_dfr(model_grid, function(entry, i) {
    entry$model |>
      gather_draws(`b_correction.*`, regex = TRUE) |>
      mutate(
        .variable = factor(.variable,
                           levels = c("b_correction3M2", "b_correction2M1"),  # 3M2 bottom, 2M1 top
                           labels = c("c1 → c2", "c0 → c1")
        ),
        dv  = entry$label,
        row = entry$row,
        col = ((i - 1) %% 3) + 1
      )
  })
  
  # ── Shared x axis range ─────────────────────────────────────────────────────
  x_range  <- range(all_draws$.value)
  x_limits <- c(floor(x_range[1]   * 10) / 10,
                ceiling(x_range[2] * 10) / 10)
  
  # ── Panel builder ───────────────────────────────────────────────────────────
  make_panel <- function(draws_sub, title, show_y = TRUE, show_x = TRUE) {
    
    draws_sub |>
      ggplot(aes(x = .value, y = .variable, fill = .variable)) +
      stat_halfeye(
        .width         = c(0.89, 0.95),
        point_interval = median_hdi,
        normalize      = "groups",
        scale          = 0.7,
        alpha          = 0.85
      ) +
      geom_vline(xintercept = 0, linetype = "dashed",
                 colour = "grey20", linewidth = 0.8) +
      scale_x_continuous(limits = x_limits,
                         breaks = scales::pretty_breaks(n = 4)) +
      scale_fill_brewer(palette = "Set1", guide = "none") +
      theme_effort_plot(base_size = 12) +
      theme(
        axis.text.y  = if (show_y) element_text(size = 11) else element_blank(),
        axis.ticks.y = if (show_y) element_line() else element_blank(),
        axis.text.x  = if (show_x) element_text(size = 10) else element_blank(),
        plot.title   = element_text(size = 11, face = "bold", hjust = 0.5)
      ) +
      labs(title = title, x = NULL, y = NULL)
  }
  
  # ── Build all panels ────────────────────────────────────────────────────────
  plots <- purrr::imap(model_grid, function(entry, i) {
    draws_sub <- all_draws |> filter(dv == entry$label)
    col       <- ((i - 1) %% 3) + 1
    show_y    <- col == 1
    show_x    <- entry$row == 2
    make_panel(draws_sub, entry$label, show_y, show_x)
  })
  
  # ── Assemble grid ───────────────────────────────────────────────────────────
  row1 <- plots[[1]] | plots[[2]] | plots[[3]]
  row2 <- plots[[4]] | plots[[5]] | plots[[6]]
  
  final_plot <- (row1 / row2) +
    plot_annotation(
      title    = "Posterior distributions — correction contrasts",
      subtitle = "Median ± 89% and 95% HDI | dashed line = 0",
      theme    = theme(
        plot.title    = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 11, colour = "grey40")
      )
    )
  
  # Shared x axis label
  wrap_elements(final_plot) +
    labs(caption = "Estimate (log scale)") +
    theme(plot.caption = element_text(hjust = 0.5, size = 11))
}

plot_fixed_effects_grid <- function(model_grid) {
  
  # ── Fixed parameter map (non-modality) ──────────────────────────────────────
  nonmod_map <- tribble(
    ~raw,                 ~label,
    "b_BFI_extra",        "BFI Extraversion",
    "b_Familiarity",      "Familiarity",
    "b_expressibility_z", "Expressibility",
    "b_TrialNumber_c",    "Trial Number"
  )
  
  param_colours <- c(
    "Gesture"          = "#2196F3",
    "Multimodal"       = "#4CAF50",
    "Vocal"            = "#FF9800",
    "BFI Extraversion" = "#9C27B0",
    "Familiarity"      = "#E91E63",
    "Expressibility"   = "#795548",
    "Trial Number"     = "#607D8B"
  )
  
  # All possible rows in fixed order
  all_labels <- c("Gesture", "Multimodal", "Vocal",
                  "BFI Extraversion", "Familiarity",
                  "Expressibility", "Trial Number")
  
  # ── Extract draws ───────────────────────────────────────────────────────────
  all_draws_fe <- purrr::imap_dfr(model_grid, function(entry, i) {
    
    raw_draws <- entry$model |>
      gather_draws(`b_.*`, regex = TRUE) |>
      filter(!grepl("Intercept|correction", .variable))
    
    mod_map   <- entry$modality_map
    has_two   <- "modality2" %in% names(mod_map)
    
    # Rename modality1
    mod1_label <- mod_map[["modality1"]]
    raw_draws <- raw_draws |>
      mutate(.variable = ifelse(.variable == "b_modality1",
                                paste0("b_mod_", mod1_label), .variable))
    
    # Rename modality2 if present
    if (has_two) {
      mod2_label <- mod_map[["modality2"]]
      raw_draws <- raw_draws |>
        mutate(.variable = ifelse(.variable == "b_modality2",
                                  paste0("b_mod_", mod2_label), .variable))
    }
    
    # Compute implied modality
    impl_label <- mod_map[["implied"]]
    
    if (has_two) {
      # implied = -(modality1 + modality2)
      implied_draws <- raw_draws |>
        filter(.variable %in% paste0("b_mod_", c(mod1_label, mod2_label))) |>
        select(.chain, .iteration, .draw, .variable, .value) |>
        tidyr::pivot_wider(
          id_cols     = c(.chain, .iteration, .draw),
          names_from  = .variable,
          values_from = .value
        ) |>
        mutate(
          .value    = -(!!sym(paste0("b_mod_", mod1_label)) + 
                          !!sym(paste0("b_mod_", mod2_label))),
          .variable = paste0("b_mod_implied_", impl_label)
        ) |>
        select(.chain, .iteration, .draw, .variable, .value)
    } else {
      # implied = -modality1
      implied_draws <- raw_draws |>
        filter(.variable == paste0("b_mod_", mod1_label)) |>
        mutate(
          .value    = -.value,
          .variable = paste0("b_mod_implied_", impl_label)
        )
    }
    
    raw_draws <- bind_rows(raw_draws, implied_draws)
    
    # Map non-modality parameters
    raw_draws <- raw_draws |>
      left_join(nonmod_map, by = c(".variable" = "raw"))
    
    # Map modality parameters
    raw_draws <- raw_draws |>
      mutate(
        label = case_when(
          !is.na(label) ~ label,
          grepl("^b_mod_implied_", .variable) ~
            sub("^b_mod_implied_", "", .variable),
          grepl("^b_mod_", .variable) ~
            sub("^b_mod_", "", .variable),
          TRUE ~ NA_character_
        ),
        implied = grepl("implied", .variable)
      ) |>
      filter(!is.na(label))
    
    raw_draws |>
      mutate(
        dv  = entry$label,
        row = entry$row,
        col = ((i - 1) %% 3) + 1
      )
  })
  
  # ── X limits per column ──────────────────────────────────────────────────────
  x_limits_by_col <- all_draws_fe |>
    group_by(col) |>
    summarise(
      x_min = floor(min(.value)   * 10) / 10,
      x_max = ceiling(max(.value) * 10) / 10,
      .groups = "drop"
    )
  
  # ── Panel builder ───────────────────────────────────────────────────────────
  make_panel_fe <- function(draws_sub, title, show_y = TRUE,
                            show_x = TRUE, x_limits = NULL) {
    
    # All labels in fixed order, present or not
    present   <- unique(draws_sub$label)
    available <- intersect(all_labels, present)
    missing   <- setdiff(all_labels, present)
    
    draws_sub     <- draws_sub |>
      mutate(label = factor(label, levels = rev(all_labels)))
    draws_implied <- draws_sub |> filter(isTRUE(implied))
    draws_direct  <- draws_sub |> filter(!isTRUE(implied))
    
    p <- ggplot(mapping = aes(x = .value, y = label, fill = label)) +
      
      # Direct estimates
      stat_halfeye(
        data = draws_direct,
        .width = c(0.89, 0.95),
        point_interval = median_hdi,
        normalize = "groups",
        scale = 0.7,
        alpha = 0.85
      )
    
    # Implied estimates — same colour, dashed slab
    if (nrow(draws_implied) > 0) {
      p <- p +
        stat_halfeye(
          data = draws_implied,
          .width = c(0.89, 0.95),
          point_interval = median_hdi,
          normalize = "groups",
          scale = 0.7,
          slab_alpha     = 0.35,
          slab_linetype  = "dashed",
          slab_linewidth = 0.6,
          alpha = 0.85
        )
    }
    
    p +
      # Empty lines for missing modalities
      scale_y_discrete(
        limits = rev(all_labels),
        drop   = FALSE   # keeps all levels even if no data
      ) +
      geom_vline(xintercept = 0, linetype = "dashed",
                 colour = "grey20", linewidth = 0.8) +
      scale_x_continuous(
        limits = x_limits,
        breaks = scales::pretty_breaks(n = 4)
      ) +
      scale_fill_manual(values = param_colours, guide = "none",
                        na.value = "grey80") +
      theme_effort_plot(base_size = 12) +
      theme(
        axis.text.y  = if (show_y) element_text(size = 10) else element_blank(),
        axis.ticks.y = if (show_y) element_line() else element_blank(),
        axis.text.x  = if (show_x) element_text(size = 10) else element_blank(),
        plot.title   = element_text(size = 11, face = "bold", hjust = 0.5)
      ) +
      labs(title = title, x = NULL, y = NULL)
  }
  
  # ── Build all panels ────────────────────────────────────────────────────────
  plots_fe <- purrr::imap(model_grid, function(entry, i) {
    draws_sub <- all_draws_fe |> filter(dv == entry$label)
    col       <- ((i - 1) %% 3) + 1
    show_y    <- col == 1
    show_x    <- entry$row == 2
    
    x_lim <- x_limits_by_col |>
      filter(col == !!col) |>
      reframe(lims = c(x_min, x_max)) |>
      pull(lims)
    
    make_panel_fe(draws_sub, entry$label, show_y, show_x, x_lim)
  })
  
  # ── Assemble ────────────────────────────────────────────────────────────────
  row1_fe <- plots_fe[[1]] | plots_fe[[2]] | plots_fe[[3]]
  row2_fe <- plots_fe[[4]] | plots_fe[[5]] | plots_fe[[6]]
  
  (row1_fe / row2_fe) +
    plot_annotation(
      title    = "Posterior distributions — fixed effects",
      subtitle = "Median ± 89% and 95% HDI | dashed line = 0 | empty row = not in model",
      theme    = theme(
        plot.title    = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 11, colour = "grey40")
      )
    )
}



# ============================================================
# plot_posterior_channel_correlations()
# FLESH project helper — Sarka Kadava
# ============================================================
# Computes posterior predictive correlations between effort
# channels (arm, envelope, COP) split by modality condition,
# and returns a patchwork plot with interval + half-eye panels.
#
# Dependencies: brms, tidybayes, dplyr, tidyr,
#               ggplot2, ggdist, patchwork
# ============================================================

plot_posterior_channel_correlations <- function(
    fit,
    data,
    resp_arm      = "logarm",
    resp_env      = "logenv",
    resp_cop      = "logcop",
    modality_var  = "modality",
    modality_labels = c("1" = "Gesture only",
                        "2" = "Vocal only",
                        "3" = "Multimodal"),
    ndraws        = 500,
    seed          = 42,
    ci_widths     = c(0.89, 0.95),
    colors        = c("Gesture only" = "#1D9E75",
                      "Vocal only"   = "#378ADD",
                      "Multimodal"   = "#7F77DD"),
    save_path     = NULL,
    width         = 8,
    height        = 12
) {
  
  # ── 0. Packages ─────────────────────────────────────────
  requireNamespace("brms",      quietly = TRUE)
  requireNamespace("tidybayes", quietly = TRUE)
  requireNamespace("dplyr",     quietly = TRUE)
  requireNamespace("tidyr",     quietly = TRUE)
  requireNamespace("ggplot2",   quietly = TRUE)
  requireNamespace("ggdist",    quietly = TRUE)
  requireNamespace("patchwork", quietly = TRUE)
  
  library(dplyr); library(tidyr); library(ggplot2)
  library(tidybayes); library(ggdist); library(patchwork)
  
  # ── 1. Column names for the three response predictions ──
  col_arm <- paste0(".prediction_", resp_arm)
  col_env <- paste0(".prediction_", resp_env)
  col_cop <- paste0(".prediction_", resp_cop)
  
  # ── 2. Draw posterior predictions ───────────────────────
  set.seed(seed)
  
  pred_draws_long <- data |>
    tidybayes::add_predicted_draws(fit, ndraws = ndraws, seed = seed)
  
  # Multivariate brms models return a single .prediction column in long
  # format, with .category identifying the response. Pivot to wide so
  # each response gets its own column before computing correlations.
  if (".category" %in% names(pred_draws_long)) {
    pred_draws <- pred_draws_long |>
      tidyr::pivot_wider(
        names_from   = .category,
        values_from  = .prediction,
        names_prefix = ".prediction_"
      )
  } else {
    pred_draws <- pred_draws_long
  }
  
  # Validate column names
  missing_cols <- setdiff(c(col_arm, col_env, col_cop), names(pred_draws))
  if (length(missing_cols) > 0) {
    available <- grep("^\\.prediction", names(pred_draws), value = TRUE)
    stop(
      "Response columns not found: ", paste(missing_cols, collapse = ", "),
      "\nAvailable prediction columns: ", paste(available, collapse = ", "),
      "\nAdjust resp_arm / resp_env / resp_cop arguments."
    )
  }
  
  # ── 3. Compute pairwise correlations per draw x modality ─
  corr_draws <- pred_draws |>
    group_by(.data[[modality_var]], .draw) |>
    summarise(
      `Arm <-> Envelope` = cor(.data[[col_arm]], .data[[col_env]], use = "complete.obs"),
      `Arm <-> COP`      = cor(.data[[col_arm]], .data[[col_cop]], use = "complete.obs"),
      `Envelope <-> COP` = cor(.data[[col_env]], .data[[col_cop]], use = "complete.obs"),
      .groups = "drop"
    ) |>
    pivot_longer(
      cols      = c(`Arm <-> Envelope`, `Arm <-> COP`, `Envelope <-> COP`),
      names_to  = "pair",
      values_to = "r"
    ) |>
    mutate(
      pair     = factor(pair, levels = c("Arm <-> Envelope",
                                         "Arm <-> COP",
                                         "Envelope <-> COP")),
      modality = dplyr::recode(
        as.character(.data[[modality_var]]),
        !!!modality_labels
      ),
      modality = factor(modality, levels = modality_labels)
    )
  
  # ── 4. Summarise ─────────────────────────────────────────
  corr_summary <- corr_draws |>
    group_by(modality, pair) |>
    tidybayes::median_qi(r, .width = ci_widths)
  
  # ── 5. Panel A: dot + interval overview ──────────────────
  p_intervals <- ggplot(
    corr_summary,
    aes(x = r, xmin = .lower, xmax = .upper,
        y = pair, colour = modality, group = modality)
  ) +
    ggdist::geom_pointinterval(
      position            = position_dodge(width = 0.55),
      interval_size_range = c(0.6, 1.4),
      fatten_point        = 2.5
    ) +
    geom_vline(
      xintercept = 0, linetype = "dashed",
      colour = "grey60", linewidth = 0.35
    ) +
    scale_colour_manual(values = colors, name = NULL) +
    scale_x_continuous(
      limits = c(-1, 1),
      breaks = seq(-0.75, 1, 0.25),
      labels = function(x) sprintf("%.2f", x)
    ) +
    labs(
      x        = "Pearson r",
      y        = NULL,
      title    = "Posterior channel correlations by modality",
      subtitle = paste0(
        "Median + ",
        paste(ci_widths * 100, collapse = "% / "),
        "% credible intervals"
      )
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title         = element_text(size = 13, face = "bold"),
      plot.subtitle      = element_text(size = 10, colour = "grey40"),
      legend.position    = "bottom",
      legend.key.size    = unit(0.5, "lines"),
      panel.grid.major.y = element_blank(),
      panel.grid.minor   = element_blank(),
      panel.grid.major.x = element_line(colour = "grey92", linewidth = 0.3)
    )
  
  # ── 6. Panel B: half-eye per channel pair ────────────────
  p_halfeye <- ggplot(
    corr_draws,
    aes(x = r, y = modality, fill = modality, colour = modality)
  ) +
    ggdist::stat_halfeye(
      aes(slab_alpha = after_stat(f)),
      point_interval = median_qi,
      .width         = ci_widths,
      normalize      = "panels",
      scale          = 0.7,
      slab_colour    = NA
    ) +
    geom_vline(
      xintercept = 0, linetype = "dashed",
      colour = "grey50", linewidth = 0.35
    ) +
    facet_wrap(~pair, ncol = 1, scales = "free_y") +
    scale_fill_manual(values   = colors, guide = "none") +
    scale_colour_manual(values = colors, guide = "none") +
    scale_x_continuous(
      limits = c(-1, 1),
      breaks = seq(-0.75, 1, 0.25),
      labels = function(x) sprintf("%.2f", x)
    ) +
    labs(
      x        = "Pearson r (posterior predictive)",
      y        = NULL,
      subtitle = "Full posterior distributions per channel pair"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.subtitle      = element_text(size = 10, colour = "grey40"),
      strip.text         = element_text(size = 11, face = "bold"),
      panel.grid.major.y = element_blank(),
      panel.grid.minor   = element_blank(),
      panel.grid.major.x = element_line(colour = "grey92", linewidth = 0.3),
      panel.spacing      = unit(1.2, "lines"),
      axis.text.y        = element_text(
        colour = colors[levels(corr_draws$modality)]
      )
    )
  
  # ── 7. Combine ───────────────────────────────────────────
  combined <- p_intervals / p_halfeye +
    plot_layout(heights = c(1, 2.5)) +
    plot_annotation(
      caption = paste0(
        "brms multivariate model · ", ndraws, " posterior draws"
      ),
      theme = theme(
        plot.caption = element_text(size = 9, colour = "grey50")
      )
    )
  
  # ── 8. Optionally save ───────────────────────────────────
  if (!is.null(save_path)) {
    ext <- tools::file_ext(save_path)
    if (ext == "pdf") {
      ggsave(save_path, combined, width = width, height = height,
             device = cairo_pdf)
    } else {
      ggsave(save_path, combined, width = width, height = height, dpi = 300)
    }
    message("Saved to: ", save_path)
  }
  
  # ── 9. Return invisibly so assignment is optional ────────
  invisible(list(
    plot          = combined,
    corr_draws    = corr_draws,
    corr_summary  = corr_summary
  ))
}


# ── Example usage ────────────────────────────────────────────
#
# result <- plot_posterior_channel_correlations(
#   fit    = fit,
#   data   = df_all,
#   ndraws = 500
# )
# result$plot                # view
# result$corr_summary        # inspect posterior summaries
#
# # Save as PDF:
# plot_posterior_channel_correlations(
#   fit       = fit,
#   data      = df_all,
#   save_path = "figures/channel_correlations_by_modality.pdf"
# )
#
# # Override response names if brms named them differently:
# plot_posterior_channel_correlations(
#   fit      = fit,
#   data     = df_all,
#   resp_arm = "log_arm",   # check with: grep(".prediction", names(pred), value=TRUE)
#   resp_env = "log_env",
#   resp_cop = "log_cop"
# )


# ============================================================
# get_modality_contrasts()
# FLESH project helper — Sarka Kadava
# ============================================================
# Extracts pairwise modality contrasts from a brms model using
# posterior draws. Handles sum coding (default in brms contr.sum)
# with 2 or 3 modality levels, reconstructing the implicit level
# automatically.
#
# Arguments:
#   model         brms fit object
#   labels        Named character vector mapping modality numbers to
#                 labels, e.g. c("1"="Gesture","2"="Vocal","3"="Multimodal")
#                 Names must match the numeric suffixes in b_modalityX.
#   coding        "sum" (default, brms contr.sum) or "treatment" (dummy)
#   ci_widths     Numeric vector of CI widths, default c(0.89, 0.95)
#   plot          Logical, whether to print a contrast plot
#   digits        Decimal places in printed table
#
# Returns invisibly:
#   list(summary, draws, plot)
# ============================================================

get_modality_contrasts <- function(
    model,
    labels    = c("1" = "Gesture only",
                  "2" = "Vocal only",
                  "3" = "Multimodal"),
    coding    = c("sum", "treatment"),
    ci_widths = c(0.89, 0.95),
    plot      = TRUE,
    digits    = 2
) {
  coding <- match.arg(coding)
  
  requireNamespace("brms",      quietly = TRUE)
  requireNamespace("dplyr",     quietly = TRUE)
  requireNamespace("tidyr",     quietly = TRUE)
  requireNamespace("ggplot2",   quietly = TRUE)
  requireNamespace("ggdist",    quietly = TRUE)
  library(dplyr); library(tidyr); library(ggplot2); library(ggdist)
  
  # ── 1. Find modality coefficient columns ──────────────────
  draws_df  <- brms::as_draws_df(model)
  mod_cols  <- sort(grep("^b_modality\\d+$", names(draws_df), value = TRUE))
  
  if (length(mod_cols) == 0)
    stop("No population-level modality coefficients found (expected b_modalityX).")
  
  explicit_nums <- as.integer(gsub("^b_modality", "", mod_cols))
  
  # ── 2. Reconstruct all level effects ──────────────────────
  # Sum coding: explicit coefficients are deviations from grand mean.
  # The implicit (omitted) level = -(sum of all explicit).
  # Treatment coding: reference level = 0, others are offsets.
  
  all_nums <- sort(as.integer(names(labels)))
  
  level_draws <- lapply(all_nums, function(n) {
    col <- paste0("b_modality", n)
    if (col %in% names(draws_df)) {
      draws_df[[col]]
    } else if (coding == "sum") {
      # Implicit level in sum coding
      explicit_cols <- paste0("b_modality", explicit_nums)
      -rowSums(draws_df[, explicit_cols, drop = FALSE])
    } else {
      # Treatment coding reference level = 0
      rep(0, nrow(draws_df))
    }
  })
  names(level_draws) <- as.character(all_nums)
  
  # ── 3. All pairwise contrasts ─────────────────────────────
  pairs <- combn(all_nums, 2, simplify = FALSE)
  
  contrast_draws <- lapply(pairs, function(p) {
    a    <- as.character(p[1])
    b    <- as.character(p[2])
    lab  <- paste0(labels[a], " - ", labels[b])
    diff <- level_draws[[a]] - level_draws[[b]]
    data.frame(contrast = lab, diff = diff)
  }) |> bind_rows()
  
  # ── 4. Summarise ──────────────────────────────────────────
  summary_df <- contrast_draws |>
    group_by(contrast) |>
    tidybayes::median_qi(diff, .width = ci_widths) |>
    mutate(
      across(c(diff, .lower, .upper), ~round(.x, digits)),
      credible = ifelse(.lower > 0 | .upper < 0, "*", "")
    )
  
  cat("\n── Modality pairwise contrasts (log-odds scale) ─────────\n")
  cat("Coding:", coding, "| CIs:", paste(ci_widths * 100, collapse = "% / "), "%\n\n")
  print(
    summary_df |>
      select(contrast, diff, .lower, .upper, .width, credible) |>
      rename(median = diff, lower = .lower, upper = .upper, width = .width),
    n = Inf
  )
  cat("\n* = CI excludes zero\n")
  
  # ── 5. Plot ───────────────────────────────────────────────
  p <- ggplot(
    contrast_draws,
    aes(x = diff, y = contrast)
  ) +
    ggdist::stat_halfeye(
      aes(slab_alpha = after_stat(f)),
      fill           = "#378ADD",
      colour         = "#378ADD",
      slab_colour    = NA,
      point_interval = median_qi,
      .width         = ci_widths,
      normalize      = "panels",
      scale          = 0.6
    ) +
    geom_vline(
      xintercept = 0, linetype = "dashed",
      colour = "grey50", linewidth = 0.4
    ) +
    scale_x_continuous(
      labels = function(x) sprintf("%.2f", x)
    ) +
    labs(
      x        = "Difference (log-odds)",
      y        = NULL,
      title    = "Pairwise modality contrasts",
      subtitle = paste0(
        "Median + ", paste(ci_widths * 100, collapse = "% / "), "% CI"
      ),
      caption  = paste("Coding:", coding)
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title         = element_text(size = 13, face = "bold"),
      plot.subtitle      = element_text(size = 10, colour = "grey40"),
      plot.caption       = element_text(size = 9,  colour = "grey50"),
      panel.grid.major.y = element_blank(),
      panel.grid.minor   = element_blank(),
      panel.grid.major.x = element_line(colour = "grey92", linewidth = 0.3)
    )
  
  if (plot) print(p)
  
  invisible(list(
    summary = summary_df,
    draws   = contrast_draws,
    plot    = p
  ))
}


# ── Example usage ─────────────────────────────────────────────
#
# result <- get_modality_contrasts(
#   model  = e3.m1,
#   labels = c("1" = "Gesture only", "2" = "Vocal only", "3" = "Multimodal")
# )
# result$summary   # table
# result$plot      # ggplot object for ggsave
#
# # Two-modality model (only gesture vs vocal):
# get_modality_contrasts(
#   model  = e2.m1,
#   labels = c("1" = "Gesture only", "2" = "Vocal only")
# )
#
# # Treatment (dummy) coding:
# get_modality_contrasts(
#   model  = e3.m1,
#   labels = c("1" = "Gesture only", "2" = "Vocal only", "3" = "Multimodal"),
#   coding = "treatment"
# )


plot_correction_by_effort <- function(models_e1, dv_labels = NULL,
                                      type = c("two_modality", "three_modality")) {
  
  type <- match.arg(type)
  
  performer_palette <- c(
    "−2 SD"   = "#37474F",
    "−1 SD"   = "#607D8B",
    "Average" = "#000000",
    "+1 SD"   = "#E53935",
    "+2 SD"   = "#B71C1C"
  )
  
  print_result <- function(draws, var, label, pct = FALSE) {
    res  <- draws |> median_hdi(!!sym(var))
    est  <- res[[var]]
    lo   <- res$.lower
    hi   <- res$.upper
    cred <- lo > 0 | hi < 0
    if (pct) {
      cat(sprintf("  %-50s %+.1f%% [%+.1f%%, %+.1f%%]%s\n",
                  label, est, lo, hi, ifelse(cred, "  *", "")))
    } else {
      cat(sprintf("  %-50s %.3f [%.3f, %.3f]%s\n",
                  label, est, lo, hi, ifelse(cred, "  *", "")))
    }
  }
  
  if (is.null(dv_labels)) dv_labels <- names(models_e1)
  
  plot_list <- list()
  
  purrr::imap(models_e1, function(mod, nm) {
    
    label <- dv_labels[[nm]]
    
    cat("═══════════════════════════════════════════════\n")
    cat(sprintf("  %s\n", label))
    cat("═══════════════════════════════════════════════\n\n")
    
    # ── Find effort coefficient name ───────────────────────────────────────────
    coef_names <- rownames(fixef(mod))
    
    ft_main <- grep("first_trial|effort_previous|baseline_", coef_names, value = TRUE) |>
      grep("correction", x = _, invert = TRUE, value = TRUE)
    ft_int_c1 <- grep(
      "correction2M1.*(first_trial|effort_previous|baseline_)|(first_trial|effort_previous|baseline_).*correction2M1",
      coef_names, value = TRUE, perl = TRUE)
    ft_int_c2 <- grep(
      "correction3M2.*(first_trial|effort_previous|baseline_)|(first_trial|effort_previous|baseline_).*correction3M2",
      coef_names, value = TRUE, perl = TRUE)
    
    if (length(ft_main) == 0) {
      message("No effort coefficient found in: ", nm)
      return(NULL)
    }
    
    # ── Build draws ────────────────────────────────────────────────────────────
    draws <- mod |>
      spread_draws(
        b_Intercept,
        b_correction2M1,
        b_correction3M2,
        !!sym(paste0("b_", ft_main)),
        !!sym(paste0("b_", ft_int_c1)),
        !!sym(paste0("b_", ft_int_c2))
      ) |>
      rename(
        ft_main   = !!paste0("b_", ft_main),
        ft_int_c1 = !!paste0("b_", ft_int_c1),
        ft_int_c2 = !!paste0("b_", ft_int_c2)
      )
    
    # ── Predicted effort at each SD level × correction ─────────────────────────
    draws <- draws |>
      mutate(
        # c0: no correction terms
        vlow_c0  = exp(b_Intercept + (-2) * ft_main),
        low_c0   = exp(b_Intercept + (-1) * ft_main),
        avg_c0   = exp(b_Intercept),
        high_c0  = exp(b_Intercept +   1  * ft_main),
        vhigh_c0 = exp(b_Intercept +   2  * ft_main),
        
        # c1: correction2M1 + interaction
        vlow_c1  = exp(b_Intercept + (-2) * ft_main +
                         b_correction2M1 + (-2) * ft_int_c1),
        low_c1   = exp(b_Intercept + (-1) * ft_main +
                         b_correction2M1 + (-1) * ft_int_c1),
        avg_c1   = exp(b_Intercept +
                         b_correction2M1),
        high_c1  = exp(b_Intercept +   1  * ft_main +
                         b_correction2M1 +   1  * ft_int_c1),
        vhigh_c1 = exp(b_Intercept +   2  * ft_main +
                         b_correction2M1 +   2  * ft_int_c1),
        
        # c2: correction2M1 + correction3M2 + both interactions
        vlow_c2  = exp(b_Intercept + (-2) * ft_main +
                         b_correction2M1 + (-2) * ft_int_c1 +
                         b_correction3M2 + (-2) * ft_int_c2),
        low_c2   = exp(b_Intercept + (-1) * ft_main +
                         b_correction2M1 + (-1) * ft_int_c1 +
                         b_correction3M2 + (-1) * ft_int_c2),
        avg_c2   = exp(b_Intercept +
                         b_correction2M1 +
                         b_correction3M2),
        high_c2  = exp(b_Intercept +   1  * ft_main +
                         b_correction2M1 +   1  * ft_int_c1 +
                         b_correction3M2 +   1  * ft_int_c2),
        vhigh_c2 = exp(b_Intercept +   2  * ft_main +
                         b_correction2M1 +   2  * ft_int_c1 +
                         b_correction3M2 +   2  * ft_int_c2),
        
        # Correction effect (% change c0→c1) per group
        corr_pct_vlow  = (vlow_c1  / vlow_c0  - 1) * 100,
        corr_pct_low   = (low_c1   / low_c0   - 1) * 100,
        corr_pct_avg   = (avg_c1   / avg_c0   - 1) * 100,
        corr_pct_high  = (high_c1  / high_c0  - 1) * 100,
        corr_pct_vhigh = (vhigh_c1 / vhigh_c0 - 1) * 100
      )
    
    # ── Print ──────────────────────────────────────────────────────────────────
    cat("  Predicted effort at c0 (baseline differences):\n")
    print_result(draws, "vlow_c0",  "  Very low (-2SD) c0:")
    print_result(draws, "low_c0",   "  Low (-1SD) c0:")
    print_result(draws, "avg_c0",   "  Average c0:")
    print_result(draws, "high_c0",  "  High (+1SD) c0:")
    print_result(draws, "vhigh_c0", "  Very high (+2SD) c0:")
    
    cat("\n  Correction effect c0→c1 by baseline effort level (%):\n")
    print_result(draws, "corr_pct_vlow",  "  Very low (-2SD):", pct = TRUE)
    print_result(draws, "corr_pct_low",   "  Low (-1SD):",      pct = TRUE)
    print_result(draws, "corr_pct_avg",   "  Average:",         pct = TRUE)
    print_result(draws, "corr_pct_high",  "  High (+1SD):",     pct = TRUE)
    print_result(draws, "corr_pct_vhigh", "  Very high (+2SD):", pct = TRUE)
    cat("\n")
    
    # ── Pivot for plotting ─────────────────────────────────────────────────────
    traj_long <- draws |>
      select(.draw, vlow_c0:vhigh_c2) |>
      tidyr::pivot_longer(
        cols      = vlow_c0:vhigh_c2,
        names_to  = c("group", "correction"),
        names_sep = "_",
        values_to = "effort"
      ) |>
      mutate(
        correction = factor(correction, levels = c("c0", "c1", "c2")),
        group      = factor(group,
                            levels = c("vlow", "low", "avg", "high", "vhigh"),
                            labels = c("−2 SD", "−1 SD", "Average", "+1 SD", "+2 SD"))
      )
    
    traj_summary <- traj_long |>
      group_by(group, correction) |>
      median_hdi(effort)
    
    # ── Correction effect posterior plot ───────────────────────────────────────
    corr_long <- draws |>
      select(.draw, corr_pct_vlow:corr_pct_vhigh) |>
      tidyr::pivot_longer(
        cols      = corr_pct_vlow:corr_pct_vhigh,
        names_to  = "group",
        values_to = "pct"
      ) |>
      mutate(group = factor(group,
                            levels = c("corr_pct_vlow", "corr_pct_low", "corr_pct_avg",
                                       "corr_pct_high", "corr_pct_vhigh"),
                            labels = c("−2 SD", "−1 SD", "Average", "+1 SD", "+2 SD")))
    
    p_traj <- traj_summary |>
      ggplot(aes(x = correction, y = effort,
                 colour = group, group = group)) +
      geom_ribbon(aes(ymin = .lower, ymax = .upper, fill = group),
                  alpha = 0.12, colour = NA) +
      geom_line(linewidth = 1.2) +
      geom_point(size = 3) +
      scale_colour_manual(values = performer_palette, name = "First-trial effort") +
      scale_fill_manual(values = performer_palette, guide = "none") +
      scale_y_log10(breaks = scales::pretty_breaks(n = 5)) +
      theme_effort_plot() +
      labs(title    = label,
           subtitle = "Effort trajectory by first-trial level | log y-axis",
           x = "Correction phase", y = "Effort (log scale)")
    
    p_corr <- corr_long |>
      ggplot(aes(x = pct, y = group, fill = group)) +
      stat_halfeye(
        .width = c(0.89, 0.95), point_interval = median_hdi,
        normalize = "groups", scale = 0.7, alpha = 0.85
      ) +
      geom_vline(xintercept = 0, linetype = "dashed",
                 colour = "grey20", linewidth = 0.8) +
      scale_fill_manual(values = performer_palette, guide = "none") +
      theme_effort_plot() +
      labs(title    = label,
           subtitle = "c0→c1 correction effect by first-trial level",
           x = "% change at first correction", y = NULL)
    
    plot_list[[nm]] <<- list(traj = p_traj, corr = p_corr)
  })
  
  # ── Assemble grid ─────────────────────────────────────────────────────────────
  nms_available <- names(Filter(Negate(is.null), plot_list))
  
  if (length(nms_available) == 0) {
    message("No plots to assemble — check coefficient names.")
    return(invisible(plot_list))
  }
  
  traj_row <- purrr::map(nms_available, ~ plot_list[[.x]]$traj) |>
    patchwork::wrap_plots(nrow = 1) +
    patchwork::plot_layout(guides = "collect") &
    theme(legend.position = "bottom")
  
  corr_row <- purrr::map(nms_available, ~ plot_list[[.x]]$corr) |>
    patchwork::wrap_plots(nrow = 1) +
    patchwork::plot_layout(guides = "collect") &
    theme(legend.position = "none")
  
  final <- (traj_row / corr_row) +
    patchwork::plot_annotation(
      title    = "Correction escalation by first-trial effort level",
      subtitle = "Top: effort trajectories across corrections | Bottom: posterior of c0→c1 effect",
      theme    = theme(
        plot.title    = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 11, colour = "grey40")
      )
    )
  
  print(final)
  
  ggsave("correction_by_effort_interaction.png", final,
         width = 20, height = 10, dpi = 300, bg = "white")
  
  invisible(plot_list)
}


## Model 8

plot_effort_by_concept <- function(model_list, dv_labels = NULL) {
  
  effort_palette <- c(
    "−2 SD"   = "#37474F",
    "−1 SD"   = "#607D8B",
    "Average" = "#000000",
    "+1 SD"   = "#E53935",
    "+2 SD"   = "#B71C1C"
  )
  
  print_result <- function(draws, var, label) {
    res  <- draws |> median_hdi(!!sym(var))
    est  <- res[[var]]; lo <- res$.lower; hi <- res$.upper
    cred <- lo > 0 | hi < 0
    cat(sprintf("  %-50s %.3f [%.3f, %.3f]%s\n",
                label, est, lo, hi, ifelse(cred, "  *", "")))
  }
  
  plot_list <- list()
  
  purrr::imap(model_list, function(mod, model_nm) {
    
    coef_names <- rownames(fixef(mod))
    fe_draws   <- fixef(mod, summary = FALSE) |> as.data.frame()
    
    # All effort main terms in this model
    all_effort_mains <- grep(
      "arm_torque.*total|envelope.*total|copc.*total",
      coef_names, value = TRUE
    ) |> grep("concept_number", x = _, invert = TRUE, value = TRUE)
    
    if (length(all_effort_mains) == 0) {
      message("No effort coefficients found in: ", model_nm)
      return(NULL)
    }
    
    concept_sd <- sd(mod$data$concept_number_c, na.rm = TRUE)
    b_intercept <- fe_draws[["Intercept"]]
    b_concept   <- fe_draws[["concept_number_c"]]
    
    # ── Inner loop: one panel set per channel ──────────────────────────────────
    purrr::walk(all_effort_mains, function(effort_nm) {
      
      # Label: look up by effort variable name, fall back to variable name itself
      label <- if (!is.null(dv_labels) && effort_nm %in% names(dv_labels))
        dv_labels[[effort_nm]]
      else effort_nm
      
      plot_key <- paste0(model_nm, "__", effort_nm)
      
      cat("═══════════════════════════════════════════════\n")
      cat(sprintf("  %s\n", label))
      cat("═══════════════════════════════════════════════\n\n")
      
      # Interaction term (may be named either way round)
      effort_x_concept_name <- grep(
        paste0("(", effort_nm, ").*concept_number|concept_number.*(", effort_nm, ")"),
        coef_names, value = TRUE, perl = TRUE
      )
      has_interaction <- length(effort_x_concept_name) > 0
      
      cat(sprintf("  Effort term:      %s\n", effort_nm))
      if (has_interaction)
        cat(sprintf("  Interaction term: %s\n", effort_x_concept_name))
      cat("\n")
      
      b_effort  <- fe_draws[[effort_nm]]
      b_ex_con  <- if (has_interaction) fe_draws[[effort_x_concept_name]] else rep(0, nrow(fe_draws))
      effort_sd <- sd(mod$data[[effort_nm]], na.rm = TRUE)
      
      draws <- tibble::tibble(
        .draw       = seq_len(nrow(fe_draws)),
        b_intercept = b_intercept,
        b_effort    = b_effort,
        b_concept   = b_concept,
        b_ex_con    = b_ex_con
      ) |>
        mutate(
          vlow_avg     = b_intercept + (-2) * effort_sd * b_effort,
          low_avg      = b_intercept + (-1) * effort_sd * b_effort,
          avg_avg      = b_intercept,
          high_avg     = b_intercept +   1  * effort_sd * b_effort,
          vhigh_avg    = b_intercept +   2  * effort_sd * b_effort,
          productivity = 2 * effort_sd * b_effort,
          slope_early  = b_effort + (-1) * concept_sd * b_ex_con,
          slope_late   = b_effort +   1  * concept_sd * b_ex_con,
          delta_slope  = slope_late - slope_early
        )
      
      cat("  Predicted similarity at average concept number:\n")
      print_result(draws, "vlow_avg",  "  Very low effort (-2SD):")
      print_result(draws, "low_avg",   "  Low effort (-1SD):")
      print_result(draws, "avg_avg",   "  Average effort:")
      print_result(draws, "high_avg",  "  High effort (+1SD):")
      print_result(draws, "vhigh_avg", "  Very high effort (+2SD):")
      cat("\n  Effort productivity (Δ similarity, -1SD to +1SD):\n")
      print_result(draws, "productivity", "  Overall productivity:")
      cat("\n  Effort slope by concept number:\n")
      print_result(draws, "slope_early", "  Effort slope at early concepts (-1SD):")
      print_result(draws, "slope_late",  "  Effort slope at late concepts (+1SD):")
      print_result(draws, "delta_slope", "  Change in slope (late - early):")
      cat("\n")
      
      # ── Trajectory ────────────────────────────────────────────────────────────
      concept_seq <- seq(-2, 2, by = 0.5) * concept_sd
      sd_levels   <- c(vlow = -2, low = -1, avg = 0, high = 1, vhigh = 2)
      
      traj_draws <- purrr::map_dfr(names(sd_levels), function(grp) {
        sd_val <- sd_levels[[grp]]
        purrr::map_dfr(concept_seq, function(con_val) {
          sim <- draws$b_intercept +
            sd_val * effort_sd * draws$b_effort +
            con_val * draws$b_concept +
            sd_val * effort_sd * con_val * draws$b_ex_con
          q <- quantile(sim, c(.025, .5, .975))
          tibble::tibble(group = grp, concept_val = con_val,
                         lo = q[1], med = q[2], hi = q[3])
        })
      }) |>
        mutate(group = factor(group,
                              levels = c("vlow","low","avg","high","vhigh"),
                              labels = c("−2 SD","−1 SD","Average","+1 SD","+2 SD")))
      
      p_traj <- traj_draws |>
        ggplot(aes(x = concept_val, y = med,
                   colour = group, fill = group, group = group)) +
        geom_ribbon(aes(ymin = lo, ymax = hi), alpha = 0.12, colour = NA) +
        geom_line(linewidth = 1.2) +
        scale_colour_manual(values = effort_palette, name = "Total effort level") +
        scale_fill_manual(values = effort_palette, guide = "none") +
        theme_effort_plot() +
        labs(title = label,
             subtitle = "Predicted final similarity across concept number by effort level",
             x = "Concept number (centred)", y = "Predicted cosine similarity")
      
      p_prod <- draws |>
        ggplot(aes(x = productivity, y = "Productivity")) +
        stat_halfeye(fill = "#1565C0", .width = c(0.89, 0.95),
                     point_interval = median_hdi, scale = 0.5, alpha = 0.85) +
        geom_vline(xintercept = 0, linetype = "dashed",
                   colour = "grey20", linewidth = 0.8) +
        theme_effort_plot() +
        labs(title = label, subtitle = "Effort productivity",
             x = "Δ cosine similarity (−1SD to +1SD effort)", y = NULL)
      
      p_slope <- draws |>
        select(.draw, slope_early, slope_late) |>
        tidyr::pivot_longer(c(slope_early, slope_late),
                            names_to = "phase", values_to = "slope") |>
        mutate(phase = factor(phase,
                              levels = c("slope_early","slope_late"),
                              labels = c("Early (−1 SD)","Late (+1 SD)"))) |>
        ggplot(aes(x = slope, y = phase, fill = phase)) +
        stat_halfeye(.width = c(0.89, 0.95), point_interval = median_hdi,
                     normalize = "groups", scale = 0.5, alpha = 0.85) +
        geom_vline(xintercept = 0, linetype = "dashed",
                   colour = "grey20", linewidth = 0.8) +
        scale_fill_manual(values = c("Early (−1 SD)" = "#607D8B",
                                     "Late (+1 SD)"  = "#E53935"),
                          guide = "none") +
        theme_effort_plot() +
        labs(title = label, subtitle = "Effort slope at early vs late concepts",
             x = "Effort slope", y = NULL)
      
      plot_list[[plot_key]] <<- list(traj = p_traj, prod = p_prod, slope = p_slope)
    })
  })
  
  # ── Assemble: one row per channel across models ────────────────────────────────
  nms_available <- names(Filter(Negate(is.null), plot_list))
  
  if (length(nms_available) == 0) {
    message("No plots to assemble.")
    return(invisible(plot_list))
  }
  
  traj_row  <- purrr::map(nms_available, ~ plot_list[[.x]]$traj) |>
    patchwork::wrap_plots(nrow = 1) +
    patchwork::plot_layout(guides = "collect") &
    theme(legend.position = "bottom")
  
  prod_row  <- purrr::map(nms_available, ~ plot_list[[.x]]$prod) |>
    patchwork::wrap_plots(nrow = 1)
  
  slope_row <- purrr::map(nms_available, ~ plot_list[[.x]]$slope) |>
    patchwork::wrap_plots(nrow = 1) +
    patchwork::plot_layout(guides = "collect") &
    theme(legend.position = "bottom")
  
  final <- (traj_row / (prod_row | slope_row)) +
    patchwork::plot_annotation(
      title    = "Total episode effort and final semantic similarity",
      subtitle = "Top: effort × concept number | Bottom left: productivity | Bottom right: slope change",
      theme    = theme(plot.title    = element_text(size = 14, face = "bold"),
                       plot.subtitle = element_text(size = 11, colour = "grey40"))
    )
  
  print(final)
  ggsave("effort_by_concept_interaction.png", final,
         width = 20, height = 12, dpi = 300, bg = "white")
  
  invisible(plot_list)
}

diagnose_random_structure <- function(model, data, threshold_rhat = 1.01, 
                                      threshold_neff = 0.1,
                                      threshold_est_ratio = 0.30) {
  
  # Helper to safely extract a scalar value
  safe_val <- function(x) {
    if (length(x) == 0 || all(is.na(x))) return(NA)
    x[1]
  }
  
  cat("═══════════════════════════════════════════════════════\n")
  cat("  RANDOM STRUCTURE DIAGNOSTICS\n")
  cat("═══════════════════════════════════════════════════════\n\n")
  
  vc        <- VarCorr(model)
  rhat_vals <- brms::rhat(model)
  neff_vals <- brms::neff_ratio(model)
  
  # ── 1. Convergence overview ─────────────────────────────────────────────────
  cat("── 1. CONVERGENCE OVERVIEW ────────────────────────────\n")
  cat(sprintf("  max Rhat:        %.4f  %s\n", max(rhat_vals, na.rm=TRUE),
              ifelse(max(rhat_vals, na.rm=TRUE) > 1.05, "⚠ CRITICAL", 
                     ifelse(max(rhat_vals, na.rm=TRUE) > 1.01, "⚠ WARNING", "✓ OK"))))
  cat(sprintf("  min neff ratio:  %.4f  %s\n", min(neff_vals, na.rm=TRUE),
              ifelse(min(neff_vals, na.rm=TRUE) < 0.05, "⚠ CRITICAL",
                     ifelse(min(neff_vals, na.rm=TRUE) < 0.1, "⚠ WARNING", "✓ OK"))))
  cat(sprintf("  n Rhat > 1.01:   %d\n",   sum(rhat_vals > 1.01, na.rm=TRUE)))
  cat(sprintf("  n neff < 0.1:    %d\n\n", sum(neff_vals < 0.1,  na.rm=TRUE)))
  
  # ── 2. Random SD diagnostics ────────────────────────────────────────────────
  cat("── 2. RANDOM EFFECT SDs ───────────────────────────────\n")
  cat(sprintf("  %-55s %8s %8s %8s %8s %s\n",
              "Parameter", "Est", "CI_low", "CI_high", "neff", "Verdict"))
  cat(paste(rep("─", 105), collapse=""), "\n")
  
  sd_remove <- c()
  
  for (group_name in names(vc)) {
    sd_df <- as.data.frame(vc[[group_name]]$sd)
    
    for (i in seq_len(nrow(sd_df))) {
      param   <- rownames(sd_df)[i]
      est     <- safe_val(sd_df[i, "Estimate"])
      ci_low  <- safe_val(sd_df[i, "Q2.5"])
      ci_high <- safe_val(sd_df[i, "Q97.5"])
      
      # Match neff
      neff_match <- neff_vals[grepl(
        paste0("sd_", group_name, ".*", gsub("[():]", ".", param)), 
        names(neff_vals)
      )]
      neff_val <- if (length(neff_match) > 0) min(neff_match, na.rm=TRUE) else NA
      
      # Match rhat
      rhat_match <- rhat_vals[grepl(
        paste0("sd_", group_name, ".*", gsub("[():]", ".", param)),
        names(rhat_vals)
      )]
      rhat_val <- if (length(rhat_match) > 0) max(rhat_match, na.rm=TRUE) else NA
      
      # Criteria
      ci_touches_zero <- length(ci_low)  > 0 && !is.na(ci_low)  && ci_low < 0.001
      
      estimate_small  <- length(est)     > 0 && !is.na(est) &&
        length(ci_high) > 0 && !is.na(ci_high) &&
        ci_high > 0 &&
        (est / ci_high) < threshold_est_ratio
      
      neff_bad        <- length(neff_val) > 0 && !is.na(neff_val) && 
        neff_val < threshold_neff
      rhat_bad        <- length(rhat_val) > 0 && !is.na(rhat_val) && 
        rhat_val > threshold_rhat
      
      ci_width <- if (length(ci_low) > 0 && length(ci_high) > 0 &&
                      !is.na(ci_low) && !is.na(ci_high)) {
        ci_high - ci_low
      } else NA
      
      relative_width <- if (!is.na(ci_width) && length(est) > 0 && !is.na(est)) {
        ci_width / (est + 0.001)
      } else NA
      
      reasons <- c()
      if (isTRUE(ci_touches_zero))                              reasons <- c(reasons, "CI->0")
      if (isTRUE(estimate_small))                               reasons <- c(reasons, "est<30%_upper_CI")
      if (isTRUE(neff_bad))                                     reasons <- c(reasons, "low_neff")
      if (isTRUE(rhat_bad))                                     reasons <- c(reasons, "bad_Rhat")
      if (!is.na(relative_width) && isTRUE(relative_width > 3)) reasons <- c(reasons, "wide_CI")
      
      verdict <- if (length(reasons) > 0) {
        paste("⚠ CONSIDER REMOVING:", paste(reasons, collapse="+"))
      } else "✓ keep"
      
      cat(sprintf("  %-55s %8s %8s %8s %8s %s\n",
                  paste0(group_name, ": ", param),
                  ifelse(is.na(est),     "NA", sprintf("%.3f", est)),
                  ifelse(is.na(ci_low),  "NA", sprintf("%.3f", ci_low)),
                  ifelse(is.na(ci_high), "NA", sprintf("%.3f", ci_high)),
                  ifelse(is.na(neff_val),"NA", sprintf("%.3f", neff_val)),
                  verdict))
      
      if (length(reasons) > 0) {
        sd_remove <- c(sd_remove, paste0("sd(", param, ") | ", group_name))
      }
    }
  }
  
  # ── 3. Correlation diagnostics ──────────────────────────────────────────────
  cat("\n── 3. RANDOM EFFECT CORRELATIONS ─────────────────────\n")
  cat(sprintf("  %-65s %8s %8s %8s %8s %s\n",
              "Parameter", "Est", "CI_low", "CI_high", "neff", "Verdict"))
  cat(paste(rep("─", 115), collapse=""), "\n")
  
  cor_remove_groups <- c()
  
  for (group_name in names(vc)) {
    cor_array <- vc[[group_name]]$cor
    if (is.null(cor_array)) next
    
    param_names <- dimnames(cor_array)[[1]]
    
    for (i in seq_along(param_names)) {
      for (j in seq_along(param_names)) {
        if (j <= i) next  # upper triangle only
        
        p1  <- param_names[i]
        p2  <- param_names[j]
        
        est     <- safe_val(cor_array[p1, "Estimate", p2])
        ci_low  <- safe_val(cor_array[p1, "Q2.5",     p2])
        ci_high <- safe_val(cor_array[p1, "Q97.5",    p2])
        param   <- paste0(p1, " x ", p2)
        
        # Match neff
        neff_match <- neff_vals[grepl(
          paste0("cor_", group_name, ".*",
                 gsub("[()[:space:]:]", ".", p1)),
          names(neff_vals)
        )]
        neff_val <- if (length(neff_match) > 0) min(neff_match, na.rm=TRUE) else NA
        
        # Criteria
        ci_excludes_zero <- length(ci_low) > 0 && length(ci_high) > 0 &&
          !is.na(ci_low) && !is.na(ci_high) &&
          (ci_low > 0 | ci_high < 0)
        
        ci_spans_zero    <- length(ci_low) > 0 && length(ci_high) > 0 &&
          !is.na(ci_low) && !is.na(ci_high) &&
          ci_low < 0 && ci_high > 0
        
        ci_width <- if (length(ci_low) > 0 && length(ci_high) > 0 &&
                        !is.na(ci_low) && !is.na(ci_high)) {
          ci_high - ci_low
        } else NA
        
        very_wide_ci <- !is.na(ci_width) && isTRUE(ci_width > 1.2)
        
        neff_bad <- length(neff_val) > 0 && !is.na(neff_val) &&
          neff_val < threshold_neff
        
        reasons <- c()
        
        if (isTRUE(ci_excludes_zero)) {
          # CI excludes zero — credible correlation, always keep
          # Only note if neff is low (needs more iterations, not removal)
          verdict <- "✓ keep (credible)"
          if (isTRUE(neff_bad))
            verdict <- "✓ keep (credible) — low neff: consider more iterations"
        } else {
          # CI spans zero
          if (isTRUE(ci_spans_zero))
            reasons <- c(reasons, "CI_spans_zero")
          if (isTRUE(ci_spans_zero) && isTRUE(very_wide_ci))
            reasons <- c(reasons, "uninformative")
          # Low neff only flagged when CI also spans zero
          if (isTRUE(neff_bad) && isTRUE(ci_spans_zero))
            reasons <- c(reasons, "low_neff")
          
          verdict <- if (length(reasons) > 0) {
            paste("⚠ CONSIDER ||:", paste(reasons, collapse="+"))
          } else "✓ keep"
        }
        
        cat(sprintf("  %-65s %8s %8s %8s %8s %s\n",
                    paste0(group_name, ": ", param),
                    ifelse(is.na(est),     "NA", sprintf("%.3f", est)),
                    ifelse(is.na(ci_low),  "NA", sprintf("%.3f", ci_low)),
                    ifelse(is.na(ci_high), "NA", sprintf("%.3f", ci_high)),
                    ifelse(is.na(neff_val),"NA", sprintf("%.3f", neff_val)),
                    verdict))
        
        if (length(reasons) > 0) {
          cor_remove_groups <- c(cor_remove_groups, group_name)
        }
      }
    }
  }
  cor_remove_groups <- unique(cor_remove_groups)
  
  # ── 4. Data sparsity check ──────────────────────────────────────────────────
  cat("\n── 4. DATA SPARSITY PER RANDOM SLOPE ─────────────────\n")
  
  sparse_slopes <- c()
  
  for (group_name in names(vc)) {
    sd_df  <- as.data.frame(vc[[group_name]]$sd)
    slopes <- rownames(sd_df)[rownames(sd_df) != "Intercept"]
    
    if (length(slopes) == 0) next
    
    group_col <- tryCatch({
      group_vars <- names(data)[names(data) %in% 
                                  c(group_name, tolower(group_name),
                                    gsub("_ID$", "", group_name),
                                    gsub("_id$", "", group_name))]
      if (length(group_vars) == 0) NULL else group_vars[1]
    }, error = function(e) NULL)
    
    if (is.null(group_col) || is.na(group_col)) {
      cat(sprintf("  [%s]: could not match to data column — check manually\n",
                  group_name))
      next
    }
    
    n_units <- length(unique(data[[group_col]]))
    cat(sprintf("  %s (%d units):\n", group_name, n_units))
    
    for (slope in slopes) {
      slope_clean <- gsub("correction(\\d+M\\d+)", "correction", slope)
      slope_col   <- names(data)[names(data) %in% 
                                   c(slope, slope_clean, gsub(":", "_", slope))]
      
      if (length(slope_col) > 0) {
        obs_per_unit <- data |>
          dplyr::group_by(.data[[group_col]]) |>
          dplyr::summarise(n = sum(!is.na(.data[[slope_col[1]]])),
                           .groups = "drop")
        pct_sufficient <- mean(obs_per_unit$n >= 3) * 100
        cat(sprintf("    slope %-35s: median obs/unit = %5.1f  pct >= 3 obs = %5.1f%%  %s\n",
                    slope,
                    median(obs_per_unit$n),
                    pct_sufficient,
                    ifelse(pct_sufficient < 50, "⚠ SPARSE", "✓ ok")))
        if (pct_sufficient < 50)
          sparse_slopes <- c(sparse_slopes, paste0(slope, " | ", group_name))
      } else {
        cat(sprintf("    slope %-35s: [could not check sparsity]\n", slope))
      }
    }
  }
  
  # ── 5. Summary of recommendations ──────────────────────────────────────────
  cat("\n═══════════════════════════════════════════════════════\n")
  cat("  RECOMMENDATIONS\n")
  cat("═══════════════════════════════════════════════════════\n\n")
  
  if (length(sd_remove) > 0) {
    cat("  Remove these random slopes (near-zero SD / small est / low neff / bad Rhat):\n")
    for (r in unique(sd_remove)) cat(sprintf("    - %s\n", r))
    cat("\n")
  }
  
  if (length(cor_remove_groups) > 0) {
    cat("  Switch from | to || for these grouping factors\n")
    cat("  (correlations span zero — uninformative or poorly estimated):\n")
    for (g in cor_remove_groups) cat(sprintf("    - %s\n", g))
    cat("\n")
  }
  
  if (length(sparse_slopes) > 0) {
    cat("  These slopes may be unestimable due to data sparsity:\n")
    for (s in sparse_slopes) cat(sprintf("    - %s\n", s))
    cat("\n")
  }
  
  if (length(c(sd_remove, cor_remove_groups, sparse_slopes)) == 0) {
    cat("  No issues detected — random structure looks appropriate.\n\n")
  }
  
  # ── 6. Current formula ──────────────────────────────────────────────────────
  cat("  Current formula:\n")
  cat(sprintf("  %s\n\n", as.character(formula(model))[1]))
  
  invisible(list(
    sd_remove         = unique(sd_remove),
    cor_remove_groups = cor_remove_groups,
    sparse_slopes     = sparse_slopes
  ))
}

plot_h2_prev_answer <- function(models_h2, dv_labels = NULL) {
  
  # ── Default DV labels ───────────────────────────────────────────────────────
  if (is.null(dv_labels)) {
    dv_labels <- names(models_h2)
  }
  
  # ── Helper ──────────────────────────────────────────────────────────────────
  print_result <- function(draws, var, label, pct = FALSE) {
    res  <- draws |> median_hdi(!!sym(var))
    est  <- res[[var]]
    lo   <- res$.lower
    hi   <- res$.upper
    cred <- lo > 0 | hi < 0
    if (pct) {
      cat(sprintf("  %-45s %+.1f%% [%+.1f%%, %+.1f%%]%s\n",
                  label, est, lo, hi, ifelse(cred, "  *", "")))
    } else {
      cat(sprintf("  %-45s %.3f [%.3f, %.3f]%s\n",
                  label, est, lo, hi, ifelse(cred, "  *", "")))
    }
  }
  
  cat("═══════════════════════════════════════════════\n")
  cat("  H2: EFFECT OF PREVIOUS ANSWER SIMILARITY\n")
  cat("  (answer_prev_dist_z: higher = closer)\n")
  cat("═══════════════════════════════════════════════\n\n")
  
  # ── Extract posteriors for all models ──────────────────────────────────────
  posterior_list <- purrr::imap(models_h2, function(mod, nm) {
    
    label <- dv_labels[[nm]]
    
    cat(sprintf("  %s\n", label))
    cat(sprintf("  %s\n", strrep("─", nchar(label))))
    
    draws <- mod |>
      spread_draws(b_answer_prev_dist_z) |>
      mutate(pct = (exp(b_answer_prev_dist_z) - 1) * 100)
    
    print_result(draws, "b_answer_prev_dist_z", "  Log scale:")
    print_result(draws, "pct",                  "  % change per SD:", pct = TRUE)
    cat("\n")
    
    draws |> mutate(dv = label)
  })
  
  # ── Combine and plot ────────────────────────────────────────────────────────
  all_draws <- bind_rows(posterior_list) |>
    mutate(dv = factor(dv, levels = rev(unlist(dv_labels))))
  
  # Compute summaries for credibility colouring
  summary_df <- all_draws |>
    group_by(dv) |>
    median_hdi(pct) |>
    mutate(credible = .lower > 0 | .upper < 0)
  
  p <- all_draws |>
    left_join(summary_df |> select(dv, credible), by = "dv") |>
    ggplot(aes(x = pct, y = dv, fill = credible)) +
    stat_halfeye(
      .width = c(0.89, 0.95),
      point_interval = median_hdi,
      normalize = "groups",
      scale = 0.7,
      alpha = 0.85
    ) +
    geom_vline(xintercept = 0, linetype = "dashed",
               colour = "grey20", linewidth = 0.8) +
    scale_fill_manual(
      values = c("TRUE" = "#2196F3", "FALSE" = "#B0BEC5"),
      guide  = "none"
    ) +
    scale_x_continuous(
      labels = function(x) paste0(ifelse(x > 0, "+", ""), round(x, 0), "%")
    ) +
    theme_effort_plot() +
    labs(
      title    = "Effect of previous answer similarity on current effort",
      subtitle = "% change per 1 SD increase in cosine similarity | Median ± 89% and 95% HDI\nBlue = credible effect (95% CI excludes zero)",
      x = "% change in effort per SD of answer similarity",
      y = NULL
    )
  
  print(p)
  
  ggsave("h2_prev_answer_effect.png", p,
         width = 9, height = 6, dpi = 300, bg = "white")
  
  invisible(p)
}
