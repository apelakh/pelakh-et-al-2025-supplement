# Detect output format
output_format <- knitr::opts_knit$get("rmarkdown.pandoc.to")
is_latex <- !is.null(output_format) && output_format == "latex"

# Function to control printing of model results based on output format
# since sjPlot tables don't work well in Latex
handle_model_print <- function(
  mods,
  nm,
  n_models,
  is_lmer = FALSE,
  raneff_rownum = NULL,
  str_estimate = "Estimate"
) {
  if (is_latex) {
    nm <- str_replace_all(nm, "<br>", " ")
  }

  mod_list <- setNames(mods, nm)
  header_args <- setNames(rep(3, length(nm)), nm)
  tbl_cols <- c("Predictor", rep(c(str_estimate, "SE", "p"), n_models))

  if (!is_latex) {
    tab_model(
      mod_list,
      show.se = T,
      string.se = "SE",
      show.ci = F,
      wrap.labels = 300,
      dv.labels = nm
    )
  } else {
    latex_symbols <- c(
      "σ2" = "$\\\\sigma^2$",
      "τ00" = "$\\\\tau_{00}$",
      "^N$" = "$N$",
      "^N\\s" = "$N$",
      "R2" = "$R^2$"
    )

    tab_models <- tab_model(
      mod_list,
      show.se = T,
      string.se = "SE",
      show.ci = F,
      wrap.labels = 300,
      dv.labels = NULL
    ) |>
      sjtable2df::mtab2df(
        n_models = n_models
      ) |>
      tibble::tibble(.name_repair = "universal") |>
      suppressMessages() |>
      mutate(
        across(starts_with("p") & !starts_with("Predict"), \(x) {
          if_else(
            (str_detect(x, "<") |
              (!is.na(as.numeric(x)) & as.numeric(x) < .05)),
            cell_spec(x, bold = TRUE),
            x
          )
        })
      )
    ## Handle random effect formatting for mixed models
    if (is_lmer) {
      tab_models <- tab_models |>
        mutate(
          Predictors = str_replace_all(Predictors, latex_symbols),
          across(
            starts_with("Estimate") | starts_with("SE"),
            \(x) {
              if_else(
                !is.na(x) & x != "" & str_detect(x, "\\s[a-zA-Z]"),
                paste0("$", str_replace(x, "\\s+", "_{"), "}$"),
                as.character(x)
              )
            }
          )
        )
    }
    tab_models <- tab_models |>
      kbl(
        format = "latex",
        booktabs = TRUE,
        escape = FALSE,
        col.names = tbl_cols
      ) |>
      kable_styling(
        latex_options = c("scale_down", "HOLD_position"),
        font_size = 8
      ) |>
      add_header_above(
        c(
          " ",
          header_args
        )
      ) |>
      column_spec(1, width = "4cm") |>
      column_spec(2:10, width = "1cm")

    if (is_lmer) {
      tab_models <- tab_models |>
        row_spec(raneff_rownum, bold = TRUE)
    }

    return(tab_models)
  }
}


kable_mediation <- function(med) {
  footnote <- paste0(
    "Sample Size Used: ",
    med$nobs,
    "; Simulations: ",
    med$sims
  )

  m.title <- "Causal Mediation Analysis using Quasi-Bayesian Confidence Intervals"

  s.title <- paste0(
    "Outcome: ",
    med$model.y$terms[[2]],
    "; Predictor: ",
    med$treat,
    "; Mediatior: ",
    med$mediator,
    "; Covariates: ",
    med$covariates %>% paste(., collapse = ", ")
  )

  r.names <- c(
    "Avg. Causal Mediation Effect",
    "Avg. Direct Effect",
    "Total Effect",
    "Proportion Mediated"
  )
  est <- c(med$d.avg, med$z.avg, med$tau.coef, med$n.avg)
  ci.l <-
    c(med$d.avg.ci[1], med$z.avg.ci[1], med$tau.ci[1], med$n.avg.ci[1])
  ci.u <-
    c(med$d.avg.ci[2], med$z.avg.ci[2], med$tau.ci[2], med$n.avg.ci[2])
  p <- c(med$d.avg.p, med$z.avg.p, med$tau.p, med$n.avg.p)

  tibble(
    Statistic = r.names,
    Estimate = est %>% round(2),
    `CI Lower` = ci.l %>% round(2),
    `CI Upper` = ci.u %>% round(2),
    p = p
  ) |>
    kbl(booktabs = TRUE) |>
    kable_classic() |>
    footnote(
      general = footnote,
      general_title = "Note.",
      footnote_as_chunk = TRUE
    )
}

standardize <- function(x) {
  (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE) %>% return
}

scale.all.numeric <- function(df) {
  df %>% mutate(across(where(is.numeric), ~ standardize(.x))) %>% return
}

se <- function(x) {
  sd(x) / sqrt(length(x))
}


name_list_columns <- function(df) {
  for (col in colnames(df)) {
    if (is_list(df[[col]])) {
      names(df[[col]]) <- df[[as.character(groups(df))]]
    }
  }
  return(df)
}

round_all_doubles <- function(df, .accuracy = .01) {
  df %>%
    mutate(across(where(is.double), ~ scales::number(.x, accuracy = .accuracy)))
}

set_contrasts <- function(df, var, .contrasts, .names = NULL) {
  # If variable is not a factor, make it so
  # TODO might add options to set reference value or labels
  if (!is.factor(pull(df, {{ var }}))) {
    df <- df %>%
      mutate(
        {{ var }} := factor({{ var }})
      )
  }
  # get factor levels
  .levels <- pull(df, {{ var }}) %>% levels
  # if .names is not specified, set names to factor levels minus the reference
  if (is.null(.names)) {
    .names <- .levels[-1]
  }
  # create a variable for the contrast matrix
  contr_mtrx <- matrix(
    .contrasts,
    ncol = length(.names),
    dimnames = list(NULL, .names)
  )
  # set contrasts using dplyr mutate
  df <- df %>%
    mutate(
      {{ var }} := `contrasts<-`({{ var }}, NULL, contr_mtrx)
    )
  # return df
  return(df)
}

# Function to set centered contrasts for a factor
get_centered_contrasts <- function(factor_var, ref_level = NULL) {
  if (!is.factor(factor_var)) {
    factor_var <- factor(factor_var)
  }

  n_levels <- nlevels(factor_var)
  level_names <- levels(factor_var)

  if (!is.null(ref_level)) {
    level_names <- c(
      level_names[level_names == ref_level],
      level_names[level_names != ref_level]
    )
    factor_var <- factor(factor_var, levels = level_names)
  }

  # Get the contrast matrix
  contrast_matrix <- contr.treatment(n_levels)

  # Center the contrasts based on the actual data proportions
  factor_table <- table(factor_var)
  proportions <- as.vector(factor_table / sum(factor_table))

  # Calculate weighted means for each contrast column
  centered_matrix <- contrast_matrix
  for (i in 1:ncol(contrast_matrix)) {
    weighted_mean <- sum(contrast_matrix[, i] * proportions)
    centered_matrix[, i] <- contrast_matrix[, i] - weighted_mean
  }

  # Set row and column names
  rownames(centered_matrix) <- level_names
  colnames(centered_matrix) <- paste0(level_names[-1])

  return(centered_matrix)
}

# Alternative function that directly assigns contrasts to a factor
set_centered_contrasts <- function(factor_var, ref_level = NULL) {
  if (!is.factor(factor_var)) {
    factor_var <- factor(factor_var)
  }

  level_names <- levels(factor_var)

  if (!is.null(ref_level)) {
    level_names <- c(
      level_names[level_names == ref_level],
      level_names[level_names != ref_level]
    )
    factor_var <- factor(factor_var, levels = level_names)
  }

  centered_contrasts <- get_centered_contrasts(
    factor_var,
    ref_level = ref_level
  )
  contrasts(factor_var) <- centered_contrasts
  return(factor_var)
}

# GGAlly helpers -----

p_to_stars <- function(p) {
  symnum(
    p,
    corr = FALSE,
    na = FALSE,
    cutpoints = c(0, 0.05, 1),
    symbols = c("*", ""),
    legend = FALSE
  )
}

fun_diag <- function(data, mapping, ...) {
  ggplot(data = data, mapping = mapping) +
    geom_histogram(
      mapping = aes(y = after_stat(density)),
      bins = 9,
      color = "slategray",
      fill = "slategray",
      alpha = .3
    ) +
    geom_density(..., color = color_dark, linewidth = 1)
}

fun_cor_stat <- function(data, mapping, method, ...) {
  x <- eval_data_col(data, mapping$x)
  y <- eval_data_col(data, mapping$y)

  # corr <- cor(x, y, method=method, use='complete.obs')

  c_test <- cor.test(x, y)
  corr <- c_test$estimate %>%
    scales::number(., accuracy = .01) %>%
    paste0(p_to_stars(c_test$p.value))

  ggally_text(
    label = corr,
    mapping = aes(color = c_test$estimate > 0),
    xP = 0.5,
    yP = 0.5,
    fontface = "bold",
    # color = 'black',
    ...
  ) +
    scale_color_manual(values = c("TRUE" = "#1984c5", "FALSE" = "#c23728"))
}

fun_lower <- function(data, mapping, ...) {
  ggplot(data = data, mapping = mapping) +
    stat_bin_2d(..., bins = 7) +
    scale_fill_viridis_b(option = "magma", begin = .2, end = .8)
}
