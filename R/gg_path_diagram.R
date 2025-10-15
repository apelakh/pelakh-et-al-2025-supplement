# Helper to calculate slope
calculate_angle <- function(x1, y1, x2, y2) {
  # Check if the x-coordinates are the same (vertical line)
  if (x1 == x2) {
    return(Inf) # Return infinity for vertical lines
  }

  # Calculate the slope using the formula: (y2 - y1) / (x2 - x1)
  slope <- (y2 - y1) / (x2 - x1)
  angle <- atan(slope) * (180 / pi)

  return(angle)
}

display_num <- function(x) {
  scales::number(x, accuracy = .01)
}


gg_path_diagram <- function(
  # 3 models required to get the coefficients and standard errors:
  mod_m,
  mod_t,
  mod_y,
  # Terms to search for within the model output. If NULL, label will be used.
  str_med = NULL,
  str_iv = NULL,
  # Labels for the boxes in the diagram:
  lbl_med,
  lbl_iv,
  lbl_dv,
  # Width and height of rectangles:
  rect_width = 4,
  rect_height = 1.5,
  # Padding between the IV and DV rectangles:
  pad_x = 5,
  # Padding between the lower and upper rectangles:
  pad_y = 1.5,
  # Text size for inside the rectangles:
  rect_lbl_size = 5,
  path_lbl_size = 4.5,
  # Padding between the labels and the arrows:
  lbl_path_pad = .2,
  pad_arrow = .05
) {
  if (is.null(str_med)) {
    str_med <- lbl_med
  }
  if (is.null(str_iv)) {
    str_iv <- lbl_iv
  }

  anchors <- list(
    Mediator = list(x = 0, y = .5 * (pad_y + rect_height)),
    IV = list(x = -(rect_width + pad_x) / 2, y = -.5 * (pad_y + rect_height)),
    DV = list(x = (rect_width + pad_x) / 2, y = -.5 * (pad_y + rect_height))
  )

  df_lines <- bind_rows(
    mod_t %>%
      broom::tidy() %>%
      filter(term == str_iv) %>%
      mutate(path = "C", .before = 1),

    mod_m %>% broom::tidy() %>% filter(term == str_iv) %>% mutate(path = "A"),

    mod_y %>%
      broom::tidy() %>%
      filter(term %in% c(str_iv, str_med)) %>%
      mutate(path = c("C_prime", "B"))
  ) %>%
    select(-term, -statistic) %>%
    arrange(path) %>%
    rename(coef = estimate, err = std.error, p = p.value) %>%
    mutate(
      # across(c(coef, err), ~ scales::number(.x, accuracy = .01) %>% as.numeric),
      # across(c(coef, err), ~ round(.x, 2)),
      p.signif = case_when(
        p < .001 ~ "***",
        p < .01 ~ "**",
        p < .05 ~ "*",
        p < .1 ~ "\u2020",
        TRUE ~ ""
      ),
      label = case_match(
        path,
        "A" ~ paste0(display_num(coef), "(", display_num(err), ")", p.signif),
        "B" ~ paste0(display_num(coef), "(", display_num(err), ")", p.signif),
        "C" ~
          paste0(
            "c = ",
            display_num(coef),
            "(",
            display_num(err),
            ")",
            p.signif
          ),
        "C_prime" ~
          paste0(
            "c\u2032 = ",
            display_num(coef),
            "(",
            display_num(err),
            ")",
            p.signif
          ),
      ),
      x = c(
        anchors$IV$x,
        anchors$Mediator$x + (.5 * rect_width),
        rep(-.5 * pad_x, 2)
      ),
      y = c(
        -.5 * pad_y,
        .5 * pad_y,
        -.5 * pad_y - .4 * rect_height,
        -.5 * pad_y - .6 * rect_height
      ),
      xend = c(
        -.5 * rect_width - pad_arrow,
        .5 * (rect_width + pad_x - pad_arrow),
        rep(.5 * pad_x - pad_arrow, 2)
      ),
      yend = c(
        .5 * pad_y - pad_arrow,
        -.5 * pad_y + pad_arrow,
        -.5 * pad_y - .4 * rect_height,
        -.5 * pad_y - .6 * rect_height
      )
    ) %>%
    rowwise() %>%
    mutate(
      lbl_x = mean(c_across(c(x, xend))),
      lbl_y = mean(c_across(c(y, yend)))
    ) %>%
    ungroup %>%
    mutate(
      line_color = case_when(
        p > .05 & coef < 0 ~ "ns_negative",
        p > .05 & coef >= 0 ~ "ns_positive",
        coef < 0 ~ "negative",
        coef >= 0 ~ "positive"
      )
    )

  df_rect <- tibble(
    rect = names(anchors),
    label = c(lbl_med, lbl_iv, lbl_dv)
  ) %>%
    rowwise() %>%
    mutate(
      x = anchors[[rect]]$x,
      y = anchors[[rect]]$y
    ) %>%
    ungroup()

  df_rect %>%
    ggplot(mapping = aes(x = x, y = y)) +
    geom_segment(
      data = df_lines,
      mapping = aes(
        group = path,
        xend = xend,
        yend = yend,
        linewidth = scales::rescale(
          abs(coef),
          to = c(.5, 2),
          from = c(.01, .6)
        ),
        color = line_color
      ),
      arrow = arrow(angle = 20, length = unit(.3, "cm"), type = "closed"),
      linejoin = "mitre",
      lineend = "butt"
    ) +
    geom_tile(
      mapping = aes(group = rect),
      width = rect_width,
      height = rect_height,
      fill = "white",
      color = "gray20",
      linewidth = .5
    ) +
    geom_text(
      mapping = aes(label = label, group = rect),
      size = rect_lbl_size
    ) +
    geom_text(
      data = df_lines,
      mapping = aes(
        group = path,
        label = label,
        x = lbl_x,
        y = lbl_y,
        color = p < .05
      ),
      hjust = c(1, 0, .5, .5),
      vjust = c(0, 0, 0, 1),
      nudge_x = c(-lbl_path_pad, lbl_path_pad, 0, 0),
      nudge_y = c(lbl_path_pad, lbl_path_pad, lbl_path_pad, -lbl_path_pad),
      size = path_lbl_size
    ) +
    coord_equal() +
    # scale_linewidth_continuous(range = c(.5, 1.2)) +
    scale_linewidth_identity() +
    scale_color_manual(
      values = c(
        "TRUE" = "gray20",
        "ns" = "gray50",
        "positive" = "#118AB2",
        "ns_positive" = "gray50",
        "negative" = "#CD131F",
        "ns_negative" = "gray50"
      )
    ) +
    theme_void(base_size = 15) +
    theme(legend.position = "none")
}
