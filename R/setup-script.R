# Libraries
if (!require(pacman)) {
  install.packages("pacman")
}

pacman::p_load(
  kableExtra,
  performance,
  gvlma,
  mediation,
  lmerTest,
  sjPlot,
  sjmisc,
  sjlabelled,
  sjtable2df,
  GGally,
  patchwork,
  showtext,
  rstatix,
  ggpubr,
  tidyverse
)

font_add_google("Oswald", "oswald")
showtext_auto(enable = TRUE)

source("R/helper_functions.R")

# Set global theme for plots

color_threat <- "#BD6C27"
color_perc <- "#099395"
color_mindful <- "#25546E"
color_cntrl <- "#acabac"
color_dark <- "#21333B"


theme_set(
  theme_bw(base_size = 13, base_family = "oswald") +
    theme(
      text = element_text(color = color_dark),
      panel.grid.major.x = element_blank(),
      panel.grid = element_line(linewidth = .3),
      plot.caption = element_text(size = 13, color = color_cntrl)
    )
)


# Variables
str_perception_levels <- c("Confidence", "Anxiety", "Difficulty")
med_sims <- 10000 # Set this to 10,000 and run before publishing

# Read data
data_raw <- read_csv("data/preregistration_3_data_public.csv")

# Custom function to generate path diagrams with GGPlot2
source("R/gg_path_diagram.R")

# Data setup for mixed models
data_rq1 <- data_raw %>%
  # select(-EMA_Threat, -Question) |>
  mutate(
    Timepoint = factor(Timepoint, levels = c("Baseline", "Posttest")),
    Condition = factor(Condition, levels = c("Control", "Mindfulness")),
    perception = factor(perception, levels = str_perception_levels),
    Gender = set_centered_contrasts(Gender, ref_level = "Men"),
    Cohort = set_centered_contrasts(Cohort, ref_level = "Cohort 1"),
    `Item-Level Accuracy` = factor(
      `Item-Level Accuracy`,
      labels = c("Incorrect", "Correct")
    ) %>%
      set_centered_contrasts()
  ) %>%
  set_contrasts(Test_Version, c(-.5, .5)) %>%
  set_contrasts(Condition, c(-.5, .5))

# Data setup for mediation models
data_rq2 <- data_raw %>%
  # Remove participant who did not complete EMA surveys
  drop_na(EMA_Threat) %>%
  pivot_wider(
    names_from = perception,
    values_from = rating
  ) %>%
  summarise(
    .by = c(
      Participant,
      Condition,
      Gender,
      Timepoint,
      Cohort,
      Semester_Week,
      Test_Version,
      Baseline_Threat,
      EMA_Threat
    ),
    n_items = n(),
    across(c(Score, Confidence, Anxiety, Difficulty), ~ mean(.x, na.rm = TRUE))
  ) %>%
  pivot_wider(
    names_from = Timepoint,
    values_from = c(
      n_items,
      Score,
      Confidence,
      Anxiety,
      Difficulty,
      Test_Version
    ),
    names_glue = "{Timepoint}_{.value}"
  ) %>%
  mutate(Gender = factor(Gender) %>% relevel(ref = "Men")) %>%
  mutate(across(Posttest_Test_Version, ~ factor(.x))) %>%
  mutate(across(Condition, ~ factor(.x))) %>%
  mutate(across(Cohort, ~ factor(.x))) %>%
  # Create a numeric dummy variables for Cohort
  mutate(
    Cohort_2 = if_else(Cohort == "Cohort 2", 1, 0),
    Cohort_3 = if_else(Cohort == "Cohort 3", 1, 0),
    # Standardize numeric variables
    across(c(where(is.numeric), -ends_with("n_items")), ~ standardize(.x))
  )
