excel_sheets("Datasets_Kokubu_2025.xlsx")

#I can read all three sheets at once
all_data <- excel_sheets("Datasets_Kokubu_2025.xlsx") |>
  set_names() |>
  map(~ read_excel("Datasets_Kokubu_2025.xlsx", sheet = .x))

#Or I can  read three different excel sheets in the data set and name them separately
Trial.Level <- f
Participant.Level <- read_excel("Datasets_Kokubu_2025.xlsx", sheet = "Participant-level summary data")
Characteristics.Videos <- read_excel("Datasets_Kokubu_2025.xlsx", sheet = "Characteristics of videos")

# To replicate Table 2
table2 <- Trial.Level |>
  filter(!is.na(`Result (1:Correct, 0:Incorrect)`),
         !is.na(`Decision time (sec)`),
         !is.na(`Confidence rating`)) |>
  mutate(
    Result = if_else(`Result (1:Correct, 0:Incorrect)` == 1, "Correct", "Incorrect")
  ) |>
  group_by(Result) |>
  summarise(
    decision_time_M = mean(`Decision time (sec)`, na.rm = TRUE),
    decision_time_SD = sd(`Decision time (sec)`, na.rm = TRUE),
    confidence_M = mean(`Confidence rating`, na.rm = TRUE),
    confidence_SD = sd(`Confidence rating`, na.rm = TRUE),
    n = n()
  )

#Format it like the paper
install.packages("gt")
library(gt)

table2_paper %>%
  gt() %>%
  tab_header(
    title = "Table 2",
    subtitle = md("**Means and standard deviations of decision time and confidence rating for correct and incorrect decisions.**")
  ) %>%
  cols_label(
    Variables = "Variables",
    Statistic = "",
    Correct = "Correct",
    Incorrect = "Incorrect"
  ) %>%
  tab_source_note(
    source_note = md("*** Significant difference between correct and incorrect decisions (*p* < .001).")
  )

# plotting number of fixation per trail vs. decision accuracy
participant_accuracy <- Trial.Level |>
  group_by(`ParticipantID`) |>
  summarise(
    fixation_per_trial = mean(`Number of fixations per trial`, na.rm = TRUE),
    accuracy = mean(`Result (1:Correct, 0:Incorrect)`, na.rm = TRUE) * 100
  )

quad_model2 <- lm(
  accuracy ~ fixation_per_trial + I(fixation_per_trial^2),
  data = participant_accuracy
)

summary(quad_model2)

coefs <- coef(quad_model2)
r2 <- summary(quad_model2)$r.squared
fstat <- summary(quad_model2)$fstatistic
pval <- pf(fstat[1], fstat[2], fstat[3], lower.tail = FALSE)

p_label <- if(pval < .001){
  "p < .001"
} else {
  paste0("p = ", sub("^0", "", sprintf("%.3f", pval)))
}

eq_label <- paste0(
  "y = ",
  sprintf("%.2f", coefs[3]), "x² + ",
  sprintf("%.2f", coefs[2]), "x ",
  ifelse(coefs[1] < 0, "- ", "+ "),
  sprintf("%.2f", abs(coefs[1])),
  "\nR² = ", sub("^0", "", sprintf("%.3f", r2)),
  "\n", p_label
)

#plot
ggplot(participant_accuracy, aes(x = fixation_per_trial, y = accuracy)) +
  geom_point(shape = 16, size = 3, color = "black") +
  stat_smooth(
    method = "lm",
    formula = y ~ x + I(x^2),
    se = FALSE,
    color = "black",
    linewidth = 0.8,
    linetype = "dotted"
  ) +
  annotate(
    "text",
    x = 3.9,   # push close to right edge
    y = 98,    # near top but inside
    label = eq_label,
    hjust = 1, # anchor text to the RIGHT
    vjust = 1, # anchor text to the TOP
    size = 5
  ) +
  coord_cartesian(
    xlim = c(1, 4),
    ylim = c(30, 100),
    clip = "off"
  ) +
  scale_x_continuous(
    limits = c(1, 4),
    breaks = c(1, 2, 3, 4),
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    limits = c(30, 100),
    breaks = seq(30, 100, 10),
    expand = c(0, 0)
  ) +
  labs(
    x = "Number of fixation per trial",
    y = "Decision accuracy (%)"
  ) +
  theme(
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 13, color = "black"),
    axis.line = element_line(linewidth = 0.8, color = "black"),
    axis.ticks = element_line(linewidth = 0.8, color = "black")
  )

#replicate figure 4
ball_data <- Trial.Level |>
  group_by(ParticipantID, GazeB) |>
  summarise(
    decision_time = mean(`Decision time (sec)`, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    gaze = if_else(GazeB == 1, "With", "Without"),
    area = "Gaze at ball"
  )

home_data <- Trial.Level |>
  group_by(ParticipantID, GazeH) |>
  summarise(
    decision_time = mean(`Decision time (sec)`, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    gaze = if_else(GazeH == 1, "With", "Without"),
    area = "Gaze at home plate"
  )

runner_data <- Trial.Level |>
  group_by(ParticipantID, GazeR) |>
  summarise(
    decision_time = mean(`Decision time (sec)`, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    gaze = if_else(GazeR == 1, "With", "Without"),
    area = "Gaze at runner"
  )
  
other_data <- Trial.Level |>
  group_by(ParticipantID, GazeO) |>
  summarise(
    decision_time = mean(`Decision time (sec)`, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    gaze = if_else(GazeO == 1, "With", "Without"),
    area = "Gaze at other areas"
  )

#plotting four separate figures
install.packages("patchwork")
library(patchwork)
make_panel <- function(data, xlab) {
  ggplot(data, aes(x = gaze, y = decision_time, group = ParticipantID)) +
    geom_line(color = "gray70", linewidth = 0.4) +
    geom_point(color = "gray40", size = 1.2) +
    stat_summary(
      fun = mean,
      geom = "line",
      aes(group = 1),
      color = "black",
      linewidth = 1.2
    ) +
    stat_summary(
      fun = mean,
      geom = "point",
      shape = 21,
      fill = "white",
      color = "black",
      size = 3
    ) +
    stat_summary(
      fun.data = mean_se,
      geom = "errorbar",
      width = 0.08,
      color = "black",
      linewidth = 0.8
    ) +
    coord_cartesian(ylim = c(1.0, 2.2)) +
    scale_y_continuous(breaks = c(1.25, 1.50, 1.75, 2.00)) +
    labs(
      x = xlab,
      y = "Decision time (s)"
    ) +
    theme_classic(base_size = 16) +
    theme(
      axis.title.x = element_text(size = 14),
      axis.title.y = element_text(size = 14),
      axis.text = element_text(size = 12)
    )
}

p_ball <- make_panel(ball_data, "Gaze at ball")
p_home <- make_panel(home_data, "Gaze at home plate")
p_runner <- make_panel(runner_data, "Gaze at runner")
p_other <- make_panel(other_data, "Gaze at other areas")

p_ball <- p_ball +
  annotate("text", x = 0.8, y = 2.18, label = "(a)", size = 6)

p_home <- p_home +
  annotate("text", x = 0.8, y = 2.18, label = "(b)", size = 6) +
  annotate("segment", x = 1, xend = 2, y = 2.07, yend = 2.07, linewidth = 0.8) +
  annotate("text", x = 1.5, y = 2.11, label = "***", size = 6) +
  annotate("text", x = 2.15, y = 1.06, label = "*** p < .001", size = 5, hjust = 0)

p_runner <- p_runner +
  annotate("text", x = 0.8, y = 2.18, label = "(c)", size = 6) +
  annotate("segment", x = 1, xend = 2, y = 2.07, yend = 2.07, linewidth = 0.8) +
  annotate("text", x = 1.5, y = 2.11, label = "***", size = 6) +
  annotate("text", x = 2.15, y = 1.06, label = "*** p < .001", size = 5, hjust = 0)

p_other <- p_other +
  annotate("text", x = 0.8, y = 2.18, label = "(d)", size = 6) +
  annotate("segment", x = 1, xend = 2, y = 2.07, yend = 2.07, linewidth = 0.8) +
  annotate("text", x = 1.5, y = 2.11, label = "***", size = 6) +
  annotate("text", x = 2.15, y = 1.06, label = "*** p < .001", size = 5, hjust = 0)

#combine four graphs together
final_fig <- (p_ball | p_home) / (p_runner | p_other)

#plot figure 5
install.packages("MASS")
library(MASS)

fig5_data <- Participant.Level |>
  transmute(
    id = ID,
    response_bias = `β`,
    prob_ball_pct = `Proportion of viewing time spent on ball (B)` * 100,
    transitions = `Number of transitions between AOIs`
  ) |>
  filter(
    !is.na(response_bias),
    !is.na(prob_ball_pct),
    !is.na(transitions)
  )

mod_a <- rlm(
  response_bias ~ prob_ball_pct,
  data = fig5_data,
  psi = psi.huber
)

mod_b <- rlm(
  response_bias ~ transitions, 
  data = fig5_data,
  psi = psi.huber
)

sum_a <- summary(mod_a)
sum_b <- summary(mod_b)

coef_a <- coef(mod_a)
coef_b <- coef(mod_b)

se_a <- sum_a$coefficients[2, 2]
se_b <- sum_a$coefficients[2, 2]

z_a <- sum_a$coefficients[2, 3]
z_b <- sum_a$coefficients[2, 3]

p_a <- 2 * pnorm(abs(z_a), lower.tail = FALSE)
p_b <- 2 * pnorm(abs(z_b), lower.tail = FALSE)

label_a <- paste0(
  "Intercept = ", sprintf("%.2f", coef_a[1]), "\n",
  "Slope = ", sprintf("%.2f", coef_a[2]), "\n",
  "SE (slope) = ", sprintf("%.2f", se_a), "\n",
  "z = ", sprintf("%.2f", z_a), "\n",
  "p = ", sub("^0", "", sprintf("%.3f", p_a))
)

label_b <- paste0(
  "Intercept = ", sprintf("%.2f", coef_b[1]), "\n",
  "Slope = ", sprintf("%.2f", coef_b[2]), "\n",
  "SE (slope) = ", sprintf("%.2f", se_b), "\n",
  "z = ", sprintf("%.2f", z_b), "\n",
  "p = ", sub("^0", "", sprintf("%.3f", p_b))
)

# Panel A
p_a <- ggplot(fig5_data, aes(x = prob_ball_pct, y = response_bias)) +
  geom_point(size = 2, color = "black") +
  geom_abline(
    intercept = coef_a[1],
    slope = coef_a[2],
    linetype = "dashed",
    linewidth = 0.6,
    color = "black"
  ) +
  annotate("text", x = 70, y = 3.6, label = label_a, hjust = 0, vjust = 1, size = 4.5) +
  annotate("text", x = -5, y = 4.1, label = "(a)", size = 7) +
  coord_cartesian(xlim = c(0, 100), ylim = c(0, 4), clip = "off") +
  labs(
    x = "Proportion of viewing time on ball (%)",
    y = "Response Bias (\u03b2)"
  ) +
  theme(plot.margin = margin(10, 20, 10, 10))

# Panel B
p_b <- ggplot(fig5_data, aes(x = transitions, y = response_bias)) +
  geom_point(size = 2, color = "black") +
  geom_abline(
    intercept = coef_b[1],
    slope = coef_b[2],
    linetype = "dashed",
    linewidth = 0.6,
    color = "black"
  ) +
  annotate("text", x = 1.35, y = 3.6, label = label_b, hjust = 0, vjust = 1, size = 4.5) +
  annotate("text", x = -0.1, y = 4.1, label = "(b)", size = 7) +
  coord_cartesian(xlim = c(0, 2), ylim = c(0, 4), clip = "off") +
  labs(
    x = "Number of transitions between AOIs",
    y = "Response Bias (\u03b2)"
  ) +
  theme(plot.margin = margin(10, 20, 10, 10))

fig5 <- p_a / p_b
fig5
