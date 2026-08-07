# ============================================================
# Practical Significance — Goals Allowed Calculation
# Using MoneyPuck data (2008-2024) to ground the SV% decline
# in real shot volume context
# ============================================================

# ── Filter: situation = all, GP >= 30 ────────────────────────
mp_filtered <- goalies_2008_to_2024 |>
  filter(situation == "all" & games_played >= 30) |>
  mutate(total_shots = lowDangerShots + mediumDangerShots + highDangerShots)

# ── Average shots faced per season ───────────────────────────
mean_shots <- mean(mp_filtered$total_shots)
median_shots <- median(mp_filtered$total_shots)

cat("Qualifying goalie-seasons:", nrow(mp_filtered), "\n")
cat("Mean shots faced per season:", round(mean_shots, 1), "\n")
cat("Median shots faced per season:", round(median_shots, 1), "\n")

# ── Translate SV% decline into goals allowed ─────────────────
mean_sv_y1   <- mean(goalies_gp30$SV_pct_Y1)
mean_sv_y2   <- mean(goalies_gp30$SV_pct_Y2)
mean_decline <- abs(mean(goalies_gp30$Diff))

goals_y1 <- mean_shots * (1 - mean_sv_y1)
goals_y2 <- mean_shots * (1 - mean_sv_y2)
goals_difference <- goals_y2 - goals_y1

cat("\nMean SV% Year 1:", round(mean_sv_y1, 4), "\n")
cat("Mean SV% Year 2:", round(mean_sv_y2, 4), "\n")
cat("Mean SV% decline:", round(mean_decline, 4), "\n")
cat("\nProjected goals allowed Year 1 (per season):", round(goals_y1, 1), "\n")
cat("Projected goals allowed Year 2 (per season):", round(goals_y2, 1), "\n")
cat("Difference (~additional goals allowed):", round(goals_difference, 1), "\n")

# ── Shortcut verification ────────────────────────────────────
cat("\nShortcut check (shots x SV% decline):", round(mean_shots * mean_decline, 1), "\n")

# ============================================================
# Wins vs Goal Differential Correlation — 2025-26 NHL Standings
# ============================================================

library(tidyverse)

# ── Clean out division header rows ───────────────────────────
standings_clean <- standings_202526 |>
  filter(!is.na(Wins))

# ── Correlation & R-squared ──────────────────────────────────
r_val <- cor(standings_clean$Wins, standings_clean$`Goal Differential`)
r_squared <- r_val^2

cat("Correlation (r):", round(r_val, 4), "\n")
cat("R-squared:", round(r_squared, 4), "\n")

# ── ggplot scatter with regression line ──────────────────────
ggplot(standings_clean, aes(x = `Goal Differential`, y = Wins)) +
  geom_point(color = "dodgerblue", size = 3) +
  geom_smooth(method = "lm", color = "red", se = TRUE) +
  annotate("text",
           x = min(standings_clean$`Goal Differential`) + 5,
           y = max(standings_clean$Wins) - 2,
           label = paste0("R² = ", round(r_squared, 3)),
           size = 5, color = "black", hjust = 0) +
  labs(
    title = "Wins vs. Goal Differential — 2025-26 NHL Season",
    x = "Goal Differential",
    y = "Wins"
  ) +
  theme_minimal(base_size = 13)