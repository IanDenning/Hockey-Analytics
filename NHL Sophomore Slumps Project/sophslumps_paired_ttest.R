library(tidyverse)

# ── 1. Recalculate Diff cleanly ──────────────────────────────
soph_slumps_skaters$Diff <- soph_slumps_skaters$PPG_Y2 - soph_slumps_skaters$PPG_Y1


# ── 2. Check Normality Assumption ────────────────────────────
hist(soph_slumps_skaters$Diff,
     main = "Distribution of PPG Differences for Skaters (Y2 - Y1)",
     xlab = "PPG Difference (Sophomore Season - Rookie Season)",
     col = "dodgerblue")

qqnorm(soph_slumps_skaters$Diff, main = "QQ Plot of PPG Differences for Skaters")
qqline(soph_slumps_skaters$Diff, col = "red")

shapiro.test(soph_slumps_skaters$Diff)

# ── 3. State Hypotheses ──────────────────────────────────────
# H0: μ_d = 0  (no difference in PPG between rookie and sophomore year)
# HA: μ_d ≠ 0  (there is a difference — two-sided)


# ── 4. Run the Paired t-Test ─────────────────────────────────
t.test(soph_slumps_skaters$PPG_Y2, soph_slumps_skaters$PPG_Y1, paired = TRUE)


# ── 5. Descriptive Stats for Write-Up ────────────────────────
mean(soph_slumps_skaters$PPG_Y1)   # mean rookie PPG
mean(soph_slumps_skaters$PPG_Y2)   # mean sophomore PPG
mean(soph_slumps_skaters$Diff)     # mean difference
sd(soph_slumps_skaters$Diff)       # SD of differences


