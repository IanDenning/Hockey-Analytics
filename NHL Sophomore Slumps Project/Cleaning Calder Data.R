# ============================================================
# Sophomore Slump Study — Calder Ballot Cleaning Script
# Goal: Keep the top 5 skaters per season by votes received.
#
# Note: Goalies have already been removed from the OG sheet.
# Rather than filtering Place <= 5 (which gives variable counts
# per season where goalies held top slots), we rank skaters
# within each season by Votes descending and take the top 5.
# This gives exactly 5 skaters per season = 100 total.
# ============================================================

library(tidyverse)


# ── 1. Data is already loaded as "skaters" ───────────────────
glimpse(skaters)


# ── 2. Filter to top 5 skaters per season ────────────────────
top5 <- skaters |>
  group_by(Year) |>
  slice_max(order_by = Votes, n = 5, with_ties = FALSE) |>
  arrange(Year, desc(Votes)) |>
  ungroup()


# ── 3. Sanity check ──────────────────────────────────────────
cat("\nPlayers per season:\n")
top5 |> count(Year) |> print(n = Inf)

cat("\nTotal players:", nrow(top5), "\n")
cat("Seasons covered:", n_distinct(top5$Year), "\n")


# ── 4. Add placeholder columns for data entry ────────────────
top5 <- top5 |>
  mutate(
    GP_Y1  = NA_real_,   # Games played — rookie season
    PPG_Y1 = NA_real_,   # Points per game — rookie season
    GP_Y2  = NA_real_,   # Games played — sophomore season
    PPG_Y2 = NA_real_,   # Points per game — sophomore season
    Diff   = NA_real_    # PPG_Y2 - PPG_Y1
  )


# ── 5. Clean column order ────────────────────────────────────
top5 <- top5 |>
  select(Year, Place, Player, Age, Team, Pos,
         Votes, Vote_pct,
         G, A, PTS,
         GP_Y1, PPG_Y1,
         GP_Y2, PPG_Y2,
         Diff)


# ── 6. Save ──────────────────────────────────────────────────
write_csv(top5, "Calder_Top5_Skaters_Clean.csv", na = "")

cat("\nSaved: Calder_Top5_Skaters_Clean.csv\n")


# ============================================================
# Goalie Data Cleaning
# Goal: Slim down to essential columns + SV% for analysis
# ============================================================

# ── 1. Check column names ────────────────────────────────────
colnames(og_goalies)

# ── 2. Select relevant columns and rename ────────────────────
goalies_clean <- og_goalies |>
  select(Year, Place, Player, Age, Team, Pos, Vote_pct, SV_pct) |>
  rename(SV_pct_Y1 = SV_pct) |>
  mutate(SV_pct_Y2 = NA_real_)

# ── 3. Sanity check ──────────────────────────────────────────
glimpse(goalies_clean)

cat("\nGoalies per season:\n")
goalies_clean |> count(Year) |> print(n = Inf)

cat("\nTotal goalies:", nrow(goalies_clean), "\n")

# ── 4. Save ──────────────────────────────────────────────────
write_csv(goalies_clean, "Calder_Goalies_Clean.csv", na = "")

cat("\nSaved: Calder_Goalies_Clean.csv\n")
