library(tidyverse)
library(fmsb)
library(png)

# ── Shared setup ──────────────────────────────────────────────────────────────
bouchard <- "Bouchard, Evan"
elite_dmen <- c(
  "Makar, Cale", "Hughes, Quinn", "Hutson, Lane",
  "Werenski, Zach", "Dahlin, Rasmus", "Heiskanen, Miro", "McAvoy, Charlie"
)
highlighted <- c(bouchard, elite_dmen)

# ── Helper: build radar data ──────────────────────────────────────────────────
build_radar <- function(metrics, labels) {
  df <- Dmen_zScores_Master %>%
    filter(Player %in% highlighted) %>%
    select(Player, all_of(metrics)) %>%
    column_to_rownames("Player")
  
  colnames(df) <- labels
  
  rbind(
    rep(100, ncol(df)),
    rep(0,   ncol(df)),
    df
  )
}

# ── Helper: draw radar ────────────────────────────────────────────────────────
draw_radar <- function(df_radar, title, subtitle) {
  
  players     <- rownames(df_radar)[-(1:2)]
  is_bouchard <- players == "Bouchard, Evan"
  
  line_colors <- ifelse(is_bouchard, "#F5A623", "#555555")
  fill_colors <- ifelse(is_bouchard, "#F5A62350", "#55555508")
  line_widths <- ifelse(is_bouchard, 4, 1)
  line_types  <- ifelse(is_bouchard, 1, 2)  # solid for Bouchard, dashed for comps
  
  # Title with spacing
  par(mar = c(2, 2, 6, 2))
  
  radarchart(
    df_radar,
    axistype    = 1,
    pcol        = line_colors,
    pfcol       = fill_colors,
    plwd        = line_widths,
    plty        = line_types,
    cglcol      = "grey80",
    cglty       = 1,
    axislabcol  = "grey50",
    caxislabels = c("0", "25", "50", "75", "100"),
    vlcex       = 0.80
  )
  
  # ── Bouchard headshot ───────────────────────────────
  headshot <- png::readPNG("bouchard_headshot.png")
  rasterImage(headshot, xleft = -1.5, ybottom = 0.9, xright = -0.95, ytop = 1.45)
  
  # Title and subtitle with manual spacing
  title(main = title, line = 4, font.main = 2, cex.main = 1.3)
  mtext(subtitle, side = 3, line = 2.6, cex = 0.75, col = "grey40")
  
  # Simplified legend — just Bouchard vs. Elite Comps
  legend(
    x      = 0.7, y = 1.35,
    legend = c("Bouchard, Evan", "Elite Comps"),
    col    = c("#F5A623", "#555555"),
    lty    = c(1, 2),
    lwd    = c(4, 1),
    bty    = "n",
    cex    = 0.85
  )
}

# ── Chart 1 — Team Defense (On-Ice) ──────────────────────────────────────────
metrics_defense <- c(
  "CF_per60_percentile",
  "CA_per60_percentile",
  "SF_per60_percentile",
  "SA_per60_percentile",
  "xGF_per60_percentile",
  "xGA_per60_percentile",
  "HDCF_per60_percentile",
  "HDCA_per60_percentile",
  "OnIce_SV_pct_percentile",
  "PDO_percentile"
)

labels_defense <- c(
  "Corsi For/60",
  "*Corsi Against/60",
  "Shots For/60",
  "*Shots Against/60",
  "xGoals For/60",
  "*xGoals Against/60",
  "HD Chances For/60",
  "*HD Chances
  Against/60",
  "Oilers SV% On-Ice",
  "Puck Luck (PDO)"
)

df_defense <- build_radar(metrics_defense, labels_defense)

png("bouchard_radar_team_defense.png",
    width = 1000, height = 1000, res = 150, bg = "#F4F4F0")

draw_radar(
  df_defense,
  title    = "Oilers Team Defense 2023–26 Regular Season (Bouchard on Ice)",
  subtitle = " Starred (*) metrics: closer to center = better | Data: NaturalStatTrick | Viz by Ian Denning (@Cest_Ian)"
)

dev.off()

# ── Chart 2 — Zone Entries Against ───────────────────────────────────────────────────
metrics_breakout <- c(
  "Targets_per60_percentile",
  "Carries_Against_per60_percentile",
  "Denials_per60_percentile (Higher = Better)",
  "Entry_Passes_Allowed_per60_percentile",
  "Carries_wChance_Allowed_per60_percentile",
  "Dump_wChance_Allowed_per60_percentile",
  "Chances_Allowed_per60_percentile"
)

labels_breakout <- c(
  "Targets/60",
  "Carries Against/60",
  "Denials/60",
  "*Entry Passes Allowed/60",
  "*Carries w/Chance Allowed/60",
  "*Dump-ins w/Chance
  Allowed/60",
  "*Total Chances Allowed/60"
)

df_breakout <- build_radar(metrics_breakout, labels_breakout)

png("bouchard_radar_puck_breakout.png",
    width = 1000, height = 1000, res = 150, bg = "#F4F4F0")

draw_radar(
  df_breakout,
  title    = "Bouchard's Zone Entries Against (2023–26 Regular Season)",
  subtitle = "Starred (*) metrics: closer to center = better  | Data: All Three Zones (Corey Sznajder) | Viz by Ian Denning (@Cest_Ian)"
)

dev.off()

# ── Chart 3 — Zone Exits (Breakout Ability) ───────────────────────────────────
metrics_exits <- c(
  "DZ_PuckTouches_per60_percentile",
  "Retrievals_per60_percentile",
  "Botched_Retrievals_per60_percentile",
  "Retrievals_leading_toExits_per60_percentile",
  "Exits_per60_percentile",
  "Exits_wPossession_per60_percentile",
  "Clears_per60_percentile",
  "Failed_Exits_per60_percentile",
  "Passed_Exits_per60_percentile",
  "Carried_Exits_per60_percentile"
)

labels_exits <- c(
  "DZ Puck Touches/60",
  "Retrievals/60",
  "*Botched Retrievals/60",
  "Retrievals leading 
  to Exits/60              ",
  "Exits/60",
  "Exits w/ Possession/60",
  "Clears/60",
  "*Failed Exits/60",
  "Passing Exits/60",
  "Carried Exits/60"
)

df_exits <- build_radar(metrics_exits, labels_exits)

png("bouchard_radar_zone_exits.png",
    width = 1000, height = 1000, res = 150, bg = "#F4F4F0")

draw_radar(
  df_exits,
  title    = "Bouchard's D-Zone Exit Ability (2023–26 Regular Season)",
  subtitle = "*Starred metrics: closer to center = better | Data: All Three Zones (Corey Sznajder) | Viz by Ian Denning (@Cest_Ian)"
)

dev.off()