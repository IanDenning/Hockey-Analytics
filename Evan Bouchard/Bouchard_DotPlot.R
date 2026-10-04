library(tidyverse)
library(ggimage)

# ── 1. Define players ─────────────────────────────────────────────────────────
bouchard   <- "Bouchard, Evan"
elite_dmen <- c(
  "Makar, Cale", "Hughes, Quinn", "Hutson, Lane",
  "Werenski, Zach", "Dahlin, Rasmus", "Heiskanen, Miro", "McAvoy, Charlie"
)
highlighted <- c(bouchard, elite_dmen)

# ── 2. Headshot lookup table ──────────────────────────────────────────────────
headshots <- tibble(
  Player = highlighted,
  image  = c(
    "bouchard_headshot.png",
    "makar_headshot.png",
    "hughesquinn_headshot.png",
    "hutsonlane_headshot.png",
    "werenski_headshot.png",
    "dahlin_headshot.png",
    "heiskanen_headshot.png",
    "mcavoy_headshot.png"
  )
)

# ── 3. Tag groups and ranks ───────────────────────────────────────────────────
df_plot <- Dmen_zScores_Master %>%
  mutate(
    group = case_when(
      Player == bouchard      ~ "Bouchard",
      Player %in% elite_dmen ~ "Elite",
      TRUE                    ~ "Rest of League"
    ),
    rank = rank(-iTotal_Points_per60, ties.method = "first")
  ) %>%
  left_join(headshots, by = "Player") %>%
  arrange(iTotal_Points_per60)

# ── 3b. Separate overlapping players ─────────────────────────────────────────
df_plot <- df_plot %>%
  mutate(
    rank_adjusted = case_when(
      Player == "Bouchard, Evan" ~ rank + 5.5,
      Player == "Hughes, Quinn"  ~ rank - 6,
      TRUE                       ~ rank
    )
  )

# ── 4. Plot ───────────────────────────────────────────────────────────────────
ggplot(df_plot, aes(x = iTotal_Points_per60, y = rank_adjusted)) +
  
  # Rest of league — grey dots
  geom_point(
    data  = filter(df_plot, group == "Rest of League"),
    color = "dodgerblue", size = 2, alpha = 0.6
  ) +
  
  # Headshots for highlighted players
  geom_image(
    data = filter(df_plot, !is.na(image)),
    aes(x = iTotal_Points_per60, y = rank_adjusted, image = image),
    size = 0.06,
    asp  = 1.4
  ) +
  
  scale_x_continuous(
    expand = expansion(mult = c(0.05, 0.1)),
    breaks = c(0.5, 1, 1.5, 2, 2.5, 3)
  ) +
  scale_y_reverse() + 
  
  theme_minimal(base_size = 11) +
  theme(
    plot.background    = element_rect(fill = "#F4F4F0", color = NA),
    panel.background   = element_rect(fill = "#F4F4F0", color = NA),
    panel.grid.major   = element_line(color = "#DCDCDC"),
    panel.grid.minor   = element_blank(),
    plot.title         = element_text(face = "bold", size = 14),
    plot.subtitle      = element_text(size = 10, color = "grey40"),
    plot.caption       = element_text(color = "grey50"),
    legend.position    = "none"
  )+
  
  labs(
    title    = "Individual Points per 60 for NHL Defensemen (2023–26 Regular Season)",
    subtitle = "Bouchard vs. Elite Comps vs. Rest of League (dodgerblue)",
    x        = "iTotal Points per 60",
    y        = "Rank",
    caption  = "Data: Natural Stat Trick | Minimum GP threshold applied | Viz by Ian Denning (@Cest_Ian)"
  )

# Save Dot Plot 
ggsave(
  "Bouchard_DotPlot.png",
  width  = 10,
  height = 7,
  dpi    = 300,
  bg     = "#F4F4F0"
)

