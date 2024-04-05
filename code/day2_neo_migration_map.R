pacman::p_load(
  collapse, ggplot2, ggfx, dplyr, scales, glue, ggthemes,
  paletteer, tidycensus, sf, showtext, ggtext
)

# add font ---------------------------------------------------------------------
font_add_google(name = "Staatliches", family = "Staatliches")
font_t <- "Staatliches"

font_add_google(name = "Work Sans", family = "Work Sans")
font_ws <- "Work Sans"

showtext_auto()
showtext_opts(dpi = 320)

options(scipen = 999)

# Data processing --------------------------------------------------------------

county_sf <- tigris::counties(year = 2022, cb = TRUE) |>
  tigris::shift_geometry() |>
  filter(substr(GEOID, 1, 2) < 60)

us <- st_union(county_sf) |> st_boundary()

county_pop_raw <- get_acs(
  geography = "county",
  variables = "B01003_001",
  year = 2022
)

county_pop  <- left_join(county_sf, county_pop_raw, by = "GEOID") |>
  mutate(
    est_thou = round(estimate / 1000),
    # cut ot quantiles
    est_cut = as.numeric(cut(
      est_thou, quantile(est_thou, probs = seq(0, 1, 0.2), include.lowest = TRUE)
    )))


net_migration <- get_estimates(
  geography = "county", variables = "NETMIG", vintage = 2023
)

mig_sf <- county_sf |>
  st_centroid() |>
  join(net_migration, on = "GEOID", drop = TRUE) |>
  fmutate(dir = ifelse(value < 0, "neg", "pos"), abs = abs(value)) |>
  roworder(-abs) |>
  rsplit(~ dir) |>
  lapply(slice_head, n = 75)

# Draw Map ---------------------------------------------------------------------
bg_col <- "#01070d"
teal <- "#06f0da"
red <- "#ff1f51"

poly_scale <- colorRampPalette(c(bg_col, teal), bias = 0.3)


p <- ggplot(county_pop) +
  with_outer_glow(geom_sf(data = us, color = bg_col), colour = red, expand = 3, sigma = 10) +
  geom_sf(aes(fill = est_cut), alpha = 0.8) +
  with_outer_glow(
    geom_sf(data = mig_sf$neg, aes(size = abs), color = "white"),
    colour = red, sigma = 5, expand = 10
  ) +
  with_outer_glow(
    geom_sf(data = mig_sf$pos, aes(size = abs), color = "white"),
    colour = teal, sigma = 5, expand = 10
  ) +
  scale_fill_gradientn(
    colors = poly_scale(5), na.value = bg_col
  ) +
  scale_size_binned(range = c(0.1, 3), n.breaks = 4) +
  ggthemes::theme_map() +
  labs(
    title = "Net Migration in 2023",
    subtitle = glue(
      "Where are people moving <span style='color:{teal}'>to</span> ",
      "and <span style='color:{red}'>from</span>?"
    ),
    caption = "@arthurgailes"
  ) +
  theme(
    plot.background = element_rect(fill = bg_col),
    panel.background = element_rect(fill = bg_col),
    legend.position = "none",
    plot.title = element_text(size = 20, color = "white", family = font_t),
    plot.subtitle = element_markdown(size = 12, color = "white", family = font_ws),
    plot.caption = element_text(size = 10, color = "white", family = font_ws)
  )

ggsave(
  "chart/202404_migration.png",
  width = 8, height = 5, dpi = 300
)