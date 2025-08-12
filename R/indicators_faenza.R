suppressPackageStartupMessages({
  library(dplyr)
  library(scales)
})

# Normalize helper
minmax_norm <- function(x) {
  rng <- range(x, na.rm = TRUE)
  if (!is.finite(diff(rng)) || diff(rng) == 0) return(rep(0.5, length(x)))
  (x - rng[1]) / (rng[2] - rng[1])
}

add_indicators <- function(g) {
  safe_div <- function(a,b) ifelse(is.finite(a) & is.finite(b) & b > 0, a/b, NA_real_)

  # Always try these two:
  if ("pop_density" %in% names(g))
    g$ind_density <- rescale(g$pop_density, to = c(0,1), from = range(g$pop_density, na.rm = TRUE))
  if ("building_cover" %in% names(g))
    g$ind_impervious <- g$building_cover  # already 0..1 from OSM coverage

  # Derive a few more from the columns you have
  if (all(c("POP_TOT","HH_TOT") %in% names(g)))
    g$ind_persons_per_hh <- safe_div(g$POP_TOT, g$HH_TOT)

  if (all(c("HH_TOT","DWELLINGS_TOTAL") %in% names(g)))
    g$ind_hh_per_dwelling <- safe_div(g$HH_TOT, g$DWELLINGS_TOTAL)

  if (all(c("BUILDINGS_TOTAL","area_km2") %in% names(g)))
    g$ind_buildings_per_km2 <- safe_div(g$BUILDINGS_TOTAL, g$area_km2)

  # Optional placeholder indicators (will be NA until we add the CSV later)
  # g$ind_over65        <- safe_div(g$POP_65PLUS, g$POP_TOT)
  # g$ind_under14       <- safe_div(g$POP_0_14, g$POP_TOT)
  # g$ind_unemployment  <- safe_div(g$UNEMPLOYED, g$LABOUR_FORCE)
  # g$ind_low_edu       <- safe_div(g$ADULTS_LOW_EDU, g$ADULTS_TOT)
  # g$ind_foreign       <- safe_div(g$NON_ITALIAN, g$POP_TOT)
  # g$ind_elderly_alone <- safe_div(g$HH_ELDERLY_ALONE, g$HH_TOT)
  # g$ind_vacancy       <- ifelse(g$DWELLINGS_TOTAL > 0,
  #                               (g$DWELLINGS_TOTAL - g$DWELLINGS_OCCUPIED) / g$DWELLINGS_TOTAL,
  #                               NA_real_)

  return(g)
}
