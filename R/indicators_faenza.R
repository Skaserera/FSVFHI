suppressPackageStartupMessages({
  library(dplyr)
  library(scales)
})

# Map ISTAT columns to indicators.
# Adjust column names below to the actual headers in the ISTAT CSV for sections.
add_indicators <- function(g) {
  # available columns should include at least POP_TOT
  if (!"POP_TOT" %in% names(g)) {
    stop("Expected POP_TOT in joined data. Please map your ISTAT CSV columns in indicators_faenza.R")
  }
  safe_div <- function(a,b) ifelse(b > 0, a/b, NA_real_)
  g <- g %>% mutate(
    ind_over65        = safe_div(POP_65PLUS, POP_TOT),
    ind_under14       = safe_div(POP_0_14, POP_TOT),
    ind_unemployment  = safe_div(UNEMPLOYED, LABOUR_FORCE),
    ind_low_edu       = safe_div(ADULTS_LOW_EDU, ADULTS_TOT),
    ind_foreign       = safe_div(NON_ITALIAN, POP_TOT),
    ind_single_parent = safe_div(FAM_SINGLE_PARENT, FAM_TOT),
    ind_elderly_alone = safe_div(HH_ELDERLY_ALONE, HH_TOT),
    ind_vacancy       = ifelse(DWELLINGS_TOTAL > 0,
                               (DWELLINGS_TOTAL - DWELLINGS_OCCUPIED) / DWELLINGS_TOTAL,
                               NA_real_),
    ind_density       = rescale(pop_density, to = c(0,1), from = range(pop_density, na.rm = TRUE)),
    ind_impervious    = building_cover  # OSM buildings fraction in section [0..1]
  )
  return(g)
}

minmax_norm <- function(x) {
  rng <- range(x, na.rm = TRUE)
  if (!is.finite(diff(rng)) || diff(rng) == 0) return(rep(0.5, length(x)))
  (x - rng[1]) / (rng[2] - rng[1])
}
