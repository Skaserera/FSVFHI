suppressPackageStartupMessages({
  library(dplyr)
  library(scales)
})

# Map raw ISTAT columns to indicators.
# TODO: Adjust the column names to your actual ISTAT CSV headers.
add_indicators <- function(g) {
  if (!all(c("POP_TOT") %in% names(g))) {
    stop("Expected POP_TOT in data; adjust field mapping in indicators_faenza.R")
  }
  # Safe helper
  safe_div <- function(num, den) ifelse(den > 0, num / den, NA_real_)

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
    ind_impervious    = imperv_mean / 100
  )
  return(g)
}

minmax_norm <- function(x) {
  rng <- range(x, na.rm = TRUE)
  if (diff(rng) == 0) return(rep(0.5, length(x)))
  (x - rng[1]) / (rng[2] - rng[1])
}