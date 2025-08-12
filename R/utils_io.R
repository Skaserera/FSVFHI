suppressPackageStartupMessages({
  library(sf)
  library(readr)
  library(dplyr)
})

ensure_dir <- function(path) {
  if (!dir.exists(path)) dir.create(path, recursive = TRUE, showWarnings = FALSE)
}

read_sections_geom <- function(path) {
  g <- sf::st_read(path, quiet = TRUE)
  g <- sf::st_make_valid(g)
  return(g)
}

read_csv_safe <- function(path) {
  if (!file.exists(path)) {
    message(sprintf("CSV not found: %s", path))
    return(NULL)
  }
  readr::read_csv(path, show_col_types = FALSE)
}

write_gpkg <- function(obj, path) {
  if (file.exists(path)) unlink(path)
  sf::st_write(obj, path, quiet = TRUE)
  message(sprintf("Wrote: %s", path))
}