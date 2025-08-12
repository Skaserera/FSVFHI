suppressPackageStartupMessages({
  library(sf)
  library(readr)
  library(dplyr)
  library(httr)
  library(xml2)
  library(rvest)
})

ensure_dir <- function(path) {
  if (!dir.exists(path)) dir.create(path, recursive = TRUE, showWarnings = FALSE)
}

write_gpkg <- function(obj, path) {
  if (file.exists(path)) unlink(path)
  sf::st_write(obj, path, quiet = TRUE)
  message(sprintf("Wrote: %s", path))
}

# Simple helper to check HTTP and download
download_if_missing <- function(url, dest) {
  if (file.exists(dest)) {
    message(sprintf("Already present: %s", dest))
    return(dest)
  }
  message(sprintf("Downloading: %s", url))
  resp <- tryCatch(httr::GET(url, timeout(120)), error=function(e) NULL)
  if (is.null(resp) || httr::http_error(resp)) {
    stop(sprintf("Failed to download from %s", url))
  }
  bin <- httr::content(resp, "raw")
  writeBin(bin, dest)
  return(dest)
}
