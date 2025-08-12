suppressPackageStartupMessages({
  library(sf)
  library(dplyr)
  library(classInt)
  library(tmap)
})

source("R/utils_io.R")
source("R/indicators_faenza.R")

raw_dir <- "data/raw"
interim_dir <- "data/interim"
out_tables <- "output/tables"
out_maps <- "output/maps"
ensure_dir(interim_dir); ensure_dir(out_tables); ensure_dir(out_maps)

geom_path <- file.path(raw_dir, "istat_2021_sezioni_geom.gpkg")
csv_path  <- file.path(raw_dir, "istat_2021_sezioni_faenza.csv")
imp_path  <- file.path(raw_dir, "copernicus_impervious_2018_faenza.tif")

# --- Step 01: Join geometry + census
if (file.exists(geom_path) && file.exists(csv_path)) {
  g <- read_sections_geom(geom_path)
  message(sprintf("Sections read: %s rows", nrow(g)))
  # Heuristic column guesses
  id_geom <- intersect(names(g), c("SEZ","SEZ_ID","SEZIONE","SEZIONI","ID_SEZ"))
  if (length(id_geom) == 0) stop("Could not find a section id column in geometry.")
  g <- dplyr::rename(g, SEZ_ID = !!rlang::sym(id_geom[1]))
  g$SEZ_ID <- as.character(g$SEZ_ID)

  census <- read_csv_safe(csv_path)
  id_c <- intersect(names(census), c("SEZ","SEZ_ID","SEZIONE","SEZIONI","ID_SEZ"))
  if (is.null(census) || length(id_c) == 0) stop("Could not find a section id column in census CSV.")
  census <- dplyr::rename(census, SEZ_ID = !!rlang::sym(id_c[1]))
  census$SEZ_ID <- as.character(census$SEZ_ID)

  g <- dplyr::left_join(g, census, by = "SEZ_ID")
  # Area in km2 (UTM 32N)
  g <- sf::st_transform(g, 32632)
  g$area_km2 <- as.numeric(sf::st_area(g)) / 1e6

  write_gpkg(g, file.path(interim_dir, "faenza_sections.gpkg"))
} else {
  message("Skipping Step 01: place geometry & census CSV in data/raw/")
}

# --- Step 02: Population density
p <- file.path(interim_dir, "faenza_sections.gpkg")
if (file.exists(p)) {
  g <- sf::st_read(p, quiet = TRUE)
  if (!"POP_TOT" %in% names(g)) stop("POP_TOT column not found; adjust your CSV column names.")
  g$pop_density <- g$POP_TOT / g$area_km2
  write_gpkg(g, file.path(interim_dir, "faenza_sections_density.gpkg"))
} else {
  message("Skipping Step 02: missing faenza_sections.gpkg")
}

# --- Step 03: Imperviousness
p <- file.path(interim_dir, "faenza_sections_density.gpkg")
if (file.exists(p) && file.exists(imp_path)) {
  library(terra); library(exactextractr); library(raster)
  g <- sf::st_read(p, quiet = TRUE)
  g3035 <- sf::st_transform(g, 3035)
  imp <- terra::rast(imp_path)
  imp <- terra::project(imp, "EPSG:3035")
  g$imperv_mean <- exactextractr::exact_extract(raster::raster(imp), g3035, 'mean')
  write_gpkg(g, file.path(interim_dir, "faenza_sections_density_imperv.gpkg"))
} else {
  message("Skipping Step 03: place copernicus_impervious_2018_faenza.tif in data/raw/")
}

# --- Step 04: Composite index
p <- file.path(interim_dir, "faenza_sections_density_imperv.gpkg")
if (file.exists(p)) {
  g <- sf::st_read(p, quiet = TRUE)
  g <- add_indicators(g)

  inds <- c("ind_over65","ind_under14","ind_unemployment","ind_low_edu","ind_foreign",
            "ind_single_parent","ind_elderly_alone","ind_vacancy","ind_density","ind_impervious")

  for (nm in inds) {
    g[[paste0(nm, "_n")]] <- minmax_norm(g[[nm]])
  }
  norm_cols <- paste0(inds, "_n")
  g$vuln_index <- rowMeans(as.data.frame(sf::st_drop_geometry(g[, norm_cols])), na.rm = TRUE)

  q <- classInt::classIntervals(g$vuln_index, n = 5, style = "quantile")
  g$vuln_class <- cut(g$vuln_index, breaks = q$brks, include.lowest = TRUE,
                      labels = c("Very Low","Low","Average","High","Very High"))

  # Save outputs
  write_gpkg(g, file.path("output/tables", "faenza_vulnerability.gpkg"))

  tmap::tmap_mode("plot")
  tm <- tmap::tm_shape(g) +
        tmap::tm_polygons("vuln_class", title = "Social Vulnerability (ISVEHI – Faenza)") +
        tmap::tm_layout(legend.outside = TRUE)
  tmap::tmap_save(tm, file.path("output/maps", "faenza_vulnerability_map.png"), width = 2000, height = 2200, dpi = 300)

  message("Pipeline finished. See output/maps & output/tables.")
} else {
  message("Skipping Step 04: missing faenza_sections_density_imperv.gpkg")
}