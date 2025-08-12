suppressPackageStartupMessages({
  library(sf)
  library(dplyr)
  library(readr)
  library(classInt)
  library(tmap)
})

source("R/utils_io.R")
source("R/downloaders.R")        # used for OSM buildings
source("R/indicators_faenza.R")  # indicator formulas (auto-picks what’s available)

raw_dir     <- "data/raw"
interim_dir <- "data/interim"
out_tables  <- "output/tables"
out_maps    <- "output/maps"

ensure_dir(interim_dir); ensure_dir(out_tables); ensure_dir(out_maps)

# -------------------------------
# STEP 1: Read sections (Faenza)
# -------------------------------
geom_path <- file.path(raw_dir, "istat_2021_sezioni_geom.gpkg")
if (!file.exists(geom_path)) stop("Missing geometry: data/raw/istat_2021_sezioni_geom.gpkg")

g <- st_read(geom_path, quiet = TRUE) |> st_make_valid()

# Standardise section id (your file has SEZ21_ID)
if ("SEZ21_ID" %in% names(g)) {
  names(g)[names(g) == "SEZ21_ID"] <- "SEZ_ID"
} else if ("SEZ" %in% names(g)) {
  names(g)[names(g) == "SEZ"] <- "SEZ_ID"
} else if ("SEZIONE" %in% names(g)) {
  names(g)[names(g) == "SEZIONE"] <- "SEZ_ID"
} else if ("ID_SEZ" %in% names(g)) {
  names(g)[names(g) == "ID_SEZ"] <- "SEZ_ID"
} else {
  stop("Could not find a section ID column (expected SEZ21_ID / SEZ / SEZIONE / ID_SEZ).")
}
g$SEZ_ID <- as.character(g$SEZ_ID)

# If it isn’t Faenza-only, filter by PRO_COM or COD_COM (your file already is)
if ("PRO_COM" %in% names(g)) {
  gf <- dplyr::filter(g, as.character(PRO_COM) %in% c("39010", 39010))
  if (nrow(gf) == 0) gf <- g
} else if ("COD_COM" %in% names(g)) {
  gf <- dplyr::filter(g, as.character(COD_COM) %in% c("039010","39010"))
  if (nrow(gf) == 0) gf <- g
} else {
  gf <- g
}

gf <- st_make_valid(gf) |> st_transform(32632)
gf$area_km2 <- as.numeric(st_area(gf)) / 1e6

# Map your built-in columns to the names used by indicators
# POP21 (population), FAM21 (families/households), ABI21 (dwellings), EDI21 (buildings)
if ("POP21" %in% names(gf)) gf$POP_TOT <- gf$POP21
if ("FAM21" %in% names(gf)) gf$HH_TOT  <- gf$FAM21
if ("ABI21" %in% names(gf)) gf$DWELLINGS_TOTAL  <- gf$ABI21
if ("EDI21" %in% names(gf)) gf$BUILDINGS_TOTAL  <- gf$EDI21

gf$pop_density <- if ("POP_TOT" %in% names(gf)) gf$POP_TOT / gf$area_km2 else NA_real_

st_write(gf, file.path(interim_dir, "faenza_sections_geom.gpkg"), delete_dsn = TRUE, quiet = TRUE)

# ----------------------------------------------------
# STEP 1b: Derive Faenza boundary from the sections
# ----------------------------------------------------
faenza_boundary <- st_as_sf(
  data.frame(name = "Faenza"),
  geometry = st_sfc(st_union(st_geometry(gf)))
)
faenza_boundary <- st_set_crs(faenza_boundary, st_crs(gf))
st_write(faenza_boundary, file.path(interim_dir, "faenza_boundary.gpkg"), delete_dsn = TRUE, quiet = TRUE)

# -------------------------------------------------
# STEP 2: Imperviousness proxy via OSM buildings
# -------------------------------------------------
g_cov <- compute_osm_building_cover(gf, faenza_boundary)
st_write(g_cov, file.path(interim_dir, "faenza_sections_with_building_cover.gpkg"), delete_dsn = TRUE, quiet = TRUE)

# -----------------------------------
# STEP 3: Indicators & composite index
# (auto-picks only indicators available in your data)
# -----------------------------------
g_cov <- add_indicators(g_cov)

# use any column starting with "ind_" that varies
cand <- grep("^ind_", names(g_cov), value = TRUE)
valid_inds <- Filter(function(nm){
  v <- g_cov[[nm]]
  v <- v[is.finite(v)]
  length(v) >= 2 && !all(is.na(v)) && (max(v, na.rm=TRUE) > min(v, na.rm=TRUE))
}, cand)
if (length(valid_inds) < 2) stop("Not enough indicators available. Add the ISTAT attributes CSV later to expand the set.")

for (nm in valid_inds) {
  g_cov[[paste0(nm, "_n")]] <- minmax_norm(g_cov[[nm]])
}
norm_cols <- paste0(valid_inds, "_n")
g_cov$vuln_index <- rowMeans(as.data.frame(st_drop_geometry(g_cov[, norm_cols])), na.rm = TRUE)

q <- classInt::classIntervals(g_cov$vuln_index, n = 5, style = "quantile")
g_cov$vuln_class <- cut(g_cov$vuln_index, breaks = q$brks, include.lowest = TRUE,
                        labels = c("Very Low","Low","Average","High","Very High"))

# ---------------------------
# STEP 4: Export data & map
# ---------------------------
write_gpkg(g_cov, file.path(out_tables, "faenza_vulnerability.gpkg"))

tmap::tmap_mode("plot")
tm <- tmap::tm_shape(g_cov) +
      tmap::tm_polygons("vuln_class", title = "FSVFHI: Social Vulnerability (Faenza)") +
      tmap::tm_layout(legend.outside = TRUE, title = "Faenza Social Vulnerability to Flood Hazards Index")
tmap::tmap_save(tm, file.path(out_maps, "faenza_vulnerability_map.png"), width = 2000, height = 2200, dpi = 300)

message("Pipeline finished. See output/maps & output/tables.")
