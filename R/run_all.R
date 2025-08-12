suppressPackageStartupMessages({
  library(sf); library(dplyr); library(readr); library(classInt); library(tmap)
})

source("R/utils_io.R")
source("R/downloaders.R")
source("R/indicators_faenza.R")

raw_dir <- "data/raw"
interim_dir <- "data/interim"
out_tables <- "output/tables"
out_maps <- "output/maps"

ensure_dir(interim_dir); ensure_dir(out_tables); ensure_dir(out_maps)

faenza_code <- "039010"

# --- Step 0: Get Faenza boundary from OSM (for clipping & OSM building coverage)
faenza_boundary <- get_faenza_boundary_osm()
sf::st_write(faenza_boundary, file.path(interim_dir, "faenza_boundary.gpkg"), delete_dsn = TRUE, quiet = TRUE)

# --- Step 1: Sections geometry (ISTAT BT 2021)
geom_path <- file.path(raw_dir, "istat_2021_sezioni_geom.gpkg")
g <- NULL
try({
  g <- download_istat_sections_geometry(geom_path)
}, silent = TRUE)

if (is.null(g)) {
  stop("Failed to download ISTAT sections geometry automatically.\nPlease put 'istat_2021_sezioni_geom.gpkg' under data/raw/ and re-run.")
}

# Try to detect the right ID columns, then filter to Faenza
# Common column candidates: 'SEZ', 'SEZ_ID', 'SEZIONE', 'SEZIONI', 'ID_SEZ', 'SEZ2011'
id_candidates <- intersect(names(g), c("SEZ","SEZ_ID","SEZIONE","SEZIONI","ID_SEZ","SEZ2011","ID_SEZIONE"))
if (length(id_candidates) == 0) {
  # create a synthetic ID if none exists
  g$SEZ_ID <- as.character(seq_len(nrow(g)))
} else {
  g <- dplyr::rename(g, SEZ_ID = !!rlang::sym(id_candidates[1]))
}
g$SEZ_ID <- as.character(g$SEZ_ID)

# Detect municipality column
muni_col <- intersect(names(g), c("COD_COM","COMUNE","DEN_COM","CODICE_COMUNE"))
if (length(muni_col) > 0) {
  col <- muni_col[1]
  if (col == "COD_COM" || grepl("COD", col)) {
    g_faenza <- g %>% dplyr::filter(as.character(.data[[col]]) %in% c("039010","39010"))
  } else {
    g_faenza <- g %>% dplyr::filter(grepl("FAENZA", toupper(.data[[col]])))
  }
} else {
  # spatial clip to boundary if municipal attribute missing
  message("Municipality attribute not found; clipping by Faenza boundary…")
  g <- sf::st_make_valid(g) %>% sf::st_transform(32632)
  g_faenza <- suppressWarnings(sf::st_intersection(g, faenza_boundary))
}

if (nrow(g_faenza) == 0) stop("No sections found for Faenza after filtering/clipping.")
g_faenza <- sf::st_make_valid(g_faenza) %>% sf::st_transform(32632)
g_faenza$area_km2 <- as.numeric(sf::st_area(g_faenza)) / 1e6
sf::st_write(g_faenza, file.path(interim_dir, "faenza_sections_geom.gpkg"), delete_dsn = TRUE, quiet = TRUE)

# --- Step 2: ISTAT section attributes for Faenza
attr_csv <- file.path(raw_dir, "istat_2021_sezioni_faenza.csv")
attrs <- NULL
try({
  attrs <- download_istat_sections_attributes(attr_csv)
}, silent = TRUE)

if (is.null(attrs)) {
  stop("Failed to fetch ISTAT attributes automatically.\nPlease place 'istat_2021_sezioni_faenza.csv' under data/raw/ and re-run.")
}

# Normalise join key
if (!"SEZ_ID" %in% names(attrs)) {
  # guess id column
  id_cand <- intersect(names(attrs), c("SEZ","SEZ_ID","SEZIONE","SEZIONI","ID_SEZ"))
  if (length(id_cand) == 0) stop("ISTAT CSV missing a section ID column. Update 'R/run_all.R' join logic.")
  attrs <- dplyr::rename(attrs, SEZ_ID = !!rlang::sym(id_cand[1]))
}
attrs$SEZ_ID <- as.character(attrs$SEZ_ID)

# --- Step 3: Join geometry + attributes
g_join <- g_faenza %>% dplyr::left_join(attrs, by = "SEZ_ID")
if (!"POP_TOT" %in% names(g_join)) {
  stop("Joined data has no POP_TOT column. Map your ISTAT CSV columns to expected names or edit indicators_faenza.R")
}
g_join$pop_density <- g_join$POP_TOT / g_join$area_km2
sf::st_write(g_join, file.path(interim_dir, "faenza_sections_joined.gpkg"), delete_dsn = TRUE, quiet = TRUE)

# --- Step 4: Imperviousness proxy via OSM buildings
g_cov <- compute_osm_building_cover(g_join, faenza_boundary)
sf::st_write(g_cov, file.path(interim_dir, "faenza_sections_with_building_cover.gpkg"), delete_dsn = TRUE, quiet = TRUE)

# --- Step 5: Indicators & composite index
g_cov <- add_indicators(g_cov)

inds <- c("ind_over65","ind_under14","ind_unemployment","ind_low_edu","ind_foreign",
          "ind_single_parent","ind_elderly_alone","ind_vacancy","ind_density","ind_impervious")

for (nm in inds) {
  g_cov[[paste0(nm, "_n")]] <- minmax_norm(g_cov[[nm]])
}
norm_cols <- paste0(inds, "_n")
g_cov$vuln_index <- rowMeans(as.data.frame(sf::st_drop_geometry(g_cov[, norm_cols])), na.rm = TRUE)

q <- classInt::classIntervals(g_cov$vuln_index, n=5, style="quantile")
g_cov$vuln_class <- cut(g_cov$vuln_index, breaks=q$brks, include.lowest=TRUE,
                        labels=c("Very Low","Low","Average","High","Very High"))

# --- Step 6: Export data & map
write_gpkg(g_cov, file.path(out_tables, "faenza_vulnerability.gpkg"))

tmap::tmap_mode("plot")
tm <- tmap::tm_shape(g_cov) +
      tmap::tm_polygons("vuln_class", title="FSVFHI: Social Vulnerability (Faenza)") +
      tmap::tm_layout(legend.outside=TRUE, title="Faenza Social Vulnerability to Flood Hazards Index")
tmap::tmap_save(tm, file.path(out_maps, "faenza_vulnerability_map.png"), width=2000, height=2200, dpi=300)

message("Pipeline finished. See output/maps & output/tables.")
