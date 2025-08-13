suppressPackageStartupMessages({
  library(sf)
  library(dplyr)
  library(httr)
  library(xml2)
  library(rvest)
  library(osmdata)
  library(jsonlite)
})

ISTAT_FAENZA_CODE <- "039010"
ISTAT_PROV_RA     <- "039"

# 1) Get Faenza municipal boundary from OSM (no auth)
get_faenza_boundary_osm <- function() {
  message("Fetching Faenza municipal boundary from OSM…")
  # Give opq a numeric bbox to avoid bbox_to_string() error
  bb <- osmdata::getbb("Faenza, Ravenna, Emilia-Romagna, Italy")
  if (is.null(bb)) {
    # Fallback bbox (your Faenza study area)
    bb <- matrix(
      c(11.821, 44.270,  # xmin, ymin (SW)
        11.951, 44.355), # xmax, ymax (NE)
      nrow = 2, byrow = TRUE,
      dimnames = list(c("x","y"), c("min","max"))
    )
  }

  q <- osmdata::opq(bbox = bb, timeout = 200) %>%
    osmdata::add_osm_feature(key = "boundary", value = "administrative") %>%
    osmdata::add_osm_feature(key = "admin_level", value = "8") %>%
    osmdata::add_osm_feature(key = "name", value = "Faenza")

  dat <- osmdata::osmdata_sf(q)

  g <- NULL
  if (!is.null(dat$osm_multipolygons) && nrow(dat$osm_multipolygons) > 0) g <- dat$osm_multipolygons
  if (is.null(g) && !is.null(dat$osm_polygons) && nrow(dat$osm_polygons) > 0) g <- dat$osm_polygons
  if (is.null(g) || nrow(g) == 0) stop("Could not fetch Faenza boundary from OSM.")

  g <- g[1, c("name","geometry")]
  sf::st_make_valid(g) %>% sf::st_transform(32632)
}


# 2) Attempt to download ISTAT sections geometry for Emilia-Romagna (BT 2021)
#    We parse the official page to find region zip links.
download_istat_sections_geometry <- function(out_path) {
  if (file.exists(out_path)) {
    message("Sections geometry already present.")
    return(sf::st_read(out_path, quiet=TRUE))
  }
  message("Attempting to find ISTAT 'Basi Territoriali 2021' sections geometry link…")
  page <- tryCatch(read_html("https://www.istat.it/notizia/basi-territoriali-e-variabili-censuarie/"), error=function(e) NULL)
  if (is.null(page)) stop("Unable to open ISTAT BT page.")
  links <- html_elements(page, "a")
  hrefs <- html_attr(links, "href")
  # Heuristic: look for links containing 'Sezioni' and '2021' and 'zip'
  cand <- hrefs[grepl("Sezion", hrefs, ignore.case = TRUE) & grepl("2021", hrefs) & grepl("zip", hrefs)]
  if (length(cand) == 0) {
    stop("Could not find a direct geometry ZIP link on ISTAT page. Please place 'istat_2021_sezioni_geom.gpkg' in data/raw/.")
  }
  # Pick the first zip candidate
  zip_url <- cand[1]
  tmp_zip <- tempfile(fileext = ".zip")
  download.file(zip_url, tmp_zip, mode="wb", quiet=TRUE)
  unzip_dir <- tempfile("istat_geom_")
  dir.create(unzip_dir, showWarnings = FALSE)
  unzip(tmp_zip, exdir = unzip_dir)
  # Find a shapefile or gpkg inside
  shp <- list.files(unzip_dir, pattern="\\.shp$|\\.gpkg$", recursive = TRUE, full.names = TRUE)
  if (length(shp) == 0) stop("Geometry ZIP downloaded but no SHP/GPKG found.")
  g <- NULL
  for (p in shp) {
    # read quietly; keep first that works
    g <- tryCatch(sf::st_read(p, quiet=TRUE), error=function(e) NULL)
    if (!is.null(g)) break
  }
  if (is.null(g)) stop("Failed to read any geometry from ISTAT zip.")
  sf::st_write(g, out_path, quiet=TRUE)
  g
}

# 3) Attempt to download ISTAT 2021 section attributes (CSV by region)
download_istat_sections_attributes <- function(out_csv) {
  if (file.exists(out_csv)) {
    message("ISTAT attributes CSV already present.")
    return(readr::read_csv(out_csv, show_col_types = FALSE))
  }
  message("Attempting to find ISTAT 'Dati per sezioni di censimento 2021' CSV zip…")
  page <- tryCatch(read_html("https://www.istat.it/notizia/dati-per-sezioni-di-censimento/"), error=function(e) NULL)
  if (is.null(page)) stop("Unable to open ISTAT 'Dati per sezioni' page.")
  # Find a link that likely points to 'Sezioni di censimento per regione (zip)'
  links <- html_elements(page, "a")
  hrefs <- html_attr(links, "href")
  cand <- hrefs[grepl("sezioni", hrefs, ignore.case = TRUE) & grepl("zip", hrefs, ignore.case = TRUE)]
  if (length(cand) == 0) {
    stop("Could not find a direct CSV ZIP link on ISTAT page. Please place 'istat_2021_sezioni_faenza.csv' in data/raw/.")
  }
  zip_url <- cand[1]
  tmp_zip <- tempfile(fileext = ".zip")
  download.file(zip_url, tmp_zip, mode="wb", quiet=TRUE)
  unzip_dir <- tempfile("istat_attr_")
  dir.create(unzip_dir, showWarnings = FALSE)
  unzip(tmp_zip, exdir = unzip_dir)
  # Find CSV files
  csvs <- list.files(unzip_dir, pattern="\\.csv$", recursive = TRUE, full.names = TRUE)
  if (length(csvs) == 0) stop("Attributes ZIP downloaded but no CSVs found.")
  # Read all, bind rows; then filter to Faenza by municipal code or name
  # Note: this may be memory intensive; we try to read only relevant rows by grepping '039010'
  faenza_rows <- NULL
  for (csv in csvs) {
    # fast prefilter: if file doesn't contain the code, skip
    txt <- tryCatch(readLines(csv, n = 100), error=function(e) "")
    if (!any(grepl("039010", txt))) next
    df <- tryCatch(readr::read_csv(csv, show_col_types = FALSE), error=function(e) NULL)
    if (is.null(df)) next
    if (!"COD_COM" %in% names(df)) {
      # try to find a column that looks like municipal code
      cc <- names(df)[grepl("COM", names(df), ignore.case = TRUE)][1]
      if (!is.na(cc)) df$COD_COM <- df[[cc]]
    }
    if ("COD_COM" %in% names(df)) {
      df <- dplyr::filter(df, as.character(COD_COM) %in% c("039010", "39010"))
    } else {
      # fallback: try by Comune text column
      nm <- names(df)[grepl("COMUNE", names(df), ignore.case = TRUE)][1]
      if (!is.na(nm)) df <- dplyr::filter(df, grepl("FAENZA", toupper(.data[[nm]])))
    }
    if (nrow(df) > 0) {
      faenza_rows <- dplyr::bind_rows(faenza_rows, df)
    }
  }
  if (is.null(faenza_rows) || nrow(faenza_rows) == 0) {
    stop("Could not extract Faenza rows from ISTAT CSVs. Please place 'istat_2021_sezioni_faenza.csv' in data/raw/.")
  }
  readr::write_csv(faenza_rows, out_csv)
  faenza_rows
}

# 4) Compute imperviousness proxy from OSM building footprints
compute_osm_building_cover <- function(sections_sf, faenza_boundary_sf) {
  message("Fetching OSM buildings within Faenza boundary (as imperviousness proxy)…")

  stopifnot("SEZ_ID" %in% names(sections_sf))
  sections_sf <- sf::st_make_valid(sections_sf)
  faenza_boundary_sf <- sf::st_make_valid(faenza_boundary_sf)

  # Work in a projected CRS for area calcs
  sections_sf <- sf::st_transform(sections_sf, 32632)
  faenza_boundary_sf <- sf::st_transform(faenza_boundary_sf, 32632)

  # get a bbox in WGS84 for the OSM query
  bb <- sf::st_bbox(sf::st_transform(faenza_boundary_sf, 4326))

  # Query OSM buildings
  q <- osmdata::opq(bbox = c(bb["xmin"], bb["ymin"], bb["xmax"], bb["ymax"]), timeout = 200) |>
    osmdata::add_osm_feature(key = "building")
  dat <- osmdata::osmdata_sf(q)

  # Collect geometry ONLY (avoid rbind() column mismatch)
  geoms <- list()
  if (!is.null(dat$osm_polygons) && nrow(dat$osm_polygons) > 0) {
    geoms <- c(geoms, list(sf::st_geometry(dat$osm_polygons)))
  }
  if (!is.null(dat$osm_multipolygons) && nrow(dat$osm_multipolygons) > 0) {
    geoms <- c(geoms, list(sf::st_geometry(dat$osm_multipolygons)))
  }

  if (length(geoms) == 0) {
    warning("No OSM building footprints found; setting building_cover = 0.")
    sections_sf$building_cover <- 0
    return(sections_sf)
  }

  # Combine geometries and make an sf
  combined_sfc <- do.call(c, geoms)             # c() works on sfc to concatenate
  b <- sf::st_as_sf(data.frame(id = seq_along(combined_sfc)),
                    geometry = combined_sfc, crs = 4326)
  b <- sf::st_make_valid(b) |> sf::st_transform(32632)

  # Clip to Faenza boundary (fast guard using st_intersects)
  b <- suppressWarnings(sf::st_intersection(b, faenza_boundary_sf))

  if (nrow(b) == 0) {
    warning("No buildings after clipping; setting building_cover = 0.")
    sections_sf$building_cover <- 0
    return(sections_sf)
  }

  # Intersect buildings with sections, sum building area per section
  inter <- suppressWarnings(sf::st_intersection(sections_sf[, c("SEZ_ID")], b))
  if (nrow(inter) == 0) {
    sections_sf$building_cover <- 0
    return(sections_sf)
  }

  inter$area_build <- as.numeric(sf::st_area(inter))
  cover_by_sez <- inter |>
    dplyr::group_by(SEZ_ID) |>
    dplyr::summarise(area_build = sum(area_build, na.rm = TRUE), .groups = "drop")

  # Section area in m²
  sections_sf$area_km2 <- if (!"area_km2" %in% names(sections_sf)) {
    as.numeric(sf::st_area(sections_sf)) / 1e6
  } else sections_sf$area_km2
  sections_sf$area_m2 <- sections_sf$area_km2 * 1e6

  # Join and compute coverage 0..1
  sections_sf <- dplyr::left_join(sections_sf, cover_by_sez, by = "SEZ_ID")
  sections_sf$area_build[is.na(sections_sf$area_build)] <- 0
  sections_sf$building_cover <- pmin(1, sections_sf$area_build / sections_sf$area_m2)

  sections_sf$area_m2 <- NULL
  sections_sf$area_build <- NULL
  sections_sf
}
