# FSVFHI — Faenza Social Vulnerability to Flood Hazards Index
*(ISVEHI clone for Faenza; English only; same design & workflow)*

This repository builds a **social vulnerability index** for **Faenza** at **census-section** scale,
following the ISVEHI method (equal weights across indicators; normalized; mapped to 5 classes).

## How it works
- **Automatic downloads** (on first run / CI) attempt to fetch:
  - ISTAT 2021 **census-section geometry** for Emilia‑Romagna.
  - ISTAT 2021 **census-section socio‑economic** attributes.
  - **Imperviousness** proxy from OpenStreetMap building footprints (fallback used to avoid CLMS authentication).
- The pipeline computes indicators, combines them into a composite index,
  and exports:
  - `output/maps/faenza_vulnerability_map.png`
  - `output/tables/faenza_vulnerability.gpkg`

> Note: Copernicus HRL Imperviousness 2018 typically requires the CLMS API (EU Login token).
> We provide an OSM-based imperviousness proxy by default to keep the pipeline fully automatic.
> If you add CLMS credentials later, the script can switch to CLMS.

## Quick start (local)
```bash
conda env create -f environment.yml
conda activate fsvfhi
R -e "IRkernel::installspec(name='ir_fsvfhi', displayname='R (FSVFHI)')"
Rscript R/run_all.R
```
Outputs will appear under `output/` and be committed by CI when run on GitHub.

## Notebooks (optional, same steps)
- `notebooks/01_data_prep_faenza.ipynb`
- `notebooks/02_population_density.ipynb`
- `notebooks/03_impervious_surfaces.ipynb`
- `notebooks/04_combine_vulnerability.ipynb`

## Data sources (pinned URLs)
- **ISTAT — Dati per sezioni di censimento 2021 (attributes):**  
  https://www.istat.it/notizia/dati-per-sezioni-di-censimento/  
  *(Provides CSV by region with population, age, citizenship, education, work, families, dwellings, etc.)*

- **ISTAT — Basi Territoriali 2021 (geometry, Sezioni di censimento):**  
  https://www.istat.it/notizia/basi-territoriali-e-variabili-censuarie/  
  *(Shapefile/GeoPackage of census sections; downloadable by region / national mosaic)*

- **Copernicus Land Monitoring Service — HRL Imperviousness 2018:**  
  https://land.copernicus.eu/en/products/high-resolution-layer-imperviousness/imperviousness-density-2018  
  *(We use OSM buildings by default; you can switch to CLMS if you add an API token.)*

## GitHub Actions (auto-run & commit)
The workflow in `.github/workflows/ci.yml`:
- installs R deps
- runs `R/run_all.R` (which downloads data and builds outputs)
- commits changes under `output/` back to the repository

---

## Configuration
- Municipality: **Faenza** (ISTAT code `039010`).
- CRS used for area calculations: **EPSG:32632** (UTM 32N).
- Styling: green (low) → purple (high), 5 classes (quantiles), ISVEHI-like.

## Troubleshooting
- If ISTAT pages restructure their links (rare), the auto-downloader will print a clear message.
  In that case, place these files in `data/raw/` and re-run:
  - `istat_2021_sezioni_geom.gpkg` (sections geometry — Emilia‑Romagna subset is fine)
  - `istat_2021_sezioni_faenza.csv` (attributes filtered to Faenza sections)
- To switch imperviousness to **Copernicus HRL** later, follow the TODO in `R/downloaders.R`.

## License
MIT
