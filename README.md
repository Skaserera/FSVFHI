# ISVEHI–Faenza (Flood Social Vulnerability – ISVEHI clone)

This repository clones the **ISVEHI** approach for **Faenza (IT)**, focusing on flood-related social vulnerability.  
**Design and function are kept identical** to the original ISVEHI: an equal‑weight composite of ~22 indicators, normalized and classified for mapping.

## Quick start

### 1) Create the R environment
```bash
conda env create -f environment.yml
conda activate isvehi-faenza
R -e "IRkernel::installspec(name='ir_isvehi_faenza', displayname='R (isvehi-faenza)')"
```

### 2) Data
Place the following in `data/raw/` (file names can differ; adjust in notebooks):
- `istat_2021_sezioni_geom.gpkg` — ISTAT census sections geometry for Faenza.
- `istat_2021_sezioni_faenza.csv` — Section‑level attributes (population, age bands, families, etc.).
- `copernicus_impervious_2018_faenza.tif` — Copernicus HRL Imperviousness (tile covering Faenza).

### 3) Run the pipeline
You can run the notebooks in order using the **R (isvehi-faenza)** kernel:

1. `notebooks/01_data_prep_faenza.ipynb`
2. `notebooks/02_population_density.ipynb`
3. `notebooks/03_impervious_surfaces.ipynb`
4. `notebooks/04_combine_vulnerability.ipynb`

Outputs are written to:
- `output/tables/faenza_vulnerability.gpkg`
- `output/maps/faenza_vulnerability_map.png`

### Headless run
Alternatively:
```bash
Rscript R/run_all.R
```

This will execute the same steps (skipping any stage where required input files are missing).

## Verify the repo after cloning from GitHub
1. **Environment builds** without errors using `conda env create -f environment.yml`.
2. **R kernel** appears in Jupyter: `R (isvehi-faenza)`.
3. **Notebook 01** runs and writes `data/interim/faenza_sections.gpkg`.
4. **Notebook 04** finishes and creates `output/maps/faenza_vulnerability_map.png`.
5. The map shows a reasonable spatial spread of classes (Very Low → Very High).

## Notes
- Indicators in `R/indicators_faenza.R` map to ISTAT fields; adjust field names to your CSV schema.
- For now, the project is **English only** (translation can be added later).
- No changes in design or weighting vs original ISVEHI.

## License
MIT