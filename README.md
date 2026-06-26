# G² Bridge 🌍

[![License: GPL v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
[![GBIF Ebbe Nielsen Challenge 2026](https://img.shields.io/badge/GBIF-Ebbe%20Nielsen%202026-green)](https://www.gbif.org/article/1G82GL7jw08kS0g6k6MuSa)
[![R Shiny](https://img.shields.io/badge/Built%20with-R%20Shiny-blue)](https://shiny.posit.co/)

> Bridging GBIF and GenBank for automated occurrence integration,
> Darwin Core quality assessment, and IUCN conservation metrics.

G² Bridge is an open-source R Shiny tool that closes the interoperability
gap between GBIF and GenBank. It retrieves, reconciles, and spatially fuses
occurrence records from both repositories, evaluates Darwin Core metadata
completeness, calculates EOO and AOO following IUCN Criterion B, and exports
IPT-ready Darwin Core Archives — all from a single species name input.

Built for the **2026 GBIF Ebbe Nielsen Challenge** by
**Fernando J. Castro & Eliana Latacunga**
Integrative Biology Laboratory · Ecuador.

---

## Requirements

- R ≥ 4.3 and RStudio
- Internet connection (the app queries GBIF and NCBI APIs in real time)

## Installation

```r
install.packages(c(
  "shiny", "bslib", "shinyjs", "shinycssloaders",
  "leaflet", "leaflet.extras2", "DT", "sf", "terra",
  "dplyr", "tidyr", "ggplot2", "GeoRange", "httr2",
  "jsonlite", "zip", "base64enc", "htmlwidgets",
  "rentrez", "stringr"
))
```

## How to run

1. Clone or download this repository
2. Open `UI_EN.R` in RStudio
3. Set your working directory to the project folder
4. Run `shinyApp(ui, server)` or click **Run App** in RStudio

## Modules

| Module | Description |
|--------|-------------|
| Taxonomic reconciliation | GBIF Species Match API with fuzzy matching |
| Spatial fusion | Equal-area deduplication at configurable threshold |
| Darwin Core quality | Weighted completeness score across 7 fields |
| IUCN assessment | EOO and AOO with automatic source comparison |

## Outputs

- Merged occurrence dataset `.csv`
- EOO convex hull shapefile `.zip`
- AOO 2 km grid shapefile `.zip`
- Darwin Core Archive for GBIF IPT `.zip`
- Interactive HTML report with GBIF citation DOI

## License

GPL-3.0 — see [LICENSE](LICENSE) for details.
