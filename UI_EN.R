#  G² Bridge— UI
#  GBIF Ebbe Nielsen Challenge 2026
#  Authors: Fernando J. Castro & Eliana Latacunga
#
getwd()
setwd("D:/1_EBBEN/R")

library(shiny)
library(bslib)
library(shinyjs)
library(shinycssloaders)
library(leaflet)
library(leaflet.extras2)
addResourcePath("img", "www")
# ── Layer colors ───────────────────────────────────────────────────────────
COL_GENBANK  <- "#E07B39"   # orange → points only in GenBank
COL_GBIF     <- "#2980b9"   # blue   → points only in GBIF
COL_AMBOS    <- "#27ae60"   # green  → points in both sources
COL_EOO      <- "#e84040"   # red    → EOO convex hull
COL_AOO      <- "#f59a23"   # yellow → AOO grid

# ── UI Palette ─────────────────────────────────────────────────────────────────
BRAND_DARK   <- "#39160C"
BRAND_MID    <- "#E28D54"
BRAND_LIGHT  <- "#EEAA80"

# CSS
#
CSS <- "
/* ── Base ── */
body {
  background: linear-gradient(135deg, #484545 0%, #535354 100%);
  min-height: 100vh;
  font-family: 'Inter', sans-serif;
}

/* ── Main Header ── */
.main-header {
  background: rgba(255,255,255,0.95);
  backdrop-filter: blur(10px);
  border-radius: 20px;
  padding: 24px 30px;
  margin-bottom: 24px;
  box-shadow: 0 10px 30px rgba(0,0,0,0.12);
  display: flex;
  align-items: center;
  gap: 20px;
}
.main-title {
  font-family: 'Poppins', sans-serif;
  font-weight: 700;
  font-size: 2.1rem;
  background: linear-gradient(135deg, #3b3c3b, #535354);
  -webkit-background-clip: text;
  -webkit-text-fill-color: transparent;
  background-clip: text;
  margin-bottom: 4px;
}
.main-subtitle {
  font-size: 0.95rem;
  color: #6c757d;
  margin-bottom: 2px;
}
.gbif-badge {
  display: inline-block;
  background: linear-gradient(135deg, #4CAF50, #2e7d32);
  color: white;
  font-size: 0.72rem;
  font-weight: 700;
  padding: 3px 10px;
  border-radius: 20px;
  letter-spacing: 0.05em;
  margin-top: 4px;
}

/* ── Panels ── */
.control-panel {
  background: rgba(255,255,255,0.95);
  backdrop-filter: blur(15px);
  border-radius: 20px;
  padding: 20px;
  box-shadow: 0 15px 40px rgba(0,0,0,0.1);
  position: sticky;
  top: 16px;
  max-height: calc(100vh - 32px);
  overflow-y: auto;
}
.viz-panel {
  background: rgba(255,255,255,0.95);
  backdrop-filter: blur(15px);
  border-radius: 20px;
  padding: 22px;
  box-shadow: 0 15px 40px rgba(0,0,0,0.1);
  min-height: 680px;
}

/* ── Control sections ── */
.control-section {
  margin-bottom: 20px;
  padding-bottom: 16px;
  border-bottom: 1px solid rgba(0,0,0,0.06);
}
.control-section:last-child {
  border-bottom: none;
  margin-bottom: 0;
  padding-bottom: 0;
}
.section-title {
  font-family: 'Poppins', sans-serif;
  font-weight: 600;
  font-size: 0.88rem;
  color: #495057;
  margin-bottom: 10px;
  display: flex;
  align-items: center;
  gap: 7px;
  text-transform: uppercase;
  letter-spacing: 0.04em;
}
.section-title i { color: #E07B39; }

/* ── Metric boxes ── */
.metrics-row {
  display: flex;
  gap: 12px;
  margin-bottom: 20px;
  flex-wrap: wrap;
}
.metric-box {
  flex: 1;
  min-width: 130px;
  background: #fff;
  border-radius: 14px;
  padding: 14px 12px;
  text-align: center;
  box-shadow: 0 4px 16px rgba(0,0,0,0.08);
  border-top: 4px solid var(--mc);
}
.metric-val {
  font-family: 'Poppins', sans-serif;
  font-size: 1.5rem;
  font-weight: 700;
  color: var(--mc);
  line-height: 1.2;
}
.metric-lbl {
  font-size: 0.68rem;
  color: #888;
  margin-top: 3px;
  text-transform: uppercase;
  letter-spacing: 0.05em;
}

/* ── Source tag chips ── */
.source-chip {
  display: inline-flex;
  align-items: center;
  gap: 5px;
  padding: 3px 10px;
  border-radius: 20px;
  font-size: 0.75rem;
  font-weight: 600;
  margin: 2px;
}
.chip-genbank { background: rgba(224,123,57,0.15); color: #b35c1a; }
.chip-gbif    { background: rgba(41,128,185,0.15); color: #1a5276; }
.chip-both    { background: rgba(39,174,96,0.15);  color: #1a5c38; }

/* ── Coverage bar ── */
.coverage-bar-wrap {
  background: rgba(255,255,255,0.9);
  border-radius: 12px;
  padding: 14px;
  margin-top: 10px;
  box-shadow: 0 3px 12px rgba(0,0,0,0.07);
}
.coverage-label {
  font-size: 0.82rem;
  color: #495057;
  font-weight: 500;
  margin-bottom: 6px;
  display: flex;
  justify-content: space-between;
}
.coverage-bar {
  height: 14px;
  border-radius: 7px;
  background: #e9ecef;
  overflow: hidden;
  margin-bottom: 5px;
}
.coverage-fill {
  height: 100%;
  border-radius: 7px;
  transition: width 0.6s ease;
}

/* ── Buttons ── */
.btn-custom {
  border-radius: 12px;
  padding: 10px 16px;
  font-weight: 600;
  font-size: 0.88rem;
  transition: all 0.3s ease;
  border: none;
  display: flex;
  align-items: center;
  justify-content: center;
  gap: 7px;
  width: 100%;
  margin-bottom: 7px;
}
.btn-search {
  background: linear-gradient(135deg, #E07B39, #c0642a);
  color: white;
  font-size: 1rem;
  padding: 12px;
}
.btn-search:hover {
  background: linear-gradient(135deg, #c0642a, #9a4d1e);
  transform: translateY(-2px);
  box-shadow: 0 8px 25px rgba(224,123,57,0.4);
  color: white;
}
.btn-calc {
  background: linear-gradient(135deg, #8e44ad, #6c3483);
  color: white;
}
.btn-calc:hover {
  background: linear-gradient(135deg, #6c3483, #4a235a);
  transform: translateY(-2px);
  box-shadow: 0 8px 25px rgba(142,68,173,0.35);
  color: white;
}
.btn-export-csv {
  background: linear-gradient(135deg, #198754, #20c997);
  color: white;
}
.btn-export-csv:hover {
  background: linear-gradient(135deg, #146c43, #1aa179);
  transform: translateY(-2px);
  box-shadow: 0 8px 25px rgba(25,135,84,0.3);
  color: white;
}
.btn-export-shp {
  background: linear-gradient(135deg, #2980b9, #1a5276);
  color: white;
}
.btn-export-shp:hover {
  background: linear-gradient(135deg, #1a5276, #154360);
  transform: translateY(-2px);
  box-shadow: 0 8px 25px rgba(41,128,185,0.35);
  color: white;
}
.btn-export-dwc {
  background: linear-gradient(135deg, #27ae60, #1a5c38);
  color: white;
}
.btn-export-dwc:hover {
  background: linear-gradient(135deg, #1a5c38, #0e3320);
  transform: translateY(-2px);
  box-shadow: 0 8px 25px rgba(39,174,96,0.35);
  color: white;
}
.btn-export-html {
  background: linear-gradient(135deg, #e88c53, #db7028);
  color: white;
}
.btn-export-html:hover {
  background: linear-gradient(135deg, #db7028, #b05a1a);
  transform: translateY(-2px);
  box-shadow: 0 8px 25px rgba(219,112,40,0.35);
  color: white;
}

/* ── Inputs ── */
.form-control, .form-select {
  border-radius: 10px;
  border: 2px solid #e9ecef;
  transition: all 0.3s ease;
  font-size: 0.9rem;
  padding: 10px 14px;
}
.form-control:focus, .form-select:focus {
  border-color: #E07B39;
  box-shadow: 0 0 0 3px rgba(224,123,57,0.15);
}

/* ── Tabs ── */
.nav-tabs { border: none; margin-bottom: 20px; gap: 6px; display: flex; flex-wrap: wrap; }
.nav-tabs .nav-link {
  border: none;
  border-radius: 10px;
  padding: 9px 16px;
  color: #6c757d;
  font-weight: 500;
  font-size: 0.86rem;
  transition: all 0.3s ease;
  background: rgba(108,117,125,0.1);
  white-space: nowrap;
}
.nav-tabs .nav-link.active {
  background: linear-gradient(135deg, #E28D54, #db7028);
  color: white !important;
  box-shadow: 0 4px 15px rgba(226,141,84,0.35);
}
.nav-tabs .nav-link:hover:not(.active) {
  background: rgba(226,141,84,0.18);
  color: #E28D54;
}

/* ── Quality score gauge ── */
.quality-gauge {
  text-align: center;
  padding: 20px;
  background: #fff;
  border-radius: 16px;
  box-shadow: 0 4px 16px rgba(0,0,0,0.08);
  margin-bottom: 16px;
}
.quality-score {
  font-family: 'Poppins', sans-serif;
  font-size: 3rem;
  font-weight: 700;
  line-height: 1;
}
.quality-label {
  font-size: 0.8rem;
  color: #888;
  text-transform: uppercase;
  letter-spacing: 0.05em;
  margin-top: 6px;
}

/* ── IUCN badges ── */
.uicn-badge {
  display: inline-block;
  padding: 3px 12px;
  border-radius: 20px;
  font-size: 0.78rem;
  font-weight: 700;
  color: #fff;
}

/* ── Alert boxes ── */
.alert-box {
  border-radius: 12px;
  padding: 12px 16px;
  margin: 10px 0;
  font-size: 0.88rem;
  display: flex;
  align-items: flex-start;
  gap: 10px;
}
.alert-info    { background: rgba(41,128,185,0.1);  border-left: 4px solid #2980b9; }
.alert-warning { background: rgba(243,156,18,0.1);  border-left: 4px solid #f39c12; }
.alert-success { background: rgba(39,174,96,0.1);   border-left: 4px solid #27ae60; }
.alert-danger  { background: rgba(231,76,60,0.1);   border-left: 4px solid #e74c3c; }

/* ── Taxon validation panel ── */
.taxon-result {
  background: rgba(39,174,96,0.08);
  border: 1px solid rgba(39,174,96,0.3);
  border-radius: 12px;
  padding: 12px 16px;
  margin-top: 10px;
  font-size: 0.88rem;
}
.taxon-name {
  font-style: italic;
  font-weight: 600;
  color: #1a5c38;
  font-size: 1rem;
}

/* ── Legend ── */
.map-legend {
  background: rgba(255,255,255,0.95);
  border-radius: 10px;
  padding: 10px 14px;
  font-size: 0.8rem;
  margin-top: 10px;
  display: flex;
  gap: 16px;
  flex-wrap: wrap;
  align-items: center;
}
.legend-dot {
  width: 12px; height: 12px;
  border-radius: 50%;
  display: inline-block;
  margin-right: 5px;
  vertical-align: middle;
}

/* ── Progress bar ── */
.progress-container {
  background: rgba(255,255,255,0.92);
  border-radius: 14px;
  padding: 16px;
  margin-top: 14px;
  box-shadow: 0 4px 18px rgba(0,0,0,0.08);
}
.progress { height: 16px; border-radius: 8px; background: rgba(224,123,57,0.12); }
.progress-bar {
  background: linear-gradient(90deg, #f5e6d3, #E07B39);
  border-radius: 8px;
}
.progress-step {
  font-size: 0.82rem;
  color: #495057;
  font-weight: 500;
  margin-bottom: 8px;
  display: flex;
  align-items: center;
  gap: 7px;
}

/* ── Steps indicator ── */
.steps-row {
  display: flex;
  justify-content: space-between;
  margin-bottom: 20px;
  position: relative;
}
.steps-row::before {
  content: '';
  position: absolute;
  top: 14px;
  left: 10%;
  width: 80%;
  height: 2px;
  background: #dee2e6;
  z-index: 0;
}
.step-item {
  display: flex;
  flex-direction: column;
  align-items: center;
  font-size: 0.72rem;
  color: #aaa;
  z-index: 1;
  width: 60px;
  text-align: center;
}
.step-circle {
  width: 28px; height: 28px;
  border-radius: 50%;
  background: #dee2e6;
  color: #888;
  display: flex;
  align-items: center;
  justify-content: center;
  font-weight: 700;
  font-size: 0.8rem;
  margin-bottom: 4px;
}
.step-circle.active  { background: #E07B39; color: white; }
.step-circle.done    { background: #27ae60; color: white; }

/* ── Animations ── */
@keyframes fadeInUp {
  from { opacity:0; transform:translateY(24px); }
  to   { opacity:1; transform:translateY(0); }
}
.fade-in-up { animation: fadeInUp 0.5s ease-out; }

@keyframes spin { to { transform: rotate(360deg); } }
.spin { animation: spin 1s linear infinite; display: inline-block; }

@keyframes heartbeat {
  0%,100% { transform:scale(1); }
  10% { transform:scale(1.15); }
  20% { transform:scale(0.95); }
  30% { transform:scale(1.08); }
  40% { transform:scale(1); }
}
.heartbeat { animation: heartbeat 1.2s infinite; }

/* ── Code/pre ── */
pre {
  background: #1e2030;
  color: #c0c8d0;
  border-radius: 10px;
  font-size: 0.75rem;
  padding: 12px;
  max-height: 180px;
  overflow-y: auto;
}

/* ── Tables ── */
.table { font-size: 0.88rem; }
.table th {
  background: linear-gradient(135deg, #484545, #535354);
  color: white;
  font-weight: 600;
  border: none;
}
.table td { vertical-align: middle; }
.table-hover tbody tr:hover { background: #fdf8f3; }

/* ── Scrollbar ── */
::-webkit-scrollbar { width: 6px; }
::-webkit-scrollbar-track { background: rgba(0,0,0,0.05); border-radius: 3px; }
::-webkit-scrollbar-thumb { background: #E07B39; border-radius: 3px; }

/* ── Footer ── */
.custom-footer {
  background: rgba(255,255,255,0.95);
  margin-top: 32px;
  padding: 22px;
  text-align: center;
  border-radius: 20px 20px 0 0;
  box-shadow: 0 -4px 16px rgba(0,0,0,0.06);
}
.footer-text { color: #EEAA80; font-size: 0.95rem; }
.footer-link { color: #E28D54; text-decoration: none; font-weight: 500; }
.footer-link:hover { color: #27ae60; text-decoration: underline; }

/* ── Responsive ── */
@media (max-width: 768px) {
  .main-title { font-size: 1.6rem; }
  .control-panel { position: static; max-height: none; margin-bottom: 16px; }
  .nav-tabs .nav-link { font-size: 0.78rem; padding: 7px 10px; }
  .metrics-row { flex-direction: column; }
}
"

# ══════════════════════════════════════════════════════════════════════════════
# HELPERS UI
# ══════════════════════════════════════════════════════════════════════════════

# Control section with icon
section_title <- function(icon_name, label) {
  div(class = "section-title",
      icon(icon_name), label)
}

# Reusable metric box
metric_box <- function(output_id, label, color) {
  div(class = "metric-box", style = paste0("--mc:", color),
      div(class = "metric-val", textOutput(output_id, inline = TRUE)),
      div(class = "metric-lbl", label))
}

# Source chip
source_chip <- function(type = c("genbank", "gbif", "both"), label) {
  type <- match.arg(type)
  cls  <- paste0("source-chip chip-", type)
  ic   <- switch(type, genbank = "dna", gbif = "globe", both = "link")
  span(class = cls, icon(ic), label)
}

# Inline alert box
alert_box <- function(type = "info", icon_name, text) {
  div(class = paste0("alert-box alert-", type),
      icon(icon_name, style = "margin-top:2px; flex-shrink:0;"),
      span(text))
}

# ══════════════════════════════════════════════════════════════════════════════
# UI PRINCIPAL
# ══════════════════════════════════════════════════════════════════════════════

ui <- fluidPage(
  
  theme = bs_theme(
    version      = 5,
    bootswatch   = "sandstone",
    primary      = "#39160C",
    secondary    = "#6c757d",
    base_font    = font_google("Inter"),
    heading_font = font_google("Poppins", wght = c(400, 600, 700))
  ),
  
  tags$head(
    tags$style(HTML(CSS)),
    tags$link(rel = "stylesheet",
              href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.4.0/css/all.min.css")
  ),
  
  useShinyjs(),
  
  # ── HEADER ────────────────────────────────────────────────────────────────
  div(class = "main-header fade-in-up",
      tags$img(src = "img/lbi.png", height = "90px"),
      div(
        h1(class = "main-title", "G² Bridge"),
        p(class = "main-subtitle",
          icon(" ", style = "color:#E07B39; margin-right:6px;"),
          "GenBank  · GBIF  · EOO/AOO  · Darwin Core Quality"),
        div(class = "gbif-badge",
            icon(" ", style = "margin-right:4px;"),
            "GBIF Ebbe Nielsen Challenge 2026")
      )
  ),
  
  # ── QUICK METRICS ROW ──────────────────────────────────────────────
  div(class = "metrics-row fade-in-up",
      style = "padding: 0 15px; justify-content: center;",
      metric_box("txt_n_gbif",    "GBIF Records",      COL_GBIF),
      metric_box("txt_n_genbank", "GenBank Records",   COL_GENBANK),
      metric_box("txt_n_ambos",   "In Both Sources",   COL_AMBOS),
      metric_box("txt_eoo",       "EOO (km²)",          COL_EOO),
      metric_box("txt_aoo",       "AOO (km²)",          COL_AOO),
      metric_box("txt_quality",   "Quality Score",      "#8e44ad")
  ),
  
  # ── MAIN LAYOUT ──────────────────────────────────────────────────────────
  fluidRow(
    
    # ── LEFT PANEL ──────────────────────────────────────────────────────────
    column(3,
           div(class = "control-panel fade-in-up",
               
               # ── SECTION 1: Taxonomic search ──────────────────────────
               div(class = "control-section",
                   section_title("magnifying-glass", "Search Species"),
                   
                   textInput("taxon_input",
                             label = NULL,
                             placeholder = "E.g.: Pristimantis curtipes",
                             width = "100%"),
                   
                   # Collapsible filters
                   tags$details(
                     tags$summary(
                       style = "font-size:0.82rem; color:#6c757d; cursor:pointer; margin-bottom:8px;",
                       icon("sliders", style = "margin-right:5px;"), "Advanced filters"
                     ),
                     div(style = "margin-top:10px;",
                         selectInput("filtro_pais",
                                     tags$span(icon("flag"), " Country / Region"),
                                     choices = c("Worldwide"      = "",
                                                 "Ecuador"        = "EC",
                                                 "Colombia"       = "CO",
                                                 "Peru"           = "PE",
                                                 "Bolivia"        = "BO",
                                                 "Venezuela"      = "VE",
                                                 "Brazil"         = "BR",
                                                 "Mexico"         = "MX",
                                                 "Costa Rica"     = "CR"),
                                     width = "100%"),
                         sliderInput("filtro_anios",
                                     tags$span(icon("calendar"), " Year range"),
                                     min = 1950, max = as.integer(format(Sys.Date(), "%Y")),
                                     value = c(2000, as.integer(format(Sys.Date(), "%Y"))),
                                     sep = "", step = 1, width = "100%"),
                         numericInput("filtro_incertidumbre",
                                      tags$span(icon("circle-dot"), " Max. uncertainty (m)"),
                                      value = 5000, min = 100, max = 100000,
                                      step = 500, width = "100%"),
                         div(class = "section-title", style = "margin-top:8px;",
                             icon("database"), "Sources to query"),
                         checkboxInput("usar_gbif",    "GBIF (occurrences)",     value = TRUE),
                         checkboxInput("usar_genbank", "GenBank (sequences)",    value = TRUE),
                         textInput("marcador_genbank",
                                   tags$span(icon("dna"), " Molecular marker"),
                                   value = "", placeholder = "COI, 16S, rbcL...",
                                   width = "100%")
                     )
                   ),
                   
                   # Main search button
                   actionButton("btn_buscar",
                                tags$span(
                                  icon("magnifying-glass", style = "margin-right:6px;"),
                                  "Validate taxon and search"),
                                class = "btn-custom btn-search"),
                   
                   # Taxonomic validation panel (hidden until search)
                   hidden(
                     div(id = "panel_taxon",
                         div(class = "taxon-result",
                             div(style = "font-size:0.76rem; color:#888; margin-bottom:4px;",
                                 icon("circle-check", style = "color:#27ae60; margin-right:4px;"),
                                 "Taxon validated — GBIF Backbone"),
                             div(class = "taxon-name", textOutput("txt_taxon_valido",   inline = TRUE)),
                             div(style = "font-size:0.78rem; color:#555; margin-top:4px;",
                                 "taxonKey: ", textOutput("txt_taxon_key", inline = TRUE),
                                 " · Confidence: ", textOutput("txt_taxon_conf", inline = TRUE), "%")
                         )
                     )
                   )
               ), # /control-section search
               
               # ── SECTION 2: Calculate EOO/AOO ───────────────────────────
               div(class = "control-section",
                   section_title("ruler", "Calculate EOO · AOO"),
                   div(style = "font-size:0.8rem; color:#6c757d; margin-bottom:10px;",
                       "Automatically uses the merged dataset (GBIF + GenBank)."),
                   numericInput("tolerancia_dup",
                                tags$span(icon("circle-dot"), " Deduplication tolerance (m)"),
                                value = 500, min = 50, max = 5000, step = 50,
                                width = "100%"),
                   actionButton("btn_calcular",
                                tags$span(
                                  icon("calculator", style = "margin-right:6px;"),
                                  "Calculate EOO · AOO"),
                                class = "btn-custom btn-calc")
               ), # /control-section EOO/AOO
               
               # ── SECTION 3: Progress ──────────────────────────────────
               hidden(
                 div(id = "progress_panel",
                     div(class = "progress-container",
                         div(class = "progress-step",
                             span(class = "spin", icon("cog")),
                             textOutput("txt_progreso", inline = TRUE)),
                         div(class = "progress",
                             div(id = "progress_fill",
                                 class = "progress-bar progress-bar-striped progress-bar-animated",
                                 role = "progressbar", style = "width:0%;"))
                     )
                 )
               ), # /progreso
               
               # ── SECTION 4: Export ──────────────────────────────────
               div(class = "control-section",
                   section_title("download", "Export"),
                   
                   downloadButton("dl_csv_fusionado",
                                  tags$span(icon("table", style = "margin-right:6px;"),
                                            "Merged dataset (.csv)"),
                                  class = "btn-custom btn-export-csv"),
                   
                   downloadButton("dl_shp_eoo",
                                  tags$span(icon("pentagon", style = "margin-right:6px;"),
                                            "EOO — Convex Hull (.shp)"),
                                  class = "btn-custom btn-export-shp"),
                   
                   downloadButton("dl_shp_aoo",
                                  tags$span(icon("grid-3x3", style = "margin-right:6px;"),
                                            "AOO — 2 km Grid (.shp)"),
                                  class = "btn-custom btn-export-shp"),
                   
                   downloadButton("dl_dwc_archive",
                                  tags$span(icon("box-archive", style = "margin-right:6px;"),
                                            "Darwin Core Archive (.zip)"),
                                  class = "btn-custom btn-export-dwc"),
                   
                   downloadButton("dl_reporte_html",
                                  tags$span(icon("file-code", style = "margin-right:6px;"),
                                            "Full report (.html)"),
                                  class = "btn-custom btn-export-html"),
                   
                   # GBIF citation note
                   hidden(
                     div(id = "gbif_citation_box",
                         alert_box("info", "quote-left",
                                   "The report includes the GBIF citation with download DOI, required for scientific publications.")
                     )
                   )
               ) # /control-section exportar
           ) # /control-panel
    ), # /column 3
    
    # ── RIGHT PANEL (TABS) ──────────────────────────────────────────────
    column(9,
           div(class = "viz-panel fade-in-up",
               
               # Step indicator
               div(class = "steps-row",
                   div(class = "step-item",
                       div(class = "step-circle", id = "step1_circle", "1"),
                       "Search"),
                   div(class = "step-item",
                       div(class = "step-circle", id = "step2_circle", "2"),
                       "Merge"),
                   div(class = "step-item",
                       div(class = "step-circle", id = "step3_circle", "3"),
                       "EOO/AOO"),
                   div(class = "step-item",
                       div(class = "step-circle", id = "step4_circle", "4"),
                       "Quality"),
                   div(class = "step-item",
                       div(class = "step-circle", id = "step5_circle", "5"),
                       "Export")
               ),
               
               tabsetPanel(
                 id   = "mainTabs",
                 type = "tabs",
                 
                 # ── TAB 1: SEARCH ────────────────────────────────────
                 tabPanel(
                   title = tags$span(icon("magnifying-glass", style = "margin-right:5px;"),
                                     "Search"),
                   value = "busqueda",
                   br(),
                   
                   # Sub-tabs GenBank vs GBIF
                   tabsetPanel(
                     type = "pills",
                     tabPanel(
                       title = tags$span(
                         span(class = "source-chip chip-genbank",
                              icon("dna"), "GenBank")),
                       br(),
                       withSpinner(
                         DT::dataTableOutput("tbl_genbank"),
                         type = 4, color = "#E07B39", size = 0.7
                       )
                     ),
                     tabPanel(
                       title = tags$span(
                         span(class = "source-chip chip-gbif",
                              icon("globe"), "GBIF")),
                       br(),
                       withSpinner(
                         DT::dataTableOutput("tbl_gbif"),
                         type = 4, color = "#2980b9", size = 0.7
                       )
                     ),
                     tabPanel(
                       title = tags$span(
                         span(class = "source-chip chip-both",
                              icon("link"), "Merged")),
                       br(),
                       # Coverage score
                       uiOutput("ui_coverage_bars"),
                       br(),
                       withSpinner(
                         DT::dataTableOutput("tbl_fusionado"),
                         type = 4, color = "#27ae60", size = 0.7
                       )
                     )
                   )
                 ), # /Tab 1
                 
                 # ── TAB 2: MERGED MAP ──────────────────────────
                 tabPanel(
                   title = tags$span(icon("map-location-dot", style = "margin-right:5px;"),
                                     "Merged Map"),
                   value = "mapa",
                   br(),
                   
                   # Layer legend
                   div(class = "map-legend",
                       span(tags$span(class = "legend-dot",
                                      style = paste0("background:", COL_GENBANK)),
                            "GenBank only (GBIF candidates)"),
                       span(tags$span(class = "legend-dot",
                                      style = paste0("background:", COL_GBIF)),
                            "GBIF only"),
                       span(tags$span(class = "legend-dot",
                                      style = paste0("background:", COL_AMBOS)),
                            "Both sources"),
                       span(tags$span(class = "legend-dot",
                                      style = paste0("background:", COL_EOO,
                                                     "; border-radius:2px; width:16px; height:10px;")),
                            "EOO"),
                       span(tags$span(class = "legend-dot",
                                      style = paste0("background:", COL_AOO,
                                                     "; border-radius:2px; width:16px; height:10px;")),
                            "AOO")
                   ),
                   
                   withSpinner(
                     leafletOutput("mapa_fusionado", height = "560px", width = "100%"),
                     type = 4, color = "#E07B39", size = 0.8
                   ),
                   
                   # Alert for unique GenBank records
                   uiOutput("ui_alerta_candidatos")
                 ), # /Tab 2
                 
                 # ── TAB 3: EOO / AOO ───────────────────────────────────
                 tabPanel(
                   title = tags$span(icon("ruler-combined", style = "margin-right:5px;"),
                                     "EOO · AOO"),
                   value = "rangos",
                   br(),
                   
                   fluidRow(
                     # Left column: metrics and IUCN
                     column(5,
                            tags$h6("Distribution metrics",
                                    style = "font-family:'Poppins'; font-weight:600; color:#495057;
                                             text-transform:uppercase; letter-spacing:.06em; font-size:.8rem;
                                             border-left:4px solid #e84040; padding-left:10px; margin-bottom:14px;"),
                            tableOutput("tbl_rangos"),
                            hr(),
                            
                            # Comparison by source (the big differentiator)
                            tags$h6("EOO comparison by data source",
                                    style = "font-family:'Poppins'; font-weight:600; color:#495057;
                                             text-transform:uppercase; letter-spacing:.06em; font-size:.8rem;
                                             border-left:4px solid #2980b9; padding-left:10px; margin-bottom:14px;"),
                            withSpinner(
                              plotOutput("plot_eoo_comparativo", height = "200px"),
                              type = 4, color = "#E07B39", size = 0.6
                            ),
                            hr(),
                            uiOutput("ui_uicn_badges")
                     ),
                     
                     # Right column: IUCN criteria + category-change alert
                     column(7,
                            uiOutput("ui_alerta_categoria_cambio"),
                            br(),
                            tags$h6("IUCN Criteria — Criterion B",
                                    style = "font-family:'Poppins'; font-weight:600; color:#495057;
                                             text-transform:uppercase; letter-spacing:.06em; font-size:.8rem;
                                             border-left:4px solid #2980b9; padding-left:10px; margin-bottom:14px;"),
                            tags$table(
                              class = "table table-sm table-hover",
                              tags$thead(tags$tr(
                                tags$th("Criterion"), tags$th("CR"), tags$th("EN"), tags$th("VU")
                              )),
                              tags$tbody(
                                tags$tr(tags$td(tags$b("B1 — EOO")),
                                        tags$td("< 100 km²"), tags$td("< 5,000 km²"), tags$td("< 20,000 km²")),
                                tags$tr(tags$td(tags$b("B2 — AOO")),
                                        tags$td("< 10 km²"),  tags$td("< 500 km²"),   tags$td("< 2,000 km²"))
                              )
                            ),
                            tags$small(class = "text-muted",
                                       icon("triangle-exclamation"), " Indicative assessment.
                               Subcriteria b(i–iii) must be evaluated separately.")
                     )
                   )
                 ), # /Tab 3
                 
                 # ── TAB 4: DARWIN CORE QUALITY ─────────────────────────
                 tabPanel(
                   title = tags$span(icon("chart-bar", style = "margin-right:5px;"),
                                     "Quality"),
                   value = "calidad",
                   br(),
                   
                   fluidRow(
                     # Quality gauge
                     column(4,
                            div(class = "quality-gauge",
                                div(class = "quality-score",
                                    style = "color: var(--qc, #888);",
                                    textOutput("txt_quality_score", inline = TRUE)),
                                div(class = "quality-label", "Darwin Core Quality Score"),
                                br(),
                                div(style = "font-size:0.8rem; color:#888;",
                                    "0 = incomplete · 100 = publishable on GBIF")
                            ),
                            br(),
                            # Candidates for GBIF publishing
                            uiOutput("ui_candidatos_gbif")
                     ),
                     
                     # DwC field completeness bars
                     column(8,
                            tags$h6("Completeness by Darwin Core field",
                                    style = "font-family:'Poppins'; font-weight:600; color:#495057;
                                             text-transform:uppercase; letter-spacing:.06em; font-size:.8rem;
                                             border-left:4px solid #8e44ad; padding-left:10px; margin-bottom:14px;"),
                            withSpinner(
                              uiOutput("ui_dwc_bars"),
                              type = 4, color = "#8e44ad", size = 0.6
                            ),
                            hr(),
                            tags$h6("Recommendations for publishing to GBIF",
                                    style = "font-family:'Poppins'; font-weight:600; color:#495057;
                                             text-transform:uppercase; letter-spacing:.06em; font-size:.8rem;
                                             border-left:4px solid #27ae60; padding-left:10px; margin-bottom:14px;"),
                            uiOutput("ui_recomendaciones_dwc")
                     )
                   )
                 ), # /Tab 4
                 
                 # ── TAB 5: EXPORT ────────────────────────────────────
                 tabPanel(
                   title = tags$span(icon("file-export", style = "margin-right:5px;"),
                                     "Export"),
                   value = "exportar",
                   br(),
                   
                   fluidRow(
                     column(6,
                            tags$h6("Available files",
                                    style = "font-family:'Poppins'; font-weight:600; color:#495057;
                                             text-transform:uppercase; letter-spacing:.06em; font-size:.8rem;
                                             border-left:4px solid #198754; padding-left:10px; margin-bottom:16px;"),
                            
                            # Table of exportable files
                            tags$table(
                              class = "table table-hover",
                              style = "font-size:0.86rem;",
                              tags$thead(tags$tr(
                                tags$th("File"), tags$th("Format"), tags$th("Content")
                              )),
                              tags$tbody(
                                tags$tr(tags$td(icon("table"), " Merged dataset"),
                                        tags$td(tags$code(".csv")),
                                        tags$td("GenBank + GBIF + source flags")),
                                tags$tr(tags$td(icon("pentagon"), " EOO"),
                                        tags$td(tags$code(".zip/.shp")),
                                        tags$td("Convex Hull ESRI Shapefile")),
                                tags$tr(tags$td(icon("grid-3x3"), " AOO"),
                                        tags$td(tags$code(".zip/.shp")),
                                        tags$td("2×2 km Grid ESRI Shapefile")),
                                tags$tr(
                                  tags$td(
                                    icon("box-archive", style = "color:#27ae60;"),
                                    tags$b(" Darwin Core Archive", style = "color:#27ae60;")),
                                  tags$td(tags$code(".zip")),
                                  tags$td("Ready to upload to GBIF IPT — includes meta.xml, eml.xml and occurrences.csv")
                                ),
                                tags$tr(tags$td(icon("file-code", style = "color:#E07B39;"),
                                                " HTML Report"),
                                        tags$td(tags$code(".html")),
                                        tags$td("Interactive map + metrics + GBIF citation"))
                              )
                            )
                     ),
                     
                     column(6,
                            tags$h6("Report preview",
                                    style = "font-family:'Poppins'; font-weight:600; color:#495057;
                                             text-transform:uppercase; letter-spacing:.06em; font-size:.8rem;
                                             border-left:4px solid #E07B39; padding-left:10px; margin-bottom:16px;"),
                            uiOutput("ui_preview_reporte"),
                            
                            br(),
                            # GBIF citation
                            hidden(
                              div(id = "div_cita_gbif",
                                  tags$h6("GBIF Citation (for publications)",
                                          style = "font-family:'Poppins'; font-weight:600; color:#2980b9; font-size:0.82rem;"),
                                  verbatimTextOutput("txt_cita_gbif")
                              )
                            )
                     )
                   )
                 ) # /Tab 5
                 
               ) # /tabsetPanel
           ) # /viz-panel
    ) # /column 9
    
  ), # /fluidRow principal
  
  # ── FOOTER ────────────────────────────────────────────────────────────────
  tags$footer(
    class = "custom-footer",
    tags$hr(style = "width:55px; margin:0 auto 20px auto; border-top:2px solid #E28D54; opacity:0.6;"),
    div(class = "footer-text",
        icon(" ", style = "color:#E28D54; margin-right:8px;"),
        tags$b(" — GBIF Ebbe Nielsen Challenge 2026 — "),
        icon("", style = "color:#6c757d; margin:0 5px;"),
        tags$strong("Fernando J. Castro & Eliana Latacunga")
    ),
    br(),
    div(class = "footer-links",
        tags$a(icon("envelope"), " Contact",
               href = "fernando.castro@est.ikiam.edu.ec", class = "footer-link"),
        " | ",
        tags$a(icon("github"), " GitHub",
               href = "https://github.com/Fernando-JCastro", class = "footer-link"),
        " | ",
        tags$a(icon("globe"), " GBIF.org",
               href = "https://www.gbif.org", class = "footer-link", target = "_blank")
    ),
    br(),
    div(style = "font-size:0.82rem; color:#9d856b; margin-top:6px;",
        "Made with ",
        icon("heart", class = "heartbeat", style = "color:#dc3545; margin:0 4px;"),
        " at the ", tags$b("Integrative Biology Laboratory"), " · Ecuador")
  )
  
) # /fluidPage

# ── Note: server.R goes in a separate file ────────────────────────────
shinyApp(ui, server)


