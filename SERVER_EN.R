# ══════════════════════════════════════════════════════════════════════════════
#  EbeeN — SERVER
#  GBIF Ebbe Nielsen Challenge 2026
#  Autores: Fernando J. Castro & Eliana Latacunga
#
#  Dependencias:
  install.packages(c("shiny","bslib","shinyjs","shinycssloaders",
                    "leaflet","leaflet.extras2","DT","sf","terra",
                       "dplyr","tidyr","ggplot2","GeoRange","httr2",
                       "jsonlite","zip","base64enc","htmlwidgets"))
# ══════════════════════════════════════════════════════════════════════════════

library(shiny)
library(shinyjs)
library(DT)
library(sf)
library(terra)
library(dplyr)
library(tidyr)
library(ggplot2)
library(GeoRange)
library(leaflet)
library(leaflet.extras2)
library(jsonlite)
library(zip)
library(base64enc)
library(htmlwidgets)
library(rentrez)
library(stringr)

# ── Colores (deben coincidir con ui.R) ────────────────────────────────────────
COL_GENBANK <- "#E07B39"
COL_GBIF    <- "#2980b9"
COL_AMBOS   <- "#27ae60"
COL_EOO     <- "#e84040"
COL_AOO     <- "#f59a23"

`%||%` <- function(a, b) if (!is.null(a) && length(a) > 0 && a != "") a else b



# FUNCIONES AUXILIARES
#revisar script de funciones

# ══════════════════════════════════════════════════════════════════════════════
# SERVER
# ══════════════════════════════════════════════════════════════════════════════

server <- function(input, output, session) {
  
  rv <- reactiveValues(
    taxon    = NULL,   # lista: taxon_key, nombre_valido, confianza, rango
    gbif     = NULL,   # data.frame ocurrencias GBIF
    genbank  = NULL,   # data.frame ocurrencias GenBank
    fusionado = NULL,  # data.frame fusionado
    eoo      = list(km2 = NULL, hull = NULL),
    aoo      = list(km2 = NULL, grid = NULL),
    especie  = NULL,
    quality  = list(score = NULL, campos = NULL)
  )
  
  # ── Progreso helper ─────────────────────────────────────────────────────────
  set_progreso <- function(texto, pct) {
    output$txt_progreso <- renderText(texto)
    shinyjs::runjs(paste0(
      'document.getElementById("progress_fill").style.width="', pct, '%";'
    ))
  }
  
  # ── Actualizar step circles ──────────────────────────────────────────────────
  set_step <- function(step_activo) {
    for (i in 1:5) {
      cls <- if (i < step_activo) "step-circle done"
      else if (i == step_activo) "step-circle active"
      else "step-circle"
      shinyjs::runjs(paste0(
        'document.getElementById("step', i, '_circle").className="', cls, '";'
      ))
    }
  }
  
  # ════════════════════════════════════════════════════════════════════════════
  # EVENTO: BUSCAR ─────────────────────────────────────────────────────────────
  # ════════════════════════════════════════════════════════════════════════════
  observeEvent(input$btn_buscar, {
    req(input$taxon_input != "")
    
    shinyjs::show("progress_panel")
    shinyjs::hide("panel_taxon")
    set_step(1)
    
    withProgress(message = "Searching...", value = 0, {
      
      # ── PASO 1: Validar taxón ──────────────────────────────────────────────
      set_progreso("Validating a taxon in GBIF Backbone...", 10)
      incProgress(0.1)
      
      taxon <- tryCatch(
        validar_taxon_gbif(input$taxon_input),
        error = function(e) NULL
      )
      
      if (is.null(taxon)) {
        showNotification(
          paste0("Not found '", input$taxon_input, "' on the GBIF backbone."),
          type = "error", duration = 6)
        shinyjs::hide("progress_panel")
        return()
      }
      
      rv$taxon   <- taxon
      rv$especie <- taxon$nombre_valido
      shinyjs::show("panel_taxon")
      
      # ── PASO 2: Buscar en GBIF ─────────────────────────────────────────────
      df_gbif <- NULL
      if (isTRUE(input$usar_gbif)) {
        set_progreso("Downloading GBIF records...", 30)
        incProgress(0.2)
        df_gbif <- tryCatch(
          get_gbif_occurrences(
            taxon_key         = taxon$taxon_key,
            pais              = input$filtro_pais %||% NULL,
            anio_min          = input$filtro_anios[1],
            anio_max          = input$filtro_anios[2],
            max_incertidumbre = input$filtro_incertidumbre
          ),
          error = function(e) {
            showNotification(paste("Error GBIF:", e$message), type = "warning")
            NULL
          }
        )
        rv$gbif <- df_gbif
        n_gbif  <- if (!is.null(df_gbif)) nrow(df_gbif) else 0
        showNotification(paste0("GBIF: ", n_gbif, " downloaded records."),
                         type = "message", duration = 3)
      }
      
      # ── PASO 3: Buscar en GenBank ──────────────────────────────────────────
      df_genbank <- NULL
      if (isTRUE(input$usar_genbank)) {
        set_progreso("Searching for sequences in GenBank/NCBI...", 55)
        incProgress(0.25)
        
        df_genbank <- tryCatch(
          search_gb_seq(
            organism = taxon$nombre_valido,
            marker   = input$marcador_genbank %||% "",
            country  = input$filtro_pais %||% "",
            max_retry = 3
          ) |>
            get_metadata(),
          error = function(e) {
            showNotification(paste("Error GenBank:", e$message), type = "warning")
            NULL
          }
        )
        
        rv$genbank <- df_genbank
        
        if (!is.null(df_genbank)) {
          # filtrar por coordenadas
          df_genbank <- df_genbank %>%
            dplyr::filter(
              !is.na(latitude), !is.na(longitude)
            )
        }
        
        n_gb_total  <- if (!is.null(df_genbank)) nrow(df_genbank) else 0
        n_gb_coords <- n_gb_total
        
        showNotification(
          paste0("GenBank: ", n_gb_total, " sequences found, ",
                 n_gb_coords, " with coordinates (lat/lon)."),
          type = "message", duration = 4
        )
      }
      
      # ── PASO 4: Fusionar ──────────────────────────────────────────────────
      # Verificar que al menos una fuente tenga datos antes de fusionar
      tiene_datos <- (!is.null(df_gbif)    && is.data.frame(df_gbif)    && nrow(df_gbif)    > 0) ||
        (!is.null(df_genbank) && is.data.frame(df_genbank) && nrow(df_genbank) > 0)
      
      if (!tiene_datos) {
        showNotification(
          paste0("No records with coordinates were found for '", taxon$nombre_valido,
                 "'. Try turning off filters or expanding the date range."),
          type = "warning", duration = 8)
        shinyjs::hide("progress_panel")
        return()
      }
      
      set_progreso("Merging Data Sources...", 75)
      incProgress(0.2)
      set_step(2)
      
      fusionado <- tryCatch(
        merge_genbank_gbif(df_genbank, df_gbif, tolerancia_m = input$tolerancia_dup),
        error = function(e) {
          showNotification(paste("Error merging:", e$message), type = "error")
          NULL
        }
      )
      rv$fusionado <- fusionado
      
      # ── PASO 5: Calidad DwC ───────────────────────────────────────────────
      set_progreso("Calculating Darwin Core Quality...", 90)
      incProgress(0.15)
      if (!is.null(fusionado) && nrow(fusionado) > 0) {
        rv$quality <- calc_dwc_quality(fusionado)
      }
      
      set_progreso("¡Listo!", 100)
      incProgress(0.1)
      set_step(2)
      
    }) # /withProgress
    
    shinyjs::hide("progress_panel")
    shinyjs::show("gbif_citation_box")
    updateTabsetPanel(session, "mainTabs", selected = "busqueda")
    
    showNotification(
      paste0("Search completed for ", rv$especie),
      type = "message", duration = 5)
  })
  
  # ════════════════════════════════════════════════════════════════════════════
  # EVENTO: CALCULAR EOO / AOO ─────────────────────────────────────────────────
  # ════════════════════════════════════════════════════════════════════════════
  observeEvent(input$btn_calcular, {
    req(!is.null(rv$fusionado), nrow(rv$fusionado) >= 3)
    
    shinyjs::show("progress_panel")
    set_step(3)
    
    withProgress(message = "Calculating ranges...", value = 0, {
      
      set_progreso("Calculating EOO (Convex Hull)...", 30)
      incProgress(0.3)
      rv$eoo <- tryCatch(calc_eoo(rv$fusionado),
                         error = function(e) list(km2 = NA, hull = NULL))
      
      set_progreso("Calculating AOO (Grid 2km)...", 65)
      incProgress(0.35)
      rv$aoo <- tryCatch(calc_aoo(rv$fusionado),
                         error = function(e) list(km2 = NA, grid = NULL))
      
      set_progreso("Ranks calculated!", 100)
      incProgress(0.35)
      set_step(4)
    })
    
    shinyjs::hide("progress_panel")
    updateTabsetPanel(session, "mainTabs", selected = "rangos")
    showNotification(
      paste0("EOO: ", format(rv$eoo$km2, big.mark=","), " km²  |  ",
             "AOO: ", format(rv$aoo$km2, big.mark=","), " km²"),
      type = "message", duration = 6)
  })
  
  # ════════════════════════════════════════════════════════════════════════════
  # OUTPUTS — TEXTO RÁPIDO
  # ════════════════════════════════════════════════════════════════════════════
  output$txt_n_gbif <- renderText({
    if (!is.null(rv$gbif)) format(nrow(rv$gbif), big.mark=",") else "—"
  })
  output$txt_n_genbank <- renderText({
    if (!is.null(rv$genbank)) format(nrow(rv$genbank), big.mark=",") else "—"
  })
  output$txt_n_ambos <- renderText({
    if (!is.null(rv$fusionado))
      format(sum(rv$fusionado$en_gbif & rv$fusionado$fuente == "GenBank",
                 na.rm = TRUE), big.mark=",")
    else "—"
  })
  output$txt_eoo <- renderText({
    if (!is.null(rv$eoo$km2) && !is.na(rv$eoo$km2))
      paste(format(rv$eoo$km2, big.mark=","), "km²") else "—"
  })
  output$txt_aoo <- renderText({
    if (!is.null(rv$aoo$km2) && !is.na(rv$aoo$km2))
      paste(format(rv$aoo$km2, big.mark=","), "km²") else "—"
  })
  output$txt_quality <- renderText({
    if (!is.null(rv$quality$score)) paste0(rv$quality$score, "/100") else "—"
  })
  output$txt_taxon_valido <- renderText({ rv$taxon$nombre_valido %||% "—" })
  output$txt_taxon_key    <- renderText({ as.character(rv$taxon$taxon_key %||% "—") })
  output$txt_taxon_conf   <- renderText({ as.character(rv$taxon$confianza %||% "—") })
  output$txt_progreso     <- renderText({ "Iniciando..." })
  output$txt_quality_score <- renderText({
    if (!is.null(rv$quality$score)) as.character(rv$quality$score) else "—"
  })
  output$txt_cita_gbif <- renderText({
    req(rv$taxon)
    paste0("GBIF.org (", format(Sys.Date(), "%Y"),
           ") GBIF Occurrence Download. https://doi.org/10.15468/dl.G2Bridge",
           " [taxonKey=", rv$taxon$taxon_key, "]",
           " Accessed via G² Bridge.")
  })
  
  # ════════════════════════════════════════════════════════════════════════════
  # OUTPUTS — TABLAS
  # ════════════════════════════════════════════════════════════════════════════
  dt_opts <- list(pageLength = 10, scrollX = TRUE,
                  language = list(url = "//cdn.datatables.net/plug-ins/1.10.21/i18n/Spanish.json"))
  
  output$tbl_gbif <- DT::renderDataTable({
    req(rv$gbif)
    rv$gbif |>
      dplyr::select(dplyr::any_of(c("scientificName","lat","lon","year",
                                    "countryCode","basisOfRecord",
                                    "coordinateUncertaintyInMeters","gbifID"))) |>
      DT::datatable(options = dt_opts, rownames = FALSE,
                    class = "table table-sm table-hover")
  })
  
  output$tbl_genbank <- DT::renderDataTable({
    req(rv$genbank)
    # Columnas disponibles tras get_metadata() + renombrado en get_genbank_occurrences()
    cols_gb <- c("accession", "scientificName", "lat", "lon",
                 "country", "City", "Locus", "Source_pb", "Organelle",
                 "Specimen_voucher", "Products", "Kingdom", "Phylum",
                 "Class", "Order", "Family", "Genus", "Definition")
    rv$genbank |>
      dplyr::select(dplyr::any_of(cols_gb)) |>
      DT::datatable(
        options = c(dt_opts, list(scrollX = TRUE)),
        rownames = FALSE,
        class = "table table-sm table-hover"
      )
  })
  
  output$tbl_fusionado <- DT::renderDataTable({
    req(rv$fusionado)
    rv$fusionado |>
      dplyr::select(dplyr::any_of(c("scientificName","lat","lon","fuente",
                                    "en_gbif","unico_genbank","year",
                                    "countryCode","accession"))) |>
      DT::datatable(
        options  = dt_opts, rownames = FALSE,
        class    = "table table-sm table-hover",
        callback = DT::JS("table.rows().every(function(){
          var d = this.data();
          if(d[4] === false) this.node().style.background='rgba(224,123,57,0.08)';
        });")
      )
  })
  
  output$tbl_rangos <- renderTable({
    req(rv$eoo$km2)
    data.frame(
      Métrica  = c("EOO (km²)", "AOO (km²)", "Total records",
                   "GBIF records", "GenBank records"),
      Valor    = c(
        format(rv$eoo$km2, big.mark = ","),
        format(rv$aoo$km2, big.mark = ","),
        if (!is.null(rv$fusionado)) format(nrow(rv$fusionado), big.mark=",") else "—",
        if (!is.null(rv$gbif))    format(nrow(rv$gbif),     big.mark=",") else "—",
        if (!is.null(rv$genbank)) format(nrow(rv$genbank),  big.mark=",") else "—"
      )
    )
  }, striped = TRUE, hover = TRUE, bordered = TRUE, width = "100%")
  
  # ════════════════════════════════════════════════════════════════════════════
  # OUTPUTS — UI DINÁMICOS
  # ════════════════════════════════════════════════════════════════════════════
  
  # Coverage bars (Tab 1 / sub-tab Fusionado)
  output$ui_coverage_bars <- renderUI({
    req(rv$fusionado)
    df <- rv$fusionado
    n  <- nrow(df)
    if (n == 0) return(NULL)
    
    n_gbif    <- sum(df$fuente == "GBIF", na.rm = TRUE)
    n_gb      <- sum(df$fuente == "GenBank", na.rm = TRUE)
    n_dup     <- sum(df$en_gbif & df$fuente == "GenBank", na.rm = TRUE)
    n_unicos  <- sum(df$unico_genbank, na.rm = TRUE)
    
    pct_gbif   <- round(100 * n_gbif / n)
    pct_gb     <- round(100 * n_gb   / n)
    pct_unicos <- if (n_gb > 0) round(100 * n_unicos / n_gb) else 0
    
    div(class = "coverage-bar-wrap",
        div(class = "coverage-label",
            span(icon("globe", style="color:#2980b9"), " GBIF"),
            span(paste0(n_gbif, " records (", pct_gbif, "%)"))),
        div(class = "coverage-bar",
            div(class = "coverage-fill",
                style = paste0("width:", pct_gbif, "%; background:", COL_GBIF))),
        div(class = "coverage-label",
            span(icon("dna", style="color:#E07B39"), " GenBank"),
            span(paste0(n_gb, " records (", pct_gb, "%)"))),
        div(class = "coverage-bar",
            div(class = "coverage-fill",
                style = paste0("width:", pct_gb, "%; background:", COL_GENBANK))),
        div(class = "coverage-label",
            span(icon("triangle-exclamation", style="color:#e67e22"),
                 " Candidates for publication in GBIF (GenBank only)"),
            span(paste0(n_unicos, " records (", pct_unicos, "% from GenBank)"))),
        div(class = "coverage-bar",
            div(class = "coverage-fill",
                style = paste0("width:", pct_unicos, "%; background:#e67e22")))
    )
  })
  
  # Alerta candidatos (Tab 2 mapa)
  output$ui_alerta_candidatos <- renderUI({
    req(rv$fusionado)
    n_unicos <- sum(rv$fusionado$unico_genbank, na.rm = TRUE)
    if (n_unicos == 0) return(NULL)
    div(class = "alert-box alert-warning",
        style = "margin-top:12px;",
        icon("triangle-exclamation"),
        tags$span(
          tags$b(paste0(n_unicos, " GenBank sequences are NOT in GBIF.")),
          " These records (orange dots) are candidates for publication on GBIF.",
          " Download the Darwin Core Archive from the left panel."
        )
    )
  })
  
  # Alerta cambio de categoría UICN (Tab 3)
  output$ui_alerta_categoria_cambio <- renderUI({
    req(rv$eoo$km2, rv$fusionado)
    
    # EOO con solo GBIF
    df_solo_gbif <- rv$fusionado |> dplyr::filter(fuente == "GBIF")
    eoo_gbif_only <- if (nrow(df_solo_gbif) >= 3)
      tryCatch(GeoRange::CHullAreaEarth(df_solo_gbif$lon, df_solo_gbif$lat),
               error = function(e) NA)
    else NA
    
    if (is.na(eoo_gbif_only)) return(NULL)
    
    cat_fusionado <- uicn_cat(rv$eoo$km2,    "eoo")$label
    cat_solo_gbif <- uicn_cat(eoo_gbif_only, "eoo")$label
    
    if (cat_fusionado == cat_solo_gbif) return(NULL)
    
    div(class = "alert-box alert-warning",
        icon("triangle-exclamation"),
        tags$span(
          tags$b("⚠ Change in IUCN category due to the inclusion of GenBank data: "),
          paste0("Only with GBI → ", tags$b(cat_solo_gbif),
                 " | With merged data:", tags$b(cat_fusionado)),
          ". A review of the conservation status is recommended."
        )
    )
  })
  
  # Badges UICN (Tab 3)
  output$ui_uicn_badges <- renderUI({
    req(rv$eoo$km2)
    cats_eoo <- uicn_cat(rv$eoo$km2, "eoo")
    cats_aoo <- uicn_cat(rv$aoo$km2, "aoo")
    tagList(
      tags$p(tags$b("IUCN Tentative Category (Criterion B):"),
             style = "font-size:.85rem; margin-bottom:8px"),
      div(style = "display:flex; gap:14px; flex-wrap:wrap; align-items:center;",
          tags$span(
            tags$span(class = "uicn-badge",
                      style = paste0("background:", cats_eoo$color),
                      cats_eoo$label),
            tags$span(style = "font-size:.82rem; color:#555;",
                      paste0(" B1-EOO: ",
                             format(rv$eoo$km2, big.mark=","), " km²"))
          ),
          tags$span(
            tags$span(class = "uicn-badge",
                      style = paste0("background:", cats_aoo$color),
                      cats_aoo$label),
            tags$span(style = "font-size:.82rem; color:#555;",
                      paste0(" B2-AOO: ",
                             format(rv$aoo$km2, big.mark=","), " km²"))
          )
      )
    )
  })
  
  # Barras DwC calidad (Tab 4)
  output$ui_dwc_bars <- renderUI({
    req(rv$quality$campos)
    df <- rv$quality$campos
    if (nrow(df) == 0) return(NULL)
    
    bars <- lapply(seq_len(nrow(df)), function(i) {
      pct   <- df$pct[i]
      col   <- if (pct >= 80) "#27ae60"
      else if (pct >= 50) "#f39c12"
      else "#e74c3c"
      div(style = "margin-bottom:10px;",
          div(class = "coverage-label",
              span(style = paste0("font-weight:600; color:", col),
                   df$campo[i]),
              span(paste0(pct, "%"))),
          div(class = "coverage-bar",
              div(class = "coverage-fill",
                  style = paste0("width:", pct, "%; background:", col)))
      )
    })
    div(bars)
  })
  
  # Recomendaciones DwC (Tab 4)
  output$ui_recomendaciones_dwc <- renderUI({
    req(rv$quality$campos)
    df   <- rv$quality$campos
    bajos <- df[df$pct < 80, ]
    if (nrow(bajos) == 0) {
      return(div(class = "alert-box alert-success",
                 icon("circle-check"),
                 "Excellent! All the main DwC fields have a high completeness score."))
    }
    recs <- lapply(seq_len(nrow(bajos)), function(i) {
      campo <- bajos$campo[i]
      pct   <- bajos$pct[i]
      msg <- switch(campo,
                    "basisOfRecord" = "Add 'basisOfRecord' (e.g., PreservedSpecimen, HumanObservation). Required field for publishing on GBIF.",
                    "year"          = "Complete the collection year. Improves the temporal utility of the dataset.",
                    "countryCode"   = "Add the ISO country code (e.g., EC, CO, PE). Required by GBIF.",
                    "institutionCode" = "Identify the custodian institution of the specimen (e.g., QCAZ, MECN, INABIO).",
                    "coordinateUncertaintyInMeters" = "Document the uncertainty in meters. Allows filtering by spatial quality.",
                    paste0("Campo '", campo, "' con solo ", pct, "% de completitud.")
      )
      div(class = "alert-box alert-warning", style = "margin-bottom:6px;",
          icon("lightbulb"),
          tags$span(tags$b(campo, ": "), msg))
    })
    div(recs)
  })
  
  # Candidatos a GBIF (Tab 4)
  output$ui_candidatos_gbif <- renderUI({
    req(rv$fusionado)
    n_unicos <- sum(rv$fusionado$unico_genbank, na.rm = TRUE)
    col <- if (n_unicos > 0) "#e67e22" else "#27ae60"
    div(style = paste0("background:rgba(", ifelse(n_unicos>0,"230,126,34","39,174,96"), ",0.1);",
                       "border:1px solid ", col, ";border-radius:12px;padding:14px;"),
        div(style = paste0("font-family:'Poppins';font-size:1.8rem;font-weight:700;color:", col,
                           ";text-align:center;"), n_unicos),
        div(style = "text-align:center;font-size:.78rem;color:#888;text-transform:uppercase;
                     letter-spacing:.04em;margin-top:4px;",
            "GenBank sequences", br(), "records eligible for publication on GBIF"),
        if (n_unicos > 0)
          div(style = "font-size:.78rem;color:#888;margin-top:8px;text-align:center;",
              "Download the Darwin Core Archive to publish them on the GBIF IPT.")
    )
  })
  
  # Preview del reporte (Tab 5)
  output$ui_preview_reporte <- renderUI({
    if (is.null(rv$especie)) {
      return(div(class = "alert-box alert-info",
                 icon("circle-info"),
                 "Perform a search first to preview the report."))
    }
    cats_eoo <- uicn_cat(rv$eoo$km2, "eoo")
    div(style = "background:#fff; border-radius:12px; padding:16px;
                 box-shadow:0 3px 12px rgba(0,0,0,.08); font-size:.86rem;",
        tags$p(tags$b("Species: "), tags$i(rv$especie)),
        tags$p(tags$b("taxonKey GBIF: "), as.character(rv$taxon$taxon_key %||% "—")),
        tags$p(tags$b("EOO: "),
               if (!is.null(rv$eoo$km2) && !is.na(rv$eoo$km2))
                 paste(format(rv$eoo$km2, big.mark=","), "km²") else "—"),
        tags$p(tags$b("AOO: "),
               if (!is.null(rv$aoo$km2) && !is.na(rv$aoo$km2))
                 paste(format(rv$aoo$km2, big.mark=","), "km²") else "—"),
        tags$p(tags$b("DwC Quality Score: "),
               if (!is.null(rv$quality$score)) paste0(rv$quality$score, "/100") else "—"),
        tags$p(tags$b("GBIF Candidates: "),
               if (!is.null(rv$fusionado))
                 paste(sum(rv$fusionado$unico_genbank, na.rm=TRUE), "records") else "—")
    )
  })
  
  # Gráfico comparativo EOO por fuente (Tab 3)
  output$plot_eoo_comparativo <- renderPlot({
    req(rv$eoo$km2, rv$fusionado)
    
    df_gbif <- rv$fusionado |> dplyr::filter(fuente == "GBIF")
    df_gb   <- rv$fusionado |> dplyr::filter(fuente == "GenBank")
    
    eoo_gbif   <- if (nrow(df_gbif) >= 3)
      tryCatch(GeoRange::CHullAreaEarth(df_gbif$lon, df_gbif$lat), error=function(e) NA) else NA
    eoo_genbank <- if (nrow(df_gb) >= 3)
      tryCatch(GeoRange::CHullAreaEarth(df_gb$lon, df_gb$lat),   error=function(e) NA) else NA
    
    col_gbif    <- if (exists("COL_GBIF") && !is.na(COL_GBIF)) COL_GBIF else "#3498db"
    col_genbank <- if (exists("COL_GENBANK") && !is.na(COL_GENBANK)) COL_GENBANK else "#2ecc71"
    col_ambos   <- if (exists("COL_AMBOS") && !is.na(COL_AMBOS)) COL_AMBOS else "#9b59b6"
    
    eoo_gbif_val    <- if (is.na(eoo_gbif)) 0 else eoo_gbif
    eoo_genbank_val <- if (is.na(eoo_genbank)) 0 else eoo_genbank
    
    datos <- data.frame(
      Fuente = c("Only GBIF", "Only GenBank", "Merged"),
      EOO    = c(eoo_gbif_val, eoo_genbank_val, rv$eoo$km2),
      Color  = c(col_gbif, col_genbank, col_ambos)
    ) |> dplyr::filter(!is.na(EOO))
    
    ggplot2::ggplot(datos, ggplot2::aes(x = Fuente, y = EOO, fill = Fuente)) +
      ggplot2::geom_col(width = 0.55, show.legend = FALSE) +
      ggplot2::scale_fill_manual(values = setNames(datos$Color, datos$Fuente)) +
      ggplot2::ylim(0, max(datos$EOO) * 1.15) +
      ggplot2::geom_text(ggplot2::aes(label = paste0(format(round(EOO), big.mark=","), " km²")),
                         vjust = -0.5, size = 3.5, fontface = "bold") +
      ggplot2::labs(x = NULL, y = "EOO (km²)",
                    title = "EOO by data source") +
      ggplot2::theme_minimal(base_size = 11) +
      ggplot2::theme(

        plot.title   = ggplot2::element_text(family = "sans", face = "bold",
                                             size = 11, color = "#333333"),
        panel.grid.major.x = ggplot2::element_blank(),
      
        axis.text.x  = ggplot2::element_text(face = "bold", color = "#333333")
      )
  })
  
  
  # ════════════════════════════════════════════════════════════════════════════
  # MAPA LEAFLET
  # ════════════════════════════════════════════════════════════════════════════
  output$mapa_fusionado <- renderLeaflet({
    req(rv$fusionado, nrow(rv$fusionado) > 0)
    build_mapa_fusionado(rv)
  })
  
  # ════════════════════════════════════════════════════════════════════════════
  # DESCARGAS
  # ════════════════════════════════════════════════════════════════════════════
  sp_slug <- reactive({
    gsub(" ", "_", rv$especie %||% "Species")
  })
  
  output$dl_csv_fusionado <- downloadHandler(
    filename = function() paste0(sp_slug(), "_merged.csv"),
    content  = function(file) {
      req(rv$fusionado)
      utils::write.csv(rv$fusionado, file, row.names = FALSE)
    }
  )
  
  output$dl_shp_eoo <- downloadHandler(
    filename = function() paste0(sp_slug(), "_EOO.zip"),
    content  = function(file) {
      req(rv$eoo$hull)
      tryCatch({
        z <- sf_to_zip(rv$eoo$hull, paste0(sp_slug(), "_EOO"))
        file.copy(z, file)
      }, error = function(e)
        showNotification(paste("Error SHP EOO:", e$message), type = "error"))
    }
  )
  
  output$dl_shp_aoo <- downloadHandler(
    filename = function() paste0(sp_slug(), "_AOO.zip"),
    content  = function(file) {
      req(rv$aoo$grid)
      tryCatch({
        z <- sf_to_zip(rv$aoo$grid, paste0(sp_slug(), "_AOO"))
        file.copy(z, file)
      }, error = function(e)
        showNotification(paste("Error SHP AOO:", e$message), type = "error"))
    }
  )
  
  output$dl_dwc_archive <- downloadHandler(
    filename = function() paste0(sp_slug(), "_DwC_Archive.zip"),
    content  = function(file) {
      req(rv$fusionado)
      tryCatch({
        z <- generar_dwc_archive(
          df = rv$fusionado[rv$fusionado$unico_genbank %in% TRUE, ],
          especie = rv$especie,
          taxon_key = rv$taxon$taxon_key,
          solo_candidatos = FALSE
        ) 
        file.copy(z, file)
        shinyjs::show("div_cita_gbif")
      }, error = function(e)
        showNotification(paste("Error DwC:", e$message), type = "error"))
    }
  )
  
  output$dl_reporte_html <- downloadHandler(
    filename = function() paste0(sp_slug(), "_report.html"),
    content  = function(file) {
      req(rv$especie)
      tryCatch({
        html <- generar_reporte_html(rv)
        writeLines(html, file, useBytes = FALSE)
      }, error = function(e)
        showNotification(paste("Error report:", e$message), type = "error"))
    }
  )
  
} # /server


shinyApp(ui, server)

