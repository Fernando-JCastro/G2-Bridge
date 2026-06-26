###########################
# ------   Functions -----------

# ── 1. Validate taxon against GBIF Backbone ─────────────────────────────────────
validar_taxon_gbif <- function(nombre) {
  url  <- paste0("https://api.gbif.org/v1/species/match?name=",
                 utils::URLencode(nombre, reserved = TRUE))
  resp <- tryCatch(jsonlite::fromJSON(url), error = function(e) NULL)
  if (is.null(resp) || is.null(resp$usageKey)) return(NULL)
  list(
    taxon_key    = resp$usageKey,
    nombre_valido = resp$canonicalName %||% resp$scientificName %||% nombre,
    confianza    = resp$confidence %||% 0,
    rango        = resp$rank %||% "UNKNOWN",
    reino        = resp$kingdom %||% NA_character_
  )
}


# ── 2. Get GBIF occurrences with pagination ─────
get_gbif_occurrences <- function(taxon_key, pais = NULL,
                                 anio_min = NULL, anio_max = NULL,
                                 max_incertidumbre = NULL,
                                 limit = 5000) {
  params <- list(
    taxonKey           = taxon_key,
    hasCoordinate      = "true",
    hasGeospatialIssue = "false",
    limit              = min(limit, 300)   # GBIF max per page = 300
  )
  if (!is.null(pais)             && pais != "")
    params$country <- pais
  if (!is.null(anio_min)         && !is.null(anio_max))
    params$year    <- paste0(anio_min, ",", anio_max)
  if (!is.null(max_incertidumbre) && max_incertidumbre > 0)
    params$coordinateUncertaintyInMeters <- paste0("0,", max_incertidumbre)
  
  todos   <- list()
  offset  <- 0
  total   <- Inf
  
  while (offset < min(total, limit)) {
    params$offset <- offset
    query_str <- paste(
      mapply(function(k, v) paste0(k, "=", v), names(params), params),
      collapse = "&"
    )
    url  <- paste0("https://api.gbif.org/v1/occurrence/search?", query_str)
    resp <- tryCatch(jsonlite::fromJSON(url, flatten = TRUE),
                     error = function(e) NULL)
    if (is.null(resp) || length(resp$results) == 0) break
    
    total  <- resp$count %||% 0
    todos  <- c(todos, list(resp$results))
    offset <- offset + nrow(resp$results)
    if (isTRUE(resp$endOfRecords)) break
    Sys.sleep(0.25)
  }
  
  if (length(todos) == 0) return(NULL)
  
  campos <- c("gbifID", "scientificName", "decimalLatitude", "decimalLongitude",
              "coordinateUncertaintyInMeters", "year", "month", "day",
              "countryCode", "stateProvince", "locality", "basisOfRecord",
              "institutionCode", "collectionCode", "catalogNumber", "license",
              "taxonKey", "speciesKey")
  
  dplyr::bind_rows(todos) |>
    dplyr::select(dplyr::any_of(campos)) |>
    dplyr::rename_with(~ "lat", dplyr::any_of("decimalLatitude"))  |>
    dplyr::rename_with(~ "lon", dplyr::any_of("decimalLongitude")) |>
    dplyr::mutate(fuente = "GBIF",
                  accession = NA_character_,
                  en_gbif   = TRUE,
                  unico_genbank = FALSE) |>
    dplyr::filter(!is.na(lat), !is.na(lon))
}



# ── 3. Search GenBank — uses search_gb_seq() + get_metadata() from the Eliat package ──


# ── 4. Merge GenBank + GBIF with spatial join ───────────────────────────────
merge_genbank_gbif <- function(df_genbank, df_gbif, tolerancia_m = 200) {
  gb <- df_genbank |> dplyr::select(
    lat = latitude,
    lon = longitude,
    scientificName = Organism,
    IDCode = Accession,
    countryCode = Country,
    specimen_voucher = Specimen_voucher
  ) |>
    dplyr::mutate(
      lat = suppressWarnings(as.numeric(lat)),
      lon = suppressWarnings(as.numeric(lon)),
      fuente = "GenBank",
      lat_original = lat,
      lon_original = lon,
      basisOfRecord = "MATERIAL_CITATION"
    )
  
  gf <- df_gbif |> dplyr::select(
    lat = lat,
    lon = lon,
    scientificName = scientificName
  ) |>
    dplyr::mutate(
      fuente = "GBIF",
      IDCode = NA_character_,
      countryCode = NA_character_,
      specimen_voucher = NA_character_,
      lat_original = lat,
      lon_original = lon
    )
  
  gb_sf <- sf::st_as_sf(
    gb |> dplyr::filter(!is.na(lat), !is.na(lon)),
    coords = c("lon", "lat"),
    crs = 4326
  )
  
  gf_sf <- sf::st_as_sf(
    gf |> dplyr::filter(!is.na(lat), !is.na(lon)),
    coords = c("lon", "lat"),
    crs = 4326
  )
  
  gb_cea <- sf::st_transform(gb_sf, "+proj=cea +units=m")
  gf_cea <- sf::st_transform(gf_sf, "+proj=cea +units=m")
  
  idx <- sf::st_is_within_distance(gb_cea, gf_cea, dist = tolerancia_m)
  gb_sf$en_gbif <- lengths(idx) > 0
  gb_sf$distancia_min_m <- sapply(seq_along(idx), function(j) {
    i <- idx[[j]]
    if (length(i) == 0) return(NA_real_)
    as.numeric(min(sf::st_distance(gb_cea[j, ], gf_cea[i, ])))
  })
  
  combinado <- dplyr::bind_rows(
    sf::st_drop_geometry(gb_sf) |> dplyr::mutate(unico_genbank = !en_gbif),
    sf::st_drop_geometry(gf_sf) |> dplyr::mutate(unico_genbank = FALSE, en_gbif = TRUE)
  ) |>
    dplyr::select(
      lat = lat_original,
      lon = lon_original,
      scientificName,
      IDCode,
      countryCode,
      specimen_voucher,
      fuente,
      en_gbif,
      distancia_min_m,
      unico_genbank,
      dplyr::everything()
    )
  
  return(combinado)
}


# ── 5. Calculate EOO with CHullAreaEarth ────────────────────────────────────────
calc_eoo <- function(df) {
  pts <- df |> dplyr::filter(!is.na(lat), !is.na(lon))
  if (nrow(pts) < 3) return(list(km2 = NA, hull = NULL))
  
  eoo_km2  <- round(GeoRange::CHullAreaEarth(pts$lon, pts$lat), 2)
  hull_idx <- chull(pts$lon, pts$lat)
  hull_c   <- as.matrix(pts[c(hull_idx, hull_idx[1]), c("lon", "lat")])
  hull_sf  <- sf::st_polygon(list(hull_c)) |>
    sf::st_sfc(crs = 4326) |>
    sf::st_sf(eoo_km2 = eoo_km2, geometry = _)
  
  list(km2 = eoo_km2, hull = hull_sf)
}




# ── 6. Calculate AOO with 2km grid ────────────────────────────────────────────
calc_aoo <- function(df) {
  pts <- df |> dplyr::filter(!is.na(lat), !is.na(lon))
  if (nrow(pts) < 1) return(list(km2 = NA, grid = NULL))
  
  pts_sf  <- sf::st_as_sf(pts, coords = c("lon", "lat"), crs = 4326)
  pts_cea <- sf::st_transform(pts_sf, "+proj=cea +units=m")
  ext_r   <- terra::ext(pts_cea) + c(-2000, 2000, -2000, 2000)
  r       <- terra::rast(ext_r, res = 2000,
                         crs = sf::st_crs(pts_cea)$wkt)
  terra::values(r) <- 0
  r_ocu  <- terra::rasterize(terra::vect(pts_cea), r, field = 1, fun = "first")
  n_cel  <- sum(terra::values(r_ocu) == 1, na.rm = TRUE)
  aoo_km2 <- n_cel * 4
  
  grid_sf <- terra::as.polygons(r_ocu > 0, na.rm = TRUE, dissolve = FALSE) |>
    sf::st_as_sf() |>
    sf::st_transform(crs = 4326)
  grid_sf$aoo_km2 <- aoo_km2
  
  list(km2 = aoo_km2, grid = grid_sf)
}



# ── 7. Indicative IUCN category ─────────────────────────────────────────────
uicn_cat <- function(valor, tipo = c("eoo", "aoo")) {
  tipo <- match.arg(tipo)
  if (is.na(valor)) return(list(label = "NA", color = "#888888"))
  umbrales <- if (tipo == "eoo")
    list(cr = 100, en = 5000, vu = 20000)
  else
    list(cr = 10,  en = 500,  vu = 2000)
  
  if      (valor < umbrales$cr) list(label = "CR", color = "#c0392b")
  else if (valor < umbrales$en) list(label = "EN", color = "#e67e22")
  else if (valor < umbrales$vu) list(label = "VU", color = "#f1c40f")
  else                           list(label = "LC/NT", color = "#27ae60")
}



# ── 8. Darwin Core Quality Score ──────────────────────────────────────────────
calc_dwc_quality <- function(df) {
  if (!"unico_genbank" %in% names(df)) {
    stop("The 'unico_genbank' column does not exist in df.")
  }
  
  df <- df[df$unico_genbank %in% TRUE, , drop = FALSE]
  n <- nrow(df)
  if (n == 0) return(list(score = 0, campos = data.frame()))
  campos_dwc <- c(
    "lat"                = "decimalLatitude",
    "lon"                = "decimalLongitude",
    "scientificName"     = "scientificName",
    "year"               = "year",
    "countryCode"        = "countryCode",
    "basisOfRecord"      = "basisOfRecord",
    "coordinateUncertaintyInMeters" = "coordinateUncertaintyInMeters"
  )
  
  pesos <- c(
    lat = 20, lon = 20, scientificName = 15, year = 10,
    countryCode = 10, basisOfRecord = 10,
    catalogNumber = 8, coordinateUncertaintyInMeters = 7
  )
  
  resultados <- lapply(names(campos_dwc), function(col_interno) {
    col_real <- if (col_interno %in% names(df)) col_interno else campos_dwc[col_interno]
    if (!col_real %in% names(df)) {
      pct <- 0
    } else {
      pct <- round(100 * sum(!is.na(df[[col_real]]) &
                               df[[col_real]] != "") / n, 1)
    }
    peso <- pesos[col_interno] %||% 5
    list(campo = campos_dwc[col_interno], pct = pct, peso = peso)
  })
  
  df_campos <- do.call(rbind, lapply(resultados, as.data.frame))
  score_total <- round(sum(df_campos$pct * df_campos$peso) / sum(df_campos$peso), 1)
  
  list(score = score_total, campos = df_campos)
}


# ── 9. Generate DwC Archive ZIP ────────────────────────────────────────────────
generar_dwc_archive <- function(df, especie, taxon_key = NA, solo_candidatos = TRUE) {
  if (solo_candidatos) {
    if (!"unico_genbank" %in% names(df)) {
      stop("The 'unico_genbank' column does not exist in df.")
    }
    df <- df[df$unico_genbank %in% TRUE, , drop = FALSE]
  }
  
  if (nrow(df) == 0) stop("No candidate records available for export.")
  
  tmp <- tempfile()
  dir.create(tmp)
  
  cols_necesarias <- c(
    "basisOfRecord","scientificName","lat","lon",
    "coordinateUncertaintyInMeters","year","month",
    "countryCode","accession"
  )
  
  for (nm in setdiff(cols_necesarias, names(df))) df[[nm]] <- NA
  
  occ <- df |>
    dplyr::transmute(
      occurrenceID = as.character(dplyr::row_number()),
      basisOfRecord = dplyr::coalesce(as.character(basisOfRecord), "MATERIAL_CITATION"),
      scientificName = scientificName,
      decimalLatitude = lat,
      decimalLongitude = lon,
      coordinateUncertaintyInMeters = coordinateUncertaintyInMeters,
      year = year,
      month = month,
      countryCode = countryCode,
      institutionCode = stringr::str_extract(as.character(specimen_voucher), "^[A-Za-z]+"),
      catalogNumer = specimen_voucher,
      associatedSequences = IDCode,
      datasetName = paste0("G² Bridge", gsub(" ", "_", especie), "_GenBank_candidates"),
      license = "http://creativecommons.org/licenses/by/4.0/legalcode"
    ) 
  
  
  utils::write.csv(occ, file.path(tmp, paste0("occurrences_", especie, ".csv")), row.names = FALSE)
  
  meta_xml <- '<?xml version="1.0" encoding="UTF-8"?>
<archive xmlns="http://rs.tdwg.org/dwc/text/"
         xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
         xsi:schemaLocation="http://rs.tdwg.org/dwc/text/ http://rs.tdwg.org/dwc/text/tdwg_dwc_text.xsd">
  <core encoding="UTF-8" fieldsTerminatedBy="," linesTerminatedBy="\n"
        fieldsEnclosedBy="&quot;" ignoreHeaderLines="1"
        rowType="http://rs.tdwg.org/dwc/terms/Occurrence">
    <files><location>occurrences.csv</location></files>
    <id index="0"/>
    <field index="1" term="http://rs.tdwg.org/dwc/terms/basisOfRecord"/>
    <field index="2" term="http://rs.tdwg.org/dwc/terms/scientificName"/>
    <field index="3" term="http://rs.tdwg.org/dwc/terms/decimalLatitude"/>
    <field index="4" term="http://rs.tdwg.org/dwc/terms/decimalLongitude"/>
    <field index="5" term="http://rs.tdwg.org/dwc/terms/coordinateUncertaintyInMeters"/>
    <field index="6" term="http://rs.tdwg.org/dwc/terms/year"/>
    <field index="7" term="http://rs.tdwg.org/dwc/terms/month"/>
    <field index="8" term="http://rs.tdwg.org/dwc/terms/countryCode"/>
    <field index="9" term="http://rs.tdwg.org/dwc/terms/institutionCode"/>
    <field index="10" term="http://rs.tdwg.org/dwc/terms/catalogNumber"/>
    <field index="11" term="http://rs.tdwg.org/dwc/terms/datasetName"/>
    <field index="12" term="http://rs.tdwg.org/dwc/terms/license"/>
  </core>
</archive>'
  
  writeLines(meta_xml, file.path(tmp, "meta.xml"))
  
  
  eml_xml <- paste0('<?xml version="1.0" encoding="UTF-8"?>
<eml:eml xmlns:eml="eml://ecoinformatics.org/eml-2.1.1"
         packageId="G² Bridge-', gsub(" ", "-", tolower(especie)), '-genbank-candidates"
         system="G² Bridge">
  <dataset>
    <title>G² Bridge — GenBank candidate occurrences for ', especie, '</title>
    <creator><individualName><surName>G² Bridge </surName></individualName></creator>
    <abstract><para>Subset of GenBank occurrence records flagged as candidates for publication in GBIF for ', especie, '.</para></abstract>
    <intellectualRights><para>CC BY 4.0 https://creativecommons.org/licenses/by/4.0/</para></intellectualRights>
  </dataset>
</eml:eml>')
  writeLines(eml_xml, file.path(tmp, "eml.xml"))
  
  zip_path <- tempfile(fileext = ".zip")
  zip::zip(zip_path, files = list.files(tmp, full.names = TRUE), mode = "cherry-pick")
  zip_path
}




# ── 10. Export SF to ZIP with shapefile ───────────────────────────────────────
sf_to_zip <- function(sf_obj, layer_name) {
  tmp_dir  <- tempfile()
  dir.create(tmp_dir)
  shp_path <- file.path(tmp_dir, paste0(layer_name, ".shp"))
  sf::st_write(sf_obj, shp_path, driver = "ESRI Shapefile",
               quiet = TRUE, delete_layer = TRUE)
  zip_path <- tempfile(fileext = ".zip")
  zip::zip(zip_path, files = list.files(tmp_dir, full.names = TRUE),
           mode = "cherry-pick")
  zip_path
}



# ── 11. Build leaflet map ──────────────────────────────────────────────────────
build_mapa_fusionado <- function(rv) {
  req(rv$fusionado)
  req(nrow(rv$fusionado) > 0)
  
  df <- rv$fusionado |>
    dplyr::filter(
      !is.na(lat), !is.na(lon),
      dplyr::between(lat, -90, 90),
      dplyr::between(lon, -180, 180)
    )
  
  req(nrow(df) > 0)
  
  lat_c <- mean(df$lat, na.rm = TRUE)
  lon_c <- mean(df$lon, na.rm = TRUE)
  
  df <- df |>
    dplyr::mutate(
      IDCode2 = ifelse(is.na(IDCode) | IDCode == "", "N/A", as.character(IDCode))
    )
  
  m <- leaflet::leaflet(options = leaflet::leafletOptions(zoomControl = TRUE)) |>
    leaflet::addProviderTiles(leaflet::providers$Esri.NatGeoWorldMap, group = "NatGeo") |>
    leaflet::addProviderTiles(leaflet::providers$Esri.WorldStreetMap, group = "Topography") |>
    leaflet::addProviderTiles(leaflet::providers$Esri.WorldImagery, group = "Satellite") |>
    leaflet::setView(lng = lon_c, lat = lat_c, zoom = 5)
  
  if (!is.null(rv$eoo) && !is.null(rv$eoo$hull) && nrow(rv$eoo$hull) > 0) {
    m <- m |> leaflet::addPolygons(
      data = rv$eoo$hull,
      fillColor = COL_EOO, fillOpacity = 0.15,
      color = COL_EOO, weight = 2.5, dashArray = "6,4", opacity = 1,
      group = "EOO",
      popup = paste0("<b style='color:", COL_EOO, "'>EOO — Convex Hull</b><br>",
                     format(rv$eoo$km2, big.mark = ","), " km²"),
      highlightOptions = leaflet::highlightOptions(weight = 4, fillOpacity = 0.3)
    )
  }
  
  if (!is.null(rv$aoo) && !is.null(rv$aoo$grid) && nrow(rv$aoo$grid) > 0) {
    m <- m |> leaflet::addPolygons(
      data = rv$aoo$grid,
      fillColor = COL_AOO, fillOpacity = 0.35,
      color = COL_AOO, weight = 1, opacity = 0.9,
      group = "AOO",
      popup = paste0("<b style='color:", COL_AOO, "'>AOO — 2 km Grid</b><br>",
                     format(rv$aoo$km2, big.mark = ","), " km²"),
      highlightOptions = leaflet::highlightOptions(weight = 2, fillOpacity = 0.6)
    )
  }
  
  gbif_only <- df |> dplyr::filter(fuente == "GBIF")
  if (nrow(gbif_only) > 0) {
    m <- m |> leaflet::addCircleMarkers(
      data = gbif_only, lng = ~lon, lat = ~lat,
      radius = 4, color = "#fff", fillColor = COL_GBIF,
      fillOpacity = 0.8, weight = 1.2,
      group = "GBIF only",
      popup = ~paste0("<b>GBIF</b><br>", scientificName)
    )
  }
  
  genbank_only <- df |> dplyr::filter(fuente == "GenBank" & unico_genbank == TRUE)
  if (nrow(genbank_only) > 0) {
    m <- m |> leaflet::addCircleMarkers(
      data = genbank_only, lng = ~lon, lat = ~lat,
      radius = 5, color = "#fff", fillColor = COL_GENBANK,
      fillOpacity = 0.85, weight = 1.2,
      group = "GenBank only",
      popup = ~paste0("<b style='color:", COL_GENBANK, "'>GenBank — GBIF Candidate</b><br>",
                      scientificName, "<br><small>Accession: ", IDCode2, "</small>")
    )
  }
  
  ambos <- df |> dplyr::filter(fuente == "GenBank" & en_gbif == TRUE)
  if (nrow(ambos) > 0) {
    m <- m |> leaflet::addCircleMarkers(
      data = ambos, lng = ~lon, lat = ~lat,
      radius = 5, color = "#fff", fillColor = COL_AMBOS,
      fillOpacity = 0.85, weight = 1.2,
      group = "Both sources",
      popup = ~paste0("<b style='color:", COL_AMBOS, "'>In GBIF and GenBank</b><br>",
                      scientificName, "<br><small>Accession: ", IDCode2, "</small>")
    )
  }
  
  m |>
    leaflet::addLayersControl(
      baseGroups = c("NatGeo", "Topography", "Satellite"),
      overlayGroups = c("EOO", "AOO", "GBIF only", "GenBank only", "Both sources"),
      options = leaflet::layersControlOptions(collapsed = FALSE)
    )
}





# ── 12. Complete HTML report ───────────────────────────────────────────────────────
generar_reporte_html <- function(rv) {
  especie   <- rv$especie   %||% "Species not specified"
  eoo_km2   <- rv$eoo$km2   %||% NA
  aoo_km2   <- rv$aoo$km2   %||% NA
  n_gbif    <- if (!is.null(rv$gbif))    nrow(rv$gbif)    else 0
  n_genbank <- if (!is.null(rv$genbank)) nrow(rv$genbank) else 0
  n_total   <- if (!is.null(rv$fusionado)) nrow(rv$fusionado) else 0
  score     <- rv$quality$score %||% 0
  taxon_key <- rv$taxon$taxon_key %||% "N/A"
  
  cats_eoo <- uicn_cat(eoo_km2, "eoo")
  cats_aoo <- uicn_cat(aoo_km2, "aoo")
  
  badge <- function(cat)
    paste0('<span style="background:', cat$color,
           ';color:#fff;padding:3px 12px;border-radius:20px;font-size:.8em;font-weight:700">',
           cat$label, '</span>')
  
  # Embedded leaflet map
  mapa_html <- ""
  if (!is.null(rv$fusionado) && nrow(rv$fusionado) > 0) {
    tryCatch({
      tmp_map <- tempfile(fileext = ".html")
      m <- build_mapa_fusionado(rv)
      htmlwidgets::saveWidget(m, tmp_map, selfcontained = TRUE)
      raw_map <- paste(readLines(tmp_map, warn = FALSE), collapse = "\n")
      mapa_html <- paste0('<div style="height:520px;border-radius:12px;overflow:hidden;',
                          'box-shadow:0 2px 18px rgba(0,0,0,.12);margin-top:10px;">',
                          '<iframe srcdoc="', gsub('"', '&quot;', raw_map),
                          '" style="width:100%;height:100%;border:none;"></iframe></div>')
    }, error = function(e) NULL)
  }
  
  # GBIF citation
  cita_gbif <- paste0("GBIF.org (", format(Sys.Date(), "%Y"), ") GBIF Occurrence Download.",
                      " https://doi.org/10.15468/dl.G² Bridge",
                      " [taxonKey=", taxon_key, "]",
                      " Accessed via G² Bridge .")
  
  paste0('<!DOCTYPE html>
<html lang="en">
<head>
<meta charset="UTF-8">
<meta name="viewport" content="width=device-width,initial-scale=1">
<title>Report: ', especie, '</title>
<link href="https://fonts.googleapis.com/css2?family=Poppins:wght@400;600;700&family=Inter:wght@400;500&display=swap" rel="stylesheet">
<style>
*,*::before,*::after{box-sizing:border-box;margin:0;padding:0}
body{font-family:"Inter",sans-serif;background:#f7f5f0;color:#1a1a2e}
header{background:linear-gradient(135deg,#484545,#535354);color:#fff;
       padding:28px 48px;border-bottom:4px solid #E07B39}
header h1{font-family:"Poppins",sans-serif;font-style:italic;font-size:2em;font-weight:600}
header p{opacity:.7;font-size:.88em;margin-top:6px}
.container{max-width:1100px;margin:0 auto;padding:28px 20px}
.sec{font-family:"Poppins",sans-serif;font-size:.75em;font-weight:700;
     letter-spacing:.1em;text-transform:uppercase;color:#E07B39;
     margin:28px 0 12px;padding-left:10px;border-left:4px solid #E07B39}
.metrics{display:grid;grid-template-columns:repeat(3,1fr);gap:14px;margin-bottom:20px}
.mc{background:#fff;border-radius:12px;padding:18px;text-align:center;
    box-shadow:0 2px 12px rgba(0,0,0,.08);border-top:4px solid var(--c)}
.mc .v{font-family:"Poppins",sans-serif;font-size:1.6em;font-weight:700;color:var(--c)}
.mc .l{font-size:.72em;color:#888;margin-top:4px;text-transform:uppercase;letter-spacing:.04em}
table{width:100%;border-collapse:collapse;background:#fff;border-radius:12px;
      overflow:hidden;box-shadow:0 2px 10px rgba(0,0,0,.07)}
th{background:linear-gradient(135deg,#484545,#535354);color:#fff;
   padding:10px 18px;text-align:left;font-size:.86em}
td{padding:9px 18px;border-bottom:1px solid #ece8e0;font-size:.9em}
tr:last-child td{border-bottom:none}
tr:hover td{background:#fdf8f3}
.uicn-row{display:flex;align-items:center;gap:10px;margin:8px 0;font-size:.9em}
.cite-box{background:#eaf4fb;border-left:4px solid #2980b9;border-radius:8px;
          padding:12px 16px;font-size:.8em;color:#333;font-family:monospace;
          word-break:break-all;margin-top:10px}
footer{text-align:center;font-size:.72em;color:#aaa;
       padding:20px;border-top:1px solid #e8e3da;margin-top:24px}
@media(max-width:600px){.metrics{grid-template-columns:repeat(2,1fr)}}
</style>
</head>
<body>
<header>
  <h1>', especie, '</h1>
  <p>G² Bridge  &mdash; GBIF G² Bridge Nielsen Challenge 2026 &mdash; Report generated on ',
         format(Sys.Date(), "%d/%m/%Y"), '</p>
</header>
<div class="container">

<div class="sec">Data sources queried</div>
<div class="metrics">
  <div class="mc" style="--c:', COL_GBIF, '">
    <div class="v">', n_gbif, '</div><div class="l">GBIF records</div></div>
  <div class="mc" style="--c:', COL_GENBANK, '">
    <div class="v">', n_genbank, '</div><div class="l">GenBank records</div></div>
  <div class="mc" style="--c:', COL_AMBOS, '">
    <div class="v">', n_total, '</div><div class="l">Merged dataset</div></div>
</div>

<div class="sec">Distribution metrics (EOO · AOO)</div>
<div class="metrics">
  <div class="mc" style="--c:', COL_EOO, '">
    <div class="v">', ifelse(is.na(eoo_km2), "N/A", paste(format(eoo_km2, big.mark=","), "km²")), '</div>
    <div class="l">EOO — Convex Hull</div></div>
  <div class="mc" style="--c:', COL_AOO, '">
    <div class="v">', ifelse(is.na(aoo_km2), "N/A", paste(format(aoo_km2, big.mark=","), "km²")), '</div>
    <div class="l">AOO — 2 km Grid</div></div>
  <div class="mc" style="--c:#8e44ad">
    <div class="v">', score, '</div><div class="l">DwC Quality Score</div></div>
</div>

<div class="sec">Indicative IUCN category (Criterion B)</div>
<div style="background:#fff;border-radius:12px;padding:18px 22px;box-shadow:0 2px 10px rgba(0,0,0,.07)">
  <div class="uicn-row">', badge(cats_eoo), ' B1-EOO: <b>', ifelse(is.na(eoo_km2),"N/A",paste(format(eoo_km2, big.mark=","), "km²")), '</b></div>
  <div class="uicn-row">', badge(cats_aoo), ' B2-AOO: <b>', ifelse(is.na(aoo_km2),"N/A",paste(format(aoo_km2, big.mark=","), "km²")), '</b></div>
  <p style="font-size:.78em;color:#999;margin-top:10px">
    Indicative assessment. Subcriteria b(i–iii) must be evaluated separately.</p>
</div>

<div class="sec">Interactive map</div>
', mapa_html, '

<div class="sec">GBIF citation (for scientific publications)</div>
<div class="cite-box">', cita_gbif, '</div>

</div>
<footer>G² Bridge  &bull; ', especie,
         ' &bull; Integrative Biology Laboratory &bull; GBIF Ebbe Nielsen Challenge 2026
</footer>
</body></html>')
}

shinyApp(ui, server)

