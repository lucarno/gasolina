# collect_states.R
# Download and filter state deputies' spending on fuel from Assembleias Legislativas
# Covers all 27 Brazilian federative units, tiered by data accessibility

library(data.table)

RAW_DIR <- "data/raw/states"
FILTERED_DIR <- "data/filtered/states"

dir.create(RAW_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(FILTERED_DIR, recursive = TRUE, showWarnings = FALSE)

FUEL_PATTERN <- paste0(
  "combust|gasolina|petroleo|petróleo|posto|etanol|diesel|",
  "lubrificante|abastecimento"
)

# Helper: download a file, return TRUE on success
safe_download <- function(url, dest) {
  tryCatch({
    download.file(url, dest, mode = "wb", quiet = TRUE)
    file.exists(dest) && file.size(dest) > 100
  }, error = function(e) {
    cat("    Download failed:", conditionMessage(e), "\n")
    FALSE
  })
}

# Helper: read CSV with multiple encoding/separator attempts
safe_read_csv <- function(path, seps = c(";", ",", "|", "\t"),
                          encodings = c("Latin-1", "UTF-8")) {
  for (enc in encodings) {
    for (sep in seps) {
      result <- tryCatch({
        dt <- fread(path, sep = sep, encoding = enc)
        if (ncol(dt) > 1) return(dt)
        NULL
      }, error = function(e) NULL)
      if (!is.null(result)) return(result)
    }
  }
  NULL
}

# Helper: filter a data.table for fuel-related rows
filter_fuel <- function(dt) {
  # First try to find an expense type/category column
  type_col <- grep("tipo.*despesa|descricao.*despesa|categoria|natureza|rubrica|subelem",
                   names(dt), ignore.case = TRUE, value = TRUE)

  if (length(type_col) > 0) {
    mask <- grepl(FUEL_PATTERN, dt[[type_col[1]]], ignore.case = TRUE)
    if (sum(mask) > 0) return(dt[mask])
  }

  # Fall back: search all text columns for fuel keywords
  text_cols <- names(dt)[sapply(dt, is.character)]
  mask <- rep(FALSE, nrow(dt))
  for (col in text_cols) {
    mask <- mask | grepl(FUEL_PATTERN, dt[[col]], ignore.case = TRUE)
  }
  dt[mask]
}

# Helper: process and save filtered data for a state
save_state <- function(dt, uf, label = "") {
  fuel <- filter_fuel(dt)
  state_dir <- file.path(FILTERED_DIR, uf)
  dir.create(state_dir, recursive = TRUE, showWarnings = FALSE)

  suffix <- if (nchar(label) > 0) paste0("_", label) else ""
  out_file <- file.path(state_dir, paste0(tolower(uf), "_combustivel", suffix, ".csv"))
  fwrite(fuel, out_file)
  cat("    ", uf, label, ":", nrow(fuel), "fuel records\n")
  nrow(fuel)
}

# ============================================================================
# TIER 1: States with confirmed programmatic CSV/API downloads
# ============================================================================

cat("=== TIER 1: Confirmed programmatic downloads ===\n\n")

# --- SC (Santa Catarina - ALESC) ---
cat("SC (ALESC)...\n")
sc_dir <- file.path(RAW_DIR, "SC")
dir.create(sc_dir, recursive = TRUE, showWarnings = FALSE)
for (year in 2019:2025) {
  url <- paste0("https://transparencia.alesc.sc.gov.br/gabinetes_csv.php?ano=", year)
  dest <- file.path(sc_dir, paste0("sc_gabinetes_", year, ".csv"))
  cat("  Downloading", year, "...\n")
  if (safe_download(url, dest)) {
    df <- safe_read_csv(dest)
    if (!is.null(df)) save_state(df, "SC", as.character(year))
  }
}

# --- SP (São Paulo - ALESP) ---
cat("\nSP (ALESP)...\n")
sp_dir <- file.path(RAW_DIR, "SP")
dir.create(sp_dir, recursive = TRUE, showWarnings = FALSE)
# ALESP publishes data at their open data portal
# Files are ZIP archives with pipe-separated .txt files
for (year in 2010:2025) {
  url <- paste0("https://www.al.sp.gov.br/repositorioDados/deputados/despesas_gabinetes_",
                year, ".zip")
  dest <- file.path(sp_dir, paste0("sp_despesas_", year, ".zip"))
  cat("  Downloading", year, "...\n")
  if (safe_download(url, dest)) {
    csv_files <- tryCatch(unzip(dest, exdir = sp_dir), error = function(e) character(0))
    for (f in csv_files) {
      df <- safe_read_csv(f)
      if (!is.null(df)) save_state(df, "SP", as.character(year))
    }
  }
}

# --- MG (Minas Gerais - ALMG) ---
cat("\nMG (ALMG)...\n")
mg_dir <- file.path(RAW_DIR, "MG")
dir.create(mg_dir, recursive = TRUE, showWarnings = FALSE)
# ALMG has an API at dadosabertos.almg.gov.br
# Verbas indenizatórias endpoint
for (year in 2019:2025) {
  for (month in 1:12) {
    url <- paste0("https://dadosabertos.almg.gov.br/ws/prestacao_contas/",
                  "verbas_indenizatorias/deputados/", year, "/", month, "?formato=json")
    dest <- file.path(mg_dir, paste0("mg_verbas_", year, "_", sprintf("%02d", month), ".json"))
    if (safe_download(url, dest)) {
      tryCatch({
        json_text <- readLines(dest, warn = FALSE, encoding = "UTF-8")
        json_data <- jsonlite::fromJSON(paste(json_text, collapse = ""))
        if (is.data.frame(json_data) || is.list(json_data)) {
          # Flatten if needed
          if (!is.data.frame(json_data)) {
            for (name in names(json_data)) {
              if (is.data.frame(json_data[[name]])) {
                json_data <- json_data[[name]]
                break
              }
            }
          }
          if (is.data.frame(json_data) && nrow(json_data) > 0) {
            save_state(as.data.table(json_data), "MG", paste0(year, "_", sprintf("%02d", month)))
          }
        }
      }, error = function(e) {
        cat("    Failed to parse JSON for MG", year, month, "\n")
      })
    }
  }
}

# --- DF (Distrito Federal - CLDF) ---
cat("\nDF (CLDF)...\n")
df_dir <- file.path(RAW_DIR, "DF")
dir.create(df_dir, recursive = TRUE, showWarnings = FALSE)
# CLDF open data portal: dados.cl.df.gov.br
url <- "https://dados.cl.df.gov.br/dataset/verbas-indenizatorias"
dest <- file.path(df_dir, "cldf_page.html")
cat("  Fetching CLDF dataset page...\n")
if (safe_download(url, dest)) {
  # Parse page to find CSV download links
  page <- readLines(dest, warn = FALSE, encoding = "UTF-8")
  csv_links <- regmatches(page, gregexpr('href="[^"]*\\.csv[^"]*"', page))
  csv_links <- unique(unlist(csv_links))
  csv_links <- gsub('^href="|"$', '', csv_links)
  for (i in seq_along(csv_links)) {
    link <- csv_links[i]
    if (!grepl("^http", link)) link <- paste0("https://dados.cl.df.gov.br", link)
    csv_dest <- file.path(df_dir, paste0("cldf_verbas_", i, ".csv"))
    if (safe_download(link, csv_dest)) {
      df <- safe_read_csv(csv_dest)
      if (!is.null(df)) save_state(df, "DF", as.character(i))
    }
  }
}

# --- PE (Pernambuco - ALEPE) ---
cat("\nPE (ALEPE)...\n")
pe_dir <- file.path(RAW_DIR, "PE")
dir.create(pe_dir, recursive = TRUE, showWarnings = FALSE)
# ALEPE open data API: dadosabertos.alepe.pe.gov.br
for (year in 2019:2025) {
  url <- paste0("https://dadosabertos.alepe.pe.gov.br/api/verbas_indenizatorias/",
                year, "?formato=csv")
  dest <- file.path(pe_dir, paste0("pe_verbas_", year, ".csv"))
  cat("  Downloading", year, "...\n")
  if (safe_download(url, dest)) {
    df <- safe_read_csv(dest)
    if (!is.null(df)) save_state(df, "PE", as.character(year))
  }
}

# --- CE (Ceará - ALCE) ---
cat("\nCE (ALCE)...\n")
ce_dir <- file.path(RAW_DIR, "CE")
dir.create(ce_dir, recursive = TRUE, showWarnings = FALSE)
for (year in 2019:2025) {
  url <- paste0("https://www2.al.ce.gov.br/api/despesas/verbas_indenizatorias/",
                year, "?formato=csv")
  dest <- file.path(ce_dir, paste0("ce_verbas_", year, ".csv"))
  cat("  Downloading", year, "...\n")
  if (safe_download(url, dest)) {
    df <- safe_read_csv(dest)
    if (!is.null(df)) save_state(df, "CE", as.character(year))
  }
}

# --- RS (Rio Grande do Sul - ALERS) ---
cat("\nRS (ALERS)...\n")
rs_dir <- file.path(RAW_DIR, "RS")
dir.create(rs_dir, recursive = TRUE, showWarnings = FALSE)
for (year in 2019:2025) {
  url <- paste0("http://www2.al.rs.gov.br/transparenciaalrs/DadosAbertos/",
                "Despesas_", year, ".csv")
  dest <- file.path(rs_dir, paste0("rs_despesas_", year, ".csv"))
  cat("  Downloading", year, "...\n")
  if (safe_download(url, dest)) {
    df <- safe_read_csv(dest)
    if (!is.null(df)) save_state(df, "RS", as.character(year))
  }
}

# --- PI (Piauí - ALEPI) ---
cat("\nPI (ALEPI)...\n")
pi_dir <- file.path(RAW_DIR, "PI")
dir.create(pi_dir, recursive = TRUE, showWarnings = FALSE)
url <- "https://www.al.pi.leg.br/transparencia-menu/dados-abertos"
dest <- file.path(pi_dir, "alepi_page.html")
cat("  Fetching ALEPI open data page...\n")
if (safe_download(url, dest)) {
  page <- readLines(dest, warn = FALSE, encoding = "UTF-8")
  csv_links <- regmatches(page, gregexpr('href="[^"]*\\.(csv|zip)[^"]*"', page))
  csv_links <- unique(unlist(csv_links))
  csv_links <- gsub('^href="|"$', '', csv_links)
  for (i in seq_along(csv_links)) {
    link <- csv_links[i]
    if (!grepl("^http", link)) link <- paste0("https://www.al.pi.leg.br", link)
    ext <- tools::file_ext(link)
    f_dest <- file.path(pi_dir, paste0("pi_dados_", i, ".", ext))
    if (safe_download(link, f_dest)) {
      if (ext == "zip") {
        extracted <- tryCatch(unzip(f_dest, exdir = pi_dir), error = function(e) character(0))
        for (f in extracted) {
          df <- safe_read_csv(f)
          if (!is.null(df)) save_state(df, "PI", as.character(i))
        }
      } else {
        df <- safe_read_csv(f_dest)
        if (!is.null(df)) save_state(df, "PI", as.character(i))
      }
    }
  }
}

# --- RN (Rio Grande do Norte - ALERN) ---
cat("\nRN (ALERN)...\n")
rn_dir <- file.path(RAW_DIR, "RN")
dir.create(rn_dir, recursive = TRUE, showWarnings = FALSE)
url <- "https://transparencia.al.rn.leg.br"
dest <- file.path(rn_dir, "alern_page.html")
cat("  Fetching ALERN transparency page...\n")
if (safe_download(url, dest)) {
  page <- readLines(dest, warn = FALSE, encoding = "UTF-8")
  csv_links <- regmatches(page, gregexpr('href="[^"]*\\.(csv|xlsx?|zip)[^"]*"', page))
  csv_links <- unique(unlist(csv_links))
  csv_links <- gsub('^href="|"$', '', csv_links)
  for (i in seq_along(csv_links)) {
    link <- csv_links[i]
    if (!grepl("^http", link)) link <- paste0("https://transparencia.al.rn.leg.br", link)
    ext <- tools::file_ext(link)
    f_dest <- file.path(rn_dir, paste0("rn_dados_", i, ".", ext))
    if (safe_download(link, f_dest)) {
      if (ext == "csv") {
        df <- safe_read_csv(f_dest)
        if (!is.null(df)) save_state(df, "RN", as.character(i))
      }
    }
  }
}

# ============================================================================
# TIER 2: Transparency portals - attempt programmatic download
# ============================================================================

cat("\n=== TIER 2: Attempting transparency portal downloads ===\n\n")

tier2_states <- list(
  AC = list(name = "Acre (ALEAC)", url = "https://app.al.ac.leg.br"),
  AL = list(name = "Alagoas (ALE-AL)", url = "https://www.al.al.leg.br/transparencia"),
  AM = list(name = "Amazonas (ALEAM)", url = "https://www.aleam.gov.br/transparencia/despesa/"),
  ES = list(name = "Espírito Santo (ALES)", url = "https://www.al.es.gov.br/Transparencia"),
  GO = list(name = "Goiás (ALEGO)", url = "https://transparencia.al.go.leg.br"),
  MT = list(name = "Mato Grosso (ALMT)", url = "https://www.al.mt.gov.br/transparencia/"),
  PR = list(name = "Paraná (ALEP)", url = "https://transparencia.assembleia.pr.leg.br"),
  PA = list(name = "Pará (ALEPA)",
            url = "https://www.alepa.pa.gov.br/Home/Page/PORTALDATRANSPARENCIA"),
  RO = list(name = "Rondônia (ALE-RO)", url = "https://transparencia.al.ro.leg.br"),
  MS = list(name = "Mato Grosso do Sul (ALEMS)",
            url = "https://www.transparencia.al.ms.gov.br/pages/"),
  AP = list(name = "Amapá (ALAP)", url = "https://al.ap.leg.br/transparencia/")
)

for (uf in names(tier2_states)) {
  state <- tier2_states[[uf]]
  cat(uf, "(", state$name, ")...\n")

  state_raw_dir <- file.path(RAW_DIR, uf)
  dir.create(state_raw_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(FILTERED_DIR, uf), recursive = TRUE, showWarnings = FALSE)

  # Try to fetch the transparency page and extract download links
  dest <- file.path(state_raw_dir, paste0(tolower(uf), "_page.html"))
  if (safe_download(state$url, dest)) {
    page <- tryCatch(readLines(dest, warn = FALSE, encoding = "UTF-8"),
                     error = function(e) character(0))
    # Look for CSV/XLS/ZIP download links
    csv_links <- regmatches(page, gregexpr('href="[^"]*\\.(csv|xlsx?|zip|json)[^"]*"', page))
    csv_links <- unique(unlist(csv_links))
    csv_links <- gsub('^href="|"$', '', csv_links)

    if (length(csv_links) > 0) {
      cat("  Found", length(csv_links), "downloadable files\n")
      for (i in seq_along(csv_links[1:min(20, length(csv_links))])) {
        link <- csv_links[i]
        if (!grepl("^http", link)) {
          base <- gsub("(https?://[^/]+).*", "\\1", state$url)
          link <- paste0(base, link)
        }
        ext <- tools::file_ext(gsub("\\?.*", "", link))
        f_dest <- file.path(state_raw_dir, paste0(tolower(uf), "_", i, ".", ext))
        if (safe_download(link, f_dest)) {
          if (ext %in% c("csv", "txt")) {
            df <- safe_read_csv(f_dest)
            if (!is.null(df)) save_state(df, uf, as.character(i))
          } else if (ext == "zip") {
            extracted <- tryCatch(unzip(f_dest, exdir = state_raw_dir),
                                 error = function(e) character(0))
            for (f in extracted) {
              df <- safe_read_csv(f)
              if (!is.null(df)) save_state(df, uf, paste0(i, "_", basename(f)))
            }
          }
        }
      }
    } else {
      cat("  No downloadable data files found on page\n")
    }
  } else {
    cat("  Could not access transparency portal\n")
  }
}

# ============================================================================
# TIER 3: Portal-only states (URLs documented for manual collection)
# ============================================================================

cat("\n=== TIER 3: Portal-only states (URLs for manual access) ===\n")
cat("These states need manual data collection:\n\n")

tier3_states <- list(
  BA = "https://www.al.ba.gov.br/transparencia",
  MA = "https://sistemas.al.ma.leg.br/transparencia/",
  PB = "https://www.al.pb.leg.br/transparencia",
  RJ = "https://transparencia.alerj.rj.gov.br",
  RR = "https://transparencia.al.rr.leg.br",
  SE = "https://al.se.leg.br/portal-da-transparencia/",
  TO = "https://www.al.to.leg.br/transparencia"
)

for (uf in names(tier3_states)) {
  cat("  ", uf, ":", tier3_states[[uf]], "\n")
}

cat("\nDone. Filtered files in:", FILTERED_DIR, "\n")
