# collect_anp.R
# Download ANP fuel volume data (municipality-level) and station price surveys
# Source: https://www.gov.br/anp/pt-br/centrais-de-conteudo/dados-abertos

library(data.table)

options(timeout = 600)

RAW_DIR <- "data/raw/anp"
FILTERED_DIR <- "data/filtered/anp"

dir.create(RAW_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(FILTERED_DIR, recursive = TRUE, showWarnings = FALSE)

# ============================================================
# 1. Municipality-level annual fuel sales volumes
# ============================================================
cat("=== ANP Municipality Fuel Volumes ===\n")

volume_urls <- list(
  gasolina = "https://www.gov.br/anp/pt-br/centrais-de-conteudo/dados-abertos/arquivos/vdpb/vaehdpm/gasolina-c/vendas-anuais-de-gasolina-c-por-municipio.csv",
  etanol   = "https://www.gov.br/anp/pt-br/centrais-de-conteudo/dados-abertos/arquivos/vdpb/vaehdpm/etanol-hidratado/vendas-anuais-de-etanol-hidratado-por-municipio.csv",
  diesel   = "https://www.gov.br/anp/pt-br/centrais-de-conteudo/dados-abertos/arquivos/vdpb/vaehdpm/oleo-diesel/vendas-anuais-de-oleo-diesel-por-municipio.csv"
)

for (fuel in names(volume_urls)) {
  dest <- file.path(RAW_DIR, paste0("vendas_", fuel, "_municipio.csv"))
  cat("Downloading", fuel, "volumes...\n")
  tryCatch({
    download.file(volume_urls[[fuel]], dest, mode = "wb", quiet = TRUE, method = "curl")
    dt <- fread(dest, sep = ";", encoding = "UTF-8")
    cat("  ", fuel, ":", nrow(dt), "rows,", uniqueN(dt$ANO), "years\n")
    cat("  Columns:", paste(names(dt), collapse = ", "), "\n")
  }, error = function(e) {
    cat("  FAILED:", conditionMessage(e), "\n")
  })
}

# ============================================================
# 2. Station-level price surveys (semiannual, 2004-2025)
#    Contains CNPJ of each station — needed to count stations per municipality
# ============================================================
cat("\n=== ANP Station Price Surveys ===\n")

price_dir <- file.path(RAW_DIR, "prices")
dir.create(price_dir, recursive = TRUE, showWarnings = FALSE)

# Download semiannual price files (2004-2025)
for (year in 2004:2025) {
  for (half in c("01", "02")) {
    fname <- paste0("ca-", year, "-", half, ".zip")
    dest <- file.path(price_dir, fname)
    url <- paste0("https://www.gov.br/anp/pt-br/centrais-de-conteudo/dados-abertos/arquivos/shpc/dsas/ca/", fname)

    if (file.exists(dest) && file.size(dest) > 1000) {
      cat("  Cached:", fname, "\n")
      next
    }

    cat("  Downloading", fname, "...\n")
    tryCatch({
      download.file(url, dest, mode = "wb", quiet = TRUE, method = "curl")
    }, error = function(e) {
      cat("    FAILED:", conditionMessage(e), "\n")
    })
  }
}

# ============================================================
# 3. Extract station registry from price surveys
#    (unique CNPJ + municipality + bandeira per station)
# ============================================================
cat("\n=== Building station registry from price surveys ===\n")

station_list <- list()
zip_files <- list.files(price_dir, pattern = "\\.zip$", full.names = TRUE)

for (zf in zip_files) {
  if (file.size(zf) < 1000) next
  tryCatch({
    # List files in zip, then extract with renamed path if it contains non-ASCII
    file_list <- unzip(zf, list = TRUE)$Name
    extract_dir <- file.path(price_dir, gsub("\\.zip$", "", basename(zf)))
    dir.create(extract_dir, recursive = TRUE, showWarnings = FALSE)
    csv_files <- unzip(zf, exdir = extract_dir)
    for (cf in csv_files) {
      if (!grepl("\\.(csv|txt)$", cf, ignore.case = TRUE)) next
      dt <- fread(cf, sep = ";", encoding = "UTF-8", select = c(
        "CNPJ da Revenda", "Revenda", "Municipio",
        "Estado - Sigla", "Bandeira", "Data da Coleta"
      ))
      # Coerce all to character to avoid type conflicts
      for (col in names(dt)) set(dt, j = col, value = as.character(dt[[col]]))
      # Keep one row per station per semester (deduplicate)
      dt <- unique(dt, by = "CNPJ da Revenda")
      station_list <- c(station_list, list(dt))
      file.remove(cf)
    }
    unlink(extract_dir, recursive = TRUE)
  }, error = function(e) {
    cat("    Failed to parse", basename(zf), ":", conditionMessage(e), "\n")
  })
}

if (length(station_list) > 0) {
  stations <- rbindlist(station_list, fill = TRUE)
  # Deduplicate: keep most recent record per station
  setnames(stations, c("CNPJ da Revenda", "Estado - Sigla", "Data da Coleta"),
           c("cnpj_revenda", "uf", "data_coleta"))
  stations <- unique(stations, by = "cnpj_revenda", fromLast = TRUE)

  out_file <- file.path(FILTERED_DIR, "anp_stations.csv")
  fwrite(stations, out_file)
  cat("Station registry:", nrow(stations), "unique stations\n")
} else {
  cat("No station data extracted\n")
}

# ============================================================
# 4. Monthly sales by state (for time-series analysis)
# ============================================================
cat("\n=== ANP Monthly State Volumes ===\n")

monthly_url <- "https://www.gov.br/anp/pt-br/centrais-de-conteudo/dados-abertos/arquivos/vdpb/vendas-derivados-petroleo-e-etanol/vendas-combustiveis-m3-1990-2025.csv"
dest <- file.path(RAW_DIR, "vendas_mensais_uf.csv")

cat("Downloading monthly state volumes...\n")
tryCatch({
  download.file(monthly_url, dest, mode = "wb", quiet = TRUE, method = "curl")
  dt <- fread(dest, sep = ";", encoding = "UTF-8")
  cat("  Monthly state data:", nrow(dt), "rows\n")
}, error = function(e) {
  cat("  FAILED:", conditionMessage(e), "\n")
})

cat("\nDone. Files in:", RAW_DIR, "and", FILTERED_DIR, "\n")
