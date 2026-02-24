# collect_candidatos.R
# Download TSE candidate registry (consulta_cand) for all election years
# Source: https://dadosabertos.tse.jus.br/dataset/candidatos

library(data.table)

options(timeout = 600)

RAW_DIR <- "data/raw/tse_candidatos"
FILTERED_DIR <- "data/filtered/tse_candidatos"

dir.create(RAW_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(FILTERED_DIR, recursive = TRUE, showWarnings = FALSE)

# Election years (federal + municipal)
YEARS <- c(2002, 2004, 2006, 2008, 2010, 2012, 2014, 2016, 2018, 2020, 2022, 2024)

base_url <- "https://cdn.tse.jus.br/estatistica/sead/odsele/consulta_cand/"

for (year in YEARS) {
  zip_file <- file.path(RAW_DIR, paste0("consulta_cand_", year, ".zip"))
  url <- paste0(base_url, "consulta_cand_", year, ".zip")

  if (file.exists(zip_file) && file.size(zip_file) > 10000) {
    cat("Using cached candidate data", year, "...\n")
  } else {
    cat("Downloading candidate registry", year, "...\n")
    tryCatch({
      download.file(url, zip_file, mode = "wb", quiet = TRUE, method = "curl")
    }, error = function(e) {
      cat("  FAILED:", conditionMessage(e), "\n")
    })
  }

  if (!file.exists(zip_file) || file.size(zip_file) < 10000) {
    cat("  Skipping", year, "\n")
    next
  }

  extract_dir <- file.path(RAW_DIR, year)
  csv_files <- unzip(zip_file, exdir = extract_dir)
  cand_files <- csv_files[grepl("consulta_cand_.*\\.(csv|txt)$", csv_files, ignore.case = TRUE)]
  # Exclude readme/leiame
  cand_files <- cand_files[!grepl("leiame|readme", cand_files, ignore.case = TRUE)]

  all_cands <- list()

  for (cf in cand_files) {
    tryCatch({
      dt <- fread(cf, sep = ";", encoding = "Latin-1")

      # Convert encoding
      setnames(dt, iconv(names(dt), from = "latin1", to = "UTF-8"))
      chr_cols <- names(dt)[vapply(dt, is.character, logical(1))]
      for (col in chr_cols) {
        set(dt, j = col, value = iconv(dt[[col]], from = "latin1", to = "UTF-8"))
      }

      # Coerce all columns to character to avoid type conflicts across UF files
      for (col in names(dt)) {
        set(dt, j = col, value = as.character(dt[[col]]))
      }

      all_cands <- c(all_cands, list(dt))
    }, error = function(e) {
      cat("  Failed:", basename(cf), "-", conditionMessage(e), "\n")
    })
  }

  if (length(all_cands) > 0) {
    cands <- rbindlist(all_cands, fill = TRUE)
    out_file <- file.path(FILTERED_DIR, paste0("candidatos_", year, ".csv"))
    fwrite(cands, out_file)
    cat("  ", year, ":", nrow(cands), "candidates\n")
  }
}

cat("\nDone. Files in:", FILTERED_DIR, "\n")
