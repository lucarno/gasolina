# collect_ceaps.R
# Download and filter federal senators' spending on fuel (CEAPS - Senado Federal)
# Source: https://www12.senado.leg.br/transparencia/dados-abertos-transparencia/dados-abertos-ceaps

RAW_DIR <- "data/raw/ceaps"
FILTERED_DIR <- "data/filtered/ceaps"

dir.create(RAW_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(FILTERED_DIR, recursive = TRUE, showWarnings = FALSE)

# The Senado publishes CEAPS data as CSV files per year
# URL pattern: https://www.senado.leg.br/transparencia/LAI/verba/{year}.csv
# Also available at: https://www12.senado.leg.br/transparencia/dados-abertos-transparencia/dados-abertos-ceaps
YEARS <- 2008:2025

for (year in YEARS) {
  csv_file <- file.path(RAW_DIR, paste0("ceaps_", year, ".csv"))
  url <- paste0("https://www.senado.leg.br/transparencia/LAI/verba/", year, ".csv")

  cat("Downloading CEAPS", year, "...\n")
  tryCatch({
    download.file(url, csv_file, mode = "wb", quiet = TRUE)
  }, error = function(e) {
    cat("  FAILED to download", year, ":", conditionMessage(e), "\n")
    return(NULL)
  })

  if (!file.exists(csv_file) || file.size(csv_file) < 100) {
    cat("  Skipping", year, "(file missing or too small)\n")
    next
  }

  cat("  Reading", csv_file, "...\n")
  tryCatch({
    df <- read.csv(csv_file, sep = ";", fileEncoding = "latin1",
                   stringsAsFactors = FALSE, quote = "\"")

    # Find the expense type column (varies: TIPO_DESPESA, tipo_despesa, etc.)
    type_col <- grep("tipo.*despesa|despesa.*tipo|TIPO", names(df),
                     ignore.case = TRUE, value = TRUE)
    if (length(type_col) == 0) {
      # Try broader search on all columns for fuel keywords
      cat("  No expense type column found, searching all text columns...\n")
      text_cols <- names(df)[sapply(df, is.character)]
      fuel_mask <- rep(FALSE, nrow(df))
      for (col in text_cols) {
        fuel_mask <- fuel_mask | grepl("combust", df[[col]], ignore.case = TRUE)
      }
      fuel <- df[fuel_mask, ]
    } else {
      fuel <- df[grepl("combust", df[[type_col[1]]], ignore.case = TRUE), ]
    }

    out_file <- file.path(FILTERED_DIR, paste0("ceaps_combustivel_", year, ".csv"))
    write.csv(fuel, out_file, row.names = FALSE, fileEncoding = "UTF-8")

    # Try to find the amount column
    val_col <- grep("valor|vlr|vl_", names(fuel), ignore.case = TRUE, value = TRUE)
    if (length(val_col) > 0) {
      total <- sum(as.numeric(gsub("[^0-9,.-]", "", gsub(",", ".",
                   fuel[[val_col[1]]]))), na.rm = TRUE)
      cat("  ", year, ":", nrow(fuel), "fuel records, R$",
          format(total, big.mark = ".", decimal.mark = ","), "\n")
    } else {
      cat("  ", year, ":", nrow(fuel), "fuel records\n")
    }
  }, error = function(e) {
    cat("  FAILED to process", year, ":", conditionMessage(e), "\n")
  })
}

cat("\nDone. Filtered files in:", FILTERED_DIR, "\n")
