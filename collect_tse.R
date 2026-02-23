# collect_tse.R
# Download and filter campaign finance spending on fuel/gas stations (TSE)
# Source: https://dadosabertos.tse.jus.br/dataset/prestacao-de-contas-eleitorais

RAW_DIR <- "data/raw/tse"
FILTERED_DIR <- "data/filtered/tse"

dir.create(RAW_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(FILTERED_DIR, recursive = TRUE, showWarnings = FALSE)

# Election years with available campaign finance data
YEARS <- c(2014, 2016, 2018, 2020, 2022, 2024)

# Regex to match gas station / fuel supplier names
FUEL_PATTERN <- paste0(
  "posto|combusti|gasolina|petroleo|petróleo|petrobras|",
  "shell|ipiranga|ale\\b|raizen|raízen|",
  "br distribui|lubrificante|etanol|diesel|",
  "auto posto|rede combusti"
)

# TSE download URL pattern for campaign expenditures
# The TSE publishes data at dadosabertos.tse.jus.br
# Direct resource URLs follow this pattern:
base_url <- "https://cdn.tse.jus.br/estatistica/sead/odsele/prestacao_contas/"

for (year in YEARS) {
  zip_file <- file.path(RAW_DIR, paste0("despesas_candidatos_", year, ".zip"))

  # TSE file naming convention varies by year
  filename <- paste0("despesas_contratadas_candidatos_", year, ".zip")
  url <- paste0(base_url, filename)

  cat("Downloading TSE campaign expenses", year, "...\n")
  tryCatch({
    download.file(url, zip_file, mode = "wb", quiet = TRUE)
  }, error = function(e) {
    # Try alternate naming
    alt_filename <- paste0("prestacao_de_contas_eleitorais_candidatos_", year, ".zip")
    alt_url <- paste0(base_url, alt_filename)
    cat("  Trying alternate URL...\n")
    tryCatch({
      download.file(alt_url, zip_file, mode = "wb", quiet = TRUE)
    }, error = function(e2) {
      cat("  FAILED to download", year, ":", conditionMessage(e2), "\n")
    })
  })

  if (!file.exists(zip_file) || file.size(zip_file) < 100) {
    cat("  Skipping", year, "(file missing or too small)\n")
    next
  }

  csv_files <- unzip(zip_file, exdir = file.path(RAW_DIR, year))
  # Look for the expenditure file (despesas)
  expense_files <- csv_files[grepl("despesa", csv_files, ignore.case = TRUE)]
  if (length(expense_files) == 0) expense_files <- csv_files

  all_fuel <- data.frame()

  for (csv_file in expense_files) {
    cat("  Reading", basename(csv_file), "...\n")
    tryCatch({
      df <- read.csv(csv_file, sep = ";", fileEncoding = "latin1",
                     stringsAsFactors = FALSE, quote = "\"")

      # Find supplier name column
      supplier_col <- grep("fornecedor|NM_FORNECEDOR|nm_fornecedor",
                          names(df), ignore.case = TRUE, value = TRUE)
      if (length(supplier_col) == 0) {
        cat("    No supplier column found in", basename(csv_file), "\n")
        next
      }

      fuel <- df[grepl(FUEL_PATTERN, df[[supplier_col[1]]], ignore.case = TRUE), ]

      if (nrow(fuel) > 0) {
        all_fuel <- rbind(all_fuel, fuel)
      }
    }, error = function(e) {
      cat("    FAILED to process", basename(csv_file), ":", conditionMessage(e), "\n")
    })
  }

  if (nrow(all_fuel) > 0) {
    out_file <- file.path(FILTERED_DIR, paste0("tse_combustivel_", year, ".csv"))
    write.csv(all_fuel, out_file, row.names = FALSE, fileEncoding = "UTF-8")

    # Try to find the amount column
    val_col <- grep("valor|VR_DESPESA|vr_despesa", names(all_fuel),
                    ignore.case = TRUE, value = TRUE)
    if (length(val_col) > 0) {
      total <- sum(as.numeric(gsub("[^0-9,.-]", "", gsub(",", ".",
                   all_fuel[[val_col[1]]]))), na.rm = TRUE)
      cat("  ", year, ":", nrow(all_fuel), "fuel records, R$",
          format(total, big.mark = ".", decimal.mark = ","), "\n")
    } else {
      cat("  ", year, ":", nrow(all_fuel), "fuel records\n")
    }
  } else {
    cat("  ", year, ": no fuel records found\n")
  }
}

cat("\nDone. Filtered files in:", FILTERED_DIR, "\n")
