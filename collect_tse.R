# collect_tse.R
# Download and filter campaign finance data related to fuel/gas stations (TSE)
# Extracts both expenditures (despesas) and donation receipts (receitas)
# Source: https://dadosabertos.tse.jus.br/dataset/prestacao-de-contas-eleitorais

library(data.table)

# TSE files are large (up to 1.3 GB), increase download timeout
options(timeout = 1800)

RAW_DIR <- "data/raw/tse"
FILTERED_DIR <- "data/filtered/tse"

dir.create(RAW_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(FILTERED_DIR, recursive = TRUE, showWarnings = FALSE)

# Election years with available campaign finance data
# Municipal: 2004, 2008, 2012, 2016, 2020, 2024
# Federal/State: 2002, 2006, 2010, 2014, 2018, 2022
YEARS <- c(2002, 2004, 2006, 2008, 2010, 2012, 2014, 2016, 2018, 2020, 2022, 2024)

# Regex to match gas station / fuel supplier names
FUEL_PATTERN <- paste0(
  "posto|combusti|gasolina|petr.leo|petrobras|",
  "shell|ipiranga|ale\\b|ra.zen|",
  "br distribui|lubrificante|etanol|diesel|",
  "auto posto|rede combusti"
)

# TSE download URL pattern for campaign expenditures
# The TSE publishes data at dadosabertos.tse.jus.br
# URL patterns vary by year:
#   2018+: prestacao_de_contas_eleitorais_candidatos_{year}.zip
#   2016:  prestacao_contas_final_2016.zip
#   2012-2014: prestacao_final_{year}.zip or prestacao_contas_final_{year}.zip
#   2002-2010: prestacao_contas_{year}.zip
base_url <- "https://cdn.tse.jus.br/estatistica/sead/odsele/prestacao_contas/"

# Build candidate URLs for each year (try multiple patterns)
tse_urls <- function(year) {
  candidates <- c(
    paste0("prestacao_de_contas_eleitorais_candidatos_", year, ".zip"),
    paste0("despesas_contratadas_candidatos_", year, ".zip")
  )
  if (year == 2016) {
    candidates <- c(paste0("prestacao_contas_final_", year, ".zip"), candidates)
  } else if (year <= 2014) {
    candidates <- c(paste0("prestacao_contas_", year, ".zip"),
                    paste0("prestacao_final_", year, ".zip"),
                    paste0("prestacao_contas_final_", year, ".zip"),
                    candidates)
  }
  paste0(base_url, candidates)
}

for (year in YEARS) {
  zip_file <- file.path(RAW_DIR, paste0("despesas_candidatos_", year, ".zip"))

  # Skip download if file already exists and is large enough
  if (file.exists(zip_file) && file.size(zip_file) > 10000) {
    cat("Using cached TSE", year, "...\n")
  } else {
    cat("Downloading TSE campaign expenses", year, "...\n")
    downloaded <- FALSE
    for (url in tse_urls(year)) {
      tryCatch({
        download.file(url, zip_file, mode = "wb", quiet = TRUE, method = "curl")
        if (file.exists(zip_file) && file.size(zip_file) > 10000) {
          downloaded <- TRUE
          break
        }
      }, error = function(e) NULL)
    }
    if (!downloaded) {
      cat("  FAILED to download", year, "\n")
    }
  }

  if (!file.exists(zip_file) || file.size(zip_file) < 10000) {
    cat("  Skipping", year, "(file missing or too small)\n")
    next
  }

  # Unzip — use system unzip for robustness with accented paths (pre-2012 zips)
  # Exclude _brasil.txt files (2012+) which are multi-GB duplicates of per-state files
  extract_dir <- file.path(RAW_DIR, year)
  dir.create(extract_dir, recursive = TRUE, showWarnings = FALSE)
  system2("unzip", c("-o", "-q", zip_file, "-x", "*_brasil.*", "-d", extract_dir))
  csv_files <- list.files(extract_dir, pattern = "\\.(csv|txt)$",
                          recursive = TRUE, full.names = TRUE, ignore.case = TRUE)
  csv_files <- csv_files[!grepl("leiame|readme|leioute", csv_files, ignore.case = TRUE)]
  # Look for the expenditure file (despesas)
  expense_files <- csv_files[grepl("despesa", csv_files, ignore.case = TRUE)]
  if (length(expense_files) == 0) expense_files <- csv_files

  all_fuel <- list()

  for (csv_file in expense_files) {
    cat("  Reading", basename(csv_file), "...\n")
    tryCatch({
      dt <- fread(csv_file, sep = ";", encoding = "Latin-1")

      # Ensure character columns are properly encoded as UTF-8
      setnames(dt, iconv(names(dt), from = "latin1", to = "UTF-8"))
      chr_cols <- names(dt)[vapply(dt, is.character, logical(1))]
      for (col in chr_cols) {
        set(dt, j = col, value = iconv(dt[[col]], from = "latin1", to = "UTF-8"))
      }

      # Find supplier name column (must be the name, not CNPJ/code columns)
      # Handles multiple TSE formats:
      #   NM_FORNECEDOR (2008+), NOME_FORNECEDOR (2006),
      #   Nome do fornecedor (2014-2016), NO_FOR (2002-2004)
      supplier_col <- grep("^NM_FORNECEDOR$|^NOME_FORNECEDOR$|^Nome do fornecedor$|^NO_FOR$",
                          names(dt), ignore.case = TRUE, value = TRUE)
      if (length(supplier_col) == 0) {
        cat("    No supplier column found in", basename(csv_file), " cols:", paste(head(names(dt), 5), collapse=", "), "\n")
        next
      }

      fuel <- dt[grepl(FUEL_PATTERN, get(supplier_col[1]), ignore.case = TRUE)]

      if (nrow(fuel) > 0) {
        # Coerce all columns to character to avoid type conflicts across files
        for (col in names(fuel)) {
          set(fuel, j = col, value = as.character(fuel[[col]]))
        }
        all_fuel <- c(all_fuel, list(fuel))
      }
    }, error = function(e) {
      cat("    FAILED to process", basename(csv_file), ":", conditionMessage(e), "\n")
    })
  }

  if (length(all_fuel) > 0) {
    all_fuel <- rbindlist(all_fuel, fill = TRUE)
    out_file <- file.path(FILTERED_DIR, paste0("tse_combustivel_", year, ".csv"))
    fwrite(all_fuel, out_file)

    # Try to find the amount column
    val_col <- grep("valor|VR_DESPESA|vr_despesa", names(all_fuel),
                    ignore.case = TRUE, value = TRUE)
    if (length(val_col) > 0) {
      total <- all_fuel[, sum(as.numeric(gsub("[^0-9,.-]", "", gsub(",", ".",
                   get(val_col[1])))), na.rm = TRUE)]
      cat("  ", year, ":", nrow(all_fuel), "fuel records, R$",
          format(total, big.mark = ".", decimal.mark = ","), "\n")
    } else {
      cat("  ", year, ":", nrow(all_fuel), "fuel records\n")
    }
  } else {
    cat("  ", year, ": no fuel records found\n")
  }

  # --- Extract donation receipts (receitas) ---
  # Receitas are in the same extracted directory; filter for donors that are gas stations
  receipt_files <- csv_files[grepl("receita", csv_files, ignore.case = TRUE)]
  # Exclude doador_originario (traced-back donors) to avoid double-counting
  receipt_files <- receipt_files[!grepl("doador_originario", receipt_files, ignore.case = TRUE)]

  all_receipts <- list()

  for (csv_file in receipt_files) {
    cat("  Reading receipts:", basename(csv_file), "...\n")
    tryCatch({
      dt <- fread(csv_file, sep = ";", encoding = "Latin-1")

      setnames(dt, iconv(names(dt), from = "latin1", to = "UTF-8"))
      chr_cols <- names(dt)[vapply(dt, is.character, logical(1))]
      for (col in chr_cols) {
        set(dt, j = col, value = iconv(dt[[col]], from = "latin1", to = "UTF-8"))
      }

      # Find donor name column
      # Handles multiple TSE formats:
      #   NM_DOADOR (2008+), NOME_DOADOR (2006),
      #   Nome do doador (2014-2016), NO_DOADOR (2002-2004)
      donor_col <- grep("^NM_DOADOR$|^NOME_DOADOR$|^Nome do doador$|^NO_DOADOR$",
                        names(dt), ignore.case = TRUE, value = TRUE)
      if (length(donor_col) == 0) {
        cat("    No donor column found in", basename(csv_file), " cols:", paste(head(names(dt), 5), collapse=", "), "\n")
        next
      }

      fuel_donors <- dt[grepl(FUEL_PATTERN, get(donor_col[1]), ignore.case = TRUE)]

      if (nrow(fuel_donors) > 0) {
        # Coerce all columns to character to avoid type conflicts across files
        for (col in names(fuel_donors)) {
          set(fuel_donors, j = col, value = as.character(fuel_donors[[col]]))
        }
        all_receipts <- c(all_receipts, list(fuel_donors))
      }
    }, error = function(e) {
      cat("    FAILED:", basename(csv_file), "-", conditionMessage(e), "\n")
    })
  }

  if (length(all_receipts) > 0) {
    all_receipts <- rbindlist(all_receipts, fill = TRUE)
    out_file <- file.path(FILTERED_DIR, paste0("tse_receitas_combustivel_", year, ".csv"))
    fwrite(all_receipts, out_file)

    val_col <- grep("valor|VR_RECEITA|vr_receita", names(all_receipts),
                    ignore.case = TRUE, value = TRUE)
    if (length(val_col) > 0) {
      total <- all_receipts[, sum(as.numeric(gsub("[^0-9,.-]", "", gsub(",", ".",
                   get(val_col[1])))), na.rm = TRUE)]
      cat("   Receipts", year, ":", nrow(all_receipts), "fuel donor records, R$",
          format(total, big.mark = ".", decimal.mark = ","), "\n")
    } else {
      cat("   Receipts", year, ":", nrow(all_receipts), "fuel donor records\n")
    }
  } else {
    cat("   Receipts", year, ": no fuel donor records found\n")
  }
}

cat("\nDone. Filtered files in:", FILTERED_DIR, "\n")
