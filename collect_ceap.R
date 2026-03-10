# collect_ceap.R
# Download and filter federal deputies' spending on fuel (CEAP - Câmara dos Deputados)
# Source: https://www2.camara.leg.br/transparencia/cota-para-exercicio-da-atividade-parlamentar

library(data.table)

YEARS <- 2008:2025
RAW_DIR <- "data/raw/ceap"
FILTERED_DIR <- "data/filtered/ceap"

dir.create(RAW_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(FILTERED_DIR, recursive = TRUE, showWarnings = FALSE)

for (year in YEARS) {
  zip_file <- file.path(RAW_DIR, paste0("Ano-", year, ".csv.zip"))
  url <- paste0("https://www.camara.leg.br/cotas/Ano-", year, ".csv.zip")

  cat("Downloading CEAP", year, "...\n")
  tryCatch({
    download.file(url, zip_file, mode = "wb", quiet = TRUE)
  }, error = function(e) {
    cat("  FAILED to download", year, ":", conditionMessage(e), "\n")
    return(NULL)
  })

  if (!file.exists(zip_file)) next

  csv_files <- unzip(zip_file, exdir = RAW_DIR)
  if (length(csv_files) == 0) {
    cat("  No CSV found in zip for", year, "\n")
    next
  }

  cat("  Reading", csv_files[1], "...\n")
  tryCatch({
    dt <- fread(csv_files[1], sep = ";", encoding = "UTF-8")

    fuel <- dt[grepl("COMBUST", txtDescricao, ignore.case = TRUE)]

    out_file <- file.path(FILTERED_DIR, paste0("ceap_combustivel_", year, ".csv"))
    fwrite(fuel, out_file)

    total <- fuel[, sum(as.numeric(gsub(",", ".", vlrLiquido)), na.rm = TRUE)]
    cat("  ", year, ":", nrow(fuel), "fuel records, R$",
        format(total, big.mark = ".", decimal.mark = ","), "\n")
  }, error = function(e) {
    cat("  FAILED to process", year, ":", conditionMessage(e), "\n")
  })
}

cat("\nDone. Filtered files in:", FILTERED_DIR, "\n")
