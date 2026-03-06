# validate_urls.R
# Smoke-test all data source URLs with HEAD requests to identify which ones actually work.
# Usage: Rscript validate_urls.R

# Build list of all URLs used across collectors
urls <- list()

# --- collect_ceap.R: Câmara dos Deputados ---
for (year in 2008:2025) {
  urls[[length(urls) + 1]] <- list(
    source = "ceap",
    label = paste("Câmara CEAP", year),
    url = paste0("https://www.camara.leg.br/cotas/Ano-", year, ".csv.zip")
  )
}

# --- collect_ceaps.R: Senado Federal ---
for (year in 2008:2025) {
  urls[[length(urls) + 1]] <- list(
    source = "ceaps",
    label = paste("Senado CEAPS", year),
    url = paste0("https://www.senado.leg.br/transparencia/LAI/verba/", year, ".csv")
  )
}

# --- collect_tse.R: Campaign finance ---
base_url <- "https://cdn.tse.jus.br/estatistica/sead/odsele/prestacao_contas/"
for (year in c(2014, 2016, 2018, 2020, 2022, 2024)) {
  urls[[length(urls) + 1]] <- list(
    source = "tse",
    label = paste("TSE despesas_contratadas", year),
    url = paste0(base_url, "despesas_contratadas_candidatos_", year, ".zip")
  )
  urls[[length(urls) + 1]] <- list(
    source = "tse",
    label = paste("TSE prestacao_contas (alt)", year),
    url = paste0(base_url, "prestacao_de_contas_eleitorais_candidatos_", year, ".zip")
  )
}

# --- collect_states.R: Tier 1 ---
# SC
for (year in 2019:2025) {
  urls[[length(urls) + 1]] <- list(
    source = "states/SC",
    label = paste("SC ALESC", year),
    url = paste0("https://transparencia.alesc.sc.gov.br/gabinetes_csv.php?ano=", year)
  )
}
# SP
for (year in 2010:2025) {
  urls[[length(urls) + 1]] <- list(
    source = "states/SP",
    label = paste("SP ALESP", year),
    url = paste0("https://www.al.sp.gov.br/repositorioDados/deputados/despesas_gabinetes_",
                 year, ".zip")
  )
}
# MG (sample a few months instead of all 12*7)
for (year in c(2019, 2022, 2025)) {
  for (month in c(1, 6)) {
    urls[[length(urls) + 1]] <- list(
      source = "states/MG",
      label = paste("MG ALMG", year, month),
      url = paste0("https://dadosabertos.almg.gov.br/ws/prestacao_contas/",
                   "verbas_indenizatorias/deputados/", year, "/", month, "?formato=json")
    )
  }
}
# DF
urls[[length(urls) + 1]] <- list(
  source = "states/DF",
  label = "DF CLDF portal",
  url = "https://dados.cl.df.gov.br/dataset/verbas-indenizatorias"
)
# PE
for (year in c(2019, 2022, 2025)) {
  urls[[length(urls) + 1]] <- list(
    source = "states/PE",
    label = paste("PE ALEPE", year),
    url = paste0("https://dadosabertos.alepe.pe.gov.br/api/verbas_indenizatorias/",
                 year, "?formato=csv")
  )
}
# CE
for (year in c(2019, 2022, 2025)) {
  urls[[length(urls) + 1]] <- list(
    source = "states/CE",
    label = paste("CE ALCE", year),
    url = paste0("https://www2.al.ce.gov.br/api/despesas/verbas_indenizatorias/",
                 year, "?formato=csv")
  )
}
# RS
for (year in c(2019, 2022, 2025)) {
  urls[[length(urls) + 1]] <- list(
    source = "states/RS",
    label = paste("RS ALERS", year),
    url = paste0("http://www2.al.rs.gov.br/transparenciaalrs/DadosAbertos/",
                 "Despesas_", year, ".csv")
  )
}
# PI
urls[[length(urls) + 1]] <- list(
  source = "states/PI",
  label = "PI ALEPI portal",
  url = "https://www.al.pi.leg.br/transparencia-menu/dados-abertos"
)
# RN
urls[[length(urls) + 1]] <- list(
  source = "states/RN",
  label = "RN ALERN portal",
  url = "https://transparencia.al.rn.leg.br"
)

# --- collect_states.R: Tier 2 portals ---
tier2 <- list(
  AC = "https://app.al.ac.leg.br",
  AL = "https://www.al.al.leg.br/transparencia",
  AM = "https://www.aleam.gov.br/transparencia/despesa/",
  ES = "https://www.al.es.gov.br/Transparencia",
  GO = "https://transparencia.al.go.leg.br",
  MT = "https://www.al.mt.gov.br/transparencia/",
  PR = "https://transparencia.assembleia.pr.leg.br",
  PA = "https://www.alepa.pa.gov.br/Home/Page/PORTALDATRANSPARENCIA",
  RO = "https://transparencia.al.ro.leg.br",
  MS = "https://www.transparencia.al.ms.gov.br/pages/",
  AP = "https://al.ap.leg.br/transparencia/"
)
for (uf in names(tier2)) {
  urls[[length(urls) + 1]] <- list(
    source = paste0("states/", uf),
    label = paste(uf, "portal"),
    url = tier2[[uf]]
  )
}

# --- collect_states.R: Tier 3 portals ---
tier3 <- list(
  BA = "https://www.al.ba.gov.br/transparencia",
  MA = "https://sistemas.al.ma.leg.br/transparencia/",
  PB = "https://www.al.pb.leg.br/transparencia",
  RJ = "https://transparencia.alerj.rj.gov.br",
  RR = "https://transparencia.al.rr.leg.br",
  SE = "https://al.se.leg.br/portal-da-transparencia/",
  TO = "https://www.al.to.leg.br/transparencia"
)
for (uf in names(tier3)) {
  urls[[length(urls) + 1]] <- list(
    source = paste0("states/", uf),
    label = paste(uf, "portal"),
    url = tier3[[uf]]
  )
}

# ============================================================================
# Run HEAD requests
# ============================================================================

cat("Validating", length(urls), "URLs...\n\n")

results <- data.frame(
  source = character(),
  label = character(),
  url = character(),
  status = integer(),
  ok = logical(),
  note = character(),
  stringsAsFactors = FALSE
)

for (entry in urls) {
  status <- NA_integer_
  ok <- FALSE
  note <- ""

  tryCatch({
    conn <- url(entry$url, method = "libcurl")
    resp <- tryCatch({
      # Use HEAD via curlGetHeaders which is available in base R
      headers <- curlGetHeaders(entry$url, redirect = TRUE, verify = TRUE)
      # Extract status code from first status line
      status_line <- headers[grep("^HTTP/", headers)][1]
      status <- as.integer(regmatches(status_line, regexpr("[0-9]{3}", status_line)))
      ok <- !is.na(status) && status >= 200 && status < 400
    }, error = function(e) {
      note <<- conditionMessage(e)
    })
  }, error = function(e) {
    note <- conditionMessage(e)
  })

  symbol <- if (ok) "OK" else "FAIL"
  status_str <- if (is.na(status)) "---" else as.character(status)
  cat(sprintf("  [%4s] %-40s %s\n", symbol, entry$label, status_str))
  if (nchar(note) > 0) cat(sprintf("         %s\n", note))

  results <- rbind(results, data.frame(
    source = entry$source,
    label = entry$label,
    url = entry$url,
    status = status,
    ok = ok,
    note = note,
    stringsAsFactors = FALSE
  ))
}

# ============================================================================
# Summary
# ============================================================================

cat("\n=== SUMMARY ===\n")
cat("Total URLs tested:", nrow(results), "\n")
cat("  OK:  ", sum(results$ok), "\n")
cat("  FAIL:", sum(!results$ok), "\n")

# Group by source
sources <- unique(results$source)
cat("\nBy source:\n")
for (src in sources) {
  sub <- results[results$source == src, ]
  cat(sprintf("  %-20s %d/%d OK\n", src, sum(sub$ok), nrow(sub)))
}

# List failures
failed <- results[!results$ok, ]
if (nrow(failed) > 0) {
  cat("\nFailed URLs:\n")
  for (i in seq_len(nrow(failed))) {
    cat(sprintf("  [%s] %s\n        %s\n",
                ifelse(is.na(failed$status[i]), "ERR", as.character(failed$status[i])),
                failed$label[i], failed$url[i]))
  }
}

# Save results to CSV
out_file <- "url_validation_results.csv"
write.csv(results, out_file, row.names = FALSE)
cat("\nFull results saved to:", out_file, "\n")
