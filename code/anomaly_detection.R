# anomaly_detection.R
# Phase 3: Anomaly Detection
#
# Computes anomaly scores for politician-gas station spending patterns:
#   1. HHI concentration — does a politician concentrate spending at few stations?
#   2. Benford's law — do first digits of spending amounts follow expected distribution?
#   3. Volume-spending mismatch — is spending at a station disproportionate to its fuel volume?
#   4. Spending outliers — are amounts abnormally high relative to peers?
#   5. Composite anomaly score — weighted combination of all signals
#
# Input:  data/linked/{politicians.csv, stations.csv, connections.csv}
#         data/raw/anp/vendas_gasolina_municipio.csv (municipality fuel volumes)
# Output: data/anomaly/{politician_scores.csv, station_scores.csv, transaction_flags.csv}

library(data.table)

OUT_DIR <- "data/anomaly"
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

cat("=== Phase 3: Anomaly Detection ===\n\n")

# ============================================================
# Load linked data
# ============================================================
cat("Loading linked data...\n")
politicians <- fread("data/linked/politicians.csv", colClasses = list(character = "cpf"))
stations    <- fread("data/linked/stations.csv", colClasses = list(character = "cnpj"))
connections <- fread("data/linked/connections.csv",
                     colClasses = list(character = c("politician_cpf", "station_cnpj")))

# Focus on spending connections (not ownership)
spending_types <- c("ceap_spending", "ceaps_spending", "campaign_spending", "state_spending")
spending <- connections[type %in% spending_types & !is.na(valor) & valor > 0]
cat("  Spending records:", nrow(spending), "\n")

# ============================================================
# 1. HHI Concentration Index
# ============================================================
cat("\n=== 1. HHI Concentration Index ===\n")

# For each politician-year, compute the HHI of spending across stations
# HHI = sum(share_i^2) where share_i = spending at station i / total spending
# HHI = 1 means all spending at one station (maximum concentration)
# HHI = 1/N means equal spending across N stations (minimum concentration)

pol_year_station <- spending[!is.na(politician_cpf), .(
  total = sum(valor, na.rm = TRUE)
), by = .(politician_cpf, year, station_cnpj)]

pol_year_total <- pol_year_station[, .(
  total_spending = sum(total)
), by = .(politician_cpf, year)]

hhi_data <- merge(pol_year_station, pol_year_total, by = c("politician_cpf", "year"))
hhi_data[, share := total / total_spending]
hhi_data[, share_sq := share^2]

hhi <- hhi_data[, .(
  hhi = sum(share_sq),
  n_stations = .N,
  total_spending = total_spending[1]
), by = .(politician_cpf, year)]

# Only flag politicians with at least R$5,000 in spending (avoid noise from tiny amounts)
hhi <- hhi[total_spending >= 5000]

# Compute percentile ranks within each spending source type
hhi[, hhi_pctile := frank(hhi, ties.method = "average") / .N, by = year]

cat("  Politicians with HHI computed:", uniqueN(hhi$politician_cpf), "\n")
cat("  Politician-years:", nrow(hhi), "\n")
cat("  Mean HHI:", round(mean(hhi$hhi), 3), "\n")
cat("  Median HHI:", round(median(hhi$hhi), 3), "\n")
cat("  HHI > 0.5 (high concentration):", sum(hhi$hhi > 0.5), "(",
    round(100 * mean(hhi$hhi > 0.5), 1), "%)\n")
cat("  HHI = 1.0 (single station):", sum(hhi$hhi == 1), "(",
    round(100 * mean(hhi$hhi == 1), 1), "%)\n")

# HHI distribution by spending level
cat("\n  HHI by spending quartile:\n")
hhi[, spending_q := cut(total_spending,
                        breaks = quantile(total_spending, c(0, .25, .5, .75, 1)),
                        labels = c("Q1 (lowest)", "Q2", "Q3", "Q4 (highest)"),
                        include.lowest = TRUE)]
print(hhi[, .(mean_hhi = round(mean(hhi), 3),
              median_hhi = round(median(hhi), 3),
              pct_high = round(100 * mean(hhi > 0.5), 1)),
          by = spending_q][order(spending_q)])

# ============================================================
# 2. Benford's Law Analysis
# ============================================================
cat("\n=== 2. Benford's Law Analysis ===\n")

# Extract first digit of spending amounts
spending[, first_digit := as.integer(substr(gsub("^0+", "", gsub("[^0-9]", "",
                                   as.character(round(abs(valor))))), 1, 1))]
spending <- spending[first_digit >= 1 & first_digit <= 9]

# Expected Benford distribution
benford_expected <- log10(1 + 1 / (1:9))

# Global first-digit distribution
global_fd <- spending[, .N, by = first_digit][order(first_digit)]
global_fd[, prop := N / sum(N)]
global_fd[, expected := benford_expected]
global_fd[, deviation := prop - expected]

cat("  Global first-digit distribution:\n")
print(global_fd[, .(digit = first_digit,
                     observed = round(prop, 4),
                     expected = round(expected, 4),
                     deviation = round(deviation, 4))])

# Chi-squared goodness of fit (global)
global_chi <- spending[, {
  obs <- tabulate(first_digit, nbins = 9)
  exp <- benford_expected * sum(obs)
  chi_sq <- sum((obs - exp)^2 / exp)
  p_val <- pchisq(chi_sq, df = 8, lower.tail = FALSE)
  list(chi_sq = chi_sq, p_value = p_val, n = sum(obs))
}]
cat("  Global chi-squared:", round(global_chi$chi_sq, 2),
    "p-value:", format(global_chi$p_value, scientific = TRUE), "\n")

# Per-politician Benford deviation (only for politicians with >= 50 transactions)
pol_benford <- spending[, {
  if (.N >= 50) {
    obs <- tabulate(first_digit, nbins = 9)
    exp <- benford_expected * sum(obs)
    chi_sq <- sum((obs - exp)^2 / exp)
    p_val <- pchisq(chi_sq, df = 8, lower.tail = FALSE)
    # Mean Absolute Deviation from Benford
    mad_stat <- mean(abs(obs / sum(obs) - benford_expected))
    list(benford_chi = chi_sq, benford_p = p_val, benford_mad = mad_stat, n_txns = .N)
  }
}, by = politician_cpf]

cat("  Politicians with Benford scores:", nrow(pol_benford), "\n")
cat("  Mean MAD:", round(mean(pol_benford$benford_mad), 4), "\n")

# Flag those with significant deviation (p < 0.01) and high MAD
benford_flagged <- pol_benford[benford_p < 0.01 & benford_mad > 0.05]
cat("  Benford-flagged (p<0.01, MAD>0.05):", nrow(benford_flagged), "\n")

# Second-digit analysis (Benford also predicts second digits)
spending[, second_digit := as.integer(substr(gsub("^0+", "", gsub("[^0-9]", "",
                                    as.character(round(abs(valor))))), 2, 2))]

# ============================================================
# 3. Volume-Spending Mismatch
# ============================================================
cat("\n=== 3. Volume-Spending Mismatch ===\n")

# Load ANP municipality fuel volumes
anp_vol <- tryCatch({
  fread("data/raw/anp/vendas_gasolina_municipio.csv", sep = ";", encoding = "UTF-8")
}, error = function(e) {
  cat("  Could not load ANP volumes:", conditionMessage(e), "\n")
  NULL
})

if (!is.null(anp_vol)) {
  # Standardize column names
  vol_cols <- names(anp_vol)
  cat("  ANP volume columns:", paste(vol_cols, collapse = ", "), "\n")

  # Compute total public spending per station
  station_spending <- spending[, .(
    total_spent = sum(valor, na.rm = TRUE),
    n_politicians = uniqueN(politician_cpf),
    n_transactions = .N
  ), by = station_cnpj]

  # Match stations to municipalities via stations table
  station_muni <- stations[!is.na(cnpj) & !is.na(municipio), .(cnpj, municipio, uf)]

  station_spending <- merge(station_spending, station_muni,
                            by.x = "station_cnpj", by.y = "cnpj", all.x = TRUE)

  # Number of stations per municipality that received political spending
  muni_stats <- station_spending[!is.na(municipio), .(
    n_stations_with_spending = .N,
    total_political_spending = sum(total_spent),
    total_transactions = sum(n_transactions)
  ), by = .(municipio, uf)]

  # Count total stations per municipality from ANP
  anp_muni <- stations[!is.na(municipio) & !is.na(uf), .(
    total_stations = .N
  ), by = .(municipio, uf)]

  muni_stats <- merge(muni_stats, anp_muni, by = c("municipio", "uf"), all.x = TRUE)

  # Compute spending intensity per station
  muni_stats[, spending_per_station := total_political_spending / n_stations_with_spending]

  # Spending per station relative to municipal average
  cat("  Municipalities with political spending:", nrow(muni_stats), "\n")
  cat("  Mean spending per station: R$",
      format(round(mean(muni_stats$spending_per_station)), big.mark = "."), "\n")

  # Identify stations receiving disproportionate spending
  # A station receiving > 5x the median spending per station in its municipality
  station_in_muni <- merge(station_spending[!is.na(municipio)],
                           muni_stats[, .(municipio, uf, spending_per_station)],
                           by = c("municipio", "uf"), all.x = TRUE)
  station_in_muni[, spending_ratio := total_spent / spending_per_station]
  station_in_muni <- station_in_muni[!is.na(spending_ratio) & is.finite(spending_ratio)]

  volume_flagged <- station_in_muni[spending_ratio > 5]
  cat("  Stations with >5x average political spending:", nrow(volume_flagged), "\n")
} else {
  cat("  Skipping volume mismatch (ANP data unavailable)\n")
  volume_flagged <- data.table()
}

# ============================================================
# 4. Spending Pattern Outliers
# ============================================================
cat("\n=== 4. Spending Pattern Outliers ===\n")

# 4a. Transaction-level: unusually round amounts or repeated exact amounts
cat("  Analyzing transaction patterns...\n")

# Round amount detection (exact multiples of 100, 500, 1000)
spending[, is_round_100 := (valor %% 100) == 0]
spending[, is_round_500 := (valor %% 500) == 0]
spending[, is_round_1000 := (valor %% 1000) == 0]

cat("    Transactions with round amounts (mod 100):", sum(spending$is_round_100),
    "(", round(100 * mean(spending$is_round_100), 1), "%)\n")
cat("    Transactions with round amounts (mod 1000):", sum(spending$is_round_1000),
    "(", round(100 * mean(spending$is_round_1000), 1), "%)\n")

# Per-politician: excessive share of round amounts
pol_round <- spending[, .(
  pct_round_100 = mean(is_round_100),
  pct_round_1000 = mean(is_round_1000),
  n_txns = .N,
  mean_amount = mean(valor),
  median_amount = median(valor),
  sd_amount = sd(valor)
), by = politician_cpf]
pol_round <- pol_round[n_txns >= 10]  # minimum transactions

# Flag politicians with >50% round-1000 amounts (highly unusual)
round_flagged <- pol_round[pct_round_1000 > 0.5 & n_txns >= 20]
cat("    Politicians with >50% round-1000 amounts (n>=20):", nrow(round_flagged), "\n")

# 4b. Duplicate amount detection (same politician, same station, same amount, close dates)
cat("  Analyzing duplicate patterns...\n")
dupes <- spending[, .(
  n_exact_same = .N
), by = .(politician_cpf, station_cnpj, valor)]
dupes <- dupes[n_exact_same >= 5]  # 5+ identical amounts at same station
cat("    Politician-station pairs with 5+ identical amounts:", nrow(dupes), "\n")

# 4c. Spending per politician vs peers in same office
cat("  Computing peer group outlier scores...\n")

# Annual spending by politician
pol_annual <- spending[, .(
  annual_spending = sum(valor)
), by = .(politician_cpf, year, type)]

# Add office type from politicians table
pol_annual <- merge(pol_annual, politicians[, .(cpf, cargos)],
                    by.x = "politician_cpf", by.y = "cpf", all.x = TRUE)

# Simplify cargo to main office level
pol_annual[, office := fcase(
  grepl("FEDERAL|Deputado Federal", cargos, ignore.case = TRUE), "dep_federal",
  grepl("SENADOR|Senador", cargos, ignore.case = TRUE), "senador",
  grepl("ESTADUAL|DISTRITAL|Deputado Estadual", cargos, ignore.case = TRUE), "dep_estadual",
  grepl("VEREADOR|Vereador", cargos, ignore.case = TRUE), "vereador",
  grepl("PREFEITO|Prefeito", cargos, ignore.case = TRUE), "prefeito",
  grepl("GOVERNADOR|Governador", cargos, ignore.case = TRUE), "governador",
  default = "outro"
)]

# Z-score within office-year-type group
pol_annual[, `:=`(
  group_mean = mean(annual_spending),
  group_sd = sd(annual_spending),
  group_n = .N
), by = .(office, year, type)]

pol_annual[group_sd > 0, z_score := (annual_spending - group_mean) / group_sd]

# Flag extreme outliers (z > 3)
outlier_spending <- pol_annual[!is.na(z_score) & z_score > 3]
cat("    Extreme spending outliers (z>3):", nrow(outlier_spending), "\n")
cat("    Unique politicians flagged:", uniqueN(outlier_spending$politician_cpf), "\n")

# ============================================================
# 5. Composite Anomaly Scores
# ============================================================
cat("\n=== 5. Computing Composite Anomaly Scores ===\n")

# Build politician-level score table
pol_scores <- politicians[, .(cpf, nome, cargos, partidos)]

# Add HHI (max across years)
hhi_max <- hhi[, .(
  max_hhi = max(hhi),
  mean_hhi = mean(hhi),
  years_high_hhi = sum(hhi > 0.5),
  max_spending = max(total_spending)
), by = politician_cpf]
pol_scores <- merge(pol_scores, hhi_max, by.x = "cpf", by.y = "politician_cpf", all.x = TRUE)

# Add Benford scores
pol_scores <- merge(pol_scores, pol_benford[, .(politician_cpf, benford_mad, benford_p, n_txns)],
                    by.x = "cpf", by.y = "politician_cpf", all.x = TRUE)

# Add round amount scores
pol_scores <- merge(pol_scores,
                    pol_round[, .(politician_cpf = politician_cpf,
                                  pct_round_1000, pct_round_100)],
                    by.x = "cpf", by.y = "politician_cpf", all.x = TRUE)

# Add max z-score
max_z <- pol_annual[!is.na(z_score), .(max_z_score = max(z_score)), by = politician_cpf]
pol_scores <- merge(pol_scores, max_z, by.x = "cpf", by.y = "politician_cpf", all.x = TRUE)

# Add flag counts
self_dealing <- tryCatch(fread("data/linked/flag_self_dealing.csv", colClasses = "character"), error = function(e) data.table())
round_trip <- tryCatch(fread("data/linked/flag_round_trip.csv", colClasses = "character"), error = function(e) data.table())

if (nrow(self_dealing) > 0) {
  sd_count <- self_dealing[, .(n_self_dealing = .N), by = politician_cpf]
  pol_scores <- merge(pol_scores, sd_count, by.x = "cpf", by.y = "politician_cpf", all.x = TRUE)
} else {
  pol_scores[, n_self_dealing := 0L]
}

if (nrow(round_trip) > 0) {
  rt_count <- round_trip[, .(n_round_trip = .N), by = politician_cpf]
  pol_scores <- merge(pol_scores, rt_count, by.x = "cpf", by.y = "politician_cpf", all.x = TRUE)
} else {
  pol_scores[, n_round_trip := 0L]
}

# Fill NAs with 0
setnafill(pol_scores, fill = 0,
          cols = c("max_hhi", "mean_hhi", "years_high_hhi", "max_spending",
                   "benford_mad", "pct_round_1000", "pct_round_100",
                   "max_z_score", "n_self_dealing", "n_round_trip"))
pol_scores[is.na(n_txns), n_txns := 0L]
pol_scores[is.na(benford_p), benford_p := 1]

# Composite score: normalize each component to [0,1] and weight
# Only score politicians with minimum data
pol_scores[, has_data := max_spending > 0 | n_txns > 0]

# Normalize to [0,1] using min-max within scored group
normalize01 <- function(x) {
  r <- range(x, na.rm = TRUE)
  if (r[2] == r[1]) return(rep(0, length(x)))
  (x - r[1]) / (r[2] - r[1])
}

scored <- pol_scores[has_data == TRUE]
scored[, `:=`(
  score_hhi = normalize01(max_hhi),
  score_benford = normalize01(benford_mad),
  score_round = normalize01(pct_round_1000),
  score_outlier = normalize01(pmin(max_z_score, 10)),  # cap at 10
  score_self_dealing = fifelse(n_self_dealing > 0, 1, 0),
  score_round_trip = fifelse(n_round_trip > 0, 1, 0)
)]

# Weighted composite
scored[, anomaly_score := (
  0.20 * score_hhi +
  0.15 * score_benford +
  0.10 * score_round +
  0.15 * score_outlier +
  0.25 * score_self_dealing +
  0.15 * score_round_trip
)]

scored <- scored[order(-anomaly_score)]

cat("  Politicians with anomaly scores:", nrow(scored), "\n")
cat("  Score distribution:\n")
cat("    Mean:", round(mean(scored$anomaly_score), 4), "\n")
cat("    Median:", round(median(scored$anomaly_score), 4), "\n")
cat("    P95:", round(quantile(scored$anomaly_score, 0.95), 4), "\n")
cat("    P99:", round(quantile(scored$anomaly_score, 0.99), 4), "\n")
cat("    Max:", round(max(scored$anomaly_score), 4), "\n")

# Top 20 most anomalous politicians
cat("\nTop 20 most anomalous politicians:\n")
print(scored[1:20, .(nome, cargos, anomaly_score = round(anomaly_score, 3),
                      max_hhi = round(max_hhi, 2),
                      benford_mad = round(benford_mad, 4),
                      pct_round_1000 = round(pct_round_1000, 2),
                      max_z_score = round(max_z_score, 1),
                      n_self_dealing, n_round_trip,
                      max_spending = round(max_spending))])

# ============================================================
# 6. Station-level anomaly scores
# ============================================================
cat("\n=== 6. Station-Level Anomaly Scores ===\n")

station_scores <- spending[!is.na(station_cnpj), .(
  total_received = sum(valor, na.rm = TRUE),
  n_politicians = uniqueN(politician_cpf),
  n_transactions = .N,
  mean_txn = mean(valor),
  pct_round_1000 = mean((valor %% 1000) == 0)
), by = station_cnpj]

# Add ownership info
station_scores <- merge(station_scores,
                        stations[, .(cnpj, nome_fantasia, municipio, uf, n_socios)],
                        by.x = "station_cnpj", by.y = "cnpj", all.x = TRUE)

# Flag: station receives from many politicians (could be legitimate busy station,
# but combined with other flags it's informative)
station_scores[, politician_diversity := n_politicians / max(n_politicians)]

# Flag: station has ownership connection AND receives lots of spending
ownership_stations <- connections[type == "ownership", .(
  n_owner_politicians = uniqueN(politician_cpf)
), by = .(station_cnpj)]
station_scores <- merge(station_scores, ownership_stations,
                        by = "station_cnpj", all.x = TRUE)
station_scores[is.na(n_owner_politicians), n_owner_politicians := 0L]

station_scores <- station_scores[order(-total_received)]
cat("  Stations with scores:", nrow(station_scores), "\n")

# ============================================================
# 7. Save outputs
# ============================================================
cat("\n=== Saving outputs ===\n")

fwrite(scored, file.path(OUT_DIR, "politician_scores.csv"))
fwrite(station_scores, file.path(OUT_DIR, "station_scores.csv"))

# Transaction-level flags
txn_flags <- spending[is_round_1000 == TRUE | valor > 50000, .(
  politician_cpf, station_cnpj, type, year, valor,
  is_round_1000, first_digit
)]
fwrite(txn_flags, file.path(OUT_DIR, "transaction_flags.csv"))

# Save HHI details
fwrite(hhi, file.path(OUT_DIR, "hhi_details.csv"))

# Save Benford details
fwrite(pol_benford, file.path(OUT_DIR, "benford_details.csv"))

cat("  Files saved to:", OUT_DIR, "\n")

cat("\n=== Phase 3 Summary ===\n")
cat("  HHI: computed for", nrow(hhi), "politician-years\n")
cat("  Benford: tested", nrow(pol_benford), "politicians\n")
cat("  Spending outliers (z>3):", nrow(outlier_spending), "\n")
cat("  Self-dealing flags:", nrow(self_dealing), "\n")
cat("  Round-trip flags:", nrow(round_trip), "\n")
cat("  Composite scores: top score =", round(max(scored$anomaly_score), 3), "\n")
cat("\nDone.\n")
