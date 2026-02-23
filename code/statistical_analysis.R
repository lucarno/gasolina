# statistical_analysis.R
# Phase 4: Statistical Analysis
#
# Formal econometric analysis of politician-gas station spending patterns:
#   1. Descriptive statistics and summary tables
#   2. Panel regression: spending ~ ownership + anomaly indicators
#   3. Event study: spending around election dates
#   4. Difference-in-differences: effect of 2015 corporate donation ban
#
# Input:  data/linked/ + data/anomaly/
# Output: output/tables/ + output/figures/

library(data.table)
library(fixest)
library(ggplot2)
library(modelsummary)

TABLE_DIR <- "output/tables"
FIG_DIR <- "output/figures"
dir.create(TABLE_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(FIG_DIR, recursive = TRUE, showWarnings = FALSE)

cat("=== Phase 4: Statistical Analysis ===\n\n")

# ============================================================
# Load data
# ============================================================
cat("Loading data...\n")
politicians <- fread("data/linked/politicians.csv", colClasses = list(character = "cpf"))
stations    <- fread("data/linked/stations.csv", colClasses = list(character = "cnpj"))
connections <- fread("data/linked/connections.csv",
                     colClasses = list(character = c("politician_cpf", "station_cnpj")))
pol_scores  <- fread("data/anomaly/politician_scores.csv",
                     colClasses = list(character = "cpf"))

# Spending subset
spending_types <- c("ceap_spending", "ceaps_spending", "campaign_spending", "state_spending")
spending <- connections[type %in% spending_types & !is.na(valor) & valor > 0]
ownership <- connections[type == "ownership"]
donations <- connections[type == "campaign_donation" & !is.na(valor) & valor > 0]

# ============================================================
# 1. Descriptive Statistics
# ============================================================
cat("\n=== 1. Descriptive Statistics ===\n")

# 1a. Overall spending by source and year
spend_by_source <- spending[, .(
  total_spending = sum(valor, na.rm = TRUE),
  n_transactions = .N,
  n_politicians = uniqueN(politician_cpf),
  n_stations = uniqueN(station_cnpj),
  mean_txn = mean(valor, na.rm = TRUE),
  median_txn = median(valor, na.rm = TRUE)
), by = .(type, year)]
spend_by_source <- spend_by_source[order(type, year)]

cat("Spending by source and year:\n")
print(spend_by_source[, .(type, year, n_transactions, n_politicians,
                           total = round(total_spending / 1e6, 1),
                           mean = round(mean_txn), median = round(median_txn))])

fwrite(spend_by_source, file.path(TABLE_DIR, "spending_by_source_year.csv"))

# 1b. Ownership statistics
owner_stats <- ownership[, .(
  n_owner_politicians = uniqueN(politician_cpf),
  n_owned_stations = uniqueN(station_cnpj)
)]
cat("\nOwnership: ", owner_stats$n_owner_politicians, " politicians own ",
    owner_stats$n_owned_stations, " gas stations\n", sep = "")

# ============================================================
# 2. Panel Regression
# ============================================================
cat("\n=== 2. Panel Regression ===\n")

# Build panel: politician-year level
# DV: total fuel spending
# Key IV: owns_station (binary: does politician own a gas station?)
# Controls: office type, year, state

# Aggregate spending to politician-year
panel <- spending[!is.na(politician_cpf), .(
  total_spending = sum(valor, na.rm = TRUE),
  n_transactions = .N,
  n_stations = uniqueN(station_cnpj),
  hhi = {
    st_totals <- .SD[, sum(valor), by = station_cnpj]$V1
    tot <- sum(st_totals)
    if (tot > 0) sum((st_totals / tot)^2) else NA_real_
  }
), by = .(politician_cpf, year)]

# Add ownership indicator
owner_cpfs <- unique(ownership$politician_cpf)
panel[, owns_station := politician_cpf %in% owner_cpfs]

# Add politician characteristics
panel <- merge(panel, politicians[, .(cpf, cargos, partidos, nome)],
               by.x = "politician_cpf", by.y = "cpf", all.x = TRUE)

# Simplify office type
panel[, office := fcase(
  grepl("FEDERAL|Deputado Federal", cargos, ignore.case = TRUE), "dep_federal",
  grepl("SENADOR|Senador", cargos, ignore.case = TRUE), "senador",
  grepl("ESTADUAL|DISTRITAL", cargos, ignore.case = TRUE), "dep_estadual",
  grepl("VEREADOR", cargos, ignore.case = TRUE), "vereador",
  grepl("PREFEITO", cargos, ignore.case = TRUE), "prefeito",
  grepl("GOVERNADOR", cargos, ignore.case = TRUE), "governador",
  default = "outro"
)]

# Self-dealing indicator
self_dealing <- fread("data/linked/flag_self_dealing.csv", colClasses = "character")
panel[, self_dealing := politician_cpf %in% self_dealing$politician_cpf]

# Log spending
panel[, log_spending := log(total_spending + 1)]
panel[, log_n_txns := log(n_transactions + 1)]

# Year as factor
panel[, year_f := factor(year)]

cat("Panel dimensions: ", nrow(panel), " obs, ",
    uniqueN(panel$politician_cpf), " politicians, ",
    uniqueN(panel$year), " years\n", sep = "")
cat("Owns station: ", sum(panel$owns_station), " obs (",
    round(100 * mean(panel$owns_station), 1), "%)\n", sep = "")

# Regression 1: OLS with year FE
cat("\n--- Model 1: OLS with year FE ---\n")
m1 <- feols(log_spending ~ owns_station + i(year_f), data = panel)
cat("owns_station coef:", round(coef(m1)["owns_stationTRUE"], 3),
    "se:", round(se(m1)["owns_stationTRUE"], 3), "\n")

# Regression 2: Add office FE
cat("--- Model 2: + office FE ---\n")
m2 <- feols(log_spending ~ owns_station + i(year_f) + i(office), data = panel)
cat("owns_station coef:", round(coef(m2)["owns_stationTRUE"], 3),
    "se:", round(se(m2)["owns_stationTRUE"], 3), "\n")

# Regression 3: Politician FE (within-politician variation)
cat("--- Model 3: Politician FE ---\n")
m3 <- feols(log_spending ~ owns_station | politician_cpf + year_f, data = panel)
cat("owns_station coef:", round(coef(m3)["owns_stationTRUE"], 3),
    "se:", round(se(m3)["owns_stationTRUE"], 3), "\n")

# Regression 4: Intensive margin — HHI
cat("--- Model 4: HHI as DV ---\n")
m4 <- feols(hhi ~ owns_station + i(year_f) + i(office),
            data = panel[!is.na(hhi)])
cat("owns_station coef:", round(coef(m4)["owns_stationTRUE"], 3),
    "se:", round(se(m4)["owns_stationTRUE"], 3), "\n")

# Regression 5: N stations
cat("--- Model 5: Number of stations ---\n")
m5 <- feols(n_stations ~ owns_station + i(year_f) + i(office), data = panel)
cat("owns_station coef:", round(coef(m5)["owns_stationTRUE"], 3),
    "se:", round(se(m5)["owns_stationTRUE"], 3), "\n")

# Save regression table
models <- list("(1) Log Spending" = m1, "(2) + Office FE" = m2,
               "(3) Politician FE" = m3, "(4) HHI" = m4, "(5) N Stations" = m5)

tryCatch({
  msummary(models, output = file.path(TABLE_DIR, "panel_regressions.tex"),
           stars = c("*" = 0.1, "**" = 0.05, "***" = 0.01),
           coef_omit = "year_f|office",
           gof_omit = "AIC|BIC|Log.Lik|Std.Errors",
           title = "Panel Regressions: Gas Station Ownership and Spending")
  cat("LaTeX table saved\n")
}, error = function(e) cat("LaTeX export failed:", conditionMessage(e), "\n"))

# Also save as plain text
tryCatch({
  msummary(models, output = file.path(TABLE_DIR, "panel_regressions.txt"),
           stars = c("*" = 0.1, "**" = 0.05, "***" = 0.01),
           coef_omit = "year_f|office",
           gof_omit = "AIC|BIC|Log.Lik|Std.Errors")
  cat("Text table saved\n")
}, error = function(e) cat("Text export failed:", conditionMessage(e), "\n"))

# ============================================================
# 3. Event Study: Spending Around Elections
# ============================================================
cat("\n=== 3. Event Study: Spending Around Elections ===\n")

# For CEAP/CEAPS (public spending), we can analyze whether spending increases
# in election years for politicians who are running

# Election years in our data: 2014, 2016, 2018, 2020, 2022, 2024
# Federal elections: 2014, 2018, 2022 (deputies, senators)
# Municipal elections: 2016, 2020, 2024 (mayors, vereadores)

# Mark election years
federal_years <- c(2014, 2018, 2022)
municipal_years <- c(2016, 2020, 2024)

# Focus on CEAP/CEAPS (public money, not campaign money)
public_spending <- spending[type %in% c("ceap_spending", "ceaps_spending")]

pub_panel <- public_spending[!is.na(politician_cpf), .(
  total_spending = sum(valor, na.rm = TRUE),
  n_transactions = .N
), by = .(politician_cpf, year)]

# Merge with politician info
pub_panel <- merge(pub_panel, politicians[, .(cpf, cargos)],
                   by.x = "politician_cpf", by.y = "cpf", all.x = TRUE)

# Determine if it's a federal or municipal office holder
pub_panel[, is_federal := grepl("FEDERAL|SENADOR", cargos, ignore.case = TRUE)]

# Mark election year
pub_panel[, election_year := fifelse(
  is_federal, year %in% federal_years, year %in% municipal_years
)]

# Event time relative to nearest election
pub_panel[is_federal == TRUE, `:=`(
  nearest_election = sapply(year, function(y) {
    ey <- federal_years
    ey[which.min(abs(y - ey))]
  }),
  event_time = sapply(year, function(y) {
    ey <- federal_years
    y - ey[which.min(abs(y - ey))]
  })
)]

pub_panel[, log_spending := log(total_spending + 1)]

# Event study for federal politicians
federal_panel <- pub_panel[is_federal == TRUE & !is.na(event_time) &
                             abs(event_time) <= 3]
federal_panel[, event_time_f := factor(event_time)]

if (nrow(federal_panel) > 100) {
  cat("Federal politicians event study:\n")
  es_model <- tryCatch({
    feols(log_spending ~ i(event_time_f, ref = "0") | politician_cpf,
          data = federal_panel)
  }, error = function(e) {
    cat("  Event study model failed:", conditionMessage(e), "\n")
    NULL
  })

  if (!is.null(es_model)) {
    cat("  Event study coefficients (relative to election year):\n")
    print(summary(es_model, se = "hetero"))

    # Plot event study
    es_coefs <- data.table(coeftable(es_model, se = "hetero"))
    es_coefs[, event_time := as.integer(gsub("event_time_f::", "", rownames(coeftable(es_model))))]
    # Add reference period (0)
    es_coefs <- rbind(
      es_coefs,
      data.table(Estimate = 0, `Std. Error` = 0, `t value` = 0, `Pr(>|t|)` = 1,
                 event_time = 0),
      use.names = FALSE
    )
    setnames(es_coefs, c("estimate", "se", "t", "p", "event_time"))
    es_coefs <- es_coefs[order(event_time)]
    es_coefs[, ci_lo := estimate - 1.96 * se]
    es_coefs[, ci_hi := estimate + 1.96 * se]

    p_es <- ggplot(es_coefs, aes(x = event_time, y = estimate)) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
      geom_vline(xintercept = 0, linetype = "dotted", color = "red", alpha = 0.5) +
      geom_ribbon(aes(ymin = ci_lo, ymax = ci_hi), alpha = 0.2, fill = "steelblue") +
      geom_point(size = 3, color = "steelblue") +
      geom_line(color = "steelblue") +
      labs(x = "Years Relative to Election",
           y = "Log Spending (relative to election year)",
           title = "Event Study: Federal Politicians' Fuel Spending Around Elections") +
      theme_minimal() +
      scale_x_continuous(breaks = -3:3)
    ggsave(file.path(FIG_DIR, "event_study_federal.pdf"), p_es, width = 8, height = 5)
    ggsave(file.path(FIG_DIR, "event_study_federal.png"), p_es, width = 8, height = 5, dpi = 300)
    cat("  Event study plot saved\n")
  }
}

# Simple comparison: election vs non-election year
cat("\n  Election year effect (simple comparison):\n")
pub_panel_clean <- pub_panel[!is.na(election_year)]
cat("    Election year mean spending: R$",
    round(mean(pub_panel_clean[election_year == TRUE]$total_spending)), "\n")
cat("    Non-election year mean:      R$",
    round(mean(pub_panel_clean[election_year == FALSE]$total_spending)), "\n")

# ============================================================
# 4. Difference-in-Differences: Corporate Donation Ban
# ============================================================
cat("\n=== 4. DiD: Corporate Donation Ban (2015) ===\n")

# The STF banned corporate donations in September 2015
# Treatment: gas stations that were significant donors pre-ban
# Control: gas stations that were not donors
# Outcome: politician spending at these stations

# Identify treated stations: those that donated before 2016
pre_ban_donors <- donations[year <= 2015, .(
  total_donated = sum(valor, na.rm = TRUE),
  n_donations = .N
), by = station_cnpj]
pre_ban_donors <- pre_ban_donors[total_donated > 1000]  # meaningful donors

cat("  Pre-ban donor stations (donated > R$1,000):", nrow(pre_ban_donors), "\n")

# Build station-year panel for campaign spending
campaign <- spending[type == "campaign_spending" & !is.na(station_cnpj)]
station_year <- campaign[, .(
  total_spending = sum(valor, na.rm = TRUE),
  n_politicians = uniqueN(politician_cpf),
  n_transactions = .N
), by = .(station_cnpj, year)]

# Mark treatment
station_year[, treated := station_cnpj %in% pre_ban_donors$station_cnpj]
station_year[, post := year >= 2016]
station_year[, log_spending := log(total_spending + 1)]

cat("  Station-year obs:", nrow(station_year), "\n")
cat("  Treated obs:", sum(station_year$treated), "\n")
cat("  Post obs:", sum(station_year$post), "\n")

# DiD regression
if (sum(station_year$treated & station_year$post) > 10 &&
    sum(station_year$treated & !station_year$post) > 10) {

  # Basic 2x2 DiD (no year FE — post captures the time effect)
  did1 <- feols(log_spending ~ treated * post, data = station_year)
  cat("\n  DiD Model 1 (basic 2x2):\n")
  cat("    treated x post coef:", round(coef(did1)["treatedTRUE:postTRUE"], 3),
      "se:", round(se(did1)["treatedTRUE:postTRUE"], 3), "\n")

  # Dynamic DiD: year-specific treatment effects (event study around ban)
  station_year[, year_f := factor(year)]
  did2 <- feols(log_spending ~ i(year_f, treated, ref = "2014"),
                data = station_year)
  cat("  DiD Model 2 (dynamic, year-specific treatment):\n")
  print(coeftable(did2, se = "hetero"))

  # N_politicians as alternative outcome
  station_year[, log_politicians := log(n_politicians + 1)]
  did3 <- feols(log_politicians ~ treated * post, data = station_year)
  cat("  DiD Model 3 (DV: log n_politicians):\n")
  cat("    treated x post coef:", round(coef(did3)["treatedTRUE:postTRUE"], 3),
      "se:", round(se(did3)["treatedTRUE:postTRUE"], 3), "\n")

  # Save DiD results
  did_models <- list("(1) Basic DiD" = did1,
                     "(2) Dynamic DiD" = did2,
                     "(3) N Politicians" = did3)
  tryCatch({
    msummary(did_models, output = file.path(TABLE_DIR, "did_donation_ban.txt"),
             stars = c("*" = 0.1, "**" = 0.05, "***" = 0.01),
             gof_omit = "AIC|BIC|Log.Lik",
             coef_omit = "year_f")
    msummary(did_models, output = file.path(TABLE_DIR, "did_donation_ban.tex"),
             stars = c("*" = 0.1, "**" = 0.05, "***" = 0.01),
             gof_omit = "AIC|BIC|Log.Lik",
             coef_omit = "year_f",
             title = "DiD: Corporate Donation Ban and Campaign Spending at Gas Stations")
    cat("  DiD tables saved\n")
  }, error = function(e) cat("  DiD table export failed:", conditionMessage(e), "\n"))

  # Pre/post means
  cat("\n  Pre/Post means (campaign spending at station):\n")
  cat("    Treated pre:", round(mean(station_year[treated == TRUE & post == FALSE]$total_spending)), "\n")
  cat("    Treated post:", round(mean(station_year[treated == TRUE & post == TRUE]$total_spending)), "\n")
  cat("    Control pre:", round(mean(station_year[treated == FALSE & post == FALSE]$total_spending)), "\n")
  cat("    Control post:", round(mean(station_year[treated == FALSE & post == TRUE]$total_spending)), "\n")
}

# ============================================================
# 5. Descriptive Figures
# ============================================================
cat("\n=== 5. Generating Figures ===\n")

# Figure 1: Total fuel spending by source and year
fig1_data <- spending[, .(total = sum(valor, na.rm = TRUE) / 1e6), by = .(type, year)]
fig1_data[, type_label := fcase(
  type == "ceap_spending", "CEAP (Federal Deputies)",
  type == "ceaps_spending", "CEAPS (Senators)",
  type == "campaign_spending", "Campaign (TSE)",
  type == "state_spending", "State Assemblies"
)]

p1 <- ggplot(fig1_data[!is.na(year)], aes(x = year, y = total, fill = type_label)) +
  geom_col(position = "dodge") +
  labs(x = "Year", y = "Total Spending (R$ millions)", fill = "Source",
       title = "Political Fuel Spending by Source and Year") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set2") +
  theme(legend.position = "bottom")
ggsave(file.path(FIG_DIR, "spending_by_source_year.pdf"), p1, width = 10, height = 6)
ggsave(file.path(FIG_DIR, "spending_by_source_year.png"), p1, width = 10, height = 6, dpi = 300)

# Figure 2: HHI distribution
hhi <- fread("data/anomaly/hhi_details.csv", colClasses = list(character = "politician_cpf"))
p2 <- ggplot(hhi, aes(x = hhi)) +
  geom_histogram(bins = 50, fill = "steelblue", alpha = 0.7) +
  geom_vline(xintercept = 0.25, linetype = "dashed", color = "red") +
  annotate("text", x = 0.27, y = Inf, label = "HHI = 0.25\n(competitive)", vjust = 1.5,
           color = "red", size = 3) +
  labs(x = "Herfindahl-Hirschman Index (HHI)",
       y = "Count (politician-years)",
       title = "Distribution of Fuel Spending Concentration") +
  theme_minimal()
ggsave(file.path(FIG_DIR, "hhi_distribution.pdf"), p2, width = 8, height = 5)
ggsave(file.path(FIG_DIR, "hhi_distribution.png"), p2, width = 8, height = 5, dpi = 300)

# Figure 3: Benford's law
benford_expected <- log10(1 + 1 / (1:9))
spending_fd <- spending[!is.na(valor) & valor > 0]
spending_fd[, first_digit := as.integer(substr(gsub("^0+", "", gsub("[^0-9]", "",
                                    as.character(round(abs(valor))))), 1, 1))]
spending_fd <- spending_fd[first_digit >= 1 & first_digit <= 9]

fd_dist <- spending_fd[, .N, by = first_digit][order(first_digit)]
fd_dist[, prop := N / sum(N)]
fd_dist[, expected := benford_expected]

fd_long <- melt(fd_dist[, .(first_digit, Observed = prop, Expected = expected)],
                id.vars = "first_digit", variable.name = "Distribution",
                value.name = "proportion")

p3 <- ggplot(fd_long, aes(x = factor(first_digit), y = proportion, fill = Distribution)) +
  geom_col(position = "dodge") +
  labs(x = "First Digit", y = "Proportion",
       title = "First-Digit Distribution vs Benford's Law") +
  theme_minimal() +
  scale_fill_manual(values = c("Observed" = "steelblue", "Expected" = "tomato"))
ggsave(file.path(FIG_DIR, "benford_first_digit.pdf"), p3, width = 8, height = 5)
ggsave(file.path(FIG_DIR, "benford_first_digit.png"), p3, width = 8, height = 5, dpi = 300)

# Figure 4: Anomaly score distribution
p4 <- ggplot(pol_scores[has_data == TRUE & anomaly_score > 0],
             aes(x = anomaly_score)) +
  geom_histogram(bins = 50, fill = "steelblue", alpha = 0.7) +
  geom_vline(xintercept = quantile(pol_scores[has_data == TRUE]$anomaly_score, 0.95,
                                    na.rm = TRUE),
             linetype = "dashed", color = "red") +
  labs(x = "Composite Anomaly Score",
       y = "Count",
       title = "Distribution of Politician Anomaly Scores") +
  theme_minimal()
ggsave(file.path(FIG_DIR, "anomaly_score_distribution.pdf"), p4, width = 8, height = 5)
ggsave(file.path(FIG_DIR, "anomaly_score_distribution.png"), p4, width = 8, height = 5, dpi = 300)

# Figure 5: Campaign donations vs spending at same station (round-tripping)
rt_data <- connections[type == "campaign_donation" | type == "campaign_spending"]
rt_pairs <- rt_data[, .(
  donations = sum(valor[type == "campaign_donation"], na.rm = TRUE),
  spending = sum(valor[type == "campaign_spending"], na.rm = TRUE)
), by = .(politician_cpf, station_cnpj)]
rt_pairs <- rt_pairs[donations > 0 & spending > 0]

if (nrow(rt_pairs) > 5) {
  p5 <- ggplot(rt_pairs, aes(x = log10(donations + 1), y = log10(spending + 1))) +
    geom_point(alpha = 0.3, size = 1.5, color = "steelblue") +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
    labs(x = "Log10(Donations Received from Station)",
         y = "Log10(Campaign Spending at Station)",
         title = "Round-Tripping: Donations vs Spending at Same Gas Station") +
    theme_minimal()
  ggsave(file.path(FIG_DIR, "round_tripping_scatter.pdf"), p5, width = 8, height = 6)
  ggsave(file.path(FIG_DIR, "round_tripping_scatter.png"), p5, width = 8, height = 6, dpi = 300)
  cat("  Round-tripping scatter: ", nrow(rt_pairs), " politician-station pairs\n", sep = "")
}

# Figure 6: Self-dealing spending comparison
sd_cpfs <- self_dealing$politician_cpf
if (length(sd_cpfs) > 0) {
  panel[, self_dealer := politician_cpf %in% sd_cpfs]
  sd_compare <- panel[, .(
    mean_spending = mean(total_spending),
    median_spending = median(total_spending),
    mean_hhi = mean(hhi, na.rm = TRUE)
  ), by = .(self_dealer, office)]
  sd_compare <- sd_compare[office %in% c("dep_federal", "dep_estadual", "vereador")]

  p6 <- ggplot(sd_compare, aes(x = office, y = mean_spending / 1000, fill = self_dealer)) +
    geom_col(position = "dodge") +
    labs(x = "Office", y = "Mean Annual Fuel Spending (R$ thousands)",
         fill = "Owns Station?",
         title = "Self-Dealing: Mean Fuel Spending by Station Ownership") +
    scale_fill_manual(values = c("FALSE" = "steelblue", "TRUE" = "tomato"),
                      labels = c("No", "Yes")) +
    theme_minimal()
  ggsave(file.path(FIG_DIR, "self_dealing_comparison.pdf"), p6, width = 8, height = 5)
  ggsave(file.path(FIG_DIR, "self_dealing_comparison.png"), p6, width = 8, height = 5, dpi = 300)
}

cat("\nFigures saved to:", FIG_DIR, "\n")

# ============================================================
# 6. Summary Statistics Table
# ============================================================
cat("\n=== 6. Summary Statistics ===\n")

# Panel-level summary
summary_stats <- panel[, .(
  Variable = c("Total Spending (R$)", "N Transactions", "N Stations", "HHI",
               "Owns Station", "Self-Dealing"),
  Mean = c(mean(total_spending), mean(n_transactions), mean(n_stations),
           mean(hhi, na.rm = TRUE), mean(owns_station), mean(self_dealing)),
  SD = c(sd(total_spending), sd(n_transactions), sd(n_stations),
         sd(hhi, na.rm = TRUE), sd(owns_station), sd(self_dealing)),
  Min = c(min(total_spending), min(n_transactions), min(n_stations),
          min(hhi, na.rm = TRUE), 0, 0),
  Max = c(max(total_spending), max(n_transactions), max(n_stations),
          max(hhi, na.rm = TRUE), 1, 1),
  N = rep(.N, 6)
)]
summary_stats[, `:=`(Mean = round(Mean, 2), SD = round(SD, 2),
                      Min = round(Min, 2), Max = round(Max, 2))]

cat("\n")
print(summary_stats)
fwrite(summary_stats, file.path(TABLE_DIR, "summary_statistics.csv"))

cat("\n=== Phase 4 Complete ===\n")
cat("Tables in:", TABLE_DIR, "\n")
cat("Figures in:", FIG_DIR, "\n")
