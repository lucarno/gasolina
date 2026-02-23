# build_linkage.R
# Phase 2: Data Linkage & Entity Resolution
#
# Builds three master tables:
#   1. politicians  — unique politicians with CPF, name, party, office level
#   2. stations     — unique gas stations with CNPJ, name, location, ownership
#   3. connections  — edges linking politicians to stations (spending, donations, ownership)
#
# Input:  data/filtered/{ceap,ceaps,tse,tse_candidatos,states,anp}/ + data/raw/socios.rds
# Output: data/linked/{politicians.parquet, stations.parquet, connections.parquet}

library(data.table)

OUT_DIR <- "data/linked"
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

# ============================================================
# Helper functions
# ============================================================

#' Clean a CNPJ string to 14-digit zero-padded format
#' Handles: formatted strings ("12.345.678/0001-90"), integer64, numeric, plain strings
clean_cnpj <- function(x) {
  x <- as.character(x)
  x <- gsub("[^0-9]", "", x)             # strip non-digits
  x[nchar(x) == 0] <- NA_character_
  x[!is.na(x)] <- sprintf("%014s", x[!is.na(x)])  # left-pad with zeros
  x[!is.na(x)] <- gsub(" ", "0", x[!is.na(x)])    # sprintf %s pads with spaces
  # Validate: must be exactly 14 digits
  x[!is.na(x) & nchar(x) != 14] <- NA_character_
  x
}

#' Clean a CPF string to 11-digit zero-padded format
clean_cpf <- function(x) {
  x <- as.character(x)
  x <- gsub("[^0-9]", "", x)
  x[nchar(x) == 0] <- NA_character_
  x[!is.na(x)] <- sprintf("%011s", x[!is.na(x)])
  x[!is.na(x)] <- gsub(" ", "0", x[!is.na(x)])
  x[!is.na(x) & nchar(x) != 11] <- NA_character_
  x
}

#' Normalize a name: uppercase, remove accents, collapse whitespace
clean_name <- function(x) {
  x <- toupper(trimws(x))
  x <- iconv(x, to = "ASCII//TRANSLIT")
  x <- gsub("[^A-Z ]", "", x)  # keep only letters and spaces
  x <- gsub("\\s+", " ", x)
  x[x == "" | x == " "] <- NA_character_
  x
}

#' Extract year from a date string (tries multiple formats)
extract_year <- function(x) {
  x <- as.character(x)
  # Try YYYY-MM-DD
  y <- as.integer(substr(x, 1, 4))
  # Try DD/MM/YYYY
  bad <- is.na(y) | y < 1990 | y > 2030
  if (any(bad, na.rm = TRUE)) {
    y2 <- as.integer(substr(x[bad], 7, 10))
    y[bad] <- y2
  }
  y
}

cat("=== Phase 2: Data Linkage & Entity Resolution ===\n\n")

# ============================================================
# 1. Load and standardize all datasets
# ============================================================

# --- 1a. Candidatos (TSE candidate registry) ---
cat("Loading candidatos...\n")
cand_files <- list.files("data/filtered/tse_candidatos", pattern = "\\.csv$", full.names = TRUE)
cand_list <- lapply(cand_files, function(f) {
  dt <- fread(f, select = c("ANO_ELEICAO", "SG_UF", "NR_CPF_CANDIDATO",
                             "NM_CANDIDATO", "NM_URNA_CANDIDATO",
                             "SG_PARTIDO", "CD_CARGO", "DS_CARGO",
                             "SQ_CANDIDATO", "DS_SIT_TOT_TURNO",
                             "DS_SITUACAO_CANDIDATURA"),
              colClasses = list(character = c("NR_CPF_CANDIDATO", "SQ_CANDIDATO")))
  dt
})
candidatos <- rbindlist(cand_list, fill = TRUE)
candidatos[, cpf := clean_cpf(NR_CPF_CANDIDATO)]
candidatos[, nome_clean := clean_name(NM_CANDIDATO)]
cat("  ", nrow(candidatos), "candidate-election records,",
    uniqueN(candidatos$cpf, na.rm = TRUE), "unique CPFs\n")

# --- 1b. CEAP (Câmara dos Deputados) ---
cat("Loading CEAP...\n")
ceap_files <- list.files("data/filtered/ceap", pattern = "\\.csv$", full.names = TRUE)
ceap_list <- lapply(ceap_files, function(f) {
  dt <- fread(f, select = c("txNomeParlamentar", "cpf", "txtCNPJCPF",
                             "sgPartido", "sgUF", "vlrLiquido",
                             "numAno", "numMes", "txtFornecedor"),
              colClasses = list(character = c("cpf", "txtCNPJCPF")))
  dt
})
ceap <- rbindlist(ceap_list, fill = TRUE)
ceap[, politician_cpf := clean_cpf(cpf)]
ceap[, supplier_cnpj := clean_cnpj(txtCNPJCPF)]
ceap[, politician_name := clean_name(txNomeParlamentar)]
cat("  ", nrow(ceap), "CEAP records\n")

# --- 1c. CEAPS (Senado Federal) ---
cat("Loading CEAPS...\n")
ceaps_files <- list.files("data/filtered/ceaps", pattern = "\\.csv$", full.names = TRUE)
ceaps_list <- lapply(ceaps_files, function(f) {
  dt <- fread(f, colClasses = "character")
  dt
})
ceaps <- rbindlist(ceaps_list, fill = TRUE)
ceaps[, supplier_cnpj := clean_cnpj(CNPJ_CPF)]
ceaps[, politician_name := clean_name(SENADOR)]
# Parse value
ceaps[, valor := as.numeric(gsub(",", ".", gsub("[^0-9,.-]", "", VALOR_REEMBOLSADO)))]
cat("  ", nrow(ceaps), "CEAPS records\n")

# --- 1d. TSE Expenditures ---
cat("Loading TSE expenditures...\n")
tse_exp_files <- list.files("data/filtered/tse", pattern = "^tse_combustivel_",
                             full.names = TRUE)
tse_exp_list <- lapply(tse_exp_files, function(f) {
  dt <- fread(f, select = c("AA_ELEICAO", "SG_UF", "NR_CPF_CANDIDATO",
                             "NM_CANDIDATO", "SQ_CANDIDATO", "SG_PARTIDO",
                             "DS_CARGO", "NR_CPF_CNPJ_FORNECEDOR",
                             "NM_FORNECEDOR", "CD_CNAE_FORNECEDOR",
                             "NM_MUNICIPIO_FORNECEDOR", "SG_UF_FORNECEDOR",
                             "DT_DESPESA", "VR_DESPESA_CONTRATADA"),
              colClasses = list(character = c("NR_CPF_CANDIDATO", "SQ_CANDIDATO",
                                              "NR_CPF_CNPJ_FORNECEDOR",
                                              "CD_CNAE_FORNECEDOR")))
  dt
})
tse_exp <- rbindlist(tse_exp_list, fill = TRUE)
tse_exp[, politician_cpf := clean_cpf(NR_CPF_CANDIDATO)]
tse_exp[, supplier_cnpj := clean_cnpj(NR_CPF_CNPJ_FORNECEDOR)]
tse_exp[, valor := as.numeric(gsub(",", ".", gsub("[^0-9,.-]", "", VR_DESPESA_CONTRATADA)))]
cat("  ", nrow(tse_exp), "TSE expenditure records\n")

# --- 1e. TSE Receipts (donations from gas stations) ---
cat("Loading TSE receipts...\n")
tse_rec_files <- list.files("data/filtered/tse", pattern = "^tse_receitas_",
                             full.names = TRUE)
tse_rec_list <- lapply(tse_rec_files, function(f) {
  dt <- fread(f, select = c("AA_ELEICAO", "SG_UF", "NR_CPF_CANDIDATO",
                             "NM_CANDIDATO", "SQ_CANDIDATO", "SG_PARTIDO",
                             "DS_CARGO", "NR_CPF_CNPJ_DOADOR",
                             "NM_DOADOR", "CD_CNAE_DOADOR",
                             "NM_MUNICIPIO_DOADOR", "SG_UF_DOADOR",
                             "DT_RECEITA", "VR_RECEITA"),
              colClasses = list(character = c("NR_CPF_CANDIDATO", "SQ_CANDIDATO",
                                              "NR_CPF_CNPJ_DOADOR",
                                              "CD_CNAE_DOADOR")))
  dt
})
tse_rec <- rbindlist(tse_rec_list, fill = TRUE)
tse_rec[, politician_cpf := clean_cpf(NR_CPF_CANDIDATO)]
tse_rec[, donor_cnpj := clean_cnpj(NR_CPF_CNPJ_DOADOR)]
tse_rec[, valor := as.numeric(gsub(",", ".", gsub("[^0-9,.-]", "", VR_RECEITA)))]
cat("  ", nrow(tse_rec), "TSE receipt records\n")

# --- 1f. States ---
cat("Loading state data...\n")

# DF
df_files <- list.files("data/filtered/states/DF", pattern = "\\.csv$", full.names = TRUE)
states_df <- rbindlist(lapply(df_files, function(f) {
  dt <- fread(f, colClasses = "character")
  data.table(
    politician_name = clean_name(dt$NOME_PARLAMENTAR),
    politician_cpf  = clean_cpf(dt$CPF_PARLAMENTAR),
    supplier_cnpj   = clean_cnpj(dt$CNPJ_PRESTADOR),
    supplier_name   = dt$NOME_PRESTADOR,
    valor           = as.numeric(gsub(",", ".", gsub("[^0-9,.-]", "", dt$VALOR_DESPESA))),
    date            = dt$DATA_COMPROVANTE,
    state_source    = "DF"
  )
}), fill = TRUE)

# MG
mg_file <- "data/filtered/states/MG/mg_combustivel.csv"
if (file.exists(mg_file)) {
  dt <- fread(mg_file, colClasses = "character")
  states_mg <- data.table(
    politician_name = clean_name(dt$NomeDeputado),
    politician_cpf  = NA_character_,
    supplier_cnpj   = clean_cnpj(dt$CpfCnpj),
    supplier_name   = dt$Emitente,
    valor           = as.numeric(gsub(",", ".", gsub("[^0-9,.-]", "", dt$ValorReembolso))),
    date            = dt$Emissao,
    state_source    = "MG"
  )
} else {
  states_mg <- data.table()
}

# SP
sp_files <- list.files("data/filtered/states/SP", pattern = "\\.csv$", full.names = TRUE)
states_sp <- rbindlist(lapply(sp_files, function(f) {
  dt <- fread(f, colClasses = "character")
  data.table(
    politician_name = clean_name(dt$Deputado),
    politician_cpf  = NA_character_,
    supplier_cnpj   = clean_cnpj(dt$CNPJ),
    supplier_name   = dt$Fornecedor,
    valor           = as.numeric(gsub(",", ".", gsub("[^0-9,.-]", "", dt$Valor))),
    date            = NA_character_,
    state_source    = "SP"
  )
}), fill = TRUE)

# SC
sc_files <- list.files("data/filtered/states/SC", pattern = "\\.csv$", full.names = TRUE)
states_sc <- rbindlist(lapply(sc_files, function(f) {
  dt <- fread(f, colClasses = "character")
  # SC has Conta (deputy name) and Favorecido (supplier name), no CNPJ
  data.table(
    politician_name = clean_name(dt$Conta),
    politician_cpf  = NA_character_,
    supplier_cnpj   = NA_character_,
    supplier_name   = dt$Favorecido,
    valor           = as.numeric(gsub(",", ".", gsub("[^0-9,.-]", "", dt$Valor))),
    date            = as.character(dt$Vencimento),
    state_source    = "SC"
  )
}), fill = TRUE)

states_all <- rbindlist(list(states_df, states_mg, states_sp, states_sc), fill = TRUE)
cat("  ", nrow(states_all), "state records (DF:", nrow(states_df),
    "MG:", nrow(states_mg), "SP:", nrow(states_sp), "SC:", nrow(states_sc), ")\n")

# --- 1g. ANP Stations ---
cat("Loading ANP stations...\n")
anp_stations <- fread("data/filtered/anp/anp_stations.csv")
anp_stations[, cnpj := clean_cnpj(cnpj_revenda)]
cat("  ", nrow(anp_stations), "ANP station records\n")

# --- 1h. QSA (firm ownership) ---
cat("Loading QSA (socios.rds)...\n")
socios <- readRDS("data/raw/socios.rds")
# Filter to gas stations (CNAE 4731-8/00 = retail fuel)
gas_socios <- socios[cnae_fiscal == 4731800]
gas_socios[, cnpj_clean := clean_cnpj(as.character(bit64::as.integer64(cnpj)))]
gas_socios[, socio_name := clean_name(nome_socio)]
cat("  ", nrow(gas_socios), "gas station ownership records,",
    uniqueN(gas_socios$cnpj_clean, na.rm = TRUE), "unique stations\n")

rm(socios)  # free memory
gc()

cat("\n")

# ============================================================
# 2. Build politician master table
# ============================================================
cat("=== Building politician master table ===\n")

# Primary source: candidatos registry (has CPF, name, party, office)
pol_from_cand <- candidatos[!is.na(cpf), .(
  nome = NM_CANDIDATO[1],
  nome_urna = NM_URNA_CANDIDATO[1],
  partidos = paste(unique(na.omit(SG_PARTIDO)), collapse = ";"),
  cargos = paste(unique(na.omit(DS_CARGO)), collapse = ";"),
  anos_eleicao = paste(sort(unique(na.omit(as.integer(ANO_ELEICAO)))), collapse = ";"),
  eleito = any(grepl("ELEITO|MEDIA", DS_SIT_TOT_TURNO, ignore.case = TRUE))
), by = .(cpf)]

# Add CEAP deputies not in candidatos
ceap_pols <- ceap[!is.na(politician_cpf), .(
  nome = txNomeParlamentar[1],
  sgPartido = sgPartido[1],
  sgUF = sgUF[1]
), by = .(cpf = politician_cpf)]
# Keep only those not already in candidatos
ceap_new <- ceap_pols[!cpf %in% pol_from_cand$cpf]
if (nrow(ceap_new) > 0) {
  ceap_new[, `:=`(nome_urna = NA_character_,
                  partidos = sgPartido,
                  cargos = "Deputado Federal",
                  anos_eleicao = NA_character_,
                  eleito = TRUE)]
  ceap_new[, c("sgPartido", "sgUF") := NULL]
}

# Add DF state deputies with CPF
df_pols <- states_all[state_source == "DF" & !is.na(politician_cpf), .(
  nome = politician_name[1]
), by = .(cpf = politician_cpf)]
df_new <- df_pols[!cpf %in% c(pol_from_cand$cpf, ceap_new$cpf)]
if (nrow(df_new) > 0) {
  df_new[, `:=`(nome_urna = NA_character_,
                partidos = NA_character_,
                cargos = "Deputado Distrital",
                anos_eleicao = NA_character_,
                eleito = TRUE)]
}

politicians <- rbindlist(list(pol_from_cand, ceap_new, df_new), fill = TRUE)
politicians[, nome_clean := clean_name(nome)]

cat("  ", nrow(politicians), "unique politicians with CPF\n")

# For CEAPS senators without CPF, try to match by name to candidatos
ceaps_names <- unique(ceaps[!is.na(politician_name), .(politician_name)])
ceaps_matched <- merge(ceaps_names, politicians[, .(cpf, nome_clean)],
                       by.x = "politician_name", by.y = "nome_clean",
                       all.x = TRUE)
cat("   CEAPS senators matched by name:", sum(!is.na(ceaps_matched$cpf)),
    "/", nrow(ceaps_matched), "\n")

# Create a name-to-CPF lookup for senators not matched
# Add unmatched CEAPS senators as politicians without CPF
ceaps_unmatched <- ceaps_matched[is.na(cpf)]$politician_name
if (length(ceaps_unmatched) > 0) {
  ceaps_add <- data.table(
    cpf = paste0("CEAPS_", seq_along(ceaps_unmatched)),  # synthetic ID
    nome = ceaps_unmatched,
    nome_urna = NA_character_,
    partidos = NA_character_,
    cargos = "Senador",
    anos_eleicao = NA_character_,
    eleito = TRUE,
    nome_clean = ceaps_unmatched
  )
  politicians <- rbindlist(list(politicians, ceaps_add), fill = TRUE)
  cat("   Added", nrow(ceaps_add), "CEAPS senators without CPF match\n")
}

# Similarly for MG/SP/SC state deputies without CPF
for (src in c("MG", "SP", "SC")) {
  st_names <- unique(states_all[state_source == src & is.na(politician_cpf) &
                                 !is.na(politician_name), .(politician_name)])
  st_matched <- merge(st_names, politicians[, .(cpf, nome_clean)],
                      by.x = "politician_name", by.y = "nome_clean", all.x = TRUE)
  st_unmatched <- st_matched[is.na(cpf)]$politician_name
  if (length(st_unmatched) > 0) {
    st_add <- data.table(
      cpf = paste0(src, "_", seq_along(st_unmatched)),
      nome = st_unmatched,
      nome_urna = NA_character_,
      partidos = NA_character_,
      cargos = paste0("Deputado Estadual (", src, ")"),
      anos_eleicao = NA_character_,
      eleito = TRUE,
      nome_clean = st_unmatched
    )
    politicians <- rbindlist(list(politicians, st_add), fill = TRUE)
    cat("   Added", nrow(st_add), "unmatched", src, "state deputies\n")
  }
}

cat("   Total politicians:", nrow(politicians), "\n\n")

# ============================================================
# 3. Build gas station master table
# ============================================================
cat("=== Building gas station master table ===\n")

# Collect all unique CNPJs from every dataset
all_cnpjs <- unique(na.omit(c(
  ceap$supplier_cnpj,
  ceaps$supplier_cnpj,
  tse_exp$supplier_cnpj,
  tse_rec$donor_cnpj,
  states_all$supplier_cnpj,
  anp_stations$cnpj,
  gas_socios$cnpj_clean
)))
cat("  Total unique fuel-related CNPJs across all sources:", length(all_cnpjs), "\n")

# Start with ANP station registry (has name, location, brand)
stations <- anp_stations[!is.na(cnpj), .(
  nome_fantasia = Revenda[1],
  municipio = Municipio[1],
  uf = uf[1],
  bandeira = Bandeira[1]
), by = .(cnpj)]

# Add CNPJs from QSA that aren't in ANP
qsa_stations <- gas_socios[!is.na(cnpj_clean), .(
  razao_social = razao_social[1]
), by = .(cnpj = cnpj_clean)]
qsa_new <- qsa_stations[!cnpj %in% stations$cnpj]
if (nrow(qsa_new) > 0) {
  qsa_new[, `:=`(nome_fantasia = razao_social,
                 municipio = NA_character_,
                 uf = NA_character_,
                 bandeira = NA_character_)]
  qsa_new[, razao_social := NULL]
  stations <- rbindlist(list(stations, qsa_new), fill = TRUE)
}

# Add CNPJs from TSE expenditures that aren't in either ANP or QSA
tse_suppliers <- tse_exp[!is.na(supplier_cnpj), .(
  nome = NM_FORNECEDOR[1],
  municipio = NM_MUNICIPIO_FORNECEDOR[1],
  uf = SG_UF_FORNECEDOR[1]
), by = .(cnpj = supplier_cnpj)]
tse_new <- tse_suppliers[!cnpj %in% stations$cnpj]
if (nrow(tse_new) > 0) {
  tse_new[, `:=`(nome_fantasia = nome, bandeira = NA_character_)]
  tse_new[, nome := NULL]
  stations <- rbindlist(list(stations, tse_new), fill = TRUE)
}

# Add remaining CNPJs from CEAP/CEAPS/states
other_cnpjs <- data.table(cnpj = setdiff(all_cnpjs, stations$cnpj))
if (nrow(other_cnpjs) > 0) {
  # Try to get names from CEAP
  ceap_names <- ceap[!is.na(supplier_cnpj), .(
    nome = txtFornecedor[1]
  ), by = .(cnpj = supplier_cnpj)]
  other_cnpjs <- merge(other_cnpjs, ceap_names, by = "cnpj", all.x = TRUE)

  # Try CEAPS names for those still missing
  ceaps_names_dt <- ceaps[!is.na(supplier_cnpj), .(
    nome2 = FORNECEDOR[1]
  ), by = .(cnpj = supplier_cnpj)]
  other_cnpjs <- merge(other_cnpjs, ceaps_names_dt, by = "cnpj", all.x = TRUE)
  other_cnpjs[is.na(nome), nome := nome2]
  other_cnpjs[, nome2 := NULL]

  other_cnpjs[, `:=`(nome_fantasia = nome,
                     municipio = NA_character_,
                     uf = NA_character_,
                     bandeira = NA_character_)]
  other_cnpjs[, nome := NULL]
  stations <- rbindlist(list(stations, other_cnpjs), fill = TRUE)
}

# Enrich stations with ownership data from QSA
# Count number of partners and flag those with politician connections (built later)
ownership <- gas_socios[!is.na(cnpj_clean) & !is.na(socio_name), .(
  n_socios = .N,
  socios = paste(unique(nome_socio), collapse = "; "),
  socios_clean = paste(unique(socio_name), collapse = "; ")
), by = .(cnpj = cnpj_clean)]

stations <- merge(stations, ownership, by = "cnpj", all.x = TRUE)
stations[is.na(n_socios), n_socios := 0L]

cat("  Total stations:", nrow(stations), "\n")
cat("    With ANP data:", sum(!is.na(stations$bandeira)), "\n")
cat("    With ownership data:", sum(stations$n_socios > 0), "\n\n")

# ============================================================
# 4. Build connections (edges)
# ============================================================
cat("=== Building connection edges ===\n")

# --- 4a. CEAP spending ---
cat("  Processing CEAP connections...\n")
conn_ceap <- ceap[!is.na(supplier_cnpj), .(
  politician_cpf = politician_cpf,
  politician_name = politician_name,
  station_cnpj = supplier_cnpj,
  type = "ceap_spending",
  year = as.integer(numAno),
  valor = vlrLiquido,
  source = "ceap"
)]
# Resolve CPF for name-only entries using politician lookup
conn_ceap[is.na(politician_cpf) & !is.na(politician_name),
          politician_cpf := politicians$cpf[match(politician_name, politicians$nome_clean)]]

# --- 4b. CEAPS spending ---
cat("  Processing CEAPS connections...\n")
# Build name-to-CPF lookup for CEAPS
ceaps_cpf_lookup <- rbind(
  ceaps_matched[!is.na(cpf), .(politician_name, cpf)],
  data.table(politician_name = ceaps_unmatched,
             cpf = paste0("CEAPS_", seq_along(ceaps_unmatched)))
)
conn_ceaps <- ceaps[!is.na(supplier_cnpj), .(
  politician_name = politician_name,
  station_cnpj = supplier_cnpj,
  type = "ceaps_spending",
  year = as.integer(ANO),
  valor = valor,
  source = "ceaps"
)]
conn_ceaps[, politician_cpf := ceaps_cpf_lookup$cpf[match(politician_name, ceaps_cpf_lookup$politician_name)]]

# --- 4c. TSE campaign expenditures ---
cat("  Processing TSE expenditure connections...\n")
conn_tse_exp <- tse_exp[!is.na(supplier_cnpj) & !is.na(politician_cpf), .(
  politician_cpf = politician_cpf,
  politician_name = clean_name(NM_CANDIDATO),
  station_cnpj = supplier_cnpj,
  type = "campaign_spending",
  year = as.integer(AA_ELEICAO),
  valor = valor,
  source = "tse_expenditure"
)]

# --- 4d. TSE campaign donations ---
cat("  Processing TSE receipt connections...\n")
conn_tse_rec <- tse_rec[!is.na(donor_cnpj) & !is.na(politician_cpf), .(
  politician_cpf = politician_cpf,
  politician_name = clean_name(NM_CANDIDATO),
  station_cnpj = donor_cnpj,
  type = "campaign_donation",
  year = as.integer(AA_ELEICAO),
  valor = valor,
  source = "tse_receipt"
)]

# --- 4e. State spending ---
cat("  Processing state connections...\n")
conn_states <- states_all[!is.na(supplier_cnpj), .(
  politician_cpf = politician_cpf,
  politician_name = politician_name,
  station_cnpj = supplier_cnpj,
  type = "state_spending",
  year = extract_year(date),
  valor = valor,
  source = paste0("state_", state_source)
)]
# Resolve CPF where missing via politician lookup
conn_states[is.na(politician_cpf) & !is.na(politician_name),
            politician_cpf := politicians$cpf[match(politician_name, politicians$nome_clean)]]

# --- 4f. Ownership connections ---
cat("  Processing ownership connections...\n")

# Match QSA partner names to politician names
# This is the key linkage: does a politician (or someone with same name) own a gas station?
pol_names <- politicians[, .(cpf, nome_clean)]
setkey(pol_names, nome_clean)

owner_pol <- gas_socios[!is.na(socio_name) & !is.na(cnpj_clean)]
owner_pol <- merge(owner_pol, pol_names, by.x = "socio_name", by.y = "nome_clean",
                   allow.cartesian = TRUE)

if (nrow(owner_pol) > 0) {
  conn_ownership <- owner_pol[, .(
    politician_cpf = cpf,
    politician_name = socio_name,
    station_cnpj = cnpj_clean,
    type = "ownership",
    year = as.integer(year_start),
    valor = NA_real_,
    source = "qsa"
  )]
  cat("    Found", nrow(conn_ownership), "ownership connections\n")
} else {
  conn_ownership <- data.table(
    politician_cpf = character(), politician_name = character(),
    station_cnpj = character(), type = character(),
    year = integer(), valor = numeric(), source = character()
  )
  cat("    No ownership connections found\n")
}

# Combine all connections
connections <- rbindlist(list(
  conn_ceap, conn_ceaps, conn_tse_exp, conn_tse_rec,
  conn_states, conn_ownership
), fill = TRUE)

cat("\n  Total connections:", nrow(connections), "\n")
cat("  By type:\n")
print(connections[, .(records = .N, total_valor = sum(valor, na.rm = TRUE)), by = type])

# ============================================================
# 5. Flag suspicious connections
# ============================================================
cat("\n=== Flagging suspicious connections ===\n")

# Flag 1: Politician owns a gas station AND spends money there
spending_types <- c("ceap_spending", "ceaps_spending", "campaign_spending", "state_spending")
ownership_pairs <- connections[type == "ownership", .(politician_cpf, station_cnpj)]
spending_pairs <- connections[type %in% spending_types, .(politician_cpf, station_cnpj)]

self_dealing <- fintersect(
  unique(ownership_pairs),
  unique(spending_pairs)
)
cat("  Self-dealing (owns station + spends there):", nrow(self_dealing), "\n")

# Flag 2: Politician receives donation from AND spends at same station
donation_pairs <- connections[type == "campaign_donation", .(politician_cpf, station_cnpj)]
round_trip <- fintersect(
  unique(donation_pairs),
  unique(spending_pairs)
)
cat("  Round-tripping (receives donation + spends there):", nrow(round_trip), "\n")

# Flag 3: Politician spends at station owned by fellow party member
# (requires cross-referencing party affiliations)
# Add party info to ownership connections
if (nrow(conn_ownership) > 0) {
  owner_parties <- merge(
    conn_ownership[, .(owner_cpf = politician_cpf, station_cnpj)],
    politicians[, .(cpf, partidos)],
    by.x = "owner_cpf", by.y = "cpf", all.x = TRUE
  )
  setnames(owner_parties, "partidos", "owner_party")

  spender_parties <- merge(
    connections[type %in% spending_types, .(spender_cpf = politician_cpf, station_cnpj)],
    politicians[, .(cpf, partidos)],
    by.x = "spender_cpf", by.y = "cpf", all.x = TRUE
  )
  setnames(spender_parties, "partidos", "spender_party")

  # Find pairs where spender and owner share a party
  party_conn <- merge(owner_parties, spender_parties, by = "station_cnpj",
                      allow.cartesian = TRUE)
  party_conn <- party_conn[owner_cpf != spender_cpf]  # exclude self-dealing

  if (nrow(party_conn) > 0) {
    # Check if any party overlaps
    party_conn[, party_overlap := mapply(function(a, b) {
      if (is.na(a) || is.na(b)) return(FALSE)
      length(intersect(strsplit(a, ";")[[1]], strsplit(b, ";")[[1]])) > 0
    }, owner_party, spender_party)]
    same_party <- party_conn[party_overlap == TRUE]
    cat("  Same-party patronage (spends at co-partisan's station):",
        uniqueN(same_party[, .(spender_cpf, station_cnpj)]), "\n")
  }
}

# ============================================================
# 6. Save outputs
# ============================================================
cat("\n=== Saving outputs ===\n")

# Check if arrow is available for parquet, fall back to CSV
has_arrow <- requireNamespace("arrow", quietly = TRUE)

if (has_arrow) {
  arrow::write_parquet(politicians, file.path(OUT_DIR, "politicians.parquet"))
  arrow::write_parquet(stations, file.path(OUT_DIR, "stations.parquet"))
  arrow::write_parquet(connections, file.path(OUT_DIR, "connections.parquet"))
  cat("  Saved as parquet\n")
} else {
  fwrite(politicians, file.path(OUT_DIR, "politicians.csv"))
  fwrite(stations, file.path(OUT_DIR, "stations.csv"))
  fwrite(connections, file.path(OUT_DIR, "connections.csv"))
  cat("  Saved as CSV (install arrow package for parquet)\n")
}

# Save flag summaries
if (nrow(self_dealing) > 0) {
  fwrite(self_dealing, file.path(OUT_DIR, "flag_self_dealing.csv"))
}
if (nrow(round_trip) > 0) {
  fwrite(round_trip, file.path(OUT_DIR, "flag_round_trip.csv"))
}

cat("\n=== Summary ===\n")
cat("  Politicians:", nrow(politicians), "\n")
cat("  Gas stations:", nrow(stations), "\n")
cat("  Connections:", nrow(connections), "\n")
cat("  Self-dealing flags:", nrow(self_dealing), "\n")
cat("  Round-trip flags:", nrow(round_trip), "\n")
cat("\nDone. Files in:", OUT_DIR, "\n")
