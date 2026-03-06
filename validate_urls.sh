#!/usr/bin/env bash
# validate_urls.sh
# Smoke-test all data source URLs with HEAD requests.
# Usage: bash validate_urls.sh

set -euo pipefail

PASS=0
FAIL=0
FAILURES=""
RESULTS="source,label,url,status,ok"

check_url() {
  local source="$1"
  local label="$2"
  local url="$3"

  status=$(curl -s -o /dev/null -w "%{http_code}" --head --max-time 15 -L "$url" 2>/dev/null || echo "000")

  if [[ "$status" -ge 200 && "$status" -lt 400 ]]; then
    printf "  [ OK ] %-45s %s\n" "$label" "$status"
    PASS=$((PASS + 1))
    RESULTS="$RESULTS"$'\n'"$source,$label,$url,$status,TRUE"
  else
    printf "  [FAIL] %-45s %s\n" "$label" "$status"
    FAIL=$((FAIL + 1))
    FAILURES="$FAILURES  [$status] $label\n         $url\n"
    RESULTS="$RESULTS"$'\n'"$source,$label,$url,$status,FALSE"
  fi
}

echo "=== CEAP (Câmara dos Deputados) ==="
for year in $(seq 2008 2025); do
  check_url "ceap" "Câmara CEAP $year" \
    "https://www.camara.leg.br/cotas/Ano-${year}.csv.zip"
done

echo ""
echo "=== CEAPS (Senado Federal) ==="
for year in $(seq 2008 2025); do
  check_url "ceaps" "Senado CEAPS $year" \
    "https://www.senado.leg.br/transparencia/LAI/verba/${year}.csv"
done

echo ""
echo "=== TSE (Campaign Finance) ==="
base="https://cdn.tse.jus.br/estatistica/sead/odsele/prestacao_contas"
for year in 2014 2016 2018 2020 2022 2024; do
  check_url "tse" "TSE despesas_contratadas $year" \
    "${base}/despesas_contratadas_candidatos_${year}.zip"
  check_url "tse" "TSE prestacao_contas (alt) $year" \
    "${base}/prestacao_de_contas_eleitorais_candidatos_${year}.zip"
done

echo ""
echo "=== State Tier 1: Programmatic downloads ==="
for year in 2019 2022 2025; do
  check_url "states/SC" "SC ALESC $year" \
    "https://transparencia.alesc.sc.gov.br/gabinetes_csv.php?ano=${year}"
done
for year in 2015 2020 2025; do
  check_url "states/SP" "SP ALESP $year" \
    "https://www.al.sp.gov.br/repositorioDados/deputados/despesas_gabinetes_${year}.zip"
done
for ym in "2019/1" "2022/6" "2025/1"; do
  check_url "states/MG" "MG ALMG $ym" \
    "https://dadosabertos.almg.gov.br/ws/prestacao_contas/verbas_indenizatorias/deputados/${ym}?formato=json"
done
check_url "states/DF" "DF CLDF portal" \
  "https://dados.cl.df.gov.br/dataset/verbas-indenizatorias"
for year in 2019 2022 2025; do
  check_url "states/PE" "PE ALEPE $year" \
    "https://dadosabertos.alepe.pe.gov.br/api/verbas_indenizatorias/${year}?formato=csv"
done
for year in 2019 2022 2025; do
  check_url "states/CE" "CE ALCE $year" \
    "https://www2.al.ce.gov.br/api/despesas/verbas_indenizatorias/${year}?formato=csv"
done
for year in 2019 2022 2025; do
  check_url "states/RS" "RS ALERS $year" \
    "http://www2.al.rs.gov.br/transparenciaalrs/DadosAbertos/Despesas_${year}.csv"
done
check_url "states/PI" "PI ALEPI portal" \
  "https://www.al.pi.leg.br/transparencia-menu/dados-abertos"
check_url "states/RN" "RN ALERN portal" \
  "https://transparencia.al.rn.leg.br"

echo ""
echo "=== State Tier 2: Transparency portals ==="
check_url "states/AC" "AC portal" "https://app.al.ac.leg.br"
check_url "states/AL" "AL portal" "https://www.al.al.leg.br/transparencia"
check_url "states/AM" "AM portal" "https://www.aleam.gov.br/transparencia/despesa/"
check_url "states/ES" "ES portal" "https://www.al.es.gov.br/Transparencia"
check_url "states/GO" "GO portal" "https://transparencia.al.go.leg.br"
check_url "states/MT" "MT portal" "https://www.al.mt.gov.br/transparencia/"
check_url "states/PR" "PR portal" "https://transparencia.assembleia.pr.leg.br"
check_url "states/PA" "PA portal" "https://www.alepa.pa.gov.br/Home/Page/PORTALDATRANSPARENCIA"
check_url "states/RO" "RO portal" "https://transparencia.al.ro.leg.br"
check_url "states/MS" "MS portal" "https://www.transparencia.al.ms.gov.br/pages/"
check_url "states/AP" "AP portal" "https://al.ap.leg.br/transparencia/"

echo ""
echo "=== State Tier 3: Manual-only portals ==="
check_url "states/BA" "BA portal" "https://www.al.ba.gov.br/transparencia"
check_url "states/MA" "MA portal" "https://sistemas.al.ma.leg.br/transparencia/"
check_url "states/PB" "PB portal" "https://www.al.pb.leg.br/transparencia"
check_url "states/RJ" "RJ portal" "https://transparencia.alerj.rj.gov.br"
check_url "states/RR" "RR portal" "https://transparencia.al.rr.leg.br"
check_url "states/SE" "SE portal" "https://al.se.leg.br/portal-da-transparencia/"
check_url "states/TO" "TO portal" "https://www.al.to.leg.br/transparencia"

echo ""
echo "=== SUMMARY ==="
TOTAL=$((PASS + FAIL))
echo "Total: $TOTAL  |  OK: $PASS  |  FAIL: $FAIL"

if [[ -n "$FAILURES" ]]; then
  echo ""
  echo "Failed URLs:"
  echo -e "$FAILURES"
fi

echo "$RESULTS" > url_validation_results.csv
echo "Full results saved to: url_validation_results.csv"
