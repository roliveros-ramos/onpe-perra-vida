library(httr2)
library(jsonlite)
library(lubridate)

source("src/download_onpe_ubigeo.R")

curl_txt = r"(curl 'https://resultadosegundavuelta.onpe.gob.pe/presentacion-backend/resumen-general/participantes?idEleccion=10&tipoFiltro=ubigeo_nivel_03&idAmbitoGeografico=1&idUbigeoDepartamento=010000&idUbigeoProvincia=010200&idUbigeoDistrito=010205' \
  -H 'accept: */*' \
  -H 'accept-language: en-ZA,en;q=0.9,es-419;q=0.8,es;q=0.7,fr;q=0.6' \
  -H 'content-type: application/json' \
  -H 'priority: u=1, i' \
  -H 'referer: https://resultadosegundavuelta.onpe.gob.pe/main/resumen' \
  -H 'sec-ch-ua: "Google Chrome";v="149", "Chromium";v="149", "Not)A;Brand";v="24"' \
  -H 'sec-ch-ua-mobile: ?0' \
  -H 'sec-ch-ua-platform: "Windows"' \
  -H 'sec-fetch-dest: empty' \
  -H 'sec-fetch-mode: cors' \
  -H 'sec-fetch-site: same-origin' \
  -H 'user-agent: Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/149.0.0.0 Safari/537.36')"

headers = headers_from_curl(curl_txt)

ubigeos = sprintf("%06d", read.csv("padron.csv")$ubigeo)

all_data = download_onpe_ubigeos_v14(
  ubigeos = ubigeos,
  headers = headers,
  header_mode = "curl_only"
)

totales = all_data$totales
participantes = all_data$participantes
failures = all_data$failures

write.csv(participantes, 
          file=sprintf("results/%s.csv", format(Sys.time(), format = "%Y%m%d%H%M")))

raise = 100/participantes$actasContabilizadas
raise[participantes$actasContabilizadas<50] = 1
# raise[is.infinite(raise)] = 1
participantes$proj = participantes$totalVotosValidos*raise
JP = sum(participantes$proj[participantes$codigoAgrupacionPolitica==10])
K = sum(participantes$proj[participantes$codigoAgrupacionPolitica==8])
JP - K
