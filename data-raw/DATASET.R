library(tidyverse)
library(lubridate)
library(readxl)
library(fs)

# estacao convencional ----------------------------------------------------

# read_html("http://www.leb.esalq.usp.br/leb/descar.html") %>%
#   html_nodes("tr") %>%
#   '['(6) %>%
#   html_children() %>%
#   html_nodes("a") %>%
#   html_attr('href') %>%
#   basename()

url_base <- "http://www.esalq.usp.br/departamentos/leb/exceldados/"

links <- tibble(
  ano = seq(1917, year(today())),
  url = ifelse(ano < 2000, str_glue("{url_base}ROB{ano-1900}.xls"),  str_glue("{url_base}DCE{ano}.xls")),
  path_arquivo = str_glue("data-raw/raw_files/conventional_{ano}.xls")
)

dir_create("data-raw/raw_files")

for (i in seq_len(nrow(links))) {

  if (str_detect(links$path_arquivo[i], as.character(year(today())))) {
    try({file_delete(links$path_arquivo[i])}, silent = TRUE)
  }

  if (!file.exists(links$path_arquivo[i])) {
    download.file(links$url[i], links$path_arquivo[i], mode="wb", quiet = TRUE)
  }

}

brutos_emc <- list.files("data-raw/raw_files", pattern = "conventional_.+.xls", full.names = TRUE) %>%
  map(~suppressMessages({read_excel(.x)})) %>%
  map(~mutate_all(.x, as.character)) %>%
  bind_rows() %>%
  select(1:22) %>%
  set_names(
    c(
      "nda", "ano", "dia", "mes", "rad_global", "ins", "ppt", "ur_med", "v_max",	"v_med", "t_max", "t_min",
      "t_med", "evp",  "pa", "amp", "t_som", "D7", "D14", "D21", "h_ppt", "t_umi"
    )
  ) %>%
  filter(as.numeric(nda) %in% 1:367)

brutos_emc[brutos_emc$nda == 246 & brutos_emc$ano == 1929, "mes"] <- "SET"
brutos_emc[brutos_emc$nda == 301 & brutos_emc$ano == 1933, "dia"] <- 28
brutos_emc[brutos_emc$nda == 120 & brutos_emc$ano == 1943, "dia"] <- 30
brutos_emc[brutos_emc$nda == 165 & brutos_emc$ano == 1948, "dia"] <- 13
brutos_emc[brutos_emc$nda == 120 & brutos_emc$ano == 1955, "dia"] <- 30
brutos_emc[brutos_emc$nda == 121 & brutos_emc$ano == 1956, "dia"] <- 30
brutos_emc[brutos_emc$nda == 138 & brutos_emc$ano == 1963, "dia"] <- 18

esalqmet_automatic <- brutos_emc %>%
  mutate(data = dmy(str_glue("{dia} {mes} {ano}"))) %>%
  filter(!is.na(data)) %>%
  mutate_if(is.character, as.numeric) %>%
  mutate_if(is.numeric, round, digits = 1) %>%
  select(
    data, ppt, t_max, t_med, t_min, ur_med, ins,
    rad_global, v_max, v_med, evp
  ) %>%
  mutate_if(is.numeric, round, digits = 3)

# skimr::skim(esalq_estacao_convencional)


# estacao automatica ----------------------------------------------------

url_base <- "http://www.esalq.usp.br/departamentos/leb/automatica/jan-dez"

links <- tibble(
  ano = seq(1997, year(today())),
  url = str_glue("{url_base}{ano}.xls"),
  path_arquivo = str_glue("data-raw/raw_files/automatic_{ano}.xls")
)

dir_create("data-raw/raw_files")

for (i in seq_len(nrow(links))) {

  if (str_detect(links$path_arquivo[i], as.character(year(today())))) {
    try({file_delete(links$path_arquivo[i])}, silent = TRUE)
  }

  if (!file.exists(links$path_arquivo[i])) {
    download.file(links$url[i], links$path_arquivo[i], method = "curl", quiet = T)
  }

}

limpa_arquivo_ema <- function(path, ano) {

  names <- read_excel(path, skip = 5) %>% names()

  raw <- read_excel(path, skip = 4)

  token_1 <- which(raw$`---------...1` == "DIA")
  token_2 <- which(raw$`---------...1` == "MED MES")

  linhas_uteis <- flatten_dbl(map2(token_1, token_2, ~seq(.x, .y, 1)))

  aux_dia_mes <- map2(token_1, token_2, ~seq(.x, .y, 1)) %>%
    set_names(month.name) %>%
    purrr::simplify() %>%
    as_tibble(rownames = "mes") %>%
    rename(rowname = value) %>%
    mutate(
      mes = str_remove_all(mes, "\\d"),
      rowname = as.character(rowname)
    )

  raw %>%
    set_names(names) %>%
    rownames_to_column() %>%
    left_join(aux_dia_mes) %>%
    slice(linhas_uteis) %>%
    filter(as.numeric(DIA) %in% 1:366) %>%
    select(-starts_with("Hora")) %>%
    select(-starts_with("...")) %>%
    mutate_at(vars(-mes), as.numeric) %>%
    mutate(
      ano = ano,
      data =  ymd(paste(ano, mes, DIA))
    ) %>%
    filter(ano == year(data)) %>%
    select(-c(rowname, mes, ano))

}

esalqmet_automatic <- list.files("data-raw/raw_files", pattern = "automatic_.+.xls", full.names = TRUE) %>%
  set_names(., str_extract(basename(.), "[:digit:]+")) %>%
  #head(n = 5) %>%
  map2(., names(.), ~suppressMessages({limpa_arquivo_ema(.x, .y)})) %>%
  bind_rows() %>%
  select(
    data,
    ppt = Chuva,
    t_max = TMAX,
    t_med = TMED,
    t_min = TMIN,
    ur_max = `UR MAX`,
    ur_med = `UR MED`,
    ur_min = `UR MIN`,
    rad_global = Rad.Glob.,
    rad_liq = `Rad Liq`,
    v_max = Vel.Vento,
    v_med = Vento
  ) %>%
  mutate_if(is.numeric, round, digits = 3)

# skim(esalq_estacao_automatica)

# prepara base ------------------------------------------------------------

usethis::use_data(esalqmet_conventional, overwrite = TRUE)

usethis::use_data(esalqmet_automatic, overwrite = TRUE)

