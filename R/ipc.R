library(tidyverse)
library(readxl)

source("R/VulIndex.R", encoding = "UTF-8")

## localização dos dados brutos
censo_dir <- file.path("~/Downloads/Censo2010")

estados <- c(
  # "CE",
  # "DF",
  # "MG",
  # "RS",
  "TO"
)

full <- NULL

for (estado in estados) {
  print(estado)
  bases <- c(
      Basico = file.path(censo_dir, paste0("Basico_", estado, ".xls")),
      Entorno = file.path(censo_dir, paste0("Entorno03_",estado,".xls")),
      Domicilio01 = file.path(censo_dir, paste0("Domicilio01_",estado,".xls")),
      Domicilio02 = file.path(censo_dir, paste0("Domicilio02_",estado,".xls")),
      Pessoa = file.path(censo_dir, paste0("Pessoa03_",estado,".xls")),
      Dom.Renda = file.path(censo_dir, paste0("DomicilioRenda_",estado,".xls")),
      Resp.Alfa = file.path(censo_dir, paste0("Responsavel02_",estado,".xls"))
    )

  # % dos arquivos encontrados
  stopifnot( mean(unlist(lapply(bases, file.exists))) == 1)

  basico <- read_excel(bases["Basico"])
  entorno <- read_excel(bases["Entorno"])
  dom.i <- read_excel(bases["Domicilio01"])
  dom.ii <- read_excel(bases["Domicilio02"])
  pessoa <- read_excel(bases["Pessoa"])
  dom.renda <- read_excel(bases["Dom.Renda"])
  resp.alfa <- read_excel(bases["Resp.Alfa"]) # não está sendo usado pela função

  ipc_f <- VulIndex(
    basico = basico,
    entorno = entorno,
    dom.i = dom.i,
    dom.ii = dom.ii,
    pessoa = pessoa,
    dom.renda = dom.renda,
    resp.alfa = resp.alfa
  )
  ipc_f$Nome_UF <- estado
  full <- rbind(full,ipc_f)
  full <- as_tibble(full)
}
