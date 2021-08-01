library(tidyverse)
library(censo2010brasil)

# checkout c3133b6 e calcular a resposta
# source("R/VulIndex.R", encoding = "UTF-8")
# resposta <- as_tibble(VulIndex(basico = Basico, entorno = Entorno03, dom.i = Domicilio01, dom.ii = Domicilio02, pessoa = Pessoa03, dom.renda = DomicilioRenda, resp.alfa = Responsavel02))
# save(resposta, file = "data/ipc_check.rda")
# rm(VulIndex)
if(!exists("resposta")) load("data/ipc_check.rda")

# checkout develop branch
devtools::load_all(".")
teste <- VulIndex(basico = Basico, entorno = Entorno03, dom.i = Domicilio01, dom.ii = Domicilio02, pessoa = Pessoa03, dom.renda = DomicilioRenda, resp.alfa = Responsavel02)

print(all.equal(resposta %>% select(Cod_UF:ipc), teste))
