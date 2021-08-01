library(dplyr)
# library(censo2010brasil)
# library(cpi.pkg.dev)

# checkout 518ad6b e calcular a resposta
# source("R/VulIndex.R", encoding = "UTF-8")
# resposta <- as_tibble(VulIndex(basico = Basico, entorno = Entorno03, dom.i = Domicilio01, dom.ii = Domicilio02, pessoa = Pessoa03, dom.renda = DomicilioRenda, resp.alfa = Responsavel02))
# save(resposta, file = "data/ipc_check.rda")
# rm(VulIndex)
if(!exists("resposta")) load("data/ipc_check.rda")
resposta <- resposta %>%
  select(Cod_setor, ipc) %>%
  arrange(Cod_setor)

# checkout develop branch
# devtools::load_all(".")
teste <- VulIndex(con = con) %>%
  ungroup() %>% # desagrupar para performance do teste
  select(Cod_setor, ipc) %>%
  arrange(Cod_setor)

print(all.equal(resposta, teste))
