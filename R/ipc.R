library(tidyverse)
library(censo2010brasil)

ipc_f <- VulIndex(
  basico = Basico,
  entorno = Entorno03,
  dom.i = Domicilio01,
  dom.ii = Domicilio02,
  pessoa = Pessoa03,
  dom.renda = DomicilioRenda,
  resp.alfa = Responsavel02
  )
