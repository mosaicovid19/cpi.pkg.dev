VulIndex <- function(Basico = Basico, Domicilio01 = Domicilio01, Domicilio02 = Domicilio02, DomicilioRenda = DomicilioRenda, Entorno03 = Entorno03, Pessoa03 = Pessoa03, Responsavel02 = Responsavel02, group = Cod_setor) {

# bases -------------------------------------------------------------------

  resumo <- base_preparo(Basico, Domicilio01, Domicilio02, DomicilioRenda, Entorno03, Pessoa03, Responsavel02)

  ## SQLite 3.33.0 não aguenta esse tranco todo
  ## Precisamos parar por aqui e coletar o resultado do preparo da base
  if ("tbl_SQLite" %in% class(resumo)) {
    resumo <- resumo %>%
      collect()
  }

# calculo componentes -----------------------------------------------------

    resumo %>%
    # Agrupamento
    group_by( {{group}} ) %>%
    # acrescenta todas as variáveis e componentes em novas colunas
    ipc() %>%
    # filtrar apenas colunas novas, em cada grupo
    ipc_resumo()
}
