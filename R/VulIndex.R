VulIndex <- function(Basico = Basico, Domicilio01 = Domicilio01, Domicilio02 = Domicilio02, DomicilioRenda = DomicilioRenda, Entorno03 = Entorno03, Pessoa03 = Pessoa03, Responsavel02 = Responsavel02, group = Cod_setor) {

# bases -------------------------------------------------------------------

  resumo <- base_preparo(Basico, Domicilio01, Domicilio02, DomicilioRenda, Entorno03, Pessoa03, Responsavel02)

# calculo componentes -----------------------------------------------------

    resumo %>%
    # Agrupamento
    group_by( {{group}} ) %>%
    # acrescenta todas as variáveis e componentes em novas colunas
    ipc() %>%
    # filtrar apenas colunas novas, em cada grupo
    ipc_resumo()
}
