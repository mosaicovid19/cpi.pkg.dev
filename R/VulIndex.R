VulIndex <- function(Basico = Basico, Domicilio01 = Domicilio01, Domicilio02 = Domicilio02, DomicilioRenda = DomicilioRenda, Entorno03 = Entorno03, Pessoa03 = Pessoa03, Responsavel02 = Responsavel02, group = Cod_setor) {

# bases -------------------------------------------------------------------

resumo <- base_preparo( group = {{group}} )

# calculo componentes -----------------------------------------------------

  # acrescenta todas as variáveis e componentes em novas colunas
  resumo %>%
  # Agrupamento
  group_by( {{group}} ) %>%
    ipc( group = {{group}} ) %>%

# finalizacao -------------------------------------------------------------

  count(ipc,
        compDomicilios,
        compEntorno,
        compPessoas,
        D1BAN,
        D2AGU,
        D3M5,
        D4REN,
        E1LOG,
        E2ILU,
        E3PAV,
        E4MEI,
        E5BOC,
        E6ESG,
        E7LIX,
        H1NB,
        H2MR,
        H3RA,
  ) %>%
    ungroup()

}
