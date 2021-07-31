comp_domicilio <- function(data, group = Cod_setor) {

  # calcula o componente Domicílios e aplica os pesos
  # Requisitos:
  # - Divide por: V422 (Entorno03), V001p, V001 (Domicilio02)
  # - V055, V056, V057, V058, V059
  # - V016
  # - V012 (DomicilioRenda)
  # - V002 (DomicilioRenda)

  # a variável V002 vem do arquivo DomicilioRenda que descreve a renda total das regiões definidas pelo setor censitário
  # ao dividir este valor total de rendas pelo número total de pessoas (representado pela variável V422) obtem-se a renda per capita da região

  data %>%
    # acrescenta variável D1BAN
    D1BAN(group = {{group}}) %>%
    # acrescenta variável D2AGU
    D2AGU(group = {{group}}) %>%
    # acrescenta variável D3M5
    D3M5(group = {{group}}) %>%
    # acrescenta variável D4REN
    D4REN(group = {{group}}) %>%
    # acrescenta componente domicílios
    mutate(
      compDomicilios =
        D1BAN * 1/5 +
        D2AGU * 1/5 +
        D3M5  * 1/5 +
        D4REN * 2/5
    )
}
