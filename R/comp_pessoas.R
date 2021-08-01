comp_pessoas <- function(data) {

  # calcula o componente Pessoas e aplica os pesos
  # Requisitos:
  # - Divide por: V422 (Entorno03), V001p, V001r
  # - V081,V082, V083, V084, V085, V086, V087
  # - V003, V004, V005, V006 (DomicilioRenda)
  # - V093
  data %>%
    # acrescenta variável H1NB
    H1NB() %>%
    # acrescenta variável H2MR
    H2MR() %>%
    # acrescenta variável H3RA
    H3RA() %>%
    # acrescenta componente humano
    mutate(
      compPessoas =
        H1NB * 1/3 +
        H2MR * 1/3 +
        H3RA * 1/3
    )
}
