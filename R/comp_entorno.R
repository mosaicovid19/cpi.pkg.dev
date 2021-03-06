comp_entorno <- function(data) {

  # calcula o componente Entorno e aplica os pesos
  # Requisitos:
  # - Divide por: V422
  # - V423, V425, V427
  # - V429, V431, V433
  # - V435, V437, V439
  # - V447, V449, V451
  # - V453, V455, V457
  # - V472, V474, V476
  # - V478, V480, V482
  data %>%
    # acrescenta variável E1LOG
    E1LOG() %>%
    # acrescenta variável E2ILU
    E2ILU() %>%
    # acrescenta variável E3PAV
    E3PAV() %>%
    # acrescenta variável E4MEI
    E4MEI() %>%
    # acrescenta variável E5BOC
    E5BOC() %>%
    # acrescenta variável E6ESG
    E6ESG() %>%
    # acrescenta variável E7LIX
    E7LIX() %>%
    # acrescenta componente entorno
    mutate(
      compEntorno =
        E1LOG * 1/7 +
        E2ILU * 1/7 +
        E3PAV * 1/7 +
        E4MEI * 1/7 +
        E5BOC * 1/7 +
        E6ESG * 1/7 +
        E7LIX * 1/7
    )
}
