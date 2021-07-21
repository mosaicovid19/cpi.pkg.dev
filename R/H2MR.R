H2MR <- function(data, group = Cod_setor) {

  # calcula a componente Docmicilios com mulheres como mantenedoras e aplica os pesos
  # base Domicilio01

  # Qtd pessoas domicios com mulheres como mantenedoras / Qtd pessoas dos domicilios
  data %>%
    group_by( {{group}} ) %>%
    mutate(H2MR = 1 - (
      sum(V081, na.rm = TRUE) +
      sum(V082, na.rm = TRUE) +
      sum(V083, na.rm = TRUE) +
      sum(V084, na.rm = TRUE) +
      sum(V085, na.rm = TRUE) +
      sum(V086, na.rm = TRUE) +
      sum(V087, na.rm = TRUE)
    ) / sum(V422, na.rm = TRUE)) %>%
    pull(H2MR)
}
