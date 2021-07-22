E2ILU <- function(data, group = Cod_setor) {

  #Iluminação Pública
  # base Entorno03
  data %>%
    group_by( {{group}} ) %>%
    mutate(E2ILU = (
      sum(V429, na.rm = TRUE) +
      sum(V431, na.rm = TRUE) +
      sum(V433, na.rm = TRUE)
    ) / sum(V422, na.rm = TRUE))
}
