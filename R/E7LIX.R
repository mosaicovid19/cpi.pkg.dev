E7LIX <- function(data, group = Cod_setor) {

  #Lixo
  # base Entorno03
  data %>%
    group_by( {{group}} ) %>%
    mutate(E7LIX = (
      sum(V478, na.rm = TRUE) +
      sum(V480, na.rm = TRUE) +
      sum(V482, na.rm = TRUE)
    ) / sum(V422, na.rm = TRUE)) %>%
    pull(E7LIX)
}
