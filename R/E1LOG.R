E1LOG <- function(data, group = Cod_setor) {

  # Logradouro
  # base Entorno03
  data %>%
    group_by( {{group}} ) %>%
    mutate(E1LOG = (
      sum(V423, na.rm = TRUE) +
      sum(V425, na.rm = TRUE) +
      sum(V427, na.rm = TRUE)
    ) / sum(V422, na.rm = TRUE))
}
