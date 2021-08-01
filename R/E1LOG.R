E1LOG <- function(data, group = Cod_setor) {

  # Logradouro
  # base Entorno03
  rowSums(data[, c("V423", "V425", "V427")])/data$V422
  data %>%
    group_by( {{group}} ) %>%
    mutate(E1LOG = (
      sum(V423, na.rm = TRUE) +
      sum(V425, na.rm = TRUE) +
      sum(V427, na.rm = TRUE)
    ) / sum(V422, na.rm = TRUE)) %>%
    pull(E1LOG)
}
