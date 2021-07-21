E3PAV <- function(data, group = Cod_setor) {

  #Pavimentação
  # base Entorno03
  data %>%
    group_by( {{group}} ) %>%
    mutate(E3PAV = (
      sum(V435, na.rm = TRUE) +
      sum(V437, na.rm = TRUE) +
      sum(V439, na.rm = TRUE)
    ) / sum(V422, na.rm = TRUE)) %>%
    pull(E3PAV)
}
