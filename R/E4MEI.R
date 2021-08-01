E4MEI <- function(data, group = Cod_setor) {

  #Meio-fio/guia
  # base Entorno03
  data %>%
    group_by( {{group}} ) %>%
    mutate(E4MEI = (
      sum(V447, na.rm = TRUE) +
      sum(V449, na.rm = TRUE) +
      sum(V451, na.rm = TRUE)
    ) / sum(V422, na.rm = TRUE)) %>%
    pull(E4MEI)
}
