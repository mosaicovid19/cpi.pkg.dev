E6ESG <- function(data, group = Cod_setor) {

  #Esgoto
  # base Entorno03
  data %>%
    group_by( {{group}} ) %>%
    mutate(E6ESG = (
      sum(V472, na.rm = TRUE) +
      sum(V474, na.rm = TRUE) +
      sum(V476, na.rm = TRUE)
    ) / sum(V422, na.rm = TRUE)) %>%
    pull(E6ESG)
}
