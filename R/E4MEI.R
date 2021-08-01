E4MEI <- function(data) {

  #Meio-fio/guia
  # base Entorno03
  data %>%
    mutate(E4MEI = (
      sum(V447, na.rm = TRUE) +
      sum(V449, na.rm = TRUE) +
      sum(V451, na.rm = TRUE)
    ) / sum(V422, na.rm = TRUE))
}
