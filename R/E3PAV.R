E3PAV <- function(data) {

  #Pavimentação
  # base Entorno03
  data %>%
    mutate(E3PAV = (
      sum(V435, na.rm = TRUE) +
      sum(V437, na.rm = TRUE) +
      sum(V439, na.rm = TRUE)
    ) / sum(V422, na.rm = TRUE))
}
