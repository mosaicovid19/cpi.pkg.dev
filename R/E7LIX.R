E7LIX <- function(data) {

  #Lixo
  # base Entorno03
  data %>%
    mutate(E7LIX = (
      sum(V478, na.rm = TRUE) +
      sum(V480, na.rm = TRUE) +
      sum(V482, na.rm = TRUE)
    ) / sum(V422, na.rm = TRUE))
}
