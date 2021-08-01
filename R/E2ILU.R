E2ILU <- function(data) {

  #Iluminação Pública
  # base Entorno03
  data %>%
    mutate(E2ILU = (
      sum(V429, na.rm = TRUE) +
      sum(V431, na.rm = TRUE) +
      sum(V433, na.rm = TRUE)
    ) / sum(V422, na.rm = TRUE))
}
