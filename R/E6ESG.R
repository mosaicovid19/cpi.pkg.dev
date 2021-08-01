E6ESG <- function(data) {

  #Esgoto
  # base Entorno03
  data %>%
    mutate(E6ESG = (
      sum(V472, na.rm = TRUE) +
      sum(V474, na.rm = TRUE) +
      sum(V476, na.rm = TRUE)
    ) / sum(V422, na.rm = TRUE))
}
