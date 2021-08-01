E5BOC <- function(data) {

  #Bueiro/Boca de lobo
  # base Entorno03
  data %>%
    mutate(E5BOC = (
      sum(V453, na.rm = TRUE) +
      sum(V455, na.rm = TRUE) +
      sum(V457, na.rm = TRUE)
    ) / sum(V422, na.rm = TRUE))
}
