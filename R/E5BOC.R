E5BOC <- function(data, group = Cod_setor) {

  #Bueiro/Boca de lobo
  # base Entorno03
  data %>%
    group_by( {{group}} ) %>%
    mutate(E5BOC = (
      sum(V453, na.rm = TRUE) +
      sum(V455, na.rm = TRUE) +
      sum(V457, na.rm = TRUE)
    ) / sum(V422, na.rm = TRUE)) %>%
    pull(E5BOC)
}
