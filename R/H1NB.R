H1NB <- function(data) {

  # calcula % Pessoas brancas
  # base Pessoa03
  data %>%
    mutate(H1NB = 1 - (
      sum(V003, na.rm = TRUE) +
      sum(V004, na.rm = TRUE) +
      sum(V005, na.rm = TRUE) +
      sum(V006, na.rm = TRUE)
    ) / sum(V001p, na.rm = TRUE))
}
