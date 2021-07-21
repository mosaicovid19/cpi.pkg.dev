H1NB <- function(data) {

  # calcula % Pessoas brancas
  # base Pessoa03
  (1 - rowSums(data[, c("V003", "V004", "V005", "V006")])/data$V001p)
}
