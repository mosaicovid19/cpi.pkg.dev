E2ILU <- function(data) {

  #Iluminação Pública
  # base Entorno03
  rowSums(data[, c("V429", "V431", "V433")])/data$V422
}
