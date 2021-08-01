E7LIX <- function(data) {

  #Lixo
  # base Entorno03
  rowSums(data[, c("V478", "V480", "V482")])/data$V422
}
