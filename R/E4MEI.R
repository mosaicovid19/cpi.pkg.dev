E4MEI <- function(data) {

  #Meio-fio/guia
  # base Entorno03
  rowSums(data[, c("V447", "V449", "V451")])/data$V422
}
