E6ESG <- function(data) {

  #Esgoto
  # base Entorno03
  rowSums(data[, c("V472", "V474", "V476")])/data$V422
}
