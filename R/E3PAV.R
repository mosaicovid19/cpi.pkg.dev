E3PAV <- function(data) {

  #Pavimentação
  # base Entorno03
  rowSums(data[, c("V435", "V437", "V439")])/data$V422
}
