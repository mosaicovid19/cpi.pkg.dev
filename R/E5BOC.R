E5BOC <- function(data) {

  #Bueiro/Boca de lobo
  # base Entorno03
  rowSums(data[, c("V453", "V455", "V457")])/data$V422
}
