H3RA <- function(data) {

  # calcula % responsaveis analfa.
  # base Responsavel02
  (1 - rowSums(data[,c("V093")]/data$V001r))
}
