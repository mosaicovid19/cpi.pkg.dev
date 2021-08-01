D2AGU <- function(data) {

  # Calcula % de pessoas com acesso a rede de distribuição de água
  # base Domicilio02
  rowSums(data[,c("V012")]/data$V001)
}
