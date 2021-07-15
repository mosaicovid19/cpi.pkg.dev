D1BAN <- function(data) {

  # Calcula % de pessoas com acesso a banheiro de uso exclusivo
  # base Domicilio02
  rowSums(data[,c("V016")]/data$V001)
}
