comp_pessoas <- function(data) {

  H1NB <- H1NB(data)

  H2MR <- H2MR(data)

  # calcula % responsaveis analfa.
  # base Responsavel02
  H3RA <-
    (1 - rowSums(data[,c("V093")]/data$V001r))

  compPessoas <-
    H1NB * 1/3 +
    H2MR * 1/3 +
    H3RA * 1/3
  compPessoas
}
