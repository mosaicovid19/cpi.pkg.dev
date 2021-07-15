comp_pessoas <- function(data) {

  # calcula % Pessoas brancas
  # base Pessoa03
  H1NB <-
    (1 - rowSums(data[, c("V003", "V004", "V005", "V006")])/data$V001p)

  # calcula a componente Docmicilios com mulheres como mantenedoras e aplica os pesos
  # base Domicilio01
  H2MR <-
    # Qtd pessoas domicios com mulheres como mantenedoras / Qtd pessoas dos domicilios
    (1-(rowSums(data[, c("V081","V082", "V083", "V084", "V085", "V086", "V087")]))/
       data$V422)

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
