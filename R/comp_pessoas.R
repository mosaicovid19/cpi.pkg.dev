comp_pessoas <- function(data) {

  # calcula a componente Docmicilios com mulheres como mantenedoras e aplica os pesos
  H2MR <-
    # Qtd pessoas domicios com mulheres como mantenedoras / Qtd pessoas dos domicilios
    (1-(rowSums(data[, c("V081","V082", "V083", "V084", "V085", "V086", "V087")]))/
       data$V422) * (1/3)

  # calcula % Pessoas brancas
  H1NB <-
    (1 - rowSums(data[, c("V003", "V004", "V005", "V006")])/data$V001p)*1/3

  # calcula % responsaveis analfa.
  H3RA <-
    (1 - rowSums(data[,c("V093")]/data$V001r))*1/3

  compPessoas <- H1NB + H2MR + H3RA
  compPessoas
}
