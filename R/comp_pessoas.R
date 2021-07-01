comp_pessoas <- function(data) {

  # calcula a componente Docmicilios com mulheres como mantenedoras e aplica os pesos
  compDomiciliosMulher <-
    # Qtd pessoas domicios com mulheres como mantenedoras / Qtd pessoas dos domicilios
    (1-(rowSums(features.abs[,
                             c("V081","V082", "V083", "V084", "V085", "V086", "V087")]))/
       features.abs$V422) * (1/3)


  # calcula % Pessoas brancas
  compPessoas <-
    (1 - rowSums(features.abs[,
                              c("V003", "V004", "V005", "V006")])/features.abs$V001p)*1/3

  # calcula % responsaveis analfa.
  compRespAlfa <-
    (1 - rowSums(features.abs[,c("V093")]/features.abs$V001r))*1/3

  compPessoas <- compPessoas + compDomiciliosMulher + compRespAlfa
  compPessoas
}
