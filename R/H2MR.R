H2MR <- function(data) {

  # calcula a componente Docmicilios com mulheres como mantenedoras e aplica os pesos
  # base Domicilio01

  # Qtd pessoas domicios com mulheres como mantenedoras / Qtd pessoas dos domicilios
  (1-(rowSums(data[, c("V081","V082", "V083", "V084", "V085", "V086", "V087")]))/
     data$V422)
}
