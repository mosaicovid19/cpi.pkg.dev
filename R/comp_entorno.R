comp_entorno <- function(data) {
  # calcula a componente Entorno do IVC e aplica os pesos
  componente <-
    #Logradouro
    rowSums(features.abs[, c("V423", "V425", "V427")])/features.abs$V422 * (1/7) +
    #Iluminação Pública
    rowSums(features.abs[, c("V429", "V431", "V433")])/features.abs$V422 * (1/7) +
    #Pavimentação
    rowSums(features.abs[, c("V435", "V437", "V439")])/features.abs$V422 * (1/7) +
    #Meio-fio/guia
    rowSums(features.abs[, c("V447", "V449", "V451")])/features.abs$V422 * (1/7) +
    #Bueiro/Boca de lobo
    rowSums(features.abs[, c("V453", "V455", "V457")])/features.abs$V422 * (1/7) +
    #Esgoto
    rowSums(features.abs[, c("V472", "V474", "V476")])/features.abs$V422 * (1/7) +
    #Lixo
    rowSums(features.abs[, c("V478", "V480", "V482")])/features.abs$V422 * (1/7)
  componente
}
