comp_entorno <- function(data) {
  # calcula a componente Entorno do IVC e aplica os pesos
  componente <-
    #Logradouro
    rowSums(data[, c("V423", "V425", "V427")])/data$V422 * (1/7) +
    #Iluminação Pública
    rowSums(data[, c("V429", "V431", "V433")])/data$V422 * (1/7) +
    #Pavimentação
    rowSums(data[, c("V435", "V437", "V439")])/data$V422 * (1/7) +
    #Meio-fio/guia
    rowSums(data[, c("V447", "V449", "V451")])/data$V422 * (1/7) +
    #Bueiro/Boca de lobo
    rowSums(data[, c("V453", "V455", "V457")])/data$V422 * (1/7) +
    #Esgoto
    rowSums(data[, c("V472", "V474", "V476")])/data$V422 * (1/7) +
    #Lixo
    rowSums(data[, c("V478", "V480", "V482")])/data$V422 * (1/7)
  componente
}
