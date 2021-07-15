comp_entorno <- function(data) {
  # calcula a componente Entorno do IVC e aplica os pesos

  E1LOG <- E1LOG(data)

  E2ILU <- E2ILU(data)

    #Pavimentação
  # base Entorno03
  E3PAV <- rowSums(data[, c("V435", "V437", "V439")])/data$V422

    #Meio-fio/guia
  # base Entorno03
  E4MEI <- rowSums(data[, c("V447", "V449", "V451")])/data$V422

    #Bueiro/Boca de lobo
  # base Entorno03
  E5BOC <- rowSums(data[, c("V453", "V455", "V457")])/data$V422

    #Esgoto
  # base Entorno03
  E6ESG <- rowSums(data[, c("V472", "V474", "V476")])/data$V422

    #Lixo
  # base Entorno03
  E7LIX <- rowSums(data[, c("V478", "V480", "V482")])/data$V422

  componente <-
    E1LOG * 1/7 +
    E2ILU * 1/7 +
    E3PAV * 1/7 +
    E4MEI * 1/7 +
    E5BOC * 1/7 +
    E6ESG * 1/7 +
    E7LIX * 1/7

  componente
}
