comp_entorno <- function(data) {
  # calcula a componente Entorno do IVC e aplica os pesos

  E1LOG <- E1LOG(data)

  E2ILU <- E2ILU(data)

  E3PAV <- E3PAV(data)

  E4MEI <- E4MEI(data)

  E5BOC <- E5BOC(data)

  E6ESG <- E6ESG(data)

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
