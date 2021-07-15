comp_domicilio <- function(data) {
  D1BAN <- D1BAN(data)

  D2AGU <- D2AGU(data)

  D3M5 <- D3M5(data)

  # Cria cluster por faixa de renda
  # calcula a componente Renda e aplica o peso
  # base DomicilioRenda
  D4REN <- ifelse(data$V002DR/data$V001p>2090,1,
                         ifelse(data$V002DR/data$V001p>1045,0.7,
                                ifelse(data$V002DR/data$V001p>522.5,0.5,
                                       ifelse(data$V002DR/data$V001p>178,0.3,
                                              ifelse(data$V002DR/data$V001p>89,0.2,0.1)))))

  compDomicilios <-
    D2AGU * 1/5 +
    D1BAN * 1/5 +
    D3M5  * 1/5 +
    D4REN * 2/5

  compDomicilios
}
