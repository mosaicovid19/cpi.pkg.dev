D4REN <- function(data) {

  # Cria cluster por faixa de renda
  # calcula a componente Renda e aplica o peso
  # base DomicilioRenda
  ifelse(data$V002DR/data$V001p>2090,1,
         ifelse(data$V002DR/data$V001p>1045,0.7,
                ifelse(data$V002DR/data$V001p>522.5,0.5,
                       ifelse(data$V002DR/data$V001p>178,0.3,
                              ifelse(data$V002DR/data$V001p>89,0.2,0.1)))))
}
