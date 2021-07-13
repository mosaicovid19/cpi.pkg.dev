comp_domicilio <- function(data) {
  # Calcula % de pessoas com acesso a banheiro de uso exclusivo
  D1BAN <-
    rowSums(data[,c("V016")]/data$V001)

  # Calcula % de pessoas com acesso a rede de distribuiÃ§Ã£o de Ã¡gua
  D2AGU <-
    rowSums(data[,c("V012")]/data$V001)

  # Calcula componente de calculo mais de duas pessoas por domicÃ­lio
  # Calcula % de pessoas que moram sÃ³s ou com atÃ© mais uma outra pessoa
  # 1 - Soma a qtde de pessoas que vivem com mais do que 5 pessoas (total)
  # 2 - Subtrai de 1, a qtde de pessoas que vivem com mais do que 2 pessoas em residÃªncias sustentadas por mulheres
  # 3 - Divide o resultado de 2 pelo nÃºmero total de pessoas vivendo com mais do que 2 pessoas
  D3M5 <-
    (1 - (rowSums(data[, c("V055", "V056", "V057", "V058", "V059")])/
            (data$V422)))

  # Cria cluster por faixa de renda
  # calcula a componente Renda e aplica o peso
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
