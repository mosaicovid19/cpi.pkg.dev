D3M5 <- function(data) {

  # Calcula componente de calculo mais de duas pessoas por domicílio
  # Calcula % de pessoas que moram sós ou com até mais uma outra pessoa
  # 1 - Soma a qtde de pessoas que vivem com mais do que 5 pessoas (total)
  # 2 - Subtrai de 1, a qtde de pessoas que vivem com mais do que 2 pessoas em residências sustentadas por mulheres
  # 3 - Divide o resultado de 2 pelo número total de pessoas vivendo com mais do que 2 pessoas
  # base Domicilio02

  (1 - (rowSums(data[, c("V055", "V056", "V057", "V058", "V059")])/
          (data$V422)))
}
