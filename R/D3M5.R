D3M5 <- function(data) {

  # Calcula componente de calculo mais de duas pessoas por domicílio
  # Calcula % de pessoas que moram sós ou com até mais uma outra pessoa
  # 1 - Soma a qtde de pessoas que vivem com mais do que 5 pessoas (total)
  # 2 - Subtrai de 1, a qtde de pessoas que vivem com mais do que 2 pessoas em residências sustentadas por mulheres
  # 3 - Divide o resultado de 2 pelo número total de pessoas vivendo com mais do que 2 pessoas
  # base Domicilio01 + Entorno03
  data %>%
    mutate(D3M5 = 1 - (
      # como a ideia é ter número de pessoas por domicílio numa dada condição,
      # multiplicamos o número de domicílios pelo número de pessoas que vivem no domicílio
      # sum(V051*2, na.rm = TRUE) +
      # sum(V052*3, na.rm = TRUE) +
      # sum(V053*4, na.rm = TRUE) +
      # sum(V055*5, na.rm = TRUE) +
      sum(V055*6, na.rm = TRUE) +
      sum(V056*7, na.rm = TRUE) +
      sum(V057*8, na.rm = TRUE) +
      sum(V058*9, na.rm = TRUE) +
      sum(V059*10, na.rm = TRUE)
      ) / sum(V422, na.rm = TRUE))
}
