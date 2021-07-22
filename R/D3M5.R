D3M5 <- function(data, group = Cod_setor) {

  # Calcula componente de calculo mais de duas pessoas por domicílio
  # Calcula % de pessoas que moram sós ou com até mais uma outra pessoa
  # 1 - Soma a qtde de pessoas que vivem com mais do que 5 pessoas (total)
  # 2 - Subtrai de 1, a qtde de pessoas que vivem com mais do que 2 pessoas em residências sustentadas por mulheres
  # 3 - Divide o resultado de 2 pelo número total de pessoas vivendo com mais do que 2 pessoas
  # base Domicilio01 + Entorno03
  data %>%
    group_by( {{group}} ) %>%
    mutate(D3M5 = 1 - (
      sum(V055, na.rm = TRUE) +
      sum(V056, na.rm = TRUE) +
      sum(V057, na.rm = TRUE) +
      sum(V058, na.rm = TRUE) +
      sum(V059, na.rm = TRUE)
      ) / sum(V422, na.rm = TRUE))
}
