D2AGU <- function(data, group = Cod_setor) {

  # Calcula % de pessoas com acesso a rede de distribuição de água
  # base Domicilio02
  data %>%
    group_by( {{group}} ) %>%
    mutate(D2AGU = sum(V012, na.rm = TRUE) / sum(V001, na.rm = TRUE))
}
