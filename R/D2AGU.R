D2AGU <- function(data) {

  # Calcula % de pessoas com acesso a rede de distribuição de água
  # base Domicilio02
  data %>%
    mutate(D2AGU = sum(V012, na.rm = TRUE) / sum(V001, na.rm = TRUE))
}
