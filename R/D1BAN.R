D1BAN <- function(data, group = Cod_setor) {

  # Calcula % de pessoas com acesso a banheiro de uso exclusivo
  # base Domicilio02
  data %>%
    group_by( {{group}} ) %>%
    mutate(D1BAN = sum(V016, na.rm = TRUE) / sum(V001, na.rm = TRUE))
}
