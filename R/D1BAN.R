D1BAN <- function(data) {

  # Calcula % de pessoas com acesso a banheiro de uso exclusivo
  # base Domicilio02
  data %>%
    mutate(D1BAN = sum(V016, na.rm = TRUE) / sum(V001, na.rm = TRUE))
}
