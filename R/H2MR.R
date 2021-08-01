H2MR <- function(data) {

  # calcula a variável Docmicilios com mulheres como mantenedoras
  # base Domicilio01 + Entorno03

  # Qtd pessoas domicios com mulheres como mantenedoras / Qtd pessoas dos domicilios
  data %>%
    mutate(H2MR = 1 - (
      # como a ideia é ter número de pessoas por domicílio numa dada condição,
      # multiplicamos o número de domicílios pelo número de pessoas que vivem no domicílio
      sum(V081*2, na.rm = TRUE) +
      sum(V082*3, na.rm = TRUE) +
      sum(V083*4, na.rm = TRUE) +
      sum(V084*5, na.rm = TRUE) +
      sum(V085*6, na.rm = TRUE) +
      sum(V086*7, na.rm = TRUE) +
      sum(V087, na.rm = TRUE) ## multiplicar por 8 ?
    ) / sum(V422, na.rm = TRUE))
}
