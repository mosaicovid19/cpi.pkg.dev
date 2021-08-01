D4REN <- function(data) {

  # Cria cluster por faixa de renda
  # calcula a componente Renda e aplica o peso
  # base DomicilioRenda + Pessoa03
  data %>%
    mutate(D4REN = case_when(
      sum(V002DR, na.rm = TRUE)/sum(V001p, na.rm = TRUE) > 2090   ~ 1.0,
      sum(V002DR, na.rm = TRUE)/sum(V001p, na.rm = TRUE) > 1045   ~ 0.7,
      sum(V002DR, na.rm = TRUE)/sum(V001p, na.rm = TRUE) >  522.5 ~ 0.5,
      sum(V002DR, na.rm = TRUE)/sum(V001p, na.rm = TRUE) >  178   ~ 0.3,
      sum(V002DR, na.rm = TRUE)/sum(V001p, na.rm = TRUE) >   89   ~ 0.2,
      TRUE                                                        ~ 0.1
    ))
}
