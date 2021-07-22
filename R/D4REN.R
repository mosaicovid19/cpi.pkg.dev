D4REN <- function(data, group = Cod_setor) {

  # Cria cluster por faixa de renda
  # calcula a componente Renda e aplica o peso
  # base DomicilioRenda + Pessoa03
  data %>%
    group_by( {{group}} ) %>%
    mutate(D4REN = case_when(
      sum(V002DR)/sum(V001p) > 2090   ~ 1.0,
      sum(V002DR)/sum(V001p) > 1045   ~ 0.7,
      sum(V002DR)/sum(V001p) >  522.5 ~ 0.5,
      sum(V002DR)/sum(V001p) >  178   ~ 0.3,
      sum(V002DR)/sum(V001p) >   89   ~ 0.2,
      TRUE                            ~ 0.1
    ))
}
