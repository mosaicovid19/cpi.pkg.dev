H3RA <- function(data, group = Cod_setor) {

  # calcula % responsaveis analfa.
  # base Responsavel02
  data %>%
    group_by( {{group}} ) %>%
    mutate(H3RA = 1 - sum(V093, na.rm = TRUE) / sum(V001r, na.rm = TRUE))
}
