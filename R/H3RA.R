H3RA <- function(data) {

  # calcula % responsaveis analfa.
  # base Responsavel02
  data %>%
    mutate(H3RA = 1 - sum(V093, na.rm = TRUE) / sum(V001r, na.rm = TRUE))
}
