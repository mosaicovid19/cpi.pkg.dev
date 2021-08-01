base_redux <- function(data, vars) {
  data %>%
    select(Cod_setor, !!!vars)
}
