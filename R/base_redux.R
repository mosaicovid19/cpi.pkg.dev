base_redux <- function(data, vars, group = Cod_setor) {
  data %>%
    select({{group}}, Cod_setor, !!!vars)
}
