comp_pessoas <- function(data, group = Cod_setor) {
  data %>%
    # acrescenta variável H1NB
    H1NB(group = {{group}}) %>%
    # acrescenta variável H2MR
    H2MR(group = {{group}}) %>%
    # acrescenta variável H3RA
    H3RA(group = {{group}}) %>%
    # acrescenta componente humano
    mutate(
      compPessoas =
        H1NB * 1/3 +
        H2MR * 1/3 +
        H3RA * 1/3
    )
}
