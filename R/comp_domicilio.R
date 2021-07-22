comp_domicilio <- function(data, group = Cod_setor) {
  base %>%
    # acrescenta variável D1BAN
    D1BAN(group = {{group}}) %>%
    # acrescenta variável D2AGU
    D2AGU(group = {{group}}) %>%
    # acrescenta variável D3M5
    D3M5(group = {{group}}) %>%
    # acrescenta variável D4REN
    D4REN(group = {{group}}) %>%
    # acrescenta componente domicílios
    mutate(
      compDomicilios =
        D1BAN * 1/5 +
        D2AGU * 1/5 +
        D3M5  * 1/5 +
        D4REN * 2/5
    )
}
