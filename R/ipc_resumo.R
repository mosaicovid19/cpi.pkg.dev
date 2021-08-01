ipc_resumo <- function(data) {
  # resumir o dataframe final com apenas as linhas calculadas, para cada grupo
  data %>%
    count(ipc,
          compDomicilios,
          compEntorno,
          compPessoas,
          D1BAN,
          D2AGU,
          D3M5,
          D4REN,
          E1LOG,
          E2ILU,
          E3PAV,
          E4MEI,
          E5BOC,
          E6ESG,
          E7LIX,
          H1NB,
          H2MR,
          H3RA,
    )
}
