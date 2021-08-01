comp_domicilio <- function(data, group = Cod_setor) {
  D1BAN <- D1BAN(data, group = {{group}})

  D2AGU <- D2AGU(data, group = {{group}})

  D3M5 <- D3M5(data, group = {{group}})

  D4REN <- D4REN(data, group = {{group}})

  compDomicilios <-
    D2AGU * 1/5 +
    D1BAN * 1/5 +
    D3M5  * 1/5 +
    D4REN * 2/5

  compDomicilios
}
