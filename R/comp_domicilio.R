comp_domicilio <- function(data) {
  D1BAN <- D1BAN(data)

  D2AGU <- D2AGU(data)

  D3M5 <- D3M5(data)

  D4REN <- D4REN(data)

  compDomicilios <-
    D2AGU * 1/5 +
    D1BAN * 1/5 +
    D3M5  * 1/5 +
    D4REN * 2/5

  compDomicilios
}
