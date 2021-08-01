base_preparo_SQL <- function(con) {
  Basico <- tbl(con, "Basico")
  Domicilio01 <- tbl(con, "Domicilio01")
  Domicilio02 <- tbl(con, "Domicilio02")
  DomicilioRenda <- tbl(con, "DomicilioRenda")
  Entorno03 <- tbl(con, "Entorno03")
  Pessoa03 <- tbl(con, "Pessoa03")
  Responsavel02 <- tbl(con, "Responsavel02")
  base_preparo_R(Basico, Domicilio01, Domicilio02, DomicilioRenda, Entorno03, Pessoa03, Responsavel02)
}
