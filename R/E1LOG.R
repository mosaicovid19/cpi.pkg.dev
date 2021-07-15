E1LOG <- function(data) {

  # Logradouro
  # base Entorno03
  rowSums(data[, c("V423", "V425", "V427")])/data$V422
}
