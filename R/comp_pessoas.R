comp_pessoas <- function(data) {

  H1NB <- H1NB(data)

  H2MR <- H2MR(data)

  H3RA <- H3RA(data)

  compPessoas <-
    H1NB * 1/3 +
    H2MR * 1/3 +
    H3RA * 1/3
  compPessoas
}
