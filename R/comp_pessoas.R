comp_pessoas <- function(data, group = Cod_setor) {

  H1NB <- H1NB(data, group = {{group}})

  H2MR <- H2MR(data, group = {{group}})

  H3RA <- H3RA(data, group = {{group}})

  compPessoas <-
    H1NB * 1/3 +
    H2MR * 1/3 +
    H3RA * 1/3
  compPessoas
}
