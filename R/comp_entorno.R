comp_entorno <- function(data, group = Cod_setor) {
  # calcula a componente Entorno do IVC e aplica os pesos

  E1LOG <- E1LOG(data, group = {{group}})

  E2ILU <- E2ILU(data, group = {{group}})

  E3PAV <- E3PAV(data, group = {{group}})

  E4MEI <- E4MEI(data, group = {{group}})

  E5BOC <- E5BOC(data, group = {{group}})

  E6ESG <- E6ESG(data, group = {{group}})

  E7LIX <- E7LIX(data, group = {{group}})

  componente <-
    E1LOG * 1/7 +
    E2ILU * 1/7 +
    E3PAV * 1/7 +
    E4MEI * 1/7 +
    E5BOC * 1/7 +
    E6ESG * 1/7 +
    E7LIX * 1/7

  componente
}
