VulIndex = function(Basico = Basico, Entorno03 = Entorno03, Domicilio01 = Domicilio01, Domicilio02 = Domicilio02, Pessoa03 = Pessoa03, DomicilioRenda = DomicilioRenda, Responsavel02 = Responsavel02, group = Cod_setor){

# bases -------------------------------------------------------------------

resumo <- base_preparo(Basico, Entorno03, Domicilio01, Domicilio02, Pessoa03, DomicilioRenda, Responsavel02, group = {{group}})

# calculo componentes -----------------------------------------------------

  # acrescenta todas as variáveis e componentes em novas colunas
  resumo %>%
    ipc( group = {{group}} ) %>%

# finalizacao -------------------------------------------------------------

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
  ) %>%
    ungroup()

}
