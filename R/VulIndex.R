VulIndex = function(Basico = Basico, Entorno03 = Entorno03, Domicilio01 = Domicilio01, Domicilio02 = Domicilio02, Pessoa03 = Pessoa03, DomicilioRenda = DomicilioRenda, Responsavel02 = Responsavel02, group = Cod_setor){

# bases -------------------------------------------------------------------

  # definição de vars para auxílio ao select
  vars.Entorno03 <- vars(V422, V423, V425, V427, V429, V431, V433, V435, V437, V439, V447, V449, V451, V453, V455, V457, V472, V474, V476, V478, V480, V482)
  vars.Domicilio01 <- vars(V001, V050, V051, V052, V053, V054, V055, V056, V057, V058, V059, V081, V082, V083, V084, V085, V086, V087)
  vars.Domicilio02 <- vars(V001, V012, V016)
  vars.DomicilioRenda <- vars(V002)
  vars.Pessoa03 <- vars(V001, V003, V004, V005, V006)
  vars.Responsavel02 <- vars(V001, V093)

  # seleciona apenas as variáveis de interesse de cada DataFrame
  # regiões onde o Censo identificou 0 pessoas, não servem para a análise, portanto tais registros devem ser removidos
  Basico <- Basico %>%
    select({{group}}, Cod_setor, Situacao_setor, starts_with(c("Cod_", "Nome_")))

  Entorno03 <- Entorno03 %>%
    base_redux(vars.Entorno03) %>%
    # filtrar valores indesejados
    filter(V422 != 0) # n = 640

  Domicilio01 <- Domicilio01 %>%
    # V001 será filtrada e descartada
    base_redux(vars.Domicilio01) %>%
    # filtrar valores indesejados
    filter(V001 >0) %>% # n = 0
    # descartar V001
    select(-V001)

  Domicilio02 <- Domicilio02 %>%
    # V001 será mantida
    base_redux(vars.Domicilio02) %>%
    # filtrar valores indesejados
    filter(V001 >0) # n = 0

  Pessoa03 <- Pessoa03 %>%
    # V001 será renomeada (V001p)
    base_redux(vars.Pessoa03) %>%
    # Renomear campo V001 da tabela Pessoa03 para V001p
    rename(V001p = V001) %>%
    # filtrar valores indesejados
    filter(V001p >0) # n = 0

  DomicilioRenda <- DomicilioRenda %>%
    # V002 será renomeada (V002DR)
    base_redux(vars.DomicilioRenda) %>%
    # renomear V002
    rename(V002DR = V002)

  Responsavel02 <- Responsavel02 %>%
    # V001 será renomeada (V001r)
    base_redux(vars.Responsavel02) %>%
    # Renomear campo V001 da tabela Responsavel02 para V001r
    rename(V001r = V001)

  # variáveis do dataframe Domicilio01 refletem o número de pessoas vivendo num determinado domicílio
  Domicilio01 <- Domicilio01 %>%
    mutate(
      # como a ideia é ter número de pessoas por domicílio numa dada condição, multiplicamos o número de domicílios pelo número de pessoas
      # que vivem no domicílio
      V051 = V051*2,
      V052 = V052*3,
      V053 = V053*4,
      V054 = V054*5,
      V055 = V055*6,
      V056 = V056*7,
      V057 = V057*8,
      V058 = V058*9,
      V059 = V059*10,
      # similar ao que foi feita nas acima, só que este cálculo é para definir quantas pessoas vivem em domicílios que tem mulheres
      # como mantenedoras
      V081 = V081*2,
      V082 = V082*3,
      V083 = V083*4,
      V084 = V084*5,
      V085 = V085*6,
      V086 = V086*7,
    )

# join --------------------------------------------------------------------

  # junta todos os DataFrames pela coluna Cod_setor
  resumo <- inner_join(
    inner_join(
      inner_join(
        inner_join(
          inner_join(Entorno03, Domicilio01, by=c("Cod_setor"), suffix = c("_entorno", "_dom01")),
          Domicilio02, by=c("Cod_setor"), suffix = c("_join_dom01", "_dom02")),
        Pessoa03, by=c("Cod_setor"), suffix = c("_join_dom02", "_pessoa")),
      Responsavel02, by=c("Cod_setor"), suffix = c("_join_pessoa", "_resp02")),
    DomicilioRenda, by=c("Cod_setor"), suffix = c("_join_resp02", "_dom.renda"))

  # Adiciona a informação de bairro ao DataFrame que contem todas as demais informações coletadas pelo Censo
  resumo <- inner_join(Basico, filter(resumo, V422!="0"), by=c("Cod_setor"), suffix = c("_join5", "_basico"))

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
