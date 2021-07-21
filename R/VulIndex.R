VulIndex = function(basico = Basico, entorno = Entorno03, dom.i = Domicilio01, dom.ii = Domicilio02, pessoa = Pessoa03, dom.renda = DomicilioRenda, resp.alfa = Responsavel02, group = Cod_setor){

# vars --------------------------------------------------------------------

  # definição de vars para auxílio ao select
  vars.entorno <- vars(V422, V423, V425, V427, V429, V431, V433, V435, V437, V439, V447, V449, V451, V453, V455, V457, V472, V474, V476, V478, V480, V482)
  vars.dom <- vars(V001, V050, V051, V052, V053, V054, V055, V056, V057, V058, V059, V081, V082, V083, V084, V085, V086, V087)
  vars.resp.alfa <- vars(V093,V001)

# bases -------------------------------------------------------------------

  # seleciona apenas as variáveis de interesse de cada DataFrame
  basico <- basico %>%
    select({{group}}, starts_with(c("Cod_", "Nome_")))

  entorno <- entorno %>%
    select(Cod_setor, Situacao_setor, !!!vars.entorno)

  dom.i <- dom.i %>%
    # V001 recebida de vars.dom, será filtrada e descartada
    select(Cod_setor, !!!vars.dom) %>%
    # renomear V002
    #   rename(V002D1 = V002) %>%
    # filtrar valores indesejados
    filter(V001 >0) %>% # n = 0
    # descartar V001
    select(-V001)

  dom.ii <- dom.ii %>%
    # esta V001 será mantida
    select(Cod_setor, V001, V012, V016) %>%
    # renomear V002
    #   rename(V002D2 = V002) %>%
    # filtrar valores indesejados
    filter(V001 >0) # n = 0

  pessoa <- pessoa %>%
    # esta V001 será renomeada para V001p
    select(Cod_setor, V001, V003, V004, V005, V006) %>%
    # Renomear campo V001 da tabela pessoa para V001p
    rename(V001p = V001) %>%
    # filtrar valores indesejados
    filter(V001p >0) # n = 0

  dom.renda <- dom.renda %>%
    # esta V002 será renomeada para V002DR
    select(Cod_setor, V002) %>%
    # renomear V002
    rename(V002DR = V002)

  resp.alfa <- resp.alfa %>%
    # esta V001 será renomeada para V001r
    select(Cod_setor, V001, V093) %>%
    # Renomear campo V001 da tabela respon.alfa para V001r
    rename(V001r = V001)

  # variáveis do dataframe dom.i refletem o número de pessoas vivendo num determinado domicílio
  dom.i <- dom.i %>%
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
  # a linha comentada abaixo sÃ³ foi utilizada para verificar se os cÃ¡lculos nas 4 linhas acima faziam sentido
  # descomentar caso queira verificar (compare a variÃ¡vel V001 de dom.i com a variÃ¡vel V422 do entorno)
  # dom.i$V001 <- rowSums(dom.i[, c("V050", "V051", "V052", "V053", "V054", "V055", "V056", "V057", "V058", "V059")])

# join --------------------------------------------------------------------

  # junta todos os DataFrames pela coluna Cod_setor
  resumo <- inner_join(
    inner_join(
      inner_join(
        inner_join(
          inner_join(entorno, dom.i, by=c("Cod_setor"), suffix = c("_entorno", "_dom.i")),
          dom.ii, by=c("Cod_setor"), suffix = c("_join_dom.i", "_dom.ii")),
        pessoa, by=c("Cod_setor"), suffix = c("_join_dom.ii", "_pessoa")),
      resp.alfa, by=c("Cod_setor"), suffix = c("_join_pessoa", "_resp.alfa")),
    dom.renda, by=c("Cod_setor"), suffix = c("_join_resp.alfa", "_dom.renda"))

  # Adiciona a informaÃ§Ã£o de bairro ao DataFrame que contem todas as demais informaÃ§Ãµes coletadas pelo Censo
  resumo <- inner_join(basico, filter(resumo, V422!="0"), by=c("Cod_setor"), suffix = c("_join5", "_basico"))
  # resumo <- resumo %>%
  #   # vars originarias de dom.renda (join5)
  #   rename(V012 = V012_join5,
  #          V003 = V003_join5,
  #          V004 = V004_join5,
  #          V005 = V005_join5,
  #          V006 = V006_join5)

  # a variÃ¡vel V002 vem do arquivo DomicilioRenda que descreve a renda total das regiÃµes definidas pelo setor censitÃ¡rio
  # ao dividir este valor total de rendas pelo nÃºmero total de pessoas (representado pela variÃ¡vel V422) obtem-se a renda per capita da regiÃ£o

  # regiÃµes onde o Censo identificou 0 pessoas, nÃ£o servem para a anÃ¡lise, portanto tais registros devem ser removidos
  # o DataFrame selected.features contem apenas as variÃ¡veis que serÃ£o utilizadas para realizar os cÃ¡lculos

  # selected.features <- select(filter(resumo, V422!="0"),features)
  resumo <- resumo %>%
    filter(V422 != 0)
  # entorno pode ser calculado sem o join - filtrar aa parte
  entorno <- entorno %>%
    filter(V422 != 0)

  # calcula a proporÃ§Ã£o de pessoas vivendo nas condiÃ§Ãµes descritas pelas variÃ¡veis selecionadas
  # features.abs <- selected.features
  # features.abs <- resumo

# calculo componentes -----------------------------------------------------

  # calcula a componente Entorno do IVC e aplica os pesos
  # Requisitos:
  # - Divide por: V422
  # - V423, V425, V427
  # - V429, V431, V433
  # - V435, V437, V439
  # - V447, V449, V451
  # - V453, V455, V457
  # - V472, V474, V476
  # - V478, V480, V482
  compEntorno <- comp_entorno(entorno, group = {{group}})

  # calcula o componente Domicílios e aplica os pesos
  # Requisitos:
  # - Divide por: V422 (entorno), V001p, V001 (??)
  # - V055, V056, V057, V058, V059
  # - V016
  # - V012 (dom.renda)
  # - V002 (dom.renda)
  compDomicilios <- comp_domicilio(resumo, group = {{group}})

  # calcula o componente Pessoas e aplica os pesos
  # Requisitos:
  # - Divide por: V422 (entorno), V001p, V001r
  # - V081,V082, V083, V084, V085, V086, V087
  # - V003, V004, V005, V006 (dom.renda)
  # - V093
  compPessoas <- comp_pessoas(resumo, group = {{group}})

  # soma todas as componentes para formar o IVC
  # subtraindo as componentes de banheiros e agua para nÃ£o penalizar as regiÃµes 100% estruturadas nesse quesito
  # ipc <- (compDomRenda * .5) + (compEntorno * .2) + (compDomicilios * .2) + (compPessoas * .05)
  resumo <- resumo %>%
    mutate(ipc = (compEntorno * (1/3)) + (compPessoas * (1/3)) + (compDomicilios * (1/3)))

# finalizacao -------------------------------------------------------------

  resumo <- select(resumo, c({{group}}, Cod_UF, Cod_setor, Cod_municipio, Nome_do_municipio,
                                         Cod_bairro, Nome_do_bairro, ipc))
                             # ,compEntorno,
                             #             # compDomRenda,
                             #             # compDomiciliosMulher,
                             #             # comp5maisdomicilio,
                             #             # compbanheiro,
                             #             # compagua,
                             #             compPessoas))

  return(resumo)

}
