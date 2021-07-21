VulIndex = function(basico = Basico, entorno = Entorno03, dom.i = Domicilio01, dom.ii = Domicilio02, pessoa = Pessoa03, dom.renda = DomicilioRenda, resp.alfa = Responsavel02, group = Cod_setor){

# vars --------------------------------------------------------------------

  # definiÃ§Ã£o das variÃ¡veis que farÃ£o parte do DataFrame final
  features <- c("V001","V001p","V001r","V003", "V004", "V005", "V006","V422", "V423", "V425", "V427", "V429", "V431", "V433", "V435", "V437", "V439", "V447", "V449", "V451", "V453", "V455", "V457",
                "V472", "V474", "V476", "V478", "V480", "V482", "V050", "V051", "V052", "V053", "V054", "V055", "V056", "V057", "V058", "V059",
                "V081", "V082", "V083", "V084", "V085", "V086", "V087", "V012", "V016", "V003", "V002","V093")

  # definiÃ§Ã£o de objetos vars para auxÃ­lio Ã s funÃ§Ãµes select
  vars.entorno <- vars(V422, V423, V425, V427, V429, V431, V433, V435, V437, V439, V447, V449, V451, V453, V455, V457, V472, V474, V476, V478, V480, V482)
  vars.dom <- vars(V001, V050, V051, V052, V053, V054, V055, V056, V057, V058, V059, V081, V082, V083, V084, V085, V086, V087)
  vars.resp.alfa <- vars(V093,V001)

# bases -------------------------------------------------------------------

  # seleciona as colunas de interesse do DataFrame bairros
  # basico <- select(basico, c(Cod_UF,Cod_setor, Cod_bairro, Nome_do_bairro,Cod_municipio,Nome_do_municipio))

  # seleciona apenas as variÃ¡veis de interesse de cada DataFrame, assim como define uma coluna extra (Mun) que contÃ©m o cÃ³digo do municÃ­pio
  # a variÃ¡vel Cod_setor Ã© mantida em todos os DataFrames, pois ela permite encontrar cÃ³digo e nome do bairro
  # entorno <- select(entorno, c(Cod_setor, Situacao_setor, !!!vars.entorno)) %>% mutate(Mun = substr(Cod_setor, 1, 7))
  # dom.i <- select(dom.i, c(Cod_setor, !!!vars.dom)) %>% mutate(Mun = substr(Cod_setor, 1, 7))
  # dom.ii <- select(dom.ii, c(Cod_setor, V001, V012, V016)) %>% mutate(Mun = substr(Cod_setor, 1, 7))
  # pessoa <- select(pessoa, c(Cod_setor, V001, V003, V004, V005, V006)) %>% mutate(Mun = substr(Cod_setor, 1, 7))
  # dom.renda <- select(dom.renda, c(Cod_setor, V001, V002)) %>% mutate(Mun = substr(Cod_setor, 1, 7))
  # resp.alfa <- select(resp.alfa, c(Cod_setor, V093,V001)) %>% mutate(Mun = substr(Cod_setor, 1, 7))

  # seleciona apenas as variáveis de interesse de cada DataFrame
  basico <- basico %>%
    select({{group}}, starts_with(c("Cod_", "Nome_")))

  entorno <- entorno %>%
    select(Cod_setor, Situacao_setor, !!!vars.entorno)
  dom.i <- dom.i %>%
    select(Cod_setor, !!!vars.dom)
  dom.ii <- dom.ii %>%
    select(Cod_setor, V001, V012, V016)
  pessoa <- pessoa %>%
    select(Cod_setor, V001, V003, V004, V005, V006)
  dom.renda <- dom.renda %>%
    select(Cod_setor, V001, V002)
  resp.alfa <- resp.alfa %>%
    select(Cod_setor, V001, V093)

  # Renomear campo V001 da tabela pessoa para V001p
  pessoa <- pessoa %>%
    rename(V001p = V001)

  # Renomear campo V001 da tabela respon.alfa para V001r
  resp.alfa <- resp.alfa %>%
    rename(V001r = V001)

  # Renomear coluna V002 das tabelas de domicilio i e ii renda
  # dom.i <- dom.i %>%
  #   rename(V002D1 = V002)
  # dom.ii <- dom.ii %>%
  #   rename(V002D2 = V002)
  dom.renda <- dom.renda %>%
    rename(V002DR = V002)

  # define uma variÃ¡vel que assume o identificador do municÃ­pio do Rio de Janeiro
  # id.mun <- 3304557

  # exclui a coluna Mun, pois ela jÃ¡ nÃ£o Ã© mais necessÃ¡ria, e remove as linhas com valores indesejados
  # entorno <- select(filter(entorno, V423 != "X" ), -Mun)
  # dom.i <- select(filter(dom.i, V001 > 0 & V052 != "X"), -Mun, -V001)
  # dom.ii <- select(filter(dom.ii, V001 > 0 & V012 != "X" ), -Mun)
  # pessoa <- select(filter(pessoa, V001p > 0 & V003 != "X"), -Mun)
  # dom.renda <- select(filter(dom.renda, V001 != "X"), -Mun, -V001)
  # resp.alfa <- select(filter(resp.alfa, V093 != "X"), -Mun)

  # remove as linhas com valores indesejados
  dom.i <- dom.i %>%
    filter(V001 >0) %>%
    select(-V001)
  dom.ii <- dom.ii %>%
    filter(V001 >0)
  pessoa <- pessoa %>%
    filter(V001p >0)
  dom.renda <- dom.renda %>%
    select(-V001)

  # cada variÃ¡vel do DataFrame dom.i se refere a nÃºmero de pessoas vivendo num determinado domicÃ­lio
  # como a idÃ©ia Ã© ter nÃºmero de pessoas por domicÃ­lio numa dada condiÃ§Ã£o, fazemos o multiplicaÃ§Ã£o do nÃºmero de domicÃ­lios pelo nÃºmero de pessoas
  # que vivem no domicÃ­lio
  # dom.i$V051 <- dom.i$V051 * 2; dom.i$V052 <- dom.i$V052 * 3; dom.i$V053 <- dom.i$V053 * 4; dom.i$V054 <- dom.i$V054 * 5; dom.i$V055 <- dom.i$V055 * 6;
  # dom.i$V056 <- dom.i$V056 * 7; dom.i$V057 <- dom.i$V057 * 8; dom.i$V058 <- dom.i$V058 * 9; dom.i$V059 <- dom.i$V059 * 10;

  # similar ao que foi feita nas duas linhas acima, sÃ³ que este cÃ¡lculo Ã© para definir quantas pessoas vivem em domicÃ­lios que tem mulheres
  # como mantenedoras
  # dom.i$V081 <- dom.i$V081 * 2; dom.i$V082 <- dom.i$V082 * 3; dom.i$V083 <- dom.i$V083 * 4; dom.i$V084 <- dom.i$V084 * 5;
  # dom.i$V085 <- dom.i$V085 * 6; dom.i$V086 <- dom.i$V086 * 7;

  dom.i <- dom.i %>%
    mutate(
      V051 = V051*2,
      V052 = V052*3,
      V053 = V053*4,
      V054 = V054*5,
      V055 = V055*6,
      V056 = V056*7,
      V057 = V057*8,
      V058 = V058*9,
      V059 = V059*10,
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
#
#   # adiciona a coluna IVC ao DataFrame que contem as informaÃ§Ãµes que permitem identificar o bairro de cada setor censitÃ¡rio
#   # resumo <- cbind(resumo, ipc)
#
#   # junta aos dados de UBS's, calcula a componentes da UBS, e adiciona ao IVC
#   # resumoFinal <- left_join(resumoFinal, ubs, by=c("Cod_bairro"))
#
#   # alguns bairros nÃ£o tÃªm UBS, o que resulta em NA
#   # substituir NA por 0
#   resumo[is.na(resumo)] <- 0
#
#   #compUBS <- ifelse(resumoFinal$hospital >= 5,1,
#   #                 ifelse(resumoFinal$hospital == 4,0.7,
#   #                       ifelse(resumoFinal$hospital == 3,0.5,
#   #                             ifelse(resumoFinal$hospital == 2,0.3,
#   #                                   ifelse(resumoFinal$hospital == 1,0.1,0)))))
#
#   # resumo.final <- cbind(resumo, compDomRenda, compEntorno, compDomiciliosMulher, comp5maisdomicilio, compbanheiro, compagua, compPessoas)
#
#   # resumoFinal$ivc <- resumoFinal$ivc + (1 - resumoFinal$hospital) * (1/14)
#   # resumoFinal$ipc <- resumoFinal$ipc
#   # resumoFinal$ivc <- resumoFinal$ipc + compUBS * 1
#
#   resumo <- cbind(resumo,
#                         #compDomRenda,
#                         compEntorno,
#                         # compDomiciliosMulher,
#                         #comp5maisdomicilio,
#                         #compbanheiro,
#                         #compagua,
#                         compPessoas)
#
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
