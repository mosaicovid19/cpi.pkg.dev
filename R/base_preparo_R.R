base_preparo_R <- function(Basico, Domicilio01, Domicilio02, DomicilioRenda, Entorno03, Pessoa03, Responsavel02) {

  # seleciona apenas as variáveis de interesse de cada DataFrame
  # regiões onde o Censo identificou 0 pessoas, não servem para a análise, portanto tais registros devem ser removidos
  Basico <- Basico %>%
    base_redux(base_vars("Basico"))

  Entorno03 <- Entorno03 %>%
    base_redux(base_vars("Entorno03")) %>%
    # filtrar valores indesejados
    filter(V422 != 0) # n = 640

  Domicilio01 <- Domicilio01 %>%
    # V001 será filtrada e descartada
    base_redux(base_vars("Domicilio01")) %>%
    # filtrar valores indesejados
    filter(V001 >0) %>% # n = 0
    # descartar V001
    select(-V001)

  Domicilio02 <- Domicilio02 %>%
    # V001 será mantida
    base_redux(base_vars("Domicilio02")) %>%
    # filtrar valores indesejados
    filter(V001 >0) # n = 0

  Pessoa03 <- Pessoa03 %>%
    # V001 será renomeada (V001p)
    base_redux(base_vars("Pessoa03")) %>%
    # Renomear campo V001 da tabela Pessoa03 para V001p
    rename(V001p = V001) %>%
    # filtrar valores indesejados
    filter(V001p >0) # n = 0

  DomicilioRenda <- DomicilioRenda %>%
    # V002 será renomeada (V002DR)
    base_redux(base_vars("DomicilioRenda")) %>%
    # renomear V002
    rename(V002DR = V002)

  Responsavel02 <- Responsavel02 %>%
    # V001 será renomeada (V001r)
    base_redux(base_vars("Responsavel02")) %>%
    # Renomear campo V001 da tabela Responsavel02 para V001r
    rename(V001r = V001)

  # join --------------------------------------------------------------------

  # junta todos os DataFrames pela coluna Cod_setor
  Entorno03 %>%
    inner_join(Domicilio01, by=c("Cod_setor"), suffix = c("_entorno", "_dom01")) %>%
    inner_join(Domicilio02, by=c("Cod_setor"), suffix = c("_join_dom01", "_dom02")) %>%
    inner_join(Pessoa03, by=c("Cod_setor"), suffix = c("_join_dom02", "_pessoa")) %>%
    inner_join(Responsavel02, by=c("Cod_setor"), suffix = c("_join_pessoa", "_resp02")) %>%
    inner_join(DomicilioRenda, by=c("Cod_setor"), suffix = c("_join_resp02", "_dom.renda")) %>%
    inner_join(Basico, by=c("Cod_setor"), suffix = c("_join5", "_basico"))

}
