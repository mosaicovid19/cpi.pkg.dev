ipc <- function(data, group = Cod_setor) {

  # soma todas as componentes para formar o IPC
  # requisitos descritos nas funções de cada componente
  data %>%
    comp_domicilio( group = {{group}} ) %>%
    comp_entorno( group = {{group}} ) %>%
    comp_pessoas( group = {{group}} ) %>%
    mutate(ipc =
             compEntorno    * (1/3) +
             compPessoas    * (1/3) +
             compDomicilios * (1/3)
    )
}
