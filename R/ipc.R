ipc <- function(data) {

  # soma todas as componentes para formar o IPC
  # requisitos descritos nas funções de cada componente
  data %>%
    comp_domicilio() %>%
    comp_entorno() %>%
    comp_pessoas() %>%
    mutate(ipc =
             compEntorno    * (1/3) +
             compPessoas    * (1/3) +
             compDomicilios * (1/3)
    )
}
