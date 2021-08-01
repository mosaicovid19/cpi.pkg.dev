base_vars <- function(base) {
  # definição de vars para auxílio ao select
  base_vars <- list(
    Basico = vars(Cod_setor, Situacao_setor, starts_with(c("Cod_", "Nome_"))),
    Domicilio01 = vars(V001, V050, V051, V052, V053, V054, V055, V056, V057, V058, V059, V081, V082, V083, V084, V085, V086, V087),
    Domicilio02 = vars(V001, V012, V016),
    DomicilioRenda = vars(V002),
    Entorno03 = vars(V422, V423, V425, V427, V429, V431, V433, V435, V437, V439, V447, V449, V451, V453, V455, V457, V472, V474, V476, V478, V480, V482),
    Pessoa03 = vars(V001, V003, V004, V005, V006),
    Responsavel02 = vars(V001, V093)
  )

  base_vars[base] %>% unlist()
}
