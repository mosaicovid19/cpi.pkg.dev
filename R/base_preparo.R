base_preparo <- function(Basico = NULL, Domicilio01 = NULL, Domicilio02 = NULL, DomicilioRenda = NULL, Entorno03 = NULL, Pessoa03 = NULL, Responsavel02 = NULL, con = NULL){

  ## caso o usuário passe uma conexão, usar SQL e retornar
  if(!missing(con) && !is.null(con) && dbIsValid(con)) return( base_preparo_SQL(con) )
  ## se o usuário passar apenas um argumento, a conexão será o primeiro (Basico)
  if(!missing(Basico) && dbIsValid(Basico)) return( base_preparo_SQL(Basico) )

  ## caso contrário, usar as bases individuais
  base_preparo_R(Basico, Domicilio01, Domicilio02, DomicilioRenda, Entorno03, Pessoa03, Responsavel02)
}
