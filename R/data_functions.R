#' Creating a connection to the IFN SQL database
#'
#' Wrapper of \code{\link{dbPool}} to connect to the IFN database
#'
#' There is no arguments, this is used to access as a guest to the database
#'
#' @export
ifn_connect <- function() {
  db_con <- dbPool(
    drv = RPostgreSQL::PostgreSQL(),
    user = 'guest',
    password = rstudioapi::askForPassword('Pasword for IFN db'),
    dbname = 'ifn',
    idleTimeout = 3600000
  )
}


