#' Creating a connection to the IFN SQL database
#'
#' Wrapper of \code{\link{dbPool}} to connect to the IFN database
#'
#' There is no arguments, this is used to access as a guest to the database
#'
#' @export
ifn_connect <- function() {
  db_conn <- pool::dbPool(
    drv = RPostgreSQL::PostgreSQL(),
    user = 'guest',
    password = rstudioapi::askForPassword('Pasword for IFN db'),
    dbname = 'ifn',
    idleTimeout = 3600000
  )

  return(db_conn)
}

#' SIG data getter
#'
#' Function to load the SIG data
#'
#' This function does NOT collect the data from the db, only performs the steps
#' necessary to retrieve and filter the data if necessary from the db. The
#' collect step must be performed with the result of the function
#'
#' @param ifn character indicating the IFN version to load
#' @param db database connection
#' @param ... filter arguments to the dplyr::filter step
#'
#' @export
data_sig <- function(ifn, db = ifn_connect(), ...) {

  # enquo the filters
  dots <- dplyr::enquos(...)

  # sig name
  name_sig <- glue::glue('parcela{ifn}_sig_etrs89')

  # sig data
  res <- tbl(db, name_sig)

  # are there filters??
  if (any(!rlang::quo_is_missing(dots))) {
    res <- res %>%
      dplyr::filter(!!! dots)
  }

  return(res)

}

#' Core data
#'
#' Function to load the "core" data, data from db tables
#'
#' This function takes the result of data_sig to retrieve the core data at the
#' breakdown level for the plots selected
#'
#' @param data_sig tbl connection, generally the result of data_sig function
#' @param ifn character indicating the IFN version to load
#' @param func_group character indicating the breakdown level (parcela, especie,
#'   especiesimpl, genere, planifconif, caducesclerconif)
#' @param db db connection
data_core <- function(
  data_sig, ifn = 'ifn2', func_group = 'parcela', db = ifn_connect()
) {
  # table name
  core_name <- glue::glue('r_{func_group}_{ifn}')

  # res table, no collect
  res <- data_sig %>%
    dplyr::select(idparcela, provincia, vegueria, comarca, municipi) %>%
    dplyr::left_join(dplyr::tbl(db, core_name), by = 'idparcela')

  return(res)
}

