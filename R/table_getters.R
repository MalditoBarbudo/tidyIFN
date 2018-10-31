## Tables retrieve functions ##

#' Creating a connection to the IFN SQL database
#'
#' Wrapper of \code{\link{dbPool}} to connect to the IFN database
#'
#' By default (no arguments supplied) it allows to access as a guest to the
#' database
#'
#' @param user character
#' @param password character
#' @param dbname character
#' @param idleTimeout numeric
#'
#' @export
ifn_connect <- function(
  user = 'guest',
  password = 'guest',
  dbname = 'ifndb',
  idleTimeout = 3600000
) {
  db_conn <- pool::dbPool(
    drv = RPostgreSQL::PostgreSQL(),
    user = user,
    password = password,
    dbname = dbname,
    idleTimeout = idleTimeout
  )

  return(db_conn)
}

#' Closing connection to the IFN SQL database
#'
#' Wrapper of \code{\link{poolClose}} to close the connection
#'
#' @param conn
#'
#' @export
ifn_close <- function(
  conn
) {
  pool::poolClose(conn)
}

#' Retrieve ifn results tables
#'
#' Retrieve total by plot or total broken down by functional group tables
#'
#' \code{functional_group} parameter allows to retrieve the table of plots
#'   broken down by the desired group. Allowed values are:
#'   \itemize{
#'     \item{\code{NULL} (No breakdown)}
#'     \item{\code{species}}
#'     \item{\code{species_simplified}}
#'     \item{\code{genus}}
#'     \item{\code{dsc} (Deciduous/Sclerophyllous/Conifer)}
#'     \item{\code{bc} (Broadleaf/Conifer)}
#'   }
#'
#' @param conn pool object to access the tables
#' @param ifn character indicating the ifn level
#' @param diameter_classes logical indicating if diameter classes are required (breaking down
#'   the table by diameter classes). Default to FALSE.
#' @param functional_group Functional group to retrieve table for, if any.
#'   Default to NULL (no functional group). See details for more information
#' @param ... filter expressions as in \code{\link[dplyr]{filter}}
#'
#' @export
get_ifn_tables <- function(
  conn,
  ifn,
  diameter_classes = FALSE,
  functional_group = NULL,
  ...
) {

  # dots
  dots <- rlang::quos(..., .ignore_empty = 'all')

  # diameter classes switch
  if (isTRUE(diameter_classes)) {
    dc <- 'cd_'
  } else {
    dc <- ''
  }

  # check if fg is null, in that case return the raw plot table
  if (is.null(functional_group)) {
    table_name <- glue::glue("{ifn}_parcela_{dc}res")
  } else {
    # if not null, switch to get the fg real name
    functional_group <- switch(
      functional_group,
      species = 'especie',
      species_simplified = 'especiesimp',
      genus = 'genere',
      dsc = 'cadesccon',
      bc = 'planifconif',
      parcela = 'parcela'
    )
    table_name <- glue::glue("{ifn}_{functional_group}_{dc}res")
  }

  # retrieve the data
  res <- dplyr::tbl(conn, table_name) %>%
    dplyr::filter(!!! dots) %>%
    dplyr::collect()
  # return the data
  return(res)
}

#' Get the GIS (topographic) info
#'
#' Retrieve the topographic information for the given ifn level
#'
#' @param conn pool object to access the tables
#' @param ifn character indicating the ifn level
#' @param ... filter expressions as in \code{\link[dplyr]{filter}}
#'
#' @export
get_ifn_gis <- function(
  conn, ifn, ...
) {
  dots <- rlang::quos(..., .ignore_empty = 'all')
  table_name <- glue::glue("{ifn}_sig")
  res <- dplyr::tbl(conn, table_name) %>%
    dplyr::filter(!!! dots) %>%
    dplyr::collect()
  return(res)
}

#' Get the Climatic info
#'
#' Retrieve the environmental (climatic) information for the given ifn level
#'
#' @param conn
#' @param ifn
#' @param ... filter expressions as in \code{\link[dplyr]{filter}}
#'
#' @export
get_ifn_clim <- function(
  conn, ifn, ...
) {
  dots <- rlang::quos(..., .ignore_empty = 'all')
  table_name <- glue::glue("{ifn}_clima")
  res <- dplyr::tbl(conn, table_name) %>%
    dplyr::filter(!!! dots) %>%
    dplyr::collect()
  return(res)
}
