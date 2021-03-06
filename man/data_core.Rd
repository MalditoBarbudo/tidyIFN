% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data_functions.R
\name{data_core}
\alias{data_core}
\title{Core data}
\usage{
data_core(data_sig, ifn = "ifn2", func_group = "parcela",
  db = ifn_connect(), clima_plots, diameter_classes = FALSE)
}
\arguments{
\item{data_sig}{tbl connection, generally the result of data_sig function}

\item{ifn}{character indicating the IFN version to load}

\item{func_group}{character indicating the breakdown level (parcela, especie,
especiesimpl, genere, planifconif, caducesclerconif)}

\item{db}{db connection}

\item{clima_plots}{plots resulting from the filtered clima data}

\item{diameter_classes}{logical indicating if breakdown the results by
diameter classes}
}
\description{
Function to load the "core" data, data from db tables
}
\details{
This function takes the result of data_sig to retrieve the core data at the
breakdown level for the plots selected
}
