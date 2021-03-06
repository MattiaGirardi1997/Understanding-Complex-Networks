% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/removed_loops_measures_function.R
\name{removed.loops.network.measures}
\alias{removed.loops.network.measures}
\title{Compute and append network measures to data table.}
\usage{
removed.loops.network.measures(
  network,
  i,
  path = "removed_loops/output/removed_loops_measures.csv"
)
}
\arguments{
\item{network}{Network name as an .rda object (corresponds to var_name in ICON_data)}

\item{i}{Network index}

\item{path}{Location folder of data table.}
}
\value{
data table
}
\description{
Compute and append network measures to data table.
}
