% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/removed_loops_measures_function.R
\name{compute.measures}
\alias{compute.measures}
\title{Compute several network measures of a complex network.}
\usage{
\method{compute}{measures}(
  igraph.network,
  i,
  data = fread("removed_loops/output/removed_loops_table.csv")
)
}
\arguments{
\item{igraph.network}{Network as an igraph object.}

\item{i}{Network index}
}
\value{
ID, Network Name, Reciprocity, Degree Distribution, Transitivity, Degree Assortativity, Betweenness, Closeness,
Average Path Lenght, Hierarchy, Density
}
\description{
Compute several network measures of a complex network.
}
