% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/removed_loops_diffusion_function.R
\name{simulate.removed.loops.diffusion}
\alias{simulate.removed.loops.diffusion}
\title{Simulate diffusion on network}
\usage{
\method{simulate}{removed.loops.diffusion}(
  j,
  p.infection,
  pct.starting.infected,
  n,
  threshold,
  runs = 10000,
  master_data = fread("removed_loops/output/master_measures_removed_loops.csv")[, 2:5]
)
}
\arguments{
\item{j}{network index (corresponds to 'Name' column in master_data)}

\item{p.infection}{probability of infection}

\item{pct.starting.infected}{percentage of nodes starting infected}

\item{n}{number of repetitions of diffusion}
}
\value{
number of infection iterations for 70% of nodes infected
}
\description{
Simulate diffusion on network
}
