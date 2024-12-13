% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calculate_power_jobs.R
\name{calculate_power_jobs}
\alias{calculate_power_jobs}
\title{Calculate Power Jobs}
\usage{
calculate_power_jobs(
  gcamdatabase,
  dataProjFile,
  dirOutputs,
  scenOrigNames = NULL,
  regionsSelect = NULL
)
}
\arguments{
\item{gcamdatabase}{Path to the GCAM database directory.}

\item{dataProjFile}{Path to the GCAM \code{.proj} file.}

\item{dirOutputs}{Path to the directory for output files.}

\item{scenOrigNames}{A vector of scenario names to include. Default is NULL (all scenarios).}

\item{regionsSelect}{A vector of regions to include. Default is NULL (all regions).}

\item{reReadData}{Logical; if TRUE, forces reloading of GCAM data. Default is TRUE.}

\item{saveData}{Logical; if TRUE, saves the output files. Default is TRUE.}
}
\value{
A dataframe containing job potentials by region, technology, and scenario.
}
\description{
This function calculates job potentials for new capacity additions and retirements
using predefined job intensities and integrates with Plutus' gcamInvest.
}
