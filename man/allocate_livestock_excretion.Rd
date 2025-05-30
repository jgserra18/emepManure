% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Excretion.R
\name{allocate_livestock_excretion}
\alias{allocate_livestock_excretion}
\title{allocate_livestock_excretion}
\usage{
allocate_livestock_excretion(exc_total, f_allocation)
}
\arguments{
\item{exc_total}{Total nutrient excreted from livestock (kg N/yr)}

\item{f_allocation}{(0-1) allocation of time spent in different systems (ie, grazing, yards, housed)}
}
\description{
allocates livestock excretion to different systems
}
\examples{
allocate_livestock_excretion(animal_no=50, exc_coef = 100, f_allocation = 0.33)
}
