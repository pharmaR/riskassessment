% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mod_metricBox.R
\name{metricBoxServer}
\alias{metricBoxServer}
\title{Server logic for the 'Metric Box' module}
\usage{
metricBoxServer(
  id,
  title,
  desc,
  value,
  score = "NULL",
  is_perc = FALSE,
  is_url = FALSE,
  succ_icon = "check",
  unsucc_icon = "triangle-exclamation",
  icon_class = "text-success",
  type = "information"
)
}
\arguments{
\item{id}{a module id name}

\item{title}{title.}

\item{desc}{description.}

\item{value}{metric value.}

\item{score}{metric score, as a character value}

\item{is_perc}{logical is the value is a percentage?}

\item{is_url}{logical is the value a url}

\item{succ_icon}{icon used if is_true.}

\item{unsucc_icon}{icon used if not is_true.}

\item{icon_class}{string type of icon}

\item{type}{string to color the icon ("information" or "danger")}
}
\description{
Server logic for the 'Metric Box' module
}
\keyword{internal}
