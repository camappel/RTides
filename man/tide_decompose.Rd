\name{tide_decompose}
\alias{tide_decompose}
\title{Fit a harmonic tide model}
\usage{tide_decompose(heights, t = NULL, t0 = NULL, interval = NULL, constituents = noaa, initial = NULL, n_period = 2, callback = NULL, full_output = FALSE)}
\arguments{
\item{heights}{Numeric vector of observed heights}
\item{t}{POSIXct times or numeric hours since t0}
\item{t0}{POSIXct start time}
\item{interval}{Numeric hours if t not given}
\item{constituents}{List of Constituent objects}
\item{initial}{Optional initial Tide model}
\item{n_period}{Minimum number of cycles}
\item{callback}{Optional function called with residuals}
\item{full_output}{If TRUE, returns optimizer output}
}
\value{A \code{Tide} model, or list with model and optim if full_output.}
\description{Fits amplitudes and phases for selected constituents by minimizing squared residuals.}
