\name{tide_at}
\alias{tide_at}
\title{Predict tide heights}
\usage{tide_at(tide, t)}
\arguments{
\item{tide}{Tide model}
\item{t}{POSIXct vector of times}
}
\value{Numeric vector of predicted heights.}
\description{Predicts tidal heights for the given times using a Tide model.}
