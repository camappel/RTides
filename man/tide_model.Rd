\name{tide_model}
\alias{tide_model}
\title{Create a tide model}
\usage{tide_model(constituents = NULL, amplitudes = NULL, phases = NULL, model = NULL, radians = FALSE)}
\arguments{
\item{constituents}{List of Constituent objects}
\item{amplitudes}{Numeric vector}
\item{phases}{Numeric vector (degrees)}
\item{model}{Optional data.frame with columns constituent, amplitude, phase}
\item{radians}{Logical whether phases are in radians}
}
\value{An object of class \code{Tide}.}
\description{Constructs a Tide model either from components or an existing model frame.}
