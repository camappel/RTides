# Rtides

Harmonic tidal analysis and prediction in R, ported from the Python package Pytides.

## Install (local)

```r
# from the package root directory
# install.packages("devtools")
# devtools::install_local(".")
```

## Usage

```r
library(Rtides)
# Fit a model
# fit <- tide_decompose(heights = heights_vec, t = times_posixct, constituents = noaa)
# Predict
# preds <- tide_at(fit, new_times_posixct)
```

Note: Update DESCRIPTION with your maintainer email/name before submitting to CRAN.
