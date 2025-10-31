d2r <- pi/180
r2d <- 180/pi

#' Sexagesimal to decimal degrees
#' @keywords internal
s2d <- function(deg, arcmin = 0, arcsec = 0, mas = 0, muas = 0) {
  deg + arcmin/60 + arcsec/3600 + mas/(3600*1e3) + muas/(3600*1e6)
}

#' Evaluate polynomial and its derivative
#' @keywords internal
polynomial <- function(coeff, x) {
  sum(coeff * (x^(seq_along(coeff)-1)))
}

#' @keywords internal
d_polynomial <- function(coeff, x) {
  idx <- seq_along(coeff)-1
  sum(coeff * idx * (x^(pmax(idx-1, 0))))
}

#' Julian Date from POSIXct (UTC)
#' @keywords internal
JD <- function(t) {
  tt <- as.POSIXlt(t, tz = "UTC")
  Y <- tt$year + 1900
  M <- tt$mon + 1
  D <- tt$mday + tt$hour/24 + tt$min/(24*60) + tt$sec/(24*3600)
  Yp <- ifelse(M <= 2, Y-1, Y)
  Mp <- ifelse(M <= 2, M+12, M)
  A <- floor(Yp/100)
  B <- 2 - A + floor(A/4)
  floor(365.25*(Yp+4716)) + floor(30.6001*(Mp+1)) + D + B - 1524.5
}

#' @keywords internal
Tcenturies <- function(t) (JD(t) - 2451545.0)/36525

# Coefficients
terrestrial_obliquity_coefficients <- {
  base <- c(
    s2d(23,26,21.448),
    -s2d(0,0,4680.93),
    -s2d(0,0,1.55),
    s2d(0,0,1999.25),
    -s2d(0,0,51.38),
    -s2d(0,0,249.67),
    -s2d(0,0,39.05),
    s2d(0,0,7.12),
    s2d(0,0,27.87),
    s2d(0,0,5.79),
    s2d(0,0,2.45)
  )
  (1e-2)^(seq_along(base)-1) * base
}

solar_perigee_coefficients <- c(
  280.46645 - 357.52910,
  36000.76932 - 35999.05030,
  0.0003032 + 0.0001559,
  0.00000048
)

solar_longitude_coefficients <- c(280.46645, 36000.76983, 0.0003032)

lunar_inclination_coefficients <- c(5.145)

lunar_longitude_coefficients <- c(
  218.3164591, 481267.88134236, -0.0013268, 1/538841, -1/65194000
)

lunar_node_coefficients <- c(
  125.0445550, -1934.1361849, 0.0020762, 1/467410, -1/60616000
)

lunar_perigee_coefficients <- c(
  83.3532430, 4069.0137111, -0.0103238, -1/80053, 1/18999000
)

# Auxiliaries per Schureman
#' @keywords internal
I_s <- function(N, i, omega) {
  N <- d2r * N; i <- d2r * i; omega <- d2r * omega
  cosI <- cos(i)*cos(omega) - sin(i)*sin(omega)*cos(N)
  r2d * acos(pmin(pmax(cosI, -1), 1))
}
#' @keywords internal
xi_s <- function(N, i, omega) {
  N <- d2r * N; i <- d2r * i; omega <- d2r * omega
  e1 <- cos(0.5*(omega-i))/cos(0.5*(omega+i)) * tan(0.5*N)
  e2 <- sin(0.5*(omega-i))/sin(0.5*(omega+i)) * tan(0.5*N)
  e1 <- atan(e1); e2 <- atan(e2)
  e1 <- e1 - 0.5*N; e2 <- e2 - 0.5*N
  -(e1 + e2) * r2d
}
#' @keywords internal
nu_s <- function(N, i, omega) {
  N <- d2r * N; i <- d2r * i; omega <- d2r * omega
  e1 <- cos(0.5*(omega-i))/cos(0.5*(omega+i)) * tan(0.5*N)
  e2 <- sin(0.5*(omega-i))/sin(0.5*(omega+i)) * tan(0.5*N)
  e1 <- atan(e1); e2 <- atan(e2)
  (e1 - e2) * r2d
}
#' @keywords internal
nup_s <- function(N, i, omega) {
  I <- d2r * I_s(N, i, omega)
  nu <- d2r * nu_s(N, i, omega)
  r2d * atan(sin(2*I)*sin(nu)/(sin(2*I)*cos(nu)+0.3347))
}
#' @keywords internal
nupp_s <- function(N, i, omega) {
  I <- d2r * I_s(N, i, omega)
  nu <- d2r * nu_s(N, i, omega)
  tan2nupp <- (sin(I)^2 * sin(2*nu)) / (sin(I)^2 * cos(2*nu) + 0.0727)
  r2d * 0.5 * atan(tan2nupp)
}

#' Astronomical arguments and speeds
#' @param t POSIXct time in UTC
#' @return named list of values and speeds
#' @export
astro <- function(t) {
  Tc <- Tcenturies(t)
  dT_dHour <- 1 / (24 * 365.25 * 100)
  poly <- list(
    s  = lunar_longitude_coefficients,
    h  = solar_longitude_coefficients,
    p  = lunar_perigee_coefficients,
    N  = lunar_node_coefficients,
    pp = solar_perigee_coefficients,
    `90` = c(90),
    omega = terrestrial_obliquity_coefficients,
    i  = lunar_inclination_coefficients
  )
  a <- lapply(poly, function(coeff) {
    list(
      value = (polynomial(coeff, Tc) %% 360),
      speed = d_polynomial(coeff, Tc) * dT_dHour
    )
  })
  N <- a$N$value; i <- a$i$value; omega <- a$omega$value
  a$I    <- list(value = (I_s(N,i,omega) %% 360), speed = NA_real_)
  a$xi   <- list(value = (xi_s(N,i,omega) %% 360), speed = NA_real_)
  a$nu   <- list(value = (nu_s(N,i,omega) %% 360), speed = NA_real_)
  a$nup  <- list(value = (nup_s(N,i,omega) %% 360), speed = NA_real_)
  a$nupp <- list(value = (nupp_s(N,i,omega) %% 360), speed = NA_real_)
  hour <- list(value = (JD(t) - floor(JD(t))) * 360, speed = 15.0)
  a[["T+h-s"]] <- list(
    value = hour$value + a$h$value - a$s$value,
    speed = hour$speed + a$h$speed - a$s$speed
  )
  a$P <- list(value = ((a$p$value - a$xi$value) %% 360), speed = NA_real_)
  a
}
