d2r <- pi/180
r2d <- 180/pi

# amplitude scale factors f
f_unity <- function(a) 1.0

f_Mm <- function(a) {
  omega <- d2r*a$omega$value; i <- d2r*a$i$value; I <- d2r*a$I$value
  mean <- (2/3 - sin(omega)^2) * (1 - 1.5*sin(i)^2)
  (2/3 - sin(I)^2) / mean
}

f_Mf <- function(a) {
  omega <- d2r*a$omega$value; i <- d2r*a$i$value; I <- d2r*a$I$value
  mean <- sin(omega)^2 * cos(0.5*i)^4
  sin(I)^2 / mean
}

f_O1 <- function(a) {
  omega <- d2r*a$omega$value; i <- d2r*a$i$value; I <- d2r*a$I$value
  mean <- sin(omega) * cos(0.5*omega)^2 * cos(0.5*i)^4
  (sin(I) * cos(0.5*I)^2) / mean
}

f_J1 <- function(a) {
  omega <- d2r*a$omega$value; i <- d2r*a$i$value; I <- d2r*a$I$value
  mean <- sin(2*omega) * (1 - 1.5*sin(i)^2)
  sin(2*I) / mean
}

f_OO1 <- function(a) {
  omega <- d2r*a$omega$value; i <- d2r*a$i$value; I <- d2r*a$I$value
  mean <- sin(omega) * sin(0.5*omega)^2 * cos(0.5*i)^4
  sin(I) * sin(0.5*I)^2 / mean
}

f_M2 <- function(a) {
  omega <- d2r*a$omega$value; i <- d2r*a$i$value; I <- d2r*a$I$value
  mean <- cos(0.5*omega)^4 * cos(0.5*i)^4
  cos(0.5*I)^4 / mean
}

f_K1 <- function(a) {
  omega <- d2r*a$omega$value; i <- d2r*a$i$value; I <- d2r*a$I$value; nu <- d2r*a$nu$value
  sin2Icosnu_mean <- sin(2*omega) * (1 - 1.5*sin(i)^2)
  mean <- 0.5023*sin2Icosnu_mean + 0.1681
  sqrt(0.2523*sin(2*I)^2 + 0.1689*sin(2*I)*cos(nu) + 0.0283) / mean
}

f_L2 <- function(a) {
  P <- d2r*a$P$value; I <- d2r*a$I$value
  R_a_inv <- sqrt(1 - 12*tan(0.5*I)^2 * cos(2*P) + 36*tan(0.5*I)^4)
  f_M2(a) * R_a_inv
}

f_K2 <- function(a) {
  omega <- d2r*a$omega$value; i <- d2r*a$i$value; I <- d2r*a$I$value; nu <- d2r*a$nu$value
  sinsqIcos2nu_mean <- sin(omega)^2 * (1 - 1.5*sin(i)^2)
  mean <- 0.5023*sinsqIcos2nu_mean + 0.0365
  sqrt(0.2523*sin(I)^4 + 0.0367*sin(I)^2 * cos(2*nu) + 0.0013) / mean
}

f_M1 <- function(a) {
  P <- d2r*a$P$value; I <- d2r*a$I$value
  Q_a_inv <- sqrt(0.25 + 1.5*cos(I)*cos(2*P)*cos(0.5*I)^(-0.5) + 2.25*cos(I)^2*cos(0.5*I)^(-4))
  f_O1(a) * Q_a_inv
}

f_Modd <- function(a, n) f_M2(a)^(n/2)

# node factors u (degrees)
u_zero <- function(a) 0.0
u_Mf   <- function(a) -2.0 * a$xi$value
u_O1   <- function(a)  2.0 * a$xi$value - a$nu$value
u_J1   <- function(a) -a$nu$value
u_OO1  <- function(a) -2.0 * a$xi$value - a$nu$value
nu_M2   <- function(a)  2.0 * a$xi$value - 2.0 * a$nu$value
u_K1   <- function(a) -a$nup$value
u_L2   <- function(a) {
  I <- d2r*a$I$value; P <- d2r*a$P$value
  R <- r2d * atan(sin(2*P) / (1/6 * tan(0.5*I)^(-2) - cos(2*P)))
  2.0*a$xi$value - 2.0*a$nu$value - R
}
nu_K2 <- function(a) -2.0 * a$nupp$value
nu_M1 <- function(a) {
  I <- d2r*a$I$value; P <- d2r*a$P$value
  Q <- r2d * atan((5*cos(I)-1)/(7*cos(I)+1) * tan(P))
  a$xi$value - a$nu$value + Q
}
nu_Modd <- function(a, n) (n/2.0) * nu_M2(a)
