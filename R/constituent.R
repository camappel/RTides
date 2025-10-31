# XDO encoding map as in Python version
xdo_int <- c(
  setNames(1:9, LETTERS[1:9]),
  setNames(10:16, LETTERS[10:16]),
  setNames(17, "Q"),
  setNames(-8:-1, LETTERS[18:25]),
  Z = 0
)

int_xdo <- setNames(names(xdo_int), as.character(xdo_int))

xdo_to_coefficients <- function(xdo) {
  chars <- strsplit(xdo, "")[[1]]
  chars <- chars[chars %in% c(letters, LETTERS)]
  unname(xdo_int[toupper(chars)])
}

coefficients_to_xdo <- function(coeff) {
  paste0(int_xdo[as.character(coeff)], collapse = "")
}

astro_xdo <- function(a) list(a[["T+h-s"]], a[["s"]], a[["h"]], a[["p"]], a[["N"]], a[["pp"]], a[["90"]])

constituent <- function(name, xdo = NULL, coefficients = NULL, u, f) {
  coeff <- if (!is.null(xdo)) xdo_to_coefficients(xdo) else coefficients
  structure(list(name = name, coefficients = as.numeric(coeff), u = u, f = f), class = "Constituent")
}

const_speed <- function(cnst, a) {
  speeds <- vapply(astro_xdo(a), function(e) e$speed, numeric(1))
  sum(cnst$coefficients * speeds)
}

const_V <- function(cnst, a) {
  values <- vapply(astro_xdo(a), function(e) e$value, numeric(1))
  sum(cnst$coefficients * values)
}

const_u <- function(cnst, a) cnst$u(a)
const_f <- function(cnst, a) cnst$f(a)

compound_constituent <- function(name, members, u = NULL, f = NULL) {
  if (is.null(u)) u <- function(a) sum(vapply(members, function(m) m$n * const_u(m$constituent, a), numeric(1)))
  if (is.null(f)) f <- function(a) prod(vapply(members, function(m) const_f(m$constituent, a)^abs(m$n), numeric(1)))
  coeff <- Reduce(`+`, lapply(members, function(m) m$n * m$constituent$coefficients))
  structure(list(name = name, coefficients = as.numeric(coeff), u = u, f = f, members = members), class = "Constituent")
}

# Base Constituents
`_Z0`      <- constituent("Z0",      "Z ZZZ ZZZ", u_zero, f_unity)
`_Sa`      <- constituent("Sa",      "Z ZAZ ZZZ", u_zero, f_unity)
`_Ssa`     <- constituent("Ssa",     "Z ZBZ ZZZ", u_zero, f_unity)
`_Mm`      <- constituent("Mm",      "Z AZY ZZZ", u_zero, f_Mm)
`_Mf`      <- constituent("Mf",      "Z BZZ ZZZ", u_Mf,   f_Mf)

# Diurnals
`_Q1`      <- constituent("Q1",      "A XZA ZZA", u_O1,   f_O1)
`_O1`      <- constituent("O1",      "A YZZ ZZA", u_O1,   f_O1)
`_K1`      <- constituent("K1",      "A AZZ ZZY", u_K1,   f_K1)
`_J1`      <- constituent("J1",      "A BZY ZZY", u_J1,   f_J1)
`_M1`      <- constituent("M1",      "A ZZZ ZZA", u_M1,   f_M1)
`_P1`      <- constituent("P1",      "A AXZ ZZA", u_zero, f_unity)
`_S1`      <- constituent("S1",      "A AYZ ZZZ", u_zero, f_unity)
`_OO1`     <- constituent("OO1",     "A CZZ ZZY", u_OO1,  f_OO1)

# Semi-Diurnals
`_2N2`     <- constituent("2N2",     "B XZB ZZZ", u_M2,   f_M2)
`_N2`      <- constituent("N2",      "B YZA ZZZ", u_M2,   f_M2)
`_nu2`     <- constituent("nu2",     "B YBY ZZZ", u_M2,   f_M2)
`_M2`      <- constituent("M2",      "B ZZZ ZZZ", u_M2,   f_M2)
`_lambda2` <- constituent("lambda2", "B AXA ZZB", u_M2,   f_M2)
`_L2`      <- constituent("L2",      "B AZY ZZB", u_L2,   f_L2)
`_T2`      <- constituent("T2",      "B BWZ ZAZ", u_zero, f_unity)
`_S2`      <- constituent("S2",      "B BXZ ZZZ", u_zero, f_unity)
`_R2`      <- constituent("R2",      "B BYZ ZYB", u_zero, f_unity)
`_K2`      <- constituent("K2",      "B BZZ ZZZ", u_K2,   f_K2)

# Third-Diurnals
`_M3`      <- constituent("M3",      "C ZZZ ZZZ", function(a) nu_Modd(a,3), function(a) f_Modd(a,3))

# Compound Constituents
`_MSF`     <- compound_constituent("MSF",  list(list(constituent=`_S2`, n=1), list(constituent=`_M2`, n=-1)))
`_2Q1`     <- compound_constituent("2Q1",  list(list(constituent=`_N2`, n=1), list(constituent=`_J1`, n=-1)))
`_rho1`    <- compound_constituent("rho1", list(list(constituent=`_nu2`,n=1), list(constituent=`_K1`, n=-1)))

`_mu2`     <- compound_constituent("mu2",  list(list(constituent=`_M2`, n=2), list(constituent=`_S2`, n=-1)))
`_2SM2`    <- compound_constituent("2SM2", list(list(constituent=`_S2`, n=2), list(constituent=`_M2`, n=-1)))

`_2MK3`    <- compound_constituent("2MK3", list(list(constituent=`_M2`, n=1), list(constituent=`_O1`, n=1)))
`_MK3`     <- compound_constituent("MK3",  list(list(constituent=`_M2`, n=1), list(constituent=`_K1`, n=1)))

`_MN4`     <- compound_constituent("MN4",  list(list(constituent=`_M2`, n=1), list(constituent=`_N2`, n=1)))
`_M4`      <- compound_constituent("M4",   list(list(constituent=`_M2`, n=2)))
`_MS4`     <- compound_constituent("MS4",  list(list(constituent=`_M2`, n=1), list(constituent=`_S2`, n=1)))
`_S4`      <- compound_constituent("S4",   list(list(constituent=`_S2`, n=2)))

`_M6`      <- compound_constituent("M6",   list(list(constituent=`_M2`, n=3)))
`_S6`      <- compound_constituent("S6",   list(list(constituent=`_S2`, n=3)))

`_M8`      <- compound_constituent("M8",   list(list(constituent=`_M2`, n=4)))

#' NOAA default constituent set
#' @format list of Constituent objects
#' @export
noaa <- list(
  `_M2`,`_S2`,`_N2`,`_K1`,`_M4`,`_O1`,`_M6`,`_MK3`,`_S4`,`_MN4`,`_nu2`,`_S6`,`_mu2`,
  `_2N2`,`_OO1`,`_lambda2`,`_S1`,`_M1`,`_J1`,`_Mm`,`_Ssa`,`_Sa`,`_MSF`,`_Mf`,
  `_rho1`,`_Q1`,`_T2`,`_R2`,`_2Q1`,`_P1`,`_2SM2`,`_M3`,`_L2`,`_2MK3`,`_K2`,
  `_M8`,`_MS4`
)
