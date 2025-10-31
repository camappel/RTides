d2r <- pi/180
r2d <- 180/pi

#' Create a tide model
#' @param constituents list of Constituent objects
#' @param amplitudes numeric vector
#' @param phases numeric vector (degrees)
#' @param model optional data.frame with columns constituent, amplitude, phase
#' @param radians logical: are phases in radians?
#' @export
tide_model <- function(constituents = NULL, amplitudes = NULL, phases = NULL, model = NULL, radians = FALSE) {
  if (!is.null(model)) {
    df <- model
  } else {
    stopifnot(length(constituents) == length(amplitudes), length(phases) == length(amplitudes))
    df <- data.frame(
      constituent = I(constituents),
      amplitude   = as.numeric(amplitudes),
      phase       = as.numeric(phases),
      stringsAsFactors = FALSE
    )
  }
  if (radians) df$phase <- df$phase * r2d
  df <- tide_normalize(df)
  structure(list(model = df), class = "Tide")
}

#' @keywords internal
tide_normalize <- function(df) {
  neg <- df$amplitude < 0
  df$amplitude[neg] <- -df$amplitude[neg]
  df$phase[neg] <- df$phase[neg] + 180
  df$phase <- df$phase %% 360
  df
}

#' @keywords internal
hours_since <- function(t0, t) as.numeric(difftime(t, t0, units = "hours"))

#' @keywords internal
partition_hours <- function(hours, partition = 3600) {
  rel <- hours - hours[1]
  total <- ceiling(tail(rel,1) / partition + .Machine$double.eps*10)
  idx <- floor(rel/partition)
  lapply(0:(total-1), function(i) hours[idx == i])
}

#' @keywords internal
times_from_hours <- function(t0, hours) as.POSIXct(t0 + hours*3600, origin = "1970-01-01", tz = "UTC")

#' @keywords internal
prepare <- function(constituents, t0, t = NULL, radians = TRUE) {
  if (length(t0) > 1) t0 <- t0[[1]]
  if (is.null(t)) t <- list(t0) else if (!is.list(t)) t <- as.list(t)
  a0 <- astro(t0)
  a_list <- lapply(t, astro)
  V0 <- matrix(vapply(constituents, function(c) const_V(c, a0), numeric(1)), ncol = 1)
  speed <- matrix(vapply(constituents, function(c) const_speed(c, a0), numeric(1)), ncol = 1)
  u <- lapply(a_list, function(ai) matrix(vapply(constituents, function(c) const_u(c, ai), numeric(1)), ncol = 1))
  f <- lapply(a_list, function(ai) matrix(vapply(constituents, function(c) const_f(c, ai), numeric(1)), ncol = 1))
  if (radians) {
    speed <- d2r * speed
    V0    <- d2r * V0
    u     <- lapply(u, function(x) d2r * x)
  }
  list(speed = speed, u = u, f = f, V0 = V0)
}

#' @keywords internal
tidal_series <- function(t, H, p, speed, u, f, V0) {
  as.numeric(colSums(H * f * cos(speed*t + (V0 + u) - p)))
}

#' Predict tide heights at given times
#' @param tide Tide model
#' @param t POSIXct vector of times
#' @export
tide_at <- function(tide, t) {
  t0 <- t[1]
  hours <- hours_since(t0, t)
  partition <- 240.0
  parts <- partition_hours(hours, partition)
  times_mid <- times_from_hours(t0, (seq_along(parts) - 0.5) * partition)
  prep <- prepare(tide$model$constituent, t0, times_mid, radians = TRUE)
  H <- matrix(tide$model$amplitude, ncol = 1)
  p <- matrix(d2r * tide$model$phase, ncol = 1)
  out <- unlist(mapply(function(ti, ui, fi) {
    tidal_series(ti, H, p, prep$speed, ui, fi, prep$V0)
  }, parts, prep$u, prep$f, SIMPLIFY = FALSE), use.names = FALSE)
  out
}

#' Fit a harmonic tide model
#' @param heights numeric vector
#' @param t POSIXct vector or numeric hours since t0
#' @param t0 POSIXct start time
#' @param interval numeric hours (if t not provided)
#' @param constituents list of Constituents
#' @param initial optional Tide model initial guess
#' @param n_period minimum cycles to include
#' @param callback optional function residual -> void
#' @param full_output logical return optimizer output
#' @export
tide_decompose <- function(heights, t = NULL, t0 = NULL, interval = NULL, constituents = noaa,
                           initial = NULL, n_period = 2, callback = NULL, full_output = FALSE) {
  if (!is.null(t)) {
    if (inherits(t[1], "POSIXt")) {
      hours <- hours_since(t[1], t)
      t0 <- t[1]
    } else if (!is.null(t0)) {
      hours <- as.numeric(t)
    } else stop("t can be POSIXct vector, or hours with t0 specified.")
  } else if (!is.null(t0) && !is.null(interval)) {
    hours <- (seq_along(heights) - 1) * interval
  } else stop("Provide t (POSIXct), or t (hours) with t0, or interval and t0.")

  constituents <- Filter(function(c) c$name != "Z0", constituents)
  z0 <- mean(heights, na.rm = TRUE)
  h  <- heights - z0

  a0 <- astro(t0)
  const_speeds <- vapply(constituents, function(c) const_speed(c, a0), numeric(1))
  keep <- 360.0 * n_period < tail(hours,1) * const_speeds
  constituents <- constituents[keep]
  n <- length(constituents)
  if (n == 0) stop("No constituents meet n_period criterion.")

  o <- order(hours); hours <- hours[o]; h <- h[o]

  partition <- 240.0
  parts <- partition_hours(hours, partition)
  times_mid <- times_from_hours(t0, (seq_along(parts) - 0.5) * partition)
  prep <- prepare(constituents, t0, times_mid, radians = TRUE)

  sse <- function(par) {
    H <- matrix(par[seq_len(n)], ncol = 1)
    p <- matrix(par[seq_len(n) + n], ncol = 1)
    s <- unlist(mapply(function(ti, ui, fi) {
      tidal_series(ti, H, p, prep$speed, ui, fi, prep$V0)
    }, parts, prep$u, prep$f, SIMPLIFY = FALSE), use.names = FALSE)
    res <- h - s
    if (!is.null(callback)) try(callback(res), silent = TRUE)
    sum(res*res)
  }

  amp0 <- rep(sqrt(sum(h*h))/length(h), n)
  ph0  <- rep(1.0, n)
  if (!is.null(initial)) {
    # match by constituent name where possible
    for (i in seq_len(nrow(initial$model))) {
      nm <- initial$model$constituent[[i]]$name
      j <- which(vapply(constituents, function(c) c$name, "") == nm)
      if (length(j)) {
        amp0[j] <- initial$model$amplitude[i]
        ph0[j]  <- initial$model$phase[i] * d2r
      }
    }
  }
  par0 <- c(amp0, ph0)
  fit <- stats::optim(par0, sse, method = "BFGS", control = list(reltol = 1e-7))
  par <- fit$par

  model <- data.frame(
    constituent = I(c(list(`_Z0`), constituents)),
    amplitude   = c(z0, par[seq_len(n)]),
    phase       = c(0, (par[seq_len(n) + n] * r2d)),
    stringsAsFactors = FALSE
  )
  out <- tide_model(model = tide_normalize(model), radians = FALSE)
  if (full_output) list(model = out, optim = fit) else out
}

#' Tide form number
#' @export
tide_form_number <- function(tide) {
  get_amp <- function(nm) {
    idx <- vapply(tide$model$constituent, function(c) c$name, "") == nm
    v <- tide$model$amplitude[idx]
    if (length(v)) v[1] else 0
  }
  (get_amp("K1") + get_amp("O1")) / (get_amp("M2") + get_amp("S2"))
}

#' Tide classification
#' @export
tide_classify <- function(tide) {
  f <- tide_form_number(tide)
  if (0 <= f && f <= 0.25) "semidiurnal"
  else if (f <= 1.5) "mixed (semidiurnal)"
  else if (f <= 3.0) "mixed (diurnal)"
  else "diurnal"
}
