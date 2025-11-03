# Harmonic Analysis of Tides


## Data

### [British Oceanographic Data Centre](https://www.bodc.ac.uk/data/hosted_data_systems/sea_level/uk_tide_gauge_network/) - UK Tide Gauge Network

| Field           | Value              |
|-----------------|--------------------|
| Station name    | Portsmouth         |
| Location        | UK                 |
| Latitude        | 50° 48’ N          |
| Longitude       | 01° 06’ W          |
| Datum reference | ACD = ODN − 2.73 m |
| Time reference  | GMT                |
| Resolution      | 15-minutes         |
| Units           | Metric (m)         |

*Variables*

- ASLVBG02 (Observed surface elevation from bubbler gauge relative to
  ACD)
- Residual = Observed − Predicted (provided directly)

### [NOAA](https://tidesandcurrents.noaa.gov/) Tides & Currents

| Field | Value |
|----|----|
| Station name | Southbank Riverwalk, St Johns River (8720226) |
| Location | Jacksonville, FL, USA |
| Latitude | 30° 19.2’ N |
| Longitude | 81° 39.5’ W |
| Datum reference | [MLLW = MSL - 1.06 m]((https://tidesandcurrents.noaa.gov/datums.html?id=8720226)) |
| Time reference | EST |
| Resolution | 6-minutes |
| Units | “Standard” (f) or Metric (m) |

*Variables*

- Predicted tide
- Verified (observed) tide

A tidal datum is a fixed vertical reference used to measure water
levels. Different regions use different zero-points, so the same numeric
tide height can represent different true sea levels unless a common
baseline is used.

- The Portsmouth dataset uses Admiralty Chart Datum (ACD), which is
  approximately the Lowest Astronomical Tide and is 2.73 m below the UK
  national height reference ODN (≈ mean sea level).
- The NOAA station uses Mean Lower-Low Water (MLLW) as its zero, with
  Mean Sea Level (MSL) at this location being 1.06 m above MLLW.

To make the two records directly comparable, both time series should be
transformed to a common vertical datum, typically Mean Sea Level (MSL).

## Load data

The raw data files for Portsmouth and Jacksonville are stored in
`data/`.

``` r
# Read CSV (expected columns: date, time, elevation)
portsmouth_raw <- read.csv("data/Portsmouth.csv", stringsAsFactors = FALSE)
fl_raw <- read.csv("data/Florida.csv", stringsAsFactors = FALSE)

# Build POSIXct timestamps (minute resolution) and convert elevation to MSL
portsmouth_time <- as.POSIXct(
  paste(portsmouth_raw$date, portsmouth_raw$time),
  format = "%Y-%m-%d %H:%M",
  tz = "UTC"
)
portsmouth_elev_msl <- as.numeric(portsmouth_raw$elevation) - 2.73
```

    Warning: NAs introduced by coercion

``` r
portsmouth_msl <- data.frame(time = portsmouth_time, elevation = portsmouth_elev_msl)

fl_time <- as.POSIXct(
  paste(fl_raw$date, fl_raw$time),
  format = "%Y-%m-%d %H:%M",
  tz = "UTC"
)
fl_elev_msl <- as.numeric(fl_raw$elevation) - 1.06
fl_msl <- data.frame(time = fl_time, elevation = fl_elev_msl)

# plot
plot(portsmouth_msl$time, portsmouth_msl$elevation, type = "l", xlab = "Time", ylab = "Elevation (m)", main = "Portsmouth Tides")
```

![](README_files/figure-commonmark/unnamed-chunk-1-1.png)

``` r
plot(fl_msl$time, fl_msl$elevation, type = "l", xlab = "Time", ylab = "Elevation (m)", main = "Jacksonville Tides")
```

![](README_files/figure-commonmark/unnamed-chunk-1-2.png)
