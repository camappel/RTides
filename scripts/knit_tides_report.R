# Knit the Portsmouth tides report to HTML

if (!requireNamespace("rmarkdown", quietly = TRUE)) {
  install.packages("rmarkdown", repos = "https://cloud.r-project.org")
}

input_file <- file.path("scripts", "tides_report.Rmd")
if (!file.exists(input_file)) {
  stop(sprintf("Input Rmd not found: %s", input_file))
}

message("Rendering ", input_file, " …")
rmarkdown::render(
  input = input_file,
  output_file = "tides_report.html",
  output_dir = "scripts",
  quiet = TRUE
)

output_path <- file.path("scripts", "tides_report.html")
message("Done. Output: ", normalizePath(output_path, winslash = "/", mustWork = FALSE))


