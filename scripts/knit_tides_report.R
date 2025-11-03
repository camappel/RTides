# Knit the Portsmouth tides report to Markdown (README.md)

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
  output_file = "README.md",
  output_dir = ".",
  output_format = rmarkdown::github_document(html_preview = FALSE),
  quiet = TRUE
)

output_path <- file.path(".", "README.md")
message("Done. Output: ", normalizePath(output_path, winslash = "/", mustWork = FALSE))


