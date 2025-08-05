# R Configuration for teal.code development

# Set up library paths
if (Sys.getenv("RSTUDIO") == "1" || file.exists("/.dockerenv")) {
  # In dev container - use mounted volume for persistence
  user_lib <- "/usr/local/lib/R/site-library"
} else {
  # Local development - use user library
  user_lib <- "~/R/library"
}

# Ensure library directory exists
if (!dir.exists(user_lib)) {
  dir.create(user_lib, recursive = TRUE, showWarnings = FALSE)
}

# Set library paths
.libPaths(c(user_lib, .libPaths()))

# Set CRAN mirror
options(repos = c(CRAN = "https://cran.rstudio.com/"))

# Set other useful options for development
options(
  # Increase download timeout for large packages
  timeout = 300,
  # Use multiple cores for package installation
  Ncpus = parallel::detectCores(),
  # Better error handling
  error = recover,
  # Show warnings immediately
  warn = 1
)

# Print library paths on startup
cat("R library paths:\n")
cat(paste(.libPaths(), collapse = "\n"), "\n\n")
