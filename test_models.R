# Backward-compatible entry point for the method-level verification suite.
local({
  files <- vapply(sys.frames(), function(frame) if (is.character(frame$ofile)) frame$ofile else "", character(1))
  files <- files[nzchar(files)]
  args <- grep("^--file=", commandArgs(), value = TRUE)
  script <- if (length(files)) normalizePath(tail(files,1L)) else if (length(args)) normalizePath(sub("^--file=", "", args[1])) else normalizePath("test_models.R")
  source(file.path(dirname(script), "tests", "run_tests.R"))
})
