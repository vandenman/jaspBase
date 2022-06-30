mkdir <- function(x, clean = TRUE) {
  if (dir.exists(x))
    unlink(x, recursive = TRUE)
  dir.create(x, recursive = TRUE)
}

mkdirs <- function(..., clean = TRUE) {
  sapply(list(...), mkdir, clean = clean)
}

# order is intentional since y may be missing
expect_file_test <- function(failure_message, op, x, y) {
  testthat::expect(
  if (op == "-L" && package_version(R.Version()) < package_version("4.2.1")) {
    # definition in R-4.2.1
    Sys.readlink((!is.na(y <- Sys.readlink(x)) & nzchar(y)))
  } else {
    utils::file_test(op, x, y)
  }, failure_message)
}

expect_file    <- function(path, failure_message) expect_file_test(failure_message, "-f", path)
expect_dir     <- function(path, failure_message) expect_file_test(failure_message, "-d", path)
expect_symlink <- function(path, failure_message) expect_file_test(failure_message, "-L", path)
