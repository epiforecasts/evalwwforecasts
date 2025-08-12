#' Suppress output and messages for code.
#' @param code Code to run quietly.
#' @return The result of running the code.
#' @export
#' @examples
#' result <- quiet(message("This message should be suppressed"))
#' print(result)
quiet <- function(code) {
  sink(nullfile())
  on.exit(sink())
  return(suppressMessages(code))
}

#' Save a dataframe to a csv and return the path for targets
#' @param df dataframe to save
#' @param filename name of dataframe
#' @param path directory to save file in
#' @export
save_csv <- function(df, filename, path, allow_empty = TRUE) {
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  path <- file.path(path, filename)

  if (allow_empty | nrow(df) > 0) {
    write_csv(df, path)
  }
  return(path)
}
