
#' Warn about deprecated parameters or functions
#'
#' @param old A character giving the old function or parameter.
#' @param new A character giving the replacement function or parameter.
#' @param caller A character vector giving the function with the deprecated
#'   parameter.
#'
#' @return
#'
#' Nothing. Just raise a warning
#'
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' raise_deprecated("XX", "YY")
#' raise_deprecated(old = "XX", new = "YY", caller = "ZZZ")
#' raise_deprecated(old = "XX", new = "YY", caller = "ZZZ()")
#' }
#'
raise_deprecated <- function(old, new, caller = NULL){
  warn_str <- paste("'{old}' {fun-caller} is deprecated, use '{new}' instead.",
                    "'{old}' will be removed in the next version")

  if(!is.null(caller)){
    if(!grepl("\\(\\)$", caller)){
      caller <- paste0(caller, "()")
    }
    warn_str <- gsub(
      "{fun-caller}",
      paste("in", caller),
      warn_str,
      fixed = TRUE)
  }else{
    warn_str <- gsub(
      "{fun-caller} ",
      "",
      warn_str,
      fixed = TRUE)

  }

  warn_str <- warn_str %>%
    gsub("{old}", old, ., fixed = TRUE) %>%
    gsub("{new}", new, ., fixed = TRUE)

  .Deprecated(msg = warn_str)
}

