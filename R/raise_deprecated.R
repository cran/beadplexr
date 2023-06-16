
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
#' beadplexr:::raise_deprecated("XX", "YY")
#' beadplexr:::raise_deprecated(old = "XX", new = "YY", caller = "ZZZ")
#' beadplexr:::raise_deprecated(old = "XX", new = "YY", caller = "ZZZ()")
#'
raise_deprecated <- function(old, new, caller = NULL){

  if(is.null(caller)){
    warn_str <- paste("'{old}' is deprecated, use '{new}' instead.",
                      "'{old}' will be removed in the next version")
  } else{
    if(!grepl("\\(\\)$", caller)){
      caller <- paste0(caller, "()")
    }
    warn_str <- paste("'{old}' in {caller} is deprecated, use '{new}' instead.",
                      "'{old}' will be removed in the next version")
    warn_str <- gsub("\\{caller\\}", caller, warn_str)
  }

  warn_str <- gsub("\\{old\\}", old, warn_str)
  warn_str <- gsub("\\{new\\}", new, warn_str)

  .Deprecated(msg = warn_str)
}
