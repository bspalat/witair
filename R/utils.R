witai_auth <- function() {
  token <- Sys.getenv('WIT_TOKEN')
  if (identical(token, "")) {
    stop("Please set env var WIT_TOKEN to your wit.ai personal access token",
         call. = FALSE)
  }
  return(token)
}

#' @importFrom attempt stop_if_not
#' @importFrom curl has_internet
check_internet <- function(){
  attempt::stop_if_not(.x = curl::has_internet(), msg = "Please check your internet connection")
}

#' #' @importFrom httr status_code
#' check_status <- function(res){
#'   if (httr::http_error(res)) {
#'     stop(
#'       sprintf(
#'         "wit.ai API request failed with code: [%s]\n%s\n<%s>",
#'         httr::status_code(res),
#'         httr::content(res)$error,
#'         httr::content(res)$errors,
#'         httr::content(res)$code
#'       ),
#'       call. = FALSE
#'     )
#'   }
#' }



na.omit.list <- function(y) {
  return(y[!sapply(y, function(x) all(is.na(x)))])
}
