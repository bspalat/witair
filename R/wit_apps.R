wit_url <- 'https://api.wit.ai'

get_apps <- function(auth_token, limit = 500, offset = 0) {
  check_internet()
  if (missing(auth_token)) auth_token <- witai_auth()

  res <- httr::GET(
    url = 'https://api.wit.ai/',
    path = 'apps',
    httr::add_headers(Authorization = paste0('Bearer ', auth_token)),
    query = list(
      limit = limit,
      offset = offset
    ))

  # check_status(res)

  return(list(
    apps = httr::content(res, simplifyDataFrame = TRUE),
    raw = res))
}




create_app <- function(auth_token, name, language, private, description, set_env_id = FALSE) {
  check_internet()
  if (missing(auth_token)) auth_token <- witai_auth()

  # language check? API returns error if language is invalid
  # https://github.com/vincentarelbundock/countrycode

  app_spec <- list(
    name = name,
    lang = language,
    private = private,
    desc = description
  )


  res <- httr::POST(
    url     = 'https://api.wit.ai/',
    path    = 'apps',
    httr::add_headers(Authorization = paste0('Bearer ', auth_token)),
    body    = app_spec,
    encode  = 'json')

  # check_status(res)

  resp <- list(
    new_app = httr::content(res),
    raw     = res)

  if (set_env_id) {
    if (!file.exists('./.Renviron')) file.create('./.Renviron')
    Sys.setenv(WIT_TOKEN = resp$new_app$access_token)
  }

  return(resp)
}


update_app <- function(auth_token, app_id, version = NA, name = NA, language = NA, private = NA, timezone = NA,  description = NA) {
  check_internet()
  if (missing(auth_token)) auth_token <- witai_auth()

  new_spec <- list(
    name = name,
    lang = language,
    private = private,
    timezone = timezone,
    desc = description
  )
  na.omit.list(new_spec)
  if (length(new_spec) < 1) {
    stop('Update app failed: no arguments provided', call. = FALSE)
  }

  if (is.na(version)) version <- gsub('[[:punct:]]', '', Sys.Date())

  res <- httr::PUT(
    url     = 'https://api.wit.ai/',
    path    = paste0('apps/', app_id),
    httr::add_headers(Authorization = paste0('Bearer ', auth_token)),
    body    = new_spec,
    encode  = 'json',
    query = list(
      v = version
    ))

  resp <- list(
    update_success = httr::content(res)$success,
    raw = res
  )

  return(resp)

}

#
# name <- "firstApp"
# language <- "french"
# private <- "true"
# description <- "My first app for doing stuff"
#
# app_spec %>% toJSON(auto_unbox = TRUE)
#
# library(jsonlite)
# library(magrittr)
library(httr)
?stop
