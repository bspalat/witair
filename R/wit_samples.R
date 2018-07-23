post_samples <- function(samples, auth_token = NA, version = NA) {

  check_internet()

  if (missing(auth_token)) auth_token <- witai_auth()

  if (is.na(version)) version <- gsub('[[:punct:]]', '', Sys.Date())



  res <- httr::POST(
    url     = 'https://api.wit.ai/',
    path    = 'samples',
    httr::add_headers(Authorization = paste0('Bearer ', auth_token)),
    body    = app_spec,
    encode  = 'json',
    query   = list(
      v = version
    ))

}




atom_1 <- list(
  text = 'I want to fly to sfo',
  entities = list(
    list(entity = 'intent',
         value = 'flight_request'),
    list(entity = 'wit$location',
         start = 17,
         end = 20,
         value = 'sfo')
  )
)

atom_2 <- list(
  text = 'I want to fly to las vegas',
  entities = list(
    list(entity = 'intent',
         value = 'flight_request'),
    list(entity = 'wit$location',
         start = 17,
         end = 26,
         value = 'las vegas')
  )
)

test_samples <- list(atom_1, atom_2)


test_samples %>% toJSON(auto_unbox = TRUE) %>% prettify


