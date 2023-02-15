## Function to wrap string
labelWrap <- function(string, width = 35, indent = 0, exdent = 0) {
  inputWasFactor <- FALSE
  if (is.factor(string)) {
    inputWasFactor <- TRUE
    factorOrdering <- levels(string)
    string <- as.character(string)
  }
  if (width <= 0) {
    width <- 1
  }
  out <- stringi::stri_wrap(string,
    width = width, indent = indent,
    exdent = exdent, simplify = FALSE
  )
  result <- vapply(out, stringi::stri_c, collapse = "<br />", character(1))

  return(result)
}

########################
### functions for image generation ###
########################


## Function to do an image generation http request to replicate.ai
replicate_create <- function(im_prompt) {
  response <- POST(REPLICATE_API_URL, add_headers("Authorization" = REPLICATE_API_KEY), body = list(version = REPLICATE_MODEL_VERSION, input = list(prompt = im_prompt)), encode = "json")
  content <- content(response, "parsed", encoding = "ISO-8859-1")
  return(content)
}

## Function to get generation status and image location
replicate_get <- function(create_response) {
  prediction_id <- create_response$id
  response <- GET(glue("https://api.replicate.com/v1/predictions/{prediction_id}"), add_headers("Authorization" = REPLICATE_API_KEY), encode = "json")
  content <- content(response, "parsed", encoding = "ISO-8859-1")
  status <- content$status
  while (status != "succeeded") {
    response <- GET(glue("https://api.replicate.com/v1/predictions/{prediction_id}"), add_headers("Authorization" = REPLICATE_API_KEY), encode = "json")
    content <- content(response, "parsed", encoding = "ISO-8859-1")
    status <- content$status
    if (status == "failed") {
      break
    }
  }
  return(content)
}

## Function to download image
replicate_download <- function(get_response, outputFile) {
  img_url <- get_response$output
  GET(img_url, add_headers("Authorization" = REPLICATE_API_KEY), write_disk(outputFile, overwrite = TRUE), encode = "json")
}

##################################
### text generation           ###
##################################

## Function to do text generations via http request

query <- function(payload) {
  response <- POST(HUGGING_API_URL, add_headers("Authorization" = HUGGING_API_KEY), accept("application/json"), body = list(inputs = payload, parameters = list(min_length = 70, max_length = 90, num_beams = 4, no_repeat_ngram_size = 2, temperature = 1.15, repetition_penalty = 1.5)), encode = "json")
  return(response)
}

## Function to remove unifinished sentences at end of generated text
remove_unfinished_ending <- function(string) {
  substr(string, 1, stringi::stri_locate_last_regex(string, "(\\.|\\!|\\?)")[, 1]) %>%
    stringi::stri_replace_all_regex("<(|/)a>", "")
}
