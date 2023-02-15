library(jsonlite)
library(httr)
library(htmltools)
library(stringi)
library(stringr)


#' Sends a templated mail with Postmark
#'
#' You need to set an environment variable POSTMARK_TOKEN
#' containing the Postmark authentication token.
#'
#' @param destination_emails: required, an
#' array with e-mail addresses. c("email1", "email2", ...)
#' @param template_alias (string): name of template to use
#' @param template_model (list): All values required by the template.
#' @param from (string): The from (and reply-to) address where
#' the mail comes from.
#' @param path (string): optional, path of a file to be included
#' as attachment to the mail.
#' @param stream_id (string): The Postmark MessageStream stream_id
#' where to send the message to.
#' @param token (string): The postmark API token, defaults to the system variable "POSTMARK_TOKEN"
#' @return Returns the number of failed sends.

refer_using_postmark <- function(
    destination_emails,
    template_alias,
    template_model,
    path = "",
    from = "info@cropland.be",
    stream_id = "broadcast",
    token=Sys.getenv("POSTMARK_TOKEN")
) {
  
  # Get the Postmark authentication token
  if (token == "") {
    stop(paste("No credentials set, Use the system variable POSTMARK_TOKEN"))
  }
  
  # Keep track
  success <- 0
  errors <- 0
  
  # We do not use the email/batch endpoint, but send each mail separately
  for (destination_email in destination_emails) {
    print(paste("Sending an e-mail to", destination_email))
    
    # Make sure we have an e-mail (we only allow very normal e-mail formats)
    if (!str_detect(destination_email, "^[^@]+@[^@]+(\\.[^@]+)*\\.[^@]{2,}$")) {
      print(paste("Not sending invalid e-mail", destination_email))
      errors <- errors + 1
      next
    }
    
    # Construct the e-mail message
    body <- list(
      "From" = from,
      "ReplyTo" = from,
      "To" = destination_email,
      "MessageStream" = stream_id,
      "TemplateAlias" = template_alias,
      "TemplateModel" = template_model
    )
    
    # Attach file if needed
    if (file.exists(path)) {
      # Read in the file
      attachment <- jsonlite::base64_enc(
        readBin(path, "raw", file.info(path)[1, "size"])
      )
      
      body$Attachments <- list(
        list(
          "Name" = basename(path),
          "Content" = attachment,
          # At the moment, we set ContentType always to octet-stream
          # You may want to include a utility function to set the
          # content type based on the supplied path.
          "ContentType"= "image/png",
          "ContentID"= "cid:part1.01030607.06070005@gmail.com"
        )
      )
    }
    
    # Send the e-mail message
    r <- POST("https://api.postmarkapp.com/email/withTemplate/",
              body = body,
              encode = "json",
              add_headers(
                "Accept" = "application/json",
                "Content-Type" = "application/json",
                "X-Postmark-Server-Token" = token
              )
    )
    
    # Inspect the answer
    answer <- content(r, "parsed")

    if (answer$ErrorCode == 0) {
      print(paste("All fine:", answer$Message))
      success <- success + 1
    } else {
      print(paste("Error:", answer$Message))
      errors <- errors + 1
    }
  }
  
  print(paste("Send all:", success, "success,", errors, "errors"))
  return(errors)
}
 