## Packages
library(shiny)
library(ggplot2)
library(shinyvalidate)
library(gridtext)
library(ggplot2)
library(shinyjs)
library(cowplot)
library(stringi)
library(magick)
library(httr)
library(future)
library(promises)
library(glue)

## Source function to send e-mails via Postmark
source("send_postmark.R", local = TRUE)

## Source functions to interact with external model services
source("services.R", local = TRUE)

## Use the multisession plan for the future package, because
## we are running this app asynchronously
plan(multisession)

### global variables for the external model services
REPLICATE_API_URL <- "https://api.replicate.com/v1/predictions"
REPLICATE_API_KEY <- Sys.getenv("REPLICATE_API_KEY")
REPLICATE_MODEL_VERSION <- "27b93a2413e7f36cd83da926f3656280b2931564ff050bf9575f1fdf9bcd7478"
HUGGING_API_URL <- "https://i23f3kiwe0wydsw9.eu-west-1.aws.endpoints.huggingface.cloud"
HUGGING_API_KEY <- Sys.getenv("HUGGING_API_KEY")


## Static CROPLAND logo to add to the output cards
logoPlot <- magick::image_ggplot(magick::image_read("cropland.png", depth = 16))

## thematic prompts to fire up the text model
prompts <- list(
  "Good health" = c(
    "Wishing you good health and",
    "May this year bring health",
    "Health and happiness to you!",
    "May you have good health!"
  ),
  "Success in business" = c(
    "I’m so proud of all your accomplishments this year!",
    "New year, new start!",
    "May all your wildest dreams manifest in 2023!", "Cheers to more shared successes in 2023!",
    "The future is your story to write!"
  ),
  "Friendship" = c(
    "Here’s to making more memories in 2023!",
    "Thank you for your friendship!",
    "May 2023 lead you to ever deeper friendships!"
  ),
  "Family" = c(
    "May the closeness of your family fill your heart with joy!",
    "Health and happiness to you and your family!",
    "Peace, love, and happiness!",
    "I wish you love and peace in the new year!"
  )
)




## Set the UI to the index.html template
ui <- htmlTemplate("index.html", document_ = TRUE)

## Build shiny app
shinyApp(
  ui = ui,
  ## Server function
  server = function(input, output, session) {
    observeEvent(input$go, ignoreNULL = TRUE, ignoreInit = TRUE, {
      ### Check inputs using shinyValidate
      iv <- InputValidator$new()
      iv$add_rule("email", sv_required())
      iv$add_rule("email", sv_email())
      iv$add_rule("opening", sv_required())
      iv$add_rule("par1", sv_required())
      iv$add_rule("par2", sv_required())
      iv$add_rule("closing", sv_required())
      iv$add_rule("imageCharacter", sv_required())
      iv$add_rule("imageActivity", sv_required())
      iv$add_rule("imageLocation", sv_required())
      iv$add_rule("imageStyle", sv_required())
      iv$enable()
      req(iv$is_valid())


      ## Sanitize free-text

      opening <- stri_replace_all_regex(isolate(input$opening), "<(|/)a>", "") %>%
        stri_trans_totitle() %>%
        stri_replace_all_regex("[[:space:]]+", " ")


      opening <- ifelse(substr(opening, stri_length(opening), stri_length(opening)) != ",", paste0(opening, ","), opening)
      closing <- stri_replace_all_regex(isolate(input$closing), "<(|/)a>", "") %>%
        stri_trans_totitle() %>%
        stri_replace_all_regex("[[:space:]]+", " ")


      ## Toggle hiding/showing of output fields for card and download button,
      ## depending on the "Go" button in the UI
      if (input$go > 1) {
        shinyjs::hide("sharedownload")

        shinyjs::hide("card")
        shinyjs::show("loader")
      }

      ## Show the output container
      shinyjs::show("output")
      ## Click the outputlink navigation link to automatically scroll the
      ## page to the output container
      shinyjs::click("outputlink")

      ## The image reactive generates the image on the card, based on the inputs
      image <- reactive({
        diffusionPrompt <- isolate(paste(input$imageCharacter, input$imageActivity, input$imageLocation, input$imageStyle))

        ## We wrap everything in a future here to perform the computation async
        future({
          ## Perform http request for the image

          create_response <- replicate_create(diffusionPrompt)
          ## request status and download location
          get_response <- replicate_get(create_response)
          ## download image to a tempfile
          outputFile <- tempfile(fileext = ".png")
          replicate_download(get_response, outputFile)
          ## We use the magick package to read in the output png and render it as a ggplot2 object
          magick::image_ggplot(magick::image_read(outputFile))
        }) %...!% {
          ## If the call fails, return a default image so we can still display an image
          return(magick::image_ggplot(magick::image_read("www/fallback_image.png"), interpolate = TRUE))
        }
      })



      ## The textModel1 reactive generates the first paragraph of the card text,
      ## based on the inputs
      textModel1 <- reactive({
        ## We sample one element from the prompts, based on the selected category
        modelPrompt1 <- isolate(sample(prompts[[input$par1]], 1))
        ## We wrap everything in a future to make sure that we do not
        ## block the app, as the text model
        ## takes some time to generate a response
        future({
          ## API call
          modelResponse1 <- query(modelPrompt1)
          ## Extract the response
          modelResponse1 <- content(modelResponse1)[[1]]$generated_text[1]
          ## Remove unfinished ending
          modelResponse1 <- labelWrap(paste0(remove_unfinished_ending(modelResponse1)), 50)
          return(modelResponse1)
        })
      })


      ## The textModel2 reactive generates the second paragraph of the card text,
      ## based on the inputs
      textModel2 <- reactive({
        ## We sample one element from the prompts, based on the selected category
        modelPrompt2 <- isolate(sample(prompts[[input$par2]], 1))
        ## We wrap everything in a future to make sure that we do
        ## not block the app, as the text model
        ## takes some time to generate a response
        future({
          ## API call
          modelResponse2 <- query(modelPrompt2)
          ## Extract the response
          modelResponse2 <- content(modelResponse2)[[1]]$generated_text[1]
          ## Remove unfinished ending

          modelResponse2 <- labelWrap(paste0(remove_unfinished_ending(modelResponse2)), 50)
          return(modelResponse2)
        })
      })


      ## Combine the outputs of the two model calls into a single text
      textModelOutput <- reactive({
        ## We use promise_all to make sure we wait on the two
        ## subprocesses to return their results
        result <- promise_all(a = textModel1(), b = textModel2()) %...>% {
          ## Once we retrieve the response, we perform some postprocessing
          stringi::stri_replace_all_regex(paste(.$a, .$b, sep = "<br /><br />"), "<(|/)a>", "") %>%
            stringi::stri_replace_all_regex(., "[[:space:]]+", " ") %>%
            stringi::stri_replace_all_regex(., "20[0-9]{2}", "2023") ## We hard-code the year to be 2023
        }

        return(result)
      })


      ## Convert texts to a richtext_grob, a R graphical object
      ## that can be plotted with ggplot2
      textMessage <- reactive({
        textModelOutput() %...>% (function(modelsoutput) {
          txt <- gridtext::richtext_grob(paste(paste0("<span style='font-weight:bold'>", opening, "</span>"), 
                                               modelsoutput, paste0("<span style='font-weight:bold'>", closing, "</span>"),
                                               sep = "<br /><br />"))

          textplot <- cowplot::ggdraw() + cowplot::draw_grob(txt)

          return(textplot)
        })
      })

      ## Combine the image and the text into a single ggplot2 plot
      cardImage <- reactive({
        card <- promise_all(img = image(), text = textMessage()) %...>% {
          pl <- cowplot::plot_grid(.$img, .$text, ncol = 2, rel_widths = c(0.5, 0.5))
          pl <- ggdraw(pl) + draw_grob(as_grob(logoPlot), x = 0.87, y = 0.025, width = 0.1, height = 0.05)
          return(pl)
        }

        return(card)
      })


      ## Calculate the width of the image output. This is controlled by a hidden input
      ## field that measures the width of the screan via javascript
      imageWidth <- max(c(ceiling(as.numeric(input$windowWidth) * 0.7), 800))
      imageHeight <- ceiling(imageWidth / 2)

      ## Render the card, hide the loader, and show the download button and the container
      ## that wraps the card image
      output$card <- renderPlot(
        {
          ## When we resolve the final promise, we hide the loader
          cardImage() %...>% {
            shinyjs::hide("loader")
            shinyjs::show("sharedownload")
            shinyjs::show("card")
            return(.)
          }
        },
        width = imageWidth,
        height = imageHeight,
        res = 72,
        execOnResize = FALSE
      )
      outputOptions(output, "card", suspendWhenHidden = FALSE)

      ## Reactive inputs cannot be used inside code chunks that process promises/futures
      email <- input$email
      ## Based on the cardImage reactive (which contains the actual christmas card)
      ## We save the output file and we send it via Postmark
      cardImage() %...>% {
        outputPath <- tempfile(pattern = "CROPLAND_AI_Christmas_Card_", fileext = ".png")
        ggsave(outputPath, plot = ., device = "png", width = 1600, height = 800, units = "px", dpi = 144, bg = "white")
        refer_using_postmark(
          destination_emails = email,
          template_alias = "code-your-own-7",
          template_model = list(name = ""),
          path = outputPath,
          from = "geert@cropland.be",
          stream_id = "new-year-result"
        )
      }

      ## This download handler sends christmas card as a png file when
      ## the user clicks the download button
      output$download <- downloadHandler(
        filename = function() {
          paste("CROPLAND_AI_Christmas_card.png")
        },
        content = function(file) {
          cardImage() %...>% {
            ggsave(file, plot = ., device = "png", width = 1600, height = 800, units = "px", dpi = 144, bg = "white")
          }
        }, contentType = "image/png"
      )
    })
  }
)
