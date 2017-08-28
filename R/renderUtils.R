helpPopup <- function(title,
                      content,
                      placement = c('right', 'top', 'left', 'bottom'),
                      trigger = c('click', 'hover', 'focus', 'manual')) {
  tagList(
    singleton(tags$head(
      tags$script(
        "$(document).ready(function(){
        $('body').popover({
        selector: '[data-toggle=\"popover\"]',
        trigger: 'focus',
        container: 'body'
        });
        });"
    )
    )),
    tags$a(
      tabindex = "0",
      class = "helpIcon btn btn-mini",
      role = "button",
      `data-toggle` = "popover",
      title = title,
      `data-content` = enc2utf8(content),
      `data-placement` = match.arg(placement, several.ok = TRUE)[1],

      # tags$i(class = "fainfo-circle")
      icon(name = "info-circle")
    )
    )
  }

headerWithHelp <- function(field, lang, useTitle = FALSE) {
  title <- ifelse(useTitle, i18nText(paste0("help.title.", field), lang), "")
  tagList(
    h4(geti18nValue(
      paste0("title.", field), lang
    ), class = "filterHeader"),
    helpPopup(
      title,
      i18nText(paste0("help.content.", field), lang)
    )
  )
}

labelWithHelp <- function(field, lang, textclass= "", useTitle = FALSE) {
  title <- ifelse(useTitle, i18nText(paste0("help.title.", field), lang), "")
  tagList(
    i18nTextOutput(field, lang, inline = TRUE, class = textclass),
    helpPopup(
      title,
      geti18nValue(paste0("help.content.", field), lang)
    )
  )
}

displayError <- function(error) {
  if (!is.null(error)) {
    textOutput2(content = error, class = "error")
  }
}

displayErrors <- function(errors) {
  lapply(errors, displayError)
}

