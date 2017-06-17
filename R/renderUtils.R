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
        trigger: 'focus'
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
      `data-content` = content,
      `data-placement` = match.arg(placement, several.ok = TRUE)[1],

      # tags$i(class = "fainfo-circle")
      icon(name = "info-circle")
    )
    )
  }

filterHeader <- function(filterName, userInfo) {
  tagList(
    h4(geti18nValue(
      paste0("title.", filterName), userInfo$lang
    ), class = "filterHeader"),
    helpPopup(
      geti18nValue(paste0("help.title.", filterName), userInfo$lang),
      geti18nValue(paste0("help.content.", filterName), userInfo$lang),
    )
  )
}
