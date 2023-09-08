box::use(bslib[value_box], shinydashboard[box, valueBox], shiny[uiOutput, renderUI, moduleServer, NS, h2], bsicons[bs_icon])

#' @export
ui <- function(id) {
  ns <- NS(id)
  uiOutput(ns("value"))
}

#' @export
server <- function(id, title, icon, value) {
  moduleServer(id, function(input, output, session) {
    output$value <- renderUI({
      valueBox(
        value = value,
        subtitle = title,
        icon = icon,
        width = 12,
        color = "black"
      )
    })
  })
}
