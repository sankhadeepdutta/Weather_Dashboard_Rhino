box::use(shinydashboard[box, valueBox], shiny[uiOutput, renderUI, moduleServer, NS, icon, HTML], htmltools[tags])

#' @export
ui <- function(id) {
  ns <- NS(id)
  uiOutput(ns("temp"))
}

#' @export
server <- function(id, current_data) {
  moduleServer(id, function(input, output, session) {
    output$temp <- renderUI({
      image_url <- current_data$condition.icon
      img_tag <- tags$img(src = image_url, alt = "Image")
      div_tag <- tags$div(img_tag, style = "text-align: center;")
      valueBox(
        width = 12,
        subtitle = HTML(paste("Current Temperature", current_data$condition.text, sep = "<br>")),
        value = paste(current_data$temp_c, "°C"),
        icon = if (is.null(current_data)) {
          NULL
        } else{
          tags$i(div_tag)
        },
        color = "navy"
      )
    })
  })
}
