box::use(
  shiny[NS, moduleServer, uiOutput, renderUI, h2],
  shinydashboard[box]
)

#' @export
ui <- function(id){
  ns <- NS(id)
  box(uiOutput(ns("header")), width = 9, background = "black")
}

#' @export
server <- function(id, jsonData){
  moduleServer(id, function(input, output, session){
    output$header <- renderUI({
      text = paste(
        "Today Overview : ",
        paste(
          jsonData$location$name,
          jsonData$location$region,
          jsonData$location$country,
          sep = ", "
        )
      )
      h2(text)
    })
  })
} 