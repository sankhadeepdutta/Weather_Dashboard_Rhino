box::use(shinydashboard[box], shiny[uiOutput, renderUI, moduleServer, NS], plotly[plotlyOutput, renderPlotly, layout, plot_ly])

#' @export
ui <- function(id) {
  ns <- NS(id)
  box(
    background = "black",
    width = 9,
    closable = F,
    status = NULL,
    plotlyOutput(ns("forecast"))
  )
}

#' @export
server <- function(id, forecast_data) {
  moduleServer(id, function(input, output, session) {
    output$forecast <- renderPlotly({
      data <-
        tryCatch({
          forecast_data
        }, error = function(e) {
          NULL
        })
      if (is.null(data)) {
        NULL
      }
      else{
        data$time <-
          as.POSIXct(data$time, format = "%Y-%m-%d %H:%M")
        
        tooltip_text <-
          paste(
            "Date and Time:",
            format(data$time, format = "%Y-%m-%d %H:%M"),
            "Temperature:",
            data$temp_c,
            "°C"
          )
        
        plot_ly(
          data,
          x = ~ time,
          y = ~ temp_c,
          text = tooltip_text,
          hovertemplate = paste('%{text}'),
          type = "scatter",
          mode = "lines",
          name = "forecast",
          line = list(color = "white")
        ) |>
          layout(
            title = list(text = '3 days temperature forecast', x = 0),
            font = list(color = "white"),
            xaxis = list(
              title = "",
              showgrid = FALSE,
              zeroline = FALSE,
              tickfont = list(color = "white")
            ),
            yaxis = list(
              title = "",
              showgrid = FALSE,
              zeroline = FALSE,
              tickfont = list(color = "white")
            ),
            paper_bgcolor = "black",
            
            plot_bgcolor = "black"
          )
      }
    })
  })
}
