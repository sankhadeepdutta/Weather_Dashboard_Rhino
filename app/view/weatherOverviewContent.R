box::use(
  shiny[NS, moduleServer, div],
  shinydashboard[dashboardBody, box],
  imola[gridPanel],
  htmltools[tags]
)

box::use(app / view / weatherOverview)
box::use(app / view / gridTemplate[contentTemplate])


#' @export
ui <- function(id) {
  ns <- NS(id)
  div(
    gridPanel(
      template = contentTemplate,
      content1 = weatherOverview$ui(ns("time")),
      content2 = weatherOverview$ui(ns("rain_chance")),
      content3 = weatherOverview$ui(ns("humidity")),
      content4 = weatherOverview$ui(ns("wind_speed")),
      content5 = weatherOverview$ui(ns("uv_index")),
      content6 = weatherOverview$ui(ns("snow_chance")),
      content7 = weatherOverview$ui(ns("feels_like")),
      content8 = weatherOverview$ui(ns("overcast"))
    )
  )
}

#' @export
server <- function(id, jsonData, current_data) {
  moduleServer(id, function(input, output, session){
    weatherOverview$server(
      "time",
      title = format(Sys.Date(), "%A, %b %d"),
      icon = tags$i(class = "fa-solid fa-clock", style = "color: #ffffff;"),
      value = if (is.null(jsonData)) {
        "No data"
      } else {
        strptime(jsonData$location$localtime, format = "%Y-%m-%d %H:%M") |> format(format = "%H:%M")
      }
    )
    weatherOverview$server(
      "rain_chance",
      title = "Rain Chance",
      icon = tags$i(class = "fa-solid fa-cloud-rain", style = "color: #ffffff;"),
      value = paste(current_data$chance_of_rain, "%")
    )
    weatherOverview$server(
      "humidity",
      title = "Humidity",
      icon = tags$i(class = "fa-solid fa-droplet", style = "color: #ffffff;"),
      value = paste(current_data$humidity, "%")
    )
    weatherOverview$server(
      "wind_speed",
      title = "Wind Speed",
      icon = tags$i(class = "fa-solid fa-wind", style = "color: #ffffff;"),
      value = paste(current_data$wind_kph, "Km/h")
    )
    weatherOverview$server(
      "uv_index",
      title = "UV Index",
      icon = tags$i(class = "fa-solid fa-circle-radiation", style = "color: #ffffff;"),
      value = max(current_data$uv, 0)
    )
    weatherOverview$server(
      "snow_chance",
      title = "Snow Chance",
      icon = tags$i(class = "fa-solid fa-snowflake", style = "color: #ffffff;"),
      value = paste(current_data$chance_of_snow, "%")
    )
    weatherOverview$server(
      "feels_like",
      title = "Feels Like",
      icon = tags$i(class = "fa-solid fa-temperature-half", style = "color: #ffffff;"),
      value = paste(current_data$feelslike_c, "°C")
    )
    weatherOverview$server(
      "overcast",
      title = "Cloud Overcast",
      icon = tags$i(class = "fa-solid fa-cloud", style = "color: #ffffff;"),
      value = paste(current_data$cloud, "%")
    )
  })
}