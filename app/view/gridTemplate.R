box::use(imola[gridTemplate])

#' @export
# Content grid template
contentTemplate <- gridTemplate("content", "grid", areas = list(
  default = c(
    "content1 content2 content3 content4",
    "content5 content6 content7 content8"
  ),
  lg = c(
    "content1 content2 content3 content4",
    "content5 content6 content7 content8"
  ),
  md = c(
    "content1",
    "content2",
    "content3",
    "content4",
    "content5",
    "content6",
    "content7",
    "content8"
  )
))

#' @export
two_row_custom <- gridTemplate("two-row-custom", "grid", areas = list(
  default = c("content1 content1 content1 content2"),
  lg = c("content1 content1 content1 content2"),
  md = c("content2", "content1")
))

#' @export
searchTemplate <- gridTemplate("myareas", "grid", areas = list(
  default = c("content1", "content2", "content3"),
  lg = c("content1", "content2", "content3"),
  md = c("content1", "content2", "content3")
))

