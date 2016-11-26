library(shiny)
library(shinyjs)
library(semanticui)

js <- "
  $('.ui.sidebar').sidebar('toggle');
"

ui <- shinyUI(semanticPage(
  title = "Pocket analytics",
  div(class = "ui container",
    style = "margin-top: 1em",
    div(class = "ui grid",
      div(class = "four wide column",
        div(class = "ui vertical menu",
          div(class = "item",
            div(class = "header", "Pocket API"),
            div(class = "menu",
              a(class = "item", uiicon("home"), "Configuration")
            )
          ),
          div(class = "item",
            div(class = "header", "Analytics"),
            div(class = "menu",
              a(class = "item", uiicon("calendar"), "History"),
              a(class = "item", uiicon("rocket"), "Throughput"),
              a(class = "item", uiicon("clock"), "Laggerds")
            )
          )
        )
      ),
      div(class = "twelve wide column",
        div(class = "ui segment",
          h1("hello")
        )
      )
    )
  )
))

server <- shinyServer(function(input, output) {
  runjs(js)
})

shinyApp(ui, server)
