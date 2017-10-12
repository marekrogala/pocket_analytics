library(shiny)
library(shinyjs)
library(shiny.semantic)
library(magrittr)
library(shiny.router)
library(DT)
library(purrr)

port <- 9972
consumer_key <- getOption("pocket_analytics_key")
source("pocket.R")

init_js_code <- "$('#auth_code').val(Cookies.get('pocket')).keyup();" %>%
  paste0(., "") %>%
  paste0("function() { ", ., "}") %>%
  paste0("$(document).ready(", .,")")

css <- ""

menu <- (
  div(class = "ui vertical menu",
    div(class = "item",
      div(class = "header", "Pocket API"),
      div(class = "menu",
        a(class = "item", href = "/", uiicon("home"), "Info"),
        a(class = "item", href = "/config", uiicon("settings"), "Configuration")
      )
    ),
    div(class = "item",
      div(class = "header", "Analytics"),
      div(class = "menu",
        a(class = "item", href = "/history", uiicon("calendar"), "History"),
        a(class = "item", href = "/throughput", uiicon("rocket"), "Throughput"),
        a(class = "item", href = "/laggerds", uiicon("clock"), "Laggerds")
      )
    )
  )
)

page <- function(...) {
  div(class = "ui container",
    style = "margin-top: 1em",
    textInput("auth_code", label = ""),
    tags$style("#auth_code { display: none }"),
    tags$script(init_js_code),
    div(class = "ui grid",
      div(class = "four wide column",
        menu
      ),
      div(class = "twelve wide column",
        div(class = "ui segment",
          ...
        )
      )
    )
  )
}

infoPage <- page(
  h1("Info"),
  p("Short info about this dashboard. What you can do? What is the purpose? Link to the blog article."),
  uiOutput("auth_button")
)

configPage <- page(
  h1("Config")
)

historyPage <- page(
  h1("History"),
  dataTableOutput("historyData")
)

throughputPage <- page(
  h1("Throughput"),
  uisegment(
    uicards(class = "two",
      uicard(
        div(class="content",
          div(class="header", "Total oldness score"),
          div(class="description", uiOutput("total_oldness"))
        )
      ),
      uicard(
        div(class="content",
          div(class="header", "Oldness score read today"),
          div(class="description", uiOutput("today_oldness"))
        )
      )
    ),
    plotlyOutput("oldness_killed_plot")
  ),
  div(class = "ui raised segment",
    a(class = "ui olive ribbon label", "How do you read day by day?"),
    p(), p(),
    calheatmapr::callheatmaprOutput('year_reading')
  ),
  div(class = "ui raised segment",
    a(class = "ui olive ribbon label", "How do you add to pocket?"),
    p(), p(),
    calheatmapr::callheatmaprOutput('year_adding')
  ),
  div(class = "ui raised segment",
    a(class = "ui blue ribbon label", "Weekdays efficiency"),
    p(), p(),
    plotlyOutput("weekdays_plot")
  ),
  div(class = "ui raised segment",
    a(class = "ui blue ribbon label", "Day efficiency"),
    p(), p(),
    plotlyOutput("hours_plot")
  ),
  plotlyOutput("throughputPlot")
)

laggerdsPage <- page(
  h1("Laggerds"),
  plotlyOutput("laggerdsPlot")
)

router <- make_router(
  default = route("/index", infoPage),
  route("/config", configPage),
  route("/throughput", throughputPage),
  route("/laggerds", laggerdsPage),
  route("/history", historyPage)
)

ui <- shinyUI(semanticPage(
  title = "Pocket analytics",
  tags$style(css),
  shiny::tags$head(
    tags$script(src = "https://cdn.rawgit.com/js-cookie/js-cookie/master/src/js.cookie.js")
  ),
  router_ui()
))


data_holder <- NULL

server <- shinyServer(function(input, output, session) {
  router(input, output)

  # data_holder <- NULL

  bookmarks <- reactive({
    print("getting data")
    if (!is.null(data_holder)) {
      return(data_holder)
    }

    print(paste("auth_code", input$auth_code))
    shiny::validate(need(input$auth_code != '', "Need authorization code"))

    tryCatch({
      step2 <- pocketr::step2(consumer_key, code = input$auth_code)
      data_holder <<- pocketr::response_to_bookmarks(step2)
    }, error = function(err) {
      runjs("Cookies.set('pocket', ''); $('#auth_code').val('');")
    })

    data_holder
  })

  bookmarks_history <- reactive(create_history(bookmarks()))
  bookmarks_with_dates <- reactive(parse_bookmarks(bookmarks()))
  output$total_oldness <- renderUI(span(
    oldness_score(not_read_yet(bookmarks_with_dates()))
  ))
  output$today_oldness <- renderUI(span(
    oldness_score(read_days_ago(bookmarks_with_dates(), days_ago = 0))
  ))
  output$oldness_killed_plot <- renderPlotly({
    scores <- map_dbl(6:0, ~ read_days_ago(bookmarks_with_dates(), .) %>% oldness_score)
    plot_ly(y = scores, type = "bar")
  })
  output[["throughputPlot"]] <- renderPlotly(plot_throughput(bookmarks_history()))
  output[["laggerdsPlot"]] <- renderPlotly(plot_laggerds(bookmarks()))

  output[["historyData"]] <- renderDataTable({
    bookmarks() %>%
      select(title = resolved_title, url  = resolved_url, word_count, excerpt) %>%
      datatable(rownames = NULL)
  })

  output[["year_reading"]] <- calheatmapr::renderCallheatmapr(year_reads_plot(bookmarks()))
  output[["year_adding"]] <- calheatmapr::renderCallheatmapr(year_adds_plot(bookmarks()))

  output[['weekdays_plot']] <- plotly::renderPlotly(plot_weekday_reads(bookmarks_history()))
  output[['hours_plot']] <- plotly::renderPlotly(day_distribution(bookmarks_history()))

  output[["auth_button"]] <- renderUI({
    if (input$auth_code == "") {
      actionButton("auth", "Authorize me")
    } else {
      NULL
    }
  })

  get_callback_url <- function(clientData) {
    paste0(clientData$url_protocol, "//", clientData$url_hostname, ":", clientData$url_port, clientData$url_pathname)
  }

  onclick("auth", {
    callback_url <- get_callback_url(session$clientData)

    step1 <- pocketr::step1(consumer_key, callback_url = callback_url)

    runjs(paste0("Cookies.set('pocket', '", step1$code, "')"))
    runjs(paste0("window.location.href =  '", step1$visit_url, "'"))
  })


})

shinyApp(ui, server)
