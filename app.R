library(dplyr)
library(sf)
library(shiny)
library(bslib)
library(leaflet)

##### DATA ---- #####
restaurants <- readRDS("restaurants.rds")

##### SIDEBAR ---- #####
# options_sidebar <- #sidebar(
  # open = TRUE,
options_sidebar <- list(tags$details(
  tags$summary("Filter"),
  open=TRUE,
  accordion(
    id = "side_accordion",
    multiple = TRUE,
    open = FALSE,# c("Cuisine"),
    accordion_panel(
      title = "Cuisine",
      icon = icon("utensils"),
      selectInput(
        "choose_cat", NULL,
        choices = c("(all)"),
        multiple = TRUE,
        selected = "(all)"
      ),
      checkboxInput("veg", "Vegetarian", FALSE),
      checkboxInput("gluten_free", "Gluten Free", FALSE)
    ),
    accordion_panel(
      title = "Neighborhood",
      icon = icon("location-dot"),
      selectInput(
        "choose_neighborhood", NULL,
        multiple = TRUE,
        choices = c("(all)"),
        selected = "(all)"
      )
    ),
    accordion_panel(
      title = "Meal",
      icon = icon("clock"),
      checkboxGroupInput(
        "checkbox_meal",
        NULL,
        c("Brunch ($30)" = "brunch",
          "Lunch ($30)" = "lunch",
          "Dinner ($45)" = "dinner_1",
          "Dinner ($60)" = "dinner_2")
        )
      )
), div(p("Check the", a("official guide", href="https://www.choosechicago.com/chicago-restaurant-week/"), "for more, or see a", a("fuller map.", href="https://www.google.com/maps/d/u/0/viewer?mid=1LH2nXX6dKf4RvZz7pW177mMtDYzXEho&ll=41.929949781100404%2C-87.63798645563968&z=13"), "Restaurant data from", a("Google Sheets", href="https://docs.google.com/spreadsheets/d/1WPxQtyb6pzK0sELnV52_rqb2DttBEjG2784lRTU7wXA/edit?gid=0#gid=0"), "and Foursquare location data from", a("fused.io.", href="https://docs.fused.io/blog/fused-public-udf-for-foursquare-pois/")), style = "margin-top: 1em; padding: 0 1.25em 0 1.25em;")
))

##### UI ---- #####
ui <- fluidPage(
  theme = bs_theme(
    version = 5, bootswatch = "spacelab",
    primary = "#593196",
    secondary = "#A991D4"),
  tags$head(
    tags$style(HTML("
      a, .link-primary {
        color: #593196 !important;
      }
      div.accordion {
        margin-top: 1em;
      }
                    "))),
  titlePanel("Chicago Restaurant Week 2025"),
  sidebarLayout(
    sidebarPanel(
      options_sidebar,
      width = 4),
    mainPanel(
      card(
        leafletOutput("restaurant_map"),
        full_screen = TRUE,
        height = "60%",
        max_height = "400px"
      ),
      card(
        tableOutput("table_data"),
        full_screen = TRUE,
        height = "40%",
        max_height = "300px"
      )
    )
    )
)

##### SERVER ---- #####
server <- function(input, output, session) {
  init_data <- reactive({
    restaurants
  })

  observe(
    updateSelectInput(
      session, "choose_cat",
      choices = c(sort(unique(pull(init_data(), Type))))
    )
  )

  observe(
    updateSelectInput(
      session, "choose_neighborhood",
      choices = c(sort(unique(pull(init_data(), Neighborhood))))
    )
  )

  limited_table <- reactive({
    result <- init_data()
    # print(result)
    if (length(input$choose_cat) != 0) {
      result <- result |>
        filter(Type %in% input$choose_cat)
    }
    if (length(input$choose_neighborhood) > 0) {
      result <- result |>
        filter(Neighborhood %in% input$choose_neighborhood)
    }
    if (input$veg) {
      result <- result |>
        filter(vegetarian)
    }
    if (input$gluten_free) {
      result <- result |>
        filter(`gluten free`)
    }
    if ("brunch" %in% input$checkbox_meal) {
      result <- result |>
        filter(`$30 Brunch`)
    }
    if ("lunch" %in% input$checkbox_meal) {
      result <- result |>
        filter(`$30 Lunch`)
    }
    if ("dinner_1" %in% input$checkbox_meal) {
      result <- result |>
        filter(`$45 Dinner`)
    }
    if ("dinner_2" %in% input$checkbox_meal) {
      result <- result |>
        filter(`$45 Dinner`)
    }

    result <- result |>
      mutate(
        popup = glue::glue(
          "<h5>{name}</h5>",
          "<h6><i>{Type}</i><h6>",
          "{if_else(vegetarian, \"<span style='color:green'>vegetarian</span>\", '')}",
          "{if_else(vegetarian & `gluten free`, ' | ', '')}",
          "{if_else(`gluten free`, \"<span style='color:#DAA520'>gluten free</span>\", '')}")
      )

    validate(
      need(
        nrow(result) > 0,
        "Unfortunately, there's nothing to see here.\n\nPlease be a little less picky."))

    return(result)
  })

  geo_table <- reactive({
    result <- limited_table() |>
      filter(!st_is_empty(geometry))

    error_msg <- glue::glue(
    "Unfortunately, Foursquare's location data was troublesome, so I can't map ",
    if_else(
      nrow(limited_table()) > 1, 'these ', 'this '),
    if_else(
      nrow(limited_table()) > 1, as.character(nrow(limited_table())), ''),
    if_else(
      nrow(limited_table()) > 1, ' restaurants', 'restaurant'),
    ".\n\n",
    "The fuller map might have what you're after.")

    validate(
      need(
        nrow(result) > 0,
        error_msg))

    return(result)
  })

  output$table_data <- renderTable(
    tibble::tibble(
      name = limited_table()$name,
      cuisine = limited_table()$Type,
      neighborhood = limited_table()$Neighborhood
    )
    )

  make_leaflet <- function(df) {
    df |>
      as_Spatial() |>
      leaflet() |>
      addProviderTiles("OpenStreetMap.Mapnik") |>
      addAwesomeMarkers(popup = df$popup)
  }

  output$restaurant_map <- renderLeaflet(
    geo_table() |>
      make_leaflet()
  )

}

# Run the application
shinyApp(ui = ui, server = server)
