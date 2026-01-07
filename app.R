# app.R
library(shiny)
library(dplyr)
library(ggplot2)
library(sf)
library(rnaturalearth)
library(countrycode)
library(viridis)

# ========= 1) Wczytanie danych =========
script_dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(script_dir)
dane <- readRDS("./dataset/dane.rds")


# Minimalna kontrola: upewnij się, że kluczowe kolumny istnieją
req_cols <- c("zmienna","geo","TIME_PERIOD","OBS_VALUE","unit","sex","age")
missing_cols <- setdiff(req_cols, names(dane))
if (length(missing_cols) > 0) {
  stop("Brakuje kolumn w danych: ", paste(missing_cols, collapse = ", "))
}

# ========= 2) Mapa Europy (sf) =========
europa <- ne_countries(continent = "Europe", returnclass = "sf") |>
  select(name, iso_a3, geometry)

# ========= 3) Funkcja: przygotowanie danych do mapy =========
prepare_map_df <- function(df, z, rok, sex_val, age_val) {
  out <- df |>
    filter(
      zmienna == z,
      TIME_PERIOD == rok,
      !is.na(OBS_VALUE)
    )
  
  # Filtry sex/age – tylko jeśli użytkownik wybrał (nie "Wszystkie")
  if (!is.null(sex_val) && sex_val != "Wszystkie") {
    out <- out |> filter(sex == sex_val)
  }
  if (!is.null(age_val) && age_val != "Wszystkie") {
    out <- out |> filter(age == age_val)
  }
  
  # W tym miejscu mogą dalej istnieć inne wymiary (np. wstatus/worktime).
  # Na start agregujemy do jednego wyniku na kraj (średnia).
  out <- out |>
    group_by(geo, unit) |>
    summarise(value = mean(OBS_VALUE, na.rm = TRUE), .groups = "drop") |>
    mutate(iso3 = countrycode(geo, origin = "country.name", destination = "iso3c"))
  
  out
}

# ========= 4) UI ========= 
ui <- fluidPage(
  titlePanel("Mapa ciepła Europy – dane Eurostatu"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput(
        "zmienna", "Wskaźnik (plik):",
        choices = sort(unique(dane$zmienna)),
        selected = if ("dlugosc_dnia_pracy" %in% dane$zmienna) "dlugosc_dnia_pracy" else unique(dane$zmienna)[1]
      ),
      
      uiOutput("year_ui"),
      
      selectInput("sex", "Płeć:", choices = "Wszystkie", selected = "Wszystkie"),
      selectInput("age", "Wiek:", choices = "Wszystkie", selected = "Wszystkie"),
      
      checkboxInput("only_eu_view", "Przytnij widok do Europy (bez krańców świata)", TRUE),
      
      hr(),
      helpText("Jeśli część krajów jest szara: zwykle problem z dopasowaniem nazw krajów do ISO (countrycode).")
    ),
    
    mainPanel(
      plotOutput("map_plot", height = 600),
      hr(),
      verbatimTextOutput("diag")
    )
  )
)

# ========= 5) SERVER =========
server <- function(input, output, session) {
  
  # dynamiczny wybór lat zależnie od wskaźnika
  output$year_ui <- renderUI({
    yrs <- dane |>
      filter(zmienna == input$zmienna, !is.na(TIME_PERIOD)) |>
      pull(TIME_PERIOD) |>
      unique() |>
      sort()
    
    # jeśli lata są numeryczne: slider
    if (is.numeric(yrs)) {
      sliderInput(
        "rok", "Rok:",
        min = min(yrs, na.rm = TRUE),
        max = max(yrs, na.rm = TRUE),
        value = max(yrs, na.rm = TRUE),
        step = 1,
        sep = ""
      )
    } else {
      selectInput("rok", "Rok:", choices = yrs, selected = tail(yrs, 1))
    }
  })
  
  # aktualizacja listy sex/age po zmianie wskaźnika
  observeEvent(input$zmienna, {
    dfz <- dane |> filter(zmienna == input$zmienna)
    
    sex_choices <- dfz$sex |> unique() |> sort()
    age_choices <- dfz$age |> unique() |> sort()
    
    updateSelectInput(session, "sex", choices = c("Wszystkie", sex_choices), selected = "Wszystkie")
    updateSelectInput(session, "age", choices = c("Wszystkie", age_choices), selected = "Wszystkie")
  }, ignoreInit = FALSE)
  
  # reactive: dane do mapy
  map_data <- reactive({
    req(input$zmienna)
    req(input$rok)
    
    prepare_map_df(
      df = dane,
      z = input$zmienna,
      rok = input$rok,
      sex_val = input$sex,
      age_val = input$age
    )
  })
  
  # reactive: połączone sf
  europa_joined <- reactive({
    df <- map_data()
    
    # jednostka (zwykle jedna, ale zabezpieczamy)
    unit_label <- df$unit |> unique()
    unit_label <- if (length(unit_label) == 1) unit_label else paste(unit_label, collapse = ", ")
    
    joined <- europa |>
      left_join(df, by = c("iso_a3" = "iso3"))
    
    list(sf = joined, unit = unit_label, raw = df)
  })
  
  # wykres mapy
  output$map_plot <- renderPlot({
    res <- europa_joined()
    m <- res$sf
    unit_label <- res$unit
    
    p <- ggplot(m) +
      geom_sf(aes(fill = value), color = "white", linewidth = 0.2) +
      scale_fill_viridis_c(option = "C", na.value = "grey90") +
      theme_minimal() +
      labs(
        title = paste("Wskaźnik:", input$zmienna, "| Rok:", input$rok),
        fill = unit_label
      )
    
    if (isTRUE(input$only_eu_view)) {
      # sensowne przycięcie do Europy
      p <- p + coord_sf(xlim = c(-25, 45), ylim = c(34, 72))
    }
    
    p
  })
  
  # diagnostyka (żebyś wiedział czy agregacja ma sens i co nie weszło na mapę)
  output$diag <- renderText({
    df <- map_data()
    
    # kraje bez kodu ISO3 (najczęstszy problem)
    no_iso <- df |> filter(is.na(iso3)) |> distinct(geo) |> pull(geo)
    
    # sprawdź, czy dla (geo) masz wiele rekordów przed agregacją w innych wymiarach
    # Tu już po agregacji jest 1 na geo (zwykle), ale pokażmy liczbę krajów i zakres
    rng <- df |> summarise(min = min(value, na.rm = TRUE), max = max(value, na.rm = TRUE))
    
    paste0(
      "Diagnoza:\n",
      "- Liczba krajów w danych po filtrach: ", n_distinct(df$geo), "\n",
      "- Zakres wartości: ", round(rng$min, 2), " .. ", round(rng$max, 2), "\n",
      "- Kraje, których nie umieliśmy dopasować (ISO3): ",
      if (length(no_iso) == 0) "brak" else paste(no_iso, collapse = ", ")
    )
  })
}

shinyApp(ui, server)
