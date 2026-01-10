# app.R ------------------------------------------------------------
suppressPackageStartupMessages({
  library(shiny)
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(ggplot2)
  library(sf)
  library(rnaturalearth)
  library(countrycode)
  library(viridis)
  library(rsconnect)
})

# =========================
# 0) KONFIGURACJA PROJEKTU
# =========================
# Wybrane wskaźniki
PKB_NAME <- "pkb_per_capita"

SELECTED_VARS <- c(
  PKB_NAME,
  "dlugosc_dnia_pracy",
  "stopa_bezrobocia",
  "korzystanie_z_internetu",
  "satysfakcja_z_zycia",
  "oczekiwana_dlugosc_zycia"
)


# Lista kodów EU-27 (ISO3) 
EU27_ISO3 <- c(
  "AUT","BEL","BGR","HRV","CYP","CZE","DNK","EST","FIN","FRA",
  "DEU","GRC","HUN","IRL","ITA","LVA","LTU","LUX","MLT","NLD",
  "POL","PRT","ROU","SVK","SVN","ESP","SWE"
)

# =========================
# 1) Wczytanie i ujednolicenie danych
# =========================

pick_value_col <- function(df) {
  cand <- c("OBS_VALUE","obs_value","value","Value","values")
  hit <- cand[cand %in% names(df)]
  if (length(hit) == 0) stop("Brak kolumny z wartościami w jednym z DF.")
  hit[1]
}

# Funkcja przygotowująca dane z listy Eurostatu do formatu długiego (zmienna, geo, rok, value)
prep_one_from_list <- function(df, name) {
  val <- pick_value_col(df)
  if (!("geo" %in% names(df))) stop(name, ": brak kolumny geo")
  if (!("TIME_PERIOD" %in% names(df))) stop(name, ": brak kolumny TIME_PERIOD")
  
  df %>%
    filter(!is.na(.data[[val]])) %>%
    transmute(
      zmienna = name,
      geo = as.character(geo),
      rok = suppressWarnings(as.integer(TIME_PERIOD)),
      value = suppressWarnings(as.numeric(.data[[val]]))
    ) %>%
    filter(!is.na(rok), !is.na(value), !is.na(geo)) %>%
    group_by(zmienna, geo, rok) %>%
    summarise(value = mean(value, na.rm = TRUE), .groups = "drop")
}

# Funkcja przygotowująca dane z pojedynczej tabeli użytkownika do formatu długiego
prep_from_table <- function(dane_tbl) {
  
  # obsługiwane tylko te nazwy kolumn
  stopifnot(all(c("zmienna","geo","TIME_PERIOD","OBS_VALUE") %in% names(dane_tbl)))
  
  dane_tbl %>%
    transmute(
      zmienna = as.character(zmienna),
      geo = as.character(geo),
      rok = suppressWarnings(as.integer(TIME_PERIOD)),
      value = suppressWarnings(as.numeric(OBS_VALUE))
    ) %>%
    filter(!is.na(rok), !is.na(value), !is.na(geo)) %>%
    group_by(zmienna, geo, rok) %>%
    summarise(value = mean(value, na.rm = TRUE), .groups = "drop")
}

load_input_as_long <- function(path = "input.rds") {
  obj <- readRDS(path)
  
  if (is.list(obj) && !is.data.frame(obj)) {
    # lista DF
    long_all <- imap_dfr(obj, prep_one_from_list)
  } else if (is.data.frame(obj)) {
    # jedna tabela
    long_all <- prep_from_table(obj)
  } else {
    stop("input.rds musi być listą data.frame'ów albo jedną tabelą (data.frame/tibble).")
  }
  
  long_all
}

data <- load_input_as_long()
# =========================
# 2) Mapa Europy + łączenie krajów
# =========================
europa_sf <- ne_countries(continent = "Europe", returnclass = "sf") %>%
  mutate(iso_a3 = ifelse(iso_a3 == "-99", adm0_a3, iso_a3)) %>%
  select(name, iso_a3, geometry)

# =========================
# 3) UI
# =========================
ui <- navbarPage(
  "Eurostat: PKB, praca i dobrostan w Europie",
  tabPanel("Mapa",
           sidebarLayout(
             sidebarPanel(
               selectInput("map_var", "Wskaźnik:", choices = SELECTED_VARS, selected = "dlugosc_dnia_pracy"),
               uiOutput("map_year_ui"),
               checkboxInput("only_eu", "Pokaż tylko UE (EU-27)", FALSE),
               helpText("Mapa: wartość wskaźnika w danym roku (kraj–rok).")
             ),
             mainPanel(
               plotOutput("map_plot", height = 620),
               verbatimTextOutput("map_diag")
             )
           )
  ),
  tabPanel("Rankingi",
           sidebarLayout(
             sidebarPanel(
               selectInput("rank_var", "Wskaźnik:", choices = SELECTED_VARS, selected = "dlugosc_dnia_pracy"),
               uiOutput("rank_year_ui"),
               radioButtons("rank_mode", "Tryb:", choices = c("Top 10"="top", "Bottom 10"="bottom"), inline = TRUE),
               checkboxInput("only_eu_rank", "Tylko UE", FALSE)
             ),
             mainPanel(
               plotOutput("rank_plot", height = 360),
               hr(),
               uiOutput("delta_year_ui"),
               plotOutput("delta_plot", height = 360)
             )
           )
  ),
  tabPanel("Trendy",
           sidebarLayout(
             sidebarPanel(
               selectInput("trend_var", "Wskaźnik:", choices = SELECTED_VARS, selected = "dlugosc_dnia_pracy"),
               uiOutput("country_ui"),
               checkboxInput("only_eu_trend", "Tylko UE", FALSE)
             ),
             mainPanel(
               plotOutput("trend_country_plot", height = 360),
               hr(),
               plotOutput("trend_top_plot", height = 360)
             )
           )
  ),
  tabPanel("Zależności (PKB)",
           sidebarLayout(
             sidebarPanel(
               uiOutput("scatter_year_ui"),
               selectInput("scatter_y", "Y (vs PKB):", choices = setdiff(SELECTED_VARS, PKB_NAME),
                           selected = "dlugosc_dnia_pracy"),
               checkboxInput("only_eu_scatter", "Tylko UE", FALSE),
               hr(),
               selectInput("scatter_x2", "X (dowolne):", choices = SELECTED_VARS, selected = PKB_NAME),
               selectInput("scatter_y2", "Y (dowolne):", choices = SELECTED_VARS, selected = "ryzyko_ubostwa")
             ),
             mainPanel(
               plotOutput("scatter_pkb_plot", height = 360),
               hr(),
               plotOutput("scatter_any_plot", height = 360)
             )
           )
  ),
  tabPanel("Korelacje",
           sidebarLayout(
             sidebarPanel(
               radioButtons("cor_scope", "Zakres:", choices = c("Wszystkie lata (panel)"="all", "Jeden rok (przekrój)"="year"),
                            inline = TRUE),
               uiOutput("cor_year_ui"),
               checkboxInput("only_eu_cor", "Tylko UE", FALSE),
               helpText("Korelacje Spearmana (odporne na outliery).")
             ),
             mainPanel(
               plotOutput("cor_heatmap", height = 650)
             )
           )
  ),
  tabPanel("Dane i metodologia",
           fluidRow(
             column(8,
                    h4("Fabuła projektu"),
                    tags$ul(
                      tags$li("Mapa: jak wskaźniki różnią się przestrzennie w Europie."),
                      tags$li("Rankingi: kto jest liderem/outsiderem w danym roku i kto się zmienia."),
                      tags$li("Trendy: jak wskaźniki zmieniają się w czasie w krajach."),
                      tags$li("Zależności: czy PKB per capita wiąże się z dobrostanem i pracą."),
                      tags$li("Korelacje: które wskaźniki są ze sobą powiązane (i czy to stabilne).")
                    ),
                    h4("Co to znaczy kraj–rok?"),
                    p("Jedna obserwacja to jeden kraj w jednym roku. Dla każdego wskaźnika dane są agregowane tak, by mieć jedną wartość na (kraj, rok)."),
                    h4("Uwaga interpretacyjna"),
                    p("Korelacja nie oznacza przyczynowości. Wyniki traktujemy eksploracyjnie i w raporcie wspieramy je analizą panelową (FE/RE).")
             )
           )
  )
)

# =========================
# 4) SERVER
# =========================
server <- function(input, output, session) {
  
  # wczytanie danych na start
  long_all <- reactiveVal(NULL)
  
  observe({
    df <- load_input_as_long("input.rds") %>%
      filter(zmienna %in% SELECTED_VARS)
    
    # przypisanie ISO3 do krajów oraz mapy UE
    df <- df %>%
      mutate(iso3 = countrycode(geo, origin = "country.name", destination = "iso3c"))
    
    long_all(df)
  })
  
  available_vars <- reactive({
    df <- long_all(); req(df)
    intersect(SELECTED_VARS, unique(df$zmienna))
  })
  
  # Helper: filtr UE lub cała Europa (po iso3)
  filter_eu <- function(df, only_eu) {
    if (isTRUE(only_eu)) df %>% filter(iso3 %in% EU27_ISO3) else df
  }
  
  # Helper: dane jednego wskaźnika w roku
  get_var_year <- function(var, year, only_eu_flag) {
    df <- long_all()
    req(df)
    
    out <- df %>%
      filter(zmienna == var, rok == year) %>%
      filter_eu(only_eu_flag) %>%
      group_by(geo, iso3) %>%
      summarise(value = mean(value, na.rm = TRUE), .groups = "drop")
    
    out
  }
  
  # Dynamiczne lata dla wybranego wskaźnika
  years_for_var <- function(var) {
    df <- long_all(); req(df)
    sort(unique(df$rok[df$zmienna == var]))
  }
  
  # UI: lata
  output$map_year_ui <- renderUI({
    yrs <- years_for_var(input$map_var)
    sliderInput("map_year", "Rok:", min(yrs), max(yrs), value = max(yrs), step = 1, sep = "")
  })
  
  output$rank_year_ui <- renderUI({
    yrs <- years_for_var(input$rank_var)
    sliderInput("rank_year", "Rok:", min(yrs), max(yrs), value = max(yrs), step = 1, sep = "")
  })
  
  output$delta_year_ui <- renderUI({
    yrs <- years_for_var(input$rank_var)
    tagList(
      sliderInput("delta_y1", "Rok start:", min(yrs), max(yrs), value = min(yrs), step = 1, sep = ""),
      sliderInput("delta_y2", "Rok koniec:", min(yrs), max(yrs), value = max(yrs), step = 1, sep = "")
    )
  })
  
  output$scatter_year_ui <- renderUI({
    df <- long_all(); req(df)
    yrs <- sort(unique(df$rok))
    sliderInput("scatter_year", "Rok:", min(yrs), max(yrs), value = max(yrs), step = 1, sep = "")
  })
  
  output$cor_year_ui <- renderUI({
    df <- long_all(); req(df)
    if (input$cor_scope == "year") {
      yrs <- sort(unique(df$rok))
      sliderInput("cor_year", "Rok:", min(yrs), max(yrs), value = max(yrs), step = 1, sep = "")
    } else NULL
  })
  
  
  # UI: kraje do trendu (wg dostępnych danych)
  output$country_ui <- renderUI({
    df <- long_all(); req(df)
    d <- df %>% filter(zmienna == input$trend_var)
    if (isTRUE(input$only_eu_trend)) d <- d %>% filter(iso3 %in% EU27_ISO3)
    choices <- sort(unique(d$geo))
    selectInput("country", "Kraj:", choices = choices, selected = if ("Poland" %in% choices) "Poland" else choices[1])
  })
  
  # =========================
  # WYKRES 1: MAPA
  # =========================
  output$map_plot <- renderPlot({
    req(input$map_var, input$map_year)
    df <- get_var_year(input$map_var, input$map_year, input$only_eu)
    
    # join do mapy (Europa)
    m <- europa_sf %>% left_join(df, by = c("iso_a3" = "iso3"))
    
    # jeśli tylko UE, przytnij geometrię do krajów UE (żeby nie świeciła reszta)
    if (isTRUE(input$only_eu)) {
      m <- m %>% filter(iso_a3 %in% EU27_ISO3)
    }
    
    ggplot(m) +
      geom_sf(aes(fill = value), color = "white", linewidth = 0.2) +
      scale_fill_viridis_c(option = "C", na.value = "grey90") +
      theme_minimal() +
      coord_sf(xlim = c(-25, 45), ylim = c(34, 72)) +
      labs(
        title = paste("Mapa:", input$map_var, "| Rok:", input$map_year),
        fill = "Wartość"
      )
  })
  
  output$map_diag <- renderText({
    df <- long_all(); req(df)
    d <- df %>% filter(zmienna == input$map_var, rok == input$map_year)
    if (isTRUE(input$only_eu)) d <- d %>% filter(iso3 %in% EU27_ISO3)
    
    no_iso <- d %>% filter(is.na(iso3)) %>% distinct(geo) %>% pull(geo)
    
    paste0(
      "Diagnoza:\n",
      "- obserwacje (kraj–rok) dla tego widoku: ", nrow(d), "\n",
      "- kraje bez dopasowania ISO3: ", ifelse(length(no_iso)==0, "brak", paste(no_iso, collapse = ", "))
    )
  })
  
  # =========================
  # WYKRES 2: RANKING TOP/BOTTOM
  # =========================
  output$rank_plot <- renderPlot({
    req(input$rank_var, input$rank_year)
    df <- get_var_year(input$rank_var, input$rank_year, input$only_eu_rank) %>%
      filter(!is.na(value))
    
    df <- if (input$rank_mode == "top") {
      df %>% arrange(desc(value)) %>% slice_head(n = 10)
    } else {
      df %>% arrange(value) %>% slice_head(n = 10)
    }
    
    ggplot(df, aes(reorder(geo, value), value)) +
      geom_col() +
      coord_flip() +
      theme_minimal() +
      labs(
        title = paste("Ranking:", input$rank_var, "| Rok:", input$rank_year),
        x = "", y = "Wartość"
      )
  })
  
  # =========================
  # WYKRES 3: DELTA (rok2-rok1)
  # =========================
  output$delta_plot <- renderPlot({
    req(input$rank_var, input$delta_y1, input$delta_y2)
    y1 <- input$delta_y1
    y2 <- input$delta_y2
    validate(need(y2 != y1, "Wybierz dwa różne lata."))
    
    a <- get_var_year(input$rank_var, y1, input$only_eu_rank) %>%
      rename(v1 = value)
    b <- get_var_year(input$rank_var, y2, input$only_eu_rank) %>%
      rename(v2 = value)
    
    d <- inner_join(a, b, by = c("geo","iso3")) %>%
      mutate(delta = v2 - v1) %>%
      arrange(desc(abs(delta))) %>%
      slice_head(n = 15)
    
    ggplot(d, aes(reorder(geo, delta), delta)) +
      geom_col() +
      coord_flip() +
      theme_minimal() +
      labs(
        title = paste("Zmiana:", input$rank_var, "|", y2, "minus", y1),
        x = "", y = "Delta"
      )
  })
  
  # =========================
  # WYKRES 4: TREND - jeden kraj
  # =========================
  output$trend_country_plot <- renderPlot({
    req(input$trend_var, input$country)
    df <- long_all(); req(df)
    
    d <- df %>% filter(zmienna == input$trend_var)
    if (isTRUE(input$only_eu_trend)) d <- d %>% filter(iso3 %in% EU27_ISO3)
    
    d <- d %>% filter(geo == input$country)
    
    ggplot(d, aes(rok, value)) +
      geom_line() +
      geom_point(size = 1.5) +
      theme_minimal() +
      labs(
        title = paste("Trend:", input$trend_var, "|", input$country),
        x = "Rok", y = "Wartość"
      )
  })
  
  # =========================
  # WYKRES 5: TREND - TOP5 + Polska (po ostatnim roku)
  # =========================
  output$trend_top_plot <- renderPlot({
    req(input$trend_var)
    df <- long_all(); req(df)
    
    d <- df %>% filter(zmienna == input$trend_var)
    if (isTRUE(input$only_eu_trend)) d <- d %>% filter(iso3 %in% EU27_ISO3)
    
    last_year <- max(d$rok, na.rm = TRUE)
    
    top5 <- d %>%
      filter(rok == last_year) %>%
      group_by(geo) %>% summarise(v = mean(value, na.rm = TRUE), .groups="drop") %>%
      arrange(desc(v)) %>%
      slice_head(n = 5) %>%
      pull(geo)
    
    keep <- unique(c(top5, "Poland"))
    
    dd <- d %>% filter(geo %in% keep) %>%
      group_by(geo, rok) %>%
      summarise(value = mean(value, na.rm = TRUE), .groups="drop")
    
    ggplot(dd, aes(rok, value, color = geo)) +
      geom_line() +
      theme_minimal() +
      labs(
        title = paste("Trend (Top5 + Poland):", input$trend_var),
        x = "Rok", y = "Wartość", color = "Kraj"
      )
  })
  
  # =========================
  # WYKRES 6: SCATTER PKB vs Y (w jednym roku)
  # =========================
  output$scatter_pkb_plot <- renderPlot({
    req(input$scatter_year, input$scatter_y)
    df <- long_all(); req(df)
    
    pkb <- df %>% filter(zmienna == PKB_NAME, rok == input$scatter_year) %>%
      select(geo, iso3, pkb = value)
    y <- df %>% filter(zmienna == input$scatter_y, rok == input$scatter_year) %>%
      select(geo, iso3, y = value)
    
    j <- inner_join(pkb, y, by = c("geo","iso3"))
    if (isTRUE(input$only_eu_scatter)) j <- j %>% filter(iso3 %in% EU27_ISO3)
    
    ggplot(j, aes(pkb, y)) +
      geom_point(alpha = 0.75, size = 2) +
      geom_smooth(method = "lm", se = FALSE) +
      theme_minimal() +
      labs(
        title = paste("PKB per capita vs", input$scatter_y, "| Rok:", input$scatter_year),
        x = "PKB per capita",
        y = input$scatter_y
      )
  })
  
  # =========================
  # WYKRES 7: SCATTER dowolne X vs Y (w jednym roku)
  # =========================
  output$scatter_any_plot <- renderPlot({
    req(input$scatter_year, input$scatter_x2, input$scatter_y2)
    df <- long_all(); req(df)
    
    x <- df %>% filter(zmienna == input$scatter_x2, rok == input$scatter_year) %>%
      select(geo, iso3, x = value)
    y <- df %>% filter(zmienna == input$scatter_y2, rok == input$scatter_year) %>%
      select(geo, iso3, y = value)
    
    j <- inner_join(x, y, by = c("geo","iso3"))
    if (isTRUE(input$only_eu_scatter)) j <- j %>% filter(iso3 %in% EU27_ISO3)
    
    ggplot(j, aes(x, y)) +
      geom_point(alpha = 0.75, size = 2) +
      geom_smooth(method = "lm", se = FALSE) +
      theme_minimal() +
      labs(
        title = paste(input$scatter_x2, "vs", input$scatter_y2, "| Rok:", input$scatter_year),
        x = input$scatter_x2,
        y = input$scatter_y2
      )
  })
  
  # =========================
  # WYKRES 8: HEATMAPA KORELACJI (Spearman)
  # =========================
  output$cor_heatmap <- renderPlot({
    
    df <- long_all(); req(df)
    
    # filtr UE
    d <- df
    if (isTRUE(input$only_eu_cor)) d <- d %>% filter(iso3 %in% EU27_ISO3)
    
    # zakres: jeden rok vs wszystkie lata
    if (input$cor_scope == "year") {
      req(input$cor_year)
      d <- d %>% filter(rok == input$cor_year)
      title_suffix <- paste("| Rok:", input$cor_year)
    } else {
      title_suffix <- "| Wszystkie lata"
    }
    
    # tylko wybrane wskaźniki
    d <- d %>% filter(zmienna %in% SELECTED_VARS)
    
    # pivot do wide (kraj-rok jako wiersz)
    wide <- d %>%
      group_by(geo, iso3, rok, zmienna) %>%
      summarise(value = mean(value, na.rm = TRUE), .groups = "drop") %>%
      pivot_wider(names_from = zmienna, values_from = value)
    
    vars_in_wide <- intersect(SELECTED_VARS, names(wide))
    
    # usunięcie zmiennych, które mają wartości NA
    vars_ok <- keep(vars_in_wide, function(v) sum(!is.na(wide[[v]])) >= 10)
    
    validate(
      need(length(vars_ok) >= 3,
           paste0("Za mało zmiennych z danymi do policzenia korelacji.\n",
                  "Dostępne po filtrach: ", paste(vars_in_wide, collapse = ", "), "\n",
                  "Zostają (>=10 nie-NA): ", paste(vars_ok, collapse = ", "), "\n",
                  "Spróbuj: zakres 'Wszystkie lata' albo inny rok."))
    )
    
    X <- wide %>% select(all_of(vars_ok))
    
    cor_mat <- cor(X, use = "pairwise.complete.obs", method = "spearman")
    cor_long <- as.data.frame(as.table(cor_mat)) %>%
      rename(z1 = Var1, z2 = Var2, cor = Freq)
    
    ggplot(cor_long, aes(z1, z2, fill = cor)) +
      geom_tile() +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      scale_fill_viridis_c(option = "C") +
      labs(
        title = paste("Korelacje (Spearman)", title_suffix),
        x = "", y = "", fill = "ρ"
      )
  })
}

shinyApp(ui, server)
# ------------------------------------------------------------------
