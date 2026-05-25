# Healthcare Access & Equity Explorer 
# Isabella De Rosa
#Final Project Shiny App
### Enter Business Logic after this line
###

## Library Packages
library(shiny)
library(tidyverse)
library(DT)
library(here)
library(stringr)
library(sf)
library(tigris)
library(bslib)
library(leaflet)
library(viridis)

options(tigris_use_cache = TRUE)

## Read in Data

health_df_raw <- read.csv(
  "C:/Users/izabe/OneDrive - american.edu/Desktop/DATA rstudio/data 413/gp01-25f-isabelladerosa/data_raw/data_raw.csv",
  stringsAsFactors = FALSE
)

## ---- 2. Columns retained for analysis ----
keep_cols <- c(
  "fips",
  "st_name",
  "st_name_abbrev",
  "cnty_name",
  "popn_est_23",
  "pers_povty_pct_22",
  "medn_famly_incom_22",
  "noins_mal_lt65_pct_21",
  "noins_fem_lt65_pct_21",
  "md_nf_activ_22",
  "md_nf_prim_care_pc_excl_rsdnt_22",
  "dent_nf_prvprac_22",
  "hosp_22",
  "hosp_beds_22"
)

ahrf_raw <- health_df_raw |>
  select(any_of(keep_cols))

## Cleaning
health_df <- ahrf_raw |>
  mutate(
    # Geographic identifiers
    GEOID = str_pad(as.character(fips), width = 5, side = "left", pad = "0"),
    state = st_name_abbrev,
    county = cnty_name,
    
    # Population and socioeconomic variables
    population_2023 = popn_est_23,
    poverty_pct_2022 = pers_povty_pct_22,
    median_family_income_2022 = medn_famly_incom_22,
    
    # Uninsured proxy (average male/female under 65)
    uninsured_pct_2021 = (noins_mal_lt65_pct_21 + noins_fem_lt65_pct_21) / 2,
    
    # Healthcare workforce and facilities
    md_active_2022 = md_nf_activ_22,
    primary_care_md_rate_2022 = md_nf_prim_care_pc_excl_rsdnt_22,
    dentists_private_2022 = dent_nf_prvprac_22,
    hospitals_2022 = hosp_22,
    hospital_beds_2022 = hosp_beds_22
  ) |>
  mutate(
    # Per-capita healthcare access measures
    md_active_per10k = if_else(population_2023 > 0,
                               md_active_2022 / population_2023 * 10000,
                               NA_real_),
    dentists_per10k = if_else(population_2023 > 0,
                              dentists_private_2022 / population_2023 * 10000,
                              NA_real_),
    beds_per10k = if_else(population_2023 > 0,
                          hospital_beds_2022 / population_2023 * 10000,
                          NA_real_),
    hospitals_per100k = if_else(population_2023 > 0,
                                hospitals_2022 / population_2023 * 100000,
                                NA_real_)
  ) |>
  select(
    GEOID, state, county,
    population_2023,
    poverty_pct_2022,
    uninsured_pct_2021,
    median_family_income_2022,
    md_active_2022, md_active_per10k,
    primary_care_md_rate_2022,
    dentists_private_2022, dentists_per10k,
    hospitals_2022, hospitals_per100k,
    hospital_beds_2022, beds_per10k
  )

## ---- 4. Attach county geometry for mapping ----
counties_sf <- tigris::counties(
  cb = TRUE,
  year = 2023,
  class = "sf"
) |>
  st_transform(4326) |>
  select(GEOID)

health_sf <- counties_sf |>
  left_join(health_df, by = "GEOID")

## ---- 5. Human-readable labels for UI ----
labels1 <- c(
  "Population" = "population_2023",
  "Poverty Rate % " = "poverty_pct_2022",
  "Uninsured % Under 65" = "uninsured_pct_2021",
  "Median Family Income" = "median_family_income_2022",
  "Active Physicians (count)" = "md_active_2022",
  "Active Physicians per 10k " = "md_active_per10k",
  "Primary Care MD Rate" = "primary_care_md_rate_2022",
  "Private Practice Dentists (count, 2022)" = "dentists_private_2022",
  "Dentists per 10k" = "dentists_per10k",
  "Hospitals (count)" = "hospitals_2022",
  "Hospitals per 100k" = "hospitals_per100k",
  "Hospital Beds (count)" = "hospital_beds_2022",
  "Hospital Beds per 10k" = "beds_per10k"
)



## ------------------------------------------
## Create t.test function with tibble output
## ------------------------------------------
one_sample_ttest_tbl <- function(x, mu) {
  tt <- t.test(x, mu = mu)
  tibble(
    null_value = mu,
    estimate = unname(tt$estimate),
    p_value = tt$p.value,
    conf_low = tt$conf.int[1],
    conf_high = tt$conf.int[2]
  )
}

### Enter Business Logic before this line

## -------------------------------
## Begin User Interface Section
## -------------------------------

## ---- UI ----
ui <- fluidPage(
  theme = bslib::bs_theme(bootswatch = "minty"),
  titlePanel("Healthcare Access & Equity Explorer"),
  tabsetPanel(
    # -------------------------
    # INTRODUCTION TAB
    # -------------------------
    tabPanel(
      "Introduction",
      fluidRow(
        column(
          10, offset = 1,
          
          h3("About This App"),
          
          p(
            "This interactive Shiny application explores healthcare access and equity across 
        counties in the United States. The goal of this app is to help users understand how 
        access to healthcare resources varies by location and how these differences relate 
        to population and socioeconomic characteristics."
          ),
          
          p(
            "The app is designed for exploratory data analysis. Users can visualize healthcare 
        access measures, compare relationships between variables, and identify geographic 
        patterns that may indicate inequities in healthcare availability."
          ),
          
          h4("Context"),
          
          p(
            "Research on healthcare access in the United States shows that access to medical care 
        is influenced by a combination of healthcare workforce availability, socioeconomic 
        conditions, and geographic location. While national statistics often suggest that 
        healthcare capacity is sufficient overall, county-level analyses reveal significant 
        disparities that are hidden when data are aggregated at higher levels."
          ),
          
          p(
            "This project focuses on county-level data to better capture local variation in 
        healthcare access. Examining counties allows for a more detailed understanding of 
        where shortages exist and which communities may be most affected."
          ),
          
          p(
            "The Health Resources and Services Administration (HRSA) uses the Health Professional 
        Shortage Area (HPSA) framework to identify areas with limited access to primary care, 
        dental, and mental health services. Research using HPSA designations shows that 
        provider shortages are disproportionately concentrated in rural counties and 
        low-income urban areas. These shortages are associated with delayed care, unmet 
        medical needs, and higher rates of preventable hospitalizations."
          ),
          
          p(
            "Beyond provider availability, socioeconomic factors play a major role in shaping 
        healthcare access. Studies using American Community Survey data show that counties 
        with higher poverty rates and lower median incomes are more likely to experience 
        healthcare shortages and worse health outcomes. Insurance coverage has been 
        identified as a key factor linking socioeconomic status and healthcare utilization. 
        Areas with higher uninsured rates face barriers to both preventive and acute care, 
        even when healthcare providers are physically present."
          ),
          
          h5("Sources"),
          
          p(
            "Kindig, D. A., & Cheng, E. R. (2013). Even as mortality fell in most U.S. counties, 
        female mortality rose in 42.8 percent of counties from 1992 to 2006. 
        Health Affairs, 32(3), 451–458. https://doi.org/10.1377/hlthaff.2011.0892"
          ),
          
          p(
            "Sommers, B. D., Gawande, A. A., & Baicker, K. (2017). Health insurance coverage and 
        health: What the recent evidence tells us. New England Journal of Medicine, 377(6), 
        586–593. https://doi.org/10.1056/NEJMsb1706645"
          ),
          
          p(
            "Health Resources and Services Administration. Health Workforce Shortage Areas. 
        https://data.hrsa.gov/topics/health-workforce/shortage-areas"
          ),
          
          h6("Data Source"),
          
          p("Health Resources and Services Administration (HRSA): https://data.hrsa.gov/data/download"
            )
        )
      )
    ),
    
    
    # -------------------------
    # UNIVARIATE TAB
    # -------------------------
    tabPanel(
      "Univariate Analysis",
      sidebarLayout(
        sidebarPanel(
          selectInput(
            "uni_var",
            "Select a variable:",
            choices = labels1,
            selected = "md_active_per10k"
          ),
          checkboxInput("log_scale", "Use log scale", FALSE),
          sliderInput("bins", "Histogram bins", 10, 100, 40)
        ),
        mainPanel(
          plotOutput("uni_plot"),
          DTOutput("uni_summary")
        )
      )
    ),
    
    # -------------------------
    # BIVARIATE TAB
    # -------------------------
    tabPanel(
      "Bivariate Analysis",
      sidebarLayout(
        sidebarPanel(
          selectInput(
            "x_var",
            "Select X variable:",
            choices = labels1,
            selected = "poverty_pct_2022"
          ),
          selectInput(
            "y_var",
            "Select Y variable:",
            choices = labels1,
            selected = "md_active_per10k"
          ),
          checkboxInput("show_lm", "Show linear trend line", TRUE),
          checkboxInput("log_x", "Log scale X-axis", FALSE),
          checkboxInput("log_y", "Log scale Y-axis", FALSE)
        ),
        mainPanel(
          plotOutput("bi_plot"),
          verbatimTextOutput("bi_note")
        )
      )
    ),

# -------------------------
# Mapping Tab
# -------------------------
tabPanel(
  "Map: Healthcare Access",
  sidebarLayout(
    sidebarPanel(
      selectInput(
        "map_var",
        "Select a variable to map:",
        choices = labels1,
        selected = "md_active_per10k"
      ),
      
      checkboxInput(
        "map_log",
        "Use log scale (numeric variables only)",
        value = FALSE
      ),
      
      checkboxInput(
        "cap_extremes",
        "Cap extreme values (95th percentile)",
        value = TRUE
      ),
      
      helpText(
        "This map shows county-level variation in healthcare access and equity.
         Hover or click on a county to view details."
      )
    ),
    
    mainPanel(
      leafletOutput("health_map", height = 650)
    )
  )
),
tabPanel(
  "Data Table",
  sidebarLayout(
    sidebarPanel(
      checkboxInput(
        "numeric_only",
        "Show numeric variables only",
        value = FALSE
      ),
      helpText(
        "Use the filters at the top of each column to search or subset the data."
      )
    ),
    mainPanel(
      DTOutput("health_table")
    )
  )
)
  )
)
### End User Interface Section
##
## -------------------------------
### Begin Server Section 
## -------------------------------

server <- function(input, output, session) {

 ##------------------------------
    #Univariate Server 
 ##------------------------------
  filtered_data <- reactive({
    v <- input$uni_var
    df <- health_df |> filter(!is.na(.data[[v]]))
    
    validate(
      need(nrow(df) > 0, "Not enough data available for this variable.")
    )
    
    df
  })
  
  output$uni_plot <- renderPlot({
    df <- filtered_data()
    v <- input$uni_var
    
    p <- ggplot(df, aes(x = .data[[v]])) +
      geom_histogram(bins = input$bins, fill = "steelblue", color = "white") +
      theme_minimal() +
      labs(
        x = names(labels1)[labels1 == v],
        y = "Count"
      )
    
    if (input$log_scale) {
      validate(
        need(all(df[[v]] > 0), "Log scale requires all values to be greater than 0.")
      )
      p <- p + scale_x_log10()
    }
    
    p
  })
  
  output$uni_summary <- renderDT({
    df <- filtered_data()
    v <- input$uni_var
    
    datatable(
      summary(df[[v]]) |> as.data.frame(),
      options = list(dom = "t")
    )
  })
  ## ------------------------------
    # Bivariate Server 
  ## ------------------------------
  
  bivariate_data <- reactive({
    x <- input$x_var
    y <- input$y_var
    
    df <- health_df |>
      filter(
        !is.na(.data[[x]]),
        !is.na(.data[[y]])
      )
    
    validate(
      need(x != y, "Please select two different variables."),
      need(nrow(df) > 5, "Not enough data available for this comparison.")
    )
    
    df
  })
  
  output$bi_plot <- renderPlot({
    df <- bivariate_data()
    x <- input$x_var
    y <- input$y_var
    
    x_label <- names(labels1)[match(x, labels1)]
    y_label <- names(labels1)[match(y, labels1)]
    
    p <- ggplot(df, aes(x = .data[[x]], y = .data[[y]])) +
      geom_point(alpha = 0.5, color = "blue") +
      theme_minimal() +
      labs(
        x = x_label,
        y = y_label
      )
    
    if (input$show_lm) {
      p <- p + geom_smooth(method = "lm", se = FALSE, color = "red")
    }
    
    if (input$log_x) {
      validate(
        need(all(df[[x]] > 0), "Log scale X requires all values to be greater than 0.")
      )
      p <- p + scale_x_log10()
    }
    
    if (input$log_y) {
      validate(
        need(all(df[[y]] > 0), "Log scale Y requires all values to be greater than 0.")
      )
      p <- p + scale_y_log10()
    }
    
    p
  })
  
  output$bi_note <- renderText({
    paste(
      "This plot shows the relationship between",
      names(labels1)[match(input$x_var, labels1)],
      "and",
      names(labels1)[match(input$y_var, labels1)],
      "at the county level."
    )
  })
  ## ------------------------------
     # Mapping Server
  ## ------------------------------
  
  map_data <- reactive({
    v <- input$map_var
    req(v)
    
    df <- health_sf |> dplyr::filter(!is.na(.data[[v]]))
    
    validate(
      need(nrow(df) > 0, "No data available for this variable.")
    )
    
    vals <- df[[v]]
    
    if (isTRUE(input$cap_extremes)) {
      upper <- stats::quantile(vals, 0.95, na.rm = TRUE)
      df[[v]] <- pmin(vals, upper)
    }
    
    if (isTRUE(input$map_log)) {
      validate(
        need(all(df[[v]] > 0), "Log scale requires all values to be > 0.")
      )
      df[[v]] <- log10(df[[v]])
    }
    
    df
  })
  
  output$health_map <- renderLeaflet({
    df <- map_data()
    v <- input$map_var
    
    label_name <- names(labels1)[match(v, labels1)]
    
    pal <- leaflet::colorNumeric(
      palette = viridis::viridis(256),
      domain = df[[v]],
      na.color = "white"
    )
    
    leaflet(df) |>
      leaflet::addProviderTiles("CartoDB.Positron") |>
      leaflet::addPolygons(
        fillColor = ~ pal(get(v)), 
        weight = 0.3,
        opacity = 1,
        color = "white",
        fillOpacity = 0.8,
        highlightOptions = leaflet::highlightOptions(
          weight = 2,
          color = "#444444",
          bringToFront = TRUE
        ),
        label = ~ htmltools::HTML(
          paste0(
            "<strong>", county, ", ", state, "</strong><br/>",
            label_name, ": ", round(get(v), 2)   
          )
        )
      ) |>
      leaflet::addLegend(
        pal = pal,
        values = ~ get(v),          
        title = label_name,
        position = "bottomright"
      )
  })
  
  ## ------------------------------
    # Data Table Server
  ## ------------------------------
  
  output$health_table <- renderDT({
    
    df <- health_df
    
    if (isTRUE(input$numeric_only)) {
      df <- df |> dplyr::select(where(is.numeric))
    }
    
    datatable(
      df,
      filter = "top",
      options = list(
        pageLength = 20,
        autoWidth = TRUE,
        scrollX = TRUE
      )
    )
  })  
  
}
### End Server Section ----------------
shinyApp(ui, server)
