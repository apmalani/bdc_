list.of.libraries <- c("shiny","ggplot2","sf","data.table","shinydashboard","leaflet","leaflet.extras2",
                       "geosphere","elevatr","terra","dplyr","pracma","bslib","progress","tidyverse",
                       "R.utils","lubridate","sqldf","stringr","shinyalert", "shinyWidgets", "DT", "ggpmisc")
new.libraries <- list.of.libraries[!(list.of.libraries %in% installed.packages()[,'Package'])]
if(length(new.libraries)>0){
  install.packages(new.libraries,repos="cran.rstudio.com")
}

library(shiny)
library(shinyWidgets)
library(tidyverse)
library(leaflet)
library(sf)
library(tigris)
library(bslib)
library(DT)
library(ggpmisc)

options(shiny.trace = FALSE)
options(shiny.suppressMissingContextError = TRUE)

jsCode <- "
Shiny.addCustomMessageHandler('quitApp', function(message) {
  window.close();
});
"

df <- read_csv("bdc_summary_data.csv")
df <- df %>%
  mutate(long_date = as.Date(paste(
    substring(short_date, 0, 4),
    "-",
    substring(short_date, 5, 6),
    "-30",
    sep = ""
  ))) %>%
  rename (`State Code` = state_code)

states <- states(cb = TRUE)
states <- st_transform(states, "+proj=longlat +datum=WGS84")
counties <- tigris::counties(cb = TRUE, resolution = '500k')
counties <- st_transform(counties, "+proj=longlat +datum=WGS84")

colors <- c("#FFFFCC", "#FFCC66", "#FF5733")

ui <- page_sidebar(
  title = "BDC Dashboard",
  tags$head(tags$script(HTML(jsCode))),
  sidebar = sidebar(
    width = 300,
    conditionalPanel(
      condition = "input.tabs == 'map'",
      dateRangeInput(
        inputId = "dr",
        label = "Select Dates:",
        format = "mm-dd-yyyy",
        start = "2022-02-01",
        startview = "decade"
      ),
      selectInput(
        inputId = "gen",
        label = "Network Generation:",
        choices = c("All", "4G", "5G", "Other")
      ),
      selectInput(
        inputId = "op",
        label = "Wireless Operator:",
        choices = c("All", "AT&T", "T-Mobile", "Verizon", "Other")
      ),
      selectInput(
        inputId = "mode",
        label = "Mode:",
        choices = c("Test Count", "Upload Speed", "Download Speed")
      ),
      prettyCheckbox(
        inputId = "cluster",
        label = "Enable County Clustering",
        value = FALSE
      ),
      downloadButton(
        outputId = "downloadData",
        label = "Download CSV",
        color = "primary"
      ),
      actionButton(
        inputId = "reset",
        label = "Reset Filters",
        color = "primary",
        icon("arrow-rotate-right")
      ),
      hr(),
      actionButton(
        inputId = "quit",
        label = "Quit App",
        style = "color: #FF0000;",
        icon("x"),
      )
    ),
    conditionalPanel(
      condition = "input.tabs == 'graphs'",
      selectInput(
        inputId = "state",
        label = "Select State:",
        choices = sort(states$NAME)
      ),
      selectInput(
        inputId = "county",
        label = "Select Counties:",
        choices = sort(counties[counties$STATE_NAME == "Alabama", ]$NAME),
        multiple = TRUE,
      ),
      dateRangeInput(
        inputId = "dr_gr",
        label = "Select Dates:",
        format = "mm-dd-yyyy",
        start = "2022-02-01",
        startview = "decade"
      ),
      selectInput(
        inputId = "gen_gr",
        label = "Network Generation:",
        choices = c("All", "4G", "5G", "Other")
      ),
      selectInput(
        inputId = "op_gr",
        label = "Wireless Operator:",
        choices = c("All", "AT&T", "T-Mobile", "Verizon", "Other")
      ),
      downloadButton(
        outputId = "downloadData_gr",
        label = "Download CSV",
        color = "primary"
      ),
      actionButton(
        inputId = "reset",
        label = "Reset Filters",
        color = "primary",
        icon("arrow-rotate-right")
      ),
      hr(),
      actionButton(
        inputId = "quit",
        label = "Quit App",
        style = "color: #FF0000;",
        icon("x")
      )
    ),
    position = "right"
  ),
  tags$style(
    HTML(
      "
      #usMap, #stateMap, #countyMap {
        height: calc(100vh - 100px) !important;
        width: 100% !important;
      }
      .leaflet-container {
        height: 100% !important;
        width: 100% !important;
      }
      "
    )
  ),
  tabsetPanel(
    id = "tabs",
    tabPanel(
      "Maps",
      value = "map",
      card(
        max_height = 600,
        card_header(
          class = "bg-primary",
          actionButton("back", "Back", icon("angles-left"), style = "color: #fff; height: 50px; text-align: justify;")
        ),
        card_body(
          conditionalPanel(condition = "output.mapState === 'us'", leafletOutput("usMap")),
          conditionalPanel(condition = "output.mapState === 'state'", leafletOutput("stateMap")),
          conditionalPanel(condition = "output.mapState === 'county'", leafletOutput("countyMap"))
        )
      ),
      card(max_height = 225, card_body(DTOutput("table")))
    ),
    tabPanel(
      "Graphs",
      value = "graphs",
      card(
        max_height = 850,
        card_header(class = "bg-primary", ""),
        card_body(
          plotOutput("download_graph"), plotOutput("upload_graph"))
      )
    )
  )
)


server <- function(input, output, session) {
  clicked_state <- reactiveVal(NULL)
  clicked_county <- reactiveVal(NULL)
  
  mapState <- reactiveVal("us")
  us_count_table <- reactiveVal(NULL)
  county_counts <- reactiveVal(NULL)
  spec_counts <- reactiveVal(NULL)
  
  to_download <- reactiveVal(NULL)
  to_download_gr <- reactiveVal(NULL)
  
  download_graph <- reactiveVal(NULL)
  upload_graph <- reactiveVal(NULL)
  
  to_listen <- reactive({
    list(input$state, input$county, input$dr_gr, input$gen_gr, input$op_gr)
  })
  
  prev_state <- reactiveVal("arun was here")
  
  observe({
    updateSelectInput(session, "gen_gr", selected = input$gen)
    updateSelectInput(session, "op_gr", selected = input$op)
    updateDateRangeInput(session, "dr_gr", start = input$dr[1], end = input$dr[2])
    
    subset_df <- df[df$long_date >= as.Date(input$dr[1]) &
                      df$long_date <= as.Date(input$dr[2]), ]
    
    if (input$gen == "4G") {
      subset_df <- subset_df %>%
        filter(net_gen == "4G")
    } else if (input$gen == "5G") {
      subset_df <- subset_df %>%
        filter(net_gen == "5G")
    } else if (input$gen == "Other") {
      subset_df <- subset_df %>%
        filter(!net_gen %in%  c("5G", "4G"))
    }
    
    if (input$op == "AT&T") {
      subset_df <- subset_df %>%
        filter(str_detect(provider_name, "AT&T"))
    } else if (input$op == "T-Mobile") {
      subset_df <- subset_df %>%
        filter(str_detect(provider_name, "T-Mobile"))
    } else if (input$op == "Verizon") {
      subset_df <- subset_df %>%
        filter(
          str_detect(provider_name, "Verizon") |
            str_detect(provider_name, "verizon")
        )
    } else if (input$op == "Other") {
      subset_df <- subset_df %>%
        filter(!(
          str_detect(provider_name, "AT&T") |
            str_detect(provider_name, "T-Mobile") |
            str_detect(provider_name, "Verizon") |
            str_detect(provider_name, "verizon")
        ))
    }
    
    state_counts <- subset_df %>%
      group_by(`State Code`) %>%
      summarize(
        `Upload Tests` = as.integer(sum(count[test_type == "upload"])),
        `Download Tests` = as.integer(sum(count[test_type == "download"])),
        `Total Tests` = as.integer(sum(count)),
        `Average Upload Speed` = round(mean(AveMbps[test_type == "upload"]), 2),
        `Average Download Speed` = round(mean(AveMbps[test_type == "download"]), 2)
      )
    
    state_counts <- na.omit(state_counts)
    
    us_count_table(state_counts)
    
    us_count_pal <- colorNumeric(palette = colors, domain = state_counts$`Total Tests`)
    us_upload_pal <- colorNumeric(palette = colors,
                                  domain = state_counts$`Average Upload Speed`)
    us_download_pal <- colorNumeric(palette = colors,
                                    domain = state_counts$`Average Download Speed`)
    
    to_download(state_counts)
    
    output$usMap <- renderLeaflet({
      if (mapState() == "us") {
        if (input$mode == "Test Count") {
          leaflet(data = states) %>%
            addGraticule(style = list(color = "grey", weight = 1)) %>%
            addPolygons(
              color = "black",
              fillColor = ~ us_count_pal(state_counts$`Total Tests`[match(STUSPS, state_counts$`State Code`)]),
              fillOpacity = 1,
              weight = 1,
              label = ~ paste0(NAME, ": ", state_counts$`Total Tests`[match(STUSPS, state_counts$`State Code`)]),
              layerId = ~ STUSPS
            ) %>%
            setView(lng = -95.7129,
                    lat = 37.0902,
                    zoom = 4) %>%
            addLegend(
              data = state_counts,
              position = "bottomright",
              pal = us_count_pal,
              values = state_counts$`Total Tests`,
              title = "Count",
              opacity = 1
            )
        } else if (input$mode == "Upload Speed") {
          leaflet(data = states) %>%
            addGraticule(style = list(color = "grey", weight = 1)) %>%
            addPolygons(
              color = "black",
              fillColor = ~ us_upload_pal(state_counts$`Average Upload Speed`[match(STUSPS, state_counts$`State Code`)]),
              fillOpacity = 1,
              weight = 1,
              label = ~ paste0(NAME, ": ", state_counts$`Average Upload Speed`[match(STUSPS, state_counts$`State Code`)]),
              layerId = ~ STUSPS
            ) %>%
            setView(lng = -95.7129,
                    lat = 37.0902,
                    zoom = 4) %>%
            addLegend(
              data = state_counts,
              position = "bottomright",
              pal = us_upload_pal,
              values = state_counts$`Average Upload Speed`,
              title = "Average Mbps",
              opacity = 1
            )
        } else if (input$mode == "Download Speed") {
          leaflet(data = states) %>%
            addGraticule(style = list(color = "grey", weight = 1)) %>%
            addPolygons(
              color = "black",
              fillColor = ~ us_download_pal(state_counts$`Average Download Speed`[match(STUSPS, state_counts$`State Code`)]),
              fillOpacity = 1,
              weight = 1,
              label = ~ paste0(NAME, ": ", state_counts$`Average Download Speed`[match(STUSPS, state_counts$`State Code`)]),
              layerId = ~ STUSPS
            ) %>%
            setView(lng = -95.7129,
                    lat = 37.0902,
                    zoom = 4) %>%
            addLegend(
              data = state_counts,
              position = "bottomright",
              pal = us_download_pal,
              values = state_counts$`Average Download Speed`,
              title = "Average Mbps",
              opacity = 1
            )
        }
      }
    })
    
    if (!is.null(clicked_state())) {
      if (mapState() == "state") {
        state_read <- paste0("state_data/",
                             clicked_state(),
                             "_total_speed_tests.csv")
        state_df <- read_csv(state_read)
        
        state_df <- state_df %>%
          mutate (cut_timestamp = as.Date(substring(timestamp, 1, 10))) %>%
          rename (`County Code` = cnty_fips)
        
        state_sub_df <- state_df[state_df$cut_timestamp >= as.Date(input$dr[1]) &
                                   state_df$cut_timestamp <= as.Date(input$dr[2]), ]
        
        if (input$gen == "4G") {
          state_sub_df <- state_sub_df %>%
            filter(start_net_gen == "4G")
        } else if (input$gen == "5G") {
          state_sub_df <- state_sub_df %>%
            filter(start_net_gen == "5G")
        } else if (input$gen == "Other") {
          state_sub_df <- state_sub_df %>%
            filter(!start_net_gen %in%  c("5G", "4G"))
        }
        
        if (input$op == "AT&T") {
          state_sub_df <- state_sub_df %>%
            filter(str_detect(provider_name, "AT&T"))
        } else if (input$op == "T-Mobile") {
          state_sub_df <- state_sub_df %>%
            filter(str_detect(provider_name, "T-Mobile"))
        } else if (input$op == "Verizon") {
          state_sub_df <- state_sub_df %>%
            filter(
              str_detect(provider_name, "Verizon") |
                str_detect(provider_name, "verizon")
            )
        } else if (input$op == "Other") {
          state_sub_df <- state_sub_df %>%
            filter(!(
              str_detect(provider_name, "AT&T") |
                str_detect(provider_name, "T-Mobile") |
                str_detect(provider_name, "Verizon") |
                str_detect(provider_name, "verizon")
            ))
        }
        
        state_sub_df <- transform(state_sub_df, `County Code` = paste0(as.character(`County Code`)))
        
        county_counts_data <- state_sub_df %>%
          group_by(`County Code`) %>%
          summarize (
            `County Name` = unique(cnty_name),
            `Upload Tests` = as.integer(sum(
              ifelse(test_type == "upload", 1, 0)
            )),
            `Download Tests` = as.integer(sum(
              ifelse(test_type == "download", 1, 0)
            )),
            `Total Tests` = as.integer(n_distinct(app_test_id)),
            `Average Upload Speed` = round(mean(Mbps[test_type == "upload"]), 2),
            `Average Download Speed` = round(mean(Mbps[test_type == "download"]), 2)
          )
        
        county_counts(county_counts_data)
        
        to_download(county_counts_data)
        
        state_count_pal <- colorNumeric(palette = colors, domain = county_counts_data$`Total Tests`)
        state_upload_pal <- colorNumeric(palette = colors,
                                         domain = county_counts_data$`Average Upload Speed`)
        state_download_pal <- colorNumeric(palette = colors,
                                           domain = county_counts_data$`Average Download Speed`)
        
        if (!is.null(clicked_state())) {
          this_state <- counties[counties$STUSPS == clicked_state(), ]
          
          output$stateMap <- renderLeaflet({
            if (mapState() == "state") {
              if (input$mode == "Test Count") {
                leaflet(data = this_state) %>%
                  addGraticule(style = list(color = "grey", weight = 1)) %>%
                  addPolygons(
                    layerId = ~ GEOID,
                    color = "black",
                    fillColor = ~ state_count_pal(county_counts_data$`Total Tests`[match(GEOID, county_counts_data$`County Code`)]),
                    fillOpacity = 1,
                    weight = 1,
                    label = ~ paste0(NAME, ": ", county_counts_data$`Total Tests`[match(GEOID, county_counts_data$`County Code`)])
                  ) %>%
                  addLegend(
                    data = county_counts_data,
                    position = "bottomright",
                    pal = state_count_pal,
                    values = county_counts_data$`Total Tests`,
                    title = "Count",
                    opacity = 1
                  )
              } else if (input$mode == "Upload Speed") {
                leaflet(data = this_state) %>%
                  addGraticule(style = list(color = "grey", weight = 1)) %>%
                  addPolygons(
                    color = "black",
                    fillColor = ~ state_upload_pal(county_counts_data$`Average Upload Speed`[match(GEOID, county_counts_data$`County Code`)]),
                    fillOpacity = 1,
                    weight = 1,
                    label = ~ paste0(
                      NAME,
                      ": ",
                      county_counts_data$`Average Upload Speed`[match(GEOID, county_counts_data$`County Code`)]
                    ),
                    layerId = ~ GEOID
                  ) %>%
                  addLegend(
                    data = county_counts_data,
                    position = "bottomright",
                    pal = state_upload_pal,
                    values = county_counts_data$`Average Upload Speed`,
                    title = "Average Mbps",
                    opacity = 1
                  )
              } else if (input$mode == "Download Speed") {
                leaflet(data = this_state) %>%
                  addGraticule(style = list(color = "grey", weight = 1)) %>%
                  addPolygons(
                    color = "black",
                    fillColor = ~ state_download_pal(county_counts_data$`Average Download Speed`[match(GEOID, county_counts_data$`County Code`)]),
                    fillOpacity = 1,
                    weight = 1,
                    label = ~ paste0(
                      NAME,
                      ": ",
                      county_counts_data$`Average Download Speed`[match(GEOID, county_counts_data$`County Code`)]
                    ),
                    layerId = ~ GEOID
                  ) %>%
                  addLegend(
                    data = county_counts_data,
                    position = "bottomright",
                    pal = state_download_pal,
                    values = county_counts_data$`Average Download Speed`,
                    title = "Average Mbps",
                    opacity = 1
                  )
              }
            }
          })
        }
      }
    }
    
    if (!is.null(clicked_county())) {
      if (mapState() == "county") {
        read_spec <- paste0("state_data/",
                            clicked_state(),
                            "_total_speed_tests.csv")
        
        if (file.exists(read_spec)) {
          spec_data <- read_csv(read_spec)
          
          spec_data <- spec_data[spec_data$cnty_fips == clicked_county(), ]
          
          spec_data <- spec_data %>%
            mutate (cut_timestamp = as.Date(substring(timestamp, 1, 10))) %>%
            rename (`County Code` = cnty_fips,
                    `Provider Name` = provider_name)
          
          spec_sub_df <- spec_data[spec_data$cut_timestamp >= as.Date(input$dr[1]) &
                                     spec_data$cut_timestamp <= as.Date(input$dr[2]), ]
          
          if (input$gen == "4G") {
            spec_sub_df <- spec_sub_df %>%
              filter(start_net_gen == "4G")
          } else if (input$gen == "5G") {
            spec_sub_df <- spec_sub_df %>%
              filter(start_net_gen == "5G")
          } else if (input$gen == "Other") {
            spec_sub_df <- spec_sub_df %>%
              filter(!start_net_gen %in%  c("5G", "4G"))
          }
          
          if (input$op == "AT&T") {
            spec_sub_df <- spec_sub_df %>%
              filter(str_detect(`Provider Name`, "AT&T"))
          } else if (input$op == "T-Mobile") {
            spec_sub_df <- spec_sub_df %>%
              filter(str_detect(`Provider Name`, "T-Mobile"))
          } else if (input$op == "Verizon") {
            spec_sub_df <- spec_sub_df %>%
              filter(
                str_detect(`Provider Name`, "Verizon") |
                  str_detect(`Provider Name`, "verizon")
              )
          } else if (input$op == "Other") {
            spec_sub_df <- spec_sub_df %>%
              filter(!(
                str_detect(`Provider Name`, "AT&T") |
                  str_detect(`Provider Name`, "T-Mobile") |
                  str_detect(`Provider Name`, "Verizon") |
                  str_detect(`Provider Name`, "verizon")
              ))
          }
          
          spec_sub_df <- spec_sub_df %>%
            mutate(
              color = case_when(
                str_detect(`Provider Name`, "AT&T") ~ "#067AB4",
                str_detect(`Provider Name`, "Verizon") |
                  str_detect(`Provider Name`, "verizon") ~ "#D52B1E",
                str_detect(`Provider Name`, "T-Mobile") ~ "#E20074",
                TRUE ~ "orange"
              )
            )
          
          old_data <- spec_sub_df
          
          spec_sub_df <- spec_sub_df[!duplicated(spec_sub_df$app_test_id), ]
          
          display_table_df <- spec_sub_df %>%
            group_by(`Provider Name`) %>%
            summarize (`Count` = n())
          
          spec_counts(display_table_df)
          
          to_download(display_table_df)
          
          d_u <- function(id) {
            download <- old_data[old_data$test_type == "download" &
                                   old_data$app_test_id == id, ]$Mbps
            download <- trunc(download * 10 ^ 2) / 10 ^ 2
            upload <- old_data[old_data$test_type == "upload" &
                                 old_data$app_test_id == id, ]$Mbps
            upload <- trunc(upload * 10 ^ 2) / 10 ^ 2
            return(paste0(
              as.character(download),
              "/",
              as.character(upload),
              " Mbps"
            ))
          }
          
          spec_sub_df <- spec_sub_df %>%
            rowwise() %>%
            mutate (download_upload = d_u(app_test_id)) %>%
            ungroup()
          
          upload_spec <- old_data[old_data$test_type == "upload", ]
          download_spec <- old_data[old_data$test_type == "download", ]
          
          county_upload_pal <- colorNumeric(palette = colors, domain = upload_spec$Mbps)
          county_download_pal <- colorNumeric(palette = colors, domain = download_spec$Mbps)
          
          if (!is.null(clicked_county())) {
            curr_county <- counties[counties$GEOID == clicked_county(), ]
            
            output$countyMap <- renderLeaflet({
              if (mapState() == "county") {
                if (input$mode == "Test Count") {
                  if (input$cluster) {
                    map <- leaflet(data = curr_county,
                                   options = leafletOptions(minZoom = 7)) %>%
                      addGraticule(style = list(color = "grey")) %>%
                      addTiles() %>%
                      addPolygons(color = "black") %>%
                      addCircleMarkers(
                        data = spec_sub_df,
                        stroke = FALSE,
                        fillOpacity = 1,
                        color = spec_sub_df$color,
                        radius = 4,
                        popup = spec_sub_df$download_upload,
                        clusterOptions = markerClusterOptions()
                      )
                  } else {
                    map <- leaflet(data = curr_county,
                                   options = leafletOptions(minZoom = 7)) %>%
                      addGraticule(style = list(color = "grey")) %>%
                      addTiles() %>%
                      addPolygons(color = "black") %>%
                      addCircleMarkers(
                        data = spec_sub_df,
                        stroke = FALSE,
                        fillOpacity = 1,
                        color = spec_sub_df$color,
                        radius = 4,
                        popup = spec_sub_df$download_upload
                      )
                  }
                  
                  map %>% addLegend(
                    position = "bottomright",
                    title = "Provider",
                    colors = c("#E20074", "#067AB4", "#D52B1E", "orange"),
                    labels = c("T-Mobile", "AT&T", "Verizon", "Other"),
                    opacity = 1
                  )
                } else if (input$mode == "Upload Speed") {
                  if (input$cluster) {
                    map <- leaflet(data = curr_county,
                                   options = leafletOptions(minZoom = 7)) %>%
                      addGraticule(style = list(
                        color = "grey",
                        weight = 1
                      )) %>%
                      addTiles() %>%
                      addPolygons(color = "black") %>%
                      addCircleMarkers (
                        data = upload_spec,
                        stroke = FALSE,
                        fillOpacity = 1,
                        radius = 4,
                        fillColor = ~ county_upload_pal(upload_spec$Mbps),
                        popup = paste0(round(upload_spec$Mbps, 2), " Mbps"),
                        clusterOptions = markerClusterOptions()
                      )
                  } else {
                    map <- leaflet(data = curr_county,
                                   options = leafletOptions(minZoom = 7)) %>%
                      addGraticule(style = list(
                        color = "grey",
                        weight = 1
                      )) %>%
                      addTiles() %>%
                      addPolygons(color = "black") %>%
                      addCircleMarkers (
                        data = upload_spec,
                        stroke = FALSE,
                        fillOpacity = 1,
                        radius = 4,
                        fillColor = ~ county_upload_pal(upload_spec$Mbps),
                        popup = paste0(round(upload_spec$Mbps, 2), " Mbps")
                      )
                  }
                  map <- map %>% addLegend (
                    data = upload_spec,
                    position = "bottomright",
                    title = "Mbps",
                    opacity = 1,
                    pal = county_upload_pal,
                    values = upload_spec$Mbps,
                  )
                } else if (input$mode == "Download Speed") {
                  if (input$cluster) {
                    map <- leaflet(data = curr_county,
                                   options = leafletOptions(minZoom = 7)) %>%
                      addGraticule(style = list(
                        color = "grey",
                        weight = 1
                      )) %>%
                      addTiles() %>%
                      addPolygons(color = "black") %>%
                      addCircleMarkers (
                        data = download_spec,
                        stroke = FALSE,
                        fillOpacity = 1,
                        radius = 4,
                        fillColor = ~ county_download_pal(download_spec$Mbps),
                        popup = paste0(round(
                          download_spec$Mbps, 2
                        ), " Mbps"),
                        clusterOptions = markerClusterOptions()
                      )
                  } else {
                    map <- leaflet(data = curr_county,
                                   options = leafletOptions(minZoom = 7)) %>%
                      addGraticule(style = list(
                        color = "grey",
                        weight = 1
                      )) %>%
                      addTiles() %>%
                      addPolygons(color = "black") %>%
                      addCircleMarkers (
                        data = download_spec,
                        stroke = FALSE,
                        fillOpacity = 1,
                        radius = 4,
                        fillColor = ~ county_download_pal(download_spec$Mbps),
                        popup = paste0(round(
                          download_spec$Mbps, 2
                        ), " Mbps")
                      )
                  }
                  
                  map <- map %>% addLegend (
                    data = download_spec,
                    position = "bottomright",
                    title = "Mbps",
                    opacity = 1,
                    pal = county_download_pal,
                    values = download_spec$Mbps,
                  )
                }
              }
            })
          }
        } else {
          mapState("state")
        }
      }
    }
    
  })
  
  observeEvent(input$usMap_shape_click, {
    clicked_state(input$usMap_shape_click$id)
    mapState("state")
    updateSelectInput(session, "state", selected = states[states$STUSPS == input$usMap_shape_click$id, ]$NAME)
  })
  
  observeEvent(input$stateMap_shape_click, {
    clicked_county(input$stateMap_shape_click$id)
    mapState("county")
    updateSelectInput(session, "state", selected = states[states$STUSPS == input$usMap_shape_click$id, ]$NAME)
    updateSelectInput(session, "county", selected = c(counties[counties$GEOID == input$stateMap_shape_click$id, ]$NAME))
  })
  
  observeEvent(input$back, {
    current <- mapState()
    if (current == "county") {
      mapState("state")
      updateSelectInput(session, "state", selected = "Alabama")
      updateSelectInput(session, "county", selected = character(0))
      updateSelectInput(session, "state", selected = states[states$STUSPS == input$usMap_shape_click$id, ]$NAME)
    } else if (current == "state") {
      mapState("us")
      updateSelectInput(session, "county", selected = character(0))
      updateSelectInput(session, "state", selected = "Alabama")
    }
  })
  
  observeEvent(input$quit, {
    stopApp()
    session$sendCustomMessage(type = 'quitApp', message = list())
  })
  
  observeEvent(input$reset, {
    updateSelectInput(session, "gen", selected = "All")
    updateSelectInput(session, "op", selected = "All")
    updateSelectInput(session, "mode", selected = "Test Count")
    updateDateRangeInput(session, "dr", start = "2022-02-01", end = Sys.Date())
    updateSelectInput(session, "state", selected = "Alabama")
    updateSelectInput(session, "county", selected = character(0))
    
    updateSelectInput(session, "gen_gr", selected = "All")
    updateSelectInput(session, "op_gr", selected = "All")
    updateDateRangeInput(session, "dr_gr", start = "2022-02-01", end = Sys.Date())
  })
  
  
  observeEvent(to_listen(), {
    updateSelectInput(session, "gen", selected = input$gen_gr)
    updateSelectInput(session, "op", selected = input$op_gr)
    updateDateRangeInput(session, "dr", start = input$dr_gr[1], end = input$dr_gr[2])
    
    if (input$state != prev_state()) {
      updateSelectInput(session, "county", choices = sort(counties[counties$STATE_NAME == input$state, ]$NAME))
    }
    
    selected_state <- states[states$NAME == input$state, ]$STUSPS
    state_read <- paste0("state_data/",
                         selected_state,
                         "_total_speed_tests.csv")
    state_df <- read_csv(state_read)
    
    state_df <- state_df %>% 
      rename(
        County = cnty_name
      )
    
    if (!is.null(input$county)) {
      state_df <- state_df %>% 
        filter(County %in% input$county)
    }
    
    state_df <- state_df[state_df$test_date >= as.Date(input$dr_gr[1]) &
                           state_df$test_date <= as.Date(input$dr_gr[2]), ]
    
    if (input$gen_gr == "4G") {
      state_df <- state_df %>%
        filter(start_net_gen == "4G")
    } else if (input$gen_gr == "5G") {
      state_df <- state_df %>%
        filter(start_net_gen == "5G")
    } else if (input$gen_gr == "Other") {
      state_df <- state_df %>%
        filter(!start_net_gen %in%  c("5G", "4G"))
    }
    
    if (input$op_gr == "AT&T") {
      state_df <- state_df %>%
        filter(str_detect(provider_name, "AT&T"))
    } else if (input$op_gr == "T-Mobile") {
      state_df <- state_df %>%
        filter(str_detect(provider_name, "T-Mobile"))
    } else if (input$op_gr == "Verizon") {
      state_df <- state_df %>%
        filter(
          str_detect(provider_name, "Verizon") |
            str_detect(provider_name, "verizon")
        )
    } else if (input$op_gr == "Other") {
      state_df <- state_df %>%
        filter(!(
          str_detect(provider_name, "AT&T") |
            str_detect(provider_name, "T-Mobile") |
            str_detect(provider_name, "Verizon") |
            str_detect(provider_name, "verizon")
        ))
    }
    
    state_download_df <- state_df[state_df$test_type == "download", ]
    state_upload_df <- state_df[state_df$test_type == "upload", ]
    
    if (is.null(input$county)) {
      state_download_df <- state_download_df %>% 
        mutate(week = floor_date(test_date, "week")) %>% 
        group_by(week) %>% 
        summarize(mean(Mbps))
      
      state_upload_df <- state_upload_df %>% 
        mutate(week = floor_date(test_date, "week")) %>% 
        group_by(week) %>% 
        summarize(mean(Mbps))
    } else {
      state_download_df <- state_download_df %>% 
        mutate(week = floor_date(test_date, "week")) %>% 
        group_by(County, week) %>% 
        summarize(mean(Mbps))
      
      state_upload_df <- state_upload_df %>% 
        mutate(week = floor_date(test_date, "week")) %>% 
        group_by(County, week) %>% 
        summarize(mean(Mbps))
    }
    
    download_graph(state_download_df)
    upload_graph(state_upload_df)
    
    combined_df <- merge(state_download_df, state_upload_df, "week")
    combined_df <- combined_df %>% 
      rename(
        `Download Speed` = `mean(Mbps).x`,
        `Upload Speed` = `mean(Mbps).y`,
      )
    
    to_download_gr(combined_df)
    
    prev_state(input$state)
  })
  
  table_data <- reactive({
    if (mapState() == "us") {
      return(us_count_table())
    } else if (mapState() == "state") {
      return(county_counts())
    } else if (mapState() == "county") {
      return(spec_counts())
    }
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste0('bdc-dash-data-', Sys.Date(), '.csv')
    },
    content = function(con) {
      write.csv(to_download(), con)
    }
  )
  
  output$downloadData_gr <- downloadHandler(
    filename = function() {
      paste0('bdc-dash-data-', Sys.Date(), '.csv')
    },
    content = function(con) {
      write.csv(to_download_gr(), con)
    }
  )
  
  output$table <- renderDT({
    datatable(
      table_data(),
      options = list(
        scrollCollapse = TRUE,
        paging = FALSE, 
        searching = FALSE,
        fixedHeader = TRUE,
        scrollY = "150px",
        info = FALSE
      ),
      style = "bootstrap"
    )
  })
  
  output$mapState <- reactive(mapState())
  outputOptions(output, "mapState", suspendWhenHidden = FALSE)
  
  output$download_graph <- renderPlot({
    if (is.null(input$county)) {
      ggplot(data = download_graph(), aes(x = week, y = `mean(Mbps)`)) +
        geom_line() +
        geom_point() +
        stat_poly_line(se = FALSE, linetype = "dashed") + stat_poly_eq() +
        ggtitle("Average Download Speeds (Mbps)") + 
        labs(x = "Date (Plotted Per Week)", y = "Average Mbps")
    } else {
      ggplot(data = download_graph(), aes(x = week, y = `mean(Mbps)`, color = County, group = County)) +
        geom_line() +
        geom_point() +
        ggtitle("Average Download Speeds (Mbps)") + 
        labs(x = "Date (Plotted Per Week)", y = "Average Mbps")
    }
  })
  
  output$upload_graph <- renderPlot({
    if (is.null(input$county)) {
      ggplot(data = upload_graph(), aes(x = week, y = `mean(Mbps)`)) +
        geom_line() +
        geom_point() + 
        stat_poly_line(se = FALSE, linetype = "dashed") + stat_poly_eq() +
        ggtitle("Average Upload Speeds (Mbps)") +
        labs(x = "Date (Plotted Per Week)", y = "Average Mbps")
    } else {
      ggplot(data = upload_graph(), aes(x = week, y = `mean(Mbps)`, color = County, group = County)) +
        geom_line() +
        geom_point() +
        ggtitle("Average Upload Speeds (Mbps)") + 
        labs(x = "Date (Plotted Per Week)", y = "Average Mbps")
    }
  })
}

shinyApp(ui, server)
