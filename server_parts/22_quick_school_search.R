# Quick schools search

output$TextMapping <- renderLeaflet({
  leaflet() %>%
    setView(lng = 122, lat = 13, zoom = 5) %>%
    addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%
    addProviderTiles(providers$CartoDB.Positron, group = "Road Map") %>%
    addMeasure(position = "topright",
               primaryLengthUnit = "kilometers",
               primaryAreaUnit = "sqmeters") %>%
    addLayersControl(baseGroups = c("Satellite", "Road Map"))
})

# --- Reactive controls for input and button ---
observe({
  txt <- trimws(input$text)
  
  # Disable button if text is empty
  shinyjs::toggleState("TextRun", condition = txt != "")
  
  # Show or hide warning message
  output$text_warning <- renderText({
    if (txt == "") {
      "⚠ Please enter a school name before showing results."
    } else {
      ""
    }
  })
})


# --- Observe button click ---
observeEvent(input$TextRun, {
  Text <- trimws(input$text)
  
  # Extra safety check (should not trigger because button is disabled when blank)
  if (Text == "") return()
  
  # --- Filter data based on input ---
  mainreact1 <- uni %>%
    arrange(Region, Division) %>%
    filter(grepl(Text, as.character(School.Name), ignore.case = TRUE))
  
  # --- Handle no matching results ---
  if (nrow(mainreact1) == 0) {
    output$text_warning <- renderText(paste0("⚠ No results found for '", Text, "'."))
    leafletProxy("TextMapping") %>%
      clearMarkers() %>%
      clearMarkerClusters()
    output$TextTable <- DT::renderDT(NULL)
    return()
  } else {
    output$text_warning <- renderText("")  # clear any old warning
  }
  
  # --- Create leaflet labels ---
  values.comp <- paste(
    strong("SCHOOL INFORMATION"),
    "<br>School Name:", mainreact1$School.Name,
    "<br>School ID:", mainreact1$SchoolID
  ) %>% lapply(htmltools::HTML)
  
  # --- Update leaflet map ---
  leafletProxy("TextMapping") %>%
    clearMarkers() %>%
    clearMarkerClusters() %>%
    setView(lng = mainreact1$Longitude[1],
            lat = mainreact1$Latitude[1],
            zoom = 4.5) %>%
    addAwesomeMarkers(
      lng = mainreact1$Longitude,
      lat = mainreact1$Latitude,
      icon = makeAwesomeIcon(
        icon = "education",
        library = "glyphicon",
        markerColor = "blue"
      ),
      label = values.comp,
      labelOptions = labelOptions(
        noHide = FALSE,
        textsize = "12px",
        direction = "top",
        fill = TRUE,
        style = list("border-color" = "rgba(0,0,0,0.5)")
      )
    )
  
  df1 <- reactive({
    
    if (is.null(input$TextMapping_bounds)) {
      mainreact1
    } else {
      bounds <- input$TextMapping_bounds
      latRng <- range(bounds$north, bounds$south)
      lngRng <- range(bounds$east, bounds$west)
      
      subset(mainreact1,
             Latitude >= latRng[1] & Latitude <= latRng[2] & Longitude >= lngRng[1] & Longitude <= lngRng[2])
    }
  })
  
  # --- Render DataTable ---
  output$TextTable <- DT::renderDT(server = TRUE, {
    datatable(
      df1() %>%
        select("Region", "Division", "Legislative.District", "Municipality", "School.Name") %>%
        rename("School" = "School.Name"),
      extension = 'Buttons',
      rownames = FALSE,
      options = list(
        scrollX = TRUE,
        pageLength = 10,
        columnDefs = list(list(className = 'dt-center', targets = "_all")),
        dom = 'lrtip'
      ),
      filter = "top"
    )
  })
})

observeEvent(input$TextRun, {
  
  Text <- input$text
  
  mainreact1 <- uni %>% arrange(Region) %>% arrange(Division) %>% filter(grepl(Text, as.character(School.Name), ignore.case = TRUE))  #arrange first by Division before filtering & make sure this is the same in row_selected
  
  values.comp <- paste(strong("SCHOOL INFORMATION"),"<br>School Name",mainreact1$School.Name,"<br>School ID:",mainreact1$SchoolID) %>% lapply(htmltools::HTML)
  
  values.df <- paste(mainreact1$School.Name %>% lapply(htmltools::HTML))
  
  leafletProxy("TextMapping") %>% clearMarkers() %>% clearMarkerClusters() %>% setView(lng = mainreact1$Longitude[1], lat = mainreact1$Latitude[1], zoom = 4.5) %>% 
    addAwesomeMarkers(lng = mainreact1$Longitude, lat = mainreact1$Latitude,  icon = makeAwesomeIcon(icon = "education",library = "glyphicon",markerColor = "blue"), label = values.comp, labelOptions = labelOptions(noHide = F, textsize = "12px", direction = "top", fill = TRUE, style = list("border-color" = "rgba(0,0,0,0.5)")))
  
  df1 <- reactive({
    
    if (is.null(input$TextMapping_bounds)) {
      mainreact1
    } else {
      bounds <- input$TextMapping_bounds
      latRng <- range(bounds$north, bounds$south)
      lngRng <- range(bounds$east, bounds$west)
      
      subset(mainreact1,
             Latitude >= latRng[1] & Latitude <= latRng[2] & Longitude >= lngRng[1] & Longitude <= lngRng[2])
    }
  })
})