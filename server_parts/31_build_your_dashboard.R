# Build your Dashboard

# --- Drilldown State Management ---
global_drill_state <- reactiveVal(list(
  level = "Region", 
  region = NULL,    
  division = NULL   
))
global_trigger <- reactiveVal(0) 

# --- Observer Lifecycle Manager ---
drilldown_observers <- reactiveVal(list())

# --- NEW: Selection Order Tracker ---
# This reactiveVal tracks the *click order* of metrics,
# because input$selected_metrics (from a standard selectInput) does not.
selection_order <- reactiveVal(character(0))

observeEvent(input$selected_metrics, {
  current_selection <- input$selected_metrics
  # Use isolate() to compare the new value to the old value
  old_selection <- isolate(selection_order())
  
  # 1. Find items that were *added*
  # (items in the new list that weren't in the old one)
  newly_added <- setdiff(current_selection, old_selection)
  
  # 2. Find items that were *kept*
  # We filter the *old* list to preserve its order
  kept_items <- old_selection[old_selection %in% current_selection]
  
  # 3. The new order is the kept items, plus the new items at the end
  new_order <- c(kept_items, newly_added)
  
  # 4. Save the new, correctly-ordered list
  selection_order(new_order)
  
}, ignoreNULL = FALSE) # ignoreNULL=FALSE ensures it runs even when cleared

# --- Back Button Logic (Unchanged) ---
output$back_button_ui <- renderUI({
  state <- global_drill_state() 
  if (state$level != "Region") { 
    actionButton("back_button", "Go Back", icon = icon("arrow-left"))
  }
})

observeEvent(input$back_button, {
  # (Unchanged)
  state <- isolate(global_drill_state()) 
  new_state <- list()
  if (state$level == "Legislative.District") {
    new_state <- list(level = "Division", region = state$region, division = NULL)
  } else if (state$level == "Division") {
    new_state <- list(level = "Region", region = NULL, division = NULL)
  }
  global_drill_state(new_state)
  global_trigger(global_trigger() + 1) 
})


# --- DYNAMIC OBSERVER MANAGER ---
observe({
  # --- CHANGED: Use our new ordered list ---
  selected_metrics <- selection_order() 
  
  # (Rest of this observer is unchanged)
  old_handles <- isolate(drilldown_observers())
  walk(old_handles, ~ .x$destroy()) 
  
  new_handles <- map(selected_metrics, ~{
    current_metric <- .x
    current_metric_source <- paste0("plot_source_", current_metric)
    
    observeEvent(event_data("plotly_click", source = current_metric_source), {
      state <- isolate(global_drill_state()) 
      if (state$level == "Legislative.District") return()
      d <- event_data("plotly_click", source = current_metric_source)
      if (is.null(d$y)) return()
      clicked_category <- d$y 
      new_state <- list()
      if (state$level == "Region") {
        new_state <- list(level = "Division", region = clicked_category, division = NULL)
      } else if (state$level == "Division") {
        new_state <- list(level = "Legislative.District", region = state$region, division = clicked_category)
      }
      global_drill_state(new_state)
      global_trigger(global_trigger() + 1)
    }, ignoreNULL = TRUE, ignoreInit = TRUE)
  })
  
  drilldown_observers(new_handles)
})


# --- Reactive Data (Unchanged) ---
# (The reactive data functions do not need to be changed)
filtered_data <- reactive({
  trigger <- global_trigger() 
  state <- global_drill_state()
  temp_data <- uni
  if (state$level == "Division") {
    req(state$region)
    temp_data <- temp_data %>% filter(Region == state$region)
  } else if (state$level == "Legislative.District") {
    req(state$region, state$division)
    temp_data <- temp_data %>% 
      filter(Region == state$region, Division == state$division)
  }
  temp_data
})

summarized_data_long <- reactive({
  # This reactive doesn't care about UI order, so it can
  # still use input$selected_metrics directly.
  req(input$selected_metrics) 
  state <- global_drill_state() 
  group_by_col <- state$level  
  
  metrics_to_process <- input$selected_metrics 
  data_in <- filtered_data()
  
  existing_metrics <- intersect(metrics_to_process, names(data_in))
  if (length(existing_metrics) == 0) {
    return(tibble(Category = character(), Metric = character(), Value = numeric()))
  }
  valid_metrics <- existing_metrics[sapply(data_in[existing_metrics], is.numeric)]
  
  if (length(valid_metrics) == 0) {
    return(tibble(Category = character(), Metric = character(), Value = numeric()))
  }
  
  data_in %>%
    select(!!sym(group_by_col), all_of(valid_metrics)) %>%
    pivot_longer(
      cols = all_of(valid_metrics),
      names_to = "Metric",
      values_to = "Value"
    ) %>%
    group_by(!!sym(group_by_col), Metric) %>%
    summarise(Value = sum(Value, na.rm = TRUE), .groups = "drop") %>%
    rename(Category = !!sym(group_by_col))
})


# --- Dynamic UI: Combined Dashboard Grid ---
output$dashboard_grid <- renderUI({
  
  # --- CHANGED: Use our new ordered list ---
  selected_metrics <- selection_order() 
  
  if (length(selected_metrics) == 0) {
    return(tags$h4("Please select at least one metric to display graphs."))
  }
  
  # --- 1. Create Plotly Renders ---
  # (Unchanged)
  walk(selected_metrics, ~{
    current_metric <- .x
    current_metric_name <- names(metric_choices)[metric_choices == current_metric]
    
    output[[paste0("plot_", current_metric)]] <- renderPlotly({
      tryCatch({
        # (All tryCatch and plot logic is unchanged)
        trigger <- global_trigger()
        state <- global_drill_state()
        plot_data <- summarized_data_long() %>%
          filter(Metric == current_metric) %>%
          filter(!is.na(Category))
        
        level_name <- stringr::str_to_title(state$level) 
        plot_title <- "" 
        
        if (state$level == "Region") {
          plot_title <- paste(current_metric_name, "by", level_name)
        } else if (state$level == "Division") {
          plot_title <- paste(current_metric_name, "by", level_name, "in", state$region)
        } else if (state$level == "Legislative.District") {
          plot_title <- paste(current_metric_name, "by", level_name, "in", state$division)
        }
        
        if (nrow(plot_data) == 0 || all(is.na(plot_data$Value))) {
          return(
            plot_ly() %>% 
              layout(
                title = list(text = plot_title, x = 0.05),
                annotations = list(x = 0.5, y = 0.5, text = "No data available for this view", showarrow = FALSE, font = list(size = 14))
              )
          )
        }
        
        max_val <- max(plot_data$Value, na.rm = TRUE)
        xaxis_range <- c(0, max_val * 1.3)
        
        p <- plot_ly(
          data = plot_data, y = ~Category, x = ~Value, type = "bar",
          orientation = 'h', name = current_metric_name,
          source = paste0("plot_source_", current_metric), 
          texttemplate = '%{x:,.0f}', textposition = "outside",
          cliponaxis = FALSE, textfont = list(color = '#000000', size = 10)
        ) %>%
          layout(
            title = list(text = plot_title, x = 0.05),
            yaxis = list(title = "", categoryorder = "total descending", autorange = "reversed"),
            xaxis = list(title = "Total Value", tickformat = ',.0f', range = xaxis_range),
            legend = list(orientation = 'h', xanchor = 'center', x = 0.5, yanchor = 'bottom', y = 1.02),
            margin = list(l = 150)
          )
        
        p
        
      }, error = function(e) {
        cat("Error in renderPlotly for metric:", current_metric, "\n")
        print(e)
        return(
          plot_ly() %>% 
            layout(
              title = list(text = paste("Error loading", current_metric_name), x = 0.05),
              annotations = list(x = 0.5, y = 0.5, text = "An error occurred while rendering this chart.", showarrow = FALSE, font = list(size = 14, color = "red"))
            )
        )
      })
    })
  })
  
  # --- 2. Create the UI Card Elements ---
  # (This loop now uses the correctly ordered selected_metrics)
  plot_cards <- map(selected_metrics, ~{
    current_metric <- .x
    current_metric_name <- names(metric_choices)[metric_choices == current_metric]
    
    total_val <- tryCatch({
      summarized_data_long() %>%
        filter(Metric == current_metric) %>%
        pull(Value) %>%
        sum(na.rm = TRUE)
    }, error = function(e) { 0 }) 
    
    bslib::card(
      full_screen = TRUE,
      card_header(current_metric_name),
      card_body(
        tags$div(
          style = "text-align: center; padding-bottom: 10px;",
          card(
            tags$h5(paste("Total", current_metric_name), style = "font-weight: 600; color: #555; margin-top: 5px; margin-bottom: 5px;"),
            tags$h2(scales::comma(total_val), style = "font-weight: 700; color: #000; margin-top: 0; margin-bottom: 10px;")
          )
        ),
        plotlyOutput(paste0("plot_", .x))
      )
    )
  })
  
  # --- 3. Arrange the cards into the layout ---
  # (Unchanged - this will now arrange the cards in the correct order)
  do.call(
    bslib::layout_columns,
    c(list(col_widths = 4), plot_cards)
  )
  
})