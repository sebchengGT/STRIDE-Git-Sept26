# TEST COMMIT
# TEST COMMIT 23

library(tidyverse)
library(DT)
library(dplyr)
library(shiny)
library(shinydashboard)
library(bslib)
library(bsicons)
library(leaflet)
library(htmltools)
library(thematic)
library(arrow)
library(forcats)
library(fontawesome)
library(shinyjs)
library(shinyauthr)
library(httr)
library(scales)
library(tidytext)
library(ggplot2)
library(plotly)
library(readr)
library(geojsonio)
library(shinyWidgets) # For pickerInput

df <- read_parquet("School-Level-v2.parquet")
uni <- read_parquet("School-Unique-v2.parquet")
IndALL <- read_parquet("IndDistance.ALL2.parquet")
ind <- read_parquet("SHS-Industry.parquet")
SDO <- read_parquet("SDOFill.parquet")
dfz <- read.csv("COS Regional Breakdown2.csv") %>% select(1:5)
Excess <- read_parquet("NationwideExcess.parquet")
SHS_Pilot <- read.csv("PilotSchools.csv")
SHS_Pilot2 <- as.list(SHS_Pilot$SchoolID)
EFDProj <- read_parquet("EFDProj.parquet")
ElecProposal <- read.csv("ElecProp.csv") %>% mutate(Estimated.Cost = as.numeric(Estimated.Cost))
DBMProp <- read.csv("DBM-Proposal.csv")
SBMaster <- read.csv("School_Building_MasterPlan.csv")
EFDDB <- read.csv("EFD-DataBuilder-2025.csv")
EFDMP <- read_parquet("EFD-Masterlist.parquet")
EFD_Projects <- read.csv("EFD-ProgramsList-Aug2025.csv") %>% mutate(Allocation = as.numeric(Allocation)) %>% mutate(Completion = as.numeric(Completion)) %>% filter(FundingYear >= 2020)
cloud <- read_parquet("Cloud_Consolidated.parquet")
cloud_v2 <- read_parquet("Cloud_Consolidated_v2.parquet")
cloud_v3 <- read_parquet("Cloud_Consolidated_v3.parquet")
geojson_data <- geojson_read("gadm41_PHL_1.json", what = "sp")
geojson_table <- as.data.frame(geojson_data)
regprov <- read.csv("RegProv.Congestion.csv")
geojson_table <- left_join(geojson_table,regprov, by="NAME_1")
LMS <- read_parquet("EFD-LMS-GIDCA-NSBI2023.parquet") %>% mutate(Region = case_when(Region == "Region IV-B" ~ "MIMAROPA", TRUE ~ Region)) %>% mutate(With_Shortage = case_when(Estimated_CL_Shortage > 0 ~ 1, TRUE ~ 0))

user_base <- tibble::tibble(
  user = c("iamdeped", "depedadmin"),
  password = c("deped123", "admin123"), # In a real app, use hashed passwords
  password_hash = sapply(c("deped123", "admin123"), sodium::password_store), # Hashed passwords
  permissions = c("admin", "standard"),
  name = c("User One", "User Two")
)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  theme = bs_theme(version = 5,
                   bootswatch = "litera",
                   font_scale = 0.9,
                   base_font = font_google("Poppins")),
  # Use shinyjs to easily show/hide elements
  shinyjs::useShinyjs(),
  
  # tags$head(
  #   tags$style(HTML("
  #     body {
  #       background-image: url('my.background.png'); #/* Path to your image in www folder */
  #       background-size: cover; #/* Cover the entire page */
  #       background-repeat: no-repeat; #/* Do not repeat the image */
  #       background-attachment: fixed; #/* Optional: Fix the background image during scroll */
  #       background-position: center center; #/* Center the image */
  #       height: 100vh; #/* Ensure body takes full viewport height */
  #       margin: 0; #/* Remove default body margin */
  #     }
  #                   "))),
  # 
  # Row for the login panel UI
  fluidRow(
    layout_columns(
      imageOutput("StrideLogo"),
      col_widths = c(-3,6,-3))),
  
  fluidRow(
    column(width = 12,
           # Add the login UI
           shinyauthr::loginUI(
             id = "login",
             title = "Please Log In",
             user_title = "Username",
             pass_title = "Password",
             login_title = "Log In",
             error_message = "Invalid username or password!" # Custom error message
           )
    )
  ),
  
  fluidRow(
    column(width = 12,
           div(
             style = "text-align: right; padding-bottom: 10px;",
             shinyauthr::logoutUI(
               id = "logout",
               label = "Log Out",
               icon = icon("sign-out-alt"),
               class = "btn btn-danger"
             )))), 
  # Custom styling
  
  shinyjs::hidden(
    div(
      id = "main_content",
      uiOutput("STRIDE1"))),
  
  shinyjs::hidden(
    div(
      id = "mgmt_content",
      uiOutput("STRIDE2"))))


# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  output$StrideLogo <- renderImage({
    image_path <- normalizePath(file.path('www', 'STRIDE logo.png'))
    list(
      src = image_path,  # Path relative to the www directory
      contentType = "image/png",
      alt = "STRIDE logo",
      width = "100%",
      height = "auto"
      # You can also set width and height here, e.g., width = 400,
      # or control them in the imageOutput in the UI.
    )
  }, deleteFile = FALSE) # deleteFile = FALSE is important for pre-existing static files
  
  
  # Call the shinyauthr::logoutServer module
  logout_init <- shinyauthr::logoutServer(
    id = "logout",
    active = reactive(credentials()$user_auth) # Logout button active only when logged in
  )
  
  # --- Authentication ---
  # Call the shinyauthr::loginServer module
  # credentials() will be a reactive returning a tibble with user_auth, info, and additional columns from user_base
  credentials <- shinyauthr::loginServer(
    id = "login",
    data = user_base,
    user_col = user,
    pwd_col = password_hash, # Use the hashed password column
    sodium_hashed = TRUE,    # Important: tell shinyauthr we are using sodium hashes
    log_out = reactive(logout_init()) # Link to the logout button
  )
  
  
  
  # --- Reactive Values & Observers ---
  # Observe the authentication status
  observe({
    auth_status <- credentials()$user_auth
    if (auth_status) {
      # User is authenticated. Let's get their details.
      user_info <- credentials()$info # This is a tibble with the user's row
      
      # Ensure user_info is available and has the username
      # (It should if auth_status is TRUE and your user_base is set up correctly)
      if (!is.null(user_info) && "user" %in% names(user_info)) {
        current_username <- user_info$user # Get the username
        
        # --- Always hide the login panel when authenticated ---
        shinyjs::hide(selector = "#login") # Or shinyjs::hide(id = "login-login_ui")
        shinyjs::hide("StrideLogo")
        # --- Conditional logic based on username ---
        if (current_username == "iamdeped") { # <<<< Your specific username condition
          # Authenticated AND username is "user1"
          shinyjs::show("main_content")
          shinyjs::hide("mgmt_content")
        } else {
          
          if (current_username == "depedadmin") {
            # Authenticated BUT username is NOT "user1"
            # This could be user2, user3, etc.
            shinyjs::show("mgmt_content")
            shinyjs::hide("main_content")
            # output$generic_secure_data <- renderPrint({"Generic secure data for other users..."})
          }}}
    } else {
      # User is NOT authenticated (e.g., after logout or initially)
      shinyjs::show(selector = "#login")
      shinyjs::show("StrideLogo")
      shinyjs::hide("main_content")
      shinyjs::hide("mgmt_content")
    }})

  output$STRIDE2 <- renderUI({
    page_navbar(
  title = div(
    tags$span(
      strong("DepEd STRIDE Dashboard"),
      style = "font-size: 1em; margin-bottom: 0.2em;"),"",
    tags$p(
      em("based on GMIS (April 2025) and eBEIS (SY 2024-2025)"),
      style = "font-size: 0.7em; color: black;  margin-top: 0.1em; margin-bottom: 0;"
    )),
  theme = bs_theme(
    version = 5,
    bootswatch = "sandstone",
    font_scale = 0.9,
    base_font = font_google("Poppins")
  ) |> bs_add_rules(
    "
    /* Ensure nav links remain bold */
    .nav-tabs .nav-link,
    .nav-pills .nav-link,
    .accordion-button {
      font-weight: bold;
    }

    /* Hide the top-level tabs of the main content area for 'page' navigation */
    .card .nav-tabs {
      display: none;
    }
    .card .card-header.bg-body-secondary ~ .tab-content {
      border-top: none; /* Remove extra border if card-header is used for main content */
    }

    /* Custom rule for the main sidebar title */
    .sidebar-title {
      color: #002D62; /* DepEd Blue */
      font-weight: bold;
    }

    /* Custom rule for card headers within the sidebar */
    .card-header {
      background-color: #e6f0ff; /* Very light blue, subtle background */
      color: #002D62; /* DepEd Blue for text */
      border-bottom: 1px solid #cce0ff; /* Slightly darker light blue border */
    }

    /* Ensure other h4 elements in sidebar (like Data Toggles) also use DepEd Blue */
    .sidebar h4 {
      color: #002D62; /* DepEd Blue */
      font-weight: bold;
    }
    "
  ),
  nav_spacer(),
  
  # --- First Top-Level Tab: Dashboard ---
  # Assuming this is part of your ui.R file, within the nav_menu("Dashboard") block
  
  nav_menu(
    title = tagList(bs_icon("speedometer"),
                    tags$b("Dashboard")),
    nav_panel(
      title = "Education Resource Dashboard",
      layout_sidebar(
        sidebar = sidebar(
          width = 300, # Keep the sidebar width
          title = "Dashboard Navigation", # Main sidebar title
          # Region Filter
          card(height = 400, # Adjusted height
               card_header(tags$b("Region Filter")),
               card_body( # Added card_body
                 pickerInput(
                   inputId = "dashboard_region_filter", # Keep the same inputId for server compatibility
                   label = NULL,
                   choices = c("Region I" = "Region I", "Region II" = "Region II", "Region III" = "Region III", "Region IV-A" = "Region IV-A", "MIMAROPA" = "MIMAROPA", "Region V" = "Region V", "Region VI" = "Region VI", "NIR" = "NIR", "Region VII" = "Region VII", "Region VIII" = "Region VIII", "Region IX" = "Region IX", "Region X" = "Region X", "Region XI" = "Region XI", "Region XII" = "Region XII", "CARAGA" = "CARAGA", "CAR" = "CAR", "NCR" = "NCR","BARMM" = "BARMM"),
                   selected = c("Region I"), # Keep the same default selected value
                   multiple = TRUE,
                   options = pickerOptions(
                     actionsBox = TRUE, # Changed to TRUE
                     liveSearch = TRUE,
                     header = "Select Regions", # Changed header text
                     title = "No Region Selected", # Changed title text
                     selectedTextFormat = "count > 3",
                     dropupAuto = FALSE, # This tells it NOT to automatically switch direction
                     dropup = FALSE # Added this option
                   ),
                   choicesOpt = list() # Added choicesOpt
                 )
               )
          ),
          card(height = 400, # Adjusted height
               card_header(tags$b("Division Filter")),
               card_body( # Added card_body
                 pickerInput(
                   inputId = "dashboard_division_filter", # Keep the same inputId for server compatibility
                   label = NULL,
                   choices = NULL, # Choices will be updated dynamically by the server
                   selected = NULL,
                   multiple = TRUE,
                   options = pickerOptions(
                     actionsBox = TRUE, # Changed to TRUE
                     liveSearch = TRUE,
                     header = "Select Divisions", # Changed header text
                     title = "No Division Selected", # Changed title text
                     selectedTextFormat = "count > 3",
                     dropupAuto = FALSE, # This tells it NOT to automatically switch direction
                     dropup = FALSE # Added this option
                   ),
                   choicesOpt = list() # Added choicesOpt
                 )
               )
          )),
      accordion(
        accordion_panel(
          title = "National Statistics",
          icon = bsicons::bs_icon("bar-chart"), # Optional icon
          # accordion_panel(
          #   title = "Learner Overview",
          #   layout_column_wrap(
          #     width = 1/4,
          #     value_box(title = "Total Learners", value = "21,669,181"),
          #     value_box(title = "Total ES Learners", value = "12,877,988"),
          #     value_box(title = "Total JHS Learners", value = "6,341,976"),
          #     value_box(title = "Total SHS Learners", value = "2,449,217"))),
          accordion_panel(
            title = "Curricular Offering",
            layout_column_wrap(
            width = 1/6,
            value_box(title = "Purely ES", value = "35,036"),
            value_box(title = "JHS with SHS", value = "6,598"),
            value_box(title = "ES and JHS (K to 10)", value = "1,690"),
            value_box(title = "Purely JHS", value = "1,367"),
            value_box(title = "All Offering (K to 12)", value = "832"),
            value_box(title = "Purely SHS", value = "262")
          )),
          accordion_panel(
            title = "School Size Typology",
            layout_column_wrap(
            width = 1/7,
            value_box(title = "Very Small", value = "24,976"),
            value_box(title = "Small", value = "10,105"),
            value_box(title = "Medium", value = "5,726"),
            value_box(title = "Large", value = "4,210"),
            value_box(title = "Very Large", value = "727"),
            value_box(title = "Extremely Large", value = "38"),
            value_box(title = "Mega", value = "3")
          )),
          accordion_panel(
            title = "Classroom Data",
            layout_column_wrap(
              width = 1/4,
              value_box(title = "Total Schools", value = "45,785"),
              value_box(title = "Schools with Classroom Shortage", value = "25,324"),
              value_box(title = "Schools with Classroom Shortage and Buildable Space", value = "11,347"),
              value_box(title = "National Classroom Shortage", value = "165,443"))
          )
        )),
      hr(),
      accordion(
        accordion_panel(
          title = "Regional Statistics",
          icon = bsicons::bs_icon("bar-chart"), # Optional icon
          layout_column_wrap(
            width = 1/2,
            uiOutput("total_schools_box"),
            uiOutput("total_schools_box_div")))),
      hr(), 
      layout_columns(
        col_widths = c(6,6,6,6,6,6,6,6,12),
        card(full_screen = TRUE,
          card_header("Curricular Offering"),
          navset_card_pill(
            nav_spacer(),
            nav_panel(
              title = "Regional Level",
                plotlyOutput("school_count_regional_graph", height = 500)
              ),
            nav_panel(
              title = "SDO Level",
                plotlyOutput("school_count_division_graph", height = 500)
              ),
            nav_panel(
              title = "Legislative District Level",
                plotlyOutput("school_count_district_graph", height = 500)
              )
            )
          ),
        card(full_screen = TRUE,
          card_header("School Size Typology"),
          navset_card_pill(
            nav_spacer(),
            nav_panel(
              title = "Regional Level",
              plotlyOutput("SOSSS_Region_Typology", height = 500)
            ),
              nav_panel(
                title = "SDO Level",
                plotlyOutput("SOSSS_Division_Typology", height = 500)
              ),
                nav_panel(
                  title = "Legislative District Level",
                  plotlyOutput("SOSSS_District_Typology", height = 500)
                )
                )
          ),
        card(full_screen = TRUE,
          card_header("Classroom Data"),
          navset_card_pill(
            nav_spacer(),
            nav_panel(
              title = "Regional Level",
              plotlyOutput("Classroom_Shortage_Region_Graph", height = 500)
              ),
              nav_panel(
                title = "SDO Level",
                plotlyOutput("Classroom_Shortage_Division_Graph", height = 500)
                ),
                nav_panel(
                  title = "Legislative District Level",
                  plotlyOutput("Classroom_Shortage_District_Graph", height = 500)
                )
                )
              ),
        card(full_screen = TRUE,
          card_header("Last Mile Schools"),
          navset_card_pill(
            nav_spacer(),
            nav_panel(
              title = "Regional Level",
              plotlyOutput("LMS_Nation_Graph", height = 500)
            ),
              nav_panel(
                title = "SDO Level",
                plotlyOutput("LMS_Division_Graph", height = 500)
              )
                )
              ),
        card(full_screen = TRUE,
          card_header("Teacher Shortage"),
          navset_card_pill(
            nav_spacer(),
            nav_panel(
              title = "Regional Level",
              plotlyOutput("Teacher_Shortage_Regional_Graph", height = 500)
            ),
              nav_panel(
                title = "SDO Level",
                plotlyOutput("Teacher_Shortage_Division_Graph", height = 500)
              )
                )
              ),
        card(full_screen = TRUE,
          card_header("School Principal Shortage"),
          navset_card_pill(
            nav_spacer(),
            nav_panel(
              title = "Regional Level",
              plotlyOutput("School_Principal_Regional_Graph", height = 500)
            ),
              nav_panel(
                title = "SDO Level",
                plotlyOutput("School_Principal_Division_Graph", height = 500)
              ),
                nav_panel(
                  title = "Legislative District Level",
                  plotlyOutput("School_Principal_District_Graph", height = 500)
                )
                )
              ),
        card(full_screen = TRUE,
          card_header("AO II Deployment"),
          navset_card_pill(
            nav_spacer(),
            nav_panel(
              title = "Regional Level",
              plotlyOutput("AOII_Regional_Graph", height = 500)
            ),
              nav_panel(
                title = "SDO Level",
                plotlyOutput("AOII_Division_Graph", height = 500)
              ),
                nav_panel(
                  title = "Legislative District Level",
                  plotlyOutput("AOII_District_Graph", height = 500)
                )
                )
              ),
        card(full_screen = TRUE,
          card_header("PDO I Deployment"),
          navset_card_pill(
            nav_spacer(),
            nav_panel(
              title = "Regional Level",
              plotlyOutput("PDOI_Regional_Graph", height = 500)
            ),
            nav_panel(
              title = "SDO Level",
              plotlyOutput("PDOI_Division_Graph", height = 500)
            ),
            nav_panel(
              title = "Legislative District Level",
              plotlyOutput("PDOI_District_Graph", height = 500)
            )
          )
        ),
        card(full_screen = TRUE,
          card_header("Sufficiency"),
          navset_card_pill(
            nav_spacer(),
            nav_panel(
              title = "Regional Level",
              selectInput("SuffOpt","Select a Category:", multiple = FALSE, selected = "Teacher.Sufficiency", choices = c("Teacher Sufficiency" = "Teacher.Sufficiency","Classroom Sufficiency" = "Classroom.Sufficiency","School Principal Sufficiency" = "SH.Sufficiency", "AO Sufficiency" = "AO.Sufficiency")),
              plotlyOutput("Sufficiency_Regional_Graph", height = 500)
            ),
            nav_panel(
              title = "SDO Level",
              selectInput("SuffOpt","Select a Category:", multiple = FALSE, selected = "Teacher.Sufficiency", choices = c("Teacher Sufficiency" = "Teacher.Sufficiency","Classroom Sufficiency" = "Classroom.Sufficiency","School Principal Sufficiency" = "SH.Sufficiency", "AO Sufficiency" = "AO.Sufficiency")),
              plotlyOutput("Sufficiency_Division_Graph", height = 500)
            )))),
      hr(),
      card(full_screen = TRUE,
        height = 800,
        card_header("School Database"),
        navset_card_pill(
          nav_spacer(),
          nav_panel(
            title = "School-level Data (SY 2024-2025)",
            dataTableOutput("regprof_DT")),
          nav_panel(
            title = "Classroom Data (SY 2023-2024)",
            dataTableOutput("regprof_DT_CL")),
      )))),
    # HROD panel
    # nav_panel(
    #   title = "Education Resource Information", # Your existing HROD content
    #   layout_sidebar(
    #     sidebar = sidebar(
    #       width = 300, # Keep the sidebar width
    #       title = "Dashboard Navigation", # Main sidebar title
    #       
    #       # Card for Main Category Picker (combining General Info, Resource Shortage, Other)
    #       # Assuming this UI code is part of your sidebar or main UI definition
    #       
    #       card(height = 400, # Adjusted height to 500
    #            card_header(tags$b("Select Category")),
    #            card_body( # Wrapped pickerInput in card_body
    #              pickerInput(
    #                inputId = "hrod_main_category_picker",
    #                label = NULL,
    #                choices = c(
    #                  "School Count" = "general_school_count",
    #                  "School Size Typology" = "general_sosss",
    #                  "Classroom Shortage" = "resource_shortage_classroom",
    #                  "Last Mile School" = "resource_lms",
    #                  "Teacher Shortage" = "resource_shortage_teacher",
    #                  "School Principal Shortage" = "resource_shortage_principal",
    #                  "Non-Teaching Personnel" = "resource_shortage_non_teaching",
    #                  "Sufficiency" = "others_sufficiency"
    #                ),
    #                selected = "general_school_count", # Keep the default selected value
    #                multiple = FALSE, # Keep as FALSE for single selection
    #                options = pickerOptions(
    #                  actionsBox = FALSE, # Keep as FALSE as it's single select
    #                  liveSearch = TRUE,
    #                  header = "Select a Category", # Keep existing header
    #                  title = "Select Category",
    #                  dropupAuto = FALSE, # This tells it NOT to automatically switch direction
    #                  dropup = FALSE # Keep existing title
    #                  # selectedTextFormat is not applicable for multiple = FALSE, so it's not added
    #                ),
    #                choicesOpt = list() # Added choicesOpt = list() for consistency
    #              )
    #            )
    #       ),
    #       
    #       hr(), # Add a separator
    #       
    #       # Card for Region Picker
    #       # Assuming this UI code is part of your sidebar or main UI definition
    #       
    #       # Region Filter
    #       card(height = 400, # Adjusted height
    #            card_header(tags$b("Region Filter")),
    #            card_body( # Added card_body
    #              pickerInput(
    #                inputId = "dashboard_region_filter", # Keep the same inputId for server compatibility
    #                label = NULL,
    #                choices = c("Region I" = "Region I", "Region II" = "Region II", "Region III" = "Region III", "Region IV-A" = "Region IV-A", "MIMAROPA" = "MIMAROPA", "Region V" = "Region V", "Region VI" = "Region VI", "NIR" = "NIR", "Region VII" = "Region VII", "Region VIII" = "Region VIII", "Region IX" = "Region IX", "Region X" = "Region X", "Region XI" = "Region XI", "Region XII" = "Region XII", "CARAGA" = "CARAGA", "CAR" = "CAR", "NCR" = "NCR","BARMM" = "BARMM"),
    #                selected = c("Region I"), # Keep the same default selected value
    #                multiple = TRUE,
    #                options = pickerOptions(
    #                  actionsBox = TRUE, # Changed to TRUE
    #                  liveSearch = TRUE,
    #                  header = "Select Regions", # Changed header text
    #                  title = "No Region Selected", # Changed title text
    #                  selectedTextFormat = "count > 3",
    #                  dropupAuto = FALSE, # This tells it NOT to automatically switch direction
    #                  dropup = FALSE # Added this option
    #                ),
    #                choicesOpt = list() # Added choicesOpt
    #              )
    #            )
    #       ),
    #       
    #       # Division Filter
    #       card(height = 400, # Adjusted height
    #            card_header(tags$b("Division Filter")),
    #            card_body( # Added card_body
    #              pickerInput(
    #                inputId = "dashboard_division_filter", # Keep the same inputId for server compatibility
    #                label = NULL,
    #                choices = NULL, # Choices will be updated dynamically by the server
    #                selected = NULL,
    #                multiple = TRUE,
    #                options = pickerOptions(
    #                  actionsBox = TRUE, # Changed to TRUE
    #                  liveSearch = TRUE,
    #                  header = "Select Divisions", # Changed header text
    #                  title = "No Division Selected", # Changed title text
    #                  selectedTextFormat = "count > 3",
    #                  dropupAuto = FALSE, # This tells it NOT to automatically switch direction
    #                  dropup = FALSE # Added this option
    #                ),
    #                choicesOpt = list() # Added choicesOpt
    #              )
    #            )
    #       )), # End of sidebar
    #     # Main content for Dashboard tab, controlled by uiOutput
    #     uiOutput("dashboard_main_content_area")
    #   ) # End of layout_sidebar
    # ),
    nav_panel("Plantilla Positions",  #GMIS
              layout_sidebar(
                sidebar = sidebar(
                  width = 450,
                  class = "bg-secondary",
                  h6("Data Toggles:"),
                  pickerInput(
                    inputId = "RegionGMIS",
                    label = "Select one or more Regions:",
                    choices = c(
                      "Region I" = "Region I - Ilocos",
                      "Region II" = "Region II - Cagayan Valley",
                      "Region III" = "Region III - Central Luzon",
                      "Region IV-A" = "Region IVA - CALABARZON",
                      "Region IV-B" = "Region IVB - MIMAROPA",
                      "Region V" = "Region V - Bicol",
                      "Region VI" = "Region VI - Western Visayas",
                      "Region VII" = "Region VII - Central Visayas",
                      "Region VIII" = "Region VIII - Eastern Visayas",
                      "Region IX" = "Region IX - Zamboanga Peninsula",
                      "Region X" = "Region X - Northern Mindanao",
                      "Region XI" = "Region XI - Davao",
                      "Region XII" = "Region XII - SOCCSKSARGEN",
                      "CARAGA" = "Region XIII - CARAGA",
                      "CAR" = "Cordillera Administrative Region (CAR)",
                      "NCR" = "National Capital Region (NCR)"
                    ),
                    selected = c(
                      "Region I" = "Region I - Ilocos",
                      "Region II" = "Region II - Cagayan Valley",
                      "Region III" = "Region III - Central Luzon",
                      "Region IV-A" = "Region IVA - CALABARZON",
                      "Region IV-B" = "Region IVB - MIMAROPA",
                      "Region V" = "Region V - Bicol",
                      "Region VI" = "Region VI - Western Visayas",
                      "Region VII" = "Region VII - Central Visayas",
                      "Region VIII" = "Region VIII - Eastern Visayas",
                      "Region IX" = "Region IX - Zamboanga Peninsula",
                      "Region X" = "Region X - Northern Mindanao",
                      "Region XI" = "Region XI - Davao",
                      "Region XII" = "Region XII - SOCCSKSARGEN",
                      "CARAGA" = "Region XIII - CARAGA",
                      "CAR" = "Cordillera Administrative Region (CAR)",
                      "NCR" = "National Capital Region (NCR)"
                    ), # You can set default selected values here
                    multiple = TRUE, # CRITICAL CHANGE: Must be TRUE to enable Select All/Deselect All
                    options = pickerOptions(
                      actionsBox = TRUE, # Already correct
                      liveSearch = TRUE,
                      header = "Select Regions",
                      title = "No Regions Selected",
                      selectedTextFormat = "count > 3",
                      dropupAuto = FALSE,
                      dropup = FALSE
                    ),
                    choicesOpt = list()
                  ),
                  uiOutput("SDOSelectionGMIS"),
                  # pickerInput(
                  #   inputId = "PosCatGMIS",
                  #   label = "Select a Position Category:",
                  #   choices = c(
                  #     "General Civil Servant" = "General Civil Servant",
                  #     "Teaching Related" = "Teaching Related",
                  #     "Allied Medical" = "Allied Medical",
                  #     "Medical" = "Medical",
                  #     "Teaching" = "Teaching"
                  #   ),
                  #   selected = c(
                  #     "Teaching" = "Teaching"
                  #   ),
                  #   multiple = TRUE,
                  #   options = pickerOptions(
                  #     liveSearch = TRUE,
                  #     actionsBox = TRUE, # This adds the "Select All" and "Deselect All" buttons
                  #     title = "No Category Selected",
                  #     header = "Select a Category"
                  #   )
                  # ),
                  uiOutput("PosSelectionGMIS"),
                  input_task_button("GMISRun", icon_busy = fontawesome::fa_i("refresh", class = "fa-spin", "aria-hidden" = "true"), strong("Show Selection"), class = "btn-danger")),
                layout_columns(
                      card(
                        card_header(strong("GMIS Data")),
                        plotlyOutput("GMISTable")),
                      card(
                        card_header(strong("GMIS Data")),
                        dataTableOutput("GMISTable1")),
                  col_widths = c(12,12)))),
    # NEW CONTENT FOR EFD NAV_PANEL STARTS HERE
    nav_panel(
      title = "Infrastructure and Education Facilities",
      layout_sidebar(
        sidebar = sidebar(
          width = 350,
          div( # This div acts as a container for the right-hand filter cards
            card( # Filter by Category
              card_header("Filter by Category"),
              height = 500,
              card_body(
                pickerInput(
                  inputId = "selected_category",
                  label = NULL,
                  choices = all_categories,
                  selected = all_categories,
                  multiple = TRUE,
                  options = pickerOptions(
                    actionsBox = TRUE,
                    liveSearch = TRUE,
                    header = "Select Categories",
                    title = "No Category Selected",
                    selectedTextFormat = "count > 3",
                    dropupAuto = FALSE, # This tells it NOT to automatically switch direction
                    dropup = FALSE
                  ),
                  choicesOpt = list()
                )
              )
            ),
            card( # Filter by Region
              card_header("Filter by Region"),
              height = 500,
              card_body(
                pickerInput(
                  inputId = "selected_region",
                  label = NULL,
                  choices = all_regions,
                  selected = all_regions,
                  multiple = TRUE,
                  options = pickerOptions(
                    actionsBox = TRUE,
                    liveSearch = TRUE,
                    header = "Select Regions",
                    title = "No Region Selected",
                    selectedTextFormat = "count > 3",
                    dropupAuto = FALSE, # This tells it NOT to automatically switch direction
                    dropup = FALSE
                  ),
                  choicesOpt = list()
                )
              )
            ),
            card( # Filter by Division
              card_header("Filter by Division"),
              height = 500,
              card_body(
                pickerInput(
                  inputId = "selected_division",
                  label = NULL,
                  choices = NULL,
                  selected = NULL,
                  multiple = TRUE,
                  options = pickerOptions(
                    actionsBox = TRUE,
                    liveSearch = TRUE,
                    header = "Select Divisions",
                    title = "No Division Selected",
                    selectedTextFormat = "count > 3",
                    dropupAuto = FALSE, # This tells it NOT to automatically switch direction
                    dropup = FALSE
                  ),
                  choicesOpt = list()
                )
              )
            )
          )
        ),
      tagList( # Start of tagList for EFD
        h3("Allocation and Completion Overview"),
        layout_columns( # Start of layout_columns for main content and filters
          col_widths = c(12,12,12,12), # Main content on left (10), Filters on right (2)
          
          # --- LEFT COLUMN: Main Dashboard Content (Value Boxes + Tabs) ---
          # div(
          #   # Value Boxes Section
          #   card(
          #     card_header("Total Allocation per Funding Year"),
          #     layout_columns(
          #       col_widths = 12 / length(unique_funding_years), # Distribute columns evenly
          #       row_heights = 1, # Forces them into a single row
          #       !!!lapply(unique_funding_years, function(year) {
          #         value_box(
          #           title = paste("Total in", year),
          #           value = tags$span(textOutput(paste0("vb_total_alloc_", year)), style = "font-size: 1.5em;"),
          #           showcase = bsicons::bs_icon("cash-coin"),
          #           theme = "primary",
          #           full_screen = TRUE
          #         )
          #       })
          #     )
          #   )),
            
            # Main Content Area (Tabs) - Below Value Boxes
            navset_card_tab(
              nav_panel("Allocation Overview",
                        layout_columns(
                          card(full_screen = TRUE,
                            plotlyOutput("allocationStackedBar", height = "100%"),
                            fill = TRUE, fillable = TRUE, max_height = "auto", height = 500
                          ))),
              nav_panel("Completion Overview",
                        card(full_screen = TRUE,
                          plotlyOutput("completionByCategoryPlot", height = "100%"),
                          fill = TRUE, fillable = TRUE, max_height = "auto", height = 500
                        )))
            , # Added comma to separate navset_card_tab from the next card
            card( # This card contains both the data table and the line graph
              layout_columns(
                col_widths = 12,
                card(
                  card_header("Detailed Project Data for Selected Bar Segment"),
                  DT::dataTableOutput("projectDetailTable", height = "100%"),
                  fill = TRUE, fillable = TRUE, max_height = "auto", height = 700)
              ),
              layout_columns(col_widths = 12,
                             row_heights = "fill",
                             card(
                               card_header("Allocation Trend per Category per Funding Year (Line Graph)"),
                               plotlyOutput("allocationTrendLine", height = "100%"),
                               fill = TRUE, fillable = TRUE, max_height = "auto", height = 600, full_screen = TRUE
                             )
              )
            )
          )
        ) # End of layout_columns for main content and filters
      ) # End of tagList for EFD
    )), # End of nav_menu
  nav_menu(
    title = tagList(bs_icon("cloud"),
                    tags$b("CLOUD")),
    
    nav_panel(
      title = "CLOUD (Regional Profile)", # Your existing HROD content
      layout_columns( # Main sidebar title
          # Card for Main Category Picker (combining General Info, Resource Shortage, Other)
          # Assuming this UI code is part of your sidebar or main UI definition
          # Region Filter
          card(height = 300, # Adjusted height
               card_header(tags$b("Region Filter")),
               card_body( # Added card_body
                 pickerInput(
                   inputId = "cloud_region_profile_filter", # Keep the same inputId for server compatibility
                   label = NULL,
                   choices = c("Region II" = "Region II", "MIMAROPA" = "MIMAROPA", "Region XII" = "Region XII", "CAR" = "CAR"),
                   selected = "Region II", # Keep the same default selected value
                   multiple = FALSE,
                   options = pickerOptions(
                     actionsBox = TRUE, # Changed to TRUE
                     liveSearch = TRUE,
                     header = "Select Regions", # Changed header text
                     title = "No Region Selected", # Changed title text
                     selectedTextFormat = "count > 3",
                     dropupAuto = FALSE, # This tells it NOT to automatically switch direction
                     dropup = FALSE # Added this option
                   ),
                   choicesOpt = list() # Added choicesOpt
                 )
               )
          )
          # 
          # # Division Filter
          # card(height = 400, # Adjusted height
          #      card_header(tags$b("Division Filter")),
          #      card_body( # Added card_body
          #        pickerInput(
          #          inputId = "cloud_dashboard_division_filter", # Keep the same inputId for server compatibility
          #          label = NULL,
          #          choices = NULL, # Choices will be updated dynamically by the server
          #          selected = NULL,
          #          multiple = FALSE,
          #          options = pickerOptions(
          #            actionsBox = TRUE, # Changed to TRUE
          #            liveSearch = TRUE,
          #            header = "Select Divisions", # Changed header text
          #            title = "No Division Selected", # Changed title text
          #            selectedTextFormat = "count > 3",
          #            dropupAuto = FALSE, # This tells it NOT to automatically switch direction
          #            dropup = FALSE # Added this option
          #          ),
          #          choicesOpt = list() # Added choicesOpt
          #        )
          #      )
          # )
        ), # End of sidebar
        # Main content for Dashboard tab, controlled by uiOutput
        uiOutput("cloud_profile_main_content_area") # End of layout_sidebar
    ), # End of nav_panel("HROD")
    
    nav_panel(
      title = "CLOUD (SDO Breakdown)", # Your existing HROD content
      layout_sidebar(
        sidebar = sidebar(
          width = 350, # Keep the sidebar width
          title = "Dashboard Navigation", # Main sidebar title
          
          # Card for Main Category Picker (combining General Info, Resource Shortage, Other)
          # Assuming this UI code is part of your sidebar or main UI definition
          
          card(height = 400, # Adjusted height to 500
               card_header(tags$b("Select Category")),
               card_body( # Wrapped pickerInput in card_body
                 pickerInput(
                   inputId = "cloud_main_category_picker",
                   label = NULL,
                   choices = c(
                     "Enrolment Data" = "cloud_enrolment",
                     "SNED Learners" = "cloud_sned",
                     "IP Learners" = "cloud_ip",
                     "Muslim Learners" = "cloud_muslim",
                     "Displaced Learners" = "cloud_displaced",
                     "ALS Learners" = "cloud_als",
                     "Dropout Data" = "cloud_dropout",
                     "Teacher Inventory" = "cloud_teacherinventory",
                     "Years in Service" = "cloud_years",
                     "Classroom Inventory" = "cloud_classroom",
                     "Multigrade" = "cloud_multigrade",
                     "Organized Class" = "cloud_organizedclass",
                     "JHS Teacher Deployment" = "cloud_jhsdeployment",
                     "Shifting" = "cloud_shifting",
                     "Learning Delivery Modality" = "cloud_LDM",
                     "ARAL" = "cloud_ARAL",
                     "CRLA" = "cloud_crla",
                     "PhilIRI" = "cloud_philiri",
                     "Alternative Delivery Modality" = "cloud_adm",
                     "Reading Proficiency" = "cloud_rf",
                     "Electricity Source" = "cloud_elec",
                     "Water Source" = "cloud_water",
                     "Internet Source" = "cloud_internet",
                     "Internet Usage" = "cloud_internet_usage",
                     "Bullying Incidence" = "cloud_bully",
                     # "School Initiatives" = "cloud_initiatives",
                     # "Medium of Instruction" = "cloud_moi",
                     # "School Structure" = "cloud_sosss",
                     # "SHS Implementation" = "cloud_shsimplem",
                     "Overload Pay" = "cloud_overload",
                     "School Resources" = "cloud_resources",
                     "NAT" = "cloud_nat",
                     "NAT Sufficiency" = "cloud_nat_sufficiency",
                     "LAC" = "cloud_lac",
                     "Feeding Program" = "cloud_feeding",
                     "SHA" = "cloud_sha"
                     # "Child Protection" = "cloud_childprotection",
                     # "Extension" = "cloud_extension"
                   ),
                   selected = "general_school_count", # Keep the default selected value
                   multiple = FALSE, # Keep as FALSE for single selection
                   options = pickerOptions(
                     actionsBox = FALSE, # Keep as FALSE as it's single select
                     liveSearch = TRUE,
                     header = "Select a Category", # Keep existing header
                     title = "Select Category",
                     dropupAuto = FALSE, # This tells it NOT to automatically switch direction
                     dropup = FALSE # Keep existing title
                     # selectedTextFormat is not applicable for multiple = FALSE, so it's not added
                   ),
                   choicesOpt = list() # Added choicesOpt = list() for consistency
                 )
               )
          ),
          
          hr(), # Add a separator
          
          # Card for Region Picker
          # Assuming this UI code is part of your sidebar or main UI definition
          
          # Region Filter
          card(height = 400, # Adjusted height
               card_header(tags$b("Region Filter")),
               card_body( # Added card_body
                 pickerInput(
                   inputId = "cloud_region_filter", # Keep the same inputId for server compatibility
                   label = NULL,
                   choices = c("Region II" = "Region II", "MIMAROPA" = "MIMAROPA", "Region XII" = "Region XII", "CAR" = "CAR"),
                   selected = "Region II", # Keep the same default selected value
                   multiple = FALSE,
                   options = pickerOptions(
                     actionsBox = TRUE, # Changed to TRUE
                     liveSearch = TRUE,
                     header = "Select Regions", # Changed header text
                     title = "No Region Selected", # Changed title text
                     selectedTextFormat = "count > 3",
                     dropupAuto = FALSE, # This tells it NOT to automatically switch direction
                     dropup = FALSE # Added this option
                   ),
                   choicesOpt = list() # Added choicesOpt
                 )
               )
          )
          # 
          # # Division Filter
          # card(height = 400, # Adjusted height
          #      card_header(tags$b("Division Filter")),
          #      card_body( # Added card_body
          #        pickerInput(
          #          inputId = "cloud_dashboard_division_filter", # Keep the same inputId for server compatibility
          #          label = NULL,
          #          choices = NULL, # Choices will be updated dynamically by the server
          #          selected = NULL,
          #          multiple = FALSE,
          #          options = pickerOptions(
          #            actionsBox = TRUE, # Changed to TRUE
          #            liveSearch = TRUE,
          #            header = "Select Divisions", # Changed header text
          #            title = "No Division Selected", # Changed title text
          #            selectedTextFormat = "count > 3",
          #            dropupAuto = FALSE, # This tells it NOT to automatically switch direction
          #            dropup = FALSE # Added this option
          #          ),
          #          choicesOpt = list() # Added choicesOpt
          #        )
          #      )
          # )
        ), # End of sidebar
        # Main content for Dashboard tab, controlled by uiOutput
        uiOutput("cloud_main_content_area")
      ) # End of layout_sidebar
    ), # End of nav_panel("HROD")
    
    nav_panel(
      title = tagList("CLOUD", em("(Multi-variable)")), # Your existing HROD content
      
      # A fluidRow to contain the three main boxes
      fluidRow(
        # First Box
        column(
          width = 6,
          card(
            card_header(tags$b("Data View 1")),
            card_body(
              pickerInput(
                inputId = "cloud_category_picker_1",
                label = NULL,
                choices = c(
                  "Enrolment Data" = "cloud_enrolment",
                  "SNED Learners" = "cloud_sned",
                  "IP Learners" = "cloud_ip",
                  "Muslim Learners" = "cloud_muslim",
                  "Displaced Learners" = "cloud_displaced",
                  "ALS Learners" = "cloud_als",
                  "Dropout Data" = "cloud_dropout",
                  "Teacher Inventory" = "cloud_teacherinventory",
                  "Years in Service" = "cloud_years",
                  "Classroom Inventory" = "cloud_classroom",
                  "Multigrade" = "cloud_multigrade",
                  "Organized Class" = "cloud_organizedclass",
                  "JHS Teacher Deployment" = "cloud_jhsdeployment",
                  "Shifting" = "cloud_shifting",
                  "Learning Delivery Modality" = "cloud_LDM",
                  "ARAL" = "cloud_ARAL",
                  "CRLA" = "cloud_crla",
                  "PhilIRI" = "cloud_philiri",
                  "Alternative Delivery Modality" = "cloud_adm",
                  "Reading Proficiency" = "cloud_rf",
                  "Electricity Source" = "cloud_elec",
                  "Water Source" = "cloud_water",
                  "Internet Source" = "cloud_internet",
                  "Internet Usage" = "cloud_internet_usage",
                  "Bullying Incidence" = "cloud_bully",
                  # "School Initiatives" = "cloud_initiatives",
                  # "Medium of Instruction" = "cloud_moi",
                  # "School Structure" = "cloud_sosss",
                  # "SHS Implementation" = "cloud_shsimplem",
                  "Overload Pay" = "cloud_overload",
                  "School Resources" = "cloud_resources",
                  "NAT" = "cloud_nat",
                  "NAT Sufficiency" = "cloud_nat_sufficiency",
                  "LAC" = "cloud_lac",
                  "Feeding Program" = "cloud_feeding",
                  "SHA" = "cloud_sha"
                  # "Child Protection" = "cloud_childprotection",
                  # "Extension" = "cloud_extension"
                ),
                selected = "cloud_enrolment",
                multiple = FALSE,
                options = pickerOptions(
                  liveSearch = TRUE,
                  title = "Select Category"
                )
              ),
              uiOutput("cloud_graph_1") # Placeholder for the first graph
            )
          )
        ),
        
        # Second Box
        column(
          width = 6,
          card(
            card_header(tags$b("Data View 2")),
            card_body(
              pickerInput(
                inputId = "cloud_category_picker_2",
                label = NULL,
                choices = c(
                  "Enrolment Data" = "cloud_enrolment",
                  "SNED Learners" = "cloud_sned",
                  "IP Learners" = "cloud_ip",
                  "Muslim Learners" = "cloud_muslim",
                  "Displaced Learners" = "cloud_displaced",
                  "ALS Learners" = "cloud_als",
                  "Dropout Data" = "cloud_dropout",
                  "Teacher Inventory" = "cloud_teacherinventory",
                  "Years in Service" = "cloud_years",
                  "Classroom Inventory" = "cloud_classroom",
                  "Multigrade" = "cloud_multigrade",
                  "Organized Class" = "cloud_organizedclass",
                  "JHS Teacher Deployment" = "cloud_jhsdeployment",
                  "Shifting" = "cloud_shifting",
                  "Learning Delivery Modality" = "cloud_LDM",
                  "ARAL" = "cloud_ARAL",
                  "CRLA" = "cloud_crla",
                  "PhilIRI" = "cloud_philiri",
                  "Alternative Delivery Modality" = "cloud_adm",
                  "Reading Proficiency" = "cloud_rf",
                  "Electricity Source" = "cloud_elec",
                  "Water Source" = "cloud_water",
                  "Internet Source" = "cloud_internet",
                  "Internet Usage" = "cloud_internet_usage",
                  "Bullying Incidence" = "cloud_bully",
                  # "School Initiatives" = "cloud_initiatives",
                  # "Medium of Instruction" = "cloud_moi",
                  # "School Structure" = "cloud_sosss",
                  # "SHS Implementation" = "cloud_shsimplem",
                  "Overload Pay" = "cloud_overload",
                  "School Resources" = "cloud_resources",
                  "NAT" = "cloud_nat",
                  "NAT Sufficiency" = "cloud_nat_sufficiency",
                  "LAC" = "cloud_lac",
                  "Feeding Program" = "cloud_feeding",
                  "SHA" = "cloud_sha"
                  # "Child Protection" = "cloud_childprotection",
                  # "Extension" = "cloud_extension"
                ),
                selected = "cloud_teacherinventory",
                multiple = FALSE,
                options = pickerOptions(
                  liveSearch = TRUE,
                  title = "Select Category"
                )
              ),
              uiOutput("cloud_graph_2") # Placeholder for the second graph
            )
          )
        ),
        
        # Third Box
        column(
          width = 6,
          card(
            card_header(tags$b("Data View 3")),
            card_body(
              pickerInput(
                inputId = "cloud_category_picker_3",
                label = NULL,
                choices = c(
                  "Enrolment Data" = "cloud_enrolment",
                  "SNED Learners" = "cloud_sned",
                  "IP Learners" = "cloud_ip",
                  "Muslim Learners" = "cloud_muslim",
                  "Displaced Learners" = "cloud_displaced",
                  "ALS Learners" = "cloud_als",
                  "Dropout Data" = "cloud_dropout",
                  "Teacher Inventory" = "cloud_teacherinventory",
                  "Years in Service" = "cloud_years",
                  "Classroom Inventory" = "cloud_classroom",
                  "Multigrade" = "cloud_multigrade",
                  "Organized Class" = "cloud_organizedclass",
                  "JHS Teacher Deployment" = "cloud_jhsdeployment",
                  "Shifting" = "cloud_shifting",
                  "Learning Delivery Modality" = "cloud_LDM",
                  "ARAL" = "cloud_ARAL",
                  "CRLA" = "cloud_crla",
                  "PhilIRI" = "cloud_philiri",
                  "Alternative Delivery Modality" = "cloud_adm",
                  "Reading Proficiency" = "cloud_rf",
                  "Electricity Source" = "cloud_elec",
                  "Water Source" = "cloud_water",
                  "Internet Source" = "cloud_internet",
                  "Internet Usage" = "cloud_internet_usage",
                  "Bullying Incidence" = "cloud_bully",
                  # "School Initiatives" = "cloud_initiatives",
                  # "Medium of Instruction" = "cloud_moi",
                  # "School Structure" = "cloud_sosss",
                  # "SHS Implementation" = "cloud_shsimplem",
                  "Overload Pay" = "cloud_overload",
                  "School Resources" = "cloud_resources",
                  "NAT" = "cloud_nat",
                  "NAT Sufficiency" = "cloud_nat_sufficiency",
                  "LAC" = "cloud_lac",
                  "Feeding Program" = "cloud_feeding",
                  "SHA" = "cloud_sha"
                  # "Child Protection" = "cloud_childprotection",
                  # "Extension" = "cloud_extension"
                ),
                selected = "cloud_classroom",
                multiple = FALSE,
                options = pickerOptions(
                  liveSearch = TRUE,
                  title = "Select Category"
                )
              ),
              uiOutput("cloud_graph_3") # Placeholder for the third graph
            )
          )
        ),
        # Fourth Box
        column(
          width = 6,
          card(
            card_header(tags$b("Data View 4")),
            card_body(
              pickerInput(
                inputId = "cloud_category_picker_4",
                label = NULL,
                choices = c(
                  "Enrolment Data" = "cloud_enrolment",
                  "SNED Learners" = "cloud_sned",
                  "IP Learners" = "cloud_ip",
                  "Muslim Learners" = "cloud_muslim",
                  "Displaced Learners" = "cloud_displaced",
                  "ALS Learners" = "cloud_als",
                  "Dropout Data" = "cloud_dropout",
                  "Teacher Inventory" = "cloud_teacherinventory",
                  "Years in Service" = "cloud_years",
                  "Classroom Inventory" = "cloud_classroom",
                  "Multigrade" = "cloud_multigrade",
                  "Organized Class" = "cloud_organizedclass",
                  "JHS Teacher Deployment" = "cloud_jhsdeployment",
                  "Shifting" = "cloud_shifting",
                  "Learning Delivery Modality" = "cloud_LDM",
                  "ARAL" = "cloud_ARAL",
                  "CRLA" = "cloud_crla",
                  "PhilIRI" = "cloud_philiri",
                  "Alternative Delivery Modality" = "cloud_adm",
                  "Reading Proficiency" = "cloud_rf",
                  "Electricity Source" = "cloud_elec",
                  "Water Source" = "cloud_water",
                  "Internet Source" = "cloud_internet",
                  "Internet Usage" = "cloud_internet_usage",
                  "Bullying Incidence" = "cloud_bully",
                  # "School Initiatives" = "cloud_initiatives",
                  # "Medium of Instruction" = "cloud_moi",
                  # "School Structure" = "cloud_sosss",
                  # "SHS Implementation" = "cloud_shsimplem",
                  "Overload Pay" = "cloud_overload",
                  "School Resources" = "cloud_resources",
                  "NAT" = "cloud_nat",
                  "NAT Sufficiency" = "cloud_nat_sufficiency",
                  "LAC" = "cloud_lac",
                  "Feeding Program" = "cloud_feeding",
                  "SHA" = "cloud_sha"
                  # "Child Protection" = "cloud_childprotection",
                  # "Extension" = "cloud_extension"
                ),
                selected = "cloud_shifting",
                multiple = FALSE,
                options = pickerOptions(
                  liveSearch = TRUE,
                  title = "Select Category"
                )
              ),
              uiOutput("cloud_graph_4") # Placeholder for the third graph
            )
          )
        ),
        column(
          width = 6,
          card(
            card_header(tags$b("Data View 5")),
            card_body(
              pickerInput(
                inputId = "cloud_category_picker_5",
                label = NULL,
                choices = c(
                  "Enrolment Data" = "cloud_enrolment",
                  "SNED Learners" = "cloud_sned",
                  "IP Learners" = "cloud_ip",
                  "Muslim Learners" = "cloud_muslim",
                  "Displaced Learners" = "cloud_displaced",
                  "ALS Learners" = "cloud_als",
                  "Dropout Data" = "cloud_dropout",
                  "Teacher Inventory" = "cloud_teacherinventory",
                  "Years in Service" = "cloud_years",
                  "Classroom Inventory" = "cloud_classroom",
                  "Multigrade" = "cloud_multigrade",
                  "Organized Class" = "cloud_organizedclass",
                  "JHS Teacher Deployment" = "cloud_jhsdeployment",
                  "Shifting" = "cloud_shifting",
                  "Learning Delivery Modality" = "cloud_LDM",
                  "ARAL" = "cloud_ARAL",
                  "CRLA" = "cloud_crla",
                  "PhilIRI" = "cloud_philiri",
                  "Alternative Delivery Modality" = "cloud_adm",
                  "Reading Proficiency" = "cloud_rf",
                  "Electricity Source" = "cloud_elec",
                  "Water Source" = "cloud_water",
                  "Internet Source" = "cloud_internet",
                  "Internet Usage" = "cloud_internet_usage",
                  "Bullying Incidence" = "cloud_bully",
                  # "School Initiatives" = "cloud_initiatives",
                  # "Medium of Instruction" = "cloud_moi",
                  # "School Structure" = "cloud_sosss",
                  # "SHS Implementation" = "cloud_shsimplem",
                  "Overload Pay" = "cloud_overload",
                  "School Resources" = "cloud_resources",
                  "NAT" = "cloud_nat",
                  "NAT Sufficiency" = "cloud_nat_sufficiency",
                  "LAC" = "cloud_lac",
                  "Feeding Program" = "cloud_feeding",
                  "SHA" = "cloud_sha"
                  # "Child Protection" = "cloud_childprotection",
                  # "Extension" = "cloud_extension"
                ),
                selected = "cloud_enrolment",
                multiple = FALSE,
                options = pickerOptions(
                  liveSearch = TRUE,
                  title = "Select Category"
                )
              ),
              uiOutput("cloud_graph_5") # Placeholder for the first graph
            )
          )
        ),
        column(
          width = 6,
          card(
            card_header(tags$b("Data View 6")),
            card_body(
              pickerInput(
                inputId = "cloud_category_picker_6",
                label = NULL,
                choices = c(
                  "Enrolment Data" = "cloud_enrolment",
                  "SNED Learners" = "cloud_sned",
                  "IP Learners" = "cloud_ip",
                  "Muslim Learners" = "cloud_muslim",
                  "Displaced Learners" = "cloud_displaced",
                  "ALS Learners" = "cloud_als",
                  "Dropout Data" = "cloud_dropout",
                  "Teacher Inventory" = "cloud_teacherinventory",
                  "Years in Service" = "cloud_years",
                  "Classroom Inventory" = "cloud_classroom",
                  "Multigrade" = "cloud_multigrade",
                  "Organized Class" = "cloud_organizedclass",
                  "JHS Teacher Deployment" = "cloud_jhsdeployment",
                  "Shifting" = "cloud_shifting",
                  "Learning Delivery Modality" = "cloud_LDM",
                  "ARAL" = "cloud_ARAL",
                  "CRLA" = "cloud_crla",
                  "PhilIRI" = "cloud_philiri",
                  "Alternative Delivery Modality" = "cloud_adm",
                  "Reading Proficiency" = "cloud_rf",
                  "Electricity Source" = "cloud_elec",
                  "Water Source" = "cloud_water",
                  "Internet Source" = "cloud_internet",
                  "Internet Usage" = "cloud_internet_usage",
                  "Bullying Incidence" = "cloud_bully",
                  # "School Initiatives" = "cloud_initiatives",
                  # "Medium of Instruction" = "cloud_moi",
                  # "School Structure" = "cloud_sosss",
                  # "SHS Implementation" = "cloud_shsimplem",
                  "Overload Pay" = "cloud_overload",
                  "School Resources" = "cloud_resources",
                  "NAT" = "cloud_nat",
                  "NAT Sufficiency" = "cloud_nat_sufficiency",
                  "LAC" = "cloud_lac",
                  "Feeding Program" = "cloud_feeding",
                  "SHA" = "cloud_sha"
                  # "Child Protection" = "cloud_childprotection",
                  # "Extension" = "cloud_extension"
                ),
                selected = "cloud_enrolment",
                multiple = FALSE,
                options = pickerOptions(
                  liveSearch = TRUE,
                  title = "Select Category"
                )
              ),
              uiOutput("cloud_graph_6") # Placeholder for the first graph
            )
          )
        )
  ))),
  # --- Second Top-Level Tab: Data Explorer (Now a Dropdown Menu) ---
  nav_panel(
    title = tags$b("Data Explorer"), # This will be the dropdown title
    icon = bs_icon("table"),
      layout_sidebar(
        sidebar = sidebar(
          width = 350,
          h6("Data Toggles:"),
          pickerInput(
            inputId = "DataBuilder_HROD_Region",
            label = "Select a Region:",
            choices = sort(unique(uni$Region)),
            selected = sort(unique(uni$Region)),
            multiple = TRUE,
            options = pickerOptions(
              actionsBox = TRUE,
              liveSearch = TRUE,
              header = "Select Categories",
              title = "No Category Selected",
              selectedTextFormat = "count > 3",
              dropupAuto = FALSE, # This tells it NOT to automatically switch direction
              dropup = FALSE
            ),
            choicesOpt = list()
          ),
          uiOutput("DataBuilder_HROD_SDO"), # Existing SDO Selection
          # School Information Data Toggles
          pickerInput("School_Data_Toggles", strong("School Information Data Toggles"), 
                      choices = c("School Size Typology" = "School.Size.Typology", "Curricular Offering" = "Modified.COC"),
                      multiple = TRUE, options = list(`actions-box` = TRUE)),
          
          # Teaching Data Toggles
          pickerInput("Teaching_Data_Toggles", strong("Teaching Data Toggles"), 
                      choices = c("Total Teachers" = "TotalTeachers", "Teacher Excess" = "Total.Excess", "Teacher Shortage" = "Total.Shortage"),
                      multiple = TRUE, options = list(`actions-box` = TRUE)),
          
          # Non-teaching Data Toggles
          pickerInput("NTP_Data_Toggles", strong("Non-teaching Data Toggles"), 
                      choices = c("COS" = "Outlier.Status", "AOII Clustering Status" = "Clustering.Status"),
                      multiple = TRUE, options = list(`actions-box` = TRUE)),
          
          # Enrolment Data Toggles
          pickerInput("Enrolment_Data_Toggles", strong("Enrolment Data Toggles"), 
                      choices = c("Total Enrolment" = "TotalEnrolment", "Kinder" = "Kinder", "Grade 1" = "G1", "Grade 2" = "G2", "Grade 3" = "G3", 
                                  "Grade 4" = "G4", "Grade 5" = "G5", "Grade 6" = "G6", "Grade 7" = "G7", "Grade 8" = "G8", 
                                  "Grade 9" = "G9", "Grade 10" = "G10", "Grade 11" = "G11", "Grade 12" = "G12"),
                      multiple = TRUE, options = list(`actions-box` = TRUE)),
          
          # Specialization Data Toggles
          pickerInput("Specialization_Data_Toggles", strong("Specialization Data Toggles"), 
                      choices = c("English" = "English", "Mathematics" = "Mathematics", "Science" = "Science", 
                                  "Biological Sciences" = "Biological.Sciences", "Physical Sciences" = "Physical.Sciences"),
                      multiple = TRUE, options = list(`actions-box` = TRUE)),
          
          # Infrastructure Data Toggles
          pickerInput("EFD_Data_Toggles", strong("Infrastructure Data Toggles"), 
                      choices = c("Number of Buildings" = "Buildings", "Instructional Rooms" = "Instructional.Rooms.2023.2024", 
                                  "Classroom Requirement" = "Classroom.Requirement", "Estimated Classroom Shortage" = "Est.CS", 
                                  "Buildable Space" = "Buidable_space", "Congestion Index" = "Congestion.Index", "Shifting" = "Shifting", 
                                  "Ownership Type" = "OwnershipType", "Electricity Source" = "ElectricitySource", "Water Source" = "WaterSource", 
                                  "For Major Repairs" = "Major.Repair.2023.2024", "School Building Priority Index" = "SBPI", 
                                  "Total Seats" = "Total.Seats.2023.2024", "Total Seats Shortage" = "Total.Seats.Shortage.2023.2024"),
                      multiple = TRUE, options = list(`actions-box` = TRUE))
        ),
        layout_columns(
          card(
            card_header(strong("HROD Data Panel")), # Renamed for clarity
            dataTableOutput("HROD_Table")
          ),
          col_widths = c(12,12)
        )
      )
    ),
  # --- Quick School Search ---
  nav_panel(
    title = tags$b("Quick School Search"),
    icon = bs_icon("search"),
    layout_sidebar(
      sidebar = sidebar(
        textInput("text","Enter School Name"),
        input_task_button("TextRun", icon_busy = fontawesome::fa_i("refresh", class = "fa-spin", "aria-hidden" = "true"), strong("Show Selection"), class = "btn-success")),
      layout_columns(
        card(
          card_header(strong("Search Output")),
          dataTableOutput("TextTable")),
        card(full_screen = TRUE,
             card_header(strong("School Mapping")),
             leafletOutput("TextMapping", height = 500, width = "100%")),
        card(full_screen = TRUE,
             card_header(div(strong("School Details"),
                             tags$span(em("(Select a school from the table above)"),
                                       style = "font-size: 0.7em; color: grey;"
                             ))),
             layout_columns(
               card(full_screen = TRUE,
                    card_header(strong("Basic Information")),
                    tableOutput("schooldetails")),
               card(full_screen = TRUE,
                    card_header(strong("HR Data")),
                    tableOutput("schooldetails2")),
               card(full_screen = TRUE,
                    card_header(strong("Classroom Data")),
                    tableOutput("schooldetails3")),
               card(full_screen = TRUE,
                    card_header(div(strong("Specialization Data"),
                                    tags$span(em("(based on eSF7 for SY 2023-2024)"),
                                              style = "font-size: 0.7em; color: grey;"
                                    ))),
                    tableOutput("schooldetails5")),
               col_widths = c(6,6,6,6))),
        col_widths = c(6,6,12)))),
  
  # --- Resource Mapping ---
  nav_panel(
    title = tags$b("Resource Mapping"),
    icon = bs_icon("map"),
    layout_sidebar(
      sidebar = sidebar(
        width = 375, # Extend sidebar width by 25%
        title = "Resource Mapping Filters",
        
        # --- Data Filters Card for Resource Mapping ---
        card(height = 400,
             card_header(tags$b("Data Filters")), # NEW: Card header for filters
             selectInput("resource_map_region", "Region:",
                         choices = c("Region I" = "Region I","Region II" = "Region II","Region III" = "Region III","Region IV-A" = "Region IV-A","MIMAROPA" = "MIMAROPA","Region V" = "Region V","Region VI" = "Region VI","NIR" = "NIR","Region VII" = "Region VII","Region VIII" = "Region VIII","Region IX" = "Region IX","Region X" = "Region X","Region XI" = "Region XI","Region XII" = "Region XII","CARAGA" = "CARAGA","CAR" = "CAR","NCR" = "NCR"),
                         selected = "Region I"),
             uiOutput("resource_map_division"),
             uiOutput("resource_map_legislative_district"),
             input_task_button("Mapping_Run", strong("Show Selection"), class = "btn-success"),
        ),
        hr(),
        
        # Radio Buttons for resource types
        card(
          card_header(tags$b("Resource Types")),
          radioButtons(
            inputId = "resource_type_selection",
            label = NULL,
            choices = c(
              "Teaching Deployment",
              "Non-teaching Deployment",
              "Classroom Inventory",
              "Learner Congestion",
              "Industries",
              "Facilities"
            ),
            selected = "Teaching Deployment"
          )
        )
      ),
      
      # 2. Main Panel: This is now a single uiOutput that will be rendered dynamically
      mainPanel(
        width = 12,
        uiOutput("dynamic_resource_panel") # This will be generated by renderUI on the server
      )
    )
  ),
  
  # --- Last Top-Level Tab: About ---
  nav_panel(
    title = tags$b("About"),
    icon = bs_icon("info-circle"),
    tagList(
      layout_columns(
        HTML('<img src="Contactus.png" width="100%" height="auto">'))
    )),
  
  # --- Contact Us Top-Level Tab ---
  nav_panel(
    title = tags$b("Contact Us"),
    icon = bs_icon("envelope"),
    h3("Contact Information"),
    fluidRow(
      column(12,
             tags$iframe(
               id = "googleform",
               src = "https://docs.google.com/forms/d/e/1FAIpQLScmWmVzlAHgsitxUncINy4OC_5gkyg2LvYcJAkAGlGAzQHNvw/viewform?embedded=true", # Replace YOUR_FORM_ID
               width = "100%", # Or a specific pixel value like "760"
               height = "700px", # Or a specific pixel value like "500"
               frameborder = "0",
               marginheight = "0",
               marginwidth = "0")
      )
    )
  ))
  })
  
  # Reactive expression to generate the main panel content
  output$dynamic_resource_panel <- renderUI({
    
    selected_resource_type <- input$resource_type_selection
    
    if (selected_resource_type == "Teaching Deployment") {
      tagList(
        h3("Teaching Deployment Overview"),
        hr(),
        layout_columns(
          selectInput("resource_map_level", "Filter Curricular Level:",
                    choices = c("Elementary School"="ES","Junior High School"="JHS","Senior High School"="SHS"),
                    selected = "ES"),
          input_task_button("Teaching_Deployment_Refresh", strong("Refresh"), class = "btn-success"),
          col_widths = c(4,-8,2)),
        hr(),
        layout_column_wrap(
          width = 1/5,
          card(
            card_header(strong("RO Filling-up Rate")),
            valueBoxOutput("f")
          ),
          card(
            card_header(strong("RO Unfilled Items")),
            valueBoxOutput("g")
          ),
          card(
            card_header(strong("SDO Filling-up Rate")),
            valueBoxOutput("a")
          ),
          card(
            card_header(strong("SDO Unfilled Items")),
            valueBoxOutput("b")
          ),
          card(
            card_header(strong("SDO Net Shortage")),
            valueBoxOutput("e")
          )
        ),
        layout_columns(
          card(
            card_header(strong("Teacher Excess and Shortage")),
            dataTableOutput("TeacherShortage_Table")
          ),
          card(full_screen = TRUE,
               card_header(strong("Personnel Deployment Mapping")),
               leafletOutput("TeacherShortage_Mapping", height = 700)
          ),
          card(height = 200,
               card_header(div("School Summary",
                               tags$span(em("(Select a school from the table above)"),
                                         style = "font-size: 0.7em; color: grey;")
               )),
               uiOutput("TeacherShortage_Assessment")
          ),
          col_widths = c(4, 8, 12)
        )
      )
    } else if (selected_resource_type == "Non-teaching Deployment") {
      tagList(
        h3("Non-teaching Deployment Overview"),
        hr(),
        layout_columns(
          navset_card_tab(
            nav_spacer(),
            nav_panel(
              title = "Regional Summary",
              layout_columns(
                card(
                  card_header(strong("Schools under Clustered AO II Deployment")),
                  valueBoxOutput("f2")
                ),
                card(
                  card_header(strong("Schools with Dedicated AOII Deployment")),
                  valueBoxOutput("g2")
                ),
                col_widths = c(6,6)
              )
            ),
            nav_panel(
              title = "Division Summary",
              layout_columns(
                card(
                  card_header(strong("Schools under Clustered AO II Deployment")),
                  valueBoxOutput("a2")
                ),
                card(
                  card_header(strong("Schools with Dedicated AOII Deployment")),
                  valueBoxOutput("b2")
                ),
                col_widths = c(6, 6)
              )
            ),
            nav_panel(
              title = "District Summary",
              layout_columns(
                card(
                  card_header(strong("Schools under Clustered AO II Deployment")),
                  valueBoxOutput("e2")
                ),
                card(
                  card_header(strong("Schools with Dedicated AOII Deployment")),
                  valueBoxOutput("h2")
                ),
                col_widths = c(6, 6)
              )
            )
          ),
          card(
            card_header(div(strong("AO II Deployment Status"),
                            tags$span(
                              em("(as of September 2, 2025)"),
                              style = "font-size: 0.8em; color: grey; margin-top: 0.1em; margin-bottom: 0;"
                            )
            )),
            dataTableOutput("AO2Table")
          ),
          card(full_screen = TRUE,
               card_header(strong("Personnel Deployment Mapping")),
               leafletOutput("AO2Mapping", height = 800)
          ),
          col_widths = c(12,5,7)
        )
      )
    } else if (selected_resource_type == "Classroom Inventory") {
      tagList(
        h3("Classroom Inventory Overview"),
        hr(),
        layout_column_wrap(
          width = 1/3,
          card(
            card_header(strong("Regional Classroom Shortage")),
            valueBoxOutput("ROCRShort")
          ),
          card(
            card_header(strong("Division Classroom Shortage")),
            valueBoxOutput("SDOCRShort")
          ),
          card(
            card_header(strong("District Classroom Shortage")),
            valueBoxOutput("DistCRShort")
          )
        ),
        layout_columns(
          card(full_screen = TRUE,
               card_header(strong("School Building Priority Index")),
               dataTableOutput("CLTable")
          ),
          card(full_screen = TRUE,
               card_header(strong("School Mapping")),
               leafletOutput("CLMapping", height = 800)
          )
        )
      )
    } else if (selected_resource_type == "Industries") {
      tagList(
        h3("Industries Overview"),
        hr(),
        layout_column_wrap(
          width = 1/3,
          card(
            card_header(strong("Total SHS Count")),
            valueBoxOutput("SHSCountUniv")
          ),
          card(
            card_header(strong("Pilot SHS Count")),
            valueBoxOutput("SHSCount")
          ),
          card(
            card_header(strong("Total Industry Count")),
            valueBoxOutput("IndCount")
          )
        ),
        card(
          card_header("Nearby Industry Count (~10 km radius):"),
          layout_column_wrap(
            width = 1/6,
            card(
              card_header(strong("Manufacturing and Engineering")),
              valueBoxOutput("AccoCount")
            ),
            card(
              card_header(strong("Hospitality and Tourism")),
              valueBoxOutput("ProfCount")
            ),
            card(
              card_header(strong("Public Administration")),
              valueBoxOutput("TranCount")
            ),
            card(
              card_header(strong("Professional/Private Services")),
              valueBoxOutput("WastCount")
            ),
            card(
              card_header(strong("Business and Finance")),
              valueBoxOutput("WholCount")
            ),
            card(
              card_header(strong("Agriculture and Agri-business")),
              valueBoxOutput("WholCount2")
            )
          )),
        layout_columns(
          card(
            card_header(strong("List of SHS")),
            dataTableOutput("SHSListTable")
          ),
          card(full_screen = TRUE,
               card_header(strong("SHS to Industry Mapping")),
               leafletOutput("SHSMapping", height = 700, width = "100%")
          ),
          card(full_screen = TRUE,
               card_header(div(strong("School Profile"),
                               tags$span(em("(Select a school in the table above)"),
                                         style = "font-size: 0.7em; color: grey;")
               )),
               tableOutput("SHSTablex")
          ),
          card(full_screen = TRUE,
               card_header(div(strong("Specialization Data"),
                               tags$span(em("(based on eSF7 for SY 2023-2024)"),
                                         style = "font-size: 0.7em; color: grey;")
               )),
               tableOutput("PilotSpec")
          ),
          card(
            card_header(div(strong("Nearby Industries"),
                            tags$span(em("(Select a school in the table above)"),
                                      style = "font-size: 0.7em; color: grey;")
            )),
            dataTableOutput("dataTableSHS")
          ),
          col_widths = c(4, 8, 6, 6, 12)
        )
      )
    } else if (selected_resource_type == "Facilities") {
      tagList(
        h3("Education Facilities Mapping"),
        layout_columns(
          col_widths = c(6, 6), # Adjust column widths to control horizontal spacing
          selectInput("EFD_Type", "Select Type",
                      choices = c("New Construction","Electrification","Health","QRF","LMS","ALS-CLC","Gabaldon", "Repairs"),
                      selected = "New Construction"
          )
        ),
        input_task_button("Facilities_Refresh", strong("Refresh"), class = "btn-success"),
        hr(),
        layout_columns(
          card(
            full_screen = TRUE,
            card_header(strong("")),
            dataTableOutput("FacTable")
          ),
          card(
            full_screen = TRUE,
            card_header(strong("School Mapping")),
            leafletOutput("FacMapping", height = 800)
          )
        )
      )
    } else if (selected_resource_type == "Learner Congestion") {
      tagList(
        h3("Learner Congestion Mapping (SY 2023-2024)"),
        hr(),
        layout_columns(
          card(
            full_screen = TRUE,
            card_header(strong("")),
            dataTableOutput("CongestTable")
          ),
          card(
            full_screen = TRUE,
            card_header(strong("School Mapping")),
            leafletOutput("CongestMapping", height = 800)
          )
        )
      )
    }
  })
  
  #For Division:
    
    output$dashboard_division_filter<- renderUI({
      filtered_division <- c(df[df$Region==input$dashboard_region_filter,"Division"])
      pickerInput(
        inputId = "Dashboard_SDO",
        label = HTML(paste0("Division ", em(style = "font-size: 0.8em;", "(for Legislative District data):"))),
        choices = filtered_division,
        selected = filtered_division[1], # This will select all choices by default
        multiple = TRUE, # Set to TRUE to enable multiple selections
        options = pickerOptions(
          actionsBox = TRUE,
          liveSearch = TRUE,
          header = "Select one or more Divisions",
          title = "No Divisions Selected",
          selectedTextFormat = "count > 3",
          dropupAuto = FALSE,
          dropup = FALSE
        ),
        choicesOpt = list()
      )
    })
    
    output$cloud_dashboard_division_filter <- renderUI({
      filtered_division <- c(df[df$Region==input$cloud_dashboard_region_filter,"Division"])
      selectInput("cloud_Dashboard_SDO",HTML(paste0("Division ", em(style = "font-size: 0.8em;", "(for Legislative District data):"))), filtered_division, selected = filtered_division[1])})
    
    output$resource_map_division<- renderUI({
      filtered_division <- c(df[df$Region==input$resource_map_region,"Division"])
      selectInput("Resource_SDO","Select a Division:", filtered_division, selected = filtered_division[1])})
    
    output$explorer_division<- renderUI({
      filtered_division <- c(df[df$Region==input$explorer_region,"Division"])
      selectInput("Explorer_SDO","Select a Division:", filtered_division, selected = filtered_division[1])})
    
  #For Legislative District:
    
    output$resource_map_legislative_district <- renderUI({
      filtered_district <- c(df[which(df$Division == input$Resource_SDO), "Legislative.District"])
      selectInput("Resource_LegDist","Select a Legislative District:", filtered_district, selected = filtered_district[1])})
  
  # Reactive value to store uploaded data
  uploaded_data <- reactiveVal(NULL)
  
  # --- Single reactive value for the currently selected dashboard sub-page ---
  # Default to the first option of the new first category ("General Info" -> "School Count")
  current_dashboard_selection <- reactiveVal("general_school_count")
  
  # --- Observers to update the single selection and deselect others (Dashboard) ---
  observeEvent(input$dashboard_general_selector, {
    if (!is.null(input$dashboard_general_selector)) {
      current_dashboard_selection(input$dashboard_general_selector)
      # Deselect other groups
      updateRadioButtons(session, "dashboard_others_selector", selected = character(0))
      updateRadioButtons(session, "dashboard_resource_shortage_selector", selected = character(0))
      updateRadioButtons(session, "dashboard_infrastructure_selector", selected = character(0))
    }
  })
  
  observeEvent(input$dashboard_others_selector, {
    if (!is.null(input$dashboard_others_selector)) {
      current_dashboard_selection(input$dashboard_others_selector)
      # Deselect other groups
      updateRadioButtons(session, "dashboard_general_selector", selected = character(0))
      updateRadioButtons(session, "dashboard_resource_shortage_selector", selected = character(0))
      updateRadioButtons(session, "dashboard_infrastructure_selector", selected = character(0))
    }
  })
  
  observeEvent(input$dashboard_resource_shortage_selector, {
    if (!is.null(input$dashboard_resource_shortage_selector)) {
      current_dashboard_selection(input$dashboard_resource_shortage_selector)
      # Deselect other groups
      updateRadioButtons(session, "dashboard_general_selector", selected = character(0))
      updateRadioButtons(session, "dashboard_others_selector", selected = character(0))
      updateRadioButtons(session, "dashboard_infrastructure_selector", selected = character(0))
    }
  })
  
  observeEvent(input$dashboard_infrastructure_selector, {
    if (!is.null(input$dashboard_infrastructure_selector)) {
      current_dashboard_selection(input$dashboard_infrastructure_selector)
      # Deselect other groups
      updateRadioButtons(session, "dashboard_general_selector", selected = character(0))
      updateRadioButtons(session, "dashboard_others_selector", selected = character(0))
      updateRadioButtons(session, "dashboard_resource_shortage_selector", selected = character(0))
    }
  })
  
  # --- Dashboard Structure ---
  # Inside your server function:
  output$dashboard_main_content_area <- renderUI({
    req(input$hrod_main_category_picker)
    
    switch(input$hrod_main_category_picker,
           "general_school_count" = {
             tagList(
               h3("School Count Overview"),
               hr(),
               layout_column_wrap(
                 width = 1/6,
                 value_box(title = "Purely ES", value = "35,036"),
                 value_box(title = "JHS with SHS", value = "6,598"),
                 value_box(title = "ES and JHS (K to 10)", value = "1,690"),
                 value_box(title = "Purely JHS", value = "1,367"),
                 value_box(title = "All Offering (K to 12)", value = "832"),
                 value_box(title = "Purely SHS", value = "262")
               ),
               layout_columns(
                 # Adjusted col_widths for the three plots/tables to be side-by-side
                 col_widths = c(12,6,6,12), # Assuming you want 3 columns for these
                 #Uncomment and adjust if you need a national data table as a plotly table
                  # Changed to plotlyOutput
                 card(full_screen = TRUE,
                      card_header(strong("Regional School Count Data"),
                                  plotlyOutput("school_count_regional_graph", height = 500))),
                 card(full_screen = TRUE,
                      card_header(strong("SDO School Count Data"),
                                  plotlyOutput("school_count_division_graph", height = 500))),
                 card(full_screen = TRUE,
                      card_header(strong("Legislative District School Count Data"),
                                  plotlyOutput("school_count_district_graph", height = 500))),
                 card(full_screen = TRUE,
                      card_header(strong("School Database"),
                                  dataTableOutput("school_count_data_table")))# This is the target for interaction
               )
             )
           },
           "resource_shortage_classroom" = {
             tagList(
               h3("Classroom Shortage Overview"),
               hr(),
               layout_column_wrap(
                 width = 1/3,
                 value_box(title = "National Classroom Shortage", value = "165,443", showcase = bs_icon("building"))
               ),
               layout_columns(
                 col_widths = c(12,6,6,12), # Keep this as is for now, but consider adjusting for better layout if needed
                 # Uncomment and adjust if you need a national data table as a plotly table
                 card(full_screen = TRUE,
                      card_header(strong("Regional Classroom Shortage Data"),
                                  plotlyOutput("Classroom_Shortage_Region_Graph", height = 500))),
                 card(full_screen = TRUE,
                      card_header(strong("SDO Classroom Shortage Data"),
                                  plotlyOutput("Classroom_Shortage_Division_Graph", height = 500))),
                 card(full_screen = TRUE,
                      card_header(strong("Legislative District Classroom Shortage Data"),
                                  plotlyOutput("Classroom_Shortage_District_Graph", height = 500))),
                 card(full_screen = TRUE,
                      card_header(strong("School Database"),
                                  dataTableOutput("Classroom_Shortage_All_List"))) # Changed to plotlyOutput
               )
             )
           },
           "resource_lms" = {
             tagList(
               h3("Last Mile Schools Overview (NSBI SY 2023-2024)"),
               hr(),
               layout_columns(
                 col_widths = c(12,12,12),
                 # Uncomment and adjust if you need a national data table as a plotly table
                 card(full_screen = TRUE,
                      card_header(strong("Regional LMS Data"),
                                  plotlyOutput("LMS_Nation_Graph", height = 500))),
                 card(full_screen = TRUE,
                      card_header(strong("SDO LMS Data"),
                                  plotlyOutput("LMS_Division_Graph", height = 500))),
                 # card(full_screen = TRUE,
                 #      card_header(strong("Legislative District Classroom Shortage Data"),
                 #                  plotlyOutput("Classroom_Shortage_District_Graph", height = 500))),
                 card(full_screen = TRUE,
                      card_header(strong("School Database"),
                                  dataTableOutput("LMS_All_List"))) # Changed to plotlyOutput
               )
             )
           },
           "general_sosss" = {
             tagList(
               h3("School Organization and Staffing Standards (SOSSS)"),
               hr(),
               layout_column_wrap(
                 width = 1/7,
                 value_box(title = "Very Small", value = "24,976"),
                 value_box(title = "Small", value = "10,105"),
                 value_box(title = "Medium", value = "5,726"),
                 value_box(title = "Large", value = "4,210"),
                 value_box(title = "Very Large", value = "727"),
                 value_box(title = "Extremely Large", value = "38"),
                 value_box(title = "Mega", value = "3")
               ),
               layout_columns(
                 col_widths = c(12,6,6,12), # Keep as is
                 # Uncomment and adjust if you need a national data table as a plotly table
                 card(full_screen = TRUE,
                      card_header(strong("Regional SOSSS Typology Data"),
                                  plotlyOutput("SOSSS_Region_Typology", height = 500))),
                 card(full_screen = TRUE,
                      card_header(strong("SDO SOSSS Typology Data"),
                                  plotlyOutput("SOSSS_Division_Typology", height = 500))),
                 card(full_screen = TRUE,
                      card_header(strong("Legislative District SOSSS Typology Data"),
                                  plotlyOutput("SOSSS_District_Typology", height = 500))),
                 card(full_screen = TRUE,
                      card_header(strong("School Database"),
                                  dataTableOutput("SOSSS_All_List_Typology"))) # Changed to plotlyOutput
               )
             )
           },
           "resource_shortage_principal" = {
             tagList(
               h3("School Principal Shortage Overview"),
               hr(),
               layout_column_wrap(
                 width = 1/3,
                 value_box(title = "School Principal", value = "21,781", showcase = bs_icon("person-badge")),
                 value_box(title = "Teacher-in-Charge", value = "23,370", showcase = bs_icon("exclamation-circle")),
                 value_box(title = "Officer-in-Charge", value = "177", showcase = bs_icon("person-fill-add"))
               ),
               layout_columns(
                 col_widths = c(12,6,6,12),
                 card(full_screen = TRUE,
                      card_header(strong("Regional School Principal Shortage Data"),
                                  plotlyOutput("School_Principal_Regional_Graph", height = 500))),
                 card(full_screen = TRUE,
                      card_header(strong("SDO School Principal Shortage Data"),
                                  plotlyOutput("School_Principal_Division_Graph", height = 500))),
                 card(full_screen = TRUE,
                      card_header(strong("Legislative District School Principal Shortage Data"),
                                  plotlyOutput("School_Principal_District_Graph", height = 500))),
                 card(full_screen = TRUE,
                      card_header(strong("School Database"),
                                  dataTableOutput("School_Principal_All_List")))),
             )
           },
           "resource_shortage_teacher" = {
             tagList(
               h3("Teacher Shortage Overview"),
               hr(),
               layout_column_wrap(
                 width = 1/5,
                 value_box(title = "Total Teacher Shortage", value = "32,916", showcase = bs_icon("person-vcard"), theme = "danger"),
                 value_box(title = "ES Shortage", value = "22,023", showcase = bs_icon("exclamation-circle")),
                 value_box(title = "JHS Shortage", value = "9,302", showcase = bs_icon("exclamation-circle")),
                 value_box(title = "SHS Shortage", value = "1,591", showcase = bs_icon("exclamation-circle")),
                 value_box(title = "Total Schools with Teacher Shortage", value = "26,102", showcase = bs_icon("person-vcard"), theme = "danger")
               ),
               layout_columns(
                 col_widths = c(12,12,12), # Keep as is
                 # Uncomment and adjust if you need a national data table as a plotly table
                 card(full_screen = TRUE,
                      card_header(strong("Regional Teacher Shortage Data"),
                                  plotlyOutput("Teacher_Shortage_Regional_Graph", height = 500))),
                 card(full_screen = TRUE,
                      card_header(strong("SDO Teacher Shortage Data"),
                                  plotlyOutput("Teacher_Shortage_Division_Graph", height = 500))),
                 card(full_screen = TRUE,
                      card_header(strong("SDO Database"),
                                  dataTableOutput("Teacher_Shortage_Regional_Table", height = 500))),
               )
             )
           },
           "resource_shortage_non_teaching" = {
             tagList(
               h3("AO II Shortage Overview"),
               hr(),
               layout_column_wrap(
                 width = 1/3,
                 value_box(title = "Dedicated", value = "12,734", showcase = bs_icon("person-lines-fill")),
                 value_box(title = "Clustered", value = "20,169", showcase = bs_icon("exclamation-circle")),
                 value_box(title = "None Deployed", value = "12,425", showcase = bs_icon("clipboard2-pulse"), theme = "danger")
               ),
               layout_columns(
                 col_widths = c(12,6,6,12), # Keep as is
                 # Uncomment and adjust if you need a national data table as a plotly table
                 card(full_screen = TRUE,
                      card_header(strong("Regional AO II Deployment Data"),
                                  plotlyOutput("AOII_Regional_Graph", height = 500))),
                 card(full_screen = TRUE,
                      card_header(strong("SDO AO II Deployment Data"),
                                  plotlyOutput("AOII_Division_Graph", height = 500))),
                 card(full_screen = TRUE,
                      card_header(strong("Legislative District AO II Deployment Data"),
                                  plotlyOutput("AOII_District_Graph", height = 500))),
                 card(full_screen = TRUE,
                      card_header(strong("School Database"),
                                  dataTableOutput("AOII_Data_Table"))), # Changed to plotlyOutput
               ),
               h3("PDO I Shortage Overview"),
               hr(),
               layout_column_wrap(
                 width = 1/2,
                 value_box(title = "With PDO I", value = "1500", showcase = bs_icon("person-lines-fill")),
                 value_box(title = "Without PDO I", value = "43,828", showcase = bs_icon("exclamation-circle"), theme = "danger")
               ),
               layout_columns(
                 col_widths = c(12,6,6,12), # Keep as is
                 # Uncomment and adjust if you need a national data table as a plotly table
                 card(full_screen = TRUE,
                      card_header(strong("Regional PDO I Deployment Data"),
                                  plotlyOutput("PDOI_Regional_Graph", height = 500))),
                 card(full_screen = TRUE,
                      card_header(strong("SDO PDO I Deployment Data"),
                                  plotlyOutput("PDOI_Division_Graph", height = 500))),
                 card(full_screen = TRUE,
                      card_header(strong("Legislative District PDO I Deployment Data"),
                                  plotlyOutput("PDOI_District_Graph", height = 500))),
                 card(full_screen = TRUE,
                      card_header(strong("School Database"),
                                  dataTableOutput("PDOI_Data_Table"))), # Changed to plotlyOutput
               )
             )
           },
           "others_sufficiency" = {
             tagList(
               h3("Sufficiency Overview"),
               hr(),
               layout_columns(
                 col_widths = c(12,12,12), # Keep as is
                 card(full_screen = TRUE,
                      card_header(strong("Regional Sufficiency Data"),
                                  selectInput("SuffOpt","Select a Category:", multiple = FALSE, selected = "Teacher.Sufficiency", choices = c("Teacher Sufficiency" = "Teacher.Sufficiency","Classroom Sufficiency" = "Classroom.Sufficiency","School Principal Sufficiency" = "SH.Sufficiency", "AO Sufficiency" = "AO.Sufficiency")),
                                  plotlyOutput("Sufficiency_Regional_Graph", height = 500))),
                 card(full_screen = TRUE,
                      card_header(strong("SDO Sufficiency Data"),
                                  selectInput("SuffOpt","Select a Category:", multiple = FALSE, selected = "Teacher.Sufficiency", choices = c("Teacher Sufficiency" = "Teacher.Sufficiency","Classroom Sufficiency" = "Classroom.Sufficiency","School Principal Sufficiency" = "SH.Sufficiency", "AO Sufficiency" = "AO.Sufficiency")),
                                  plotlyOutput("Sufficiency_Division_Graph", height = 500))),
                 card(full_screen = TRUE,
                      card_header(strong("School Database"),
                                  dataTableOutput("Sufficiency_All_List"))) # Changed to plotlyOutput
               )
             )
           }
    )
  })
  
  ### CLOUD COMPARE SERVER ###
  
  ### CLOUD SERVER ###
  
  output$cloud_graph_1 <- renderUI({
    req(input$cloud_category_picker_1)
    
    switch(input$cloud_category_picker_1,
           "cloud_enrolment" = {
             tagList(
               layout_columns(
                 card(full_screen = TRUE,
                      card_header(strong("Regional Data"),
                                  plotlyOutput("enrolment_regional_graph", height = 500))))
               )
           },
           "cloud_sned" = {
             tagList(
               layout_columns(
                 card(full_screen = TRUE,
                      card_header(strong("Regional Data"),
                                  plotlyOutput("sned_regional_graph", height = 500)))
               )
             )
           },
           "cloud_ip" = {
             tagList(
               layout_columns(
                 card(full_screen = TRUE,
                      card_header(strong("Regional Data"),
                                  plotlyOutput("ip_regional_graph", height = 500)))
               )
             )
           },
           "cloud_muslim" = {
             tagList(
               layout_columns(
                 card(full_screen = TRUE,
                      card_header(strong("Regional Data"),
                                  plotlyOutput("muslim_regional_graph", height = 500)))
               )
             )
           },
           "cloud_displaced" = {
             tagList(
               layout_columns(
                 card(full_screen = TRUE,
                      card_header(strong("Regional Data"),
                                  plotlyOutput("displaced_regional_graph", height = 500)))
               )
             )
           },
           "cloud_als" = {
             tagList(
               layout_columns(
                 card(full_screen = TRUE,
                      card_header(strong("Regional Data"),
                                  plotlyOutput("als_regional_graph", height = 500)))
               )
             )
           },
           "cloud_dropout" = {
             tagList(
               layout_columns(
                 card(full_screen = TRUE,
                      card_header(strong("Regional Data"),
                                  plotlyOutput("dropout_regional_graph", height = 500)))
               )
             )
           },
           "cloud_teacherinventory" = {
             tagList(
               layout_columns(
                 card(full_screen = TRUE,
                      card_header(strong("Regional  Data"),
                                  plotlyOutput("teacherinventory_regional_graph", height = 500)))
               )
             )
           },
           "cloud_classroom" = {
             tagList(
               layout_columns(
                 card(full_screen = TRUE,
                      card_header(strong("Regional  Data"),
                                  plotlyOutput("classroom_regional_graph", height = 500)))
               )
             )
           },
           "cloud_multigrade" = {
             tagList(
               layout_columns(
                 card(full_screen = TRUE,
                      card_header(strong("Regional  Data"),
                                  plotlyOutput("multigrade_regional_graph", height = 500)))
               )
             )
           },
           "cloud_organizedclass" = {
             tagList(
               layout_columns(
                 card(full_screen = TRUE,
                      card_header(strong("Regional  Data"),
                                  plotlyOutput("organizedclass_regional_graph", height = 500)))
               )
             )
           },
           "cloud_jhsdeployment" = {
             tagList(
               layout_columns(
                 card(full_screen = TRUE,
                      card_header(strong("Regional  Data"),
                                  plotlyOutput("jhsdeployment_regional_graph", height = 500)))
               )
             )
           },
           "cloud_shifting" = {
             tagList(
               layout_columns(
                 card(full_screen = TRUE,
                      card_header(strong("Regional  Data"),
                                  plotlyOutput("shifting_regional_graph", height = 500)))
               )
             )
           },
           "cloud_LDM" = {
             tagList(
               layout_columns(
                 card(full_screen = TRUE,
                      card_header(strong("Regional Data"),
                                  plotlyOutput("ldm_regional_graph", height = 500)))
               )
             )
           },
           "cloud_adm" = {
             tagList(
               layout_columns(
                 card(full_screen = TRUE,
                      card_header(strong("Regional Data"),
                                  plotlyOutput("adm_regional_graph", height = 500)))
               )
             )
           },
           "cloud_ARAL" = {
             tagList(
               layout_columns(
                 card(full_screen = TRUE,
                      card_header(strong("Regional  Data"),
                                  plotlyOutput("aral_regional_graph", height = 500)))
               )
             )
           },
           "cloud_crla" = {
             tagList(
               layout_columns(
                 card(full_screen = TRUE,
                      card_header(strong("Regional  Data"),
                                  plotlyOutput("crla_regional_graph", height = 500)))
               )
             )
           },
           "cloud_philiri" = {
             tagList(
               layout_columns(
                 card(full_screen = TRUE,
                      card_header(strong("Regional  Data"),
                                  plotlyOutput("philiri_regional_graph", height = 500)))
               )
             )
           },
           "cloud_initiatives" = {
             tagList(
               layout_columns(
                 card(full_screen = TRUE,
                      card_header(strong("Regional  Data"),
                                  plotlyOutput("initiatives_regional_graph", height = 500)))
               )
             )
           },
           "cloud_moi" = {
             tagList(
               layout_columns(
                 card(full_screen = TRUE,
                      card_header(strong("Regional  Data"),
                                  plotlyOutput("moi_regional_graph", height = 500)))
               )
             )
           },
           "cloud_sosss" = {
             tagList(
               layout_columns(
                 card(full_screen = TRUE,
                      card_header(strong("Regional  Data"),
                                  plotlyOutput("sosss_regional_graph", height = 500)))
               )
             )
           },
           "cloud_shsimplem" = {
             tagList(
               layout_columns(
                 card(full_screen = TRUE,
                      card_header(strong("Regional  Data"),
                                  plotlyOutput("shsimplem_regional_graph", height = 500)))
               )
             )
           },
           "cloud_overload" = {
             tagList(
               layout_columns(
                 card(full_screen = TRUE,
                      card_header(strong("Regional  Data"),
                                  plotlyOutput("overload_regional_graph", height = 500)))
               )
             )
           },
           "cloud_resources" = {
             tagList(
               layout_columns(
                 card(full_screen = TRUE,
                      card_header(strong("Regional  Data"),
                                  plotlyOutput("resources_regional_graph", height = 500)))
               )
             )
           },
           "cloud_nat" = {
             tagList(
               layout_columns(
                 card(full_screen = TRUE,
                      card_header(strong("Regional  Data"),
                                  plotlyOutput("nat_regional_graph", height = 500)))
               )
             )
           },
           "cloud_nat_sufficiency" = {
             tagList(
               layout_columns(
                 card(full_screen = TRUE,
                      card_header(strong("Regional  Data"),
                                  plotlyOutput("nat_sufficiency_regional_graph", height = 500)))
               )
             )
           },
           "cloud_rf" = {
             tagList(
               layout_columns(
                 card(full_screen = TRUE,
                      card_header(strong("Regional  Data"),
                                  plotlyOutput("rf_regional_graph", height = 500)))
               )
             )
           },
           "cloud_lac" = {
             tagList(
               layout_columns(
                 card(full_screen = TRUE,
                      card_header(strong("Regional  Data"),
                                  plotlyOutput("lac_regional_graph", height = 500)))
               )
             )
           },
           "cloud_feeding" = {
             tagList(
               layout_columns(
                 card(full_screen = TRUE,
                      card_header(strong("Regional  Data"),
                                  plotlyOutput("feeding_regional_graph", height = 500)))
               )
             )
           },
           "cloud_sha" = {
             tagList(
               layout_columns(
                 card(full_screen = TRUE,
                      card_header(strong("Regional  Data"),
                                  plotlyOutput("sha_regional_graph", height = 500)))
               )
             )
           },
           "cloud_childprotection" = {
             tagList(
               layout_columns(
                 card(full_screen = TRUE,
                      card_header(strong("Regional  Data"),
                                  plotlyOutput("childprotection_regional_graph", height = 500)))
               )
             )
           },
           "cloud_support" = {
             tagList(
               layout_columns(
                 card(full_screen = TRUE,
                      card_header(strong("Regional  Data"),
                                  plotlyOutput("support_regional_graph", height = 500)))
               )
             )
           },
           "cloud_extension" = {
             tagList(
               layout_columns(
                 card(full_screen = TRUE,
                      card_header(strong("Regional  Data"),
                                  plotlyOutput("extension_regional_graph", height = 500)))
               )
             )
           },
           "cloud_extension" = {
             tagList(
               layout_columns(
                 card(full_screen = TRUE,
                      card_header(strong("Regional  Data"),
                                  plotlyOutput("school_count_regional_graph", height = 500)))
               )
             )
           },
           "cloud_years" = {
             tagList(
               layout_columns(
                 # Changed to plotlyOutput
                 card(full_screen = TRUE,
                      card_header(strong("Regional Data"),
                                  plotlyOutput("years_regional_graph", height = 500)))
               )
             )
           },
           "cloud_elec" = {
             tagList(
               layout_columns(
                 card(full_screen = TRUE,
                      card_header(strong("Regional Data"),
                                  plotlyOutput("elec_regional_graph", height = 500)))
               )
             )
           },
           "cloud_water" = {
             tagList(
               layout_columns(
                 card(full_screen = TRUE,
                      card_header(strong("Regional Data"),
                                  plotlyOutput("water_regional_graph", height = 500)))
               )
             )
           },
           "cloud_internet" = {
             tagList(
               layout_columns(
                 card(full_screen = TRUE,
                      card_header(strong("Regional Data"),
                                  plotlyOutput("internet_regional_graph", height = 500)))
               )
             )
           },
           "cloud_internet_usage" = {
             tagList(
               layout_columns(
                 card(full_screen = TRUE,
                      card_header(strong("Regional Data"),
                                  plotlyOutput("internet_usage_regional_graph", height = 500)))
               )
             )
           },
           "cloud_bully" = {
             tagList(
               layout_columns(
                 card(full_screen = TRUE,
                      card_header(strong("Regional Data"),
                                  plotlyOutput("bully_regional_graph", height = 500)))
               )
             )
           }
           
    )
  })
  
  output$cloud_graph_2 <- renderUI({
    req(input$cloud_category_picker_2)
    
    switch(input$cloud_category_picker_2,
           "cloud_enrolment" = {
             tagList(
               layout_columns(
                 card(full_screen = TRUE,
                      card_header(strong("Regional Data"),
                                  plotlyOutput("enrolment_regional_graph", height = 500))))
             )
           },
           "cloud_sned" = {
             tagList(
               layout_columns(
                 card(full_screen = TRUE,
                      card_header(strong("Regional Data"),
                                  plotlyOutput("sned_regional_graph", height = 500)))
               )
             )
           },
           "cloud_ip" = {
             tagList(
               layout_columns(
                 card(full_screen = TRUE,
                      card_header(strong("Regional Data"),
                                  plotlyOutput("ip_regional_graph", height = 500)))
               )
             )
           },
           "cloud_muslim" = {
             tagList(
               layout_columns(
                 card(full_screen = TRUE,
                      card_header(strong("Regional Data"),
                                  plotlyOutput("muslim_regional_graph", height = 500)))
               )
             )
           },
           "cloud_displaced" = {
             tagList(
               layout_columns(
                 card(full_screen = TRUE,
                      card_header(strong("Regional Data"),
                                  plotlyOutput("displaced_regional_graph", height = 500)))
               )
             )
           },
           "cloud_als" = {
             tagList(
               layout_columns(
                 card(full_screen = TRUE,
                      card_header(strong("Regional Data"),
                                  plotlyOutput("als_regional_graph", height = 500)))
               )
             )
           },
           "cloud_dropout" = {
             tagList(
               layout_columns(
                 card(full_screen = TRUE,
                      card_header(strong("Regional Data"),
                                  plotlyOutput("dropout_regional_graph", height = 500)))
               )
             )
           },
           "cloud_teacherinventory" = {
             tagList(
               layout_columns(
                 card(full_screen = TRUE,
                      card_header(strong("Regional  Data"),
                                  plotlyOutput("teacherinventory_regional_graph", height = 500)))
               )
             )
           },
           "cloud_classroom" = {
             tagList(
               layout_columns(
                 card(full_screen = TRUE,
                      card_header(strong("Regional  Data"),
                                  plotlyOutput("classroom_regional_graph", height = 500)))
               )
             )
           },
           "cloud_multigrade" = {
             tagList(
               layout_columns(
                 card(full_screen = TRUE,
                      card_header(strong("Regional  Data"),
                                  plotlyOutput("multigrade_regional_graph", height = 500)))
               )
             )
           },
           "cloud_organizedclass" = {
             tagList(
               layout_columns(
                 card(full_screen = TRUE,
                      card_header(strong("Regional  Data"),
                                  plotlyOutput("organizedclass_regional_graph", height = 500)))
               )
             )
           },
           "cloud_jhsdeployment" = {
             tagList(
               layout_columns(
                 card(full_screen = TRUE,
                      card_header(strong("Regional  Data"),
                                  plotlyOutput("jhsdeployment_regional_graph", height = 500)))
               )
             )
           },
           "cloud_shifting" = {
             tagList(
               layout_columns(
                 card(full_screen = TRUE,
                      card_header(strong("Regional  Data"),
                                  plotlyOutput("shifting_regional_graph", height = 500)))
               )
             )
           },
           "cloud_LDM" = {
             tagList(
               layout_columns(
                 card(full_screen = TRUE,
                      card_header(strong("Regional Data"),
                                  plotlyOutput("ldm_regional_graph", height = 500)))
               )
             )
           },
           "cloud_adm" = {
             tagList(
               layout_columns(
                 card(full_screen = TRUE,
                      card_header(strong("Regional Data"),
                                  plotlyOutput("adm_regional_graph", height = 500)))
               )
             )
           },
           "cloud_ARAL" = {
             tagList(
               layout_columns(
                 card(full_screen = TRUE,
                      card_header(strong("Regional  Data"),
                                  plotlyOutput("aral_regional_graph", height = 500)))
               )
             )
           },
           "cloud_crla" = {
             tagList(
               layout_columns(
                 card(full_screen = TRUE,
                      card_header(strong("Regional  Data"),
                                  plotlyOutput("crla_regional_graph", height = 500)))
               )
             )
           },
           "cloud_philiri" = {
             tagList(
               layout_columns(
                 card(full_screen = TRUE,
                      card_header(strong("Regional  Data"),
                                  plotlyOutput("philiri_regional_graph", height = 500)))
               )
             )
           },
           "cloud_initiatives" = {
             tagList(
               layout_columns(
                 card(full_screen = TRUE,
                      card_header(strong("Regional  Data"),
                                  plotlyOutput("initiatives_regional_graph", height = 500)))
               )
             )
           },
           "cloud_moi" = {
             tagList(
               layout_columns(
                 card(full_screen = TRUE,
                      card_header(strong("Regional  Data"),
                                  plotlyOutput("moi_regional_graph", height = 500)))
               )
             )
           },
           "cloud_sosss" = {
             tagList(
               layout_columns(
                 card(full_screen = TRUE,
                      card_header(strong("Regional  Data"),
                                  plotlyOutput("sosss_regional_graph", height = 500)))
               )
             )
           },
           "cloud_shsimplem" = {
             tagList(
               layout_columns(
                 card(full_screen = TRUE,
                      card_header(strong("Regional  Data"),
                                  plotlyOutput("shsimplem_regional_graph", height = 500)))
               )
             )
           },
           "cloud_overload" = {
             tagList(
               layout_columns(
                 card(full_screen = TRUE,
                      card_header(strong("Regional  Data"),
                                  plotlyOutput("overload_regional_graph", height = 500)))
               )
             )
           },
           "cloud_resources" = {
             tagList(
               layout_columns(
                 card(full_screen = TRUE,
                      card_header(strong("Regional  Data"),
                                  plotlyOutput("resources_regional_graph", height = 500)))
               )
             )
           },
           "cloud_nat" = {
             tagList(
               layout_columns(
                 card(full_screen = TRUE,
                      card_header(strong("Regional  Data"),
                                  plotlyOutput("nat_regional_graph", height = 500)))
               )
             )
           },
           "cloud_nat_sufficiency" = {
             tagList(
               layout_columns(
                 card(full_screen = TRUE,
                      card_header(strong("Regional  Data"),
                                  plotlyOutput("nat_sufficiency_regional_graph", height = 500)))
               )
             )
           },
           "cloud_rf" = {
             tagList(
               layout_columns(
                 card(full_screen = TRUE,
                      card_header(strong("Regional  Data"),
                                  plotlyOutput("rf_regional_graph", height = 500)))
               )
             )
           },
           "cloud_lac" = {
             tagList(
               layout_columns(
                 card(full_screen = TRUE,
                      card_header(strong("Regional  Data"),
                                  plotlyOutput("lac_regional_graph", height = 500)))
               )
             )
           },
           "cloud_feeding" = {
             tagList(
               layout_columns(
                 card(full_screen = TRUE,
                      card_header(strong("Regional  Data"),
                                  plotlyOutput("feeding_regional_graph", height = 500)))
               )
             )
           },
           "cloud_sha" = {
             tagList(
               layout_columns(
                 card(full_screen = TRUE,
                      card_header(strong("Regional  Data"),
                                  plotlyOutput("sha_regional_graph", height = 500)))
               )
             )
           },
           "cloud_childprotection" = {
             tagList(
               layout_columns(
                 card(full_screen = TRUE,
                      card_header(strong("Regional  Data"),
                                  plotlyOutput("childprotection_regional_graph", height = 500)))
               )
             )
           },
           "cloud_support" = {
             tagList(
               layout_columns(
                 card(full_screen = TRUE,
                      card_header(strong("Regional  Data"),
                                  plotlyOutput("support_regional_graph", height = 500)))
               )
             )
           },
           "cloud_extension" = {
             tagList(
               layout_columns(
                 card(full_screen = TRUE,
                      card_header(strong("Regional  Data"),
                                  plotlyOutput("extension_regional_graph", height = 500)))
               )
             )
           },
           "cloud_extension" = {
             tagList(
               layout_columns(
                 card(full_screen = TRUE,
                      card_header(strong("Regional  Data"),
                                  plotlyOutput("school_count_regional_graph", height = 500)))
               )
             )
           },
           "cloud_years" = {
             tagList(
               layout_columns(
                 # Changed to plotlyOutput
                 card(full_screen = TRUE,
                      card_header(strong("Regional Data"),
                                  plotlyOutput("years_regional_graph", height = 500)))
               )
             )
           },
           "cloud_elec" = {
             tagList(
               layout_columns(
                 card(full_screen = TRUE,
                      card_header(strong("Regional Data"),
                                  plotlyOutput("elec_regional_graph", height = 500)))
               )
             )
           },
           "cloud_water" = {
             tagList(
               layout_columns(
                 card(full_screen = TRUE,
                      card_header(strong("Regional Data"),
                                  plotlyOutput("water_regional_graph", height = 500)))
               )
             )
           },
           "cloud_internet" = {
             tagList(
               layout_columns(
                 card(full_screen = TRUE,
                      card_header(strong("Regional Data"),
                                  plotlyOutput("internet_regional_graph", height = 500)))
               )
             )
           },
           "cloud_internet_usage" = {
             tagList(
               layout_columns(
                 card(full_screen = TRUE,
                      card_header(strong("Regional Data"),
                                  plotlyOutput("internet_usage_regional_graph", height = 500)))
               )
             )
           },
           "cloud_bully" = {
             tagList(
               layout_columns(
                 card(full_screen = TRUE,
                      card_header(strong("Regional Data"),
                                  plotlyOutput("bully_regional_graph", height = 500)))
               )
             )
           }
           
    )
  })
  
  output$cloud_graph_3 <- renderUI({
    req(input$cloud_category_picker_3)
    
    switch(input$cloud_category_picker_3,
           "cloud_enrolment" = {
             tagList(
               layout_columns(
                 card(full_screen = TRUE,
                      card_header(strong("Regional Data"),
                                  plotlyOutput("enrolment_regional_graph", height = 500))))
             )
           },
           "cloud_sned" = {
             tagList(
               layout_columns(
                 card(full_screen = TRUE,
                      card_header(strong("Regional Data"),
                                  plotlyOutput("sned_regional_graph", height = 500)))
               )
             )
           },
           "cloud_ip" = {
             tagList(
               layout_columns(
                 card(full_screen = TRUE,
                      card_header(strong("Regional Data"),
                                  plotlyOutput("ip_regional_graph", height = 500)))
               )
             )
           },
           "cloud_muslim" = {
             tagList(
               layout_columns(
                 card(full_screen = TRUE,
                      card_header(strong("Regional Data"),
                                  plotlyOutput("muslim_regional_graph", height = 500)))
               )
             )
           },
           "cloud_displaced" = {
             tagList(
               layout_columns(
                 card(full_screen = TRUE,
                      card_header(strong("Regional Data"),
                                  plotlyOutput("displaced_regional_graph", height = 500)))
               )
             )
           },
           "cloud_als" = {
             tagList(
               layout_columns(
                 card(full_screen = TRUE,
                      card_header(strong("Regional Data"),
                                  plotlyOutput("als_regional_graph", height = 500)))
               )
             )
           },
           "cloud_dropout" = {
             tagList(
               layout_columns(
                 card(full_screen = TRUE,
                      card_header(strong("Regional Data"),
                                  plotlyOutput("dropout_regional_graph", height = 500)))
               )
             )
           },
           "cloud_teacherinventory" = {
             tagList(
               layout_columns(
                 card(full_screen = TRUE,
                      card_header(strong("Regional  Data"),
                                  plotlyOutput("teacherinventory_regional_graph", height = 500)))
               )
             )
           },
           "cloud_classroom" = {
             tagList(
               layout_columns(
                 card(full_screen = TRUE,
                      card_header(strong("Regional  Data"),
                                  plotlyOutput("classroom_regional_graph", height = 500)))
               )
             )
           },
           "cloud_multigrade" = {
             tagList(
               layout_columns(
                 card(full_screen = TRUE,
                      card_header(strong("Regional  Data"),
                                  plotlyOutput("multigrade_regional_graph", height = 500)))
               )
             )
           },
           "cloud_organizedclass" = {
             tagList(
               layout_columns(
                 card(full_screen = TRUE,
                      card_header(strong("Regional  Data"),
                                  plotlyOutput("organizedclass_regional_graph", height = 500)))
               )
             )
           },
           "cloud_jhsdeployment" = {
             tagList(
               layout_columns(
                 card(full_screen = TRUE,
                      card_header(strong("Regional  Data"),
                                  plotlyOutput("jhsdeployment_regional_graph", height = 500)))
               )
             )
           },
           "cloud_shifting" = {
             tagList(
               layout_columns(
                 card(full_screen = TRUE,
                      card_header(strong("Regional  Data"),
                                  plotlyOutput("shifting_regional_graph", height = 500)))
               )
             )
           },
           "cloud_LDM" = {
             tagList(
               layout_columns(
                 card(full_screen = TRUE,
                      card_header(strong("Regional Data"),
                                  plotlyOutput("ldm_regional_graph", height = 500)))
               )
             )
           },
           "cloud_adm" = {
             tagList(
               layout_columns(
                 card(full_screen = TRUE,
                      card_header(strong("Regional Data"),
                                  plotlyOutput("adm_regional_graph", height = 500)))
               )
             )
           },
           "cloud_ARAL" = {
             tagList(
               layout_columns(
                 card(full_screen = TRUE,
                      card_header(strong("Regional  Data"),
                                  plotlyOutput("aral_regional_graph", height = 500)))
               )
             )
           },
           "cloud_crla" = {
             tagList(
               layout_columns(
                 card(full_screen = TRUE,
                      card_header(strong("Regional  Data"),
                                  plotlyOutput("crla_regional_graph", height = 500)))
               )
             )
           },
           "cloud_philiri" = {
             tagList(
               layout_columns(
                 card(full_screen = TRUE,
                      card_header(strong("Regional  Data"),
                                  plotlyOutput("philiri_regional_graph", height = 500)))
               )
             )
           },
           "cloud_initiatives" = {
             tagList(
               layout_columns(
                 card(full_screen = TRUE,
                      card_header(strong("Regional  Data"),
                                  plotlyOutput("initiatives_regional_graph", height = 500)))
               )
             )
           },
           "cloud_moi" = {
             tagList(
               layout_columns(
                 card(full_screen = TRUE,
                      card_header(strong("Regional  Data"),
                                  plotlyOutput("moi_regional_graph", height = 500)))
               )
             )
           },
           "cloud_sosss" = {
             tagList(
               layout_columns(
                 card(full_screen = TRUE,
                      card_header(strong("Regional  Data"),
                                  plotlyOutput("sosss_regional_graph", height = 500)))
               )
             )
           },
           "cloud_shsimplem" = {
             tagList(
               layout_columns(
                 card(full_screen = TRUE,
                      card_header(strong("Regional  Data"),
                                  plotlyOutput("shsimplem_regional_graph", height = 500)))
               )
             )
           },
           "cloud_overload" = {
             tagList(
               layout_columns(
                 card(full_screen = TRUE,
                      card_header(strong("Regional  Data"),
                                  plotlyOutput("overload_regional_graph", height = 500)))
               )
             )
           },
           "cloud_resources" = {
             tagList(
               layout_columns(
                 card(full_screen = TRUE,
                      card_header(strong("Regional  Data"),
                                  plotlyOutput("resources_regional_graph", height = 500)))
               )
             )
           },
           "cloud_nat" = {
             tagList(
               layout_columns(
                 card(full_screen = TRUE,
                      card_header(strong("Regional  Data"),
                                  plotlyOutput("nat_regional_graph", height = 500)))
               )
             )
           },
           "cloud_nat_sufficiency" = {
             tagList(
               layout_columns(
                 card(full_screen = TRUE,
                      card_header(strong("Regional  Data"),
                                  plotlyOutput("nat_sufficiency_regional_graph", height = 500)))
               )
             )
           },
           "cloud_rf" = {
             tagList(
               layout_columns(
                 card(full_screen = TRUE,
                      card_header(strong("Regional  Data"),
                                  plotlyOutput("rf_regional_graph", height = 500)))
               )
             )
           },
           "cloud_lac" = {
             tagList(
               layout_columns(
                 card(full_screen = TRUE,
                      card_header(strong("Regional  Data"),
                                  plotlyOutput("lac_regional_graph", height = 500)))
               )
             )
           },
           "cloud_feeding" = {
             tagList(
               layout_columns(
                 card(full_screen = TRUE,
                      card_header(strong("Regional  Data"),
                                  plotlyOutput("feeding_regional_graph", height = 500)))
               )
             )
           },
           "cloud_sha" = {
             tagList(
               layout_columns(
                 card(full_screen = TRUE,
                      card_header(strong("Regional  Data"),
                                  plotlyOutput("sha_regional_graph", height = 500)))
               )
             )
           },
           "cloud_childprotection" = {
             tagList(
               layout_columns(
                 card(full_screen = TRUE,
                      card_header(strong("Regional  Data"),
                                  plotlyOutput("childprotection_regional_graph", height = 500)))
               )
             )
           },
           "cloud_support" = {
             tagList(
               layout_columns(
                 card(full_screen = TRUE,
                      card_header(strong("Regional  Data"),
                                  plotlyOutput("support_regional_graph", height = 500)))
               )
             )
           },
           "cloud_extension" = {
             tagList(
               layout_columns(
                 card(full_screen = TRUE,
                      card_header(strong("Regional  Data"),
                                  plotlyOutput("extension_regional_graph", height = 500)))
               )
             )
           },
           "cloud_extension" = {
             tagList(
               layout_columns(
                 card(full_screen = TRUE,
                      card_header(strong("Regional  Data"),
                                  plotlyOutput("school_count_regional_graph", height = 500)))
               )
             )
           },
           "cloud_years" = {
             tagList(
               layout_columns(
                 # Changed to plotlyOutput
                 card(full_screen = TRUE,
                      card_header(strong("Regional Data"),
                                  plotlyOutput("years_regional_graph", height = 500)))
               )
             )
           },
           "cloud_elec" = {
             tagList(
               layout_columns(
                 card(full_screen = TRUE,
                      card_header(strong("Regional Data"),
                                  plotlyOutput("elec_regional_graph", height = 500)))
               )
             )
           },
           "cloud_water" = {
             tagList(
               layout_columns(
                 card(full_screen = TRUE,
                      card_header(strong("Regional Data"),
                                  plotlyOutput("water_regional_graph", height = 500)))
               )
             )
           },
           "cloud_internet" = {
             tagList(
               layout_columns(
                 card(full_screen = TRUE,
                      card_header(strong("Regional Data"),
                                  plotlyOutput("internet_regional_graph", height = 500)))
               )
             )
           },
           "cloud_internet_usage" = {
             tagList(
               layout_columns(
                 card(full_screen = TRUE,
                      card_header(strong("Regional Data"),
                                  plotlyOutput("internet_usage_regional_graph", height = 500)))
               )
             )
           },
           "cloud_bully" = {
             tagList(
               layout_columns(
                 card(full_screen = TRUE,
                      card_header(strong("Regional Data"),
                                  plotlyOutput("bully_regional_graph", height = 500)))
               )
             )
           }
           
    )
  })
  
  output$cloud_graph_4 <- renderUI({
    req(input$cloud_category_picker_4)
    
    switch(input$cloud_category_picker_4,
           "cloud_enrolment" = {
             tagList(
               layout_columns(
                 card(full_screen = TRUE,
                      card_header(strong("Regional Data"),
                                  plotlyOutput("enrolment_regional_graph", height = 500))))
             )
           },
           "cloud_sned" = {
             tagList(
               layout_columns(
                 card(full_screen = TRUE,
                      card_header(strong("Regional Data"),
                                  plotlyOutput("sned_regional_graph", height = 500)))
               )
             )
           },
           "cloud_ip" = {
             tagList(
               layout_columns(
                 card(full_screen = TRUE,
                      card_header(strong("Regional Data"),
                                  plotlyOutput("ip_regional_graph", height = 500)))
               )
             )
           },
           "cloud_muslim" = {
             tagList(
               layout_columns(
                 card(full_screen = TRUE,
                      card_header(strong("Regional Data"),
                                  plotlyOutput("muslim_regional_graph", height = 500)))
               )
             )
           },
           "cloud_displaced" = {
             tagList(
               layout_columns(
                 card(full_screen = TRUE,
                      card_header(strong("Regional Data"),
                                  plotlyOutput("displaced_regional_graph", height = 500)))
               )
             )
           },
           "cloud_als" = {
             tagList(
               layout_columns(
                 card(full_screen = TRUE,
                      card_header(strong("Regional Data"),
                                  plotlyOutput("als_regional_graph", height = 500)))
               )
             )
           },
           "cloud_dropout" = {
             tagList(
               layout_columns(
                 card(full_screen = TRUE,
                      card_header(strong("Regional Data"),
                                  plotlyOutput("dropout_regional_graph", height = 500)))
               )
             )
           },
           "cloud_teacherinventory" = {
             tagList(
               layout_columns(
                 card(full_screen = TRUE,
                      card_header(strong("Regional  Data"),
                                  plotlyOutput("teacherinventory_regional_graph", height = 500)))
               )
             )
           },
           "cloud_classroom" = {
             tagList(
               layout_columns(
                 card(full_screen = TRUE,
                      card_header(strong("Regional  Data"),
                                  plotlyOutput("classroom_regional_graph", height = 500)))
               )
             )
           },
           "cloud_multigrade" = {
             tagList(
               layout_columns(
                 card(full_screen = TRUE,
                      card_header(strong("Regional  Data"),
                                  plotlyOutput("multigrade_regional_graph", height = 500)))
               )
             )
           },
           "cloud_organizedclass" = {
             tagList(
               layout_columns(
                 card(full_screen = TRUE,
                      card_header(strong("Regional  Data"),
                                  plotlyOutput("organizedclass_regional_graph", height = 500)))
               )
             )
           },
           "cloud_jhsdeployment" = {
             tagList(
               layout_columns(
                 card(full_screen = TRUE,
                      card_header(strong("Regional  Data"),
                                  plotlyOutput("jhsdeployment_regional_graph", height = 500)))
               )
             )
           },
           "cloud_shifting" = {
             tagList(
               layout_columns(
                 card(full_screen = TRUE,
                      card_header(strong("Regional  Data"),
                                  plotlyOutput("shifting_regional_graph", height = 500)))
               )
             )
           },
           "cloud_LDM" = {
             tagList(
               layout_columns(
                 card(full_screen = TRUE,
                      card_header(strong("Regional Data"),
                                  plotlyOutput("ldm_regional_graph", height = 500)))
               )
             )
           },
           "cloud_adm" = {
             tagList(
               layout_columns(
                 card(full_screen = TRUE,
                      card_header(strong("Regional Data"),
                                  plotlyOutput("adm_regional_graph", height = 500)))
               )
             )
           },
           "cloud_ARAL" = {
             tagList(
               layout_columns(
                 card(full_screen = TRUE,
                      card_header(strong("Regional  Data"),
                                  plotlyOutput("aral_regional_graph", height = 500)))
               )
             )
           },
           "cloud_crla" = {
             tagList(
               layout_columns(
                 card(full_screen = TRUE,
                      card_header(strong("Regional  Data"),
                                  plotlyOutput("crla_regional_graph", height = 500)))
               )
             )
           },
           "cloud_philiri" = {
             tagList(
               layout_columns(
                 card(full_screen = TRUE,
                      card_header(strong("Regional  Data"),
                                  plotlyOutput("philiri_regional_graph", height = 500)))
               )
             )
           },
           "cloud_initiatives" = {
             tagList(
               layout_columns(
                 card(full_screen = TRUE,
                      card_header(strong("Regional  Data"),
                                  plotlyOutput("initiatives_regional_graph", height = 500)))
               )
             )
           },
           "cloud_moi" = {
             tagList(
               layout_columns(
                 card(full_screen = TRUE,
                      card_header(strong("Regional  Data"),
                                  plotlyOutput("moi_regional_graph", height = 500)))
               )
             )
           },
           "cloud_sosss" = {
             tagList(
               layout_columns(
                 card(full_screen = TRUE,
                      card_header(strong("Regional  Data"),
                                  plotlyOutput("sosss_regional_graph", height = 500)))
               )
             )
           },
           "cloud_shsimplem" = {
             tagList(
               layout_columns(
                 card(full_screen = TRUE,
                      card_header(strong("Regional  Data"),
                                  plotlyOutput("shsimplem_regional_graph", height = 500)))
               )
             )
           },
           "cloud_overload" = {
             tagList(
               layout_columns(
                 card(full_screen = TRUE,
                      card_header(strong("Regional  Data"),
                                  plotlyOutput("overload_regional_graph", height = 500)))
               )
             )
           },
           "cloud_resources" = {
             tagList(
               layout_columns(
                 card(full_screen = TRUE,
                      card_header(strong("Regional  Data"),
                                  plotlyOutput("resources_regional_graph", height = 500)))
               )
             )
           },
           "cloud_nat" = {
             tagList(
               layout_columns(
                 card(full_screen = TRUE,
                      card_header(strong("Regional  Data"),
                                  plotlyOutput("nat_regional_graph", height = 500)))
               )
             )
           },
           "cloud_nat_sufficiency" = {
             tagList(
               layout_columns(
                 card(full_screen = TRUE,
                      card_header(strong("Regional  Data"),
                                  plotlyOutput("nat_sufficiency_regional_graph", height = 500)))
               )
             )
           },
           "cloud_rf" = {
             tagList(
               layout_columns(
                 card(full_screen = TRUE,
                      card_header(strong("Regional  Data"),
                                  plotlyOutput("rf_regional_graph", height = 500)))
               )
             )
           },
           "cloud_lac" = {
             tagList(
               layout_columns(
                 card(full_screen = TRUE,
                      card_header(strong("Regional  Data"),
                                  plotlyOutput("lac_regional_graph", height = 500)))
               )
             )
           },
           "cloud_feeding" = {
             tagList(
               layout_columns(
                 card(full_screen = TRUE,
                      card_header(strong("Regional  Data"),
                                  plotlyOutput("feeding_regional_graph", height = 500)))
               )
             )
           },
           "cloud_sha" = {
             tagList(
               layout_columns(
                 card(full_screen = TRUE,
                      card_header(strong("Regional  Data"),
                                  plotlyOutput("sha_regional_graph", height = 500)))
               )
             )
           },
           "cloud_childprotection" = {
             tagList(
               layout_columns(
                 card(full_screen = TRUE,
                      card_header(strong("Regional  Data"),
                                  plotlyOutput("childprotection_regional_graph", height = 500)))
               )
             )
           },
           "cloud_support" = {
             tagList(
               layout_columns(
                 card(full_screen = TRUE,
                      card_header(strong("Regional  Data"),
                                  plotlyOutput("support_regional_graph", height = 500)))
               )
             )
           },
           "cloud_extension" = {
             tagList(
               layout_columns(
                 card(full_screen = TRUE,
                      card_header(strong("Regional  Data"),
                                  plotlyOutput("extension_regional_graph", height = 500)))
               )
             )
           },
           "cloud_extension" = {
             tagList(
               layout_columns(
                 card(full_screen = TRUE,
                      card_header(strong("Regional  Data"),
                                  plotlyOutput("school_count_regional_graph", height = 500)))
               )
             )
           },
           "cloud_years" = {
             tagList(
               layout_columns(
                 # Changed to plotlyOutput
                 card(full_screen = TRUE,
                      card_header(strong("Regional Data"),
                                  plotlyOutput("years_regional_graph", height = 500)))
               )
             )
           },
           "cloud_elec" = {
             tagList(
               layout_columns(
                 card(full_screen = TRUE,
                      card_header(strong("Regional Data"),
                                  plotlyOutput("elec_regional_graph", height = 500)))
               )
             )
           },
           "cloud_water" = {
             tagList(
               layout_columns(
                 card(full_screen = TRUE,
                      card_header(strong("Regional Data"),
                                  plotlyOutput("water_regional_graph", height = 500)))
               )
             )
           },
           "cloud_internet" = {
             tagList(
               layout_columns(
                 card(full_screen = TRUE,
                      card_header(strong("Regional Data"),
                                  plotlyOutput("internet_regional_graph", height = 500)))
               )
             )
           },
           "cloud_internet_usage" = {
             tagList(
               layout_columns(
                 card(full_screen = TRUE,
                      card_header(strong("Regional Data"),
                                  plotlyOutput("internet_usage_regional_graph", height = 500)))
               )
             )
           },
           "cloud_bully" = {
             tagList(
               layout_columns(
                 card(full_screen = TRUE,
                      card_header(strong("Regional Data"),
                                  plotlyOutput("bully_regional_graph", height = 500)))
               )
             )
           }
           
    )
  })
  
  output$cloud_graph_5 <- renderUI({
    req(input$cloud_category_picker_5)
    
    switch(input$cloud_category_picker_5,
           "cloud_enrolment" = {
             tagList(
               layout_columns(
                 card(full_screen = TRUE,
                      card_header(strong("Regional Data"),
                                  plotlyOutput("enrolment_regional_graph", height = 500))))
             )
           },
           "cloud_sned" = {
             tagList(
               layout_columns(
                 card(full_screen = TRUE,
                      card_header(strong("Regional Data"),
                                  plotlyOutput("sned_regional_graph", height = 500)))
               )
             )
           },
           "cloud_ip" = {
             tagList(
               layout_columns(
                 card(full_screen = TRUE,
                      card_header(strong("Regional Data"),
                                  plotlyOutput("ip_regional_graph", height = 500)))
               )
             )
           },
           "cloud_muslim" = {
             tagList(
               layout_columns(
                 card(full_screen = TRUE,
                      card_header(strong("Regional Data"),
                                  plotlyOutput("muslim_regional_graph", height = 500)))
               )
             )
           },
           "cloud_displaced" = {
             tagList(
               layout_columns(
                 card(full_screen = TRUE,
                      card_header(strong("Regional Data"),
                                  plotlyOutput("displaced_regional_graph", height = 500)))
               )
             )
           },
           "cloud_als" = {
             tagList(
               layout_columns(
                 card(full_screen = TRUE,
                      card_header(strong("Regional Data"),
                                  plotlyOutput("als_regional_graph", height = 500)))
               )
             )
           },
           "cloud_dropout" = {
             tagList(
               layout_columns(
                 card(full_screen = TRUE,
                      card_header(strong("Regional Data"),
                                  plotlyOutput("dropout_regional_graph", height = 500)))
               )
             )
           },
           "cloud_teacherinventory" = {
             tagList(
               layout_columns(
                 card(full_screen = TRUE,
                      card_header(strong("Regional  Data"),
                                  plotlyOutput("teacherinventory_regional_graph", height = 500)))
               )
             )
           },
           "cloud_classroom" = {
             tagList(
               layout_columns(
                 card(full_screen = TRUE,
                      card_header(strong("Regional  Data"),
                                  plotlyOutput("classroom_regional_graph", height = 500)))
               )
             )
           },
           "cloud_multigrade" = {
             tagList(
               layout_columns(
                 card(full_screen = TRUE,
                      card_header(strong("Regional  Data"),
                                  plotlyOutput("multigrade_regional_graph", height = 500)))
               )
             )
           },
           "cloud_organizedclass" = {
             tagList(
               layout_columns(
                 card(full_screen = TRUE,
                      card_header(strong("Regional  Data"),
                                  plotlyOutput("organizedclass_regional_graph", height = 500)))
               )
             )
           },
           "cloud_jhsdeployment" = {
             tagList(
               layout_columns(
                 card(full_screen = TRUE,
                      card_header(strong("Regional  Data"),
                                  plotlyOutput("jhsdeployment_regional_graph", height = 500)))
               )
             )
           },
           "cloud_shifting" = {
             tagList(
               layout_columns(
                 card(full_screen = TRUE,
                      card_header(strong("Regional  Data"),
                                  plotlyOutput("shifting_regional_graph", height = 500)))
               )
             )
           },
           "cloud_LDM" = {
             tagList(
               layout_columns(
                 card(full_screen = TRUE,
                      card_header(strong("Regional Data"),
                                  plotlyOutput("ldm_regional_graph", height = 500)))
               )
             )
           },
           "cloud_adm" = {
             tagList(
               layout_columns(
                 card(full_screen = TRUE,
                      card_header(strong("Regional Data"),
                                  plotlyOutput("adm_regional_graph", height = 500)))
               )
             )
           },
           "cloud_ARAL" = {
             tagList(
               layout_columns(
                 card(full_screen = TRUE,
                      card_header(strong("Regional  Data"),
                                  plotlyOutput("aral_regional_graph", height = 500)))
               )
             )
           },
           "cloud_crla" = {
             tagList(
               layout_columns(
                 card(full_screen = TRUE,
                      card_header(strong("Regional  Data"),
                                  plotlyOutput("crla_regional_graph", height = 500)))
               )
             )
           },
           "cloud_philiri" = {
             tagList(
               layout_columns(
                 card(full_screen = TRUE,
                      card_header(strong("Regional  Data"),
                                  plotlyOutput("philiri_regional_graph", height = 500)))
               )
             )
           },
           "cloud_initiatives" = {
             tagList(
               layout_columns(
                 card(full_screen = TRUE,
                      card_header(strong("Regional  Data"),
                                  plotlyOutput("initiatives_regional_graph", height = 500)))
               )
             )
           },
           "cloud_moi" = {
             tagList(
               layout_columns(
                 card(full_screen = TRUE,
                      card_header(strong("Regional  Data"),
                                  plotlyOutput("moi_regional_graph", height = 500)))
               )
             )
           },
           "cloud_sosss" = {
             tagList(
               layout_columns(
                 card(full_screen = TRUE,
                      card_header(strong("Regional  Data"),
                                  plotlyOutput("sosss_regional_graph", height = 500)))
               )
             )
           },
           "cloud_shsimplem" = {
             tagList(
               layout_columns(
                 card(full_screen = TRUE,
                      card_header(strong("Regional  Data"),
                                  plotlyOutput("shsimplem_regional_graph", height = 500)))
               )
             )
           },
           "cloud_overload" = {
             tagList(
               layout_columns(
                 card(full_screen = TRUE,
                      card_header(strong("Regional  Data"),
                                  plotlyOutput("overload_regional_graph", height = 500)))
               )
             )
           },
           "cloud_resources" = {
             tagList(
               layout_columns(
                 card(full_screen = TRUE,
                      card_header(strong("Regional  Data"),
                                  plotlyOutput("resources_regional_graph", height = 500)))
               )
             )
           },
           "cloud_nat" = {
             tagList(
               layout_columns(
                 card(full_screen = TRUE,
                      card_header(strong("Regional  Data"),
                                  plotlyOutput("nat_regional_graph", height = 500)))
               )
             )
           },
           "cloud_nat_sufficiency" = {
             tagList(
               layout_columns(
                 card(full_screen = TRUE,
                      card_header(strong("Regional  Data"),
                                  plotlyOutput("nat_sufficiency_regional_graph", height = 500)))
               )
             )
           },
           "cloud_rf" = {
             tagList(
               layout_columns(
                 card(full_screen = TRUE,
                      card_header(strong("Regional  Data"),
                                  plotlyOutput("rf_regional_graph", height = 500)))
               )
             )
           },
           "cloud_lac" = {
             tagList(
               layout_columns(
                 card(full_screen = TRUE,
                      card_header(strong("Regional  Data"),
                                  plotlyOutput("lac_regional_graph", height = 500)))
               )
             )
           },
           "cloud_feeding" = {
             tagList(
               layout_columns(
                 card(full_screen = TRUE,
                      card_header(strong("Regional  Data"),
                                  plotlyOutput("feeding_regional_graph", height = 500)))
               )
             )
           },
           "cloud_sha" = {
             tagList(
               layout_columns(
                 card(full_screen = TRUE,
                      card_header(strong("Regional  Data"),
                                  plotlyOutput("sha_regional_graph", height = 500)))
               )
             )
           },
           "cloud_childprotection" = {
             tagList(
               layout_columns(
                 card(full_screen = TRUE,
                      card_header(strong("Regional  Data"),
                                  plotlyOutput("childprotection_regional_graph", height = 500)))
               )
             )
           },
           "cloud_support" = {
             tagList(
               layout_columns(
                 card(full_screen = TRUE,
                      card_header(strong("Regional  Data"),
                                  plotlyOutput("support_regional_graph", height = 500)))
               )
             )
           },
           "cloud_extension" = {
             tagList(
               layout_columns(
                 card(full_screen = TRUE,
                      card_header(strong("Regional  Data"),
                                  plotlyOutput("extension_regional_graph", height = 500)))
               )
             )
           },
           "cloud_extension" = {
             tagList(
               layout_columns(
                 card(full_screen = TRUE,
                      card_header(strong("Regional  Data"),
                                  plotlyOutput("school_count_regional_graph", height = 500)))
               )
             )
           },
           "cloud_years" = {
             tagList(
               layout_columns(
                 # Changed to plotlyOutput
                 card(full_screen = TRUE,
                      card_header(strong("Regional Data"),
                                  plotlyOutput("years_regional_graph", height = 500)))
               )
             )
           },
           "cloud_elec" = {
             tagList(
               layout_columns(
                 card(full_screen = TRUE,
                      card_header(strong("Regional Data"),
                                  plotlyOutput("elec_regional_graph", height = 500)))
               )
             )
           },
           "cloud_water" = {
             tagList(
               layout_columns(
                 card(full_screen = TRUE,
                      card_header(strong("Regional Data"),
                                  plotlyOutput("water_regional_graph", height = 500)))
               )
             )
           },
           "cloud_internet" = {
             tagList(
               layout_columns(
                 card(full_screen = TRUE,
                      card_header(strong("Regional Data"),
                                  plotlyOutput("internet_regional_graph", height = 500)))
               )
             )
           },
           "cloud_internet_usage" = {
             tagList(
               layout_columns(
                 card(full_screen = TRUE,
                      card_header(strong("Regional Data"),
                                  plotlyOutput("internet_usage_regional_graph", height = 500)))
               )
             )
           },
           "cloud_bully" = {
             tagList(
               layout_columns(
                 card(full_screen = TRUE,
                      card_header(strong("Regional Data"),
                                  plotlyOutput("bully_regional_graph", height = 500)))
               )
             )
           }
           
    )
  })
  
  output$cloud_graph_6 <- renderUI({
    req(input$cloud_category_picker_6)
    
    switch(input$cloud_category_picker_6,
           "cloud_enrolment" = {
             tagList(
               layout_columns(
                 card(full_screen = TRUE,
                      card_header(strong("Regional Data"),
                                  plotlyOutput("enrolment_regional_graph", height = 500))))
             )
           },
           "cloud_sned" = {
             tagList(
               layout_columns(
                 card(full_screen = TRUE,
                      card_header(strong("Regional Data"),
                                  plotlyOutput("sned_regional_graph", height = 500)))
               )
             )
           },
           "cloud_ip" = {
             tagList(
               layout_columns(
                 card(full_screen = TRUE,
                      card_header(strong("Regional Data"),
                                  plotlyOutput("ip_regional_graph", height = 500)))
               )
             )
           },
           "cloud_muslim" = {
             tagList(
               layout_columns(
                 card(full_screen = TRUE,
                      card_header(strong("Regional Data"),
                                  plotlyOutput("muslim_regional_graph", height = 500)))
               )
             )
           },
           "cloud_displaced" = {
             tagList(
               layout_columns(
                 card(full_screen = TRUE,
                      card_header(strong("Regional Data"),
                                  plotlyOutput("displaced_regional_graph", height = 500)))
               )
             )
           },
           "cloud_als" = {
             tagList(
               layout_columns(
                 card(full_screen = TRUE,
                      card_header(strong("Regional Data"),
                                  plotlyOutput("als_regional_graph", height = 500)))
               )
             )
           },
           "cloud_dropout" = {
             tagList(
               layout_columns(
                 card(full_screen = TRUE,
                      card_header(strong("Regional Data"),
                                  plotlyOutput("dropout_regional_graph", height = 500)))
               )
             )
           },
           "cloud_teacherinventory" = {
             tagList(
               layout_columns(
                 card(full_screen = TRUE,
                      card_header(strong("Regional  Data"),
                                  plotlyOutput("teacherinventory_regional_graph", height = 500)))
               )
             )
           },
           "cloud_classroom" = {
             tagList(
               layout_columns(
                 card(full_screen = TRUE,
                      card_header(strong("Regional  Data"),
                                  plotlyOutput("classroom_regional_graph", height = 500)))
               )
             )
           },
           "cloud_multigrade" = {
             tagList(
               layout_columns(
                 card(full_screen = TRUE,
                      card_header(strong("Regional  Data"),
                                  plotlyOutput("multigrade_regional_graph", height = 500)))
               )
             )
           },
           "cloud_organizedclass" = {
             tagList(
               layout_columns(
                 card(full_screen = TRUE,
                      card_header(strong("Regional  Data"),
                                  plotlyOutput("organizedclass_regional_graph", height = 500)))
               )
             )
           },
           "cloud_jhsdeployment" = {
             tagList(
               layout_columns(
                 card(full_screen = TRUE,
                      card_header(strong("Regional  Data"),
                                  plotlyOutput("jhsdeployment_regional_graph", height = 500)))
               )
             )
           },
           "cloud_shifting" = {
             tagList(
               layout_columns(
                 card(full_screen = TRUE,
                      card_header(strong("Regional  Data"),
                                  plotlyOutput("shifting_regional_graph", height = 500)))
               )
             )
           },
           "cloud_LDM" = {
             tagList(
               layout_columns(
                 card(full_screen = TRUE,
                      card_header(strong("Regional Data"),
                                  plotlyOutput("ldm_regional_graph", height = 500)))
               )
             )
           },
           "cloud_adm" = {
             tagList(
               layout_columns(
                 card(full_screen = TRUE,
                      card_header(strong("Regional Data"),
                                  plotlyOutput("adm_regional_graph", height = 500)))
               )
             )
           },
           "cloud_ARAL" = {
             tagList(
               layout_columns(
                 card(full_screen = TRUE,
                      card_header(strong("Regional  Data"),
                                  plotlyOutput("aral_regional_graph", height = 500)))
               )
             )
           },
           "cloud_crla" = {
             tagList(
               layout_columns(
                 card(full_screen = TRUE,
                      card_header(strong("Regional  Data"),
                                  plotlyOutput("crla_regional_graph", height = 500)))
               )
             )
           },
           "cloud_philiri" = {
             tagList(
               layout_columns(
                 card(full_screen = TRUE,
                      card_header(strong("Regional  Data"),
                                  plotlyOutput("philiri_regional_graph", height = 500)))
               )
             )
           },
           "cloud_initiatives" = {
             tagList(
               layout_columns(
                 card(full_screen = TRUE,
                      card_header(strong("Regional  Data"),
                                  plotlyOutput("initiatives_regional_graph", height = 500)))
               )
             )
           },
           "cloud_moi" = {
             tagList(
               layout_columns(
                 card(full_screen = TRUE,
                      card_header(strong("Regional  Data"),
                                  plotlyOutput("moi_regional_graph", height = 500)))
               )
             )
           },
           "cloud_sosss" = {
             tagList(
               layout_columns(
                 card(full_screen = TRUE,
                      card_header(strong("Regional  Data"),
                                  plotlyOutput("sosss_regional_graph", height = 500)))
               )
             )
           },
           "cloud_shsimplem" = {
             tagList(
               layout_columns(
                 card(full_screen = TRUE,
                      card_header(strong("Regional  Data"),
                                  plotlyOutput("shsimplem_regional_graph", height = 500)))
               )
             )
           },
           "cloud_overload" = {
             tagList(
               layout_columns(
                 card(full_screen = TRUE,
                      card_header(strong("Regional  Data"),
                                  plotlyOutput("overload_regional_graph", height = 500)))
               )
             )
           },
           "cloud_resources" = {
             tagList(
               layout_columns(
                 card(full_screen = TRUE,
                      card_header(strong("Regional  Data"),
                                  plotlyOutput("resources_regional_graph", height = 500)))
               )
             )
           },
           "cloud_nat" = {
             tagList(
               layout_columns(
                 card(full_screen = TRUE,
                      card_header(strong("Regional  Data"),
                                  plotlyOutput("nat_regional_graph", height = 500)))
               )
             )
           },
           "cloud_nat_sufficiency" = {
             tagList(
               layout_columns(
                 card(full_screen = TRUE,
                      card_header(strong("Regional  Data"),
                                  plotlyOutput("nat_sufficiency_regional_graph", height = 500)))
               )
             )
           },
           "cloud_rf" = {
             tagList(
               layout_columns(
                 card(full_screen = TRUE,
                      card_header(strong("Regional  Data"),
                                  plotlyOutput("rf_regional_graph", height = 500)))
               )
             )
           },
           "cloud_lac" = {
             tagList(
               layout_columns(
                 card(full_screen = TRUE,
                      card_header(strong("Regional  Data"),
                                  plotlyOutput("lac_regional_graph", height = 500)))
               )
             )
           },
           "cloud_feeding" = {
             tagList(
               layout_columns(
                 card(full_screen = TRUE,
                      card_header(strong("Regional  Data"),
                                  plotlyOutput("feeding_regional_graph", height = 500)))
               )
             )
           },
           "cloud_sha" = {
             tagList(
               layout_columns(
                 card(full_screen = TRUE,
                      card_header(strong("Regional  Data"),
                                  plotlyOutput("sha_regional_graph", height = 500)))
               )
             )
           },
           "cloud_childprotection" = {
             tagList(
               layout_columns(
                 card(full_screen = TRUE,
                      card_header(strong("Regional  Data"),
                                  plotlyOutput("childprotection_regional_graph", height = 500)))
               )
             )
           },
           "cloud_support" = {
             tagList(
               layout_columns(
                 card(full_screen = TRUE,
                      card_header(strong("Regional  Data"),
                                  plotlyOutput("support_regional_graph", height = 500)))
               )
             )
           },
           "cloud_extension" = {
             tagList(
               layout_columns(
                 card(full_screen = TRUE,
                      card_header(strong("Regional  Data"),
                                  plotlyOutput("extension_regional_graph", height = 500)))
               )
             )
           },
           "cloud_extension" = {
             tagList(
               layout_columns(
                 card(full_screen = TRUE,
                      card_header(strong("Regional  Data"),
                                  plotlyOutput("school_count_regional_graph", height = 500)))
               )
             )
           },
           "cloud_years" = {
             tagList(
               layout_columns(
                 # Changed to plotlyOutput
                 card(full_screen = TRUE,
                      card_header(strong("Regional Data"),
                                  plotlyOutput("years_regional_graph", height = 500)))
               )
             )
           },
           "cloud_elec" = {
             tagList(
               layout_columns(
                 card(full_screen = TRUE,
                      card_header(strong("Regional Data"),
                                  plotlyOutput("elec_regional_graph", height = 500)))
               )
             )
           },
           "cloud_water" = {
             tagList(
               layout_columns(
                 card(full_screen = TRUE,
                      card_header(strong("Regional Data"),
                                  plotlyOutput("water_regional_graph", height = 500)))
               )
             )
           },
           "cloud_internet" = {
             tagList(
               layout_columns(
                 card(full_screen = TRUE,
                      card_header(strong("Regional Data"),
                                  plotlyOutput("internet_regional_graph", height = 500)))
               )
             )
           },
           "cloud_internet_usage" = {
             tagList(
               layout_columns(
                 card(full_screen = TRUE,
                      card_header(strong("Regional Data"),
                                  plotlyOutput("internet_usage_regional_graph", height = 500)))
               )
             )
           },
           "cloud_bully" = {
             tagList(
               layout_columns(
                 card(full_screen = TRUE,
                      card_header(strong("Regional Data"),
                                  plotlyOutput("bully_regional_graph", height = 500)))
               )
             )
           }
           
    )
  })
  
  ### CLOUD SERVER ###
  
  output$cloud_profile_main_content_area <- renderUI({
    layout_columns(
      col_widths = 12,
      card(
        card_header(strong("Enrolment")),
        layout_columns(
          card(
            card_header("Total Enrolment per Category"),
            plotlyOutput("totalenrolment_regional_profile_graph")),
          card(
            card_header("Enrolment"),
            plotlyOutput("enrolment_regional_profile_graph")),
          card(
            card_header("SNED Learners"),
            plotlyOutput("sned_regional_profile_graph")),
          card(
            card_header("IP Learners"),
            plotlyOutput("ip_regional_profile_graph")),
          card(
            card_header("Muslim Learners"),
            plotlyOutput("muslim_regional_profile_graph")),
          card(
            card_header("Displaced Learners"),
            plotlyOutput("displaced_regional_profile_graph")),
          card(
            card_header("ALS Learners"),
            plotlyOutput("als_regional_profile_graph")),
          card(
            card_header("Dropouts"),
            plotlyOutput("dropout_regional_profile_graph")),
          col_widths = c(4,4,4,4,4,4,4,4)
          )
        ),
      card(
        card_header(strong("Teacher Deployment")),
        layout_columns(
          card(
            card_header("Teacher Inventory"),
            plotlyOutput("teacherinventory_regional_profile_graph")),
          card(
            card_header("JHS Teacher Deployment"),
            plotlyOutput("jhsdeployment_regional_profile_graph")),
          card(
            card_header("Teacher Overload"),
            plotlyOutput("overload_regional_profile_graph")),
          card(
            card_header("Multigrade Teachers"),
            plotlyOutput("multigrade_regional_profile_graph")),
          card(
            card_header("Years in Service"),
            plotlyOutput("years_regional_profile_graph")),
          col_widths = c(4,4,4,4,4,4)
        )
      ),
      card(
        card_header(strong("Classroom Data")),
        layout_columns(
          card(
            card_header("Classroom Data"),
            plotlyOutput("classroom_regional_profile_graph")),
          card(
            card_header("Resources Data"),
            plotlyOutput("resources_regional_profile_graph")),
          card(
            card_header("Shifting"),
            plotlyOutput("shifting_regional_profile_graph")),
          card(
            card_header("Organized Class"),
            plotlyOutput("organizedclass_regional_profile_graph")),
          card(
            card_header("Electricity"),
            plotlyOutput("elec_regional_profile_graph")),
          card(
            card_header("Water"),
            plotlyOutput("water_regional_profile_graph")),
          card(
            card_header("Internet"),
            plotlyOutput("internet_regional_profile_graph")),
          card(
            card_header("Internet Usage"),
            plotlyOutput("internet_usage_regional_profile_graph")),
          col_widths = c(4,4,4,4,4,4,4,4)
        )
      ),
      card(
        card_header(strong("Remediation")),
        layout_columns(
          card(
            card_header("Reading Proficiency"),
            plotlyOutput("rf_regional_profile_graph")),
          card(
            card_header("ARAL"),
            plotlyOutput("aral_regional_profile_graph")),
          card(
            card_header("CRLA"),
            plotlyOutput("crla_regional_profile_graph")),
          card(
            card_header("PhilIRI"),
            plotlyOutput("philiri_regional_profile_graph")),
          # card(
          #   card_header("NAT"),
          #   plotlyOutput("nat_regional_profile_graph")),
          # card(
          #   card_header("NAT Sufficiency"),
          #   plotlyOutput("nat_sufficiency_regional_profile_graph")),
          col_widths = c(4,4,4,4)
        )
      ),
      card(
        card_header(strong("Child Protection")),
        layout_columns(
          card(
            card_header("Bullying"),
            plotlyOutput("bully_regional_profile_graph")),
          card(
            card_header("Feeding Program Recipients"),
            plotlyOutput("feeding_regional_profile_graph")),
          col_widths = c(4,4,4,4)
        )
      ))
    })

  
  output$cloud_main_content_area <- renderUI({
    req(input$cloud_main_category_picker)
    
    switch(input$cloud_main_category_picker,
           "cloud_enrolment" = {
             tagList(
               h3("Enrolment Data"),
               hr(),
               layout_columns(
                 # Adjusted col_widths for the three plots/tables to be side-by-side
                 col_widths = c(12,12,12), # Assuming you want 3 columns for these
                 #Uncomment and adjust if you need a national data table as a plotly table
                 # Changed to plotlyOutput
                 card(full_screen = TRUE,
                      card_header(strong("Regional Data"),
                                  plotlyOutput("enrolment_regional_graph", height = 500))),
                 card(full_screen = TRUE,
                      card_header(strong("SDO Data"),
                                  plotlyOutput("enrolment_division_graph", height = 800))),
                 # card(full_screen = TRUE,
                 #      card_header(strong("Legislative District Data"),
                 #                  plotlyOutput("enrolment_district_graph", height = 500))),
                 card(full_screen = TRUE,
                      card_header(strong("School Database"),
                                  dataTableOutput("enrolment_data_table")))# This is the target for interaction
               )
             )
           },
           "cloud_sned" = {
             tagList(
               h3("SNED Learners"),
               hr(),
               layout_columns(
                 # Adjusted col_widths for the three plots/tables to be side-by-side
                 col_widths = c(12,12,12), # Assuming you want 3 columns for these
                 #Uncomment and adjust if you need a national data table as a plotly table
                 # Changed to plotlyOutput
                 card(full_screen = TRUE,
                      card_header(strong("Regional Data"),
                                  plotlyOutput("sned_regional_graph", height = 500))),
                 card(full_screen = TRUE,
                      card_header(strong("SDO Data"),
                                  plotlyOutput("sned_division_graph", height = 800))),
                 # card(full_screen = TRUE,
                 #      card_header(strong("Legislative District Data"),
                 #                  plotlyOutput("sned_district_graph", height = 500))),
                 card(full_screen = TRUE,
                      card_header(strong("School Database"),
                                  dataTableOutput("sned_data_table")))# This is the target for interaction
               )
             )
           },
           "cloud_ip" = {
             tagList(
               h3("IP Learners"),
               hr(),
               layout_columns(
                 # Adjusted col_widths for the three plots/tables to be side-by-side
                 col_widths = c(12,12,12), # Assuming you want 3 columns for these
                 #Uncomment and adjust if you need a national data table as a plotly table
                 # Changed to plotlyOutput
                 card(full_screen = TRUE,
                      card_header(strong("RegionalData"),
                                  plotlyOutput("ip_regional_graph", height = 500))),
                 card(full_screen = TRUE,
                      card_header(strong("SDO Data"),
                                  plotlyOutput("ip_division_graph", height = 800))),
                 # card(full_screen = TRUE,
                 #      card_header(strong("Legislative District Data"),
                 #                  plotlyOutput("ip_district_graph", height = 500))),
                 card(full_screen = TRUE,
                      card_header(strong("School Database"),
                                  dataTableOutput("ip_data_table")))# This is the target for interaction
               )
             )
           },
           "cloud_muslim" = {
             tagList(
               h3("Muslim Learners"),
               hr(),
               layout_columns(
                 # Adjusted col_widths for the three plots/tables to be side-by-side
                 col_widths = c(12,12,12), # Assuming you want 3 columns for these
                 #Uncomment and adjust if you need a national data table as a plotly table
                 # Changed to plotlyOutput
                 card(full_screen = TRUE,
                      card_header(strong("Regional Data"),
                                  plotlyOutput("muslim_regional_graph", height = 500))),
                 card(full_screen = TRUE,
                      card_header(strong("SDO Data"),
                                  plotlyOutput("muslim_division_graph", height = 800))),
                 # card(full_screen = TRUE,
                 #      card_header(strong("Legislative District Data"),
                 #                  plotlyOutput("muslim_district_graph", height = 500))),
                 card(full_screen = TRUE,
                      card_header(strong("School Database"),
                                  dataTableOutput("muslim_data_table")))# This is the target for interaction
               )
             )
           },
           "cloud_displaced" = {
             tagList(
               h3("Displaced Learners"),
               hr(),
               layout_columns(
                 # Adjusted col_widths for the three plots/tables to be side-by-side
                 col_widths = c(12,12,12), # Assuming you want 3 columns for these
                 #Uncomment and adjust if you need a national data table as a plotly table
                 # Changed to plotlyOutput
                 card(full_screen = TRUE,
                      card_header(strong("Regional Data"),
                                  plotlyOutput("displaced_regional_graph", height = 500))),
                 card(full_screen = TRUE,
                      card_header(strong("SDO Data"),
                                  plotlyOutput("displaced_division_graph", height = 800))),
                 # card(full_screen = TRUE,
                 #      card_header(strong("Legislative District Data"),
                 #                  plotlyOutput("displaced_district_graph", height = 500))),
                 card(full_screen = TRUE,
                      card_header(strong("School Database"),
                                  dataTableOutput("displaced_data_table")))# This is the target for interaction
               )
             )
           },
           "cloud_als" = {
             tagList(
               h3("ALS Learners"),
               hr(),
               layout_columns(
                 # Adjusted col_widths for the three plots/tables to be side-by-side
                 col_widths = c(12,12,12), # Assuming you want 3 columns for these
                 #Uncomment and adjust if you need a national data table as a plotly table
                 # Changed to plotlyOutput
                 card(full_screen = TRUE,
                      card_header(strong("Regional Data"),
                                  plotlyOutput("als_regional_graph", height = 500))),
                 card(full_screen = TRUE,
                      card_header(strong("SDO Data"),
                                  plotlyOutput("als_division_graph", height = 800))),
                 # card(full_screen = TRUE,
                 #      card_header(strong("Legislative District Data"),
                 #                  plotlyOutput("als_district_graph", height = 500))),
                 card(full_screen = TRUE,
                      card_header(strong("School Database"),
                                  dataTableOutput("als_data_table")))# This is the target for interaction
               )
             )
           },
           "cloud_dropout" = {
             tagList(
               h3("School Dropout Data"),
               hr(),
               layout_columns(
                 # Adjusted col_widths for the three plots/tables to be side-by-side
                 col_widths = c(12,12,12), # Assuming you want 3 columns for these
                 #Uncomment and adjust if you need a national data table as a plotly table
                 # Changed to plotlyOutput
                 card(full_screen = TRUE,
                      card_header(strong("Regional Data"),
                                  plotlyOutput("dropout_regional_graph", height = 500))),
                 card(full_screen = TRUE,
                      card_header(strong("SDO Data"),
                                  plotlyOutput("dropout_division_graph", height = 500))),
                 # card(full_screen = TRUE,
                 #      card_header(strong("Legislative District Data"),
                 #                  plotlyOutput("dropout_district_graph", height = 500))),
                 card(full_screen = TRUE,
                      card_header(strong("School Database"),
                                  dataTableOutput("dropout_data_table")))# This is the target for interaction
               )
             )
           },
           "cloud_teacherinventory" = {
             tagList(
               h3("Teaching Inventory Data"),
               hr(),
               layout_columns(
                 # Adjusted col_widths for the three plots/tables to be side-by-side
                 col_widths = c(12,12,12), # Assuming you want 3 columns for these
                 #Uncomment and adjust if you need a national data table as a plotly table
                 # Changed to plotlyOutput
                 card(full_screen = TRUE,
                      card_header(strong("Regional  Data"),
                                  plotlyOutput("teacherinventory_regional_graph", height = 500))),
                 card(full_screen = TRUE,
                      card_header(strong("SDO Data"),
                                  plotlyOutput("teacherinventory_division_graph", height = 500))),
                 # card(full_screen = TRUE,
                 #      card_header(strong("Legislative District  Data"),
                 #                  plotlyOutput("teacherinventory_district_graph", height = 500))),
                 card(full_screen = TRUE,
                      card_header(strong("School Database"),
                                  dataTableOutput("teacherinventory_data_table")))# This is the target for interaction
               )
             )
           },
           "cloud_classroom" = {
             tagList(
               h3("Classroom Inventory"),
               hr(),
               layout_columns(
                 # Adjusted col_widths for the three plots/tables to be side-by-side
                 col_widths = c(12,12,12), # Assuming you want 3 columns for these
                 #Uncomment and adjust if you need a national data table as a plotly table
                 # Changed to plotlyOutput
                 card(full_screen = TRUE,
                      card_header(strong("Regional  Data"),
                                  plotlyOutput("classroom_regional_graph", height = 500))),
                 card(full_screen = TRUE,
                      card_header(strong("SDO Data"),
                                  plotlyOutput("classroom_division_graph", height = 500))),
                 # card(full_screen = TRUE,
                 #      card_header(strong("Legislative District  Data"),
                 #                  plotlyOutput("classroom_district_graph", height = 500))),
                 card(full_screen = TRUE,
                      card_header(strong("School Database"),
                                  dataTableOutput("classroom_data_table")))# This is the target for interaction
               )
             )
           },
           "cloud_multigrade" = {
             tagList(
               h3("Multigrade Classes"),
               hr(),
               layout_columns(
                 # Adjusted col_widths for the three plots/tables to be side-by-side
                 col_widths = c(12,12,12), # Assuming you want 3 columns for these
                 #Uncomment and adjust if you need a national data table as a plotly table
                 # Changed to plotlyOutput
                 card(full_screen = TRUE,
                      card_header(strong("Regional  Data"),
                                  plotlyOutput("multigrade_regional_graph", height = 500))),
                 card(full_screen = TRUE,
                      card_header(strong("SDO Data"),
                                  plotlyOutput("multigrade_division_graph", height = 500))),
                 # card(full_screen = TRUE,
                 #      card_header(strong("Legislative District  Data"),
                 #                  plotlyOutput("multigrade_district_graph", height = 500))),
                 card(full_screen = TRUE,
                      card_header(strong("School Database"),
                                  dataTableOutput("multigrade_data_table")))# This is the target for interaction
               )
             )
           },
           "cloud_organizedclass" = {
             tagList(
               h3("Number of Organized Class"),
               hr(),
               layout_columns(
                 # Adjusted col_widths for the three plots/tables to be side-by-side
                 col_widths = c(12,12,12), # Assuming you want 3 columns for these
                 #Uncomment and adjust if you need a national data table as a plotly table
                 # Changed to plotlyOutput
                 card(full_screen = TRUE,
                      card_header(strong("Regional  Data"),
                                  plotlyOutput("organizedclass_regional_graph", height = 500))),
                 card(full_screen = TRUE,
                      card_header(strong("SDO Data"),
                                  plotlyOutput("organizedclass_division_graph", height = 500))),
                 # card(full_screen = TRUE,
                 #      card_header(strong("Legislative District  Data"),
                 #                  plotlyOutput("organizedclass_district_graph", height = 500))),
                 card(full_screen = TRUE,
                      card_header(strong("School Database"),
                                  dataTableOutput("organizedclass_data_table")))# This is the target for interaction
               )
             )
           },
           "cloud_jhsdeployment" = {
             tagList(
               h3("JHS Deployment"),
               hr(),
               layout_columns(
                 # Adjusted col_widths for the three plots/tables to be side-by-side
                 col_widths = c(12,12,12), # Assuming you want 3 columns for these
                 #Uncomment and adjust if you need a national data table as a plotly table
                 # Changed to plotlyOutput
                 card(full_screen = TRUE,
                      card_header(strong("Regional  Data"),
                                  plotlyOutput("jhsdeployment_regional_graph", height = 500))),
                 card(full_screen = TRUE,
                      card_header(strong("SDO Data"),
                                  plotlyOutput("jhsdeployment_division_graph", height = 500))),
                 # card(full_screen = TRUE,
                 #      card_header(strong("Legislative District  Data"),
                 #                  plotlyOutput("jhsdeployment_district_graph", height = 500))),
                 card(full_screen = TRUE,
                      card_header(strong("School Database"),
                                  dataTableOutput("jhsdeployment_data_table")))# This is the target for interaction
               )
             )
           },
           "cloud_shifting" = {
             tagList(
               h3("Shifting Strategies"),
               hr(),
               layout_columns(
                 # Adjusted col_widths for the three plots/tables to be side-by-side
                 col_widths = c(12,12,12), # Assuming you want 3 columns for these
                 #Uncomment and adjust if you need a national data table as a plotly table
                 # Changed to plotlyOutput
                 card(full_screen = TRUE,
                      card_header(strong("Regional  Data"),
                                  plotlyOutput("shifting_regional_graph", height = 500))),
                 card(full_screen = TRUE,
                      card_header(strong("SDO Data"),
                                  plotlyOutput("shifting_division_graph", height = 500))),
                 # card(full_screen = TRUE,
                 #      card_header(strong("Legislative District  Data"),
                 #                  plotlyOutput("shifting_district_graph", height = 500))),
                 card(full_screen = TRUE,
                      card_header(strong("School Database"),
                                  dataTableOutput("shifting_data_table")))# This is the target for interaction
               )
             )
           },
           "cloud_LDM" = {
             tagList(
               h3("Learning Delivery Modality"),
               hr(),
               layout_columns(
                 # Adjusted col_widths for the three plots/tables to be side-by-side
                 col_widths = c(12,12,12), # Assuming you want 3 columns for these
                 #Uncomment and adjust if you need a national data table as a plotly table
                 # Changed to plotlyOutput
                 card(full_screen = TRUE,
                      card_header(strong("Regional Data"),
                                  plotlyOutput("ldm_regional_graph", height = 500))),
                 card(full_screen = TRUE,
                      card_header(strong("SDO Data"),
                                  plotlyOutput("ldm_division_graph", height = 500))),
                 # card(full_screen = TRUE,
                 #      card_header(strong("Legislative District  Data"),
                 #                  plotlyOutput("school_count_district_graph", height = 500))),
                 card(full_screen = TRUE,
                      card_header(strong("School Database"),
                                  dataTableOutput("ldm_data_table")))# This is the target for interaction
               )
             )
           },
           "cloud_adm" = {
             tagList(
               h3("Alternative Delivery Modality"),
               hr(),
               layout_columns(
                 # Adjusted col_widths for the three plots/tables to be side-by-side
                 col_widths = c(12,12,12), # Assuming you want 3 columns for these
                 #Uncomment and adjust if you need a national data table as a plotly table
                 # Changed to plotlyOutput
                 card(full_screen = TRUE,
                      card_header(strong("Regional Data"),
                                  plotlyOutput("adm_regional_graph", height = 500))),
                 card(full_screen = TRUE,
                      card_header(strong("SDO Data"),
                                  plotlyOutput("adm_division_graph", height = 500))),
                 # card(full_screen = TRUE,
                 #      card_header(strong("Legislative District  Data"),
                 #                  plotlyOutput("school_count_district_graph", height = 500))),
                 card(full_screen = TRUE,
                      card_header(strong("School Database"),
                                  dataTableOutput("adm_data_table")))# This is the target for interaction
               )
             )
           },
           "cloud_ARAL" = {
             tagList(
               h3("Prospective ARAL Learners"),
               hr(),
               layout_columns(
                 # Adjusted col_widths for the three plots/tables to be side-by-side
                 col_widths = c(12,12,12,12), # Assuming you want 3 columns for these
                 #Uncomment and adjust if you need a national data table as a plotly table
                 # Changed to plotlyOutput
                 card(full_screen = TRUE,
                      card_header(strong("Regional  Data"),
                                  plotlyOutput("aral_regional_graph", height = 500))),
                 card(full_screen = TRUE,
                      card_header(strong("SDO Data"),
                                  plotlyOutput("aral_division_graph", height = 500))),
                 # card(full_screen = TRUE,
                 #      card_header(strong("Legislative District  Data"),
                 #                  plotlyOutput("aral_district_graph", height = 500))),
                 card(full_screen = TRUE,
                      card_header(strong("School Database"),
                                  dataTableOutput("aral_data_table")))# This is the target for interaction
               )
             )
           },
           "cloud_crla" = {
             tagList(
               h3("CRLA Data"),
               hr(),
               layout_columns(
                 # Adjusted col_widths for the three plots/tables to be side-by-side
                 col_widths = c(12,12,12,12), # Assuming you want 3 columns for these
                 #Uncomment and adjust if you need a national data table as a plotly table
                 # Changed to plotlyOutput
                 card(full_screen = TRUE,
                      card_header(strong("Regional  Data"),
                                  plotlyOutput("crla_regional_graph", height = 500))),
                 card(full_screen = TRUE,
                      card_header(strong("SDO Data"),
                                  plotlyOutput("crla_division_graph", height = 500))),
                 # card(full_screen = TRUE,
                 #      card_header(strong("Legislative District  Data"),
                 #                  plotlyOutput("crla_district_graph", height = 500))),
                 card(full_screen = TRUE,
                      card_header(strong("School Database"),
                                  dataTableOutput("crla_data_table")))# This is the target for interaction
               )
             )
           },
           "cloud_philiri" = {
             tagList(
               h3("PhilIRI Data"),
               hr(),
               layout_columns(
                 # Adjusted col_widths for the three plots/tables to be side-by-side
                 col_widths = c(12,12,12,12), # Assuming you want 3 columns for these
                 #Uncomment and adjust if you need a national data table as a plotly table
                 # Changed to plotlyOutput
                 card(full_screen = TRUE,
                      card_header(strong("Regional  Data"),
                                  plotlyOutput("philiri_regional_graph", height = 500))),
                 card(full_screen = TRUE,
                      card_header(strong("SDO Data"),
                                  plotlyOutput("philiri_division_graph", height = 500))),
                 # card(full_screen = TRUE,
                 #      card_header(strong("Legislative District  Data"),
                 #                  plotlyOutput("philiri_district_graph", height = 500))),
                 card(full_screen = TRUE,
                      card_header(strong("School Database"),
                                  dataTableOutput("philiri_data_table")))# This is the target for interaction
               )
             )
           },
           "cloud_initiatives" = {
             tagList(
               h3("School Initiatives"),
               hr(),
               layout_columns(
                 # Adjusted col_widths for the three plots/tables to be side-by-side
                 col_widths = c(12,12,12,12), # Assuming you want 3 columns for these
                 #Uncomment and adjust if you need a national data table as a plotly table
                 # Changed to plotlyOutput
                 card(full_screen = TRUE,
                      card_header(strong("Regional  Data"),
                                  plotlyOutput("initiatives_regional_graph", height = 500))),
                 card(full_screen = TRUE,
                      card_header(strong("SDO Data"),
                                  plotlyOutput("initiatives_division_graph", height = 500))),
                 # card(full_screen = TRUE,
                 #      card_header(strong("Legislative District  Data"),
                 #                  plotlyOutput("initiatives_district_graph", height = 500))),
                 card(full_screen = TRUE,
                      card_header(strong("School Database"),
                                  dataTableOutput("initiatives_data_table")))# This is the target for interaction
               )
             )
           },
           "cloud_moi" = {
             tagList(
               h3("Medium of Instruction"),
               hr(),
               layout_columns(
                 # Adjusted col_widths for the three plots/tables to be side-by-side
                 col_widths = c(12,12,12,12), # Assuming you want 3 columns for these
                 #Uncomment and adjust if you need a national data table as a plotly table
                 # Changed to plotlyOutput
                 card(full_screen = TRUE,
                      card_header(strong("Regional  Data"),
                                  plotlyOutput("moi_regional_graph", height = 500))),
                 card(full_screen = TRUE,
                      card_header(strong("SDO Data"),
                                  plotlyOutput("moi_division_graph", height = 500))),
                 # card(full_screen = TRUE,
                 #      card_header(strong("Legislative District  Data"),
                 #                  plotlyOutput("moi_district_graph", height = 500))),
                 card(full_screen = TRUE,
                      card_header(strong("School Database"),
                                  dataTableOutput("moi_data_table")))# This is the target for interaction
               )
             )
           },
           "cloud_sosss" = {
             tagList(
               h3("School Organizational Structure and Staffing Standards"),
               hr(),
               layout_columns(
                 # Adjusted col_widths for the three plots/tables to be side-by-side
                 col_widths = c(12,12,12,12), # Assuming you want 3 columns for these
                 #Uncomment and adjust if you need a national data table as a plotly table
                 # Changed to plotlyOutput
                 card(full_screen = TRUE,
                      card_header(strong("Regional  Data"),
                                  plotlyOutput("sosss_regional_graph", height = 500))),
                 card(full_screen = TRUE,
                      card_header(strong("SDO Data"),
                                  plotlyOutput("sosss_division_graph", height = 500))),
                 # card(full_screen = TRUE,
                 #      card_header(strong("Legislative District  Data"),
                 #                  plotlyOutput("sosss_district_graph", height = 500))),
                 card(full_screen = TRUE,
                      card_header(strong("School Database"),
                                  dataTableOutput("sosss_data_table")))# This is the target for interaction
               )
             )
           },
           "cloud_shsimplem" = {
             tagList(
               h3("SHS Implementation"),
               hr(),
               layout_columns(
                 # Adjusted col_widths for the three plots/tables to be side-by-side
                 col_widths = c(12,12,12,12), # Assuming you want 3 columns for these
                 #Uncomment and adjust if you need a national data table as a plotly table
                 # Changed to plotlyOutput
                 card(full_screen = TRUE,
                      card_header(strong("Regional  Data"),
                                  plotlyOutput("shsimplem_regional_graph", height = 500))),
                 card(full_screen = TRUE,
                      card_header(strong("SDO Data"),
                                  plotlyOutput("shsimplem_division_graph", height = 500))),
                 # card(full_screen = TRUE,
                 #      card_header(strong("Legislative District  Data"),
                 #                  plotlyOutput("shsimplem_district_graph", height = 500))),
                 card(full_screen = TRUE,
                      card_header(strong("School Database"),
                                  dataTableOutput("shsimplem_data_table")))# This is the target for interaction
               )
             )
           },
           "cloud_overload" = {
             tagList(
               h3("Teaching Overload Data"),
               hr(),
               layout_columns(
                 # Adjusted col_widths for the three plots/tables to be side-by-side
                 col_widths = c(12,12,12,12), # Assuming you want 3 columns for these
                 #Uncomment and adjust if you need a national data table as a plotly table
                 # Changed to plotlyOutput
                 card(full_screen = TRUE,
                      card_header(strong("Regional  Data"),
                                  plotlyOutput("overload_regional_graph", height = 500))),
                 card(full_screen = TRUE,
                      card_header(strong("SDO Data"),
                                  plotlyOutput("overload_division_graph", height = 500))),
                 # card(full_screen = TRUE,
                 #      card_header(strong("Legislative District  Data"),
                 #                  plotlyOutput("overload_district_graph", height = 500))),
                 card(full_screen = TRUE,
                      card_header(strong("School Database"),
                                  dataTableOutput("overload_data_table")))# This is the target for interaction
               )
             )
           },
           "cloud_resources" = {
             tagList(
               h3("School Resources"),
               hr(),
               layout_columns(
                 # Adjusted col_widths for the three plots/tables to be side-by-side
                 col_widths = c(12,12,12), # Assuming you want 3 columns for these
                 #Uncomment and adjust if you need a national data table as a plotly table
                 # Changed to plotlyOutput
                 card(full_screen = TRUE,
                      card_header(strong("Regional  Data"),
                                  plotlyOutput("resources_regional_graph", height = 500))),
                 card(full_screen = TRUE,
                      card_header(strong("SDO Data"),
                                  plotlyOutput("resources_division_graph", height = 500))),
                 # card(full_screen = TRUE,
                 #      card_header(strong("Legislative District  Data"),
                 #                  plotlyOutput("resources_district_graph", height = 500))),
                 card(full_screen = TRUE,
                      card_header(strong("School Database"),
                                  dataTableOutput("resources_data_table")))# This is the target for interaction
               )
             )
           },
           "cloud_nat" = {
             tagList(
               h3("NAT Data"),
               hr(),
               layout_columns(
                 # Adjusted col_widths for the three plots/tables to be side-by-side
                 col_widths = c(12,12,12), # Assuming you want 3 columns for these
                 #Uncomment and adjust if you need a national data table as a plotly table
                 # Changed to plotlyOutput
                 card(full_screen = TRUE,
                      card_header(strong("Regional  Data"),
                                  plotlyOutput("nat_regional_graph", height = 500))),
                 card(full_screen = TRUE,
                      card_header(strong("SDO Data"),
                                  plotlyOutput("nat_division_graph", height = 500))),
                 # card(full_screen = TRUE,
                 #      card_header(strong("Legislative District  Data"),
                 #                  plotlyOutput("nat_district_graph", height = 500))),
                 card(full_screen = TRUE,
                      card_header(strong("School Database"),
                                  dataTableOutput("nat_data_table")))# This is the target for interaction
               )
             )
           },
           "cloud_nat_sufficiency" = {
             tagList(
               h3("NAT Sufficiency Data"),
               hr(),
               layout_columns(
                 # Adjusted col_widths for the three plots/tables to be side-by-side
                 col_widths = c(12,12,12), # Assuming you want 3 columns for these
                 #Uncomment and adjust if you need a national data table as a plotly table
                 # Changed to plotlyOutput
                 card(full_screen = TRUE,
                      card_header(strong("Regional  Data"),
                                  plotlyOutput("nat_sufficiency_regional_graph", height = 500))),
                 card(full_screen = TRUE,
                      card_header(strong("SDO Data"),
                                  plotlyOutput("nat_sufficiency_division_graph", height = 500))),
                 # card(full_screen = TRUE,
                 #      card_header(strong("Legislative District  Data"),
                 #                  plotlyOutput("nat_district_graph", height = 500))),
                 card(full_screen = TRUE,
                      card_header(strong("School Database"),
                                  dataTableOutput("nat_sufficiency_data_table")))# This is the target for interaction
               )
             )
           },
           "cloud_rf" = {
             tagList(
               h3("Reading Proficiency Data"),
               hr(),
               layout_columns(
                 # Adjusted col_widths for the three plots/tables to be side-by-side
                 col_widths = c(12,12,12), # Assuming you want 3 columns for these
                 #Uncomment and adjust if you need a national data table as a plotly table
                 # Changed to plotlyOutput
                 card(full_screen = TRUE,
                      card_header(strong("Regional  Data"),
                                  plotlyOutput("rf_regional_graph", height = 500))),
                 card(full_screen = TRUE,
                      card_header(strong("SDO Data"),
                                  plotlyOutput("rf_division_graph", height = 500))),
                 # card(full_screen = TRUE,
                 #      card_header(strong("Legislative District  Data"),
                 #                  plotlyOutput("nat_district_graph", height = 500))),
                 card(full_screen = TRUE,
                      card_header(strong("School Database"),
                                  dataTableOutput("rf_data_table")))# This is the target for interaction
               )
             )
           },
           "cloud_lac" = {
             tagList(
               h3("LAC Sessions"),
               hr(),
               layout_columns(
                 # Adjusted col_widths for the three plots/tables to be side-by-side
                 col_widths = c(12,12,12), # Assuming you want 3 columns for these
                 #Uncomment and adjust if you need a national data table as a plotly table
                 # Changed to plotlyOutput
                 card(full_screen = TRUE,
                      card_header(strong("Regional  Data"),
                                  plotlyOutput("lac_regional_graph", height = 500))),
                 card(full_screen = TRUE,
                      card_header(strong("SDO Data"),
                                  plotlyOutput("lac_division_graph", height = 500))),
                 # card(full_screen = TRUE,
                 #      card_header(strong("Legislative District  Data"),
                 #                  plotlyOutput("lac_district_graph", height = 500))),
                 card(full_screen = TRUE,
                      card_header(strong("School Database"),
                                  dataTableOutput("lac_data_table")))# This is the target for interaction
               )
             )
           },
           "cloud_feeding" = {
             tagList(
               h3("Feeding Program"),
               hr(),
               layout_columns(
                 # Adjusted col_widths for the three plots/tables to be side-by-side
                 col_widths = c(12,12,12,12), # Assuming you want 3 columns for these
                 #Uncomment and adjust if you need a national data table as a plotly table
                 # Changed to plotlyOutput
                 card(full_screen = TRUE,
                      card_header(strong("Regional  Data"),
                                  plotlyOutput("feeding_regional_graph", height = 500))),
                 card(full_screen = TRUE,
                      card_header(strong("SDO Data"),
                                  plotlyOutput("feeding_division_graph", height = 500))),
                 # card(full_screen = TRUE,
                 #      card_header(strong("Legislative District  Data"),
                 #                  plotlyOutput("feeding_district_graph", height = 500))),
                 card(full_screen = TRUE,
                      card_header(strong("School Database"),
                                  dataTableOutput("feeding_data_table")))# This is the target for interaction
               )
             )
           },
           "cloud_sha" = {
             tagList(
               h3("Special Hardship Allowance"),
               hr(),
               layout_columns(
                 # Adjusted col_widths for the three plots/tables to be side-by-side
                 col_widths = c(12,12,12), # Assuming you want 3 columns for these
                 #Uncomment and adjust if you need a national data table as a plotly table
                 # Changed to plotlyOutput
                 card(full_screen = TRUE,
                      card_header(strong("Regional  Data"),
                                  plotlyOutput("sha_regional_graph", height = 500))),
                 card(full_screen = TRUE,
                      card_header(strong("SDO Data"),
                                  plotlyOutput("sha_division_graph", height = 500))),
                 # card(full_screen = TRUE,
                 #      card_header(strong("Legislative District  Data"),
                 #                  plotlyOutput("sha_district_graph", height = 500))),
                 card(full_screen = TRUE,
                      card_header(strong("School Database"),
                                  dataTableOutput("sha_data_table")))# This is the target for interaction
               )
             )
           },
           "cloud_childprotection" = {
             tagList(
               h3("Child Protection"),
               hr(),
               layout_columns(
                 # Adjusted col_widths for the three plots/tables to be side-by-side
                 col_widths = c(12,12,12,12), # Assuming you want 3 columns for these
                 #Uncomment and adjust if you need a national data table as a plotly table
                 # Changed to plotlyOutput
                 card(full_screen = TRUE,
                      card_header(strong("Regional  Data"),
                                  plotlyOutput("childprotection_regional_graph", height = 500))),
                 card(full_screen = TRUE,
                      card_header(strong("SDO Data"),
                                  plotlyOutput("childprotection_division_graph", height = 500))),
                 # card(full_screen = TRUE,
                 #      card_header(strong("Legislative District  Data"),
                 #                  plotlyOutput("childprotection_district_graph", height = 500))),
                 card(full_screen = TRUE,
                      card_header(strong("School Database"),
                                  dataTableOutput("childprotection_data_table")))# This is the target for interaction
               )
             )
           },
           "cloud_support" = {
             tagList(
               h3("Support Received"),
               hr(),
               layout_columns(
                 # Adjusted col_widths for the three plots/tables to be side-by-side
                 col_widths = c(12,12,12,12), # Assuming you want 3 columns for these
                 #Uncomment and adjust if you need a national data table as a plotly table
                 # Changed to plotlyOutput
                 card(full_screen = TRUE,
                      card_header(strong("Regional  Data"),
                                  plotlyOutput("support_regional_graph", height = 500))),
                 card(full_screen = TRUE,
                      card_header(strong("SDO Data"),
                                  plotlyOutput("support_division_graph", height = 500))),
                 # card(full_screen = TRUE,
                 #      card_header(strong("Legislative District  Data"),
                 #                  plotlyOutput("support_district_graph", height = 500))),
                 card(full_screen = TRUE,
                      card_header(strong("School Database"),
                                  dataTableOutput("support_data_table")))# This is the target for interaction
               )
             )
           },
           "cloud_extension" = {
             tagList(
               h3("Extension Schools Data"),
               hr(),
               layout_columns(
                 # Adjusted col_widths for the three plots/tables to be side-by-side
                 col_widths = c(12,12,12,12), # Assuming you want 3 columns for these
                 #Uncomment and adjust if you need a national data table as a plotly table
                 # Changed to plotlyOutput
                 card(full_screen = TRUE,
                      card_header(strong("Regional  Data"),
                                  plotlyOutput("extension_regional_graph", height = 500))),
                 card(full_screen = TRUE,
                      card_header(strong("SDO Data"),
                                  plotlyOutput("extension_division_graph", height = 500))),
                 # card(full_screen = TRUE,
                 #      card_header(strong("Legislative District  Data"),
                 #                  plotlyOutput("extension_district_graph", height = 500))),
                 card(full_screen = TRUE,
                      card_header(strong("School Database"),
                                  dataTableOutput("extension_data_table")))# This is the target for interaction
               )
             )
           },
           "cloud_extension" = {
             tagList(
               h3("Extension Schools Data"),
               hr(),
               layout_columns(
                 # Adjusted col_widths for the three plots/tables to be side-by-side
                 col_widths = c(12,12,12), # Assuming you want 3 columns for these
                 #Uncomment and adjust if you need a national data table as a plotly table
                 # Changed to plotlyOutput
                 card(full_screen = TRUE,
                      card_header(strong("Regional  Data"),
                                  plotlyOutput("school_count_regional_graph", height = 500))),
                 card(full_screen = TRUE,
                      card_header(strong("SDO Data"),
                                  plotlyOutput("school_count_division_graph", height = 00))),
                 # card(full_screen = TRUE,
                 #      card_header(strong("Legislative District  Data"),
                 #                  plotlyOutput("school_count_district_graph", height = 500))),
                 card(full_screen = TRUE,
                      card_header(strong("School Database"),
                                  dataTableOutput("school_count_data_table")))# This is the target for interaction
               )
             )
           },
           "cloud_years" = {
             tagList(
               h3("Years in Service"),
               hr(),
               layout_columns(
                 # Adjusted col_widths for the three plots/tables to be side-by-side
                 col_widths = c(12,12,12), # Assuming you want 3 columns for these
                 #Uncomment and adjust if you need a national data table as a plotly table
                 # Changed to plotlyOutput
                 card(full_screen = TRUE,
                      card_header(strong("Regional Data"),
                                  plotlyOutput("years_regional_graph", height = 500))),
                 card(full_screen = TRUE,
                      card_header(strong("SDO Data"),
                                  plotlyOutput("years_division_graph", height = 800))),
                 # card(full_screen = TRUE,
                 #      card_header(strong("Legislative District Data"),
                 #                  plotlyOutput("als_district_graph", height = 500))),
                 card(full_screen = TRUE,
                      card_header(strong("School Database"),
                                  dataTableOutput("years_data_table")))# This is the target for interaction
               )
             )
           },
           "cloud_elec" = {
             tagList(
               h3("Electricity Source"),
               hr(),
               layout_columns(
                 # Adjusted col_widths for the three plots/tables to be side-by-side
                 col_widths = c(12,12,12), # Assuming you want 3 columns for these
                 #Uncomment and adjust if you need a national data table as a plotly table
                 # Changed to plotlyOutput
                 card(full_screen = TRUE,
                      card_header(strong("Regional Data"),
                                  plotlyOutput("elec_regional_graph", height = 500))),
                 card(full_screen = TRUE,
                      card_header(strong("SDO Data"),
                                  plotlyOutput("elec_division_graph", height = 800))),
                 # card(full_screen = TRUE,
                 #      card_header(strong("Legislative District Data"),
                 #                  plotlyOutput("als_district_graph", height = 500))),
                 card(full_screen = TRUE,
                      card_header(strong("School Database"),
                                  dataTableOutput("elec_data_table")))# This is the target for interaction
               )
             )
           },
           "cloud_water" = {
             tagList(
               h3("Water Source"),
               hr(),
               layout_columns(
                 # Adjusted col_widths for the three plots/tables to be side-by-side
                 col_widths = c(12,12,12), # Assuming you want 3 columns for these
                 #Uncomment and adjust if you need a national data table as a plotly table
                 # Changed to plotlyOutput
                 card(full_screen = TRUE,
                      card_header(strong("Regional Data"),
                                  plotlyOutput("water_regional_graph", height = 500))),
                 card(full_screen = TRUE,
                      card_header(strong("SDO Data"),
                                  plotlyOutput("water_division_graph", height = 800))),
                 # card(full_screen = TRUE,
                 #      card_header(strong("Legislative District Data"),
                 #                  plotlyOutput("als_district_graph", height = 500))),
                 card(full_screen = TRUE,
                      card_header(strong("School Database"),
                                  dataTableOutput("water_data_table")))# This is the target for interaction
               )
             )
           },
           "cloud_internet" = {
             tagList(
               h3("Internet Source"),
               hr(),
               layout_columns(
                 # Adjusted col_widths for the three plots/tables to be side-by-side
                 col_widths = c(12,12,12), # Assuming you want 3 columns for these
                 #Uncomment and adjust if you need a national data table as a plotly table
                 # Changed to plotlyOutput
                 card(full_screen = TRUE,
                      card_header(strong("Regional Data"),
                                  plotlyOutput("internet_regional_graph", height = 500))),
                 card(full_screen = TRUE,
                      card_header(strong("SDO Data"),
                                  plotlyOutput("internet_division_graph", height = 800))),
                 # card(full_screen = TRUE,
                 #      card_header(strong("Legislative District Data"),
                 #                  plotlyOutput("als_district_graph", height = 500))),
                 card(full_screen = TRUE,
                      card_header(strong("School Database"),
                                  dataTableOutput("internet_data_table")))# This is the target for interaction
               )
             )
           },
           "cloud_internet_usage" = {
             tagList(
               h3("Internet Usage"),
               hr(),
               layout_columns(
                 # Adjusted col_widths for the three plots/tables to be side-by-side
                 col_widths = c(12,12,12), # Assuming you want 3 columns for these
                 #Uncomment and adjust if you need a national data table as a plotly table
                 # Changed to plotlyOutput
                 card(full_screen = TRUE,
                      card_header(strong("Regional Data"),
                                  plotlyOutput("internet_usage_regional_graph", height = 500))),
                 card(full_screen = TRUE,
                      card_header(strong("SDO Data"),
                                  plotlyOutput("internet_usage_division_graph", height = 800))),
                 # card(full_screen = TRUE,
                 #      card_header(strong("Legislative District Data"),
                 #                  plotlyOutput("als_district_graph", height = 500))),
                 card(full_screen = TRUE,
                      card_header(strong("School Database"),
                                  dataTableOutput("internet_usage_data_table")))# This is the target for interaction
               )
             )
           },
           "cloud_bully" = {
             tagList(
               h3("Recorded Bullying Incidence"),
               hr(),
               layout_columns(
                 # Adjusted col_widths for the three plots/tables to be side-by-side
                 col_widths = c(12,12,12), # Assuming you want 3 columns for these
                 #Uncomment and adjust if you need a national data table as a plotly table
                 # Changed to plotlyOutput
                 card(full_screen = TRUE,
                      card_header(strong("Regional Data"),
                                  plotlyOutput("bully_regional_graph", height = 500))),
                 card(full_screen = TRUE,
                      card_header(strong("SDO Data"),
                                  plotlyOutput("bully_division_graph", height = 800))),
                 # card(full_screen = TRUE,
                 #      card_header(strong("Legislative District Data"),
                 #                  plotlyOutput("als_district_graph", height = 500))),
                 card(full_screen = TRUE,
                      card_header(strong("School Database"),
                                  dataTableOutput("bully_data_table")))# This is the target for interaction
               )
             )
           }
           
    )
  })
  
  #--------Server Outputs-------------------#
  
  
  filtered_school_data_region <- reactive({
    req(uni)
    
    temp_data <- uni
    
    if (!is.null(input$dashboard_region_filter) && length(input$dashboard_region_filter) > 0) {
      temp_data <- temp_data %>%
        filter(Region %in% input$dashboard_region_filter)
    }
    
    return(temp_data)
  })
  
  filtered_LMS_region <- reactive({
    req(LMS)
    
    temp_data <- LMS
    
    if (!is.null(input$dashboard_region_filter) && length(input$dashboard_region_filter) > 0) {
      temp_data <- temp_data %>%
        filter(Region %in% input$dashboard_region_filter)
    }
    
    return(temp_data)
  })
  
  filtered_teacher_shortage_data_region <- reactive({
    req(DBMProp)
    
    temp_data <- DBMProp
    
    if (!is.null(input$dashboard_region_filter) && length(input$dashboard_region_filter) > 0) {
      temp_data <- temp_data %>%
        filter(Region %in% input$dashboard_region_filter)
    }
    
    return(temp_data)
  })
  
  filtered_teacher_shortage_data_division <- reactive({
    req(DBMProp)
    
    temp_data <- DBMProp
    
    if (!is.null(input$dashboard_region_filter) && length(input$dashboard_region_filter) > 0) {
      temp_data <- temp_data %>%
        filter(Region %in% input$dashboard_region_filter)
    }
    
    if (!is.null(input$dashboard_division_filter) && length(input$dashboard_division_filter) > 0) {
      temp_data <- temp_data %>%
        filter(Division %in% input$dashboard_division_filter)
    }
    
    return(temp_data)
  })
  
  filtered_school_data_division <- reactive({
    req(uni)
    
    temp_data <- uni
    
    if (!is.null(input$dashboard_region_filter) && length(input$dashboard_region_filter) > 0) {
      temp_data <- temp_data %>%
        filter(Region %in% input$dashboard_region_filter)
    }
    
    if (!is.null(input$dashboard_division_filter) && length(input$dashboard_division_filter) > 0) {
      temp_data <- temp_data %>%
        filter(Division %in% input$dashboard_division_filter)
    }
    
    return(temp_data)
  })
  # Inside your server function
  # Make sure you have the 'scales' library loaded for comma formatting
  library(scales) # Add this if not already present
  
  # Assuming 'filtered_school_data_region' is a reactive expression that provides
  # the data, similar to how it was introduced in the first response.
  # Make sure your 'filtered_school_data_region' reactive is defined in your server logic.
  
  
  # Server
  output$total_schools_box <- renderUI({
    # Get the count from the reactive data object
    total_schools <- nrow(filtered_school_data_region())
    
    # Create the value_box with the reactive value
    value_box(
      title = "Total Schools in the Selected Regions",
      value = total_schools,
      showcase = bs_icon("building-fill")
    )
  })
  
  output$total_schools_box_div <- renderUI({
    # Get the count from the reactive data object
    total_schools <- nrow(filtered_school_data_division())
    
    # Create the value_box with the reactive value
    value_box(
      title = "Total Schools in the Selected Divisions",
      value = total_schools,
      showcase = bs_icon("building-fill")
    )
  })
  
  output$school_count_regional_graph <- renderPlotly({
    
    current_filtered_data <- filtered_school_data_region() # Use the reactive filtered data
    
    # --- Empty Data Handling ---
    if (nrow(current_filtered_data) == 0) {
      return(ggplotly(ggplot() +
                        annotate("text", x = 0.5, y = 0.5, label = "No data for selected regions/divisions") +
                        theme_void()))
    }
    
    # Define the levels for Modified.COC for consistent ordering
    coc_levels <- c("Purely ES", "JHS with SHS", "ES and JHS (K to 10)", "Purely JHS", "All Offering (K to 12)", "Purely SHS")
    
    # Prepare the data for plotting
    # Group by Region and Modified.COC
    plot_data <- current_filtered_data %>%
      group_by(Region, Modified.COC) %>%
      summarise(Count = n(), .groups = 'drop')
    
    plot_data_totals <- plot_data %>%
      group_by(Modified.COC) %>%
      summarise(TotalCount = sum(Count))
    
    # Calculate total counts per Region for reordering the X-axis
    region_total_counts <- plot_data %>%
      group_by(Region) %>%
      summarise(Total_Region_Count = sum(Count), .groups = 'drop')
    
    # Join total counts back to plot_data for reordering
    plot_data <- plot_data %>%
      left_join(region_total_counts, by = "Region") %>%
      # Create a reordered factor for Region based on Total_Region_Count
      mutate(Region_reordered = reorder(Region, Total_Region_Count))
    

    
    # Create the ggplot
    p <- ggplot(plot_data,
                aes(x = factor(Modified.COC, levels = coc_levels), # Regions on the X-axis
                    y = Count,
                    fill = Region, # Fill by Modified.COC
                    text = paste("Region: ", Region,
                                 "<br>School Type: ", Modified.COC,
                                 "<br>Count: ", scales::comma(Count)))) + # Custom tooltip text
      geom_bar(stat = "identity", position = "stack", color = "black", size = 0.25) + # Dodged bars
      geom_text(data = plot_data_totals,
                aes(x = Modified.COC, y = TotalCount * 1.05, label = scales::comma(TotalCount)), # Modified line
                inherit.aes = FALSE,
                size = 3.5,
                color = "black") +
      labs(x = "Modified Curricular Offering",
           y = "Number of Schools",
           fill = "Region") +
      scale_y_continuous(labels = scales::comma) + # Format y-axis labels as comma-separated numbers
      theme_minimal() +
      theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
            legend.position = "bottom", # Position legend at the bottom
            plot.title = element_text(hjust = 0.5)) # Center the plot title
    
    # Convert ggplot to plotly, ensuring custom text is used for hover
    ggplotly(p, tooltip = "text", source = "schoolcountplot_region") %>%
      layout(hoverlabel = list(bgcolor = "white"),
             margin = list(b = 100)) # Increase bottom margin for x-axis labels
  })
  
  output$SOSSS_DataTable <- DT::renderDT({
    uni %>% mutate(School.Size.Typology = factor(School.Size.Typology, levels = c("Very Small","Small","Medium","Large","Very Large","Extremely Large","Mega"))) %>% 
      select(School.Size.Typology) %>%
      group_by(School.Size.Typology) %>%
      summarize(Total = n()) %>% 
      pivot_wider(names_from = "School.Size.Typology", values_from = "Total")
  },
  rownames = FALSE,
  options = list(
    scrollX = TRUE,
    columnDefs = list(list(className = 'dt-center', targets = "_all"))
  ))
  
  output$SOSSS_Region_Typology <- renderPlotly({
    
    # Assume 'filtered_sosss_data' is a reactive expression that provides
    # the 'uni' data filtered by input$dashboard_region_filter and
    # input$dashboard_division_filter.
    current_filtered_data <- filtered_school_data_region() # Use the reactive filtered data
    
    # --- Empty Data Handling ---
    if (nrow(current_filtered_data) == 0) {
      return(ggplotly(ggplot() +
                        annotate("text", x = 0.5, y = 0.5, label = "No data for selected regions/divisions") +
                        theme_void()))
    }
    
    # Prepare the data for plotting
    plot_data <- current_filtered_data %>%
      group_by(Region, School.Size.Typology) %>%
      summarise(Count = n(), .groups = 'drop')
    
    plot_data_totals <- plot_data %>%
      group_by(School.Size.Typology) %>%
      summarise(TotalCount = sum(Count))
    
    # Calculate total counts per region for reordering (optional, but good practice for clarity)
    # This makes the reorder factor consistent based on total schools per region.
    region_totals <- plot_data %>%
      group_by(Region) %>%
      summarise(TotalCount = sum(Count), .groups = 'drop')
    
    plot_data <- plot_data %>%
      left_join(region_totals, by = "Region") %>%
      mutate(Old.Region_reordered = reorder(Region, -TotalCount))
    
    # Define the levels for School.Size.Typology for consistent ordering
    sosss_levels <- c("Very Small", "Small", "Medium", "Large", "Very Large", "Extremely Large", "Mega")
    
    # Create the ggplot
    p <- ggplot(plot_data,
                aes(x = factor(School.Size.Typology, levels = sosss_levels),
                    y = Count,
                    fill = Old.Region_reordered,
                    text = paste("Region: ", Region,
                                 "<br>School Size: ", School.Size.Typology,
                                 "<br>Count: ", scales::comma(Count)))) + # Custom tooltip text
      geom_bar(stat = "identity", position = "stack", color = "black", size = 0.25) +
      geom_text(data = plot_data_totals,
                aes(x = School.Size.Typology, y = TotalCount * 1.05, label = scales::comma(TotalCount)), # Modified line
                inherit.aes = FALSE,
                size = 3.5,
                color = "black") +
      labs(x = "School Size Typology",
           y = "Number of Schools",
           fill = "Region") +
      scale_y_continuous(labels = scales::comma) + # Format y-axis labels as comma-separated numbers
      theme_minimal() +
      theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
            legend.position = "bottom", # Position legend at the bottom
            plot.title = element_text(hjust = 0.5)) # Center the plot title
    
    # Convert ggplot to plotly, ensuring custom text is used for hover
    # And apply hovermode = "x unified" for better multi-trace hover
    ggplotly(p, tooltip = "text", source = "sosssRegionPlot") %>%
      layout(hoverlabel = list(bgcolor = "white"),
             # Adjust margins to prevent labels from being cut off if needed
             margin = list(b = 100)) # Increase bottom margin for x-axis labels
  })
  
  output$SOSSS_All_List_Typology <- DT::renderDT({
    datatable(
      uni %>%
        select(Region, Division, SchoolID, School.Name, TotalEnrolment, School.Size.Typology) %>%
        mutate(School.Size.Typology = factor(School.Size.Typology, levels = c("Very Small","Small","Medium","Large","Very Large","Extremely Large","Mega"))) %>% 
        filter(Region %in% input$dashboard_region_filter) %>% 
        arrange(School.Size.Typology), # This is the data argument to datatable()
      extension = 'Buttons',  # These are now separate arguments to datatable()
      rownames = FALSE,
      filter = "top",
      options = list(
        scrollX = TRUE,
        pageLength = 10,
        columnDefs = list(list(className = 'dt-center', targets = "_all")),
        dom = 'lfrtip', # Corrected dom string to include 'f' for filter/search input
        buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
      )
    )
  })
  
  observeEvent(input$dashboard_region_filter, {
    # Assuming 'df' is your main dataset and it has a 'Division' column
    # and 'Region' column
    req(input$dashboard_region_filter, df) # Make sure df is available
    
    filtered_divisions <- df %>%
      filter(Region  %in%  input$dashboard_region_filter) %>%
      pull(Division) %>%
      unique() %>%
      sort()
    
    updatePickerInput(
      session = session,
      inputId = "dashboard_division_filter",
      choices = filtered_divisions,
      # CHANGE THIS LINE:
      selected = filtered_divisions[1] # This will select all available divisions
    )
  })
  
  output$Classroom_Shortage_Region_Graph <- renderPlotly({
    
    # Use the reactive filtered data
    current_filtered_data <- filtered_LMS_region()
    
    # --- Empty Data Handling ---
    if (nrow(current_filtered_data) == 0) {
      return(ggplotly(ggplot() +
                        annotate("text", x = 0.5, y = 0.5, label = "No data for selected regions/divisions") +
                        theme_void()))
    }
    
    # Prepare the data for plotting
    # Ensure 'Estimated_CL_Shortage' is treated as numeric
    plot_data <- current_filtered_data %>%
      group_by(Region) %>%
      summarise(Count = sum(as.numeric(Estimated_CL_Shortage), na.rm = TRUE), .groups = 'drop')

    # Create the ggplot
    p <- ggplot(plot_data,
                aes(x = reorder(Region, -Count),
                    y = Count,
                    fill = Region,
                    text = paste("Region: ", Region,
                                 "<br>Classroom Shortage: ", scales::comma(Count)))) + # Custom tooltip text
      geom_bar(stat = "identity", color = "black") +
      geom_text(data = plot_data,
                aes(x = Region, y = Count * 1.05, label = scales::comma(Count)), # Modified line
                inherit.aes = FALSE,
                size = 3.5,
                color = "black") +
      labs(x = "Region",
           y = "Classroom Shortage") +
      scale_y_continuous(labels = scales::comma) + # Format y-axis labels as comma-separated numbers
      theme_minimal() +
      theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
            legend.position = "none", # No legend needed for single fill
            plot.title = element_text(hjust = 0.5)) # Center the plot title
    
    # Convert ggplot to plotly, ensuring custom text is used for hover
    ggplotly(p, tooltip = "text", source = "classroomShortageRegionPlot") %>%
      layout(hoverlabel = list(bgcolor = "white"),
             # Adjust margins to prevent labels from being cut off if needed
             margin = list(b = 100)) # Increase bottom margin for x-axis labels
  })
  
  output$LMS_Nation_Graph <- renderPlotly({
    # Use the reactive filtered data
    # Correct the column selection: pivot columns 13-17 without excluding any.
    current_filtered_data <- filtered_LMS_region() %>% rename("With Buildable Space" = Buildable_space, "With Excess Classrooms" = With_Excess, "Without Classroom Shortage" = Without_Shortage, "Last Mile Schools" = LMS, "GIDCA" = GIDCA, "With Shortage" = With_Shortage) %>%
      pivot_longer(13:18, names_to = "Type", values_to = "Count")
    
    # --- Empty Data Handling ---
    if (nrow(current_filtered_data) == 0) {
      return(ggplotly(ggplot() +
                        annotate("text", x = 0.5, y = 0.5, label = "No data for selected regions/divisions") +
                        theme_void()))
    }
    
    # Prepare the data for plotting
    # Ensure 'Count' is treated as numeric
    plot_data <- current_filtered_data %>%
      group_by(Region, Type) %>%
      summarise(
        Count = sum(as.numeric(Count), na.rm = TRUE), 
        .groups = 'drop'
      )
    
    plot_data_totals <- plot_data %>%
      group_by(Type) %>%
      summarise(TotalCount = sum(Count))
    
    # Create the ggplot
    # Use the correct column names for the tooltip text
    p <- ggplot(plot_data,
                aes(
                  x = reorder(Type, -Count),
                  y = Count,
                  fill = Region,
                  text = paste(
                    "Type:", Type,
                    "<br>Count:", scales::comma(Count)
                  )
                )) +
      geom_bar(stat = "identity", position = "stack", color = "black", size = 0.25) +
      geom_text(data = plot_data_totals,
                aes(x = Type, y = TotalCount * 1.05, label = scales::comma(TotalCount)), # Modified line
                inherit.aes = FALSE,
                size = 3.5,
                color = "black") +
      labs(x = "Type",
           y = "Count",
           fill = "Region") +
      scale_y_continuous(labels = scales::comma) + 
      theme_minimal() +
      theme(
        axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
        legend.position = "right", # Display the legend to show the `Type` fill
        plot.title = element_text(hjust = 0.5) 
      )
    
    # Convert ggplot to plotly, ensuring custom text is used for hover
    # Also, hide unnecessary hover info from ggplotly
    ggplotly(p, tooltip = "text", source = "classroomShortageRegionPlot") %>%
      layout(hoverlabel = list(bgcolor = "white"),
             margin = list(b = 100)) %>% 
      # Use plotly's style function to hide the "trace" hover info and keep the custom text
      style(hoverinfo = "text") 
  })
  
  output$LMS_Division_Graph <- renderPlotly({
    
    current_filtered_data <- filtered_LMS_division() %>% rename("With Buildable Space" = Buildable_space, "With Excess Classrooms" = With_Excess, "Without Classroom Shortage" = Without_Shortage, "Last Mile Schools" = LMS, "GIDCA" = GIDCA, "With Shortage" = With_Shortage) %>%
      pivot_longer(13:18, names_to = "Type", values_to = "Count")
    
    # --- Empty Data Handling ---
    if (nrow(current_filtered_data) == 0) {
      return(ggplotly(ggplot() +
                        annotate("text", x = 0.5, y = 0.5, label = "No data for selected regions/divisions") +
                        theme_void()))
    }
    
    # Prepare the data for plotting
    # Ensure 'Count' is treated as numeric
    plot_data <- current_filtered_data %>%
      group_by(Division, Type) %>%
      summarise(
        Count = sum(as.numeric(Count), na.rm = TRUE), 
        .groups = 'drop'
      )
    
    # Create the ggplot
    # Use the correct column names for the tooltip text
    p <- ggplot(plot_data,
                aes(
                  x = reorder(Type,-Count),
                  y = Count,
                  fill = Division,
                  text = paste(
                    "Division:", Division,
                    "<br>Count:", scales::comma(Count)
                  )
                )) +
      geom_bar(stat = "identity", color = "black", size = 0.25) +
      geom_text(data = plot_data,
                aes(x = Type, y = Count * 1.05, label = scales::comma(Count)), # Modified line
                inherit.aes = FALSE,
                size = 3.5,
                color = "black") +
      labs(x = "Division",
           y = "Count") +
      scale_y_continuous(labels = scales::comma) + 
      theme_minimal() +
      theme(
        axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
        legend.position = "right", # Display the legend to show the `Type` fill
        plot.title = element_text(hjust = 0.5) 
      )
    
    # Convert ggplot to plotly, ensuring custom text is used for hover
    # Also, hide unnecessary hover info from ggplotly
    ggplotly(p, tooltip = "text", source = "classroomShortageRegionPlot") %>%
      layout(hoverlabel = list(bgcolor = "white"),
             margin = list(b = 100)) %>% 
      # Use plotly's style function to hide the "trace" hover info and keep the custom text
      style(hoverinfo = "text") 
  })
  
  output$LMS_All_List  <- DT::renderDT({
    
    data_to_display <- filtered_LMS_division() %>% select(Region, Division, School_Name, With_Excess, Without_Shortage, Buildable_space, LMS, GIDCA) 
    
    # You might want to add a check for NULL or empty data if filtered_school_data_division()
    # could return such states and you want to display a message or an empty table.
    if (is.null(data_to_display) || nrow(data_to_display) == 0) {
      return(DT::datatable(
        data.frame("Message" = "No data available based on current selection."),
        options = list(dom = 't'), # 't' hides all controls, showing only the table body
        rownames = FALSE
      ))
    }
    
    DT::datatable(
      data_to_display, # Use the output of your reactive expression
      options = list(
        pageLength = 10, # Number of rows to display per page
        lengthMenu = c(5, 10, 15, 20), # Options for number of rows per page
        searching = TRUE, # Enable search box
        filter = "top",
        paging = TRUE, # Enable pagination
        info = TRUE, # Display table information (e.g., "Showing 1 to 10 of 50 entries")
        ordering = TRUE # Enable column sorting
      ),
      rownames = FALSE # Do not display row names
    )
  })
  
  
  output$Teacher_Shortage_Data_Table <- DT::renderDT({
    # Ensure DBMProp is accessible within this reactive context.
    # If DBMProp is a reactive expression, call it like DBMProp().
    # If it's a static dataframe loaded globally, it's fine as is.
    DBMProp
  },
  rownames = FALSE,
  options = list(
    scrollX = TRUE,
    columnDefs = list(list(className = 'dt-center', targets = "_all"))
  ))
  
  output$School_Principal_Shortage_Data_Table <- DT::renderDT({
    
    uni %>% filter(Region != "BARMM") %>% mutate(Country = "Philippines") %>%
      select(Country, Designation) %>%
      group_by(Designation) %>%
      summarize(Count = n()) %>% 
      arrange(desc(Count))
  },
  rownames = FALSE,
  options = list(
    scrollX = TRUE,
    columnDefs = list(list(className = 'dt-center', targets = "_all"))
  ))
  
  # Assuming 'uni' is your original unfiltered data frame for School Principal data
  # Make sure 'uni' is loaded and available in your Shiny app's global environment or server.R
  
  # Define filtered_school_principal_data as a reactive expression in your server logic
  # This reactive expression will filter the 'uni' data based on the selected region and division.
  
  
  output$School_Principal_Regional_Graph <- renderPlotly({
    # Use the reactive filtered data
    current_filtered_data <- filtered_school_data_region()
    
    # --- Empty Data Handling ---
    if (nrow(current_filtered_data) == 0) {
      return(ggplotly(ggplot() +
                        annotate("text", x = 0.5, y = 0.5, label = "No data for selected regions/divisions") +
                        theme_void()))
    }
    
    # Prepare the data for plotting
    plot_data <- current_filtered_data %>%
      select(Region, Designation) %>%
      group_by(Region, Designation) %>%
      summarize(Count = n(), .groups = 'drop') # Ungroup after summarize
    
    plot_data_totals <- plot_data %>%
      group_by(Designation) %>%
      summarise(TotalCount = sum(Count))
    
    # Define the levels for Designation for consistent ordering
    designation_levels <- c("School Principal", "Teacher-in-Charge", "Officer-in-Charge")
    
    # Create the ggplot
    p <- ggplot(plot_data,
                aes(x = factor(Designation, levels = designation_levels), # Reorder regions based on overall count (across all designations)
                    y = Count,
                    fill = Region, # Ensure fill order is consistent
                    text = paste("Region: ", Region,
                                 "<br>Designation: ", Designation,
                                 "<br>Count: ", scales::comma(Count)))) + # Custom tooltip text
      geom_bar(stat = "identity", position = "stack", color = "black", size = 0.25) +
      geom_text(data = plot_data_totals,
                aes(x = Designation, y = TotalCount * 1.05, label = scales::comma(TotalCount)), # Modified line
                inherit.aes = FALSE,
                size = 3.5,
                color = "black") +
      labs(x = "Designation",
           y = "Count of Individuals",
           fill = "Region") +
      scale_y_continuous(labels = scales::comma) + # Format y-axis labels as comma-separated numbers
      theme_minimal() +
      theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
            legend.position = "bottom", # Position legend at the bottom
            plot.title = element_text(hjust = 0.5)) # Center the plot title
    
    # Convert ggplot to plotly, ensuring custom text is used for hover
    ggplotly(p, tooltip = "text", source = "schoolPrincipalRegionPlot") %>%
      layout(hoverlabel = list(bgcolor = "white"),
             margin = list(b = 100)) # Increase bottom margin for x-axis labels
  })
  
  output$School_Principal_All_List <- DT::renderDT({
    datatable(uni %>% filter(Region != "BARMM") %>% filter(Region %in% input$dashboard_region_filter) %>% select("Division","SchoolID","School.Name","Designation") %>% rename("School" = School.Name, "School ID" = SchoolID), extension = 'Buttons', rownames = FALSE, filter = 'top', options = list(scrollX = TRUE, pageLength = 10, columnDefs = list(list(className = 'dt-center', targets ="_all")), dom = 'Bfrtip', buttons = list('csv','excel','pdf','print')))})
  
  
  output$AOII_Data_Table <- DT::renderDT({
    uni %>% filter(Region != "BARMM") %>% 
      select(Clustering.Status) %>% 
      mutate(Country = "Philippines") %>%
      group_by(Country,Clustering.Status) %>%
      summarize(Count = n()) %>% 
      select(Clustering.Status, Count) %>% 
      rename("AO II Deployment" = Clustering.Status, "Total" = Count)
  },
  rownames = FALSE,
  options = list(
    scrollX = TRUE,
    columnDefs = list(list(className = 'dt-center', targets = "_all"))
  ))
  
  output$PDOI_Data_Table <- DT::renderDT({
    uni %>% filter(Region != "BARMM") %>%
      select(PDOI_Deployment) %>% 
      mutate(Country = "Philippines") %>%
      group_by(Country,PDOI_Deployment) %>%
      summarize(Count = n()) %>% 
      select(PDOI_Deployment, Count) %>% 
      rename("PDO I Deployment" = PDOI_Deployment, "Total" = Count)
  },
  rownames = FALSE,
  options = list(
    scrollX = TRUE,
    columnDefs = list(list(className = 'dt-center', targets = "_all"))
  ))
  
  # Assuming 'uni' is your original unfiltered data frame for AOII Deployment
  # Make sure 'uni' is loaded and available in your Shiny app's global environment or server.R
  
  # Assuming 'filtered_school_data_region' and 'filtered_pdoi_data' are reactive expressions
  # defined in your server logic.
  
  output$AOII_Regional_Graph <- renderPlotly({
    # Use the reactive filtered data
    current_filtered_data <- filtered_school_data_region()
    
    # --- Empty Data Handling ---
    if (nrow(current_filtered_data) == 0) {
      return(ggplotly(ggplot() +
                        annotate("text", x = 0.5, y = 0.5, label = "No data for selected regions/divisions") +
                        theme_void()))
    }
    
    # Prepare the data for plotting
    plot_data <- current_filtered_data %>%
      group_by(Region, Clustering.Status) %>%
      summarize(Total_Count = n(), .groups = 'drop')
    
    plot_data_totals <- plot_data %>%
      group_by(Clustering.Status) %>%
      summarise(TotalCount = sum(Total_Count))
    
    # Calculate total counts per Region for the overall labels on top of stacked bars
    total_labels_data <- plot_data %>%
      group_by(Region) %>%
      summarise(Grand_Total = sum(Total_Count), .groups = 'drop')
    
    # Define the levels for Clustering.Status for consistent stacking order in the legend and on the bars
    clustering_levels <- c("None Deployed", "Clustered", "Dedicated")
    
    # Create the ggplot
    p <- ggplot(plot_data,
                aes(x = factor(Clustering.Status, levels = clustering_levels), # Reorder regions based on overall total count for the region
                    y = Total_Count,
                    fill = Region, # Fill by Clustering.Status for coloring and consistent order
                    text = paste("Region: ", Region,
                                 "<br>Clustering Status: ", Clustering.Status,
                                 "<br>Count: ", scales::comma(Total_Count)))) + # Custom tooltip text
      geom_bar(stat = "identity", position = "stack", color = "black", size = 0.25) +
      geom_text(data = plot_data_totals,
                aes(x = Clustering.Status, y = TotalCount * 1.05, label = scales::comma(TotalCount)), # Modified line
                inherit.aes = FALSE,
                size = 3.5,
                color = "black") +
      labs(x = "Region",
           y = "Total Count of Schools") +
      scale_y_continuous(labels = scales::comma) + # Format y-axis labels as comma-separated numbers
      theme_minimal() +
      theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
            legend.position = "bottom", # Position legend at the bottom
            plot.title = element_text(hjust = 0.5)) # Center the plot title
    
    # Convert ggplot to plotly, ensuring custom text is used for hover
    ggplotly(p, tooltip = "text", source = "aoiiRegionPlot") %>%
      layout(hoverlabel = list(bgcolor = "white"),
             margin = list(b = 100)) # Increase bottom margin for x-axis labels
  })
  
  output$PDOI_Regional_Graph <- renderPlotly({
    # Use the reactive filtered data
    current_filtered_data <- filtered_school_data_region()
    
    # --- Empty Data Handling ---
    if (nrow(current_filtered_data) == 0) {
      return(ggplotly(ggplot() +
                        annotate("text", x = 0.5, y = 0.5, label = "No data for selected regions/divisions") +
                        theme_void()))
    }
    
    # Prepare the data for plotting
    plot_data <- current_filtered_data %>%
      group_by(Region, PDOI_Deployment) %>%
      summarize(Total_Count = n(), .groups = 'drop')
    
    plot_data_totals <- plot_data %>%
      group_by(PDOI_Deployment) %>%
      summarise(TotalCount = sum(Total_Count))
    
    # Calculate total counts per Region for the overall labels on top of stacked bars
    total_labels_data <- plot_data %>%
      group_by(Region) %>%
      summarise(Grand_Total = sum(Total_Count), .groups = 'drop')
    
    # Define the levels for PDOI_Deployment for consistent stacking order in the legend and on the bars
    pdoi_levels <- c("Without PDO I", "With PDO I")
    
    
    # Create the ggplot
    p <- ggplot(plot_data,
                aes(x = factor(PDOI_Deployment, levels = pdoi_levels), # Reorder regions based on overall total count for the region
                    y = Total_Count,
                    fill = Region, # Fill by PDOI_Deployment for coloring and consistent order
                    text = paste("Region: ", Region,
                                 "<br>PDO I Deployment: ", PDOI_Deployment,
                                 "<br>Total Count: ", scales::comma(Total_Count)))) + # Custom tooltip text
      geom_bar(stat = "identity", position = "stack", color = "black", size = 0.25) +
      geom_text(data = plot_data_totals,
                aes(x = PDOI_Deployment, y = TotalCount * 1.05, label = scales::comma(TotalCount)), # Modified line
                inherit.aes = FALSE,
                size = 3.5,
                color = "black") +
      labs(x = "PDOI_Deployment",
           y = "Total Count of Schools") +
      scale_y_continuous(labels = scales::comma) + # Format y-axis labels as comma-separated numbers
      theme_minimal() +
      theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
            legend.position = "bottom", # Position legend at the bottom
            plot.title = element_text(hjust = 0.5)) # Center the plot title
    
    # Convert ggplot to plotly, ensuring custom text is used for hover
    ggplotly(p, tooltip = "text", source = "pdoiRegionPlot") %>%
      layout(hoverlabel = list(bgcolor = "white"),
             margin = list(b = 100)) # Increase bottom margin for x-axis labels
  })
  
  # Assuming 'uni' is your original unfiltered data frame for Sufficiency data
  # Make sure 'uni' is loaded and available in your Shiny app's global environment or server.R
  
  # Define filtered_sufficiency_data_region as a reactive expression in your server logic
  # This reactive expression will filter the 'uni' data based on the selected region and division.
  filtered_sufficiency_data_region <- reactive({
    # Ensure uni data is available.
    req(uni)
    
    # Start with the full dataset
    data <- uni
    
    # Data transformation as per your original code, but applied to the filtered data
    data %>%
      select(SchoolID, Region, Division, Teacher, Classroom, School.Head, AO) %>%
      mutate(
        Teacher = as.numeric(Teacher),
        Classroom = as.numeric(Classroom),
        School.Head = as.numeric(School.Head),
        AO = as.numeric(AO) # Ensure AO is also numeric
      ) %>%
      mutate(
        Teacher.Sufficiency = case_when(
          (Teacher >= 0 & Teacher <= 0.25) ~ "Critically Under-Resourced",
          (Teacher > 0.25 & Teacher <= 0.5) ~ "Under-Resourced",
          (Teacher > 0.5 & Teacher <= 0.75) ~ "Resource-Deficient",
          (Teacher > 0.75 & Teacher <= 0.9) ~ "Adequately Resourced",
          (Teacher > 0.9 & Teacher <= 1) ~ "Generously Resourced",
          Teacher > 1 ~ "For Validation",
          TRUE ~ NA_character_
        ),
        Classroom.Sufficiency = case_when(
          (Classroom >= 0 & Classroom <= 0.25) ~ "Critically Under-Resourced",
          (Classroom > 0.25 & Classroom <= 0.5) ~ "Under-Resourced",
          (Classroom > 0.5 & Classroom <= 0.75) ~ "Resource-Deficient",
          (Classroom > 0.75 & Classroom <= 0.9) ~ "Adequately Resourced",
          (Classroom > 0.9 & Classroom <= 1) ~ "Generously Resourced",
          Classroom > 1 ~ "For Validation",
          TRUE ~ NA_character_
        ),
        SH.Sufficiency = case_when(
          (School.Head >= 0 & School.Head <= 0.25) ~ "Critically Under-Resourced",
          (School.Head > 0.25 & School.Head <= 0.5) ~ "Under-Resourced",
          (School.Head > 0.5 & School.Head <= 0.75) ~ "Resource-Deficient",
          (School.Head > 0.75 & School.Head <= 0.9) ~ "Adequately Resourced",
          (School.Head > 0.9 & School.Head <= 1) ~ "Generously Resourced",
          School.Head > 1 ~ "For Validation",
          TRUE ~ NA_character_
        ),
        AO.Sufficiency = case_when(
          (AO >= 0 & AO <= 0.25) ~ "Critically Under-Resourced",
          (AO > 0.25 & AO <= 0.5) ~ "Under-Resourced",
          (AO > 0.5 & AO <= 0.75) ~ "Resource-Deficient",
          (AO > 0.75 & AO <= 0.9) ~ "Adequately Resourced",
          (AO > 0.9 & AO <= 1) ~ "Generously Resourced",
          AO > 1 ~ "For Validation",
          TRUE ~ NA_character_
        )
      ) %>%
      pivot_longer(
        cols = c(Teacher.Sufficiency, Classroom.Sufficiency, SH.Sufficiency, AO.Sufficiency),
        names_to = "Criteria",
        values_to = "Sufficiency"
      ) %>%
      filter(!is.na(Sufficiency)) %>% # Filter NA sufficiency before grouping
      group_by(Region, Criteria, Sufficiency) %>%
      summarise(SufficiencyTotal = n(), .groups = 'drop_last') %>%
      # Calculate the total for each Region and Criteria for percentage calculation
      mutate(RegionCriteriaTotal = sum(SufficiencyTotal)) %>%
      mutate(Percentage = (SufficiencyTotal / RegionCriteriaTotal)) %>%
      ungroup() %>%
      mutate(Sufficiency = factor(Sufficiency, levels = c(
        "Critically Under-Resourced",
        "Under-Resourced",
        "Resource-Deficient",
        "Adequately Resourced",
        "Generously Resourced",
        "For Validation"))) # Ensure consistent order for plotting
  })
  
  
  # Assuming 'filtered_sufficiency_data_region' is a reactive expression that provides
  # the data, similar to how it was introduced in previous responses.
  # Make sure your 'filtered_sufficiency_data_region' reactive is defined in your server logic.
  
  output$Sufficiency_Regional_Graph <- renderPlotly({
    # Use the reactive filtered data
    current_filtered_data_for_plot <- filtered_sufficiency_data_region()
    
    # Filter by the selected category from input$SuffOpt
    plot_data <- current_filtered_data_for_plot %>%
      filter(Criteria == input$SuffOpt)
    
    # --- Empty Data Handling ---
    if (nrow(plot_data) == 0) {
      return(ggplotly(ggplot() +
                        annotate("text", x = 0.5, y = 0.5, label = paste("No data for selected regions/divisions and category:", input$SuffOpt)) +
                        theme_void()))
    }
    
    # Determine the title based on the selected input$SuffOpt
    plot_title <- switch(input$SuffOpt,
                         "Teacher.Sufficiency" = "Regional Teacher Sufficiency by Category",
                         "Classroom.Sufficiency" = "Regional Classroom Sufficiency by Category",
                         "SH.Sufficiency" = "Regional School Principal Sufficiency by Category",
                         "AO.Sufficiency" = "Regional AO Sufficiency by Category",
                         "Regional Sufficiency Overview")
    
    # Calculate total percentages per region for the overall labels on top of stacked bars
    # This needs to be done *after* filtering by Criteria
    total_labels_data <- plot_data %>%
      group_by(Region) %>%
      summarise(Grand_Total_Percentage = sum(Percentage), .groups = 'drop') # Sum percentages for labeling
    
    # Create the ggplot
    p <- ggplot(plot_data,
                aes(x = reorder(Region, -Percentage), # Reorder regions based on the percentage of the current 'Criteria' and 'Sufficiency'
                    y = Percentage,
                    fill = Sufficiency, # Fill by Sufficiency for stacking
                    text = paste("Region: ", Region,
                                 "<br>Sufficiency: ", Sufficiency,
                                 "<br>Percentage: ", scales::percent(Percentage, accuracy = 0.1),
                                 "<br>Total Schools (Category): ", scales::comma(SufficiencyTotal)))) + # Custom tooltip text
      geom_bar(stat = "identity", position = "stack", color = "black", size = 0.25) + # Changed to position="stack" for stacked bars
      labs(x = "Region",
           y = "Percentage",
           fill = "Sufficiency Category") +
      scale_y_continuous(labels = scales::percent, limits = c(0, 1), breaks = seq(0, 1, by = 0.2)) + # Format y-axis as percentage
      theme_minimal() +
      theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
            legend.position = "bottom", # Position legend at the bottom
            plot.title = element_text(hjust = 0.5)) # Center the plot title
    
    # Convert ggplot to plotly, ensuring custom text is used for hover
    ggplotly(p, tooltip = "text", source = "sufficiencyRegionPlot") %>%
      layout(hoverlabel = list(bgcolor = "white"),
             margin = list(b = 100)) # Increase bottom margin for x-axis labels
  })
  
  output$Sufficiency_All_List <- DT::renderDT({
    datatable(
      uni %>% filter(Region %in% input$dashboard_region_filter) %>% 
        select(SchoolID, School.Name, Region, Division, Teacher, Classroom, School.Head, AO) %>%
        mutate(
          Teacher = as.numeric(Teacher),
          Classroom = as.numeric(Classroom),
          School.Head = as.numeric(School.Head)
        ) %>%
        mutate(
          Teacher.Sufficiency = case_when(
            (Teacher >= 0 & Teacher <= 0.25) ~ "Critically Under-Resourced",
            (Teacher > 0.25 & Teacher <= 0.5) ~ "Under-Resourced",
            (Teacher > 0.5 & Teacher <= 0.75) ~ "Resource-Deficient",
            (Teacher > 0.75 & Teacher <= 0.9) ~ "Adequately Resourced",
            (Teacher > 0.9 & Teacher <= 1) ~ "Generously Resourced",
            Teacher > 1 ~ "For Validation",
            TRUE ~ NA_character_
          ),
          Classroom.Sufficiency = case_when(
            (Classroom >= 0 & Classroom <= 0.25) ~ "Critically Under-Resourced",
            (Classroom > 0.25 & Classroom <= 0.5) ~ "Under-Resourced",
            (Classroom > 0.5 & Classroom <= 0.75) ~ "Resource-Deficient",
            (Classroom > 0.75 & Classroom <= 0.9) ~ "Adequately Resourced",
            (Classroom > 0.9 & Classroom <= 1) ~ "Generously Resourced",
            Classroom > 1 ~ "For Validation",
            TRUE ~ NA_character_
          ),
          SH.Sufficiency = case_when(
            (School.Head >= 0 & School.Head <= 0.25) ~ "Critically Under-Resourced",
            (School.Head > 0.25 & School.Head <= 0.5) ~ "Under-Resourced",
            (School.Head > 0.5 & School.Head <= 0.75) ~ "Resource-Deficient",
            (School.Head > 0.75 & School.Head <= 0.9) ~ "Adequately Resourced",
            (School.Head > 0.9 & School.Head <= 1) ~ "Generously Resourced",
            School.Head > 1 ~ "For Validation",
            TRUE ~ NA_character_
          ),
          AO.Sufficiency = case_when(
            (AO >= 0 & AO <= 0.25) ~ "Critically Under-Resourced",
            (AO > 0.25 & AO <= 0.5) ~ "Under-Resourced",
            (AO > 0.5 & AO <= 0.75) ~ "Resource-Deficient",
            (AO > 0.75 & AO <= 0.9) ~ "Adequately Resourced",
            (AO > 0.9 & AO <= 1) ~ "Generously Resourced",
            AO > 1 ~ "For Validation",
            TRUE ~ NA_character_
          )
        ) %>% select("SchoolID","School.Name","Division","Teacher.Sufficiency","Classroom.Sufficiency","SH.Sufficiency","AO.Sufficiency") %>% rename("School" = School.Name, "School ID" = SchoolID, "Teacher Sufficiency" = Teacher.Sufficiency, "Classroom Sufficiency" = Classroom.Sufficiency, "School Principal Sufficiency" = SH.Sufficiency, "AO Sufficiency" = AO.Sufficiency), extension = 'Buttons', rownames = FALSE, filter ='top', options = list(scrollX = TRUE, pageLength = 10, columnDefs = list(list(className = 'dt-center', targets ="_all")), dom = 'Bfrtip', buttons = list('csv','excel','pdf','print')))})
  
  filtered_divisions_choices <- reactive({
    req(input$selected_region) 
    
    divisions <- EFD_Projects %>%
      filter(Region %in% input$selected_region) %>%
      distinct(Division) %>%
      pull(Division) %>%
      sort()
    
    return(divisions)
  })
  
  # Update Division pickerInput choices and selected values based on selected_region
  observeEvent(filtered_divisions_choices(), {
    # Get the currently selected division(s) from the input
    current_selected <- input$selected_division
    
    # Get the new available choices for divisions from the reactive expression
    new_choices <- filtered_divisions_choices()
    
    # Determine what should be selected
    selected_value <- NULL # Initialize selected_value to NULL
    
    # Logic for determining the selected value:
    if (length(new_choices) > 0) {
      # If there are new choices available:
      if (!is.null(current_selected) && all(current_selected %in% new_choices)) {
        # Case 1: If something was previously selected AND all previous selections are still valid among the new choices,
        # then keep those previous selections. This handles changes in region filters while preserving user intent.
        selected_value <- current_selected
      } else {
        # Case 2: If there's nothing previously selected, OR if the previous selections are no longer valid
        # (e.g., region filter changed and old divisions are gone),
        # then default to selecting ALL of the new available choices. This is crucial for the initial load.
        selected_value <- new_choices
      }
    }
    # Case 3: If new_choices is empty (e.g., no region selected that has divisions),
    # selected_value remains NULL, which correctly results in "Nothing selected".
    
    # Update the picker input with the determined choices and selection
    updatePickerInput(
      session = session,
      inputId = "selected_division",
      choices = new_choices,
      selected = new_choices
    )
  }, ignoreNULL = FALSE, ignoreInit = FALSE) # Ensure ignoreInit is FALSE to run on app start
  
  # Reactive expression for filtered data (by Category, Region AND Division)
  filtered_data <- reactive({
    req(input$selected_category, input$selected_region, input$selected_division)
    
    data_filtered <- EFD_Projects %>% 
      filter(Category %in% input$selected_category) %>%
      filter(Region %in% input$selected_region) %>%
      filter(Division %in% input$selected_division) 
    
    print("--- filtered_data() Check ---")
    print("Categories selected in input:")
    print(input$selected_category)
    print("Regions selected in input:")
    print(input$selected_region) 
    print("Divisions selected in input:") 
    print(input$selected_division)      
    print("Head of filtered_data():")
    print(head(data_filtered))
    print("Unique categories in filtered_data():")
    print(length(unique(data_filtered$Category)))
    print("Rows in filtered_data():")
    print(nrow(data_filtered))
    print("-----------------------------")
    return(data_filtered)
  })
  
  filtered_data2 <- reactive({
    req(input$selected_category, input$selected_region, input$selected_division)
    
    data_filtered <- EFD_Pipeline %>%
      mutate(Allocation = as.numeric(Allocation)) %>% 
      filter(FundingYear <= 2030) %>% 
      filter(Category %in% input$selected_category) %>%
      filter(Region %in% input$selected_region) %>%
      filter(Division %in% input$selected_division) 
    
    print("--- filtered_data() Check ---")
    print("Categories selected in input:")
    print(input$selected_category)
    print("Regions selected in input:")
    print(input$selected_region) 
    print("Divisions selected in input:") 
    print(input$selected_division)      
    print("Head of filtered_data():")
    print(head(data_filtered))
    print("Unique categories in filtered_data():")
    print(length(unique(data_filtered$Category)))
    print("Rows in filtered_data():")
    print(nrow(data_filtered))
    print("-----------------------------")
    return(data_filtered)
  })
  
  filtered_data3 <- reactive({
    req(input$selected_category, input$selected_region, input$selected_division)
    
    data_filtered <- EFD_Projects %>% 
      filter(Category %in% input$selected_category) %>%
      filter(Region %in% input$selected_region) %>%
      filter(Division %in% input$selected_division) 
    
    print("--- filtered_data() Check ---")
    print("Categories selected in input:")
    print(input$selected_category)
    print("Regions selected in input:")
    print(input$selected_region) 
    print("Divisions selected in input:") 
    print(input$selected_division)      
    print("Head of filtered_data():")
    print(head(data_filtered))
    print("Unique categories in filtered_data():")
    print(length(unique(data_filtered$Category)))
    print("Rows in filtered_data():")
    print(nrow(data_filtered))
    print("-----------------------------")
    return(data_filtered)
  })
  
  # Get unique funding years for dynamic value boxes
  alloc_filter <- EFD_Projects
  unique_funding_years <- sort(unique(alloc_filter$FundingYear))
  
  # Get unique categories and regions for filter choices (initial load)
  all_categories <- sort(unique(alloc_filter$Category))
  all_regions <- sort(unique(alloc_filter$Region))
  all_divisions <- sort(unique(alloc_filter$Division)) # All divisions for initial selection
  
  # --- Render Value Boxes for Each Funding Year (using renderText for value) ---
  lapply(unique_funding_years, function(year) {
    output[[paste0("vb_total_alloc_", year)]] <- renderText({
      total_alloc_for_year <- filtered_data() %>%
        filter(FundingYear == year) %>%
        summarise(Total = sum(Allocation, na.rm = TRUE)) %>%
        pull(Total)
      paste0(scales::comma(total_alloc_for_year))
    })
  })
  
  # --- Plot 1: Allocation per Category per FundingYear (Grouped Bar Graph) ---
  output$allocationStackedBar <- renderPlotly({
    # Define the custom color palette
    category_colors <- c(
      "ALS-CLC" = "#E41A1C",
      "Electrification" = "#FF7F00",
      "Gabaldon" = "#4DAF4A",
      "Health" = "#1B9E77",
      "LMS" = "#17BECF",
      "New Construction" = "#A6CEE3",
      "QRF" = "#984EA3",
  "Repairs" = "#F781BF",
  "SPED-ILRC" = "#FDBF6F", # You can choose any color here, this is an example
  "LIH" = "#CAB2D6"       # And another one for LIH
)
    
    if (nrow(filtered_data3()) == 0) {
      return(ggplotly(ggplot() + annotate("text", x = 0.5, y = 0.5, label = "No data for selected categories/regions/divisions") + theme_void()))
    }
    
    plot_data_stacked <- filtered_data3() %>% 
      group_by(FundingYear, Category) %>%
      summarise(TotalAllocation = sum(Allocation, na.rm = TRUE), .groups = 'drop') %>%
      ungroup() %>%
      # Calculate percentage of each category within its FundingYear
      group_by(FundingYear) %>%
      mutate(
        TotalAllocationYear = sum(TotalAllocation, na.rm = TRUE),
        Percentage = (TotalAllocation / TotalAllocationYear) * 100
      ) %>%
      ungroup()
    
    plot_data_totals <- plot_data_stacked %>% 
      group_by(FundingYear) %>% 
      summarise(GrandTotalAllocation = sum(TotalAllocation, na.rm = TRUE), .groups = 'drop')
    
    p <- ggplot(plot_data_stacked, aes(x = factor(FundingYear), y = TotalAllocation, fill = Category,
                                       text = paste("Year: ", FundingYear,
                                                    "<br>Category: ", Category,
                                                    "<br>Allocation: PhP ", scales::dollar(round(TotalAllocation,2), prefix = "₱"),
                                                    "<br>Percentage: ", round(Percentage, 2), "%"), # <--- ADDED PERCENTAGE HERE
                                       key = paste(FundingYear, Category, "allocation"))) +
      geom_bar(stat = "identity", position = "stack", color = "black", size = 0.25, size = 0.25) +
      geom_text(data = plot_data_totals,
                aes(x = factor(FundingYear), y = GrandTotalAllocation * 1.05, label = scales::dollar(GrandTotalAllocation, prefix = "₱")), # Modified line
                inherit.aes = FALSE,
                size = 3.5,
                color = "black") +
      labs(title = "Total Allocation per Category per Funding Year", x = "Funding Year", y = "Total Allocation (PhP)", fill = "BEFF Allocation") +
      scale_y_continuous(labels = scales::comma) +
      scale_fill_manual(values = category_colors) +
      theme_minimal() +
      theme(legend.position = "bottom")
    
    ggplotly(p, tooltip = "text", source = "stackedBarPlot") %>% layout(hoverlabel = list(bgcolor = "white"))
  })
  
  # --- Plot 3: Allocation Trend per Category per FundingYear (Line Graph) ---
  output$allocationTrendLine <- renderPlotly({
    # Define the custom color palette
    category_colors <- c(
      "ALS-CLC" = "#E41A1C",
      "Electrification" = "#FF7F00",
      "Gabaldon" = "#4DAF4A",
      "Health" = "#1B9E77",
      "LMS" = "#17BECF",
      "New Construction" = "#A6CEE3",
      "QRF" = "#984EA3",
      "Repairs" = "#F781BF",
      "SPED-ILRC" = "#FDBF6F", # You can choose any color here, this is an example
      "LIH" = "#CAB2D6"       # And another one for LIH
    )
    
    if (nrow(filtered_data3()) == 0) {
      return(ggplotly(ggplot() + annotate("text", x = 0.5, y = 0.5, label = "No data for selected categories/regions/divisions") + theme_void()))
    }
    
    plot_data_trend <- filtered_data3() %>% 
      group_by(FundingYear, Category) %>%
      summarise(TotalAllocation = sum(Allocation, na.rm = TRUE), .groups = 'drop') %>%
      ungroup()
    
    p <- ggplot(plot_data_trend, aes(x = FundingYear, y = TotalAllocation, color = Category, group = Category,
                                     text = paste("Year: ", FundingYear,
                                                  "<br>Category: ", Category,
                                                  "<br>Allocation: PhP ", scales::comma(TotalAllocation)))) +
      geom_line(size = 1) +
      geom_point(size = 2) + # Removed aes(color = Category) here as it's already defined globally in ggplot()
      labs(title = "Allocation Trend per Category", x = "Funding Year", y = "Total Allocation (PhP)") +
      scale_y_continuous(labels = scales::comma) +
      scale_x_continuous(breaks = min(plot_data_trend$FundingYear):max(plot_data_trend$FundingYear)) +
      scale_color_manual(values = category_colors) + # <--- ADDED CUSTOM COLORS HERE
      theme_minimal() +
      theme(legend.position = "bottom")
    
    ggplotly(p, tooltip = "text") %>% layout(hoverlabel = list(bgcolor = "white"))
  })
  
  # --- NEW PLOT: Average Completion per Category per Funding Year (Bar Graph) ---
  output$completionByCategoryPlot <- renderPlotly({
    # Define the custom color palette based on your image
    category_colors <- c(
      "ALS-CLC" = "#E41A1C",       # Red
      "Electrification" = "#FF7F00",   # Orange
      "Gabaldon" = "#4DAF4A",       # Green
      "Health" = "#1B9E77",         # Darker Teal-Green
      "LMS" = "#17BECF",            # Teal
      "New Construction" = "#A6CEE3", # Light Blue
      "QRF" = "#984EA3",           # Purple
      "Repairs" = "#F781BF"        # Pink
    )
    
    filtered_data_no_2025 <- filtered_data3() %>%
      filter(FundingYear != 2025)
    
    if (nrow(filtered_data_no_2025) == 0) {
      return(plotly_empty() %>% layout(title = "No data available for selected filters."))
    }
    
    completion_threshold <- 0.0001
    
    category_yearly_avg <- filtered_data_no_2025 %>%
      group_by(FundingYear, Category) %>%
      summarise(AverageCompletion = mean(Completion, na.rm = TRUE), .groups = 'drop') %>%
      filter(AverageCompletion > completion_threshold)
    
    if (nrow(category_yearly_avg) == 0) {
      return(plotly_empty() %>% layout(title = "No data available after filtering for non-zero completion."))
    }
    
    plot_data <- category_yearly_avg %>%
      mutate(FundingYear = factor(FundingYear, levels = sort(unique(FundingYear)))) %>%
      mutate(Category = factor(Category)) %>%
      arrange(FundingYear)
    
    plot_ly(plot_data, x = ~FundingYear, y = ~AverageCompletion, color = ~Category,
            colors = category_colors, # <--- ADDED CUSTOM COLORS HERE
            type = 'bar',
            marker = list(line = list(color = 'black', width = 1)),
            text = ~paste("Year: ", FundingYear, "<br>Category: ", Category, "<br>Avg Completion: ", scales::percent(AverageCompletion, accuracy = 1)),
            hoverinfo = "text",
            source = "completionPlot",
            key = ~paste(FundingYear, Category, "completion")
    ) %>%
      layout(title = "Average Completion per Category per Year",
             xaxis = list(title = "Funding Year", type = "category",
                          categoryorder = "array", categoryarray = levels(plot_data$FundingYear)),
             yaxis = list(title = "Average Completion (%)", tickformat = ".0%", range = c(0, 1)),
             barmode = 'group',
             legend = list(traceorder = "reversed")
      )
  })
  
  output$PipelinePrograms <- renderPlotly({
    # Define the custom color palette
    category_colors <- c(
      "ALS-CLC" = "#E41A1C",        # Red
      "Electrification" = "#FF7F00",    # Orange
      "Gabaldon" = "#4DAF4A",      # Green
      "Health" = "#1B9E77",         # Darker Teal-Green
      "LMS" = "#17BECF",            # Teal
      "New Construction" = "#A6CEE3", # Light Blue
      "QRF" = "#984EA3",            # Purple
      "Repairs" = "#F781BF"         # Pink
    )
    
    # Filter data for FundingYear 2026 onwards
    filtered_data_2026_onwards <- filtered_data2() %>%
      filter(FundingYear >= 2026)
    
    # Check if there's any data after filtering
    if (nrow(filtered_data_2026_onwards) == 0) {
      # Changed to ggplotly and annotate for consistency with allocationStackedBar
      return(ggplotly(ggplot() + annotate("text", x = 0.5, y = 0.5, label = "No data available for 2026 onwards.") + theme_void()))
    }
    
    # Summarize data: Sum Allocation per year and category
    pipeline_programs_summary <- filtered_data_2026_onwards %>%
      group_by(FundingYear, Category) %>%
      summarise(TotalAllocation = sum(Allocation, na.rm = TRUE), .groups = 'drop') %>%
      # Calculate percentage of each category within its FundingYear
      group_by(FundingYear) %>%
      mutate(
        TotalAllocationYear = sum(TotalAllocation, na.rm = TRUE),
        Percentage = (TotalAllocation / TotalAllocationYear) * 100
      ) %>%
      ungroup()
    
    # Check if there's any data after summarization
    if (nrow(pipeline_programs_summary) == 0) {
      # Changed to ggplotly and annotate for consistency
      return(ggplotly(ggplot() + annotate("text", x = 0.5, y = 0.5, label = "No pipeline program data available after aggregation.") + theme_void()))
    }
    
    # Prepare data for plotting (Factor levels for x-axis order)
    plot_data_pipeline <- pipeline_programs_summary %>%
      mutate(FundingYear = factor(FundingYear, levels = sort(unique(FundingYear))))
    # Category already factor from group_by or assumed from previous step. No need for explicit mutate(Category = factor(Category)) if it's already a factor.
    
    # Create the ggplot object
    p <- ggplot(plot_data_pipeline, aes(x = FundingYear, y = TotalAllocation, fill = Category,
                                        text = paste("Year: ", FundingYear,
                                                     "<br>Category: ", Category,
                                                     "<br>Total Allocation: PhP ", scales::comma(TotalAllocation),
                                                     "<br>Percentage: ", round(Percentage, 2), "%"),
                                        key = paste(FundingYear, Category, "pipeline"))) +
      geom_bar(stat = "identity", position = "stack") + # Stacked bar chart
      labs(title = "Total Allocation for Pipeline Programs per Category per Year (2026 Onwards)",
           x = "Funding Year",
           y = "Total Allocation (PhP)") +
      scale_y_continuous(labels = scales::comma) + # Format y-axis as currency with commas
      scale_fill_manual(values = category_colors) + # Apply custom colors
      theme_minimal() + # Minimal theme
      theme(legend.position = "bottom") # Place legend at the bottom
    
    # Convert ggplot to plotly, specifying tooltip to use the 'text' aesthetic
    ggplotly(p, tooltip = "text", source = "pipelinePlot") %>% layout(hoverlabel = list(bgcolor = "white"))
  })
  
  # --- Reactive Values to store the latest click data ---
  rv <- reactiveValues(
    latest_click_key = NULL,
    latest_click_type = NULL 
  )
  
  # Observer for allocation plot clicks
  observeEvent(event_data("plotly_click", source = "stackedBarPlot"), {
    click_data <- event_data("plotly_click", source = "stackedBarPlot")
    if (!is.null(click_data)) {
      rv$latest_click_key <- click_data$key
      rv$latest_click_type <- "allocation"
    }
  })
  
  # Observer for completion plot clicks
  observeEvent(event_data("plotly_click", source = "completionPlot"), {
    click_data <- event_data("plotly_click", source = "completionPlot")
    if (!is.null(click_data)) {
      rv$latest_click_key <- click_data$key
      rv$latest_click_type <- "completion"
    }
  })
  
  # In your server.R file, after your output$PipelinePrograms definition
  
  # Define a reactiveValues object to store click data (if you don't have one already)
  # This should be defined once at the top of your server function:
  # rv <- reactiveValues(latest_click_key = NULL, latest_click_type = NULL)
  
  observeEvent(event_data("plotly_click", source = "pipelinePlot"), { # <--- Changed source to "pipelinePlot"
    click_data <- event_data("plotly_click", source = "pipelinePlot") # <--- Changed source
    if (!is.null(click_data)) {
      # The 'key' attribute you defined in plot_ly is passed here
      # For PipelinePrograms, your key is ~paste(FundingYear, Category, "pipeline")
      rv$latest_click_key <- click_data$key
      rv$latest_click_type <- "pipeline" # <--- Set type to "pipeline" for this graph
      
      # Optional: Print click data to console for debugging
      # print("Click on PipelinePrograms plot:")
      # print(click_data)
      # print(paste("Clicked Key:", click_data$key))
    }
  })
  
  # Your existing output$PipelinePrograms definition would be above this observeEvent
  # output$PipelinePrograms <- renderPlotly({ ... })
  
  # --- Reactive Table Data based on Plotly Click (Updated for Multiple Sources) ---
  output$projectDetailTable <- DT::renderDT(server = TRUE, {
    # Initialize table_to_display with the pre-filtered data
    table_to_display <- filtered_data3()
    
    # Only apply additional filters if a plot segment has been clicked
    if (!is.null(rv$latest_click_key) && !is.null(rv$latest_click_type)) {
      clicked_key <- rv$latest_click_key
      clicked_type <- rv$latest_click_type
      
      clicked_split <- strsplit(as.character(clicked_key), " ")[[1]]
      
      if (length(clicked_split) >= 3) {
        clicked_year <- as.numeric(clicked_split[1])
        clicked_category <- paste(clicked_split[2:(length(clicked_split)-1)], collapse = " ")
        
        # Now, apply specific filters to the ALREADY-FILTERED data
        if (clicked_type == "completion") {
          table_to_display <- table_to_display %>%
            filter(FundingYear == clicked_year, Category == clicked_category, FundingYear != 2025)
        } else if (clicked_type == "allocation") {
          table_to_display <- table_to_display %>%
            filter(FundingYear == clicked_year, Category == clicked_category)
        } else if (clicked_type == "pipeline") {
          table_to_display <- table_to_display %>%
            filter(FundingYear == clicked_year, Category == clicked_category, FundingYear >= 2026)
        }
      }
    }
    
    table_to_display <- table_to_display %>%
      select(Region, Division, SchoolName, FundingYear, Category, Allocation, Completion)
    
    if (nrow(table_to_display) == 0) {
      return(DT::datatable(data.frame(Message = "No project data available for this selection."), options = list(dom = 't')))
    }
    
    DT::datatable(table_to_display %>% mutate(FundingYear = as.character(FundingYear)),
                  options = list(
                    pageLength = 10),
                  filter = 'top',
                  rownames = FALSE
    ) %>%
      formatCurrency('Allocation', currency = 'PhP') %>%
      formatPercentage('Completion', digits = 1)
  })
  
  output$SchoolBuilding_MasterPlan <- renderPlot({
    # Ensure SBMaster is a data frame. If it's a reactive expression, use SBMaster()
    mainreactreghist <- SBMaster 
    
    # Define a consistent dodge width for both bars and text
    dodge_width <- 0.9 # Default for position_dodge is 0.9
    
    ggplot(mainreactreghist, aes(x = reorder(Region, Classrooms, FUN = sum), # Reorder by sum of classrooms for better overall ordering
                                 y = Classrooms, 
                                 fill = factor(Year, levels = c("2025","2026","2027","2028","2029","2030")))) +
      geom_bar(stat = "identity", position = position_dodge(width = dodge_width), color = "black") + 
      labs(
        title = "School Building Master Plan", # Main plot title
        x = "Region", 
        y = "Number of Classrooms",
        fill = "Year" # Legend title
      ) +
      scale_fill_discrete(guide = guide_legend(nrow = 1)) + 
      theme_minimal() + # Use a clean theme
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1), # Keep x-axis labels slanted
        legend.position = "top",
        legend.direction = "horizontal",
        legend.box = "horizontal",# <--- Make the legend horizontal
        legend.justification = "center", # <--- Center the legend items
        legend.box.just = "center", # <--- Center the legend box if multiple legends exist
        plot.title = element_text(hjust = 0.5) # Center the plot title
      )
  })
  
  output$DataBuilder_HROD_SDO <- renderUI({
    
    divisions_vector <- uni %>% 
      filter(Region %in% input$DataBuilder_HROD_Region) %>%
      pull(Division)
    
    # Ensure divisions are unique and sorted
    sorted_unique_divisions <- sort(unique(divisions_vector))
    
    # Create the final choices list
    choices_list <- c(sorted_unique_divisions)
    
    # Check if there are any divisions to display
    if (length(choices_list) > 1) {
      pickerInput(
        inputId = "DataBuilder_SDO",
        label = "Select a Division:",
        choices = choices_list,
        selected = choices_list,
        multiple = TRUE,
        options = pickerOptions(
          actionsBox = TRUE,
          liveSearch = TRUE,
          header = "Select Categories",
          title = "No Category Selected",
          selectedTextFormat = "count > 3",
          dropupAuto = FALSE,
          dropup = FALSE
        ),
        choicesOpt = list()
      )
    } else {
      # Return NULL if there are no divisions to display.
      return(NULL)
    }
  })
  
  output$HROD_Table <- DT::renderDT(server = TRUE, {
    # Get all unique choices from the original dataset for comparison
    all_regions <- unique(uni$Region)
    all_divisions <- unique(uni$Division)
    
    # Determine if all choices are selected in the pickerInputs
    all_regions_selected <- length(input$DataBuilder_HROD_Region) == length(all_regions)
    all_divisions_selected <- length(input$DataBuilder_SDO) == length(all_divisions)
    
    # Create a reactive dataframe for your table
    filtered_uni <- reactive({
      
      # If both 'all regions' and 'all divisions' are selected, return the full dataset
      if (all_regions_selected && all_divisions_selected) {
        return(uni)
      } else {
        # Otherwise, apply your normal filtering logic
        uni %>%
          filter(Region %in% input$DataBuilder_HROD_Region) %>%
          filter(Division %in% input$DataBuilder_SDO)
      }
    })
    
    # Render the data table using the reactive dataframe
    datatable(
      filtered_uni() %>%
        mutate(across(where(is.character), ~ str_replace_all(., "ñ", "n"))) %>%
        mutate(across(18:43, ~ if_else(. == 0, "-", as.character(.)))) %>%
        select(Region, School.Name, SchoolID, Division, District, input$School_Data_Toggles,
               input$Teaching_Data_Toggles, input$NTP_Data_Toggles, input$Enrolment_Data_Toggles,
               input$Specialization_Data_Toggles, input$EFD_Data_Toggles) %>%
        arrange(desc(District)),
      extension = 'Buttons',
      filter = 'top',
      options = list(
        scrollX = TRUE,
        pageLength = 10,
        columnDefs = list(list(className = 'dt-center', targets = "_all")),
        rownames = FALSE,
        dom = 'Bfrtip',
        buttons = list('csv', 'excel', 'print')
      )
    )
  })
  
  output$explorer_efd_data_table <- DT::renderDT(server = FALSE, {datatable(EFDDB %>% filter(Region == input$explorer_efd_region_filter) %>% filter(Division == input$explorer_efd_SDO) %>% arrange(District), extension = 'Buttons', filter = 'top', options = list(scrollX = TRUE, pageLength = 10, columnDefs = list(list(className = 'dt-center', targets ="_all")), rownames = FALSE, dom = 'Bfrtip', buttons = list('csv','excel','print')))})
  
  output$explorer_masterlist_data_table <- DT::renderDT(server = FALSE, {datatable(EFDMP %>% filter(Region == input$explorer_masterlist_region_filter) %>% filter(Division == input$explorer_masterlist_SDO) %>% arrange(desc(FundingYear)) %>% select(Region, Division, District, SchoolID, School.Name,FundingYear,Category,Allocation,Completion,Status), extension = 'Buttons', filter = 'top', options = list(scrollX = TRUE, pageLength = 10, columnDefs = list(list(className = 'dt-center', targets ="_all")), rownames = FALSE, dom = 'Bfrtip', buttons = list('csv','excel','print')))})
  
  output$explorer_efd_division_filter <- renderUI({
    req(input$explorer_efd_region_filter) # Ensure region is selected before updating division
    selectInput("explorer_efd_SDO","Select a Division:", 
                choices = c(unique(df[df$Region==input$explorer_efd_region_filter,"Division"])),
                selected = "")
  })
  
  output$explorer_masterlist_division_filter <- renderUI({
    req(input$explorer_masterlist_region_filter) # Ensure region is selected before updating division
    selectInput("explorer_masterlist_SDO","Select a Division:", 
                choices = c(unique(df[df$Region==input$explorer_masterlist_region_filter,"Division"])),
                selected = "")
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
    
    output$TextTable <- DT::renderDT(server = FALSE, {datatable(df1() %>% select("Region","Division","Legislative.District","Municipality","School.Name") %>% rename("School" = School.Name), extension = 'Buttons', rownames = FALSE, options = list(scrollX = TRUE, pageLength = 10, columnDefs = list(list(className = 'dt-center', targets ="_all")), dom = 'lrtip'), filter = "top")})
  })
  
  output$TextMapping <- renderLeaflet({
    leaflet() %>% 
      setView(lng = 122, lat = 13, zoom =5) %>%
      addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>% 
      addProviderTiles(providers$CartoDB.Positron, group = "Road Map") %>% 
      addMeasure(position = "topright", primaryLengthUnit = "kilometers", primaryAreaUnit = "sqmeters") %>% 
      addLayersControl(
        baseGroups = c("Satellite","Road Map"))
  })
  
  output$deped <- renderImage({
    list(src="deped.png", width= "100%",
         filetype = "image/png"
    )}, deleteFile = FALSE)
  
  output$A1 <- renderImage({
    list(src="map.png", width= "100%",
         filetype = "image/png"
    )}, deleteFile = FALSE)
  
  output$A2 <- renderImage({
    list(src="person.png", width= "100%",
         filetype = "image/png"
    )}, deleteFile = FALSE)
  
  output$A3 <- renderImage({
    list(src="inform.png", width= "100%",
         filetype = "image/png"
    )}, deleteFile = FALSE)
  
  output$B1 <- renderImage({
    list(src="filter.png", width= "80%",
         filetype = "image/png"
    )}, deleteFile = FALSE)
  
  output$B2 <- renderImage({
    list(src="database.png", width= "80%", 
         filetype = "image/png"
    )}, deleteFile = FALSE)
  
  output$B3 <- renderImage({
    list(src="sliders.png", width= "110%",
         filetype = "image/png"
    )}, deleteFile = FALSE)
  
  output$B4 <- renderImage({
    list(src="click.png", width= "70%",
         filetype = "image/png"
    )}, deleteFile = FALSE)
  
  output$B5 <- renderImage({
    list(src="dot.png", width= "70%",
         filetype = "image/png"
    )}, deleteFile = FALSE)
  
  output$B6 <- renderImage({
    list(src="six.png", width= "70%",
         filetype = "image/png"
    )}, deleteFile = FALSE)
  
  output$NationwideExcess <- DT::renderDT({datatable(Excess, options = list(scrollX = TRUE, pageLength = 50, columnDefs = list(list(className = 'dt-center', targets ="_all")), rownames = FALSE))})
  
  
  output$ESEx <- renderValueBox({
    valueBox(strong("50,579"), subtitle = strong("Excess"), icon = icon("users"), color = "blue")
  })
  
  output$ESSh <- renderValueBox({
    valueBox(strong("24,634"), subtitle = strong("Shortage"), icon = icon("users"), color = "red")
  })
  
  output$JHSEx <- renderValueBox({
    valueBox(strong("108,342"), subtitle = strong("Excess"), icon = icon("users"), color = "blue")
  })
  
  output$JHSSh <- renderValueBox({
    valueBox(strong("10,108"), subtitle = strong("Shortage"), icon = icon("users"), color = "red")
  })
  
  output$SHSEx <- renderValueBox({
    valueBox(strong("4,383"), subtitle = strong("Excess"), icon = icon("users"), color = "blue")
  })
  
  output$SHSSh <- renderValueBox({
    valueBox(strong("24,581"), subtitle = strong("Shortage"), icon = icon("users"), color = "red")
  })
  
  output$AO2 <- renderValueBox({
    valueBox(strong("21,262"), subtitle = strong("AOII Items Created"), icon = icon("users"), color = "blue")
  })
  
  output$clustered <- renderValueBox({
    valueBox("9,627", subtitle = "Clustered Schools", icon = icon("school"), color = "green")
  })
  
  output$cos <- renderValueBox({
    valueBox("7,062", subtitle = "Outlier Schools", icon = icon("school"), color = "purple")
  })
  
  output$COSRB <- DT::renderDT({datatable(dfz, rownames = FALSE, options = list(scrollX = TRUE, pageLength = 20, columnDefs = list(list(className = 'dt-left', targets ="_all"))))
  })
  
  output$otherdataselection <- renderUI({
    otherdata2 <- input$OtherData
    data_column <- uni[[otherdata2]]
    selectInput("otherdataselect",strong("Select a Category:"), c(unique(data_column[!is.na(data_column) & data_column != "#N/A" & data_column != "For Verification"])))
  })
  
  output$SDOSelectionGMIS <- renderUI({
    dfGMISRegDiv <- read.csv("GMIS-Apr2025-RegDiv.csv")
    # Get the list of divisions based on the selected region
    divisions <- dfGMISRegDiv[dfGMISRegDiv$Region %in% input$RegionGMIS, "Division"]
    
    # Render the pickerInput
    pickerInput(
      inputId = "SDOGMIS",
      label = "Select a Division:",
      choices = divisions,
      selected = divisions,
      multiple = TRUE,
      options = pickerOptions(
        actionsBox = TRUE, # Changed to TRUE
        liveSearch = TRUE,
        header = "Select one or more Divisions", # Changed header text
        title = "No Divisions Selected", # Changed title text
        selectedTextFormat = "count > 3",
        dropupAuto = FALSE, # This tells it NOT to automatically switch direction
        dropup = FALSE # Added this option
        ),
      choicesOpt = list()
      )
  })
  
  output$PosSelectionGMIS <- renderUI({
    dfGMISPosCat <- read.csv("GMIS-Apr2025-PosCat.csv")
    # Filter the data frame to get positions based on the selected position category
    positions <- sort(unique(dfGMISPosCat$Position))
    
    # Render the pickerInput
    pickerInput(
      inputId = "PosSelGMIS",
      label = "Select a Position:",
      choices = positions,
      selected = c("Teacher I","Teacher II","Teacher III"),
      multiple = TRUE,
      options = pickerOptions(
        actionsBox = TRUE, # Changed to TRUE
        liveSearch = TRUE,
        header = "Select Positions", # Changed header text
        title = "No Positions Selected", # Changed title text
        selectedTextFormat = "count > 3",
        dropupAuto = FALSE, # This tells it NOT to automatically switch direction
        dropup = FALSE # Added this option
      ),
      choicesOpt = list()
    )
  })
  
  ### GMIS Count
  
  observe({
    PosCatGMISRCT <- input$PosCatGMIS
    req(PosCatGMISRCT)
    PosSelGMISRCT <- input$PosSelGMIS
    req(PosSelGMISRCT)
    
    output$itemcount <- renderPlot({
      dfGMIS <- read.csv("GMIS-FillingUpPerPosition-2025.csv")
      mainreact1g <- dfGMIS %>% filter(Position == PosSelGMISRCT) %>% group_by(Region) %>% summarise(Filled = sum(Total.Filled, na.rm = TRUE), Unfilled = sum(Total.Unfilled, na.rm = TRUE)) |>  pivot_longer(cols = c(Filled,Unfilled), names_to = "Category", values_to = "Count")
      ggplot(mainreact1g, aes(x=reorder(Region,-Count), y=Count, fill = factor(Category, levels = c("Unfilled","Filled")))) +
        geom_bar(stat = "identity",position = "stack", color = "black") +
        geom_text(data = subset(mainreact1g, Count != 0), aes(label = Count),vjust =-1,position = position_stack(vjust = 0.5), check_overlap = TRUE)+
        scale_fill_manual(values = c("Filled" = "green","Unfilled" = "grey")) +
        guides(fill = guide_legend(nrow = 1)) + 
        labs(x ="", y="",fill="Inventory")+
        theme(axis.text.x = element_text(size = 14, angle = 45, hjust = 1),
              legend.position = "top")})
    
    output$itemcount2 <- renderPlot({
      dfGMIS <- read.csv("GMIS-FillingUpPerPosition-2025.csv")
      mainreact1g <- dfGMIS %>% filter(Position == PosSelGMISRCT) %>% filter(Region == input$RegionCountItem) %>% group_by(Division) %>% summarise(Filled = sum(Total.Filled, na.rm = TRUE), Unfilled = sum(Total.Unfilled, na.rm = TRUE))  %>%  pivot_longer(cols = c(Filled,Unfilled), names_to = "Category", values_to = "Count")
      ggplot(mainreact1g, aes(x=reorder(Division,-Count), y=Count, fill = factor(Category, levels = c("Unfilled","Filled")))) +
        geom_bar(stat = "identity",position = "stack", color = "black") +
        geom_text(data = subset(mainreact1g, Count != 0), aes(label = Count),vjust =-1, position = position_stack(vjust = 0.5), check_overlap = TRUE)+
        scale_fill_manual(values = c("Filled" = "green","Unfilled" = "grey")) +
        guides(fill = guide_legend(nrow = 1)) + 
        labs(x ="", y="",fill="Inventory")+
        theme(axis.text.x = element_text(size = 14, angle = 45, hjust = 1),
              legend.position = "top")})
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
  
  observeEvent(input$Facilities_Refresh, {

    # Ensure all necessary inputs are ready before proceeding
    req(input$resource_map_region, input$Resource_SDO, input$EFD_Type)

    # Filter the data based on user inputs
    mainreactEFD <- EFDMP %>% 
      filter(!is.na(Old.Region), Old.Region != "") %>% 
      filter(!is.na(Latitude), !is.na(Longitude)) %>% 
      mutate(Latitude = as.numeric(Latitude),
             Allocation = dollar(Allocation, prefix = "₱")) %>%  # Use 'dollar' to format Allocation) %>% 
      distinct(SchoolID, FundingYear, Allocation, Category, .keep_all = TRUE) %>%
      filter(Region == input$resource_map_region) %>%
      filter(Division == input$Resource_SDO) %>%
      filter(Category %in% input$EFD_Type) %>% 
      mutate(FundingCategory = factor(
        case_when(
          FundingYear < 2025 ~ "Before 2025",
          (FundingYear >= 2025 & FundingYear <= 2030) ~ "2025-2030",
          FundingYear > 2030 ~ "After 2030"
        ),
        levels = c("Before 2025", "2025-2030", "After 2030")
      ))

    # This req() is crucial: it stops the code if the filter returns no rows, preventing the crash.
    req(mainreactEFD)

    # Now that you have a non-empty data frame, you can use leafletProxy
    
    values.efdmasterlist <- paste(strong("SCHOOL INFORMATION"),"<br>School Name:",mainreactEFD$School.Name,"<br>School ID:",mainreactEFD$SchoolID,"<br>Category:",mainreactEFD$Category,"<br>Funding Year:",mainreactEFD$FundingYear,"<br>Allocation:",mainreactEFD$Allocation) %>% lapply(htmltools::HTML)
    
    color_palette <- colorFactor(
      palette = c("red", "green", "blue"),
      domain = mainreactEFD$FundingCategory,
      levels = levels(mainreactEFD$FundingCategory) # Ensure the order is respected
    )
    
    # 3. Use the new factor variable and color palette in your leaflet code
    leafletProxy("FacMapping", data = mainreactEFD) %>%
      clearMarkers() %>%
      clearControls() %>%
      setView(lng = mainreactEFD$Longitude[1], lat = mainreactEFD$Latitude[1], zoom = 7) %>%
      addAwesomeMarkers(
        clusterOptions = markerClusterOptions(disableClusteringAtZoom = 15),
        lng = ~Longitude,
        lat = ~Latitude,
        popup = values.efdmasterlist,
        icon = makeAwesomeIcon(
          icon = "education",
          library = "glyphicon",
          markerColor = case_when(mainreactEFD$FundingCategory == "Before 2025" ~ "red", mainreactEFD$FundingCategory == "2025-2030" ~ "green", mainreactEFD$FundingCategory == "After 2030" ~ "blue") # Use the new factor variable
        )
      ) %>%
      addLegend(
        "bottomright",
        pal = color_palette,
        values = ~FundingCategory, # Use the new factor variable
        title = "Funding Year",
        opacity = 1
      )
    
    dfreact_fac <- reactive({
      if (is.null(input$FacMapping_bounds)) {
        mainreactEFD
      } else {
        bounds <- input$FacMapping_bounds
        latRng <- range(bounds$north, bounds$south)
        lngRng <- range(bounds$east, bounds$west)
        
        subset(mainreactEFD,
               Latitude >= latRng[1] & Latitude <= latRng[2] & Longitude >= lngRng[1] & Longitude <= lngRng[2])
      }
    })
    
    output$FacTable <- DT::renderDT(server = FALSE, {
      datatable(dfreact_fac() %>%
                  arrange(FundingYear) %>% 
                  select("Region","Division","School.Name","FundingYear","Allocation") %>%
                  rename("School" = School.Name, "Funding Year" = FundingYear),
                extension = 'Buttons',
                rownames = FALSE,
                options = list(scrollX = TRUE, pageLength = 10, columnDefs = list(list(className = 'dt-center', targets ="_all")), dom = 'Bfrtip', buttons = list('csv','excel','pdf','print')))
    })
  })
  
  observeEvent(input$Teaching_Deployment_Refresh, {
    
    output$TeacherShortage_Mapping <- renderLeaflet({
      p = colorFactor(palette = c("red","deepskyblue","green"),domain = c("Shortage","Excess","Balanced"), ordered = T)
      leaflet() %>%
        setView(lng = 122, lat = 13, zoom =6) %>%
        addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>% 
        addProviderTiles(providers$CartoDB.Positron, group = "Road Map") %>% 
        addMeasure(position = "topright", primaryLengthUnit = "kilometers", primaryAreaUnit = "sqmeters") %>% 
        addLegend(position = "bottomright", title = "Legend", pal = p, values = c("Shortage","Excess","Balanced")) %>% 
        addLayersControl(
          baseGroups = c("Satellite","Road Map"))
    })
    
    RegRCT <- input$resource_map_region
    SDORCT1 <- input$Resource_SDO
    DistRCT1 <- input$Resource_LegDist
    Lev <- input$resource_map_level
    
    mainreact1 <- df %>% filter(Region == RegRCT) %>% filter(Division == SDORCT1) %>% filter(Legislative.District == DistRCT1) %>% filter(Level == Lev) %>% arrange(desc(TeacherShortage))
    
    NetShortage <- df %>% select(Region,Division,Level,TeacherShortage,TeacherExcess) %>%
      pivot_longer(cols = c(TeacherShortage, TeacherExcess), names_to = "Inventory", values_to = "Count") %>% mutate(Count=as.numeric(Count)) %>% na.omit(Count) %>% group_by(Region, Division,Level, Inventory) %>% summarize(Count = sum(Count)) %>% pivot_wider(names_from = "Inventory", values_from = "Count") %>% mutate(NetShortage=TeacherShortage-TeacherExcess) %>% mutate(NetShortage = ifelse(NetShortage < 0, 0, NetShortage))
    
    SDONetShortage <- NetShortage %>% filter(Region == RegRCT) %>% filter(Division == SDORCT1) %>% filter(Level == Lev)
    
    values_teacher_shortage <- paste(mainreact1$School.Name,"<br>Teacher Excess:", mainreact1$TeacherExcess,"<br>Teacher Shortage:", mainreact1$TeacherShortage) %>% lapply(htmltools::HTML)
    
    values_teacher_shortage_popup <- paste(strong("SCHOOL INFORMATION"),"<br>School Name:",mainreact1$School.Name,"<br>School ID:",mainreact1$SchoolID,"<br>Enrolment Size:",mainreact1$TotalEnrolment,"<br>","<br>",strong("TEACHING PERSONNEL DATA"),"<br>Teacher Inventory:", mainreact1$TotalTeachers,"<br>Teacher Excess:", mainreact1$TeacherExcess,"<br>Teacher Shortage:", mainreact1$TeacherShortage,"<br>","<br>",strong("SPECIALIZATION DATA"),"<br>English:", mainreact1$English,"<br>Mathematics:", mainreact1$Mathematics,"<br>Science:", mainreact1$Science,"<br>Biological Science:", mainreact1$Biological.Sciences,"<br>Physical Sciences:", mainreact1$Physical.Sciences,"<br>General Education:", mainreact1$General.Ed,"<br>Araling Panlipunan:", mainreact1$Araling.Panlipunan,"<br>TLE:", mainreact1$TLE,"<br>MAPEH:", mainreact1$MAPEH,"<br>Filipino:", mainreact1$Filipino,"<br>ESP:", mainreact1$ESP,"<br>Agriculture:", mainreact1$Agriculture,"<br>ECE:", mainreact1$ECE,"<br>SPED:", mainreact1$SPED) %>% lapply(htmltools::HTML)
    
    leafletProxy("TeacherShortage_Mapping") %>% clearMarkers() %>% clearMarkerClusters() %>% setView(lng = mainreact1$Longitude[1], lat = mainreact1$Latitude[1], zoom = 7) %>% 
      addAwesomeMarkers(clusterOptions = markerClusterOptions(disableClusteringAtZoom = 15), lng = mainreact1$Longitude, lat = mainreact1$Latitude, popup = values_teacher_shortage_popup, options = popupOptions(), label = values_teacher_shortage, labelOptions = labelOptions(noHide = F, textsize = "12px", direction = "top"), icon = makeAwesomeIcon(icon = "education", library = "glyphicon", markerColor = case_when(mainreact1$TeacherShortage > 0 ~ "red", mainreact1$TeacherExcess > 0 ~ "blue", (mainreact1$TeacherExcess == 0 & mainreact1$TeacherShortage == 0) ~ "green", is.na(mainreact1$TeacherShortage) ~ "gray")))
    
    dfreact_TS <- reactive({
      
      if (is.null(input$TeacherShortage_Mapping_bounds)) {
        mainreact1
      } else {
        bounds <- input$TeacherShortage_Mapping_bounds
        latRng <- range(bounds$north, bounds$south)
        lngRng <- range(bounds$east, bounds$west)
        
        subset(mainreact1,
               Latitude >= latRng[1] & Latitude <= latRng[2] & Longitude >= lngRng[1] & Longitude <= lngRng[2])
      }
    })
    
    output$TeacherShortage_Table <- DT::renderDT(server = FALSE, {datatable(dfreact_TS() %>% select("School.Name","TeacherShortage","TeacherExcess") %>% rename("School" = School.Name, "Shortage" = TeacherShortage, "Excess" = TeacherExcess), extension = 'Buttons', rownames = FALSE, options = list(scrollX = TRUE, pageLength = 5, columnDefs = list(list(className = 'dt-center', targets ="_all")), dom = 'Bfrtip', buttons = list('csv','excel','pdf','print')))})
    
    output$a <- renderValueBox({
      valueBox(tags$p(strong(SDO[which(SDO$Region==RegRCT & SDO$Division==SDORCT1),"FillUpRate"]), style = "font-family: Poppins; font-size: 20px; color: #111111; text-align: center;"), subtitle = NULL)
    })
    
    output$b <- renderValueBox({
      valueBox(tags$p(strong(SDO[which(SDO$Region==RegRCT & SDO$Division==SDORCT1),"Unfilled"]), style = "font-family: Poppins; font-size: 20px; color: #111111; text-align: center;"), subtitle = NULL)
      
    })
    
    output$e <- renderValueBox({
      valueBox(tags$p(strong(SDONetShortage$NetShortage), style = "font-family: Poppins; font-size: 20px; color: #111111; text-align: center;"), subtitle = NULL)
      
    })
    
    output$c <- renderValueBox({
      valueBox(tags$p(strong(sum(df1()$TeacherExcess)), style = "font-size: 65%;"), subtitle = NULL)
      
    })
    
    output$d <- renderValueBox({
      valueBox(tags$p(strong("-"), style = "font-size: 65%;"), subtitle = NULL)})
    
    output$f <- renderValueBox({
      valueBox(tags$p(strong(SDO[which(SDO$Division==RegRCT),"FillUpRate"]), style = "font-family: Poppins; font-size: 20px; color: #111111; text-align: center;"), subtitle = NULL)
    })
    
    output$g <- renderValueBox({
      valueBox(tags$p(strong(SDO[which(SDO$Division==RegRCT),"Unfilled"]), style = "font-family: Poppins; font-size: 20px; color: #111111; text-align: center;"), subtitle = NULL)
      
    })
    
  })
    
  
  observeEvent(input$Mapping_Run, {
    
    output$TeacherShortage_Mapping <- renderLeaflet({
      p = colorFactor(palette = c("red","deepskyblue","green"),domain = c("Shortage","Excess","Balanced"), ordered = T)
      leaflet() %>%
        setView(lng = 122, lat = 13, zoom =6) %>%
        addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>% 
        addProviderTiles(providers$CartoDB.Positron, group = "Road Map") %>% 
        addMeasure(position = "topright", primaryLengthUnit = "kilometers", primaryAreaUnit = "sqmeters") %>% 
        addLegend(position = "bottomright", title = "Legend", pal = p, values = c("Shortage","Excess","Balanced")) %>% 
        addLayersControl(
          baseGroups = c("Satellite","Road Map"))
    })
    
    output$SHSMapping <- renderLeaflet({
      domain = c("Manufacturing and Engineering","Hospitality and Tourism","Professional/Private Services","Public Administration","Business and Finance","Agriculture and Agri-business")
      p = colorFactor(palette = c("red","orange","violet","green","blue","magenta"), levels = as.factor(domain), ordered = F)
      leaflet() %>%
        setView(lng = 122, lat = 13, zoom =6) %>%
        addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>% 
        addProviderTiles(providers$CartoDB.Positron, group = "Road Map") %>%  
        addMeasure(position = "topright", primaryLengthUnit = "kilometers", primaryAreaUnit = "sqmeters") %>% 
        addLegend(position = "bottomright", title = "Legend", pal = p, values = c("Manufacturing and Engineering","Hospitality and Tourism","Professional/Private Services","Public Administration","Business and Finance","Agriculture and Agri-business")) %>% 
        addLayersControl(
          baseGroups = c("Satellite","Road Map")
        )
    })
    
    output$SHSMapping <- renderLeaflet({
      leaflet() %>%
        setView(lng = 122, lat = 13, zoom =6) %>%
        addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>% 
        addProviderTiles(providers$CartoDB.Positron, group = "Road Map") %>% 
        addMeasure(position = "topright", primaryLengthUnit = "kilometers", primaryAreaUnit = "sqmeters") %>%
        addLayersControl(
          baseGroups = c("Satellite","Road Map")
        )
    })
    
    output$AO2Mapping <- renderLeaflet({
      p = colorFactor(palette = c("red","orange","green"),domain = c("No AO II and PDO I","With at least 1 AO II or PDO I","With AO II and PDO I"), ordered = T)
      leaflet() %>%
        setView(lng = 122, lat = 13, zoom =6) %>%
        addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>% 
        addProviderTiles(providers$CartoDB.Positron, group = "Road Map") %>%  
        addMeasure(position = "topright", primaryLengthUnit = "kilometers", primaryAreaUnit = "sqmeters") %>% 
        addLegend(position = "bottomright", title = "Legend", pal = p, values = c("No AO II and PDO I","With at least 1 AO II or PDO I","With AO II and PDO I")) %>% 
        addLayersControl(
          baseGroups = c("Satellite","Road Map")
        )
    })
    
    output$CLMapping <- renderLeaflet({
      domain = c("Extreme (>2.0)","Major (1.6-2.0)","Minor (0.6-1.5)","Mild (0-0.5)")
      p = colorFactor(palette = c("red","orange","yellow","green"), levels = as.factor(domain), ordered = F)
      leaflet() %>%
        setView(lng = 122, lat = 13, zoom =6) %>%
        addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>% 
        addProviderTiles(providers$CartoDB.Positron, group = "Road Map") %>%  
        addMeasure(position = "topright", primaryLengthUnit = "kilometers", primaryAreaUnit = "sqmeters") %>% 
        addLegend(position = "bottomright", title = "Legend", pal = p, values = c("Extreme (>2.0)","Major (1.6-2.0)","Minor (0.6-1.5)","Mild (0-0.5)")) %>% 
        addLayersControl(
          baseGroups = c("Satellite","Road Map")
        )
    })
    
    output$FacMapping <- renderLeaflet({
      leaflet() %>%
        setView(lng = 122, lat = 13, zoom =6) %>%
        addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%
        addProviderTiles(providers$CartoDB.Positron, group = "Road Map") %>%
        addMeasure(position = "topright", primaryLengthUnit = "kilometers", primaryAreaUnit = "sqmeters") %>%
        addLayersControl(
          baseGroups = c("Satellite","Road Map")
        )
    })
    
    values.coord <- paste("Region: ", geojson_table$Region, "<br>",
                          "Learner Congestion:",":", geojson_table$Congestion.Index) %>% lapply(htmltools::HTML)
    
    output$CongestMapping <- renderLeaflet({
      
      pal <- colorBin(
        palette = c("green", "orange", "red"),
        domain = geojson_table$Congestion.Index
      )
      
      domain = c("Not Congested","Moderately Congested","Severely Congested")
      p = colorFactor(palette = c("green","orange","red"), levels = as.factor(domain), ordered = F)
      
      leaflet() %>%
        setView(lng = 122, lat = 13, zoom = 6) %>%
        addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%
        addProviderTiles(providers$CartoDB.Positron, group = "Road Map") %>%
        addMeasure(position = "topright", primaryLengthUnit = "kilometers", primaryAreaUnit = "sqmeters") %>%
        addLayersControl(
          baseGroups = c("Satellite", "Road Map")
        ) %>%
        addTiles() %>%
        addPolygons(
          data = geojson_data,
          stroke = FALSE,
          weight = 4,
          fillOpacity = 0.75,
          fillColor = ~pal(geojson_table$Congestion.Index),
          label = values.coord
        ) %>% 
        addLegend(
          position = "bottomright", 
          title = "Legend", 
          pal = p, 
          values = c("Not Congested","Moderately Congested","Severely Congested"))
    })
    

    
    RegRCT <- input$resource_map_region
    SDORCT1 <- input$Resource_SDO
    DistRCT1 <- input$Resource_LegDist
    Lev <- input$resource_map_level
    TypeEFD <- input$EFD_Type
    
    mainreact1 <- df %>% filter(Region == RegRCT) %>% filter(Division == SDORCT1) %>% filter(Legislative.District == DistRCT1) %>% filter(Level == Lev) %>% arrange(desc(TeacherShortage))
    
    mainreactreg <- df %>% filter(Region == RegRCT)
    mainreactunireg <- uni %>% filter(Region == RegRCT)
    mainreactunidiv <- uni %>% filter(Region == RegRCT) %>% filter(Division == SDORCT1)
    mainreactdiv <- df %>% filter(Region == RegRCT) %>% filter(Division == SDORCT1)
    mainreactNTP <- uni %>% filter(Region == RegRCT) %>% filter(Division == SDORCT1) %>% filter(Legislative.District == DistRCT1)
    mainreactlevreg <- df %>% filter(Region == RegRCT) %>% filter(Level == Lev)
    mainreactlevdiv <- df %>% filter(Region == RegRCT) %>% filter(Division == SDORCT1) %>% filter(Level == Lev)
    mainreactCR <- uni %>% filter(Region == RegRCT) %>% filter(Division == SDORCT1) %>% filter(Legislative.District == DistRCT1) %>% distinct(SchoolID, .keep_all = TRUE) %>% arrange(desc(SBPI))
    mainreactSHS <- df %>% filter(Region == RegRCT) %>% filter(Level == "SHS") %>% distinct(SchoolID, .keep_all = TRUE) %>% filter(SchoolID %in% SHS_Pilot2)
    mainreactind <- ind %>% filter(Region == RegRCT)
    mainreactEFD <- EFDMP %>% 
      filter(!is.na(Old.Region), Old.Region != "") %>% 
      filter(!is.na(Latitude), !is.na(Longitude)) %>% 
      mutate(Latitude = as.numeric(Latitude),
             Allocation = dollar(Allocation, prefix = "₱")) %>%  # Use 'dollar' to format Allocation) %>% 
      distinct(SchoolID, FundingYear, Allocation, Category, .keep_all = TRUE) %>%
      arrange(FundingYear) %>%
      filter(Region == input$resource_map_region) %>%
      filter(Division == input$Resource_SDO) %>%
      filter(Category %in% input$EFD_Type) %>% 
      mutate(FundingCategory = factor(
        case_when(
          FundingYear < 2025 ~ "Before 2025",
          (FundingYear >= 2025 & FundingYear <= 2030) ~ "2025-2030",
          FundingYear > 2030 ~ "After 2030"
        ),
        levels = c("Before 2025", "2025-2030", "After 2030")
      ))

    NetShortage <- df %>% select(Region,Division,Level,TeacherShortage,TeacherExcess) %>%
      pivot_longer(cols = c(TeacherShortage, TeacherExcess), names_to = "Inventory", values_to = "Count") %>% mutate(Count=as.numeric(Count)) %>% na.omit(Count) %>% group_by(Region, Division,Level, Inventory) %>% summarize(Count = sum(Count)) %>% pivot_wider(names_from = "Inventory", values_from = "Count") %>% mutate(NetShortage=TeacherShortage-TeacherExcess) %>% mutate(NetShortage = ifelse(NetShortage < 0, 0, NetShortage))
    
    SDONetShortage <- NetShortage %>% filter(Region == RegRCT) %>% filter(Division == SDORCT1) %>% filter(Level == Lev)
    
    values_teacher_shortage <- paste(mainreact1$School.Name,"<br>Teacher Excess:", mainreact1$TeacherExcess,"<br>Teacher Shortage:", mainreact1$TeacherShortage) %>% lapply(htmltools::HTML)
    
    values_teacher_shortage_popup <- paste(strong("SCHOOL INFORMATION"),"<br>School Name:",mainreact1$School.Name,"<br>School ID:",mainreact1$SchoolID,"<br>Enrolment Size:",mainreact1$TotalEnrolment,"<br>","<br>",strong("TEACHING PERSONNEL DATA"),"<br>Teacher Inventory:", mainreact1$TotalTeachers,"<br>Teacher Excess:", mainreact1$TeacherExcess,"<br>Teacher Shortage:", mainreact1$TeacherShortage,"<br>","<br>",strong("SPECIALIZATION DATA"),"<br>English:", mainreact1$English,"<br>Mathematics:", mainreact1$Mathematics,"<br>Science:", mainreact1$Science,"<br>Biological Science:", mainreact1$Biological.Sciences,"<br>Physical Sciences:", mainreact1$Physical.Sciences,"<br>General Education:", mainreact1$General.Ed,"<br>Araling Panlipunan:", mainreact1$Araling.Panlipunan,"<br>TLE:", mainreact1$TLE,"<br>MAPEH:", mainreact1$MAPEH,"<br>Filipino:", mainreact1$Filipino,"<br>ESP:", mainreact1$ESP,"<br>Agriculture:", mainreact1$Agriculture,"<br>ECE:", mainreact1$ECE,"<br>SPED:", mainreact1$SPED) %>% lapply(htmltools::HTML)
    
    values.non_teaching <- mainreactNTP$School.Name %>% lapply(htmltools::HTML)
    
    values.non_teaching_popup <- paste(strong("SCHOOL INFORMATION"),"<br>School Name:",mainreactNTP$School.Name,"<br>School ID:",mainreactNTP$SchoolID,"<br>Enrolment Size:",mainreactNTP$TotalEnrolment,"<br>","<br>",strong("TEACHING PERSONNEL DATA"),"<br>Teacher Inventory:", mainreactNTP$TotalTeachers,"<br>Teacher Excess:", mainreactNTP$TeacherExcess,"<br>Teacher Shortage:", mainreactNTP$TeacherShortage,"<br>","<br>",strong("NON-TEACHING PERSONNEL DATA"),"<br>Plantilla Number of AOII:", mainreactNTP$Plantilla.Number,"<br>Clustering Status:", mainreactNTP$Clustering.Status,"<br>PDO I Deployment:", mainreactNTP$PDOI_Deployment) %>% lapply(htmltools::HTML)
    
    values_classrooom_shortage <- paste(mainreactCR$School.Name,"<br>Classroom Shortage:", mainreactCR$Est.CS, "<br>School Building Priority Index:", mainreactCR$SBPI) %>% lapply(htmltools::HTML)
    
    values_classrooom_shortage_popup <- paste(strong("SCHOOL INFORMATION"),"<br>School Name:",mainreactCR$School.Name,"<br>School ID:",mainreactCR$SchoolID,"<br>Enrolment Size:",mainreactCR$TotalEnrolment,"<br>","<br>",strong("CLASSROOM DATA"),"<br>Estimate Classroom Shortage:", mainreactCR$Est.CS,"<br>Type of Ownership:", mainreactCR$OwnershipType,"<br>Shifting:", mainreactCR$Shifting,"<br>Electricity Source:", mainreactCR$ElectricitySource,"<br>Water Source:", mainreactCR$WaterSource) %>% lapply(htmltools::HTML)
    
    values.efdmasterlist <- paste(strong("SCHOOL INFORMATION"),"<br>School Name:",mainreactEFD$School.Name,"<br>School ID:",mainreactEFD$SchoolID,"<br>Category:",mainreactEFD$Category,"<br>Funding Year:",mainreactEFD$FundingYear,"<br>Allocation:",mainreactEFD$Allocation) %>% lapply(htmltools::HTML)
    
    
    values_industry <- paste(strong("SCHOOL INFORMATION"),"<br>School Name:",mainreactSHS$School.Name,"<br>School ID:",mainreactSHS$SchoolID) %>% lapply(htmltools::HTML)
    
    values.ind <- paste(mainreactind$Company,"<br>Province:",mainreactind$Province) %>% lapply(htmltools::HTML)
    
    leafletProxy("SHSMapping") %>% clearMarkers() %>% clearMarkerClusters() %>% setView(lng = mainreactSHS$Longitude[1], lat = mainreactSHS$Latitude[1], zoom = 7) %>%
      addCircleMarkers(clusterOptions = markerClusterOptions(disableClusteringAtZoom = 15), lng = mainreactSHS$Longitude, lat = mainreactSHS$Latitude, radius= 6, fillOpacity = 1, fillColor = "orange", stroke = TRUE,label = values_industry, labelOptions = labelOptions(noHide = F, textsize = "12px", direction = "top", fill = TRUE, style = list("border-color" = "rgba(0,0,0,0.5)")), color = "black") %>% 
      addAwesomeMarkers(clusterOptions = markerClusterOptions(disableClusteringAtZoom = 12), lng = mainreactind$Longitude, lat = mainreactind$Latitude, icon = makeAwesomeIcon(icon = "cog",library = "glyphicon", ,markerColor = case_when(mainreactind$Sector == "Manufacturing and Engineering" ~ "red", mainreactind$Sector == "Hospitality and Tourism" ~ "orange", mainreactind$Sector == "Professional/Private Services" ~ "violet", mainreactind$Sector == "Public Administration" ~ "green", mainreactind$Sector == "Business and Finance" ~ "blue",mainreactind$Sector == "Agriculture and Agri-business" ~ "magenta")), label = values.ind, labelOptions = labelOptions(noHide = F, textsize = "12px", direction = "top"))
    
    leafletProxy("CLMapping") %>% clearMarkers() %>% clearMarkerClusters() %>%  setView(lng = mainreactCR$Longitude[1], lat = mainreactCR$Latitude[1], zoom = 7) %>% addCircleMarkers(clusterOptions = markerClusterOptions(), lng = mainreactCR$Longitude, lat = mainreactCR$Latitude, radius= 6, fillOpacity = 1, stroke = FALSE, popup = values_classrooom_shortage_popup, options = popupOptions(),label = values_classrooom_shortage, labelOptions = labelOptions(noHide = T, textsize = "12px", direction = "top", fill = TRUE, style = list("border-color" = "rgba(0,0,0,0.5)")),color = case_when(mainreactCR$SBPI <=1.0 & mainreactCR$SBPI > 0 ~ "green", mainreactCR$SBPI > 1.0 & mainreactCR$SBPI <=1.5 ~ "yellow", mainreactCR$SBPI > 1.5 & mainreactCR$SBPI<= 2.0 ~ "orange",  mainreactCR$SBPI > 2.0  ~ "red"))
    
    
    leafletProxy("AO2Mapping") %>% clearMarkers() %>% clearMarkerClusters() %>%
      setView(lng = mainreactNTP$Longitude[1], lat = mainreactNTP$Latitude[1], zoom = 7) %>%
      addCircleMarkers(
        clusterOptions = markerClusterOptions(),
        lng = mainreactNTP$Longitude,
        lat = mainreactNTP$Latitude,
        radius = 6,
        fillOpacity = 1,
        stroke = FALSE,
        popup = values.non_teaching_popup,
        options = popupOptions(),
        label = values.non_teaching,
        labelOptions = labelOptions(noHide = TRUE, textsize = "12px", direction = "top"),
        fillColor = case_when( # Changed from markerColor to fillColor
          mainreactNTP$Clustering.Status %in% c("Dedicated","Clustered") & mainreactNTP$PDOI_Deployment == "With PDO I" ~ "green",
          mainreactNTP$Clustering.Status %in% c("Dedicated","Clustered") & mainreactNTP$PDOI_Deployment == "Without PDO I" ~ "orange",
          mainreactNTP$Clustering.Status == "None Deployed" & mainreactNTP$PDOI_Deployment == "With PDO I" ~ "orange",
          mainreactNTP$Clustering.Status == "None Deployed" & mainreactNTP$PDOI_Deployment == "Without PDO I" ~ "red",
          TRUE ~ "lightgray" # Default color
        )
      )
    
    leafletProxy("TeacherShortage_Mapping") %>% clearMarkers() %>% clearMarkerClusters() %>% setView(lng = mainreact1$Longitude[1], lat = mainreact1$Latitude[1], zoom = 7) %>% 
      addAwesomeMarkers(clusterOptions = markerClusterOptions(disableClusteringAtZoom = 15), lng = mainreact1$Longitude, lat = mainreact1$Latitude, popup = values_teacher_shortage_popup, options = popupOptions(), label = values_teacher_shortage, labelOptions = labelOptions(noHide = F, textsize = "12px", direction = "top"), icon = makeAwesomeIcon(icon = "education", library = "glyphicon", markerColor = case_when(mainreact1$TeacherShortage > 0 ~ "red", mainreact1$TeacherExcess > 0 ~ "blue", (mainreact1$TeacherExcess == 0 & mainreact1$TeacherShortage == 0) ~ "green", is.na(mainreact1$TeacherShortage) ~ "gray")))
    
    dfreact_TS <- reactive({
      
      if (is.null(input$TeacherShortage_Mapping_bounds)) {
        mainreact1
      } else {
        bounds <- input$TeacherShortage_Mapping_bounds
        latRng <- range(bounds$north, bounds$south)
        lngRng <- range(bounds$east, bounds$west)
        
        subset(mainreact1,
               Latitude >= latRng[1] & Latitude <= latRng[2] & Longitude >= lngRng[1] & Longitude <= lngRng[2])
      }
    })
    
    output$TeacherShortage_Table <- DT::renderDT(server = FALSE, {datatable(dfreact_TS() %>% select("School.Name","TeacherShortage","TeacherExcess") %>% rename("School" = School.Name, "Shortage" = TeacherShortage, "Excess" = TeacherExcess), extension = 'Buttons', rownames = FALSE, options = list(scrollX = TRUE, pageLength = 5, columnDefs = list(list(className = 'dt-center', targets ="_all")), dom = 'Bfrtip', buttons = list('csv','excel','pdf','print')))})
    
    output$a <- renderValueBox({
      valueBox(tags$p(strong(SDO[which(SDO$Region==RegRCT & SDO$Division==SDORCT1),"FillUpRate"]), style = "font-family: Poppins; font-size: 20px; color: #111111; text-align: center;"), subtitle = NULL)
    })
    
    output$b <- renderValueBox({
      valueBox(tags$p(strong(SDO[which(SDO$Region==RegRCT & SDO$Division==SDORCT1),"Unfilled"]), style = "font-family: Poppins; font-size: 20px; color: #111111; text-align: center;"), subtitle = NULL)
      
    })
    
    output$e <- renderValueBox({
      valueBox(tags$p(strong(SDONetShortage$NetShortage), style = "font-family: Poppins; font-size: 20px; color: #111111; text-align: center;"), subtitle = NULL)
      
    })
    
    output$c <- renderValueBox({
      valueBox(tags$p(strong(sum(df1()$TeacherExcess)), style = "font-size: 65%;"), subtitle = NULL)
      
    })
    
    output$d <- renderValueBox({
      valueBox(tags$p(strong("-"), style = "font-size: 65%;"), subtitle = NULL)})
    
    output$f <- renderValueBox({
      valueBox(tags$p(strong(SDO[which(SDO$Division==RegRCT),"FillUpRate"]), style = "font-family: Poppins; font-size: 20px; color: #111111; text-align: center;"), subtitle = NULL)
    })
    
    output$g <- renderValueBox({
      valueBox(tags$p(strong(SDO[which(SDO$Division==RegRCT),"Unfilled"]), style = "font-family: Poppins; font-size: 20px; color: #111111; text-align: center;"), subtitle = NULL)
      
    })
    
    output$Single <- renderValueBox({
      valueBox(strong(sum(mainreactdiv$Clustering.Status == "NOT CLUSTERED")), subtitle = strong("Number of Unclustered Schools (Division)"), icon = icon("users"), color = "green")
    })
    
    output$Cluster <- renderValueBox({
      valueBox(strong(sum(mainreactdiv$Clustering.Status == "CLUSTERED")), subtitle = strong("Number of Clustered Schools (Division)"), icon = icon("school"), color = "green")
    })
    
    output$Outlier <- renderValueBox({
      valueBox(strong(sum(mainreactdiv$Clustering.Status == "Outlier")), subtitle = strong("Number of Outlier Schools (Division)"), icon = icon("school"), color = "green")
    })
    
    output$SingleR <- renderValueBox({
      valueBox(strong(sum(mainreactreg$Clustering.Status == "NOT CLUSTERED")), subtitle = strong("Number of Unclustered Schools (Region)"), icon = icon("users"), color = "navy")
    })
    
    output$ClusterR <- renderValueBox({
      valueBox(strong(sum(mainreactreg$Clustering.Status == "CLUSTERED")), subtitle = strong("Number of Clustered Schools (Region)"), icon = icon("school"), color = "navy")
    })
    
    output$OutlierR <- renderValueBox({
      valueBox(strong(sum(mainreactreg$Clustering.Status == "Outlier")), subtitle = strong("Number of Outlier Schools (Region)"), icon = icon("school"), color = "navy")
    })

    Ao21 <- reactive({
      
      if (is.null(input$AO2Mapping_bounds)) {
        mainreactNTP
      } else {
        bounds <- input$AO2Mapping_bounds
        latRng <- range(bounds$north, bounds$south)
        lngRng <- range(bounds$east, bounds$west)
        
        subset(mainreactNTP,
               Latitude >= latRng[1] & Latitude <= latRng[2] & Longitude >= lngRng[1] & Longitude <= lngRng[2])
      }
    })
    
    output$AO2Table <- DT::renderDT(Ao21() %>% select("School.Name","Clustering.Status","PDOI_Deployment") %>% rename("School" = School.Name, "AO II Deployment" = Clustering.Status, "PDOI Deployment" = PDOI_Deployment), rownames = FALSE, filter = 'top', options = list(scrollX = TRUE, columnDefs = list(list(className = 'dt-center', targets ="_all")), dom = 'Bfrtip', buttons = list('csv','excel','pdf','print')))
    
    output$f2 <- renderValueBox({
      valueBox(tags$p(strong(sum(mainreactreg$Clustering.Status == "Clustered")), style = "font-family: Poppins; font-size: 20px; color: #111111; text-align: center;"), subtitle = NULL)
    })
    
    output$g2 <- renderValueBox({
      valueBox(tags$p(strong(sum(mainreactreg$Clustering.Status == "Dedicated")), style = "font-family: Poppins; font-size: 20px; color: #111111; text-align: center;"), subtitle = NULL)
    })
    
    output$a2 <- renderValueBox({
      valueBox(tags$p(strong(sum(mainreactdiv$Clustering.Status == "Clustered")), style = "font-family: Poppins; font-size: 20px; color: #111111; text-align: center;"), subtitle = NULL)
    })
    
    output$b2 <- renderValueBox({
      valueBox(tags$p(strong(sum(mainreactdiv$Clustering.Status == "Dedicated")), style = "font-family: Poppins; font-size: 20px; color: #111111; text-align: center;"), subtitle = NULL)
    })
    
    output$e2 <- renderValueBox({
      valueBox(tags$p(strong(sum(mainreactNTP$Clustering.Status == "Clustered")), style = "font-family: Poppins; font-size: 20px; color: #111111; text-align: center;"), subtitle = NULL)
    })
    
    output$h2 <- renderValueBox({
      valueBox(tags$p(strong(sum(mainreactNTP$Clustering.Status == "Dedicated")), style = "font-family: Poppins; font-size: 20px; color: #111111; text-align: center;"), subtitle = NULL)
    })
    
    output$ROCRShort <- renderValueBox({
      valueBox(tags$p(strong(sum(mainreactunireg$Est.CS, na.rm = TRUE)), style = "font-family: Poppins; font-size: 20px; color: #111111; text-align: center;"), subtitle = NULL)
    })
    
    output$SDOCRShort <- renderValueBox({
      valueBox(tags$p(strong(sum(mainreactunidiv$Est.CS, na.rm = TRUE)), style = "font-family: Poppins; font-size: 20px; color: #111111; text-align: center;"), subtitle = NULL)
    })
    
    output$DistCRShort <- renderValueBox({
      valueBox(tags$p(strong(sum(mainreactNTP$Est.CS, na.rm = TRUE)), style = "font-family: Poppins; font-size: 20px; color: #111111; text-align: center;"), subtitle = NULL)
    })
    
    dfreact_CL <- reactive({
      
      if (is.null(input$CLMapping_bounds)) {
        mainreactCR
      } else {
        bounds <- input$CLMapping_bounds
        latRng <- range(bounds$north, bounds$south)
        lngRng <- range(bounds$east, bounds$west)
        
        subset(mainreactCR,
               Latitude >= latRng[1] & Latitude <= latRng[2] & Longitude >= lngRng[1] & Longitude <= lngRng[2])
      }
    })
    
    output$CLTable <- DT::renderDT(dfreact_CL() %>% select("School.Name","SBPI","Instructional.Rooms.2023.2024","Classroom.Requirement","Est.CS","Buidable_space") %>% rename("School" = School.Name, "School Building Priority Index" = SBPI, "Classroom Inventory" = Instructional.Rooms.2023.2024, "Classroom Requirement" = Classroom.Requirement, "Estimate Classroom Shortage" = Est.CS, "Buildable Space" = Buidable_space), filter = 'top', options = list(scrollX = TRUE, columnDefs = list(list(className = 'dt-center', targets ="_all")), rownames = FALSE, dom = 'Bfrtip', buttons = list('csv','excel','pdf','print')))
    
    dfreact_SHS <- reactive({
      
      if (is.null(input$SHSMapping_bounds)) {
        mainreactSHS
      } else {
        bounds <- input$SHSMapping_bounds
        latRng <- range(bounds$north, bounds$south)
        lngRng <- range(bounds$east, bounds$west)
        
        subset(mainreactSHS,
               Latitude >= latRng[1] & Latitude <= latRng[2] & Longitude >= lngRng[1] & Longitude <= lngRng[2])
      }
    })
    
    output$SHSListTable <- DT::renderDT(server = FALSE, {datatable(dfreact_SHS() %>% select("School.Name", "TotalEnrolment") %>% rename("School" = School.Name, "Total Enrolment" = TotalEnrolment) %>% arrange(desc("School.Name")), extension = 'Buttons', rownames = FALSE, options = list(scrollX = TRUE, pageLength = 5, columnDefs = list(list(className = 'dt-center', targets ="_all")), dom = 'Bfrtip', buttons = list('csv','excel','pdf','print')))})
    
    output$SHSCount <- renderValueBox({
      valueBox(tags$p(strong(nrow(mainreactSHS)), style = "font-size: 100%; text-align: center;"), subtitle = NULL)})
    
    output$SHSCountUniv <- renderValueBox({
      mainvalue <- df %>% filter(Region == input$resource_map_region) %>% filter(Level == "SHS")
      valueBox(tags$p(strong(nrow(mainvalue)), style = "font-size: 100%; text-align: center;"), subtitle = NULL)})
    
    output$IndCount <- renderValueBox({
      valueBox(tags$p(strong(nrow(mainreactind)), style = "font-size: 100%; text-align: center;"), subtitle = NULL)})
    
    output$assessmentSHS <- renderUI({
      p(HTML(paste(strong(RegRCT),"has",strong(nrow(mainreactSHS)),"senior high schools and a total of ",strong(nrow(mainreactind)),"industries composed of",strong(sum(mainreactind$Sector == "Food Establishments")),"industries on Food Establishments, ",strong(sum(mainreactind$Sector == "Professional/Private Services")),"industries on Professional/Private Services, ",strong(sum(mainreactind$Sector == "Transportation")),"industries on Transportation, ",strong(sum(mainreactind$Sector == "Utilities")),"industries on Utilities",", and",strong(sum(mainreactind$Sector == "Retail")),"industries on Retail")), style = "font-family: Century Gothic; font-size: 15px; color: #111111;")
    })
    
    color_palette <- colorFactor(
      palette = c("red", "green", "blue"),
      domain = mainreactEFD$FundingCategory,
      levels = levels(mainreactEFD$FundingCategory) # Ensure the order is respected
    )
    
    # 3. Use the new factor variable and color palette in your leaflet code
    leafletProxy("FacMapping", data = mainreactEFD) %>%
      clearMarkers() %>%
      clearControls() %>%
      setView(lng = mainreactEFD$Longitude[1], lat = mainreactEFD$Latitude[1], zoom = 7) %>%
      addAwesomeMarkers(
        clusterOptions = markerClusterOptions(disableClusteringAtZoom = 15),
        lng = ~Longitude,
        lat = ~Latitude,
        popup = values.efdmasterlist,
        icon = makeAwesomeIcon(
          icon = "education",
          library = "glyphicon",
          markerColor = case_when(mainreactEFD$FundingCategory == "Before 2025" ~ "red", mainreactEFD$FundingCategory == "2025-2030" ~ "green", mainreactEFD$FundingCategory == "After 2030" ~ "blue") # Use the new factor variable
        )
      ) %>%
      addLegend(
        "bottomright",
        pal = color_palette,
        values = ~FundingCategory, # Use the new factor variable
        title = "Funding Year",
        opacity = 1
      )
    
     dfreact_fac <- reactive({
       if (is.null(input$FacMapping_bounds)) {
         mainreactEFD
       } else {
         bounds <- input$FacMapping_bounds
         latRng <- range(bounds$north, bounds$south)
         lngRng <- range(bounds$east, bounds$west)
         
         subset(mainreactEFD,
                Latitude >= latRng[1] & Latitude <= latRng[2] & Longitude >= lngRng[1] & Longitude <= lngRng[2])
      }
     })
     
     output$FacTable <- DT::renderDT(server = FALSE, {
       datatable(dfreact_fac() %>% 
                   select("Region","Division","School.Name","FundingYear","Allocation") %>%
                   rename("School" = School.Name, "Funding Year" = FundingYear),
                 extension = 'Buttons',
                 rownames = FALSE,
                 options = list(scrollX = TRUE, pageLength = 10, columnDefs = list(list(className = 'dt-center', targets ="_all")), dom = 'Bfrtip', buttons = list('csv','excel','pdf','print')))
     })
     
     color_palette_cong <- colorFactor(
       palette = c("red", "green", "blue"),
       domain = mainreactNTP$Congestion.Index,
       levels = levels(mainreactNTP$Congestion.Index) # Ensure the order is respected
     )
     
     values.congest <- paste(strong("SCHOOL INFORMATION"),"<br>School Name:",mainreactNTP$School.Name,"<br>School ID:",mainreactNTP$SchoolID,"<br>Instructional Rooms (2023-2024):",mainreactNTP$Instructional.Rooms.2023.2024,"<br>Enrolment (2023-2024):",mainreactNTP$Enrolment.2023.2024,"<br>Congestion Index:",mainreactNTP$Congestion.Index) %>% lapply(htmltools::HTML)
     
     # 3. Use the new factor variable and color palette in your leaflet code
     leafletProxy("CongestMapping", data = mainreactNTP) %>%
       clearMarkers() %>%
       clearControls() %>%
       setView(lng = mainreactNTP$Longitude[1], lat = mainreactNTP$Latitude[1], zoom = 7) %>%
       addAwesomeMarkers(
         clusterOptions = markerClusterOptions(disableClusteringAtZoom = 15),
         lng = ~Longitude,
         lat = ~Latitude,
         label = values.congest,
         icon = makeAwesomeIcon(
           icon = "education",
           library = "glyphicon",
           markerColor = case_when(mainreactNTP$Congestion.Index >= 0 & mainreactNTP$Congestion.Index < 0.25 ~ "green", mainreactNTP$Congestion.Index >= 0.25 & mainreactNTP$Congestion.Index < 0.5 ~ "green", mainreactNTP$Congestion.Index >= 0.5 & mainreactNTP$Congestion.Index < 0.75 ~ "orange",mainreactNTP$Congestion.Index >= 0.75 ~ "red")))
     
     dfreact_cong <- reactive({
       if (is.null(input$CongestMapping_bounds)) {
         mainreactNTP %>% arrange(desc(Congestion.Index))
       } else {
         bounds <- input$CongestMapping_bounds
         latRng <- range(bounds$north, bounds$south)
         lngRng <- range(bounds$east, bounds$west)
         
         subset(mainreactNTP,
                Latitude >= latRng[1] & Latitude <= latRng[2] & Longitude >= lngRng[1] & Longitude <= lngRng[2])
       }
     })
     
     output$CongestTable <- DT::renderDT(server = FALSE, {
       datatable(dfreact_cong() %>% 
                   select("Region","Division","School.Name","Instructional.Rooms.2023.2024","Enrolment.2023.2024","Congestion.Index") %>%
                   rename("School" = School.Name, "Instructional Rooms" = Instructional.Rooms.2023.2024, "Total Enrolment" = Enrolment.2023.2024, "Congestion Index" = Congestion.Index),
                 extension = 'Buttons',
                 rownames = FALSE,
                 options = list(scrollX = TRUE, pageLength = 10, columnDefs = list(list(className = 'dt-center', targets ="_all")), dom = 'Bfrtip', buttons = list('csv','excel','pdf','print')))
     })
     
  })

  observeEvent(input$TextTable_rows_selected, {
    
    Text <- input$text
    
    mainreact1 <- uni %>% arrange(Division) %>% filter(grepl(Text, as.character(School.Name), ignore.case = TRUE))
    
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
    
    row_selected = df1()[input$TextTable_rows_selected,]
    leafletProxy("TextMapping") %>%
      setView(lng = row_selected$Longitude, lat = row_selected$Latitude, zoom = 15)
    
    rowselected_table1 <- row_selected %>% select(Region,Province,Municipality,Division,District,Barangay,Street.Address,SchoolID,School.Name,School.Head.Name,SH.Position,Implementing.Unit,Modified.COC,Latitude,Longitude) %>% rename("Modified Curricular Offering" = Modified.COC, "School ID" = SchoolID, "School Name" = School.Name, "Street Address" = Street.Address, "Implementing Unit" = Implementing.Unit, "School Head" = School.Head.Name,"School Head Position" = SH.Position) %>% mutate(dplyr::across(tidyr::everything(), as.character)) %>% pivot_longer(
      cols = everything(),    # Pivot all columns selected in details_to_pivot
      names_to = "Basic Info",     # Name of the new column holding the original column names
      values_to = "Data")     # Name of the new column holding the original values
    
    rowselected_table2 <- row_selected %>% select(ES.Excess,ES.Shortage,JHS.Excess,JHS.Shortage,SHS.Excess,SHS.Shortage,ES.Teachers,JHS.Teachers,SHS.Teachers,ES.Enrolment,JHS.Enrolment,SHS.Enrolment,School.Size.Typology,Clustering.Status,Outlier.Status) %>% rename("ES Teachers"=ES.Teachers,"JHS Teachers"=JHS.Teachers,"SHS Teachers"=SHS.Teachers, "ES Enrolment" = ES.Enrolment, "JHS Enrolment" = JHS.Enrolment, "SHS Enrolment" = SHS.Enrolment, "School Size Typology" = School.Size.Typology, "AO II Deployment" = Clustering.Status,"COS Deployment" = Outlier.Status, "ES Shortage" = ES.Shortage,"ES Excess" = ES.Excess,"JHS Shortage" = JHS.Shortage,"JHS Excess" = JHS.Excess,"SHS Shortage" = SHS.Shortage,"SHS Excess" = SHS.Excess) %>% mutate(dplyr::across(tidyr::everything(), as.character)) %>% pivot_longer(
      cols = everything(),    # Pivot all columns selected in details_to_pivot
      names_to = "HR Data",     # Name of the new column holding the original column names
      values_to = "Data")     # Name of the new column holding the original values
    
    rowselected_table3 <- row_selected %>% select(Buildings,Instructional.Rooms.2023.2024,Classroom.Requirement,Est.CS,Buidable_space,Major.Repair.2023.2024,SBPI,Shifting,OwnershipType,ElectricitySource,WaterSource,Total.Seats.2023.2024,Total.Seats.Shortage.2023.2024) %>% rename("With Buildable Space" = Buidable_space,"Number of Instructional Rooms" = Instructional.Rooms.2023.2024,"Classroom Requirement" = Classroom.Requirement,"Ownership Type" = OwnershipType,"Source of Electricity" = ElectricitySource,"Source of Water" = WaterSource,"Estimated Classroom Shortage"= Est.CS,"School Building Priority Index" = SBPI,"For Major Repairs"= Major.Repair.2023.2024,"Total Seats"=Total.Seats.2023.2024,"Total Seats Shortage"=Total.Seats.Shortage.2023.2024, "Number of Buildings"=Buildings) %>% mutate(dplyr::across(tidyr::everything(), as.character)) %>% pivot_longer(
      cols = everything(),    # Pivot all columns selected in details_to_pivot
      names_to = "Classroom Data",     # Name of the new column holding the original column names
      values_to = "Data")     # Name of the new column holding the original values
    
    
    rowselected_table4 <- row_selected %>% select(SHA.2021.Index,Travel..Cost,Travel.Time,No.Piped.Water,No.Grid.Electricity,No.Internet,Conflict,TLS) %>% rename("HI 2021" = SHA.2021.Index,"Travel Cost" = Travel..Cost,"Travel Time" = Travel.Time,"No Access to Piped Water" = No.Piped.Water,"No Access to Grid Electricity"= No.Grid.Electricity,"No Access to Internet" = No.Internet,"Incidence of Conflict" = Conflict,"Existence of Temporary Learning Spaces"= TLS) %>% mutate(dplyr::across(tidyr::everything(), as.character)) %>% pivot_longer(
      cols = everything(),    # Pivot all columns selected in details_to_pivot
      names_to = "Other Data",     # Name of the new column holding the original column names
      values_to = "Data")     # Name of the new column holding the original values
    
    rowselected_table5 <- row_selected %>% select(English,Mathematics,Science,Biological.Sciences,Physical.Sciences,General.Ed,Araling.Panlipunan,TLE,MAPEH,Filipino,ESP,Agriculture,ECE,SPED) %>% rename("Biological Sciences" = Biological.Sciences,"Physical Sciences" = Physical.Sciences,"General Education" = General.Ed,"Araling Panlipunan" = Araling.Panlipunan,"Early Chilhood Education" = ECE) %>% mutate(dplyr::across(tidyr::everything(), as.character)) %>% pivot_longer(
      cols = everything(),    # Pivot all columns selected in details_to_pivot
      names_to = "Other Data",     # Name of the new column holding the original column names
      values_to = "Data")     # Name of the new column holding the original values
    
    output$schooldetails <- renderTable({
      # Pass the pivoted data frame directly
      rowselected_table1
    },
    rownames = FALSE, # Don't show automatic row names
    colnames = TRUE,  # Show column names (Field, Value)
    hover = TRUE,     # Add hover effect to rows (optional styling)
    bordered = TRUE)
    
    output$schooldetails2 <- renderTable({
      # Pass the pivoted data frame directly
      rowselected_table2
    },
    rownames = FALSE, # Don't show automatic row names
    colnames = TRUE,  # Show column names (Field, Value)
    hover = TRUE,     # Add hover effect to rows (optional styling)
    bordered = TRUE)
    
    output$schooldetails3 <- renderTable({
      # Pass the pivoted data frame directly
      rowselected_table3
    },
    rownames = FALSE, # Don't show automatic row names
    colnames = TRUE,  # Show column names (Field, Value)
    hover = TRUE,     # Add hover effect to rows (optional styling)
    bordered = TRUE)
    
    output$schooldetails4 <- renderTable({
      # Pass the pivoted data frame directly
      rowselected_table4
    },
    rownames = FALSE, # Don't show automatic row names
    colnames = TRUE,  # Show column names (Field, Value)
    hover = TRUE,     # Add hover effect to rows (optional styling)
    bordered = TRUE)
    
    output$schooldetails5 <- renderTable({
      # Pass the pivoted data frame directly
      rowselected_table5
    },
    rownames = FALSE, # Don't show automatic row names
    colnames = TRUE,  # Show column names (Field, Value)
    hover = TRUE,     # Add hover effect to rows (optional styling)
    bordered = TRUE)
    
  })# Add borders to the table (optional styling)
  
  observeEvent(input$CongestTable_rows_selected, {
    
    
    RegRCT <- input$resource_map_region
    SDORCT1 <- input$Resource_SDO
    DistRCT1 <- input$Resource_LegDist
    
    mainreactNTP <- uni %>% filter(Region == RegRCT) %>% filter(Division == SDORCT1) %>% filter(Legislative.District == DistRCT1)
    
    dfreact_cong <- reactive({
      if (is.null(input$CongestMapping_bounds)) {
        mainreactNTP
      } else {
        bounds <- input$CongestMapping_bounds
        latRng <- range(bounds$north, bounds$south)
        lngRng <- range(bounds$east, bounds$west)
        
        subset(mainreactNTP,
               Latitude >= latRng[1] & Latitude <= latRng[2] & Longitude >= lngRng[1] & Longitude <= lngRng[2])
      }
    })
    
    row_selected = dfreact_cong()[input$CongestTable_rows_selected,]
    leafletProxy("CongestMapping") %>%
      setView(lng = row_selected$Longitude, lat = row_selected$Latitude, zoom = 15)})
  
  observeEvent(input$CLTable_rows_selected, {
    
    RegRCT <- input$resource_map_region
    SDORCT3 <- input$Resource_SDO
    DistRCT3 <- input$Resource_LegDist
    
    mainreact1x <- uni %>% filter(Region == RegRCT) %>% filter(Division == SDORCT3) %>% filter(Legislative.District == DistRCT3) %>% arrange(desc(SBPI))
    
    CL1 <- reactive({
      
      if (is.null(input$CLMapping_bounds)) {
        mainreact1x
      } else {
        bounds <- input$CLMapping_bounds
        latRng <- range(bounds$north, bounds$south)
        lngRng <- range(bounds$east, bounds$west)
        
        subset(mainreact1x,
               Latitude >= latRng[1] & Latitude <= latRng[2] & Longitude >= lngRng[1] & Longitude <= lngRng[2])
      }
    })
    
    row_selected = CL1()[input$CLTable_rows_selected,]
    leafletProxy("CLMapping") %>%
      setView(lng = row_selected$Longitude, lat = row_selected$Latitude, zoom = 15)})
  
  observeEvent(input$FacTable_rows_selected, {
    
    RegRCT <- input$resource_map_region
    SDORCT1 <- input$Resource_SDO
    DistRCT1 <- input$Resource_LegDist
    Lev <- input$resource_map_level
    TypeEFD <- input$EFD_Type
    
    mainreactEFD <- EFDMP %>% 
      filter(!is.na(Old.Region), Old.Region != "") %>% 
      filter(!is.na(Latitude), !is.na(Longitude)) %>% 
      mutate(Latitude = as.numeric(Latitude),
             Allocation = dollar(Allocation, prefix = "₱")) %>%  # Use 'dollar' to format Allocation) %>% 
      distinct(SchoolID, FundingYear, Allocation, Category, .keep_all = TRUE) %>%
      arrange(FundingYear) %>% 
      filter(Region == input$resource_map_region) %>%
      filter(Division == input$Resource_SDO) %>%
      filter(Category %in% input$EFD_Type) %>% 
      mutate(FundingCategory = factor(
        case_when(
          FundingYear < 2025 ~ "Before 2025",
          (FundingYear >= 2025 & FundingYear <= 2030) ~ "2025-2030",
          FundingYear > 2030 ~ "After 2030"
        ),
        levels = c("Before 2025", "2025-2030", "After 2030")
      ))
    
    dfreact_fac <- reactive({
      if (is.null(input$FacMapping_bounds)) {
        mainreactEFD
      } else {
        bounds <- input$FacMapping_bounds
        latRng <- range(bounds$north, bounds$south)
        lngRng <- range(bounds$east, bounds$west)
        
        subset(mainreactEFD,
               Latitude >= latRng[1] & Latitude <= latRng[2] & Longitude >= lngRng[1] & Longitude <= lngRng[2])
      }
    })
    
    row_selected = dfreact_fac()[input$FacTable_rows_selected,]
    leafletProxy("FacMapping") %>%
      setView(lng = row_selected$Longitude, lat = row_selected$Latitude, zoom = 15)})
  
  observeEvent(input$AO2Table_rows_selected, {
    
    RegRCT <- input$resource_map_region
    SDORCT2 <- input$Resource_SDO
    DistRCT2 <- input$Resource_LegDist
    
    Ao2Filter <- uni %>% filter(Region == RegRCT) %>% filter(Division == SDORCT2) %>% filter(Legislative.District == DistRCT2)
    
    SDOfillup <- SDO[which(SDO$Division==SDORCT2),"FillUpRate"]
    Unfilled <- SDO[which(SDO$Division==SDORCT2),"Unfilled"]
    
    xy1 <- reactive({
      
      if (is.null(input$AO2Mapping_bounds)) {
        Ao2Filter
      } else {
        bounds <- input$AO2Mapping_bounds
        latRng <- range(bounds$north, bounds$south)
        lngRng <- range(bounds$east, bounds$west)
        
        subset(Ao2Filter,
               Latitude >= latRng[1] & Latitude <= latRng[2] & Longitude >= lngRng[1] & Longitude <= lngRng[2])
      }
    })
    
    row_selected = xy1()[input$AO2Table_rows_selected,]
    leafletProxy("AO2Mapping") %>%
      setView(lng = row_selected$Longitude, lat = row_selected$Latitude, zoom = 15)
  })
  
  observeEvent(input$TeacherShortage_Table_rows_selected, {
    
    RegRCT <- input$resource_map_region
    SDORCT1 <- input$Resource_SDO
    DistRCT1 <- input$Resource_LegDist
    Lev <- input$resource_map_level
    
    mainreact1 <- df %>% filter(Region == RegRCT) %>% filter(Division == SDORCT1) %>% filter(Legislative.District == DistRCT1) %>% filter(Level == Lev) %>% arrange(desc(TeacherShortage))
    
    SDOfillup <- SDO[which(SDO$Division==SDORCT1),"FillUpRate"]
    Unfilled <- SDO[which(SDO$Division==SDORCT1),"Unfilled"]
    
    NetShortage <- df %>% select(Region,Division,Level,TeacherShortage,TeacherExcess) %>%
      pivot_longer(cols = c(TeacherExcess, TeacherShortage), names_to = "Inventory", values_to = "Count") %>% mutate(Count=as.numeric(Count)) %>% na.omit(Count) %>% group_by(Region, Division,Level, Inventory) %>% summarize(Count = sum(Count)) %>% pivot_wider(names_from = "Inventory", values_from = "Count") %>% mutate(NetShortage=TeacherShortage-TeacherExcess) %>% mutate(NetShortage = ifelse(NetShortage < 0, 0, NetShortage))
    
    SDONetShortage <- NetShortage %>% filter(Region == RegRCT) %>% filter(Division == SDORCT1) %>% filter(Level == Lev)
    
    
    df1 <- reactive({
      
      if (is.null(input$TeacherShortage_Mapping_bounds)) {
        mainreact1
      } else {
        bounds <- input$TeacherShortage_Mapping_bounds
        latRng <- range(bounds$north, bounds$south)
        lngRng <- range(bounds$east, bounds$west)
        
        subset(mainreact1,
               Latitude >= latRng[1] & Latitude <= latRng[2] & Longitude >= lngRng[1] & Longitude <= lngRng[2])
      }
    })
    
    row_selected = df1()[input$TeacherShortage_Table_rows_selected,]
    leafletProxy("TeacherShortage_Mapping") %>%
      setView(lng = row_selected$Longitude, lat = row_selected$Latitude, zoom = 15)
    
    output$d <- renderValueBox({
      valueBox(tags$p(strong(row_selected$TeacherShortage), style = "font-size: 65%;"), subtitle = tags$p(strong("School Teacher Shortage"), style = "font-size: 60%;"), color = "red")})
    
    output$TeacherShortage_Assessment <- renderUI({
      p(HTML(paste(strong(row_selected$School.Name),"is located in the Division of", strong(row_selected$Division),". According to the PSIPOP data as of December 2024, this SDO has ",strong(SDOfillup)," filling-up rate with ",strong(Unfilled),"unfilled item/s. Moreover, this SDO has a net shortage of ",strong(SDONetShortage$NetShortage),"teacher/s for the selected level based on SY 2024-2025 enrolment data and teaching inventory using the standard planning parameters. The said school has a shortage of ",strong(row_selected$TeacherShortage),"teacher/s and a total excess of",strong(sum(df1()$TeacherExcess, na.rm = TRUE)),"teacher/s captured within the map and table above")), style = "font-family: Century Gothic; font-size: 15px; color: #111111;")
    })
    
    
    EDtable <- row_selected %>% select(School.Name,Kinder,G1,G2,G3,G4,G5,G6,G7,G8,G9,G10,G11,G12)
    Loctable <- row_selected %>% select(School.Name,Province,Municipality,District, Barangay,Street.Address)
    Spectable <- row_selected %>% select(School.Name,English,Mathematics,Science,Biological.Sciences,Physical.Sciences) 
    
    observeEvent(input$SelectSchoolData, {
      selectSD = input$SelectSchoolData
      
      if (selectSD == "Enrolment Data") {
        output$SchoolData <- DT::renderDT(EDtable, rownames = FALSE, options = list(scrollX = TRUE, columnDefs = list(list(className = 'dt-left', targets ="_all"))))}
      else {if (selectSD == "School Location") {
        output$SchoolData <- DT::renderDT(Loctable, rownames = FALSE, options = list(scrollX = TRUE, columnDefs = list(list(className = 'dt-left', targets ="_all"))))}
        else {if  (selectSD == "Specialization") {
          output$SchoolData <- DT::renderDT(Spectable, rownames = FALSE, options = list(scrollX = TRUE, columnDefs = list(list(className = 'dt-left', targets ="_all"))))}
        }}})
    
  }, ignoreNULL = TRUE, ignoreInit = TRUE)
  
  observeEvent(input$GMISRun, {
    
    dfGMIS <- read.csv("GMIS-FillingUpPerPosition-2025.csv")
    
    RegGMISRCT <- input$RegionGMIS
    SDOGMISRCT <- input$SDOGMIS
    PosCatGMISRCT <- input$PosCatGMIS
    PosSelGMISRCT <- input$PosSelGMIS
    
    # 1. Corrected: Filter only. Keep all columns.
    GMISDiv <- dfGMIS %>% 
      filter(GMIS.Region %in% RegGMISRCT) %>% 
      filter(Position %in% PosSelGMISRCT) %>% 
      filter(GMIS.Division %in% SDOGMISRCT) %>% 
      group_by(Position) %>% 
      summarise(Filled = sum(Total.Filled), Unfilled = sum(Total.Unfilled, na.rm = TRUE)) %>% 
      pivot_longer(cols = c("Filled","Unfilled"), names_to = "Status", values_to = "Count")
    
    # %>% 
    #   mutate(Filling.Up.Rate = round(Filled/Authorized, digits = 4)*100)
    
    # This part is for a separate data table and can remain as-is.
    # It correctly performs its own summarization on the filtered GMISDiv.
    # GMISreactFinalDiv <- GMISDiv %>% 
    #   group_by(GMIS.Region,GMIS.Division,Position) %>% 
    #   summarise(Filled = sum(Total.Filled), 
    #             Authorized = sum(Total.Authorized)) |> 
    #   mutate(Filling.Up.Rate = round(Filled/Authorized, digits = 4)*100)
    # 
    # GMISreactFinalDiv <- GMISreactFinalDiv |> 
    #   mutate(Filling.Up.Rate = sprintf("%.2f", `Filling.Up.Rate`)) %>% 
    #   rename("Division" = GMIS.Division, "Filling Up Rate" = Filling.Up.Rate)
    
    output$GMISTable <- renderPlotly({
      
      # 2. Corrected: Summarize the data for the plot here.
      # It now works because GMISDiv still contains GMIS.Division.
      plot_data_stacked <- GMISDiv 
      plot_data_totals <- plot_data_stacked %>%
        group_by(Position) %>%
        summarise(TotalCount = sum(Count))
      category_colors <- c("Filled" = "blue", "Unfilled" = "red")
      
      
      # The rest of the plot code remains the same.
      p <- ggplot(plot_data_stacked, aes(x = factor(Position), y = Count, fill = factor(Status, levels = c("Unfilled", "Filled")),
                                         text = paste("Position: ", Position,
                                                      "<br>Category: ", Status,
                                                      "<br>Total", Count))) +
        geom_bar(stat = "identity", position = "stack") +
        
        # Add the text layer for the totals on top of the bars
        geom_text(data = plot_data_totals,
                  aes(x = Position, y = TotalCount * 1.05, label = scales::comma(TotalCount)), # Modified line
                  inherit.aes = FALSE,
                  size = 3.5,
                  color = "black") +
        
        labs(x = "Position", y = "Count") +
        scale_y_continuous(labels = scales::comma) +
        scale_fill_manual(name = "Legend", values = category_colors) +
        theme_minimal() +
        theme(legend.position = "bottom")
      
      ggplotly(p, tooltip = "text", source = "stackedBarPlot") %>% 
        layout(hoverlabel = list(bgcolor = "white"))
    })
    
    # Assuming the above code for creating GMISDiv is already in your server function
    # and `GMISDiv` is a reactive expression. If it's not a reactive, you may need to
    # make it one or define it inside the render function.
    
    output$GMISTable1 <- renderDataTable({
      
      GMISDiv2 <- dfGMIS %>% 
        filter(GMIS.Region %in% RegGMISRCT) %>% 
        filter(Position %in% PosSelGMISRCT) %>% 
        filter(GMIS.Division %in% SDOGMISRCT) %>% 
        group_by(GMIS.Region, GMIS.Division, Position) %>% 
        summarise(
          Filled = sum(Total.Filled), 
          Unfilled = sum(Total.Unfilled, na.rm = TRUE),
          Authorized = sum(Total.Authorized) # Corrected: Add the Authorized column
        ) %>% 
        mutate(Filling.Up.Rate = round(Filled / Authorized, digits = 4) * 100) %>%
        mutate(Filling.Up.Rate = sprintf("%.2f", `Filling.Up.Rate`)) %>%
        rename("Division" = GMIS.Division, "Filling Up Rate" = `Filling.Up.Rate`)
      
      # Ensure the data is not empty before rendering
      req(GMISDiv2) 
      
      # The GMISDiv is already in the long format, so we can directly pass it to renderDataTable.
      # We use the DT package functions to create a well-formatted and interactive table.
      DT::datatable(
        GMISDiv2,
        filter = 'top', # Adds filter boxes to the top of each column
        extensions = c("FixedColumns", "Buttons"),
        options = list(
          dom = 'Bfrtip',
          buttons = list(
            list(extend = 'csv', filename = 'GMIS_Data'),
            list(extend = 'excel', filename = 'GMIS_Data'), # Excel download button
            list(extend = 'pdf', filename = 'GMIS_Data')    # PDF download button
          ),
          fixedColumns = list(leftColumns = 2),
          columnDefs = list(list(className = 'dt-center', targets = '_all')),
          rownames = FALSE
        )
      )
    })
    
  })

  observeEvent(input$SHSListTable_rows_selected, {
    
    RegRCT <- input$resource_map_region
    
    region_selected <- IndALL %>% filter(Region == RegRCT) %>% arrange(Distance)
    
    mainreact1 <- df %>% filter(Region == RegRCT) %>% filter(Level == "SHS") %>% distinct(SchoolID, .keep_all = TRUE) %>% filter(SchoolID %in% SHS_Pilot2)
    
    df1 <- reactive({
      
      if (is.null(input$SHSMapping_bounds)) {
        mainreact1
      } else {
        bounds <- input$SHSMapping_bounds
        latRng <- range(bounds$north, bounds$south)
        lngRng <- range(bounds$east, bounds$west)
        
        subset(mainreact1,
               Latitude >= latRng[1] & Latitude <= latRng[2] & Longitude >= lngRng[1] & Longitude <= lngRng[2])
      }
    })
    
    row_selected = df1()[input$SHSListTable_rows_selected,]
    rowschool = row_selected$School.Name
    leafletProxy("SHSMapping") %>%
      setView(lng = row_selected$Longitude, lat = row_selected$Latitude, zoom = 15)
    
    SHSIndustries <- region_selected %>% filter(School.Name %in% rowschool) %>% select("Company","Sector","Distance") %>% rename("Distance in KM" = Distance)
    
    output$dataTableSHS <- DT::renderDT({datatable(SHSIndustries, extensions = "FixedColumns", options = list(fixedColumns = list(leftColumns = 2), columnDefs = list(list(className = 'dt-center', targets ="_all")), rownames = FALSE))})
    
    output$AccoCount <- renderValueBox({
      valueBox(tags$p(strong(sum(SHSIndustries$Sector == "Manufacturing and Engineering")), style = "font-size: 100%; text-align: center;"), subtitle = NULL)})
    
    output$ProfCount <- renderValueBox({
      valueBox(tags$p(strong(sum(SHSIndustries$Sector == "Hospitality and Tourism")), style = "font-size: 100%; text-align: center;"), subtitle = NULL)})
    
    output$WastCount <- renderValueBox({
      valueBox(tags$p(strong(sum(SHSIndustries$Sector == "Public Administration")), style = "font-size: 100%; text-align: center;"), subtitle = NULL)})
    
    output$TranCount <- renderValueBox({
      valueBox(tags$p(strong(sum(SHSIndustries$Sector == "Professional/Private Services")), style = "font-size: 100%; text-align: center;"), subtitle = NULL)})
    
    output$WholCount <- renderValueBox({
      valueBox(tags$p(strong(sum(SHSIndustries$Sector == "Business and Finance")), style = "font-size: 100%; text-align: center;"), subtitle = NULL)})
    
    output$WholCount2 <- renderValueBox({
      valueBox(tags$p(strong(sum(SHSIndustries$Sector == "Agriculture and Agri-business")), style = "font-size: 100%; text-align: center;"), subtitle = NULL)})
    
    rowselected_SHStable <- SHS_Pilot %>%  filter(SchoolID == row_selected$SchoolID) %>% select(Region,Division,Modified.COC,School.Name,SchoolID,Typology,SHS.Enrolment,ClassesOrganized,TeacherRequirement,JHSTeachers,SHSTeachers,TrackOffering,TrackOffering.Intended,SHS.Packages) %>% rename("School ID" = SchoolID,"Schools Division Office" = Division,"Modified Curricular Offering" = Modified.COC,"School Name" = School.Name,"School Size Typology" = Typology,"Track Offering" = TrackOffering,"SHS Enrolment" = SHS.Enrolment,"Classes Organized" = ClassesOrganized,"Teacher Requirement" = TeacherRequirement,"JHS Teacher Inventory" = JHSTeachers,"SHS Teacher Inventory" = SHSTeachers,"Delivered Learning Packages (2018-2025)"=SHS.Packages,"Track Offering (Intended)"=TrackOffering.Intended) %>% mutate(dplyr::across(tidyr::everything(), as.character)) %>% pivot_longer(
      cols = everything(),    # Pivot all columns selected in details_to_pivot
      names_to = "Profile Item",     # Name of the new column holding the original column names
      values_to = "Data")     # Name of the new column holding the original values
    
    rowselected_SHStablespec <- SHS_Pilot %>%  filter(SchoolID == row_selected$SchoolID) %>% select(English,Mathematics,Science,Biological.Sciences,Physical.Sciences,General.Ed,Araling.Panlipunan,TLE,MAPEH,Filipino,ESP,Agriculture,ECE,SPED) %>% rename("Biological Sciences" = Biological.Sciences,"Physical Sciences" = Physical.Sciences,"General Education" = General.Ed,"Araling Panlipunan" = Araling.Panlipunan,"Early Chilhood Education" = ECE) %>% mutate(dplyr::across(tidyr::everything(), as.character)) %>% pivot_longer(
      cols = everything(),    # Pivot all columns selected in details_to_pivot
      names_to = "Profile Item",     # Name of the new column holding the original column names
      values_to = "Data")     # Name of the new column holding the original values
    
    rowselected_SHStablespec2 <- SHS_Pilot %>% select(English,Mathematics,Science,Biological.Sciences,Physical.Sciences,General.Ed,Araling.Panlipunan,TLE,MAPEH,Filipino,ESP,Agriculture,ECE,SPED) %>% rename("Biological Sciences" = Biological.Sciences,"Physical Sciences" = Physical.Sciences,"General Education" = General.Ed,"Araling Panlipunan" = Araling.Panlipunan,"Early Chilhood Education" = ECE) %>% mutate(dplyr::across(tidyr::everything(), as.character)) %>% pivot_longer(
      cols = everything(),    # Pivot all columns selected in details_to_pivot
      names_to = "Profile",     # Name of the new column holding the original column names
      values_to = "Data") %>% mutate(Data = as.numeric(Data)) %>% group_by(Profile) %>% summarise(Data = sum(Data, na.rm = TRUE))
    
    output$SHSTablex <- renderTable({
      # Pass the pivoted data frame directly
      rowselected_SHStable
    },
    rownames = FALSE, # Don't show automatic row names
    colnames = TRUE,  # Show column names (Field, Value)
    hover = TRUE,     # Add hover effect to rows (optional styling)
    bordered = TRUE)
    
    output$PilotSpec <- renderTable({
      # Pass the pivoted data frame directly
      rowselected_SHStablespec
    },
    rownames = FALSE, # Don't show automatic row names
    colnames = TRUE,  # Show column names (Field, Value)
    hover = TRUE,     # Add hover effect to rows (optional styling)
    bordered = TRUE)
    
  })
  

output$school_count_division_graph <- renderPlotly({
  
  current_filtered_data <- filtered_school_data_division() # Use the reactive filtered data
  
  # --- Empty Data Handling ---
  if (nrow(current_filtered_data) == 0) {
    return(ggplotly(ggplot() +
                      annotate("text", x = 0.5, y = 0.5, label = "No data for selected regions/divisions") +
                      theme_void()))
  }
  
  # Define the levels for Modified.COC for consistent ordering
  coc_levels <- c("Purely ES", "JHS with SHS", "ES and JHS (K to 10)", "Purely JHS", "All Offering (K to 12)", "Purely SHS")
  
  # Prepare the data for plotting
  # Group by Region and Modified.COC
  plot_data <- current_filtered_data %>%
    group_by(Division, Modified.COC) %>%
    summarise(Count = n(), .groups = 'drop')
  
  plot_data_totals <- plot_data %>%
    group_by(Modified.COC) %>%
    summarise(TotalCount = sum(Count))
  
  
  # Calculate total counts per Region for reordering the X-axis
  division_total_counts <- plot_data %>%
    group_by(Division) %>%
    summarise(Total_Division_Count = sum(Count), .groups = 'drop')
  
  # Join total counts back to plot_data for reordering
  plot_data <- plot_data %>%
    left_join(division_total_counts, by = "Division") %>%
    # Create a reordered factor for Region based on Total_Region_Count
    mutate(Division_reordered = reorder(Division, Total_Division_Count))
  
  
  # Create the ggplot
  p <- ggplot(plot_data,
              aes(x = factor(Modified.COC, levels = coc_levels), # Regions on the X-axis
                  y = Count,
                  fill = Division_reordered, # Fill by Modified.COC
                  # IMPORTANT: Add 'key' aesthetic here to store combined info for click events
                  key = paste(Division, Modified.COC),
                  text = paste("Division: ", Division,
                               "<br>School Type: ", Modified.COC,
                               "<br>Count: ", scales::comma(Count)))) + # Custom tooltip text
    geom_bar(stat = "identity", position = "stack", color = "black", size = 0.25) + # Dodged bars
    geom_text(data = plot_data_totals,
              aes(x = Modified.COC, y = TotalCount * 1.05, label = scales::comma(TotalCount)), # Modified line
              inherit.aes = FALSE,
              size = 3.5,
              color = "black") +
    labs(x = "Region",
         y = "Number of Schools",
         fill = "School Type") +
    scale_y_continuous(labels = scales::comma) + # Format y-axis labels as comma-separated numbers
    theme_minimal() +
    theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
          legend.position = "bottom", # Position legend at the bottom
          plot.title = element_text(hjust = 0.5)) # Center the plot title
  
  # Convert ggplot to plotly, ensuring custom text is used for hover
  # Ensure the 'key' is passed through to plotly as 'customdata' for reliable clicks
  ggplotly(p, tooltip = "text", source = "schoolcountplot_division") %>%
    layout(hoverlabel = list(bgcolor = "white"),
           hovermode = "closest",
           margin = list(b = 100))
})

output$school_count_district_graph <- renderPlotly({
  
  current_filtered_data <- filtered_school_data_division()
  
  # --- Empty Data Handling ---
  if (nrow(current_filtered_data) == 0) {
    return(ggplotly(ggplot() +
                      annotate("text", x = 0.5, y = 0.5, label = "No data for selected regions/Legislative.Districts") +
                      theme_void()))
  }
  
  # Define the levels for Modified.COC for consistent ordering
  coc_levels <- c("Purely ES", "JHS with SHS", "ES and JHS (K to 10)", "Purely JHS", "All Offering (K to 12)", "Purely SHS")
  
  # Prepare the data for plotting
  plot_data <- current_filtered_data %>%
    group_by(Division, Legislative.District, Modified.COC) %>%
    summarise(Count = n(), .groups = 'drop') %>%
    # Use as.character() on the grouping variables right away
    mutate(
      Division = as.character(Division),
      Legislative.District = as.character(Legislative.District),
      Modified.COC = as.character(Modified.COC)
    )
  
  plot_data_totals <- plot_data %>%
    group_by(Modified.COC) %>%
    summarise(TotalCount = sum(Count))
  
  # Calculate total counts per Legislative.District for reordering the X-axis
  Legislative.District_total_counts <- plot_data %>%
    group_by(Division, Legislative.District) %>%
    summarise(Total_Legislative.District_Count = sum(Count), .groups = 'drop')
  
  # Join total counts back to plot_data for reordering and create concatenated variable
  plot_data <- plot_data %>% mutate(Division_LegDist = paste0(Division, "- ", Legislative.District))
  
  # Create the ggplot
  p <- ggplot(plot_data,
              aes(x = factor(Modified.COC, levels = coc_levels),
                  y = Count,
                  fill = Division_LegDist,
                  text = paste("Legislative District:", Division_LegDist,
                               "<br>Modified.COC:", Modified.COC,
                               "<br>Count:", scales::comma(Count)))) +
    geom_bar(stat = "identity", position = "stack", color = "black", size = 0.25) +
    geom_text(data = plot_data_totals,
              aes(x = Modified.COC, y = TotalCount * 1.05, label = scales::comma(TotalCount)), # Modified line
              inherit.aes = FALSE,
              size = 3.5,
              color = "black") +
    labs(x = "Modified COC",
         y = "Count",
         fill = "Legislative District") +
    scale_y_continuous(labels = scales::comma) +
    theme_minimal() +
    theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
          legend.position = "bottom",
          plot.title = element_text(hjust = 0.5))
  
  # Convert ggplot to plotly, specifying the correct tooltip aesthetic
  ggplotly(p, tooltip = "text") %>%
    layout(
      hoverlabel = list(bgcolor = "white"),
      hovermode = "closest",
      margin = list(b = 100)
    )
})


# In your server.R file, ensure 'rv' is initialized once, e.g., at the top of the server function:
# rv <- reactiveValues(latest_sosss_click_key = NULL) # Only if not already initialized

# Observer for SOSSS_Region_Typology clicks
# This should be placed in your server.R file, outside of any output$... block,
# but within the main server function.
observeEvent(event_data("plotly_click", source = "sosssRegionPlot"), {
  click_data <- event_data("plotly_click", source = "sosssRegionPlot")
  if (!is.null(click_data)) {
    # Store the combined key (e.g., "Region I Very Small")
    rv$latest_sosss_click_key <- click_data$key
  } else {
    # If a click somehow clears, reset the key
    rv$latest_sosss_click_key <- NULL
  }
})

output$SOSSS_Division_Typology <- renderPlotly({
  
  # Assume 'filtered_school_data_region' provides the 'uni' data filtered by dashboard region and division.
  current_filtered_data <- filtered_school_data_division() # Use the reactive filtered data
  
  # --- Empty Data Handling ---
  if (nrow(current_filtered_data) == 0) {
    return(ggplotly(ggplot() +
                      annotate("text", x = 0.5, y = 0.5, label = "No data for selected regions/divisions") +
                      theme_void()))
  }
  
  # Prepare the data for plotting
  plot_data <- current_filtered_data %>%
    group_by(Division, School.Size.Typology) %>%
    summarise(Count = n(), .groups = 'drop')
  
  plot_data_totals <- plot_data %>%
    group_by(School.Size.Typology) %>%
    summarise(TotalCount = sum(Count))
  
  # Calculate total counts per region for reordering (optional, but good practice for clarity)
  region_totals <- plot_data %>%
    group_by(Division) %>%
    summarise(TotalCount = sum(Count), .groups = 'drop')
  
  plot_data <- plot_data %>%
    left_join(region_totals, by = "Division") %>%
    mutate(Division_reordered = reorder(Division, -TotalCount))
  
  # Define the levels for School.Size.Typology for consistent ordering
  sosss_levels <- c("Very Small", "Small", "Medium", "Large", "Very Large", "Extremely Large", "Mega")
  
  # Create the ggplot
  p <- ggplot(plot_data,
              aes(x = factor(School.Size.Typology, levels = sosss_levels),
                  y = Count,
                  fill = Division_reordered,
                  # IMPORTANT: Add 'key' aesthetic here to store combined info for click events
                  key = paste(Division, School.Size.Typology),
                  text = paste("Region: ", Division,
                               "<br>School Size: ", School.Size.Typology,
                               "<br>Count: ", scales::comma(Count)))) + # Custom tooltip text
    geom_bar(stat = "identity", position = "stack", color = "black", size = 0.25) + # Label for each bar
    geom_text(data = plot_data_totals,
              aes(x = School.Size.Typology, y = TotalCount + 10 * (max(TotalCount) / TotalCount), label = scales::comma(TotalCount)), # Modified line
              inherit.aes = FALSE,
              size = 3.5,
              color = "black") +
    labs(x = "School Size Typology",
         y = "Number of Schools",
         fill = "Division") +
    scale_y_continuous(labels = scales::comma) + # Format y-axis labels as comma-separated numbers
    theme_minimal() +
    theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
          legend.position = "bottom", # Position legend at the bottom
          plot.title = element_text(hjust = 0.5)) # Center the plot title
  
  # Convert ggplot to plotly, ensuring custom text is used for hover
  ggplotly(p, tooltip = "text", source = "sosssRegionPlot") %>%
    layout(hoverlabel = list(bgcolor = "white"),
           hovermode = "closest", # Changed to "closest" as per previous request
           margin = list(b = 100)) # Increase bottom margin for x-axis labels
})

output$SOSSS_District_Typology <- renderPlotly({
  
  # Assume 'filtered_school_data_region' provides the 'uni' data filtered by dashboard region and Legislative.District.
  current_filtered_data <- filtered_school_data_division() # Use the reactive filtered data
  
  # --- Empty Data Handling ---
  if (nrow(current_filtered_data) == 0) {
    return(ggplotly(ggplot() +
                      annotate("text", x = 0.5, y = 0.5, label = "No data for selected regions/Legislative.Districts") +
                      theme_void()))
  }
  
  # Prepare the data for plotting
  plot_data <- current_filtered_data %>%
    group_by(Division, Legislative.District, School.Size.Typology) %>%
    summarise(Count = n(), .groups = 'drop')
  
  plot_data_totals <- plot_data %>%
    group_by(School.Size.Typology) %>%
    summarise(TotalCount = sum(Count))
  
  # Calculate total counts per region for reordering (optional, but good practice for clarity)
  region_totals <- plot_data %>%
    group_by(Legislative.District) %>%
    summarise(TotalCount = sum(Count), .groups = 'drop')
  
  plot_data <- plot_data %>%
    left_join(region_totals, by = "Legislative.District") %>%
    mutate(Legislative.District_reordered = reorder(Legislative.District, -TotalCount))
  
  plot_data <- plot_data %>% mutate(Division_LegDist = paste0(Division, "- ", Legislative.District))
  
  
  # Define the levels for School.Size.Typology for consistent ordering
  sosss_levels <- c("Very Small", "Small", "Medium", "Large", "Very Large", "Extremely Large", "Mega")
  
  # Create the ggplot
  p <- ggplot(plot_data,
              aes(x = factor(School.Size.Typology, levels = sosss_levels),
                  y = Count,
                  fill = Division_LegDist,
                  # IMPORTANT: Add 'key' aesthetic here to store combined info for click events
                  text = paste("Legislative District: ", Division_LegDist,
                               "<br>School Size: ", School.Size.Typology,
                               "<br>Count: ", scales::comma(Count)))) + # Custom tooltip text
    geom_bar(stat = "identity", position = "stack", color = "black", size = 0.25) + # Label for each bar
    geom_text(data = plot_data_totals,
              aes(x = School.Size.Typology, y = TotalCount * 1.05, label = scales::comma(TotalCount)), # Modified line
              inherit.aes = FALSE,
              size = 3.5,
              color = "black") +
    labs(x = "School Size Typology",
         y = "Number of Schools",
         fill = "Legislative District") +
    scale_y_continuous(labels = scales::comma) + # Format y-axis labels as comma-separated numbers
    theme_minimal() +
    theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
          legend.position = "bottom", # Position legend at the bottom
          plot.title = element_text(hjust = 0.5)) # Center the plot title
  
  # Convert ggplot to plotly, ensuring custom text is used for hover
  ggplotly(p, tooltip = "text", source = "sosssRegionPlot") %>%
    layout(hoverlabel = list(bgcolor = "white"),
           hovermode = "closest", # Changed to "closest" as per previous request
           margin = list(b = 100)) # Increase bottom margin for x-axis labels
})


# Reactive expression for filtering the SOSSS table data based on dashboard filters and plot clicks
filtered_sosss_table_data <- reactive({
  # Ensure uni data is available.
  req(uni)
  
  # Start with the full dataset for filtering based on dashboard inputs
  data <- uni
  
  if (!is.null(input$dashboard_region_filter) && length(input$dashboard_region_filter) > 0) {
    data <- data %>%
      filter(Region %in% input$dashboard_region_filter)
  }
  
  if (!is.null(input$dashboard_division_filter) && length(input$dashboard_division_filter) > 0) {
    data <- data %>%
      filter(Division %in% input$dashboard_division_filter)
  }
  
  # Now, filter based on the latest plot click for SOSSS graph
  clicked_region <- NULL
  clicked_sosss_typology <- NULL
  
  if (!is.null(rv$latest_sosss_click_key)) {
    # Define the levels for School.Size.Typology for consistent mapping and parsing
    sosss_levels_for_parsing <- c("Very Small", "Small", "Medium", "Large", "Very Large", "Extremely Large", "Mega")
    # Sort by length descending to match multi-word typologies first
    sosss_levels_ordered_by_length <- sosss_levels_for_parsing[order(nchar(sosss_levels_for_parsing), decreasing = TRUE)]
    
    found_typology <- FALSE
    for (typology_val in sosss_levels_ordered_by_length) {
      # Check if the typology is at the end of the key string
      if (grepl(paste0(typology_val, "$"), rv$latest_sosss_click_key)) {
        clicked_sosss_typology <- typology_val
        # The region is everything before this typology
        clicked_region <- sub(paste0(" ", typology_val, "$"), "", rv$latest_sosss_click_key)
        found_typology <- TRUE
        break
      }
    }
    
    if (!found_typology) {
      # Fallback if parsing fails, return data filtered only by dashboard inputs
      clicked_region <- NULL
      clicked_sosss_typology <- NULL
      warning("Could not parse clicked SOSSS key for Region and Typology: ", rv$latest_sosss_click_key)
    }
  }
  
  if (!is.null(clicked_region) && !is.null(clicked_sosss_typology)) {
    # Apply filter based on click data if both are valid
    data <- data %>%
      filter(Region == clicked_region,
             School.Size.Typology == clicked_sosss_typology)
  }
  
  return(data)
})


# Your Teacher_Shortage_Regional_Graph renderPlotly
output$Teacher_Shortage_Regional_Graph <- renderPlotly({
  # Use the reactive filtered data, which already applies Region and Division filters
  current_filtered_data <- filtered_teacher_shortage_data_region() # <-- CHANGE: Use reactive data
  
  # --- Empty Data Handling ---
  if (nrow(current_filtered_data) == 0) {
    return(ggplotly(ggplot() +
                      annotate("text", x = 0.5, y = 0.5, label = "No data for selected regions/divisions") +
                      theme_void()))
  }
  
  # Prepare the data for plotting
  plot_data <- current_filtered_data %>%
    pivot_longer(cols = c(ES_Shortage, JHS_Shortage, SHS_Shortage), names_to = "Inventory", values_to = "Count") %>%
    mutate(Count = as.numeric(Count)) %>%
    na.omit() %>%
    group_by(Region, Inventory) %>%
    summarize(Total_Count = sum(Count, na.rm = TRUE), .groups = 'drop')
  
  plot_data_totals <- plot_data %>%
    group_by(Inventory) %>%
    summarise(TotalCount = sum(Total_Count))
  
  # total_labels_data is for stacked bars, not typically used for dodged bars
  # total_labels_data <- plot_data %>%
  #   group_by(Region) %>%
  #   summarise(Grand_Total = sum(Total_Count), .groups = 'drop')

  # Define the levels for Inventory for consistent order
  inventory_levels <- c("ES_Shortage", "JHS_Shortage", "SHS_Shortage")
  
  # Create the ggplot
  p <- ggplot(plot_data,
              aes(x = factor(Inventory, levels = inventory_levels), # Reorder regions based on overall total count for the region
                  y = Total_Count,
                  fill = Region, # Fill by Inventory for coloring and consistent order
                  # IMPORTANT: Add 'key' aesthetic here to store the region+inventory for click events
                  text = paste("Region: ", Region,
                               "<br>School Type: ", gsub("_Shortage", "", Inventory), # Clean up label for tooltip
                               "<br>Count: ", scales::comma(Total_Count)))) +
    geom_bar(stat = "identity", position = "stack", color = "black", size = 0.25) +
    geom_text(data = plot_data_totals,
              aes(x = Inventory, y = TotalCount * 1.05, label = scales::comma(TotalCount)), # Modified line
              inherit.aes = FALSE,
              size = 3.5,
              color = "black") +
    labs(x = "Region",
         y = "Total Shortage Count",
         title = "Regional Teacher Shortage by School Type") +
    scale_y_continuous(labels = scales::comma) +
    theme_minimal() +
    theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
          legend.position = "bottom",
          plot.title = element_text(hjust = 0.5))
  
  # Convert ggplot to plotly, ensuring custom text is used for hover and source for clicks
  ggplotly(p, tooltip = "text", source = "teacherShortageRegionPlot") %>%
    layout(hoverlabel = list(bgcolor = "white"),
           hovermode = "closest", # Ensure hover targets closest bar
           margin = list(b = 100))
})

output$Teacher_Shortage_Division_Graph <- renderPlotly({
  # Use the reactive filtered data, which already applies Division and Division filters
  current_filtered_data <- filtered_teacher_shortage_data_division() # <-- CHANGE: Use reactive data
  
  # --- Empty Data Handling ---
  if (nrow(current_filtered_data) == 0) {
    return(ggplotly(ggplot() +
                      annotate("text", x = 0.5, y = 0.5, label = "No data for selected Divisions/divisions") +
                      theme_void()))
  }
  
  # Prepare the data for plotting
  plot_data <- current_filtered_data %>%
    pivot_longer(cols = c(ES_Shortage, JHS_Shortage, SHS_Shortage), names_to = "Inventory", values_to = "Count") %>%
    mutate(Count = as.numeric(Count)) %>%
    na.omit() %>%
    group_by(Division, Inventory) %>%
    summarize(Total_Count = sum(Count, na.rm = TRUE), .groups = 'drop')
  
  plot_data_totals <- plot_data %>%
    group_by(Inventory) %>%
    summarise(TotalCount = sum(Total_Count))
  
  # total_labels_data is for stacked bars, not typically used for dodged bars
  # total_labels_data <- plot_data %>%
  #   group_by(Division) %>%
  #   summarise(Grand_Total = sum(Total_Count), .groups = 'drop')
  
  # Define specific colors for each inventory type for consistency
  
  # Define the levels for Inventory for consistent order
  inventory_levels <- c("ES_Shortage", "JHS_Shortage", "SHS_Shortage")
  
  # Create the ggplot
  p <- ggplot(plot_data,
              aes(x = factor(Inventory, levels = inventory_levels), # Reorder Divisions based on overall total count for the Division
                  y = Total_Count,
                  fill = Division, # Fill by Inventory for coloring and consistent order
                  # IMPORTANT: Add 'key' aesthetic here to store the Division+inventory for click events
                  key = paste(Division, Inventory, sep = "::"), # Key as per your latest code
                  text = paste("Division: ", Division,
                               "<br>School Type: ", gsub("_Shortage", "", Inventory), # Clean up label for tooltip
                               "<br>Count: ", scales::comma(Total_Count)))) +
    geom_bar(stat = "identity", position = "stack", color = "black", size = 0.25) +
    geom_text(data = plot_data_totals,
              aes(x = Inventory, y = TotalCount * 1.05, label = scales::comma(TotalCount)), # Modified line
              inherit.aes = FALSE,
              size = 3.5,
              color = "black") +
    labs(x = "Inventory",
         y = "Total Shortage Count",
         title = "Division Teacher Shortage by School Type") +
    scale_y_continuous(labels = scales::comma) +
    theme_minimal() +
    theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
          legend.position = "bottom",
          plot.title = element_text(hjust = 0.5))
  
  # Convert ggplot to plotly, ensuring custom text is used for hover and source for clicks
  ggplotly(p, tooltip = "text", source = "teacherShortageDivisionPlot") %>%
    layout(hoverlabel = list(bgcolor = "white"),
           hovermode = "closest", # Ensure hover targets closest bar
           margin = list(b = 100))
})

# In your server.R file, ensure 'rv' is initialized with the new reactive value:
# rv <- reactiveValues(
#   latest_click_key = NULL,           # For school_count graph
#   latest_sosss_click_key = NULL,     # For SOSSS graph
#   latest_classroom_click_key = NULL, # For Classroom Shortage graph
#   latest_teacher_shortage_click_key = NULL, # For Teacher Shortage graph
#   latest_principal_click_key = NULL # New: For School Principal graph
# )
# Make sure to include all rv initializations from previous steps if you are creating it from scratch.

# Observer for School_Principal_Regional_Graph clicks
# This should be placed in your server.R file, outside of any output$... block,
# but within the main server function.
observeEvent(event_data("plotly_click", source = "schoolPrincipalRegionPlot"), {
  click_data <- event_data("plotly_click", source = "schoolPrincipalRegionPlot")
  if (!is.null(click_data)) {
    # Store the combined key (e.g., "Region I School Principal")
    rv$latest_principal_click_key <- click_data$key
  } else {
    # If a click somehow clears, reset the key
    rv$latest_principal_click_key <- NULL
  }
})

output$School_Principal_Division_Graph <- renderPlotly({
  # Use the reactive filtered data
  current_filtered_data <- filtered_school_data_division()
  
  # --- Empty Data Handling ---
  if (nrow(current_filtered_data) == 0) {
    return(ggplotly(ggplot() +
                      annotate("text", x = 0.5, y = 0.5, label = "No data for selected regions/divisions") +
                      theme_void()))
  }
  
  # Prepare the data for plotting
  plot_data <- current_filtered_data %>%
    select(Division, Designation) %>%
    group_by(Division, Designation) %>%
    summarize(Count = n(), .groups = 'drop') # Ungroup after summarize
  
  plot_data_totals <- plot_data %>%
    group_by(Designation) %>%
    summarise(TotalCount = sum(Count))
  
  # Calculate total counts per region for reordering the x-axis
  region_totals <- plot_data %>%
    group_by(Division) %>%
    summarise(TotalCount = sum(Count), .groups = 'drop')
  
  plot_data <- plot_data %>%
    left_join(region_totals, by = "Division") %>%
    mutate(Division_reordered = reorder(Division, -TotalCount))
  
  
  # Define the levels for Designation for consistent ordering
  designation_levels <- c("School Principal", "Teacher-in-Charge", "Officer-in-Charge")
  
  # Create the ggplot
  p <- ggplot(plot_data,
              aes(x = factor(Designation, levels = designation_levels), # Reorder regions based on overall count (across all designations)
                  y = Count,
                  fill = Division_reordered, # Ensure fill order is consistent
                  # IMPORTANT: Add 'key' aesthetic here to store combined info for click events
                  key = paste(Division, Designation),
                  text = paste("Region: ", Division,
                               "<br>Designation: ", Designation,
                               "<br>Count: ", scales::comma(Count)))) + # Custom tooltip text
    geom_bar(stat = "identity", position = "stack", color = "black", size = 0.25) + 
    geom_text(data = plot_data_totals,
              aes(x = Designation, y = TotalCount * 1.05, label = scales::comma(TotalCount)), # Modified line
              inherit.aes = FALSE,
              size = 3.5,
              color = "black") +
    labs(x = "Designation",
         y = "Count of Individuals",
         fill = "Division") +
    scale_y_continuous(labels = scales::comma) + # Format y-axis labels as comma-separated numbers
    theme_minimal() +
    theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
          legend.position = "bottom", # Position legend at the bottom
          plot.title = element_text(hjust = 0.5)) # Center the plot title
  
  # Convert ggplot to plotly, ensuring custom text is used for hover
  ggplotly(p, tooltip = "text", source = "schoolPrincipalRegionPlot") %>%
    layout(hoverlabel = list(bgcolor = "white"),
           hovermode = "closest", # Changed to "closest" for individual bar tooltips
           margin = list(b = 100)) # Increase bottom margin for x-axis labels
})

output$School_Principal_District_Graph <- renderPlotly({
  # Use the reactive filtered data
  current_filtered_data <- filtered_school_data_division()
  
  # --- Empty Data Handling ---
  if (nrow(current_filtered_data) == 0) {
    return(ggplotly(ggplot() +
                      annotate("text", x = 0.5, y = 0.5, label = "No data for selected regions/Legislative.Districts") +
                      theme_void()))
  }
  
  # Prepare the data for plotting
  plot_data <- current_filtered_data %>%
    select(Division, Legislative.District, Designation) %>%
    group_by(Division, Legislative.District, Designation) %>%
    summarize(Count = n(), .groups = 'drop') # Ungroup after summarize
  
  plot_data_totals <- plot_data %>%
    group_by(Designation) %>%
    summarise(TotalCount = sum(Count))
  
  # Calculate total counts per region for reordering the x-axis
  region_totals <- plot_data %>%
    group_by(Legislative.District) %>%
    summarise(TotalCount = sum(Count), .groups = 'drop')
  
  plot_data <- plot_data %>%
    left_join(region_totals, by = "Legislative.District") %>%
    mutate(Legislative.District_reordered = reorder(Legislative.District, -TotalCount))
  
  plot_data <- plot_data %>% mutate(Division_LegDist = paste0(Division, "- ", Legislative.District))
  
  
  # Define the levels for Designation for consistent ordering
  designation_levels <- c("School Principal", "Teacher-in-Charge", "Officer-in-Charge")
  
  # Create the ggplot
  p <- ggplot(plot_data,
              aes(x = factor(Designation, levels = designation_levels), # Reorder regions based on overall count (across all designations)
                  y = Count,
                  fill = Division_LegDist, # Ensure fill order is consistent
                  # IMPORTANT: Add 'key' aesthetic here to store combined info for click events
                  key = paste(Division_LegDist, Designation),
                  text = paste("Region: ", Legislative.District,
                               "<br>Designation: ", Designation,
                               "<br>Count: ", scales::comma(Count)))) + # Custom tooltip text
    geom_bar(stat = "identity", position = "stack", color = "black", size = 0.25) +
    geom_text(data = plot_data_totals,
              aes(x = Designation, y = TotalCount * 1.05, label = scales::comma(TotalCount)), # Modified line
              inherit.aes = FALSE,
              size = 3.5,
              color = "black") +
    labs(x = "Region",
         y = "Count of Individuals",
         fill = "Designation",
         title = "Regional School Principal Designation by Type") +
    scale_y_continuous(labels = scales::comma) + # Format y-axis labels as comma-separated numbers
    theme_minimal() +
    theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
          legend.position = "bottom", # Position legend at the bottom
          plot.title = element_text(hjust = 0.5)) # Center the plot title
  
  # Convert ggplot to plotly, ensuring custom text is used for hover
  ggplotly(p, tooltip = "text", source = "schoolPrincipalRegionPlot") %>%
    layout(hoverlabel = list(bgcolor = "white"),
           hovermode = "closest", # Changed to "closest" for individual bar tooltips
           margin = list(b = 100)) # Increase bottom margin for x-axis labels
})


# Reactive expression for filtering the School Principal table data
# based on dashboard filters and plot clicks
# This assumes 'filtered_school_principal_data()' provides the base data.
filtered_principal_table_data <- reactive({
  # Ensure base data is available.
  req(filtered_school_data_division())
  
  # Start with the data filtered by dashboard inputs
  data <- filtered_school_data_division()
  
  # Now, filter based on the latest plot click for School Principal graph
  clicked_region <- NULL
  clicked_designation <- NULL
  
  if (!is.null(rv$latest_principal_click_key)) {
    clicked_key <- as.character(rv$latest_principal_click_key)
    
    # Define the possible designation levels for parsing (order matters for accurate matching)
    designation_levels_for_parsing <- c("School Principal", "Teacher-in-Charge", "Officer-in-Charge")
    # Sort by length descending to match longer names first
    designation_levels_ordered_by_length <- designation_levels_for_parsing[order(nchar(designation_levels_for_parsing), decreasing = TRUE)]
    
    found_designation <- FALSE
    for (desig_val in designation_levels_ordered_by_length) {
      # Check if the designation type is at the end of the key string
      if (grepl(paste0(" ", desig_val, "$"), clicked_key)) { # Prepend space to ensure full word match
        clicked_designation <- desig_val
        # The region is everything before this designation type
        clicked_region <- sub(paste0(" ", desig_val, "$"), "", clicked_key)
        found_designation <- TRUE
        break
      }
    }
    
    if (!found_designation) {
      # Fallback if parsing fails, return data filtered only by dashboard inputs
      clicked_region <- NULL
      clicked_designation <- NULL
      warning("Could not parse clicked Principal key for Region and Designation: ", clicked_key)
    }
  }
  
  if (!is.null(clicked_region) && !is.null(clicked_designation)) {
    # Apply filter based on click data if both are valid
    data <- data %>%
      filter(Region == clicked_region,
             Designation == clicked_designation) %>%
      select(SchoolID, Region, Division, Designation) # Adjust columns as needed for your table
  } else {
    # If no specific bar is clicked, or parsing failed,
    # show all schools from the dashboard filters with any of these designations
    data <- data %>%
      filter(Designation %in% designation_levels) %>% # Ensure only valid designations are included
      select(SchoolID, Region, Division, Designation) # Adjust columns as needed for your table
  }
  
  return(data)
})

# Your DT::renderDT for School_Principal_All_List
output$School_Principal_All_List <- DT::renderDT({
  table_to_display <- filtered_school_data_division() %>% select(Region, Division, Legislative.District, SchoolID, School.Name, Designation) %>% rename("School Head Designation" = Designation)
  
  if (nrow(table_to_display) == 0) {
    return(DT::datatable(data.frame(Message = "No principal data available for this selection."), options = list(dom = 't')))
  }
  
  DT::datatable(table_to_display %>% slice(1:100),
                options = list(pageLength = 10, scrollX = TRUE, filter = "top"),
                rownames = FALSE)
})

# In your server.R file, ensure 'rv' is initialized with the new reactive value:
# rv <- reactiveValues(
#   latest_click_key = NULL,           # For school_count graph
#   latest_sosss_click_key = NULL,     # For SOSSS graph
#   latest_classroom_click_key = NULL, # For Classroom Shortage graph
#   latest_teacher_shortage_click_key = NULL, # For Teacher Shortage graph
#   latest_principal_click_key = NULL, # For School Principal graph
#   latest_aoii_click_key = NULL       # New: For AOII Deployment graph
# )
# Make sure to include all rv initializations from previous steps if you are creating it from scratch.

# Observer for AOII_Regional_Graph clicks
# This should be placed in your server.R file, outside of any output$... block,
# but within the main server function.
observeEvent(event_data("plotly_click", source = "aoiiRegionPlot"), {
  click_data <- event_data("plotly_click", source = "aoiiRegionPlot")
  if (!is.null(click_data)) {
    # Store the combined key (e.g., "Region I None Deployed")
    rv$latest_aoii_click_key <- click_data$key
  } else {
    # If a click somehow clears, reset the key
    rv$latest_aoii_click_key <- NULL
  }
})

output$AOII_Division_Graph <- renderPlotly({
  # Use the reactive filtered data
  current_filtered_data <- filtered_school_data_division()
  
  # --- Empty Data Handling ---
  if (nrow(current_filtered_data) == 0) {
    return(ggplotly(ggplot() +
                      annotate("text", x = 0.5, y = 0.5, label = "No data for selected Divisions/divisions") +
                      theme_void()))
  }
  
  # Prepare the data for plotting
  plot_data <- current_filtered_data %>%
    group_by(Division, Clustering.Status) %>%
    summarize(Total_Count = n(), .groups = 'drop')
  
  plot_data_totals <- plot_data %>%
    group_by(Clustering.Status) %>%
    summarise(TotalCount = sum(Total_Count))
  
  # Calculate total counts per Division for the overall labels on top of stacked bars
  total_labels_data <- plot_data %>%
    group_by(Division) %>%
    summarise(Grand_Total = sum(Total_Count), .groups = 'drop')
  
  # Define the levels for Clustering.Status for consistent stacking order in the legend and on the bars
  clustering_levels <- c("None Deployed", "Clustered", "Dedicated")
  
  # Create the ggplot
  p <- ggplot(plot_data,
              aes(x = factor(Clustering.Status, levels = clustering_levels), # Reorder Divisions based on overall total count for the Division
                  y = Total_Count,
                  fill = Division, # Fill by Clustering.Status for coloring and consistent order
                  # IMPORTANT: Add 'key' aesthetic here to store combined info for click events
                  key = paste(Division, Clustering.Status),
                  text = paste("Division: ", Division,
                               "<br>Clustering Status: ", Clustering.Status,
                               "<br>Count: ", scales::comma(Total_Count)))) + # Custom tooltip text
    geom_bar(stat = "identity", position = "stack", color = "black", size = 0.25) + 
    geom_text(data = plot_data_totals,
              aes(x = Clustering.Status, y = TotalCount * 1.05, label = scales::comma(TotalCount)), # Modified line
              inherit.aes = FALSE,
              size = 3.5,
              color = "black") +
    labs(x = "Clustering Status",
         y = "Total Count of Schools",
         title = "Division-Level AO II Deployment by Clustering Status") +
    scale_y_continuous(labels = scales::comma) + # Format y-axis labels as comma-separated numbers
    theme_minimal() +
    theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
          legend.position = "bottom", # Position legend at the bottom
          plot.title = element_text(hjust = 0.5)) # Center the plot title
  
  # Convert ggplot to plotly, ensuring custom text is used for hover
  ggplotly(p, tooltip = "text", source = "aoiiDivisionPlot") %>%
    layout(hoverlabel = list(bgcolor = "white"),
           hovermode = "closest", # Changed to "closest" for individual segment tooltips
           margin = list(b = 100)) # Increase bottom margin for x-axis labels
})

output$AOII_District_Graph <- renderPlotly({
  # Use the reactive filtered data
  current_filtered_data <- filtered_school_data_division()
  
  # --- Empty Data Handling ---
  if (nrow(current_filtered_data) == 0) {
    return(ggplotly(ggplot() +
                      annotate("text", x = 0.5, y = 0.5, label = "No data for selected Legislative.Districts/divisions") +
                      theme_void()))
  }
  
  # Prepare the data for plotting
  plot_data <- current_filtered_data %>%
    group_by(Division, Legislative.District, Clustering.Status) %>%
    summarize(Total_Count = n(), .groups = 'drop')
  
  plot_data_totals <- plot_data %>%
    group_by(Clustering.Status) %>%
    summarise(TotalCount = sum(Total_Count))
  
  # Calculate total counts per Legislative.District for the overall labels on top of stacked bars
  total_labels_data <- plot_data %>%
    group_by(Division, Legislative.District) %>%
    summarise(Grand_Total = sum(Total_Count), .groups = 'drop')

  # Define the levels for Clustering.Status for consistent stacking order in the legend and on the bars
  clustering_levels <- c("None Deployed", "Clustered", "Dedicated")
  
  plot_data <- plot_data %>% mutate(Division_LegDist = paste0(Division, "- ", Legislative.District))
  
  
  # Create the ggplot
  p <- ggplot(plot_data,
              aes(x = factor(Clustering.Status, levels = clustering_levels), # Reorder Legislative.Districts based on overall total count for the Legislative.District
                  y = Total_Count,
                  fill = reorder(Division_LegDist, -Total_Count), # Fill by Clustering.Status for coloring and consistent order
                  # IMPORTANT: Add 'key' aesthetic here to store combined info for click events
                  key = paste(Division_LegDist, Clustering.Status),
                  text = paste("Legislative.District: ", Legislative.District,
                               "<br>Clustering Status: ", Clustering.Status,
                               "<br>Count: ", scales::comma(Total_Count)))) + # Custom tooltip text
    geom_bar(stat = "identity", position = "stack", color = "black", size = 0.25) + 
    geom_text(data = plot_data_totals,
              aes(x = Clustering.Status, y = TotalCount * 1.05, label = scales::comma(TotalCount)), # Modified line
              inherit.aes = FALSE,
              size = 3.5,
              color = "black") +
    labs(x = "Legislative.District",
         y = "Total Count of Schools",
         fill = "Legislative District") +
    scale_y_continuous(labels = scales::comma) + # Format y-axis labels as comma-separated numbers
    theme_minimal() +
    theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
          legend.position = "bottom", # Position legend at the bottom
          plot.title = element_text(hjust = 0.5)) # Center the plot title
  
  # Convert ggplot to plotly, ensuring custom text is used for hover
  ggplotly(p, tooltip = "text", source = "aoiiLegislative.DistrictPlot") %>%
    layout(hoverlabel = list(bgcolor = "white"),
           hovermode = "closest", # Changed to "closest" for individual segment tooltips
           margin = list(b = 100)) # Increase bottom margin for x-axis labels
})


# Reactive expression for filtering the AOII table data
# based on dashboard filters and plot clicks
# This assumes 'filtered_school_data_region()' provides the base data.
filtered_aoii_table_data <- reactive({
  # Ensure base data is available.
  req(filtered_school_data_region())
  
  # Start with the data filtered by dashboard inputs
  data <- filtered_school_data_region()
  
  # Now, filter based on the latest plot click for AOII graph
  clicked_region <- NULL
  clicked_clustering_status <- NULL
  
  if (!is.null(rv$latest_aoii_click_key)) {
    clicked_key <- as.character(rv$latest_aoii_click_key)
    
    # Define the possible clustering status levels for parsing (order matters for accurate matching)
    clustering_levels_for_parsing <- c("None Deployed", "Clustered", "Dedicated")
    # Sort by length descending to match longer names first
    clustering_levels_ordered_by_length <- clustering_levels_for_parsing[order(nchar(clustering_levels_for_parsing), decreasing = TRUE)]
    
    found_status <- FALSE
    for (status_val in clustering_levels_ordered_by_length) {
      # Check if the status type is at the end of the key string
      if (grepl(paste0(" ", status_val, "$"), clicked_key)) { # Prepend space to ensure full word match
        clicked_clustering_status <- status_val
        # The region is everything before this status type
        clicked_region <- sub(paste0(" ", status_val, "$"), "", clicked_key)
        found_status <- TRUE
        break
      }
    }
    
    if (!found_status) {
      # Fallback if parsing fails, return data filtered only by dashboard inputs
      clicked_region <- NULL
      clicked_clustering_status <- NULL
      warning("Could not parse clicked AOII key for Region and Clustering Status: ", clicked_key)
    }
  }
  
  if (!is.null(clicked_region) && !is.null(clicked_clustering_status)) {
    # Apply filter based on click data if both are valid
    data <- data %>%
      filter(Region == clicked_region,
             Clustering.Status == clicked_clustering_status) %>%
      # Select relevant columns for display in the table
      select(SchoolID, Region, Division, Clustering.Status) # Adjust columns as needed
  } else {
    # If no specific bar segment is clicked, or parsing failed,
    # show all schools from the dashboard filters that have any of these clustering statuses
    data <- data %>%
      filter(Clustering.Status %in% clustering_levels) %>% # Ensure only valid statuses are included
      select(SchoolID, Region, Division, Clustering.Status) # Adjust columns as needed
  }
  
  return(data)
})

# Your DT::renderDT for AOII_Data_Table
output$AOII_Data_Table <- DT::renderDT({
  table_to_display <- filtered_school_data_division() %>% select(Region, Division, Legislative.District, SchoolID, School.Name, Clustering.Status, Outlier.Status,PDOI_Deployment)
  
  if (nrow(table_to_display) == 0) {
    return(DT::datatable(data.frame(Message = "No AO II deployment data available for this selection."), options = list(dom = 't')))
  }
  
  DT::datatable(table_to_display %>% slice(1:100),
                options = list(pageLength = 10, scrollX = TRUE, filter = "top"),
                rownames = FALSE)
})

# In your server.R file, ensure 'rv' is initialized with the new reactive value:
# rv <- reactiveValues(
#   latest_click_key = NULL,           # For school_count graph
#   latest_sosss_click_key = NULL,     # For SOSSS graph
#   latest_classroom_click_key = NULL, # For Classroom Shortage graph
#   latest_teacher_shortage_click_key = NULL, # For Teacher Shortage graph
#   latest_principal_click_key = NULL, # For School Principal graph
#   latest_aoii_click_key = NULL,      # For AOII Deployment graph
#   latest_pdoi_click_key = NULL       # New: For PDOI Deployment graph
# )
# Make sure to include all rv initializations from previous steps if you are creating it from scratch.

# Observer for PDOI_Regional_Graph clicks
# This should be placed in your server.R file, outside of any output$... block,
# but within the main server function.
observeEvent(event_data("plotly_click", source = "pdoiRegionPlot"), {
  click_data <- event_data("plotly_click", source = "pdoiRegionPlot")
  if (!is.null(click_data)) {
    # Store the combined key (e.g., "Region I With PDO I")
    rv$latest_pdoi_click_key <- click_data$key
  } else {
    # If a click somehow clears, reset the key
    rv$latest_pdoi_click_key <- NULL
  }
})


output$PDOI_Division_Graph <- renderPlotly({
  # Use the reactive filtered data. User provided `filtered_school_data_Division()` here.
  current_filtered_data <- filtered_school_data_division()
  
  # --- Empty Data Handling ---
  if (nrow(current_filtered_data) == 0) {
    return(ggplotly(ggplot() +
                      annotate("text", x = 0.5, y = 0.5, label = "No data for selected Divisions/divisions") +
                      theme_void()))
  }
  
  # Prepare the data for plotting
  plot_data <- current_filtered_data %>%
    group_by(Division, PDOI_Deployment) %>%
    summarize(Total_Count = n(), .groups = 'drop')
  
  plot_data_totals <- plot_data %>%
    group_by(PDOI_Deployment) %>%
    summarise(TotalCount = sum(Total_Count))
  
  # Calculate total counts per Division for the overall labels on top of stacked bars
  total_labels_data <- plot_data %>%
    group_by(Division) %>%
    summarise(Grand_Total = sum(Total_Count), .groups = 'drop')
  
  # Define the levels for PDOI_Deployment for consistent stacking order in the legend and on the bars
  pdoi_levels <- c("Without PDO I", "With PDO I")
  
  
  # Create the ggplot
  p <- ggplot(plot_data,
              aes(x = factor(PDOI_Deployment, levels = pdoi_levels), # Reorder Divisions based on overall total count for the Division
                  y = Total_Count,
                  fill = Division, # Fill by PDOI_Deployment for coloring and consistent order
                  # IMPORTANT: Add 'key' aesthetic here to store combined info for click events
                  key = paste(Division, PDOI_Deployment),
                  text = paste("Division: ", Division,
                               "<br>PDO I Deployment: ", PDOI_Deployment,
                               "<br>Total Count: ", scales::comma(Total_Count)))) + # Custom tooltip text
    geom_bar(stat = "identity", position = "stack", color = "black", size = 0.25) + 
    geom_text(data = plot_data_totals,
              aes(x = PDOI_Deployment, y = TotalCount * 1.05, label = scales::comma(TotalCount)), # Modified line
              inherit.aes = FALSE,
              size = 3.5,
              color = "black") +
    labs(x = "Clustering Status",
         y = "Total Count of Schools",
         title = "Division-Level PDO I Deployment by Status") +
    scale_y_continuous(labels = scales::comma) + # Format y-axis labels as comma-separated numbers
    theme_minimal() +
    theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
          legend.position = "bottom", # Position legend at the bottom
          plot.title = element_text(hjust = 0.5)) # Center the plot title
  
  # Convert ggplot to plotly, ensuring custom text is used for hover
  ggplotly(p, tooltip = "text", source = "pdoiDivisionPlot") %>%
    layout(hoverlabel = list(bgcolor = "white"),
           hovermode = "closest", # Changed to "closest" for individual segment tooltips
           margin = list(b = 100)) # Increase bottom margin for x-axis labels
})

output$PDOI_District_Graph <- renderPlotly({
  # Use the reactive filtered data. User provided `filtered_school_data_Legislative.District()` here.
  current_filtered_data <- filtered_school_data_division()
  
  # --- Empty Data Handling ---
  if (nrow(current_filtered_data) == 0) {
    return(ggplotly(ggplot() +
                      annotate("text", x = 0.5, y = 0.5, label = "No data for selected Legislative.Districts/divisions") +
                      theme_void()))
  }
  
  # Prepare the data for plotting
  plot_data <- current_filtered_data %>%
    group_by(Division, Legislative.District, PDOI_Deployment) %>%
    summarize(Total_Count = n(), .groups = 'drop')
  
  plot_data_totals <- plot_data %>%
    group_by(PDOI_Deployment) %>%
    summarise(TotalCount = sum(Total_Count))
  
  # Calculate total counts per Legislative.District for the overall labels on top of stacked bars
  total_labels_data <- plot_data %>%
    group_by(Legislative.District) %>%
    summarise(Grand_Total = sum(Total_Count), .groups = 'drop')
  
  # Define the levels for PDOI_Deployment for consistent stacking order in the legend and on the bars
  pdoi_levels <- c("Without PDO I", "With PDO I")
  
  plot_data <- plot_data %>% mutate(Division_LegDist = paste0(Division, "- ", Legislative.District))
  
  # Create the ggplot
  p <- ggplot(plot_data,
              aes(x = factor(PDOI_Deployment, levels = pdoi_levels), # Reorder Legislative.Districts based on overall total count for the Legislative.District
                  y = Total_Count,
                  fill = reorder(Division_LegDist, -Total_Count), # Fill by PDOI_Deployment for coloring and consistent order
                  # IMPORTANT: Add 'key' aesthetic here to store combined info for click events
                  key = paste(Legislative.District, PDOI_Deployment),
                  text = paste("Legislative.District: ", Legislative.District,
                               "<br>PDO I Deployment: ", PDOI_Deployment,
                               "<br>Total Count: ", scales::comma(Total_Count)))) + # Custom tooltip text
    geom_bar(stat = "identity", position = "stack", color = "black", size = 0.25) + 
    geom_text(data = plot_data_totals,
              aes(x = PDOI_Deployment, y = TotalCount * 1.05, label = scales::comma(TotalCount)), # Modified line
              inherit.aes = FALSE,
              size = 3.5,
              color = "black") +
    labs(x = "Deployment Status",
         y = "Total Count of Schools",
         fill = "Legislative District") +
    scale_y_continuous(labels = scales::comma) + # Format y-axis labels as comma-separated numbers
    theme_minimal() +
    theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
          legend.position = "bottom", # Position legend at the bottom
          plot.title = element_text(hjust = 0.5)) # Center the plot title
  
  # Convert ggplot to plotly, ensuring custom text is used for hover
  ggplotly(p, tooltip = "text", source = "pdoiLegislative.DistrictPlot") %>%
    layout(hoverlabel = list(bgcolor = "white"),
           hovermode = "closest", # Changed to "closest" for individual segment tooltips
           margin = list(b = 100)) # Increase bottom margin for x-axis labels
})


# Reactive expression for filtering the PDOI table data
# based on dashboard filters and plot clicks
# This assumes 'filtered_school_data_region()' provides the base data as per the graph's usage.
filtered_pdoi_table_data <- reactive({
  # Ensure base data is available.
  req(filtered_school_data_region()) # Using filtered_school_data_region() as per the graph's source
  
  # Start with the data filtered by dashboard inputs
  data <- filtered_school_data_region() # Using filtered_school_data_region() as per the graph's source
  
  # Now, filter based on the latest plot click for PDOI graph
  clicked_region <- NULL
  clicked_pdoi_deployment <- NULL
  
  if (!is.null(rv$latest_pdoi_click_key)) {
    clicked_key <- as.character(rv$latest_pdoi_click_key)
    
    # Define the possible PDOI Deployment levels for parsing (order matters if some are substrings of others)
    pdoi_levels_for_parsing <- c("Without PDO I", "With PDO I")
    # Sort by length descending to match longer names first
    pdoi_levels_ordered_by_length <- pdoi_levels_for_parsing[order(nchar(pdoi_levels_for_parsing), decreasing = TRUE)]
    
    found_deployment <- FALSE
    for (deployment_val in pdoi_levels_ordered_by_length) {
      # Check if the deployment type is at the end of the key string
      if (grepl(paste0(" ", deployment_val, "$"), clicked_key)) { # Prepend space to ensure full word match
        clicked_pdoi_deployment <- deployment_val
        # The region is everything before this deployment type
        clicked_region <- sub(paste0(" ", deployment_val, "$"), "", clicked_key)
        found_deployment <- TRUE
        break
      }
    }
    
    if (!found_deployment) {
      # Fallback if parsing fails, return data filtered only by dashboard inputs
      clicked_region <- NULL
      clicked_pdoi_deployment <- NULL
      warning("Could not parse clicked PDOI key for Region and PDOI Deployment: ", clicked_key)
    }
  }
  
  if (!is.null(clicked_region) && !is.null(clicked_pdoi_deployment)) {
    # Apply filter based on click data if both are valid
    data <- data %>%
      filter(Region == clicked_region,
             PDOI_Deployment == clicked_pdoi_deployment) %>%
      # Select relevant columns for display in the table (adjust as needed)
      select(SchoolID, Region, Division, PDOI_Deployment)
  } else {
    # If no specific bar segment is clicked, or parsing failed,
    # show all schools from the dashboard filters that have any of these PDOI statuses
    data <- data %>%
      filter(PDOI_Deployment %in% pdoi_levels) %>% # Ensure only valid statuses are included
      select(SchoolID, Region, Division, PDOI_Deployment) # Adjust columns as needed
  }
  
  return(data)
})

# Your DT::renderDT for PDOI_Data_Table
output$PDOI_Data_Table <- DT::renderDT({
  table_to_display <- filtered_school_data_division() %>% select(Region, Division, Legislative.District, SchoolID, School.Name, Clustering.Status, Outlier.Status,PDOI_Deployment)
  
  if (nrow(table_to_display) == 0) {
    return(DT::datatable(data.frame(Message = "No PDO I deployment data available for this selection."), options = list(dom = 't')))
  }
  
  DT::datatable(table_to_display %>% slice(1:100),
                options = list(pageLength = 10, scrollX = TRUE, filter = "top"),
                rownames = FALSE)
})

# In your server.R file, ensure 'rv' is initialized with the new reactive value:
# rv <- reactiveValues(
#   latest_click_key = NULL,           # For school_count graph
#   latest_sosss_click_key = NULL,     # For SOSSS graph
#   latest_classroom_click_key = NULL, # For Classroom Shortage graph
#   latest_teacher_shortage_click_key = NULL, # For Teacher Shortage graph
#   latest_principal_click_key = NULL, # For School Principal graph
#   latest_aoii_click_key = NULL,      # For AOII Deployment graph
#   latest_pdoi_click_key = NULL,      # For PDOI Deployment graph
#   latest_sufficiency_click_key = NULL # New: For Sufficiency graph
# )
# Make sure to include all rv initializations from previous steps if you are creating it from scratch.

# In your server.R file, ensure 'rv' is initialized with the new reactive value:
# rv <- reactiveValues(
#   latest_click_key = NULL,           # For school_count graph
#   latest_sosss_click_key = NULL,     # For SOSSS graph
#   latest_classroom_click_key = NULL, # For Classroom Shortage graph
#   latest_teacher_shortage_click_key = NULL, # For Teacher Shortage graph
#   latest_principal_click_key = NULL, # For School Principal graph
#   latest_aoii_click_key = NULL,      # For AOII Deployment graph
#   latest_pdoi_click_key = NULL,      # For PDOI Deployment graph
#   latest_sufficiency_click_key = NULL # New: For Sufficiency graph
# )
# Make sure to include all rv initializations from previous steps if you are creating it from scratch.

# Observer for Sufficiency_Regional_Graph clicks
# This should be placed in your server.R file, outside of any output$... block,
# but within the main server function.
observeEvent(event_data("plotly_click", source = "sufficiencyRegionPlot"), {
  click_data <- event_data("plotly_click", source = "sufficiencyRegionPlot")
  if (!is.null(click_data)) {
    # Store the combined key (e.g., "Region I Critically Under-Resourced")
    rv$latest_sufficiency_click_key <- click_data$key
  } else {
    # If a click somehow clears, reset the key
    rv$latest_sufficiency_click_key <- NULL
  }
})

# Reactive for the raw data filtered by dashboard inputs and with sufficiency categories calculated
# This will be the base for both the graph's aggregated data and the table's detailed data.
filtered_sufficiency_raw_data_region <- reactive({
  # Ensure uni data is available.
  req(uni)
  
  # Start with the full dataset
  data <- uni
  
  # Filter by selected region(s) if any are chosen
  if (!is.null(input$dashboard_region_filter) && length(input$dashboard_region_filter) > 0) {
    data <- data %>%
      filter(Region %in% input$dashboard_region_filter)
  }
  
  # Data transformation to calculate sufficiency categories for each school
  data %>%
    select(SchoolID, Region, Division, Teacher, Classroom, School.Head, AO) %>%
    mutate(
      Teacher = as.numeric(Teacher),
      Classroom = as.numeric(Classroom),
      School.Head = as.numeric(School.Head),
      AO = as.numeric(AO) # Ensure AO is also numeric
    ) %>%
    mutate(
      Teacher.Sufficiency = case_when(
        (Teacher >= 0 & Teacher <= 0.25) ~ "Critically Under-Resourced",
        (Teacher > 0.25 & Teacher <= 0.5) ~ "Under-Resourced",
        (Teacher > 0.5 & Teacher <= 0.75) ~ "Resource-Deficient",
        (Teacher > 0.75 & Teacher <= 0.9) ~ "Adequately Resourced",
        (Teacher > 0.9 & Teacher <= 1) ~ "Generously Resourced",
        Teacher > 1 ~ "For Validation",
        TRUE ~ NA_character_
      ),
      Classroom.Sufficiency = case_when(
        (Classroom >= 0 & Classroom <= 0.25) ~ "Critically Under-Resourced",
        (Classroom > 0.25 & Classroom <= 0.5) ~ "Under-Resourced",
        (Classroom > 0.5 & Classroom <= 0.75) ~ "Resource-Deficient",
        (Classroom > 0.75 & Classroom <= 0.9) ~ "Adequately Resourced",
        (Classroom > 0.9 & Classroom <= 1) ~ "Generously Resourced",
        Classroom > 1 ~ "For Validation",
        TRUE ~ NA_character_
      ),
      SH.Sufficiency = case_when(
        (School.Head >= 0 & School.Head <= 0.25) ~ "Critically Under-Resourced",
        (School.Head > 0.25 & School.Head <= 0.5) ~ "Under-Resourced",
        (School.Head > 0.5 & School.Head <= 0.75) ~ "Resource-Deficient",
        (School.Head > 0.75 & School.Head <= 0.9) ~ "Adequately Resourced",
        (School.Head > 0.9 & School.Head <= 1) ~ "Generously Resourced",
        School.Head > 1 ~ "For Validation",
        TRUE ~ NA_character_
      ),
      AO.Sufficiency = case_when(
        (AO >= 0 & AO <= 0.25) ~ "Critically Under-Resourced",
        (AO > 0.25 & AO <= 0.5) ~ "Under-Resourced",
        (AO > 0.5 & AO <= 0.75) ~ "Resource-Deficient",
        (AO > 0.75 & AO <= 0.9) ~ "Adequately Resourced",
        (AO > 0.9 & AO <= 1) ~ "Generously Resourced",
        AO > 1 ~ "For Validation",
        TRUE ~ NA_character_
      )
    )
})

filtered_sufficiency_data_division <- reactive({
  req(filtered_sufficiency_raw_data_region())
  
  filtered_sufficiency_raw_data_region() %>%
    pivot_longer(
      cols = c(Teacher.Sufficiency, Classroom.Sufficiency, SH.Sufficiency, AO.Sufficiency),
      names_to = "Criteria",
      values_to = "Sufficiency"
    ) %>%
    filter(!is.na(Sufficiency)) %>% # Filter NA sufficiency before grouping
    group_by(Region, Division, Criteria, Sufficiency) %>%
    summarise(SufficiencyTotal = n(), .groups = 'drop_last') %>%
    # Calculate the total for each Division and Criteria for percentage calculation
    mutate(RegionCriteriaTotal = sum(SufficiencyTotal)) %>%
    mutate(Percentage = (SufficiencyTotal / RegionCriteriaTotal)) %>%
    ungroup() %>%
    mutate(Sufficiency = factor(Sufficiency, levels = c(
      "Critically Under-Resourced",
      "Under-Resourced",
      "Resource-Deficient",
      "Adequately Resourced",
      "Generously Resourced",
      "For Validation"))) # Ensure consistent order for plotting
})


output$Sufficiency_Division_Graph <- renderPlotly({
  # Use the reactive filtered data
  current_filtered_data_for_plot <- filtered_sufficiency_data_division()
  
  # Filter by the selected category from input$SuffOpt
  plot_data <- current_filtered_data_for_plot %>%
    filter(Criteria == input$SuffOpt)
  
  # --- Empty Data Handling ---
  if (nrow(plot_data) == 0) {
    return(ggplotly(ggplot() +
                      annotate("text", x = 0.5, y = 0.5, label = paste("No data for selected regions/divisions and category:", input$SuffOpt)) +
                      theme_void()))
  }
  
  # Determine the title based on the selected input$SuffOpt
  plot_title <- switch(input$SuffOpt,
                       "Teacher.Sufficiency" = "Regional Teacher Sufficiency by Category",
                       "Classroom.Sufficiency" = "Regional Classroom Sufficiency by Category",
                       "SH.Sufficiency" = "Regional School Principal Sufficiency by Category",
                       "AO.Sufficiency" = "Regional AO Sufficiency by Category",
                       "Regional Sufficiency Overview")
  
  # Calculate total percentages per region for the overall labels on top of stacked bars
  # This needs to be done *after* filtering by Criteria
  total_labels_data <- plot_data %>%
    group_by(Division) %>%
    summarise(Grand_Total_Percentage = sum(Percentage), .groups = 'drop') # Sum percentages for labeling
  
  # Define specific colors for Sufficiency (adjust as per your actual categories and desired colors)
  sufficiency_colors <- c(
    "Critically Under-Resourced" = "#E41A1C", # Red
    "Under-Resourced" = "#FF7F00",        # Orange
    "Resource-Deficient" = "#FFFF33",     # Yellow
    "Adequately Resourced" = "#A6CEE3",   # Light Blue
    "Generously Resourced" = "#33A02C",   # Green
    "For Validation" = "#BEBADA"          # Grey/Purple for validation
  )
  # Define the levels for Sufficiency for consistent stacking order in the legend and on the bars
  sufficiency_levels_ordered <- c(
    "Critically Under-Resourced",
    "Under-Resourced",
    "Resource-Deficient",
    "Adequately Resourced",
    "Generously Resourced",
    "For Validation"
  )
  
  # Create the ggplot
  p <- ggplot(plot_data,
              aes(x = reorder(Division, -Percentage), # Reorder regions based on the percentage of the current 'Criteria' and 'Sufficiency'
                  y = Percentage,
                  fill = factor(Sufficiency, levels = sufficiency_levels_ordered), # Fill by Sufficiency for stacking and consistent order
                  # IMPORTANT: Add 'key' aesthetic here to store combined info for click events
                  key = paste(Division, Sufficiency),
                  text = paste("Region: ", Division,
                               "<br>Sufficiency: ", Sufficiency,
                               "<br>Percentage: ", scales::percent(Percentage, accuracy = 0.1),
                               "<br>Total Schools (Category): ", scales::comma(SufficiencyTotal)))) + # Custom tooltip text
    geom_bar(stat = "identity", position = "stack", color = "black", size = 0.25) + # Changed to position="stack" for stacked bars
    scale_fill_manual(values = sufficiency_colors, name = "Sufficiency Category") + # Apply custom colors
    labs(x = "Region",
         y = "Percentage",
         fill = "Sufficiency Category",
         title = plot_title) +
    scale_y_continuous(labels = scales::percent, limits = c(0, 1), breaks = seq(0, 1, by = 0.2)) + # Format y-axis as percentage
    theme_minimal() +
    theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
          legend.position = "bottom", # Position legend at the bottom
          plot.title = element_text(hjust = 0.5)) # Center the plot title
  
  # Convert ggplot to plotly, ensuring custom text is used for hover
  ggplotly(p, tooltip = "text", source = "sufficiencyRegionPlot") %>%
    layout(hoverlabel = list(bgcolor = "white"),
           hovermode = "closest", # Changed to "closest" for individual segment tooltips
           margin = list(b = 100)) # Increase bottom margin for x-axis labels
})


# Reactive expression for filtering the Sufficiency table data
# based on dashboard filters, input$SuffOpt, and plot clicks
filtered_sufficiency_table_data <- reactive({
  # Ensure raw data with calculated sufficiency is available.
  req(filtered_sufficiency_raw_data_region())
  
  # Start with the data already filtered by dashboard inputs and with sufficiency calculated
  data <- filtered_sufficiency_raw_data_region()
  
  # Apply filter by input$SuffOpt immediately
  req(input$SuffOpt) # Ensure input$SuffOpt is not NULL
  data <- data %>%
    filter(get(input$SuffOpt) %in% c(
      "Critically Under-Resourced",
      "Under-Resourced",
      "Resource-Deficient",
      "Adequately Resourced",
      "Generously Resourced",
      "For Validation"
    )) # Filter out NA or unexpected values in the selected sufficiency column
  
  # Select only the relevant sufficiency column to simplify subsequent filtering
  data_filtered_by_suffopt <- data %>%
    select(SchoolID, Region, Division, Teacher, Classroom, School.Head, AO, !!sym(input$SuffOpt)) %>%
    rename(SelectedSufficiency = !!sym(input$SuffOpt)) # Rename for easier generic filtering below
  
  # Now, filter based on the latest plot click for Sufficiency graph
  clicked_region <- NULL
  clicked_sufficiency_status <- NULL
  
  if (!is.null(rv$latest_sufficiency_click_key)) {
    clicked_key <- as.character(rv$latest_sufficiency_click_key)
    
    # Define the possible Sufficiency levels for parsing (order matters: longest first)
    sufficiency_levels_for_parsing <- c(
      "Critically Under-Resourced",
      "Under-Resourced",
      "Resource-Deficient",
      "Adequately Resourced",
      "Generously Resourced",
      "For Validation"
    )
    # Sort by length descending to match longer names first
    sufficiency_levels_ordered_by_length <- sufficiency_levels_for_parsing[order(nchar(sufficiency_levels_for_parsing), decreasing = TRUE)]
    
    found_status <- FALSE
    for (status_val in sufficiency_levels_ordered_by_length) {
      # Check if the status type is at the end of the key string with a preceding space
      if (grepl(paste0(" ", status_val, "$"), clicked_key)) {
        clicked_sufficiency_status <- status_val
        # The region is everything before this status type
        clicked_region <- sub(paste0(" ", status_val, "$"), "", clicked_key)
        found_status <- TRUE
        break
      }
    }
    
    if (!found_status) {
      # Fallback if parsing fails, return data filtered only by dashboard inputs and SuffOpt
      clicked_region <- NULL
      clicked_sufficiency_status <- NULL
      warning("Could not parse clicked Sufficiency key for Region and Sufficiency Status: ", clicked_key)
    }
  }
  
  if (!is.null(clicked_region) && !is.null(clicked_sufficiency_status)) {
    # Apply filter based on click data if both are valid
    final_data <- data_filtered_by_suffopt %>%
      filter(Region == clicked_region,
             SelectedSufficiency == clicked_sufficiency_status)
  } else {
    # If no specific bar segment is clicked, or parsing failed,
    # show all schools from the dashboard filters for the selected SuffOpt
    final_data <- data_filtered_by_suffopt
  }
  
  # Select final columns for the table display. Adjust as needed.
  # Include the original numeric columns if desired, and the specific sufficiency column selected.
  final_data %>%
    select(SchoolID, Region, Division, Teacher, Classroom, School.Head, AO, SelectedSufficiency)
  
})

# Your DT::renderDT for Sufficiency_All_List
output$Sufficiency_All_List <- DT::renderDT({
  table_to_display <- filtered_sufficiency_table_data()
  
  if (nrow(table_to_display) == 0) {
    # Display a more informative message including the selected Sufficiency Option
    return(DT::datatable(data.frame(Message = paste0("No school data available for '",
                                                     gsub("\\.", " ", input$SuffOpt), # Clean up SuffOpt for display
                                                     "' with current filters and graph selection.")),
                         options = list(dom = 't', filter = "top")))
  }
  
  # Dynamically format columns if they exist.
  # This section needs to consider which numeric columns are relevant based on input$SuffOpt.
  # You might want to format 'Teacher', 'Classroom', 'School.Head', 'AO' as percentages or numbers.
  # For simplicity, let's just format the "SelectedSufficiency" column as text.
  # For numeric columns like Teacher, Classroom etc., you'd typically apply formatPercentage or formatCurrency.
  # If the table should show all original numeric values, ensure they are selected above in filtered_sufficiency_table_data.
  
  dt_table <- DT::datatable(table_to_display %>% slice(1:100),
                            options = list(pageLength = 10, scrollX = TRUE, filter = "top"),
                            rownames = FALSE)
  
  # Example: Format Teacher, Classroom, SH, AO as percentages if they are in the table
  # You might need to adjust this based on the specific columns you want to display in the final table.
  numeric_sufficiency_cols <- c("Teacher", "Classroom", "School.Head", "AO")
  for (col in numeric_sufficiency_cols) {
    if (col %in% colnames(table_to_display)) {
      dt_table <- dt_table %>% formatPercentage(col, 2) # Format as percentage with 2 decimal places
    }
  }
  
  dt_table
})

rv <- reactiveValues(
  latest_click_key = NULL,
  latest_sosss_click_key = NULL,
  latest_classroom_click_key = NULL,
  latest_teacher_shortage_click_key = NULL,
  latest_principal_click_key = NULL,
  latest_aoii_click_key = NULL,
  latest_pdoi_click_key = NULL,
  latest_sufficiency_click_key = NULL
)

observeEvent(
  event_data("plotly_click", source = "classroomShortageRegionPlot"),
  {
    click_data <- event_data("plotly_click", source = "classroomShortageRegionPlot")
    rv$latest_classroom_click_key <- if (!is.null(click_data)) click_data$key else NULL
  }
)

output$Classroom_Shortage_Division_Graph <- renderPlotly({
  current_filtered_data <- filtered_LMS_division()
  
  if (nrow(current_filtered_data) == 0) {
    return(
      ggplotly(
        ggplot() +
          annotate("text", x = 0.5, y = 0.5, label = "No data for selected regions/divisions") +
          theme_void()
      )
    )
  }
  
  plot_data <- current_filtered_data |>
    mutate(Estimated_CL_Shortage = as.numeric(Estimated_CL_Shortage)) |>
    group_by(Division) |>
    summarise(Count = sum(Estimated_CL_Shortage, na.rm = TRUE), .groups = "drop")
  
  p <- ggplot(
    plot_data,
    aes(
      x = reorder(Division, -Count),
      y = Count,
      fill = Division,
      key = Division,
      text = paste0("Region: ", Division, "<br>Classroom Shortage: ", scales::comma(Count))
    )
  ) +
    geom_bar(stat = "identity", color = "black") +
    geom_text(data = plot_data,
              aes(x = Division, y = Count * 1.05, label = scales::comma(Count)), # Modified line
              inherit.aes = FALSE,
              size = 3.5,
              color = "black") +
    labs(
      x = "Division",
      y = "Classroom Shortage"
    ) +
    scale_y_continuous(labels = scales::comma) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
      legend.position = "none",
      plot.title = element_text(hjust = 0.5)
    )
  
  ggplotly(p, tooltip = "text", source = "classroomShortage_division") |>
    layout(
      hoverlabel = list(bgcolor = "white"),
      hovermode = "closest",
      margin = list(b = 100)
    )
})

output$Classroom_Shortage_District_Graph <- renderPlotly({
  current_filtered_data <- filtered_LMS_division()
  
  if (nrow(current_filtered_data) == 0) {
    return(
      ggplotly(
        ggplot() +
          annotate("text", x = 0.5, y = 0.5, label = "No data for selected Legislative_Districts/divisions") +
          theme_void()
      )
    )
  }
  
  plot_data <- current_filtered_data |>
    mutate(Estimated_CL_Shortage = as.numeric(Estimated_CL_Shortage)) |>
    group_by(Division, Legislative_District) |>
    summarise(Count = sum(Estimated_CL_Shortage, na.rm = TRUE), .groups = "drop")
  
  plot_data <- plot_data %>% mutate(Division_LegDist = paste0(Division, "- ", Legislative_District))
  
  p <- ggplot(
    plot_data,
    aes(
      x = reorder(Division_LegDist, -Count),
      y = Count,
      fill = Division_LegDist,
      key = Division_LegDist,
      text = paste0("Legislative_District: ", Division_LegDist, "<br>Classroom Shortage: ", scales::comma(Count))
    )
  ) +
    geom_bar(stat = "identity", color = "black") +
    geom_text(data = plot_data,
              aes(x = Division_LegDist, y = Count * 1.05, label = scales::comma(Count)), # Modified line
              inherit.aes = FALSE,
              size = 3.5,
              color = "black") +
    labs(
      x = "Legislative District",
      y = "Classroom Shortage"
    ) +
    scale_y_continuous(labels = scales::comma) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
      legend.position = "none",
      plot.title = element_text(hjust = 0.5)
    )
  
  ggplotly(p, tooltip = "text", source = "classroomShortage_district") |>
    layout(
      hoverlabel = list(bgcolor = "white"),
      hovermode = "closest",
      margin = list(b = 100)
    )
})

filtered_classroom_shortage_table_data <- reactive({
  req(filtered_school_data_region())
  
  data <- filtered_school_data_region() |>
    mutate(Est.CS = as.numeric(Est.CS)) |>
    filter(Est.CS > 0)
  
  if (!is.null(rv$latest_classroom_click_key)) {
    data <- data |> filter(Region == rv$latest_classroom_click_key)
  }
  
  data |>
    select(SchoolID, Region, Division, Est.CS) |>
    rename(`Classroom Shortage` = Est.CS)
})

clicked_info <- reactiveVal(NULL)

observeEvent(plotly::event_data("plotly_click", source = "teacherShortageRegionPlot"), {
  click <- plotly::event_data("plotly_click", source = "teacherShortageRegionPlot")
  
  if (!is.null(click)) {
    region_clicked <- click$x
    
    # Print for debugging
    print(paste("Clicked Region:", region_clicked))
    
    clicked_info(region_clicked)
  }
})



output$Teacher_Shortage_Data_Table <- DT::renderDT({
  data <- DBMProp
  
  # Clean Region column to remove any whitespace
  data <- data %>%
    mutate(Region = stringr::str_trim(as.character(Region)))
  
  # Apply region filter if user has clicked
  region <- clicked_info()
  
  if (!is.null(region)) {
    region <- stringr::str_trim(as.character(region))  # trim click data too
    data <- data %>%
      filter(Region == region)
  }
  
  datatable(
    data,
    rownames = FALSE,
    options = list(
      scrollX = TRUE,
      columnDefs = list(list(className = 'dt-center', targets = "_all"))
    )
  )
})

# New reactive expression for the Teacher Shortage Regional Summary Table
# New reactive expression for the Teacher Shortage Regional Summary Table (now by Division within a clicked Region)
filtered_teacher_shortage_regional_summary_data <- reactive({
  req(filtered_teacher_shortage_data_region())
  data <- filtered_teacher_shortage_data_region()
  
  # Ensure shortage columns are numeric
  data <- data %>%
    mutate(
      ES_Shortage = as.numeric(ES_Shortage),
      JHS_Shortage = as.numeric(JHS_Shortage),
      SHS_Shortage = as.numeric(SHS_Shortage)
    )
  
  clicked_region <- NULL
  # We only care about the region here, not the specific inventory type clicked for this summary
  if (!is.null(rv$latest_teacher_shortage_click_key)) {
    clicked_key <- as.character(rv$latest_teacher_shortage_click_key)
    split_key <- strsplit(clicked_key, "::")[[1]]
    if (length(split_key) >= 1) { # Only need the first part for Region
      clicked_region <- split_key[1]
    }
  }
  
  if (!is.null(clicked_region)) {
    # If a specific region was clicked: summarize by Division within that region
    summary_data <- data %>%
      filter(Region == clicked_region) %>%
      group_by(Region, Division) %>%
      summarize(
        `Total ES Shortage` = sum(ES_Shortage, na.rm = TRUE),
        `Total JHS Shortage` = sum(JHS_Shortage, na.rm = TRUE),
        `Total SHS Shortage` = sum(SHS_Shortage, na.rm = TRUE),
        `Overall Shortage` = sum(ES_Shortage, JHS_Shortage, SHS_Shortage, na.rm = TRUE),
        .groups = 'drop'
      ) %>%
      select(Region, Division, `Total ES Shortage`, `Total JHS Shortage`, `Total SHS Shortage`, `Overall Shortage`)
  } else {
    # If no specific region is clicked (initial state or click reset):
    # Summarize by Division across all currently dashboard-filtered regions
    summary_data <- data %>%
      group_by(Region, Division) %>%
      summarize(
        `Total ES Shortage` = sum(ES_Shortage, na.rm = TRUE),
        `Total JHS Shortage` = sum(JHS_Shortage, na.rm = TRUE),
        `Total SHS Shortage` = sum(SHS_Shortage, na.rm = TRUE),
        `Overall Shortage` = sum(ES_Shortage, JHS_Shortage, SHS_Shortage, na.rm = TRUE),
        .groups = 'drop'
      ) %>%
      select(Region, Division, `Total ES Shortage`, `Total JHS Shortage`, `Total SHS Shortage`, `Overall Shortage`)
  }
  
  return(summary_data)
})
# Make sure to include the 'filtered_teacher_shortage_regional_summary_data' reactive
# that I provided in the previous step, before this DT::renderDT.

# DT::renderDT for the new Teacher Shortage Regional Summary Table
# This will display the aggregated regional data, filtered by graph clicks or dashboard inputs.
# DT::renderDT for the new Teacher Shortage Regional Summary Table
# This will display the aggregated regional data by division, filtered by graph clicks or dashboard inputs.
output$Teacher_Shortage_Regional_Table <- DT::renderDT({
  table_to_display <- filtered_teacher_shortage_regional_summary_data()
  
  # Handle case where no data is available based on filters/clicks
  if (nrow(table_to_display) == 0) {
    return(DT::datatable(data.frame(Message = "No regional teacher shortage data available for this selection."), options = list(dom = 't')))
  }
  
  # Render the data table
  DT::datatable(table_to_display %>% slice(1:100),
                options = list(pageLength = 10, scrollX = TRUE, filter = "top"),
                rownames = FALSE) %>%
    # Format the shortage columns as integers with commas
    formatCurrency(c("Total ES Shortage", "Total JHS Shortage", "Total SHS Shortage", "Overall Shortage"), currency = '', digits = 0)
})

observeEvent(event_data("plotly_click", source = "teacherShortageRegionPlot"), {
  click_data <- event_data("plotly_click", source = "teacherShortageRegionPlot")
  if (!is.null(click_data)) {
    rv$latest_teacher_shortage_click_key <- click_data$key
    # Add a print statement here to see if the click is registered
    print(paste("Graph Clicked! Key:", click_data$key))
  } else {
    rv$latest_teacher_shortage_click_key <- NULL
  }
})

filtered_teacher_shortage_table_data <- reactive({
  # Add this print statement
  print(paste("filtered_teacher_shortage_table_data: rv$latest_teacher_shortage_click_key =", rv$latest_teacher_shortage_click_key))
  
  req(filtered_teacher_shortage_data())
  data <- filtered_teacher_shortage_data()
  
  # ... (rest of your reactive code) ...
  
  if (!is.null(clicked_region) && !is.null(clicked_inventory_type)) {
    # Add this print statement
    print(paste("Filtering for Region:", clicked_region, "Type:", clicked_inventory_type))
    # ... (rest of filtering logic) ...
  } else {
    # Add this print statement
    print("No specific click, showing all relevant data.")
    # ... (rest of else logic) ...
  }
  
  # Add this print statement before returning
  print(paste("filtered_teacher_shortage_table_data: Rows in final_data =", nrow(final_data)))
  
  return(final_data)
})

# server.R

# Ensure 'uni' is accessible (e.g., loaded at the top of server.R or globally)
# For demonstration, let's assume 'uni' is a data.frame already loaded.
# If 'uni' is a reactive expression, use uni() when accessing it.
# Example:
# uni <- read.csv("your_data.csv") # Or whatever your data source is

# --- Reactive Values for Global Filtering (if you have them, not shown here) ---
# Example:
# filtered_school_data <- reactive({
#   # Your global filtering logic here
#   # e.g., uni %>%
#   #   filter(Region %in% input$selected_region,
#   #          Modified.COC %in% input$selected_coc_type)
#   # For this example, let's assume it's just 'uni' for simplicity
#   uni
# })


# reactiveVal to store the data for the table based on clicks
reactive_clicked_school_data <- reactiveVal(
  data.frame(
    SchoolID = character(),
    SchoolName = character(),
    Region = character(),
    Division = character(),
    Legislative.District = character(),
    Modified.COC = character(),
    stringsAsFactors = FALSE
  )
)

# A reactive value to track if a chart has been clicked (and to show click-filtered data)
# This helps distinguish between initial load/global filter and click-filtered state
rv <- reactiveValues(
  chart_clicked = FALSE # TRUE if any chart was clicked, FALSE otherwise
)

# Observe click events from the regional plot
observeEvent(event_data("plotly_click", source = "schoolcountplot_region"), {
  click_data <- event_data("plotly_click", source = "schoolcountplot_region")
  print("Regional graph clicked!") # DEBUG
  
  req(click_data, click_data$key, is.character(click_data$key))
  
  clicked_info <- click_data$key
  print(paste0("Regional Clicked Key (raw): '", clicked_info, "'")) # DEBUG
  
  # Use strsplit with fixed = TRUE for literal "|"
  parsed_info <- strsplit(clicked_info, "|", fixed = TRUE)[[1]]
  region_name <- trimws(parsed_info[1]) # Trim whitespace, just in case
  school_type <- trimws(parsed_info[2]) # Trim whitespace
  
  print(paste0("Regional Parsed: Region = '", region_name, "', School Type = '", school_type, "'")) # DEBUG
  
  # Filter the original 'uni' data based on the click
  # IMPORTANT: Make sure 'uni' is the correct dataset and columns match
  filtered_schools <- uni %>%
    filter(Region == region_name, Modified.COC == school_type)
  
  print(paste("Regional Filtered Schools Count:", nrow(filtered_schools))) # DEBUG
  if (nrow(filtered_schools) > 0) {
    print("Head of Regional Filtered Schools:") # DEBUG
    print(head(filtered_schools)) # DEBUG (See first few rows of filtered data)
  } else {
    print("No schools found after filtering for this regional click.")
  }
  
  reactive_clicked_school_data(filtered_schools)
  rv$chart_clicked <- TRUE # Indicate that a chart has been clicked
})

# Observe click events from the division plot
observeEvent(event_data("plotly_click", source = "schoolcountplot_division"), {
  click_data <- event_data("plotly_click", source = "schoolcountplot_division")
  print("Division graph clicked!") # DEBUG
  
  req(click_data, click_data$key, is.character(click_data$key))
  
  clicked_info <- click_data$key
  print(paste0("Division Clicked Key (raw): '", clicked_info, "'")) # DEBUG
  
  # Assuming key is now "DivisionName|SchoolTypeWithSpaces"
  parsed_info <- strsplit(clicked_info, "|", fixed = TRUE)[[1]]
  division_name <- trimws(parsed_info[1])
  school_type <- trimws(parsed_info[2])
  
  print(paste0("Division Parsed: Division = '", division_name, "', School Type = '", school_type, "'")) # DEBUG
  
  # Make sure you're filtering the correct base data.
  # If filtered_school_data_region() is meant to be the *source* for division filtering, use it.
  # Otherwise, filter from the main 'uni' dataset, applying prior filters if necessary.
  # For simplicity and correctness, let's filter from 'uni' directly here.
  # If you have an existing chain of filters (e.g., region -> division), ensure that logic.
  # For now, let's assume `uni` is the base.
  filtered_schools <- uni %>%
    filter(Division == division_name, Modified.COC == school_type)
  
  print(paste("Division Filtered Schools Count:", nrow(filtered_schools))) # DEBUG
  
  reactive_clicked_school_data(filtered_schools)
  rv$chart_clicked <- TRUE # Indicate that a chart has been clicked
})

# Observe click events from the district plot
observeEvent(event_data("plotly_click", source = "schoolcountplot_district"), {
  click_data <- event_data("plotly_click", source = "schoolcountplot_district")
  print("District graph clicked!") # DEBUG
  
  req(click_data, click_data$key, is.character(click_data$key))
  
  clicked_info <- click_data$key
  print(paste0("District Clicked Key (raw): '", clicked_info, "'")) # DEBUG
  
  # Assuming key is now "DistrictName|SchoolTypeWithSpaces"
  parsed_info <- strsplit(clicked_info, "|", fixed = TRUE)[[1]]
  district_name <- trimws(parsed_info[1])
  school_type <- trimws(parsed_info[2])
  
  print(paste0("District Parsed: District = '", district_name, "', School Type = '", school_type, "'")) # DEBUG
  
  # Make sure you're filtering the correct base data.
  # Same as division, filter from 'uni' or your intended pre-filtered reactive.
  filtered_schools <- uni %>%
    filter(Legislative.District == district_name, Modified.COC == school_type)
  
  print(paste("District Filtered Schools Count:", nrow(filtered_schools))) # DEBUG
  
  reactive_clicked_school_data(filtered_schools)
  rv$chart_clicked <- TRUE # Indicate that a chart has been clicked
})

output$school_count_data_table <- DT::renderDT({
  
  data_to_display <- filtered_school_data_division() %>% select(Region, Division, Legislative.District, SchoolID, School.Name, Modified.COC) %>% rename("Modified Curricular OFfering" = Modified.COC)
  
  # You might want to add a check for NULL or empty data if filtered_school_data_division()
  # could return such states and you want to display a message or an empty table.
  if (is.null(data_to_display) || nrow(data_to_display) == 0) {
    return(DT::datatable(
      data.frame("Message" = "No data available based on current selection."),
      options = list(dom = 't'), # 't' hides all controls, showing only the table body
      rownames = FALSE
    ))
  }
  
  DT::datatable(
    data_to_display,
    options = list(pageLength = 10, scrollX = TRUE),
    filter = 'top',
    selection = 'multiple',
    rownames = FALSE
  )
})

output$regprof_DT <- DT::renderDT({
  
  data_to_display <- filtered_school_data_region() %>% select(Region, Division, Legislative.District, SchoolID, School.Name, School.Type, School.Size.Typology, Modified.COC, TotalEnrolment, Clustering.Status, PDOI_Deployment) %>% rename("Modified Curricular Offering" = Modified.COC)
  
  # You might want to add a check for NULL or empty data if filtered_school_data_division()
  # could return such states and you want to display a message or an empty table.
  if (is.null(data_to_display) || nrow(data_to_display) == 0) {
    return(DT::datatable(
      data.frame("Message" = "No data available based on current selection."),
      options = list(dom = 't',
                     scrollX = TRUE,
                     fixedColumns = list(leftColumns = 5)), # 't' hides all controls, showing only the table body
      rownames = FALSE
    ))
  }
  
  DT::datatable(
    data_to_display,
    extensions = c("FixedHeader", "FixedColumns"),
    options = list(
      pageLength = 10, 
      scrollX = TRUE,
      scrollY = 400,
      autoWidth = TRUE,
      fixedHeader = TRUE,
      fixedColumns = list(leftColumns = 5)),
    filter = 'top',
    selection = 'multiple',
    rownames = FALSE
  )
})

output$regprof_DT_CL <- DT::renderDT({
  
  data_to_display <- filtered_LMS_region() %>%
    mutate(across(13:17, ~ case_when(
      . == 0 ~ "No",
      TRUE ~ "Yes"
    ))) %>% select(Region, Division, Legislative_District, School_ID, School_Name, Total_Enrollment, Instructional_Rooms, CL_Req, Estimated_CL_Shortage, With_Excess, Without_Shortage,Buildable_space,LMS,GIDCA) %>% rename("Classroom Requirement" = CL_Req,"Estimated Classroom Shortage" = Estimated_CL_Shortage,"Schools with Excess Classrooms" = With_Excess,"Schools without Classroom Shortage" = Without_Shortage,"Schools with Buildable Space" = Buildable_space)
  
  # You might want to add a check for NULL or empty data if filtered_school_data_division()
  # could return such states and you want to display a message or an empty table.
  if (is.null(data_to_display) || nrow(data_to_display) == 0) {
    return(DT::datatable(
      data.frame("Message" = "No data available based on current selection."),
      options = list(dom = 't',
                     scrollX = TRUE,
                     fixedColumns = list(leftColumns = 5)), # 't' hides all controls, showing only the table body
      rownames = FALSE
    ))
  }
  
  DT::datatable(
    data_to_display,
    extensions = c("FixedHeader", "FixedColumns"),
    options = list(
      pageLength = 10, 
      scrollX = TRUE,
      scrollY = 400,
      autoWidth = TRUE,
      fixedHeader = TRUE,
      fixedColumns = list(leftColumns = 5)),
    filter = 'top',
    selection = 'multiple',
    rownames = FALSE
  )
})
filtered_cloud_region <- reactive({
  req(cloud)
  
  temp_data <- cloud
  
  if (!is.null(input$cloud_region_filter) && length(input$cloud_region_filter) > 0) {
    temp_data <- temp_data %>%
      filter(BASIC.REGION %in% input$cloud_region_filter)
  }
  
  if (!is.null(input$cloud_division_filter) && length(input$cloud_division_filter) > 0) {
    temp_data <- temp_data %>%
      filter(BASIC.DIVISION %in% input$cloud_division_filter)
  }
  
  return(temp_data)
})

filtered_cloud_region <- reactive({
  req(cloud)
  
  temp_data <- cloud
  
  if (!is.null(input$cloud_region_filter) && length(input$cloud_region_filter) > 0) {
    temp_data <- temp_data %>%
      filter(BASIC.REGION %in% input$cloud_region_filter)
  }
  
  if (!is.null(input$cloud_division_filter) && length(input$cloud_division_filter) > 0) {
    temp_data <- temp_data %>%
      filter(BASIC.DIVISION %in% input$cloud_division_filter)
  }
  
  return(temp_data)
})

filtered_LMS_region <- reactive({
  req(LMS)
  
  temp_data <- LMS
  
  if (!is.null(input$dashboard_region_filter) && length(input$dashboard_region_filter) > 0) {
    temp_data <- temp_data %>%
      filter(Region %in% input$dashboard_region_filter)
  }
  
  return(temp_data)
})

filtered_LMS_division <- reactive({
  req(LMS)
  
  temp_data <- LMS
  
  if (!is.null(input$dashboard_region_filter) && length(input$dashboard_region_filter) > 0) {
    temp_data <- temp_data %>%
      filter(Region %in% input$dashboard_region_filter)
  }
  
  if (!is.null(input$dashboard_division_filter) && length(input$dashboard_division_filter) > 0) {
    temp_data <- temp_data %>%
      filter(Division %in% input$dashboard_division_filter)
  }
  
  return(temp_data)
})

filtered_cloud_region_v2 <- reactive({
  req(cloud_v2)
  
  temp_data <- cloud_v2
  
  if (!is.null(input$cloud_region_filter) && length(input$cloud_region_filter) > 0) {
    temp_data <- temp_data %>%
      filter(BASIC.REGION %in% input$cloud_region_filter)
  }
  
  if (!is.null(input$cloud_division_filter) && length(input$cloud_division_filter) > 0) {
    temp_data <- temp_data %>%
      filter(BASIC.DIVISION %in% input$cloud_division_filter)
  }
  
  return(temp_data)
})

filtered_cloud_region_v3 <- reactive({
  req(cloud_v3)
  
  temp_data <- cloud_v3
  
  if (!is.null(input$cloud_region_filter) && length(input$cloud_region_filter) > 0) {
    temp_data <- temp_data %>%
      filter(BASIC.REGION %in% input$cloud_region_filter)
  }
  
  if (!is.null(input$cloud_division_filter) && length(input$cloud_division_filter) > 0) {
    temp_data <- temp_data %>%
      filter(BASIC.DIVISION %in% input$cloud_division_filter)
  }
  
  return(temp_data)
})

# Add a mechanism to reset the table when global filters change (optional but recommended)
# This will make the table revert to globally filtered data if user changes region/division
# after clicking on a chart.
observeEvent(list(input$selected_region, input$selected_division), {
  rv$chart_clicked <- FALSE # Reset the click state
  print("Global filters changed, resetting chart_clicked state.") # DEBUG
})

# For Classroom_Shortage_Division_Graph
observeEvent(plotly::event_data("plotly_click", source = "classroomShortage_division"), {
  click_data <- plotly::event_data("plotly_click", source = "classroomShortage_division")
  if (!is.null(click_data)) {
    rv$latest_classroom_click_key <- click_data$x
    rv$latest_classroom_click_type <- "classroom_division"
    print(paste("DEBUG: Classroom Division Clicked:", rv$latest_classroom_click_key))
    print(paste("DEBUG: Classroom Click Type:", rv$latest_classroom_click_type))
  }
})

# For Classroom_Shortage_District_Graph
observeEvent(plotly::event_data("plotly_click", source = "classroomShortage_district"), {
  click_data <- plotly::event_data("plotly_click", source = "classroomShortage_district")
  if (!is.null(click_data)) {
    rv$latest_classroom_click_key <- click_data$x
    rv$latest_classroom_click_type <- "classroom_district"
    print(paste("DEBUG: Classroom District Clicked:", rv$latest_classroom_click_key))
    print(paste("DEBUG: Classroom Click Type:", rv$latest_classroom_click_type))
  }
})

# Assuming 'rv' is a reactiveValues object initialized in your server.R,
# for example:
# rv <- reactiveValues(
#   latest_classroom_click_key = NULL,
#   latest_classroom_click_type = NULL
# )

# Assuming Classroom_Shortage_All_List is a data frame available in your server.R
# Example dummy data for demonstration:
# Classroom_Shortage_All_List <- data.frame(
#   Division = c("Division A", "Division B", "Division A", "Division C", "Division B"),
#   Legislative.District = c("District 1", "District 2", "District 1", "District 3", "District 1"),
#   Shortage = c(10, 5, 12, 8, 3),
#   Other_Column = c("X", "Y", "Z", "A", "B")
# )

# Assuming 'filtered_school_data_division' is a reactive expression defined elsewhere in your server.R,
# for example:
# filtered_school_data_division <- reactive({
#   # Your filtering logic here, e.g.,
#   # if (!is.null(rv$some_filter_key)) {
#   #   Classroom_Shortage_All_List[Classroom_Shortage_All_List$Division == rv$some_filter_key, ]
#   # } else {
#   #   Classroom_Shortage_All_List
#   # }
# })

output$Classroom_Shortage_All_List  <- DT::renderDT({
  # Ensure that filtered_school_data_division() returns a data frame
  # and handle cases where it might be NULL or empty initially.
  data_to_display <- filtered_school_data_division() %>% select(Region, Division, Legislative.District, SchoolID, School.Name, Est.CS) %>% rename("Estimated Classroom Shortage" = Est.CS)
  
  # You might want to add a check for NULL or empty data if filtered_school_data_division()
  # could return such states and you want to display a message or an empty table.
  if (is.null(data_to_display) || nrow(data_to_display) == 0) {
    return(DT::datatable(
      data.frame("Message" = "No data available based on current selection."),
      options = list(dom = 't'), # 't' hides all controls, showing only the table body
      rownames = FALSE
    ))
  }
  
  DT::datatable(
    data_to_display, # Use the output of your reactive expression
    options = list(
      pageLength = 10, # Number of rows to display per page
      lengthMenu = c(5, 10, 15, 20), # Options for number of rows per page
      searching = TRUE, # Enable search box
      filter = "top",
      paging = TRUE, # Enable pagination
      info = TRUE, # Display table information (e.g., "Showing 1 to 10 of 50 entries")
      ordering = TRUE # Enable column sorting
    ),
    rownames = FALSE # Do not display row names
  )
})

output$enrolment_data_table <- DT::renderDT({
  
  data_to_display <- filtered_cloud_region() %>%
    select(BASIC.REGION,BASIC.DIVISION,ENROLLMENT.G1,ENROLLMENT.G2,ENROLLMENT.G3,ENROLLMENT.G4,ENROLLMENT.G5,ENROLLMENT.G6,ENROLLMENT.G7,ENROLLMENT.G8,ENROLLMENT.G9,ENROLLMENT.G10,ENROLMENT.G11,ENROLMENT.G12) %>%
    mutate(across(3:last_col(), as.numeric)) %>% 
    mutate(BASIC.DIVISION = as.character(BASIC.DIVISION)) %>%
    filter(BASIC.DIVISION != "") %>% 
    group_by(BASIC.REGION,BASIC.DIVISION) %>% 
    summarise(across(everything(), sum, na.rm = TRUE)) %>%
    rename("REGION" = BASIC.REGION,
           "DIVISION" = BASIC.DIVISION,
           "GRADE 1" = ENROLLMENT.G1,
           "GRADE 2" = ENROLLMENT.G2,
           "GRADE 3" = ENROLLMENT.G3,
           "GRADE 4" = ENROLLMENT.G4,
           "GRADE 5" = ENROLLMENT.G5,
           "GRADE 6" = ENROLLMENT.G6,
           "GRADE 7" = ENROLLMENT.G7,
           "GRADE 8" = ENROLLMENT.G8,
           "GRADE 9" = ENROLLMENT.G9,
           "GRADE 10" = ENROLLMENT.G10,
           "GRADE 11" = ENROLMENT.G11,
           "GRADE 12" = ENROLMENT.G12)
  
  # You might want to add a check for NULL or empty data if filtered_school_data_division()
  # could return such states and you want to display a message or an empty table.
  if (is.null(data_to_display) || nrow(data_to_display) == 0) {
    return(DT::datatable(
      data.frame("Message" = "No data available based on current selection."),
      options = list(dom = 't'), # 't' hides all controls, showing only the table body
      rownames = FALSE
    ))
  }
  
  DT::datatable(
    data_to_display,
    options = list(pageLength = 10, scrollX = TRUE),
    filter = 'top',
    selection = 'multiple',
    rownames = FALSE
  )
})

output$enrolment_regional_graph <- renderPlotly({
  # Use the reactive filtered data
  
  data_to_display <- cloud %>%
    select(BASIC.REGION,ENROLLMENT.G1,ENROLLMENT.G2,ENROLLMENT.G3,ENROLLMENT.G4,ENROLLMENT.G5,ENROLLMENT.G6,ENROLLMENT.G7,ENROLLMENT.G8,ENROLLMENT.G9,ENROLLMENT.G10,ENROLMENT.G11,ENROLMENT.G12) %>% 
    mutate(across(2:last_col(), as.numeric)) %>% 
    mutate(BASIC.REGION = as.character(BASIC.REGION)) %>%
    filter(BASIC.REGION != "") %>% 
    group_by(BASIC.REGION) %>%
    summarise(across(everything(), sum, na.rm = TRUE)) %>%
    rename("REGION" = BASIC.REGION,
           "GRADE 1" = ENROLLMENT.G1,
           "GRADE 2" = ENROLLMENT.G2,
           "GRADE 3" = ENROLLMENT.G3,
           "GRADE 4" = ENROLLMENT.G4,
           "GRADE 5" = ENROLLMENT.G5,
           "GRADE 6" = ENROLLMENT.G6,
           "GRADE 7" = ENROLLMENT.G7,
           "GRADE 8" = ENROLLMENT.G8,
           "GRADE 9" = ENROLLMENT.G9,
           "GRADE 10" = ENROLLMENT.G10,
           "GRADE 11" = ENROLMENT.G11,
           "GRADE 12" = ENROLMENT.G12) %>% 
    pivot_longer(
      cols = 2:last_col(), # Specifies the columns to pivot
      names_to = "Enrolment", # The new column to hold the original column names
      values_to = "Count" # The new column to hold the values
    ) %>%
    mutate(Enrolment = factor(Enrolment, levels = c(
      "GRADE 1", "GRADE 2", "GRADE 3",
      "GRADE 4", "GRADE 5", "GRADE 6",
      "GRADE 7", "GRADE 8", "GRADE 9",
      "GRADE 10", "GRADE 11", "GRADE 12"
    ))) 
  
  current_filtered_data <- data_to_display
  
  # --- Empty Data Handling ---
  if (nrow(current_filtered_data) == 0) {
    return(ggplotly(ggplot() +
                      annotate("text", x = 0.5, y = 0.5, label = "No data for selected regions/divisions") +
                      theme_void()))
  }
  
  # Prepare the data for plotting
  plot_data <- data_to_display
  
  # Create the ggplot
  p <- ggplot(plot_data,
              aes(x = REGION, # Reorder regions based on overall total count for the region
                  y = Count,
                  fill = Enrolment, # Fill by Clustering.Status for coloring and consistent order
                  text = paste("Region: ", REGION,
                               "<br>Section: ", Enrolment,
                               "<br>Count: ", scales::comma(Count)))) + # Custom tooltip text
    geom_bar(stat = "identity", position = "dodge", color = "black") + # Changed to position="stack" for stacked bars
    labs(x = "",
         y = "") +
    scale_y_continuous(labels = scales::comma) + # Format y-axis labels as comma-separated numbers
    theme_minimal() +
    theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
          legend.position = "bottom", # Position legend at the bottom
          plot.title = element_text(hjust = 0.5)) # Center the plot title
  
  # Convert ggplot to plotly, ensuring custom text is used for hover
  ggplotly(p, tooltip = "text", source = "cloudenrolmentRegionPlot") %>%
    layout(hoverlabel = list(bgcolor = "white"),
           margin = list(b = 100)) # Increase bottom margin for x-axis labels
})

output$enrolment_division_graph <- renderPlotly({
  # Use the reactive filtered data
  
  data_to_display <- filtered_cloud_region() %>%
    select(BASIC.DIVISION,ENROLLMENT.G1,ENROLLMENT.G2,ENROLLMENT.G3,ENROLLMENT.G4,ENROLLMENT.G5,ENROLLMENT.G6,ENROLLMENT.G7,ENROLLMENT.G8,ENROLLMENT.G9,ENROLLMENT.G10,ENROLMENT.G11,ENROLMENT.G12) %>% 
    mutate(across(2:last_col(), as.numeric)) %>% 
    mutate(BASIC.DIVISION = as.character(BASIC.DIVISION)) %>%
    filter(BASIC.DIVISION != "") %>% 
    group_by(BASIC.DIVISION) %>%
    summarise(across(everything(), sum, na.rm = TRUE)) %>%
    rename("DIVISION" = BASIC.DIVISION,
           "GRADE 1" = ENROLLMENT.G1,
           "GRADE 2" = ENROLLMENT.G2,
           "GRADE 3" = ENROLLMENT.G3,
           "GRADE 4" = ENROLLMENT.G4,
           "GRADE 5" = ENROLLMENT.G5,
           "GRADE 6" = ENROLLMENT.G6,
           "GRADE 7" = ENROLLMENT.G7,
           "GRADE 8" = ENROLLMENT.G8,
           "GRADE 9" = ENROLLMENT.G9,
           "GRADE 10" = ENROLLMENT.G10,
           "GRADE 11" = ENROLMENT.G11,
           "GRADE 12" = ENROLMENT.G12) %>% 
    pivot_longer(
      cols = 2:last_col(), # Specifies the columns to pivot
      names_to = "Enrolment", # The new column to hold the original column names
      values_to = "Count" # The new column to hold the values
    ) %>%
    mutate(Enrolment = factor(Enrolment, levels = c(
      "GRADE 1", "GRADE 2", "GRADE 3",
      "GRADE 4", "GRADE 5", "GRADE 6",
      "GRADE 7", "GRADE 8", "GRADE 9",
      "GRADE 10", "GRADE 11", "GRADE 12"
    ))) 
  
  current_filtered_data <- data_to_display
  
  # --- Empty Data Handling ---
  if (nrow(current_filtered_data) == 0) {
    return(ggplotly(ggplot() +
                      annotate("text", x = 0.5, y = 0.5, label = "No data for selected regions/divisions") +
                      theme_void()))
  }
  
  # Prepare the data for plotting
  plot_data <- data_to_display
  
  # Create the ggplot
  p <- ggplot(plot_data,
              aes(x = Enrolment, # Reorder regions based on overall total count for the region
                  y = Count,
                  fill = Enrolment, # Fill by Clustering.Status for coloring and consistent order
                  text = paste("Division: ", DIVISION,
                               "<br>Section: ", Enrolment,
                               "<br>Count: ", scales::comma(Count)))) + # Custom tooltip text
    geom_bar(stat = "identity", position = "dodge", color = "black") + # Changed to position="stack" for stacked bars
    scale_y_continuous(labels = scales::comma) + # Format y-axis labels as comma-separated numbers
    theme_minimal() +
    theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
          legend.position = "bottom", # Position legend at the bottom
          plot.title = element_text(hjust = 0.5)) +
    facet_wrap(~DIVISION) + coord_flip()+
    labs(x = NULL, y = NULL, fill = NULL)
# Center the plot title
  
  # Convert ggplot to plotly, ensuring custom text is used for hover
  ggplotly(p, tooltip = "text", source = "cloudenrolmentDivisionPlot") %>%
    layout(hoverlabel = list(bgcolor = "white"),
           margin = list(b = 100)) # Increase bottom margin for x-axis labels
})

output$sned_data_table <- DT::renderDT({
  
  data_to_display <- filtered_cloud_region() %>%
    select(BASIC.REGION,BASIC.DIVISION, SPECIAL.NEEDS.G1, SPECIAL.NEEDS.G2, SPECIAL.NEEDS.G3, SPECIAL.NEEDS.G4, SPECIAL.NEEDS.G5, SPECIAL.NEEDS.G6, SPECIAL.NEEDS.G7, SPECIAL.NEEDS.G8, SPECIAL.NEEDS.G9, SPECIAL.NEEDS.G10) %>% 
    mutate(across(3:last_col(), as.numeric)) %>% 
    mutate(BASIC.DIVISION = as.character(BASIC.DIVISION)) %>%
    filter(BASIC.DIVISION != "") %>% 
    group_by(BASIC.REGION,BASIC.DIVISION) %>%
    summarise(across(everything(), sum, na.rm = TRUE)) %>% 
    mutate(SPECIAL.NEEDS.G1 = as.integer(SPECIAL.NEEDS.G1),SPECIAL.NEEDS.G2 = as.integer(SPECIAL.NEEDS.G2)) %>% 
    rename(
      "REGION" = BASIC.REGION,
      "DIVISION" = BASIC.DIVISION,
      "GRADE 1" = SPECIAL.NEEDS.G1,
      "GRADE 2" = SPECIAL.NEEDS.G2,
      "GRADE 3" = SPECIAL.NEEDS.G3,
      "GRADE 4" = SPECIAL.NEEDS.G4,
      "GRADE 5" = SPECIAL.NEEDS.G5,
      "GRADE 6" = SPECIAL.NEEDS.G6,
      "GRADE 7" = SPECIAL.NEEDS.G7,
      "GRADE 8" = SPECIAL.NEEDS.G8,
      "GRADE 9" = SPECIAL.NEEDS.G9,
      "GRADE 10" = SPECIAL.NEEDS.G10
    )
  
  # You might want to add a check for NULL or empty data if filtered_school_data_division()
  # could return such states and you want to display a message or an empty table.
  if (is.null(data_to_display) || nrow(data_to_display) == 0) {
    return(DT::datatable(
      data.frame("Message" = "No data available based on current selection."),
      options = list(dom = 't'), # 't' hides all controls, showing only the table body
      rownames = FALSE
    ))
  }
  
  DT::datatable(
    data_to_display,
    options = list(pageLength = 10, scrollX = TRUE),
    filter = 'top',
    selection = 'multiple',
    rownames = FALSE
  )
})

output$sned_regional_graph <- renderPlotly({
  # Use the reactive filtered data
  
  data_to_display <- cloud %>%
    select(BASIC.REGION, SPECIAL.NEEDS.G1, SPECIAL.NEEDS.G2, SPECIAL.NEEDS.G3, SPECIAL.NEEDS.G4, SPECIAL.NEEDS.G5, SPECIAL.NEEDS.G6, SPECIAL.NEEDS.G7, SPECIAL.NEEDS.G8, SPECIAL.NEEDS.G9, SPECIAL.NEEDS.G10) %>% 
    mutate(across(2:last_col(), as.numeric)) %>% 
    mutate(BASIC.REGION = as.character(BASIC.REGION)) %>%
    filter(BASIC.REGION != "") %>% 
    group_by(BASIC.REGION) %>%
    summarise(across(everything(), sum, na.rm = TRUE)) %>% 
    rename(
      "REGION" = BASIC.REGION,
      "GRADE 1" = SPECIAL.NEEDS.G1,
      "GRADE 2" = SPECIAL.NEEDS.G2,
      "GRADE 3" = SPECIAL.NEEDS.G3,
      "GRADE 4" = SPECIAL.NEEDS.G4,
      "GRADE 5" = SPECIAL.NEEDS.G5,
      "GRADE 6" = SPECIAL.NEEDS.G6,
      "GRADE 7" = SPECIAL.NEEDS.G7,
      "GRADE 8" = SPECIAL.NEEDS.G8,
      "GRADE 9" = SPECIAL.NEEDS.G9,
      "GRADE 10" = SPECIAL.NEEDS.G10
    )  %>% 
    pivot_longer(
      cols = 2:last_col(), # Specifies the columns to pivot
      names_to = "SNED_Learners", # The new column to hold the original column names
      values_to = "Count" # The new column to hold the values
    ) %>%
    mutate(SNED_Learners = factor(SNED_Learners, levels = c(
      "GRADE 1", "GRADE 2", "GRADE 3",
      "GRADE 4", "GRADE 5", "GRADE 6",
      "GRADE 7", "GRADE 8", "GRADE 9",
      "GRADE 10")))
  
  current_filtered_data <- data_to_display
  
  # --- Empty Data Handling ---
  if (nrow(current_filtered_data) == 0) {
    return(ggplotly(ggplot() +
                      annotate("text", x = 0.5, y = 0.5, label = "No data for selected regions/divisions") +
                      theme_void()))
  }
  
  # Prepare the data for plotting
  plot_data <- data_to_display
  
  # Create the ggplot
  p <- ggplot(plot_data,
              aes(x = REGION, # Reorder regions based on overall total count for the region
                  y = Count,
                  fill = SNED_Learners, # Fill by Clustering.Status for coloring and consistent order
                  text = paste("Region: ", REGION,
                               "<br>Section: ", SNED_Learners,
                               "<br>Count: ", scales::comma(Count)))) + # Custom tooltip text
    geom_bar(stat = "identity", position = "dodge", color = "black") + # Changed to position="stack" for stacked bars
    labs(x = "",
         y = "") +
    scale_y_continuous(labels = scales::comma) + # Format y-axis labels as comma-separated numbers
    theme_minimal() +
    theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
          legend.position = "bottom", # Position legend at the bottom
          plot.title = element_text(hjust = 0.5)) # Center the plot title
  
  # Convert ggplot to plotly, ensuring custom text is used for hover
  ggplotly(p, tooltip = "text", source = "cloudenrolmentRegionPlot") %>%
    layout(hoverlabel = list(bgcolor = "white"),
           margin = list(b = 100)) # Increase bottom margin for x-axis labels
})

output$sned_division_graph <- renderPlotly({
  # Use the reactive filtered data
  
  data_to_display <- filtered_cloud_region() %>%
    select(BASIC.DIVISION, SPECIAL.NEEDS.G1, SPECIAL.NEEDS.G2, SPECIAL.NEEDS.G3, SPECIAL.NEEDS.G4, SPECIAL.NEEDS.G5, SPECIAL.NEEDS.G6, SPECIAL.NEEDS.G7, SPECIAL.NEEDS.G8, SPECIAL.NEEDS.G9, SPECIAL.NEEDS.G10) %>% 
    mutate(across(2:last_col(), as.numeric)) %>% 
    mutate(BASIC.DIVISION = as.character(BASIC.DIVISION)) %>%
    filter(BASIC.DIVISION != "") %>% 
    group_by(BASIC.DIVISION) %>%
    summarise(across(everything(), sum, na.rm = TRUE)) %>% 
    rename(
      "DIVISION" = BASIC.DIVISION,
      "GRADE 1" = SPECIAL.NEEDS.G1,
      "GRADE 2" = SPECIAL.NEEDS.G2,
      "GRADE 3" = SPECIAL.NEEDS.G3,
      "GRADE 4" = SPECIAL.NEEDS.G4,
      "GRADE 5" = SPECIAL.NEEDS.G5,
      "GRADE 6" = SPECIAL.NEEDS.G6,
      "GRADE 7" = SPECIAL.NEEDS.G7,
      "GRADE 8" = SPECIAL.NEEDS.G8,
      "GRADE 9" = SPECIAL.NEEDS.G9,
      "GRADE 10" = SPECIAL.NEEDS.G10
    )  %>% 
    pivot_longer(
      cols = 2:last_col(), # Specifies the columns to pivot
      names_to = "SNED_Learners", # The new column to hold the original column names
      values_to = "Count" # The new column to hold the values
    ) %>%
    mutate(SNED_Learners = factor(SNED_Learners, levels = c(
      "GRADE 1", "GRADE 2", "GRADE 3",
      "GRADE 4", "GRADE 5", "GRADE 6",
      "GRADE 7", "GRADE 8", "GRADE 9",
      "GRADE 10")))
  
  current_filtered_data <- data_to_display
  
  # --- Empty Data Handling ---
  if (nrow(current_filtered_data) == 0) {
    return(ggplotly(ggplot() +
                      annotate("text", x = 0.5, y = 0.5, label = "No data for selected regions/divisions") +
                      theme_void()))
  }
  
  # Prepare the data for plotting
  plot_data <- data_to_display
  
  # Create the ggplot
  p <- ggplot(plot_data,
              aes(x = SNED_Learners, y = Count,
                  fill = SNED_Learners,
                  text = paste("Region: ", DIVISION,
                               "<br>Section: ", SNED_Learners,
                               "<br>Count: ", scales::comma(Count)))) +
    geom_bar(stat = "identity", position = "dodge", color = "black") +
    scale_y_continuous(labels = scales::comma) +
    theme_minimal() +
    theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
          legend.position = "bottom",
          plot.title = element_text(hjust = 0.5)) +
    facet_wrap(~DIVISION) + coord_flip()+
    labs(x = NULL, y = NULL, fill = NULL)

  
  ggplotly(p, tooltip = "text", source = "cloudenrolmentDivisionPlot") %>%
    layout(hoverlabel = list(bgcolor = "white"),
           margin = list(b = 100)) # Increase bottom margin for x-axis labels
})

output$ip_data_table <- DT::renderDT({
  
  data_to_display <- filtered_cloud_region() %>%
    select(BASIC.REGION,BASIC.DIVISION, IP.G1.IP, IP.G2.IP, IP.G3.IP, IP.G4.IP, IP.G5.IP, IP.G6.IP, IP.G7.IP, IP.G8.IP, IP.G9.IP, IP.G10.IP, IP.G11, IP.G12) %>% 
    mutate(across(3:last_col(), as.numeric)) %>% 
    mutate(BASIC.DIVISION = as.character(BASIC.DIVISION)) %>%
    filter(BASIC.DIVISION != "") %>% 
    group_by(BASIC.REGION,BASIC.DIVISION) %>%
    summarise(across(everything(), sum, na.rm = TRUE)) %>% 
    rename(
      "REGION" = BASIC.REGION,
      "DIVISION" = BASIC.DIVISION,
      "GRADE 1" = IP.G1.IP,
      "GRADE 2" = IP.G2.IP,
      "GRADE 3" = IP.G3.IP,
      "GRADE 4" = IP.G4.IP,
      "GRADE 5" = IP.G5.IP,
      "GRADE 6" = IP.G6.IP,
      "GRADE 7" = IP.G7.IP,
      "GRADE 8" = IP.G8.IP,
      "GRADE 9" = IP.G9.IP,
      "GRADE 10" = IP.G10.IP,
      "GRADE 11" = IP.G11,
      "GRADE 12" = IP.G12
    ) 
  
  # You might want to add a check for NULL or empty data if filtered_school_data_division()
  # could return such states and you want to display a message or an empty table.
  if (is.null(data_to_display) || nrow(data_to_display) == 0) {
    return(DT::datatable(
      data.frame("Message" = "No data available based on current selection."),
      options = list(dom = 't'), # 't' hides all controls, showing only the table body
      rownames = FALSE
    ))
  }
  
  DT::datatable(
    data_to_display,
    options = list(pageLength = 10, scrollX = TRUE),
    filter = 'top',
    selection = 'multiple',
    rownames = FALSE
  )
})

output$ip_regional_graph <- renderPlotly({
  # Use the reactive filtered data
  
  data_to_display <- cloud %>%
    select(BASIC.REGION, IP.G1.IP, IP.G2.IP, IP.G3.IP, IP.G4.IP, IP.G5.IP, IP.G6.IP, IP.G7.IP, IP.G8.IP, IP.G9.IP, IP.G10.IP, IP.G11, IP.G12) %>% 
    mutate(across(2:last_col(), as.numeric)) %>% 
    mutate(BASIC.REGION = as.character(BASIC.REGION)) %>%
    filter(BASIC.REGION != "") %>% 
    group_by(BASIC.REGION) %>%
    summarise(across(everything(), sum, na.rm = TRUE))  %>% 
    rename(
      "REGION" = BASIC.REGION,
      "GRADE 1" = IP.G1.IP,
      "GRADE 2" = IP.G2.IP,
      "GRADE 3" = IP.G3.IP,
      "GRADE 4" = IP.G4.IP,
      "GRADE 5" = IP.G5.IP,
      "GRADE 6" = IP.G6.IP,
      "GRADE 7" = IP.G7.IP,
      "GRADE 8" = IP.G8.IP,
      "GRADE 9" = IP.G9.IP,
      "GRADE 10" = IP.G10.IP,
      "GRADE 11" = IP.G11,
      "GRADE 12" = IP.G12
    ) %>% 
    pivot_longer(
      cols = 2:last_col(), # Specifies the columns to pivot
      names_to = "IP_Learners", # The new column to hold the original column names
      values_to = "Count" # The new column to hold the values
    )  %>%
    mutate(IP_Learners = factor(IP_Learners, levels = c(
      "GRADE 1", "GRADE 2", "GRADE 3",
      "GRADE 4", "GRADE 5", "GRADE 6",
      "GRADE 7", "GRADE 8", "GRADE 9",
      "GRADE 10", "GRADE 11", "GRADE 12"
    ))) 
  
  current_filtered_data <- data_to_display
  
  # --- Empty Data Handling ---
  if (nrow(current_filtered_data) == 0) {
    return(ggplotly(ggplot() +
                      annotate("text", x = 0.5, y = 0.5, label = "No data for selected regions/divisions") +
                      theme_void()))
  }
  
  # Prepare the data for plotting
  plot_data <- data_to_display
  
  # Create the ggplot
  p <- ggplot(plot_data,
              aes(x = REGION, # Reorder regions based on overall total count for the region
                  y = Count,
                  fill = IP_Learners, # Fill by Clustering.Status for coloring and consistent order
                  text = paste("Region: ", REGION,
                               "<br>Section: ", IP_Learners,
                               "<br>Count: ", scales::comma(Count)))) + # Custom tooltip text
    geom_bar(stat = "identity", position = "dodge", color = "black") + # Changed to position="stack" for stacked bars
    labs(x = "",
         y = "") +
    scale_y_continuous(labels = scales::comma) + # Format y-axis labels as comma-separated numbers
    theme_minimal() +
    theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
          legend.position = "bottom", # Position legend at the bottom
          plot.title = element_text(hjust = 0.5)) # Center the plot title
  
  # Convert ggplot to plotly, ensuring custom text is used for hover
  ggplotly(p, tooltip = "text", source = "cloudenrolmentRegionPlot") %>%
    layout(hoverlabel = list(bgcolor = "white"),
           margin = list(b = 100)) # Increase bottom margin for x-axis labels
})

output$ip_division_graph <- renderPlotly({
  # Use the reactive filtered data
  
  data_to_display <- filtered_cloud_region() %>%
    select(BASIC.DIVISION, IP.G1.IP, IP.G2.IP, IP.G3.IP, IP.G4.IP, IP.G5.IP, IP.G6.IP, IP.G7.IP, IP.G8.IP, IP.G9.IP, IP.G10.IP, IP.G11, IP.G12) %>% 
    mutate(across(2:last_col(), as.numeric)) %>% 
    mutate(BASIC.DIVISION = as.character(BASIC.DIVISION)) %>%
    filter(BASIC.DIVISION != "") %>% 
    group_by(BASIC.DIVISION) %>%
    summarise(across(everything(), sum, na.rm = TRUE))  %>% 
    rename(
      "DIVISION" = BASIC.DIVISION,
      "GRADE 1" = IP.G1.IP,
      "GRADE 2" = IP.G2.IP,
      "GRADE 3" = IP.G3.IP,
      "GRADE 4" = IP.G4.IP,
      "GRADE 5" = IP.G5.IP,
      "GRADE 6" = IP.G6.IP,
      "GRADE 7" = IP.G7.IP,
      "GRADE 8" = IP.G8.IP,
      "GRADE 9" = IP.G9.IP,
      "GRADE 10" = IP.G10.IP,
      "GRADE 11" = IP.G11,
      "GRADE 12" = IP.G12
    ) %>% 
    pivot_longer(
      cols = 2:last_col(), # Specifies the columns to pivot
      names_to = "IP_Learners", # The new column to hold the original column names
      values_to = "Count" # The new column to hold the values
    ) %>%
    mutate(IP_Learners = factor(IP_Learners, levels = c(
      "GRADE 1", "GRADE 2", "GRADE 3",
      "GRADE 4", "GRADE 5", "GRADE 6",
      "GRADE 7", "GRADE 8", "GRADE 9",
      "GRADE 10", "GRADE 11", "GRADE 12"
    ))) 
  
  current_filtered_data <- data_to_display
  
  # --- Empty Data Handling ---
  if (nrow(current_filtered_data) == 0) {
    return(ggplotly(ggplot() +
                      annotate("text", x = 0.5, y = 0.5, label = "No data for selected regions/divisions") +
                      theme_void()))
  }
  
  # Prepare the data for plotting
  plot_data <- data_to_display
  
  # Create the ggplot
  p <- ggplot(plot_data,
              aes(x = IP_Learners, y = Count,
                  fill = IP_Learners,
                  text = paste("Region: ", DIVISION,
                               "<br>Section: ", IP_Learners,
                               "<br>Count: ", scales::comma(Count)))) +
    geom_bar(stat = "identity", position = "dodge", color = "black") +
    scale_y_continuous(labels = scales::comma) +
    theme_minimal() +
    theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
          legend.position = "bottom",
          plot.title = element_text(hjust = 0.5)) +
    facet_wrap(~DIVISION) + coord_flip()+
    labs(x = NULL, y = NULL, fill = NULL)

  
  ggplotly(p, tooltip = "text", source = "cloudenrolmentDivisionPlot") %>%
    layout(hoverlabel = list(bgcolor = "white"),
           margin = list(b = 100)) # Increase bottom margin for x-axis labels
})

output$muslim_data_table <- DT::renderDT({
  
  data_to_display <- filtered_cloud_region() %>%
    select(BASIC.REGION,BASIC.DIVISION, MUSLIM.G1.MUSLIM, MUSLIM.G2.MUSLIM, MUSLIM.G3.MUSLIM, MUSLIM.G4.MUSLIM, MUSLIM.G5.MUSLIM, MUSLIM.G6.MUSLIM, MUSLIM.G7.MUSLIM, MUSLIM.G8.MUSLIM, MUSLIM.G9.MUSLIM, MUSLIM.G10.MUSLIM, MUSLIM.G11, MUSLIM.G12) %>% 
    mutate(across(3:last_col(), as.numeric)) %>% 
    mutate(BASIC.DIVISION = as.character(BASIC.DIVISION)) %>%
    filter(BASIC.DIVISION != "") %>% 
    group_by(BASIC.REGION,BASIC.DIVISION) %>%
    summarise(across(everything(), sum, na.rm = TRUE)) %>% 
    rename(
      "REGION" = BASIC.REGION,
      "DIVISION" = BASIC.DIVISION,
      "GRADE 1" = MUSLIM.G1.MUSLIM,
      "GRADE 2" = MUSLIM.G2.MUSLIM,
      "GRADE 3" = MUSLIM.G3.MUSLIM,
      "GRADE 4" = MUSLIM.G4.MUSLIM,
      "GRADE 5" = MUSLIM.G5.MUSLIM,
      "GRADE 6" = MUSLIM.G6.MUSLIM,
      "GRADE 7" = MUSLIM.G7.MUSLIM,
      "GRADE 8" = MUSLIM.G8.MUSLIM,
      "GRADE 9" = MUSLIM.G9.MUSLIM,
      "GRADE 10" = MUSLIM.G10.MUSLIM,
      "GRADE 11" = MUSLIM.G11,
      "GRADE 12" = MUSLIM.G12
    )
  
  # You might want to add a check for NULL or empty data if filtered_school_data_division()
  # could return such states and you want to display a message or an empty table.
  if (is.null(data_to_display) || nrow(data_to_display) == 0) {
    return(DT::datatable(
      data.frame("Message" = "No data available based on current selection."),
      options = list(dom = 't'), # 't' hides all controls, showing only the table body
      rownames = FALSE
    ))
  }
  
  DT::datatable(
    data_to_display,
    options = list(pageLength = 10, scrollX = TRUE),
    filter = 'top',
    selection = 'multiple',
    rownames = FALSE
  )
})

output$muslim_regional_graph <- renderPlotly({
  # Use the reactive filtered data
  
  data_to_display <- cloud %>%
    select(BASIC.REGION, MUSLIM.G1.MUSLIM, MUSLIM.G2.MUSLIM, MUSLIM.G3.MUSLIM, MUSLIM.G4.MUSLIM, MUSLIM.G5.MUSLIM, MUSLIM.G6.MUSLIM, MUSLIM.G7.MUSLIM, MUSLIM.G8.MUSLIM, MUSLIM.G9.MUSLIM, MUSLIM.G10.MUSLIM, MUSLIM.G11, MUSLIM.G12) %>% 
    mutate(across(2:last_col(), as.numeric)) %>% 
    mutate(BASIC.REGION = as.character(BASIC.REGION)) %>%
    filter(BASIC.REGION != "") %>% 
    group_by(BASIC.REGION) %>%
    summarise(across(everything(), sum, na.rm = TRUE)) %>% 
    rename(
      "REGION" = BASIC.REGION,
      "GRADE 1" = MUSLIM.G1.MUSLIM,
      "GRADE 2" = MUSLIM.G2.MUSLIM,
      "GRADE 3" = MUSLIM.G3.MUSLIM,
      "GRADE 4" = MUSLIM.G4.MUSLIM,
      "GRADE 5" = MUSLIM.G5.MUSLIM,
      "GRADE 6" = MUSLIM.G6.MUSLIM,
      "GRADE 7" = MUSLIM.G7.MUSLIM,
      "GRADE 8" = MUSLIM.G8.MUSLIM,
      "GRADE 9" = MUSLIM.G9.MUSLIM,
      "GRADE 10" = MUSLIM.G10.MUSLIM,
      "GRADE 11" = MUSLIM.G11,
      "GRADE 12" = MUSLIM.G12
    )  %>% 
    pivot_longer(
      cols = 2:last_col(), # Specifies the columns to pivot
      names_to = "Muslim_Learners", # The new column to hold the original column names
      values_to = "Count" # The new column to hold the values
    )  %>%
    mutate(Muslim_Learners = factor(Muslim_Learners, levels = c(
      "GRADE 1", "GRADE 2", "GRADE 3",
      "GRADE 4", "GRADE 5", "GRADE 6",
      "GRADE 7", "GRADE 8", "GRADE 9",
      "GRADE 10", "GRADE 11", "GRADE 12"
    ))) 
  
  current_filtered_data <- data_to_display
  
  # --- Empty Data Handling ---
  if (nrow(current_filtered_data) == 0) {
    return(ggplotly(ggplot() +
                      annotate("text", x = 0.5, y = 0.5, label = "No data for selected regions/divisions") +
                      theme_void()))
  }
  
  # Prepare the data for plotting
  plot_data <- data_to_display
  
  # Create the ggplot
  p <- ggplot(plot_data,
              aes(x = REGION, # Reorder regions based on overall total count for the region
                  y = Count,
                  fill = Muslim_Learners, # Fill by Clustering.Status for coloring and consistent order
                  text = paste("Region: ", REGION,
                               "<br>Section: ", Muslim_Learners,
                               "<br>Count: ", scales::comma(Count)))) + # Custom tooltip text
    geom_bar(stat = "identity", position = "dodge", color = "black") + # Changed to position="stack" for stacked bars
    labs(x = "",
         y = "") +
    scale_y_continuous(labels = scales::comma) + # Format y-axis labels as comma-separated numbers
    theme_minimal() +
    theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
          legend.position = "bottom", # Position legend at the bottom
          plot.title = element_text(hjust = 0.5)) # Center the plot title
  
  # Convert ggplot to plotly, ensuring custom text is used for hover
  ggplotly(p, tooltip = "text", source = "cloudenrolmentRegionPlot") %>%
    layout(hoverlabel = list(bgcolor = "white"),
           margin = list(b = 100)) # Increase bottom margin for x-axis labels
})

output$muslim_division_graph <- renderPlotly({
  # Use the reactive filtered data
  
  data_to_display <- filtered_cloud_region() %>%
    select(BASIC.DIVISION, MUSLIM.G1.MUSLIM, MUSLIM.G2.MUSLIM, MUSLIM.G3.MUSLIM, MUSLIM.G4.MUSLIM, MUSLIM.G5.MUSLIM, MUSLIM.G6.MUSLIM, MUSLIM.G7.MUSLIM, MUSLIM.G8.MUSLIM, MUSLIM.G9.MUSLIM, MUSLIM.G10.MUSLIM, MUSLIM.G11, MUSLIM.G12) %>% 
    mutate(across(2:last_col(), as.numeric)) %>% 
    mutate(BASIC.DIVISION = as.character(BASIC.DIVISION)) %>%
    filter(BASIC.DIVISION != "") %>% 
    group_by(BASIC.DIVISION) %>%
    summarise(across(everything(), sum, na.rm = TRUE)) %>% 
    rename(
      "DIVISION" = BASIC.DIVISION,
      "GRADE 1" = MUSLIM.G1.MUSLIM,
      "GRADE 2" = MUSLIM.G2.MUSLIM,
      "GRADE 3" = MUSLIM.G3.MUSLIM,
      "GRADE 4" = MUSLIM.G4.MUSLIM,
      "GRADE 5" = MUSLIM.G5.MUSLIM,
      "GRADE 6" = MUSLIM.G6.MUSLIM,
      "GRADE 7" = MUSLIM.G7.MUSLIM,
      "GRADE 8" = MUSLIM.G8.MUSLIM,
      "GRADE 9" = MUSLIM.G9.MUSLIM,
      "GRADE 10" = MUSLIM.G10.MUSLIM,
      "GRADE 11" = MUSLIM.G11,
      "GRADE 12" = MUSLIM.G12
    )  %>% 
    pivot_longer(
      cols = 2:last_col(), # Specifies the columns to pivot
      names_to = "Muslim_Learners", # The new column to hold the original column names
      values_to = "Count" # The new column to hold the values
    ) %>%
    mutate(Muslim_Learners = factor(Muslim_Learners, levels = c(
      "GRADE 1", "GRADE 2", "GRADE 3",
      "GRADE 4", "GRADE 5", "GRADE 6",
      "GRADE 7", "GRADE 8", "GRADE 9",
      "GRADE 10", "GRADE 11", "GRADE 12"
    ))) 
  
  current_filtered_data <- data_to_display
  
  # --- Empty Data Handling ---
  if (nrow(current_filtered_data) == 0) {
    return(ggplotly(ggplot() +
                      annotate("text", x = 0.5, y = 0.5, label = "No data for selected regions/divisions") +
                      theme_void()))
  }
  
  # Prepare the data for plotting
  plot_data <- data_to_display
  
  # Create the ggplot
  p <- ggplot(plot_data,
              aes(x = Muslim_Learners, # Reorder regions based on overall total count for the region
                  y = Count,
                  fill = Muslim_Learners, # Fill by Clustering.Status for coloring and consistent order
                  text = paste("Region: ", DIVISION,
                               "<br>Section: ", Muslim_Learners,
                               "<br>Count: ", scales::comma(Count)))) + # Custom tooltip text
    geom_bar(stat = "identity", position = "dodge", color = "black") + # Changed to position="dodge" for dodgeed bars
    scale_y_continuous(labels = scales::comma) + # Format y-axis labels as comma-separated numbers
    theme_minimal() +
    theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
          legend.position = "bottom", # Position legend at the bottom
          plot.title = element_text(hjust = 0.5)) +
    facet_wrap(~DIVISION) + coord_flip()+
    labs(x = NULL, y = NULL, fill = NULL)
# Center the plot title # Center the plot title
  
  # Convert ggplot to plotly, ensuring custom text is used for hover
  ggplotly(p, tooltip = "text", source = "cloudenrolmentDivisionPlot") %>%
    layout(hoverlabel = list(bgcolor = "white"),
           margin = list(b = 100)) # Increase bottom margin for x-axis labels
})

output$displaced_data_table <- DT::renderDT({
  
  data_to_display <- filtered_cloud_region() %>%
    select(BASIC.REGION,BASIC.DIVISION, DISPLACED.GRADE1.DISPLACED.LEARNER, DISPLACED.GRADE2.DISPLACED.LEARNER, DISPLACED.GRADE3.DISPLACED.LEARNER, DISPLACED.GRADE4.DISPLACED.LEARNER, DISPLACED.GRADE5.DISPLACED.LEARNER, DISPLACED.GRADE6.DISPLACED.LEARNER,DISPLACED.G7.DISPLACED,DISPLACED.G8.DISPLACED,DISPLACED.G9.DISPLACED,DISPLACED.G10.DISPLACED,DISPLACED.G11,DISPLACED.G12) %>% 
    mutate(across(3:last_col(), as.numeric)) %>% 
    mutate(BASIC.DIVISION = as.character(BASIC.DIVISION)) %>%
    filter(BASIC.DIVISION != "") %>% 
    group_by(BASIC.REGION,BASIC.DIVISION) %>%
    summarise(across(everything(), sum, na.rm = TRUE)) %>% 
    rename(
      "REGION" = BASIC.REGION,
      "DIVISION" = BASIC.DIVISION,
      "GRADE 1" = DISPLACED.GRADE1.DISPLACED.LEARNER,
      "GRADE 2" = DISPLACED.GRADE2.DISPLACED.LEARNER,
      "GRADE 3" = DISPLACED.GRADE3.DISPLACED.LEARNER,
      "GRADE 4" = DISPLACED.GRADE4.DISPLACED.LEARNER,
      "GRADE 5" = DISPLACED.GRADE5.DISPLACED.LEARNER,
      "GRADE 6" = DISPLACED.GRADE6.DISPLACED.LEARNER,
      "GRADE 7" = DISPLACED.G7.DISPLACED,
      "GRADE 8" = DISPLACED.G8.DISPLACED,
      "GRADE 9" = DISPLACED.G9.DISPLACED,
      "GRADE 10" = DISPLACED.G10.DISPLACED,
      "GRADE 11" = DISPLACED.G11,
      "GRADE 12" = DISPLACED.G12
    )
  
  # You might want to add a check for NULL or empty data if filtered_school_data_division()
  # could return such states and you want to display a message or an empty table.
  if (is.null(data_to_display) || nrow(data_to_display) == 0) {
    return(DT::datatable(
      data.frame("Message" = "No data available based on current selection."),
      options = list(dom = 't'), # 't' hides all controls, showing only the table body
      rownames = FALSE
    ))
  }
  
  DT::datatable(
    data_to_display,
    options = list(pageLength = 10, scrollX = TRUE),
    filter = 'top',
    selection = 'multiple',
    rownames = FALSE
  )
})

output$displaced_regional_graph <- renderPlotly({
  # Use the reactive filtered data
  
  data_to_display <- cloud %>%
    select(BASIC.REGION, DISPLACED.GRADE1.DISPLACED.LEARNER, DISPLACED.GRADE2.DISPLACED.LEARNER, DISPLACED.GRADE3.DISPLACED.LEARNER, DISPLACED.GRADE4.DISPLACED.LEARNER, DISPLACED.GRADE5.DISPLACED.LEARNER, DISPLACED.GRADE6.DISPLACED.LEARNER,DISPLACED.G7.DISPLACED,DISPLACED.G8.DISPLACED,DISPLACED.G9.DISPLACED,DISPLACED.G10.DISPLACED,DISPLACED.G11,DISPLACED.G12) %>% 
    mutate(across(2:last_col(), as.numeric)) %>% 
    mutate(BASIC.REGION = as.character(BASIC.REGION)) %>%
    filter(BASIC.REGION != "") %>% 
    group_by(BASIC.REGION) %>%
    summarise(across(everything(), sum, na.rm = TRUE)) %>% 
    rename(
      "REGION" = BASIC.REGION,
      "GRADE 1" = DISPLACED.GRADE1.DISPLACED.LEARNER,
      "GRADE 2" = DISPLACED.GRADE2.DISPLACED.LEARNER,
      "GRADE 3" = DISPLACED.GRADE3.DISPLACED.LEARNER,
      "GRADE 4" = DISPLACED.GRADE4.DISPLACED.LEARNER,
      "GRADE 5" = DISPLACED.GRADE5.DISPLACED.LEARNER,
      "GRADE 6" = DISPLACED.GRADE6.DISPLACED.LEARNER,
      "GRADE 7" = DISPLACED.G7.DISPLACED,
      "GRADE 8" = DISPLACED.G8.DISPLACED,
      "GRADE 9" = DISPLACED.G9.DISPLACED,
      "GRADE 10" = DISPLACED.G10.DISPLACED,
      "GRADE 11" = DISPLACED.G11,
      "GRADE 12" = DISPLACED.G12
    )  %>% 
    pivot_longer(
      cols = 2:last_col(), # Specifies the columns to pivot
      names_to = "Displaced_Learners", # The new column to hold the original column names
      values_to = "Count" # The new column to hold the values
    )  %>%
    mutate(Displaced_Learners = factor(Displaced_Learners, levels = c(
      "GRADE 1", "GRADE 2", "GRADE 3",
      "GRADE 4", "GRADE 5", "GRADE 6",
      "GRADE 7", "GRADE 8", "GRADE 9",
      "GRADE 10", "GRADE 11", "GRADE 12"
    ))) 
  
  current_filtered_data <- data_to_display
  
  # --- Empty Data Handling ---
  if (nrow(current_filtered_data) == 0) {
    return(ggplotly(ggplot() +
                      annotate("text", x = 0.5, y = 0.5, label = "No data for selected regions/divisions") +
                      theme_void()))
  }
  
  # Prepare the data for plotting
  plot_data <- data_to_display
  
  # Create the ggplot
  p <- ggplot(plot_data,
              aes(x = REGION, # Reorder regions based on overall total count for the region
                  y = Count,
                  fill = Displaced_Learners, # Fill by Clustering.Status for coloring and consistent order
                  text = paste("Region: ", REGION,
                               "<br>Section: ", Displaced_Learners,
                               "<br>Count: ", scales::comma(Count)))) + # Custom tooltip text
    geom_bar(stat = "identity", position = "dodge", color = "black") + # Changed to position="dodge" for dodgeed bars
    labs(x = "Region",
         y = "") +
    scale_y_continuous(labels = scales::comma) + # Format y-axis labels as comma-separated numbers
    theme_minimal() +
    theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
          legend.position = "bottom", # Position legend at the bottom
          plot.title = element_text(hjust = 0.5)) # Center the plot title
  
  # Convert ggplot to plotly, ensuring custom text is used for hover
  ggplotly(p, tooltip = "text", source = "cloudenrolmentRegionPlot") %>%
    layout(hoverlabel = list(bgcolor = "white"),
           margin = list(b = 100)) # Increase bottom margin for x-axis labels
})

output$displaced_division_graph <- renderPlotly({
  # Use the reactive filtered data
  
  data_to_display <- filtered_cloud_region() %>%
    select(BASIC.DIVISION, DISPLACED.GRADE1.DISPLACED.LEARNER, DISPLACED.GRADE2.DISPLACED.LEARNER, DISPLACED.GRADE3.DISPLACED.LEARNER, DISPLACED.GRADE4.DISPLACED.LEARNER, DISPLACED.GRADE5.DISPLACED.LEARNER, DISPLACED.GRADE6.DISPLACED.LEARNER,DISPLACED.G7.DISPLACED,DISPLACED.G8.DISPLACED,DISPLACED.G9.DISPLACED,DISPLACED.G10.DISPLACED,DISPLACED.G11,DISPLACED.G12) %>% 
    mutate(across(2:last_col(), as.numeric)) %>% 
    mutate(BASIC.DIVISION = as.character(BASIC.DIVISION)) %>%
    filter(BASIC.DIVISION != "") %>% 
    group_by(BASIC.DIVISION) %>%
    summarise(across(everything(), sum, na.rm = TRUE)) %>% 
    rename(
      "DIVISION" = BASIC.DIVISION,
      "GRADE 1" = DISPLACED.GRADE1.DISPLACED.LEARNER,
      "GRADE 2" = DISPLACED.GRADE2.DISPLACED.LEARNER,
      "GRADE 3" = DISPLACED.GRADE3.DISPLACED.LEARNER,
      "GRADE 4" = DISPLACED.GRADE4.DISPLACED.LEARNER,
      "GRADE 5" = DISPLACED.GRADE5.DISPLACED.LEARNER,
      "GRADE 6" = DISPLACED.GRADE6.DISPLACED.LEARNER,
      "GRADE 7" = DISPLACED.G7.DISPLACED,
      "GRADE 8" = DISPLACED.G8.DISPLACED,
      "GRADE 9" = DISPLACED.G9.DISPLACED,
      "GRADE 10" = DISPLACED.G10.DISPLACED,
      "GRADE 11" = DISPLACED.G11,
      "GRADE 12" = DISPLACED.G12
    )  %>% 
    pivot_longer(
      cols = 2:last_col(), # Specifies the columns to pivot
      names_to = "Displaced_Learners", # The new column to hold the original column names
      values_to = "Count" # The new column to hold the values
    )  %>%
    mutate(Displaced_Learners = factor(Displaced_Learners, levels = c(
      "GRADE 1", "GRADE 2", "GRADE 3",
      "GRADE 4", "GRADE 5", "GRADE 6",
      "GRADE 7", "GRADE 8", "GRADE 9",
      "GRADE 10", "GRADE 11", "GRADE 12"
    ))) 
  
  current_filtered_data <- data_to_display
  
  # --- Empty Data Handling ---
  if (nrow(current_filtered_data) == 0) {
    return(ggplotly(ggplot() +
                      annotate("text", x = 0.5, y = 0.5, label = "No data for selected regions/divisions") +
                      theme_void()))
  }
  
  # Prepare the data for plotting
  plot_data <- data_to_display
  
  # Create the ggplot
  p <- ggplot(plot_data,
              aes(x = Displaced_Learners, # Reorder regions based on overall total count for the region
                  y = Count,
                  fill = Displaced_Learners, # Fill by Clustering.Status for coloring and consistent order
                  text = paste("Region: ", DIVISION,
                               "<br>Section: ", Displaced_Learners,
                               "<br>Count: ", scales::comma(Count)))) + # Custom tooltip text
    geom_bar(stat = "identity", position = "dodge", color = "black") + # Changed to position="dodge" for dodgeed bars
    scale_y_continuous(labels = scales::comma) + # Format y-axis labels as comma-separated numbers
    theme_minimal() +
    theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
          legend.position = "bottom", # Position legend at the bottom
          plot.title = element_text(hjust = 0.5)) +
    facet_wrap(~DIVISION) + coord_flip()+
    labs(x = NULL, y = NULL, fill = NULL)
# Center the plot title # Center the plot title
  
  # Convert ggplot to plotly, ensuring custom text is used for hover
  ggplotly(p, tooltip = "text", source = "cloudenrolmentDivisionPlot") %>%
    layout(hoverlabel = list(bgcolor = "white"),
           margin = list(b = 100)) # Increase bottom margin for x-axis labels
})

output$als_data_table <- DT::renderDT({
  
  data_to_display <- filtered_cloud_region() %>%
    select(BASIC.REGION,BASIC.DIVISION, contains("ENROLLEDALS.")) %>% 
    mutate(across(3:last_col(), as.numeric)) %>% 
    mutate(BASIC.DIVISION = as.character(BASIC.DIVISION)) %>%
    filter(BASIC.DIVISION != "") %>% 
    group_by(BASIC.REGION,BASIC.DIVISION) %>%
    summarise(across(everything(), sum, na.rm = TRUE)) %>% 
    rename(
      "REGION" = BASIC.REGION,
      "DIVISION" = BASIC.DIVISION,
      "ES" = ENROLLEDALS.ELEM.ALS,
      "JHS" = ENROLLEDALS.JHS.ALS,
      "SHS" = ENROLLEDALS.SHS.ALS
    )
  
  # You might want to add a check for NULL or empty data if filtered_school_data_division()
  # could return such states and you want to display a message or an empty table.
  if (is.null(data_to_display) || nrow(data_to_display) == 0) {
    return(DT::datatable(
      data.frame("Message" = "No data available based on current selection."),
      options = list(dom = 't'), # 't' hides all controls, showing only the table body
      rownames = FALSE
    ))
  }
  
  DT::datatable(
    data_to_display,
    options = list(pageLength = 10, scrollX = TRUE),
    filter = 'top',
    selection = 'multiple',
    rownames = FALSE
  )
})

output$als_regional_graph <- renderPlotly({
  # Use the reactive filtered data
  
  data_to_display <- cloud %>%
    select(BASIC.REGION, contains("ENROLLEDALS.")) %>% 
    mutate(across(2:last_col(), as.numeric)) %>% 
    mutate(BASIC.REGION = as.character(BASIC.REGION)) %>%
    filter(BASIC.REGION != "") %>% 
    group_by(BASIC.REGION) %>%
    summarise(across(everything(), sum, na.rm = TRUE)) %>% 
    rename(
      "REGION" = BASIC.REGION,
      "ES" = ENROLLEDALS.ELEM.ALS,
      "JHS" = ENROLLEDALS.JHS.ALS,
      "SHS" = ENROLLEDALS.SHS.ALS
    )  %>% 
    pivot_longer(
      cols = 2:last_col(), # Specifies the columns to pivot
      names_to = "ALS_Learners", # The new column to hold the original column names
      values_to = "Count" # The new column to hold the values
    )
  
  current_filtered_data <- data_to_display
  
  # --- Empty Data Handling ---
  if (nrow(current_filtered_data) == 0) {
    return(ggplotly(ggplot() +
                      annotate("text", x = 0.5, y = 0.5, label = "No data for selected regions/divisions") +
                      theme_void()))
  }
  
  # Prepare the data for plotting
  plot_data <- data_to_display
  
  # Create the ggplot
  p <- ggplot(plot_data,
              aes(x = REGION, # Reorder regions based on overall total count for the region
                  y = Count,
                  fill = ALS_Learners, # Fill by Clustering.Status for coloring and consistent order
                  text = paste("Region: ", REGION,
                               "<br>Section: ", ALS_Learners,
                               "<br>Count: ", scales::comma(Count)))) + # Custom tooltip text
    geom_bar(stat = "identity", position = "dodge", color = "black") + # Changed to position="dodge" for dodgeed bars
    labs(x = "",
         y = "") +
    scale_y_continuous(labels = scales::comma) + # Format y-axis labels as comma-separated numbers
    theme_minimal() +
    theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
          legend.position = "bottom", # Position legend at the bottom
          plot.title = element_text(hjust = 0.5)) # Center the plot title
  
  # Convert ggplot to plotly, ensuring custom text is used for hover
  ggplotly(p, tooltip = "text", source = "cloudenrolmentRegionPlot") %>%
    layout(hoverlabel = list(bgcolor = "white"),
           margin = list(b = 100)) # Increase bottom margin for x-axis labels
})

output$als_division_graph <- renderPlotly({
  # Use the reactive filtered data
  
  data_to_display <- filtered_cloud_region() %>%
    select(BASIC.DIVISION, contains("ENROLLEDALS.")) %>% 
    mutate(across(2:last_col(), as.numeric)) %>% 
    mutate(BASIC.DIVISION = as.character(BASIC.DIVISION)) %>%
    filter(BASIC.DIVISION != "") %>% 
    group_by(BASIC.DIVISION) %>%
    summarise(across(everything(), sum, na.rm = TRUE)) %>% 
    rename(
      "DIVISION" = BASIC.DIVISION,
      "ES" = ENROLLEDALS.ELEM.ALS,
      "JHS" = ENROLLEDALS.JHS.ALS,
      "SHS" = ENROLLEDALS.SHS.ALS
    )  %>% 
    pivot_longer(
      cols = 2:last_col(), # Specifies the columns to pivot
      names_to = "ALS_Learners", # The new column to hold the original column names
      values_to = "Count" # The new column to hold the values
    )
  
  current_filtered_data <- data_to_display
  
  # --- Empty Data Handling ---
  if (nrow(current_filtered_data) == 0) {
    return(ggplotly(ggplot() +
                      annotate("text", x = 0.5, y = 0.5, label = "No data for selected regions/divisions") +
                      theme_void()))
  }
  
  # Prepare the data for plotting
  plot_data <- data_to_display
  
  # Create the ggplot
  p <- ggplot(plot_data,
              aes(x = ALS_Learners, # Reorder regions based on overall total count for the region
                  y = Count,
                  fill = ALS_Learners, # Fill by Clustering.Status for coloring and consistent order
                  text = paste("Region: ", DIVISION,
                               "<br>Section: ", ALS_Learners,
                               "<br>Count: ", scales::comma(Count)))) + # Custom tooltip text
    geom_bar(stat = "identity", position = "dodge", color = "black") + # Changed to position="dodge" for dodgeed bars
    scale_y_continuous(labels = scales::comma) + # Format y-axis labels as comma-separated numbers
    theme_minimal() +
    theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
          legend.position = "bottom", # Position legend at the bottom
          plot.title = element_text(hjust = 0.5)) +
    facet_wrap(~DIVISION) + coord_flip()+
    labs(x = NULL, y = NULL, fill = NULL)
# Center the plot title # Center the plot title
  
  # Convert ggplot to plotly, ensuring custom text is used for hover
  ggplotly(p, tooltip = "text", source = "cloudenrolmentDivisionPlot") %>%
    layout(hoverlabel = list(bgcolor = "white"),
           margin = list(b = 100)) # Increase bottom margin for x-axis labels
})

output$dropout_data_table <- DT::renderDT({
  
  data_to_display <- filtered_cloud_region() %>%
    select(BASIC.REGION,BASIC.DIVISION, DROPOUT.PREVSY) %>% 
    mutate(across(3:last_col(), as.numeric)) %>% 
    mutate(BASIC.DIVISION = as.character(BASIC.DIVISION)) %>%
    filter(BASIC.DIVISION != "") %>% 
    group_by(BASIC.REGION,BASIC.DIVISION) %>%
    summarise(across(everything(), sum, na.rm = TRUE)) %>% 
    rename(
      "REGION" = BASIC.REGION,
      "DIVISION" = BASIC.DIVISION,
      "NUMBER OF DROPOUTS (PREVIOUS SY)" = DROPOUT.PREVSY)
  
  # You might want to add a check for NULL or empty data if filtered_school_data_division()
  # could return such states and you want to display a message or an empty table.
  if (is.null(data_to_display) || nrow(data_to_display) == 0) {
    return(DT::datatable(
      data.frame("Message" = "No data available based on current selection."),
      options = list(dom = 't'), # 't' hides all controls, showing only the table body
      rownames = FALSE
    ))
  }
  
  DT::datatable(
    data_to_display,
    options = list(pageLength = 10, scrollX = TRUE),
    filter = 'top',
    selection = 'multiple',
    rownames = FALSE
  )
})

output$dropout_regional_graph <- renderPlotly({
  # Use the reactive filtered data
  
  data_to_display <- cloud %>%
    select(BASIC.REGION, DROPOUT.PREVSY) %>% 
    mutate(across(2:last_col(), as.numeric)) %>% 
    mutate(BASIC.REGION = as.character(BASIC.REGION)) %>%
    filter(BASIC.REGION != "") %>% 
    group_by(BASIC.REGION) %>%
    summarise(across(everything(), sum, na.rm = TRUE)) %>% 
    rename(
      "REGION" = BASIC.REGION,
      "NUMBER OF DROPOUTS (PREVIOUS SY)" = DROPOUT.PREVSY) %>% 
    pivot_longer(
      cols = 2:last_col(), # Specifies the columns to pivot
      names_to = "Sections", # The new column to hold the original column names
      values_to = "Count" # The new column to hold the values
    )
  
  current_filtered_data <- data_to_display
  
  # --- Empty Data Handling ---
  if (nrow(current_filtered_data) == 0) {
    return(ggplotly(ggplot() +
                      annotate("text", x = 0.5, y = 0.5, label = "No data for selected regions/divisions") +
                      theme_void()))
  }
  
  # Prepare the data for plotting
  plot_data <- data_to_display
  
  # Create the ggplot
  p <- ggplot(plot_data,
              aes(x = REGION, # Reorder regions based on overall total count for the region
                  y = Count,
                  fill = Sections, # Fill by Clustering.Status for coloring and consistent order
                  text = paste("Region: ", REGION,
                               "<br>Section: ", Sections,
                               "<br>Count: ", scales::comma(Count)))) + # Custom tooltip text
    geom_bar(stat = "identity", position = "dodge", color = "black") + # Changed to position="dodge" for dodgeed bars
    labs(x = "",
         y = "") +
    scale_y_continuous(labels = scales::comma) + # Format y-axis labels as comma-separated numbers
    theme_minimal() +
    theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
          legend.position = "bottom", # Position legend at the bottom
          plot.title = element_text(hjust = 0.5)) # Center the plot title
  
  # Convert ggplot to plotly, ensuring custom text is used for hover
  ggplotly(p, tooltip = "text", source = "cloudenrolmentRegionPlot") %>%
    layout(hoverlabel = list(bgcolor = "white"),
           margin = list(b = 100)) # Increase bottom margin for x-axis labels
})

output$dropout_division_graph <- renderPlotly({
  # Use the reactive filtered data
  
  data_to_display <- filtered_cloud_region() %>%
    select(BASIC.DIVISION, DROPOUT.PREVSY) %>% 
    mutate(across(2:last_col(), as.numeric)) %>% 
    mutate(BASIC.DIVISION = as.character(BASIC.DIVISION)) %>%
    filter(BASIC.DIVISION != "") %>% 
    group_by(BASIC.DIVISION) %>%
    summarise(across(everything(), sum, na.rm = TRUE)) %>% 
    rename(
      "DIVISION" = BASIC.DIVISION,
      "NUMBER OF DROPOUTS (PREVIOUS SY)" = DROPOUT.PREVSY)  %>% 
    pivot_longer(
      cols = 2:last_col(), # Specifies the columns to pivot
      names_to = "Sections", # The new column to hold the original column names
      values_to = "Count" # The new column to hold the values
    )
  
  current_filtered_data <- data_to_display
  
  # --- Empty Data Handling ---
  if (nrow(current_filtered_data) == 0) {
    return(ggplotly(ggplot() +
                      annotate("text", x = 0.5, y = 0.5, label = "No data for selected regions/divisions") +
                      theme_void()))
  }
  
  # Prepare the data for plotting
  plot_data <- data_to_display
  
  # Create the ggplot
  p <- ggplot(plot_data,
              aes(x = Sections, # Reorder regions based on overall total count for the region
                  y = Count,
                  fill = Sections, # Fill by Clustering.Status for coloring and consistent order
                  text = paste("Region: ", DIVISION,
                               "<br>Section: ", Sections,
                               "<br>Count: ", scales::comma(Count)))) + # Custom tooltip text
    geom_bar(stat = "identity", position = "dodge", color = "black") + # Changed to position="dodge" for dodgeed bars
    scale_y_continuous(labels = scales::comma) + # Format y-axis labels as comma-separated numbers
    theme_minimal() +
    theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
          legend.position = "bottom", # Position legend at the bottom
          plot.title = element_text(hjust = 0.5)) +
    facet_wrap(~DIVISION) + coord_flip()+
    labs(x = NULL, y = NULL, fill = NULL)
# Center the plot title # Center the plot title
  
  # Convert ggplot to plotly, ensuring custom text is used for hover
  ggplotly(p, tooltip = "text", source = "cloudenrolmentDivisionPlot") %>%
    layout(hoverlabel = list(bgcolor = "white"),
           margin = list(b = 100)) # Increase bottom margin for x-axis labels
})

output$sosss_data_table <- DT::renderDT({
  
  data_to_display <- filtered_cloud_region() %>%
    select(BASIC.REGION,BASIC.DIVISION, contains("SOSSS.")) %>% 
    mutate(across(3:last_col(), as.numeric)) %>% 
    mutate(BASIC.DIVISION = as.character(BASIC.DIVISION)) %>%
    filter(BASIC.DIVISION != "") %>% 
    group_by(BASIC.REGION,BASIC.DIVISION) %>%
    summarise(across(everything(), sum, na.rm = TRUE))
  
  # You might want to add a check for NULL or empty data if filtered_school_data_division()
  # could return such states and you want to display a message or an empty table.
  if (is.null(data_to_display) || nrow(data_to_display) == 0) {
    return(DT::datatable(
      data.frame("Message" = "No data available based on current selection."),
      options = list(dom = 't'), # 't' hides all controls, showing only the table body
      rownames = FALSE
    ))
  }
  
  DT::datatable(
    data_to_display,
    options = list(pageLength = 10, scrollX = TRUE),
    filter = 'top',
    selection = 'multiple',
    rownames = FALSE
  )
})

output$sosss_regional_graph <- renderPlotly({
  # Use the reactive filtered data
  
  data_to_display <- cloud %>%
    select(BASIC.REGION, contains("SOSSS.")) %>% 
    mutate(across(2:last_col(), as.numeric)) %>% 
    mutate(BASIC.REGION = as.character(BASIC.REGION)) %>%
    filter(BASIC.REGION != "") %>% 
    group_by(BASIC.REGION) %>%
    summarise(across(everything(), sum, na.rm = TRUE))  %>% 
    pivot_longer(
      cols = 2:last_col(), # Specifies the columns to pivot
      names_to = "Sections", # The new column to hold the original column names
      values_to = "Count" # The new column to hold the values
    )
  
  current_filtered_data <- data_to_display
  
  # --- Empty Data Handling ---
  if (nrow(current_filtered_data) == 0) {
    return(ggplotly(ggplot() +
                      annotate("text", x = 0.5, y = 0.5, label = "No data for selected regions/divisions") +
                      theme_void()))
  }
  
  # Prepare the data for plotting
  plot_data <- data_to_display
  
  # Create the ggplot
  p <- ggplot(plot_data,
              aes(x = BASIC.REGION, # Reorder regions based on overall total count for the region
                  y = Count,
                  fill = Sections, # Fill by Clustering.Status for coloring and consistent order
                  text = paste("Region: ", BASIC.REGION,
                               "<br>Section: ", Sections,
                               "<br>Count: ", scales::comma(Count)))) + # Custom tooltip text
    geom_bar(stat = "identity", position = "dodge", color = "black") + # Changed to position="dodge" for dodgeed bars
    labs(x = "",
         y = "") +
    scale_y_continuous(labels = scales::comma) + # Format y-axis labels as comma-separated numbers
    theme_minimal() +
    theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
          legend.position = "bottom", # Position legend at the bottom
          plot.title = element_text(hjust = 0.5)) # Center the plot title
  
  # Convert ggplot to plotly, ensuring custom text is used for hover
  ggplotly(p, tooltip = "text", source = "cloudenrolmentRegionPlot") %>%
    layout(hoverlabel = list(bgcolor = "white"),
           margin = list(b = 100)) # Increase bottom margin for x-axis labels
})

output$sosss_division_graph <- renderPlotly({
  # Use the reactive filtered data
  
  data_to_display <- filtered_cloud_region() %>%
    select(BASIC.DIVISION, contains("SOSSS.")) %>% 
    mutate(across(2:last_col(), as.numeric)) %>% 
    mutate(BASIC.DIVISION = as.character(BASIC.DIVISION)) %>%
    filter(BASIC.DIVISION != "") %>% 
    group_by(BASIC.DIVISION) %>%
    summarise(across(everything(), sum, na.rm = TRUE))  %>% 
    pivot_longer(
      cols = 2:last_col(), # Specifies the columns to pivot
      names_to = "Sections", # The new column to hold the original column names
      values_to = "Count" # The new column to hold the values
    )
  
  current_filtered_data <- data_to_display
  
  # --- Empty Data Handling ---
  if (nrow(current_filtered_data) == 0) {
    return(ggplotly(ggplot() +
                      annotate("text", x = 0.5, y = 0.5, label = "No data for selected regions/divisions") +
                      theme_void()))
  }
  
  # Prepare the data for plotting
  plot_data <- data_to_display
  
  # Create the ggplot
  p <- ggplot(plot_data,
              aes(x = Sections, # Reorder regions based on overall total count for the region
                  y = Count,
                  fill = Sections, # Fill by Clustering.Status for coloring and consistent order
                  text = paste("Region: ", BASIC.DIVISION,
                               "<br>Section: ", Sections,
                               "<br>Count: ", scales::comma(Count)))) + # Custom tooltip text
    geom_bar(stat = "identity", position = "dodge", color = "black") + # Changed to position="dodge" for dodgeed bars
    labs(x = "",
         y = "") +
    scale_y_continuous(labels = scales::comma) + # Format y-axis labels as comma-separated numbers
    theme_minimal() +
    theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
          legend.position = "bottom", # Position legend at the bottom
          plot.title = element_text(hjust = 0.5)) +
    facet_wrap(~BASIC.DIVISION)# Center the plot title # Center the plot title
  
  # Convert ggplot to plotly, ensuring custom text is used for hover
  ggplotly(p, tooltip = "text", source = "cloudenrolmentDivisionPlot") %>%
    layout(hoverlabel = list(bgcolor = "white"),
           margin = list(b = 100)) # Increase bottom margin for x-axis labels
})

output$extension_data_table <- DT::renderDT({
  
  data_to_display <- filtered_cloud_region() %>%
    select(BASIC.REGION,BASIC.DIVISION, contains("EXTENSION.")) %>% 
    mutate(across(3:last_col(), as.numeric)) %>% 
    mutate(BASIC.DIVISION = as.character(BASIC.DIVISION)) %>%
    filter(BASIC.DIVISION != "") %>% 
    group_by(BASIC.REGION,BASIC.DIVISION) %>%
    summarise(across(everything(), sum, na.rm = TRUE))
  
  # You might want to add a check for NULL or empty data if filtered_school_data_division()
  # could return such states and you want to display a message or an empty table.
  if (is.null(data_to_display) || nrow(data_to_display) == 0) {
    return(DT::datatable(
      data.frame("Message" = "No data available based on current selection."),
      options = list(dom = 't'), # 't' hides all controls, showing only the table body
      rownames = FALSE
    ))
  }
  
  DT::datatable(
    data_to_display,
    options = list(pageLength = 10, scrollX = TRUE),
    filter = 'top',
    selection = 'multiple',
    rownames = FALSE
  )
})

output$extension_regional_graph <- renderPlotly({
  # Use the reactive filtered data
  
  data_to_display <- cloud %>%
    select(BASIC.REGION, contains("EXTENSION.")) %>% 
    mutate(across(2:last_col(), as.numeric)) %>% 
    mutate(BASIC.REGION = as.character(BASIC.REGION)) %>%
    filter(BASIC.REGION != "") %>% 
    group_by(BASIC.REGION) %>%
    summarise(across(everything(), sum, na.rm = TRUE))  %>% 
    pivot_longer(
      cols = 2:last_col(), # Specifies the columns to pivot
      names_to = "Sections", # The new column to hold the original column names
      values_to = "Count" # The new column to hold the values
    )
  
  current_filtered_data <- data_to_display
  
  # --- Empty Data Handling ---
  if (nrow(current_filtered_data) == 0) {
    return(ggplotly(ggplot() +
                      annotate("text", x = 0.5, y = 0.5, label = "No data for selected regions/divisions") +
                      theme_void()))
  }
  
  # Prepare the data for plotting
  plot_data <- data_to_display
  
  # Create the ggplot
  p <- ggplot(plot_data,
              aes(x = BASIC.REGION, # Reorder regions based on overall total count for the region
                  y = Count,
                  fill = Sections, # Fill by Clustering.Status for coloring and consistent order
                  text = paste("Region: ", BASIC.REGION,
                               "<br>Section: ", Sections,
                               "<br>Count: ", scales::comma(Count)))) + # Custom tooltip text
    geom_bar(stat = "identity", position = "dodge", color = "black") + # Changed to position="dodge" for dodgeed bars
    labs(x = "",
         y = "") +
    scale_y_continuous(labels = scales::comma) + # Format y-axis labels as comma-separated numbers
    theme_minimal() +
    theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
          legend.position = "bottom", # Position legend at the bottom
          plot.title = element_text(hjust = 0.5)) # Center the plot title
  
  # Convert ggplot to plotly, ensuring custom text is used for hover
  ggplotly(p, tooltip = "text", source = "cloudenrolmentRegionPlot") %>%
    layout(hoverlabel = list(bgcolor = "white"),
           margin = list(b = 100)) # Increase bottom margin for x-axis labels
})

output$extension_division_graph <- renderPlotly({
  # Use the reactive filtered data
  
  data_to_display <- filtered_cloud_region() %>%
    select(BASIC.DIVISION, contains("EXTENSION.")) %>% 
    mutate(across(2:last_col(), as.numeric)) %>% 
    mutate(BASIC.DIVISION = as.character(BASIC.DIVISION)) %>%
    filter(BASIC.DIVISION != "") %>% 
    group_by(BASIC.DIVISION) %>%
    summarise(across(everything(), sum, na.rm = TRUE))  %>% 
    pivot_longer(
      cols = 2:last_col(), # Specifies the columns to pivot
      names_to = "Sections", # The new column to hold the original column names
      values_to = "Count" # The new column to hold the values
    )
  
  current_filtered_data <- data_to_display
  
  # --- Empty Data Handling ---
  if (nrow(current_filtered_data) == 0) {
    return(ggplotly(ggplot() +
                      annotate("text", x = 0.5, y = 0.5, label = "No data for selected regions/divisions") +
                      theme_void()))
  }
  
  # Prepare the data for plotting
  plot_data <- data_to_display
  
  # Create the ggplot
  p <- ggplot(plot_data,
              aes(x = Sections, # Reorder regions based on overall total count for the region
                  y = Count,
                  fill = Sections, # Fill by Clustering.Status for coloring and consistent order
                  text = paste("Region: ", BASIC.DIVISION,
                               "<br>Section: ", Sections,
                               "<br>Count: ", scales::comma(Count)))) + # Custom tooltip text
    geom_bar(stat = "identity", position = "dodge", color = "black") + # Changed to position="dodge" for dodgeed bars
    labs(x = "",
         y = "") +
    scale_y_continuous(labels = scales::comma) + # Format y-axis labels as comma-separated numbers
    theme_minimal() +
    theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
          legend.position = "bottom", # Position legend at the bottom
          plot.title = element_text(hjust = 0.5)) +
    facet_wrap(~BASIC.DIVISION)# Center the plot title # Center the plot title
  
  # Convert ggplot to plotly, ensuring custom text is used for hover
  ggplotly(p, tooltip = "text", source = "cloudenrolmentDivisionPlot") %>%
    layout(hoverlabel = list(bgcolor = "white"),
           margin = list(b = 100)) # Increase bottom margin for x-axis labels
})


output$teacherinventory_data_table <- DT::renderDT({
  
  data_to_display <- filtered_cloud_region() %>%
    select(BASIC.REGION,BASIC.DIVISION, TEACHING.PERSONNEL.ELEM, TEACHING.PERSONNEL.JHS, TEACHING.PERSONNEL.SHS) %>% 
    mutate(across(3:last_col(), as.numeric)) %>% 
    mutate(BASIC.DIVISION = as.character(BASIC.DIVISION)) %>%
    filter(BASIC.DIVISION != "") %>% 
    group_by(BASIC.REGION,BASIC.DIVISION) %>%
    summarise(across(everything(), sum, na.rm = TRUE)) %>% 
    rename(
      "REGION" = BASIC.REGION,
      "DIVISION" = BASIC.DIVISION,
      "ES" = TEACHING.PERSONNEL.ELEM,
      "JHS" = TEACHING.PERSONNEL.JHS,
      "SHS" = TEACHING.PERSONNEL.SHS
    )
  
  # You might want to add a check for NULL or empty data if filtered_school_data_division()
  # could return such states and you want to display a message or an empty table.
  if (is.null(data_to_display) || nrow(data_to_display) == 0) {
    return(DT::datatable(
      data.frame("Message" = "No data available based on current selection."),
      options = list(dom = 't'), # 't' hides all controls, showing only the table body
      rownames = FALSE
    ))
  }
  
  DT::datatable(
    data_to_display,
    options = list(pageLength = 10, scrollX = TRUE),
    filter = 'top',
    selection = 'multiple',
    rownames = FALSE
  )
})

output$teacherinventory_regional_graph <- renderPlotly({
  # Use the reactive filtered data
  
  data_to_display <- cloud %>%
    select(BASIC.REGION, TEACHING.PERSONNEL.ELEM, TEACHING.PERSONNEL.JHS, TEACHING.PERSONNEL.SHS) %>% 
    mutate(across(2:last_col(), as.numeric)) %>% 
    mutate(BASIC.REGION = as.character(BASIC.REGION)) %>%
    filter(BASIC.REGION != "") %>% 
    group_by(BASIC.REGION) %>%
    summarise(across(everything(), sum, na.rm = TRUE)) %>% 
    rename(
      "REGION" = BASIC.REGION,
      "ES" = TEACHING.PERSONNEL.ELEM,
      "JHS" = TEACHING.PERSONNEL.JHS,
      "SHS" = TEACHING.PERSONNEL.SHS
    )  %>% 
    pivot_longer(
      cols = 2:last_col(), # Specifies the columns to pivot
      names_to = "Number_of_Teaching_Personnel", # The new column to hold the original column names
      values_to = "Count" # The new column to hold the values
    )
  
  current_filtered_data <- data_to_display
  
  # --- Empty Data Handling ---
  if (nrow(current_filtered_data) == 0) {
    return(ggplotly(ggplot() +
                      annotate("text", x = 0.5, y = 0.5, label = "No data for selected regions/divisions") +
                      theme_void()))
  }
  
  # Prepare the data for plotting
  plot_data <- data_to_display
  
  # Create the ggplot
  p <- ggplot(plot_data,
              aes(x = REGION, # Reorder regions based on overall total count for the region
                  y = Count,
                  fill = Number_of_Teaching_Personnel, # Fill by Clustering.Status for coloring and consistent order
                  text = paste("Region: ", REGION,
                               "<br>Section: ", Number_of_Teaching_Personnel,
                               "<br>Count: ", scales::comma(Count)))) + # Custom tooltip text
    geom_bar(stat = "identity", position = "dodge", color = "black") + # Changed to position="dodge" for dodgeed bars
    labs(x = "",
         y = "") +
    scale_y_continuous(labels = scales::comma) + # Format y-axis labels as comma-separated numbers
    theme_minimal() +
    theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
          legend.position = "bottom", # Position legend at the bottom
          plot.title = element_text(hjust = 0.5)) # Center the plot title
  
  # Convert ggplot to plotly, ensuring custom text is used for hover
  ggplotly(p, tooltip = "text", source = "cloudenrolmentRegionPlot") %>%
    layout(hoverlabel = list(bgcolor = "white"),
           margin = list(b = 100)) # Increase bottom margin for x-axis labels
})

output$teacherinventory_division_graph <- renderPlotly({
  # Use the reactive filtered data
  
  data_to_display <- filtered_cloud_region() %>%
    select(BASIC.DIVISION, TEACHING.PERSONNEL.ELEM, TEACHING.PERSONNEL.JHS, TEACHING.PERSONNEL.SHS) %>% 
    mutate(across(2:last_col(), as.numeric)) %>% 
    mutate(BASIC.DIVISION = as.character(BASIC.DIVISION)) %>%
    filter(BASIC.DIVISION != "") %>% 
    group_by(BASIC.DIVISION) %>%
    summarise(across(everything(), sum, na.rm = TRUE)) %>% 
    rename(
      "DIVISION" = BASIC.DIVISION,
      "ES" = TEACHING.PERSONNEL.ELEM,
      "JHS" = TEACHING.PERSONNEL.JHS,
      "SHS" = TEACHING.PERSONNEL.SHS
    )  %>% 
    pivot_longer(
      cols = 2:last_col(), # Specifies the columns to pivot
      names_to = "Number_of_Teaching_Personnel", # The new column to hold the original column names
      values_to = "Count" # The new column to hold the values
    )
  
  current_filtered_data <- data_to_display
  
  # --- Empty Data Handling ---
  if (nrow(current_filtered_data) == 0) {
    return(ggplotly(ggplot() +
                      annotate("text", x = 0.5, y = 0.5, label = "No data for selected regions/divisions") +
                      theme_void()))
  }
  
  # Prepare the data for plotting
  plot_data <- data_to_display
  
  # Create the ggplot
  p <- ggplot(plot_data,
              aes(x = Number_of_Teaching_Personnel, # Reorder regions based on overall total count for the region
                  y = Count,
                  fill = Number_of_Teaching_Personnel, # Fill by Clustering.Status for coloring and consistent order
                  text = paste("Region: ", DIVISION,
                               "<br>Section: ", Number_of_Teaching_Personnel,
                               "<br>Count: ", scales::comma(Count)))) + # Custom tooltip text
    geom_bar(stat = "identity", position = "dodge", color = "black") + # Changed to position="dodge" for dodgeed bars
    scale_y_continuous(labels = scales::comma) + # Format y-axis labels as comma-separated numbers
    theme_minimal() +
    theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
          legend.position = "bottom", # Position legend at the bottom
          plot.title = element_text(hjust = 0.5)) +
    facet_wrap(~DIVISION) + coord_flip()+
    labs(x = NULL, y = NULL, fill = NULL)
# Center the plot title # Center the plot title
  
  # Convert ggplot to plotly, ensuring custom text is used for hover
  ggplotly(p, tooltip = "text", source = "cloudenrolmentDivisionPlot") %>%
    layout(hoverlabel = list(bgcolor = "white"),
           margin = list(b = 100)) # Increase bottom margin for x-axis labels
})

output$classroom_data_table <- DT::renderDT({
  
  data_to_display <- filtered_cloud_region() %>%
    select(BASIC.REGION,BASIC.DIVISION, NO.CLASSROOM.ELEM, NO.CLASSROOM.SNED.ELEM, NO.CLASSROOM.JHS, NO.CLASSROOM.SNED.JHS, NO.CLASSROOM.SHS) %>% 
    mutate(across(3:last_col(), as.numeric)) %>% 
    mutate(BASIC.DIVISION = as.character(BASIC.DIVISION)) %>%
    filter(BASIC.DIVISION != "") %>% 
    group_by(BASIC.REGION,BASIC.DIVISION) %>%
    summarise(across(everything(), sum, na.rm = TRUE)) %>% 
    rename(
      "REGION" = BASIC.REGION,
      "DIVISION" = BASIC.DIVISION,
      "ES" = NO.CLASSROOM.ELEM,
      "JHS" = NO.CLASSROOM.JHS,
      "SHS" = NO.CLASSROOM.SHS,
      "SNED-ES" = NO.CLASSROOM.SNED.ELEM,
      "SNED-JHS" = NO.CLASSROOM.SNED.JHS)
  
  # You might want to add a check for NULL or empty data if filtered_school_data_division()
  # could return such states and you want to display a message or an empty table.
  if (is.null(data_to_display) || nrow(data_to_display) == 0) {
    return(DT::datatable(
      data.frame("Message" = "No data available based on current selection."),
      options = list(dom = 't'), # 't' hides all controls, showing only the table body
      rownames = FALSE
    ))
  }
  
  DT::datatable(
    data_to_display,
    options = list(pageLength = 10, scrollX = TRUE),
    filter = 'top',
    selection = 'multiple',
    rownames = FALSE
  )
})

output$classroom_regional_graph <- renderPlotly({
  # Use the reactive filtered data
  
  data_to_display <- cloud %>%
    select(BASIC.REGION, NO.CLASSROOM.ELEM, NO.CLASSROOM.SNED.ELEM, NO.CLASSROOM.JHS, NO.CLASSROOM.SNED.JHS, NO.CLASSROOM.SHS) %>% 
    mutate(across(2:last_col(), as.numeric)) %>% 
    mutate(BASIC.REGION = as.character(BASIC.REGION)) %>%
    filter(BASIC.REGION != "") %>% 
    group_by(BASIC.REGION) %>%
    summarise(across(everything(), sum, na.rm = TRUE)) %>% 
    rename(
      "REGION" = BASIC.REGION,
      "ES" = NO.CLASSROOM.ELEM,
      "JHS" = NO.CLASSROOM.JHS,
      "SHS" = NO.CLASSROOM.SHS,
      "SNED-ES" = NO.CLASSROOM.SNED.ELEM,
      "SNED-JHS" = NO.CLASSROOM.SNED.JHS)  %>% 
    pivot_longer(
      cols = 2:last_col(), # Specifies the columns to pivot
      names_to = "Number_of_Classrooms", # The new column to hold the original column names
      values_to = "Count" # The new column to hold the values
    )
  
  current_filtered_data <- data_to_display
  
  # --- Empty Data Handling ---
  if (nrow(current_filtered_data) == 0) {
    return(ggplotly(ggplot() +
                      annotate("text", x = 0.5, y = 0.5, label = "No data for selected regions/divisions") +
                      theme_void()))
  }
  
  # Prepare the data for plotting
  plot_data <- data_to_display
  
  # Create the ggplot
  p <- ggplot(plot_data,
              aes(x = REGION, # Reorder regions based on overall total count for the region
                  y = Count,
                  fill = Number_of_Classrooms, # Fill by Clustering.Status for coloring and consistent order
                  text = paste("Region: ", REGION,
                               "<br>Section: ", Number_of_Classrooms,
                               "<br>Count: ", scales::comma(Count)))) + # Custom tooltip text
    geom_bar(stat = "identity", position = "dodge", color = "black") + # Changed to position="dodge" for dodgeed bars
    labs(x = "",
         y = "") +
    scale_y_continuous(labels = scales::comma) + # Format y-axis labels as comma-separated numbers
    theme_minimal() +
    theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
          legend.position = "bottom", # Position legend at the bottom
          plot.title = element_text(hjust = 0.5))
  
  # Convert ggplot to plotly, ensuring custom text is used for hover
  ggplotly(p, tooltip = "text", source = "cloudenrolmentRegionPlot") %>%
    layout(hoverlabel = list(bgcolor = "white"),
           margin = list(b = 100)) # Increase bottom margin for x-axis labels
})

output$classroom_division_graph <- renderPlotly({
  # Use the reactive filtered data
  
  data_to_display <- filtered_cloud_region() %>%
    select(BASIC.DIVISION, contains("NO.CLASSROOM.")) %>% 
    mutate(across(2:last_col(), as.numeric)) %>% 
    mutate(BASIC.DIVISION = as.character(BASIC.DIVISION)) %>%
    filter(BASIC.DIVISION != "") %>% 
    group_by(BASIC.DIVISION) %>%
    summarise(across(everything(), sum, na.rm = TRUE)) %>% 
    rename(
      "DIVISION" = BASIC.DIVISION,
      "ES" = NO.CLASSROOM.ELEM,
      "JHS" = NO.CLASSROOM.JHS,
      "SHS" = NO.CLASSROOM.SHS,
      "SNED-ES" = NO.CLASSROOM.SNED.ELEM,
      "SNED-JHS" = NO.CLASSROOM.SNED.JHS)  %>% 
    pivot_longer(
      cols = 2:last_col(), # Specifies the columns to pivot
      names_to = "Number_of_Classrooms", # The new column to hold the original column names
      values_to = "Count" # The new column to hold the values
    )
  
  current_filtered_data <- data_to_display
  
  # --- Empty Data Handling ---
  if (nrow(current_filtered_data) == 0) {
    return(ggplotly(ggplot() +
                      annotate("text", x = 0.5, y = 0.5, label = "No data for selected regions/divisions") +
                      theme_void()))
  }
  
  # Prepare the data for plotting
  plot_data <- data_to_display
  
  # Create the ggplot
  p <- ggplot(plot_data,
              aes(x = Number_of_Classrooms, # Reorder regions based on overall total count for the region
                  y = Count,
                  fill = Number_of_Classrooms, # Fill by Clustering.Status for coloring and consistent order
                  text = paste("Region: ", DIVISION,
                               "<br>Section: ", Number_of_Classrooms,
                               "<br>Count: ", scales::comma(Count)))) + # Custom tooltip text
    geom_bar(stat = "identity", position = "dodge", color = "black") + # Changed to position="dodge" for dodgeed bars
    scale_y_continuous(labels = scales::comma) + # Format y-axis labels as comma-separated numbers
    theme_minimal() +
    theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
          legend.position = "bottom", # Position legend at the bottom
          plot.title = element_text(hjust = 0.5)) +
    facet_wrap(~DIVISION) + coord_flip()+
    labs(x = NULL, y = NULL, fill = NULL)
# Center the plot title # Center the plot title
  
  # Convert ggplot to plotly, ensuring custom text is used for hover
  ggplotly(p, tooltip = "text", source = "cloudenrolmentDivisionPlot") %>%
    layout(hoverlabel = list(bgcolor = "white"),
           margin = list(b = 100)) # Increase bottom margin for x-axis labels
})

output$multigrade_data_table <- DT::renderDT({
  
  data_to_display <- filtered_cloud_region() %>%
    select(BASIC.REGION,BASIC.DIVISION, contains("MG.")) %>% 
    mutate(across(3:last_col(), as.numeric)) %>% 
    mutate(BASIC.DIVISION = as.character(BASIC.DIVISION)) %>%
    filter(BASIC.DIVISION != "") %>% 
    group_by(BASIC.REGION,BASIC.DIVISION) %>%
    summarise(across(everything(), sum, na.rm = TRUE)) %>% 
    rename(
      "REGION" = BASIC.REGION,
      "DIVISION" = BASIC.DIVISION,
      "G1 & G2" = MG.CLASSES.1.2,
      "G3 & G4" = MG.CLASSES.3.4,
      "G5 & G6" = MG.CLASSES.5.6
    )
  
  # You might want to add a check for NULL or empty data if filtered_school_data_division()
  # could return such states and you want to display a message or an empty table.
  if (is.null(data_to_display) || nrow(data_to_display) == 0) {
    return(DT::datatable(
      data.frame("Message" = "No data available based on current selection."),
      options = list(dom = 't'), # 't' hides all controls, showing only the table body
      rownames = FALSE
    ))
  }
  
  DT::datatable(
    data_to_display,
    options = list(pageLength = 10, scrollX = TRUE),
    filter = 'top',
    selection = 'multiple',
    rownames = FALSE
  )
})

output$multigrade_regional_graph <- renderPlotly({
  # Use the reactive filtered data
  
  data_to_display <- cloud %>%
    select(BASIC.REGION, contains("MG.")) %>% 
    mutate(across(2:last_col(), as.numeric)) %>% 
    mutate(BASIC.REGION = as.character(BASIC.REGION)) %>%
    filter(BASIC.REGION != "") %>% 
    group_by(BASIC.REGION) %>%
    summarise(across(everything(), sum, na.rm = TRUE)) %>% 
    rename(
      "REGION" = BASIC.REGION,
      "G1 & G2" = MG.CLASSES.1.2,
      "G3 & G4" = MG.CLASSES.3.4,
      "G5 & G6" = MG.CLASSES.5.6
    )  %>% 
    pivot_longer(
      cols = 2:last_col(), # Specifies the columns to pivot
      names_to = "Multigrade", # The new column to hold the original column names
      values_to = "Count" # The new column to hold the values
    )
  
  current_filtered_data <- data_to_display
  
  # --- Empty Data Handling ---
  if (nrow(current_filtered_data) == 0) {
    return(ggplotly(ggplot() +
                      annotate("text", x = 0.5, y = 0.5, label = "No data for selected regions/divisions") +
                      theme_void()))
  }
  
  # Prepare the data for plotting
  plot_data <- data_to_display
  
  # Create the ggplot
  p <- ggplot(plot_data,
              aes(x = REGION, # Reorder regions based on overall total count for the region
                  y = Count,
                  fill = Multigrade, # Fill by Clustering.Status for coloring and consistent order
                  text = paste("Region: ", REGION,
                               "<br>Section: ", Multigrade,
                               "<br>Count: ", scales::comma(Count)))) + # Custom tooltip text
    geom_bar(stat = "identity", position = "dodge", color = "black") + # Changed to position="dodge" for dodgeed bars
    labs(x = "",
         y = "") +
    scale_y_continuous(labels = scales::comma) + # Format y-axis labels as comma-separated numbers
    theme_minimal() +
    theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
          legend.position = "bottom", # Position legend at the bottom
          plot.title = element_text(hjust = 0.5)) # Center the plot title
  
  # Convert ggplot to plotly, ensuring custom text is used for hover
  ggplotly(p, tooltip = "text", source = "cloudenrolmentRegionPlot") %>%
    layout(hoverlabel = list(bgcolor = "white"),
           margin = list(b = 100)) # Increase bottom margin for x-axis labels
})

output$multigrade_division_graph <- renderPlotly({
  # Use the reactive filtered data
  
  data_to_display <- filtered_cloud_region() %>%
    select(BASIC.DIVISION, contains("MG.")) %>% 
    mutate(across(2:last_col(), as.numeric)) %>% 
    mutate(BASIC.DIVISION = as.character(BASIC.DIVISION)) %>%
    filter(BASIC.DIVISION != "") %>% 
    group_by(BASIC.DIVISION) %>%
    summarise(across(everything(), sum, na.rm = TRUE)) %>% 
    rename(
      "DIVISION" = BASIC.DIVISION,
      "G1 & G2" = MG.CLASSES.1.2,
      "G3 & G4" = MG.CLASSES.3.4,
      "G5 & G6" = MG.CLASSES.5.6
    ) %>% 
    pivot_longer(
      cols = 2:last_col(), # Specifies the columns to pivot
      names_to = "Multigrade", # The new column to hold the original column names
      values_to = "Count" # The new column to hold the values
    )
  
  current_filtered_data <- data_to_display
  
  # --- Empty Data Handling ---
  if (nrow(current_filtered_data) == 0) {
    return(ggplotly(ggplot() +
                      annotate("text", x = 0.5, y = 0.5, label = "No data for selected regions/divisions") +
                      theme_void()))
  }
  
  # Prepare the data for plotting
  plot_data <- data_to_display
  
  # Create the ggplot
  p <- ggplot(plot_data,
              aes(x = Multigrade, # Reorder regions based on overall total count for the region
                  y = Count,
                  fill = Multigrade, # Fill by Clustering.Status for coloring and consistent order
                  text = paste("Region: ", DIVISION,
                               "<br>Section: ", Multigrade,
                               "<br>Count: ", scales::comma(Count)))) + # Custom tooltip text
    geom_bar(stat = "identity", position = "dodge", color = "black") + # Changed to position="dodge" for dodgeed bars
    scale_y_continuous(labels = scales::comma) + # Format y-axis labels as comma-separated numbers
    theme_minimal() +
    theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
          legend.position = "bottom", # Position legend at the bottom
          plot.title = element_text(hjust = 0.5)) +
    facet_wrap(~DIVISION) +
    coord_flip()+
    labs(x = NULL, y = NULL, fill = NULL) # Center the plot title # Center the plot title
  
  # Convert ggplot to plotly, ensuring custom text is used for hover
  ggplotly(p, tooltip = "text", source = "cloudenrolmentDivisionPlot") %>%
    layout(hoverlabel = list(bgcolor = "white"),
           margin = list(b = 100)) # Increase bottom margin for x-axis labels
})

output$organizedclass_data_table <- DT::renderDT({
  
  data_to_display <- filtered_cloud_region() %>%
    select(BASIC.REGION,BASIC.DIVISION, CLASS.ORG.BELOW, CLASS.ORG.ABOVE, CLASS.ORG.WITHIN) %>% 
    mutate(across(3:last_col(), as.numeric)) %>% 
    mutate(BASIC.DIVISION = as.character(BASIC.DIVISION)) %>%
    filter(BASIC.DIVISION != "") %>% 
    group_by(BASIC.REGION,BASIC.DIVISION) %>%
    summarise(across(everything(), sum, na.rm = TRUE)) %>% 
    rename(
      "REGION" = BASIC.REGION,
      "DIVISION" = BASIC.DIVISION,
      "BELOW THE STANDARD" = CLASS.ORG.BELOW,
      "ABOVE THE STANDARD" = CLASS.ORG.ABOVE,
      "WITHIN THE STANDARD" = CLASS.ORG.WITHIN
    )
  
  # You might want to add a check for NULL or empty data if filtered_school_data_division()
  # could return such states and you want to display a message or an empty table.
  if (is.null(data_to_display) || nrow(data_to_display) == 0) {
    return(DT::datatable(
      data.frame("Message" = "No data available based on current selection."),
      options = list(dom = 't'), # 't' hides all controls, showing only the table body
      rownames = FALSE
    ))
  }
  
  DT::datatable(
    data_to_display,
    options = list(pageLength = 10, scrollX = TRUE),
    filter = 'top',
    selection = 'multiple',
    rownames = FALSE
  )
})

output$organizedclass_regional_graph <- renderPlotly({
  # Use the reactive filtered data
  
  data_to_display <- cloud %>%
    select(BASIC.REGION, CLASS.ORG.BELOW, CLASS.ORG.ABOVE, CLASS.ORG.WITHIN) %>% 
    mutate(across(2:last_col(), as.numeric)) %>% 
    mutate(BASIC.REGION = as.character(BASIC.REGION)) %>%
    filter(BASIC.REGION != "") %>% 
    group_by(BASIC.REGION) %>%
    summarise(across(everything(), sum, na.rm = TRUE)) %>% 
    rename(
      "REGION" = BASIC.REGION,
      "BELOW THE STANDARD" = CLASS.ORG.BELOW,
      "ABOVE THE STANDARD" = CLASS.ORG.ABOVE,
      "WITHIN THE STANDARD" = CLASS.ORG.WITHIN
    )  %>% 
    pivot_longer(
      cols = 2:last_col(), # Specifies the columns to pivot
      names_to = "Classes_Organized", # The new column to hold the original column names
      values_to = "Count" # The new column to hold the values
    ) %>%
    # This is the key change: Convert 'Classes_Organized' to a factor with specific levels
    mutate(Classes_Organized = factor(Classes_Organized, levels = c(
      "BELOW THE STANDARD",
      "WITHIN THE STANDARD",
      "ABOVE THE STANDARD"
    )))
  
  current_filtered_data <- data_to_display
  
  # --- Empty Data Handling ---
  if (nrow(current_filtered_data) == 0) {
    return(ggplotly(ggplot() +
                      annotate("text", x = 0.5, y = 0.5, label = "No data for selected regions/divisions") +
                      theme_void()))
  }
  
  # Prepare the data for plotting
  plot_data <- data_to_display
  
  # Create the ggplot
  p <- ggplot(plot_data,
              aes(x = REGION, # Reorder regions based on overall total count for the region
                  y = Count,
                  fill = Classes_Organized, # Fill by Clustering.Status for coloring and consistent order
                  text = paste("Region: ", REGION,
                               "<br>Section: ", Classes_Organized,
                               "<br>Count: ", scales::comma(Count)))) + # Custom tooltip text
    geom_bar(stat = "identity", position = "dodge", color = "black") + # Changed to position="dodge" for dodgeed bars
    labs(x = "",
         y = "") +
    scale_y_continuous(labels = scales::comma) + # Format y-axis labels as comma-separated numbers
    scale_fill_manual(values = c(
      "BELOW THE STANDARD" = "blue",
      "WITHIN THE STANDARD" = "green", # Optional: you can set a color for this too
      "ABOVE THE STANDARD" = "red"
    )) +
    theme_minimal() +
    theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
          legend.position = "bottom", # Position legend at the bottom
          plot.title = element_text(hjust = 0.5)) # Center the plot title
  
  # Convert ggplot to plotly, ensuring custom text is used for hover
  ggplotly(p, tooltip = "text", source = "cloudenrolmentRegionPlot") %>%
    layout(hoverlabel = list(bgcolor = "white"),
           margin = list(b = 100)) # Increase bottom margin for x-axis labels
})

output$organizedclass_division_graph <- renderPlotly({
  # Use the reactive filtered data
  
  data_to_display <- filtered_cloud_region() %>%
    select(BASIC.DIVISION, CLASS.ORG.BELOW, CLASS.ORG.ABOVE, CLASS.ORG.WITHIN) %>% 
    mutate(across(2:last_col(), as.numeric)) %>% 
    mutate(BASIC.DIVISION = as.character(BASIC.DIVISION)) %>%
    filter(BASIC.DIVISION != "") %>% 
    group_by(BASIC.DIVISION) %>%
    summarise(across(everything(), sum, na.rm = TRUE)) %>% 
    rename(
      "DIVISION" = BASIC.DIVISION,
      "BELOW THE STANDARD" = CLASS.ORG.BELOW,
      "ABOVE THE STANDARD" = CLASS.ORG.ABOVE,
      "WITHIN THE STANDARD" = CLASS.ORG.WITHIN
    )  %>% 
    pivot_longer(
      cols = 2:last_col(), # Specifies the columns to pivot
      names_to = "Classes_Organized", # The new column to hold the original column names
      values_to = "Count" # The new column to hold the values
    ) %>%
    # This is the key change: Convert 'Classes_Organized' to a factor with specific levels
    mutate(Classes_Organized = factor(Classes_Organized, levels = c(
      "BELOW THE STANDARD",
      "WITHIN THE STANDARD",
      "ABOVE THE STANDARD"
    )))
  
  current_filtered_data <- data_to_display
  
  # --- Empty Data Handling ---
  if (nrow(current_filtered_data) == 0) {
    return(ggplotly(ggplot() +
                      annotate("text", x = 0.5, y = 0.5, label = "No data for selected regions/divisions") +
                      theme_void()))
  }
  
  # Prepare the data for plotting
  plot_data <- data_to_display
  
  # Create the ggplot
  p <- ggplot(plot_data,
              aes(x = Classes_Organized, # Reorder regions based on overall total count for the region
                  y = Count,
                  fill = Classes_Organized, # Fill by Clustering.Status for coloring and consistent order
                  text = paste("Region: ", DIVISION,
                               "<br>Section: ", Classes_Organized,
                               "<br>Count: ", scales::comma(Count)))) + # Custom tooltip text
    geom_bar(stat = "identity", position = "dodge", color = "black") + # Changed to position="dodge" for dodgeed bars
    scale_y_continuous(labels = scales::comma) + # Format y-axis labels as comma-separated numbers
    scale_fill_manual(values = c(
      "BELOW THE STANDARD" = "blue",
      "WITHIN THE STANDARD" = "green", # Optional: you can set a color for this too
      "ABOVE THE STANDARD" = "red"
    )) +
    theme_minimal() +
    theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
          legend.position = "bottom", # Position legend at the bottom
          plot.title = element_text(hjust = 0.5)) +
    facet_wrap(~DIVISION) + coord_flip()+
    labs(x = NULL, y = NULL, fill = NULL)
# Center the plot title # Center the plot title
  
  # Convert ggplot to plotly, ensuring custom text is used for hover
  ggplotly(p, tooltip = "text", source = "cloudenrolmentDivisionPlot") %>%
    layout(hoverlabel = list(bgcolor = "white"),
           margin = list(b = 100)) # Increase bottom margin for x-axis labels
})

output$jhsdeployment_data_table <- DT::renderDT({
  
  data_to_display <- filtered_cloud_region() %>%
    select(BASIC.REGION,BASIC.DIVISION,Actual.Teacher.AFA,Actual.Teacher.FCS,Actual.Teacher.IA,Actual.Teacher.ICT) %>% 
    mutate(across(3:last_col(), as.numeric)) %>% 
    mutate(BASIC.DIVISION = as.character(BASIC.DIVISION)) %>%
    filter(BASIC.DIVISION != "") %>% 
    group_by(BASIC.REGION,BASIC.DIVISION) %>%
    summarise(across(everything(), sum, na.rm = TRUE)) %>% 
    rename(
      "REGION" = BASIC.REGION,
      "DIVISION" = BASIC.DIVISION,
      "AFA" = Actual.Teacher.AFA,
      "FCS" = Actual.Teacher.FCS,
      "IA" = Actual.Teacher.IA,
      "ICT" = Actual.Teacher.ICT)
  
  # You might want to add a check for NULL or empty data if filtered_school_data_division()
  # could return such states and you want to display a message or an empty table.
  if (is.null(data_to_display) || nrow(data_to_display) == 0) {
    return(DT::datatable(
      data.frame("Message" = "No data available based on current selection."),
      options = list(dom = 't'), # 't' hides all controls, showing only the table body
      rownames = FALSE
    ))
  }
  
  DT::datatable(
    data_to_display,
    options = list(pageLength = 10, scrollX = TRUE),
    filter = 'top',
    selection = 'multiple',
    rownames = FALSE
  )
})

output$jhsdeployment_regional_graph <- renderPlotly({
  # Use the reactive filtered data
  
  data_to_display <- cloud %>%
    select(BASIC.REGION,Actual.Teacher.AFA,Actual.Teacher.FCS,Actual.Teacher.IA,Actual.Teacher.ICT) %>% 
    mutate(across(2:last_col(), as.numeric)) %>% 
    mutate(BASIC.REGION = as.character(BASIC.REGION)) %>%
    filter(BASIC.REGION != "") %>% 
    group_by(BASIC.REGION) %>%
    summarise(across(everything(), sum, na.rm = TRUE)) %>% 
    rename(
      "REGION" = BASIC.REGION,
      "AFA" = Actual.Teacher.AFA,
      "FCS" = Actual.Teacher.FCS,
      "IA" = Actual.Teacher.IA,
      "ICT" = Actual.Teacher.ICT) %>% 
    pivot_longer(
      cols = 2:last_col(), # Specifies the columns to pivot
      names_to = "Actual_JHS_Teachers_Deployed", # The new column to hold the original column names
      values_to = "Count" # The new column to hold the values
    )
  
  current_filtered_data <- data_to_display
  
  # --- Empty Data Handling ---
  if (nrow(current_filtered_data) == 0) {
    return(ggplotly(ggplot() +
                      annotate("text", x = 0.5, y = 0.5, label = "No data for selected regions/divisions") +
                      theme_void()))
  }
  
  # Prepare the data for plotting
  plot_data <- data_to_display
  
  # Create the ggplot
  p <- ggplot(plot_data,
              aes(x = REGION, # Reorder regions based on overall total count for the region
                  y = Count,
                  fill = Actual_JHS_Teachers_Deployed, # Fill by Clustering.Status for coloring and consistent order
                  text = paste("Region: ", REGION,
                               "<br>Section: ", Actual_JHS_Teachers_Deployed,
                               "<br>Count: ", scales::comma(Count)))) + # Custom tooltip text
    geom_bar(stat = "identity", position = "dodge", color = "black") + # Changed to position="dodge" for dodgeed bars
    labs(x = "",
         y = "") +
    scale_y_continuous(labels = scales::comma) + # Format y-axis labels as comma-separated numbers
    theme_minimal() +
    theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
          legend.position = "bottom", # Position legend at the bottom
          plot.title = element_text(hjust = 0.5)) # Center the plot title
  
  # Convert ggplot to plotly, ensuring custom text is used for hover
  ggplotly(p, tooltip = "text", source = "cloudenrolmentRegionPlot") %>%
    layout(hoverlabel = list(bgcolor = "white"),
           margin = list(b = 100)) # Increase bottom margin for x-axis labels
})

output$jhsdeployment_division_graph <- renderPlotly({
  # Use the reactive filtered data
  
  data_to_display <- filtered_cloud_region() %>%
    select(BASIC.DIVISION,Actual.Teacher.AFA,Actual.Teacher.FCS,Actual.Teacher.IA,Actual.Teacher.ICT) %>% 
    mutate(across(2:last_col(), as.numeric)) %>% 
    mutate(BASIC.DIVISION = as.character(BASIC.DIVISION)) %>%
    filter(BASIC.DIVISION != "") %>% 
    group_by(BASIC.DIVISION) %>%
    summarise(across(everything(), sum, na.rm = TRUE)) %>% 
    rename(
      "DIVISION" = BASIC.DIVISION,
      "AFA" = Actual.Teacher.AFA,
      "FCS" = Actual.Teacher.FCS,
      "IA" = Actual.Teacher.IA,
      "ICT" = Actual.Teacher.ICT)  %>% 
    pivot_longer(
      cols = 2:last_col(), # Specifies the columns to pivot
      names_to = "Actual_JHS_Teachers_Deployed", # The new column to hold the original column names
      values_to = "Count" # The new column to hold the values
    )
  
  current_filtered_data <- data_to_display
  
  # --- Empty Data Handling ---
  if (nrow(current_filtered_data) == 0) {
    return(ggplotly(ggplot() +
                      annotate("text", x = 0.5, y = 0.5, label = "No data for selected regions/divisions") +
                      theme_void()))
  }
  
  # Prepare the data for plotting
  plot_data <- data_to_display
  
  # Create the ggplot
  p <- ggplot(plot_data,
              aes(x = Actual_JHS_Teachers_Deployed, # Reorder regions based on overall total count for the region
                  y = Count,
                  fill = Actual_JHS_Teachers_Deployed, # Fill by Clustering.Status for coloring and consistent order
                  text = paste("Region: ", DIVISION,
                               "<br>Section: ", Actual_JHS_Teachers_Deployed,
                               "<br>Count: ", scales::comma(Count)))) + # Custom tooltip text
    geom_bar(stat = "identity", position = "dodge", color = "black") + # Changed to position="dodge" for dodgeed bars
    scale_y_continuous(labels = scales::comma) + # Format y-axis labels as comma-separated numbers
    theme_minimal() +
    theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
          legend.position = "bottom", # Position legend at the bottom
          plot.title = element_text(hjust = 0.5)) +
    facet_wrap(~DIVISION) + coord_flip()+
    labs(x = NULL, y = NULL, fill = NULL)
# Center the plot title # Center the plot title
  
  # Convert ggplot to plotly, ensuring custom text is used for hover
  ggplotly(p, tooltip = "text", source = "cloudenrolmentDivisionPlot") %>%
    layout(hoverlabel = list(bgcolor = "white"),
           margin = list(b = 100)) # Increase bottom margin for x-axis labels
})

output$lac_data_table <- DT::renderDT({
  
  data_to_display <- filtered_cloud_region_v3() %>%
    select(BASIC.REGION,BASIC.DIVISION, contains("LAC.OFTEN")) %>% 
    mutate(across(3:last_col(), as.numeric)) %>% 
    mutate(BASIC.DIVISION = as.character(BASIC.DIVISION)) %>%
    filter(BASIC.DIVISION != "") %>% 
    group_by(BASIC.REGION,BASIC.DIVISION) %>%
    summarise(across(everything(), sum, na.rm = TRUE))
  
  # You might want to add a check for NULL or empty data if filtered_school_data_division()
  # could return such states and you want to display a message or an empty table.
  if (is.null(data_to_display) || nrow(data_to_display) == 0) {
    return(DT::datatable(
      data.frame("Message" = "No data available based on current selection."),
      options = list(dom = 't'), # 't' hides all controls, showing only the table body
      rownames = FALSE
    ))
  }
  
  DT::datatable(
    data_to_display,
    options = list(pageLength = 10, scrollX = TRUE),
    filter = 'top',
    selection = 'multiple',
    rownames = FALSE
  )
})

output$shifting_data_table <- DT::renderDT({
  
  data_to_display <- filtered_cloud_region_v3() %>%
    select(BASIC.REGION,BASIC.DIVISION, contains("SHIFTING.")) %>% 
    mutate(across(3:last_col(), as.numeric)) %>% 
    mutate(BASIC.DIVISION = as.character(BASIC.DIVISION)) %>%
    filter(BASIC.DIVISION != "") %>% 
    group_by(BASIC.REGION,BASIC.DIVISION) %>%
    summarise(across(everything(), sum, na.rm = TRUE))
  
  # You might want to add a check for NULL or empty data if filtered_school_data_division()
  # could return such states and you want to display a message or an empty table.
  if (is.null(data_to_display) || nrow(data_to_display) == 0) {
    return(DT::datatable(
      data.frame("Message" = "No data available based on current selection."),
      options = list(dom = 't'), # 't' hides all controls, showing only the table body
      rownames = FALSE
    ))
  }
  
  DT::datatable(
    data_to_display,
    options = list(pageLength = 10, scrollX = TRUE),
    filter = 'top',
    selection = 'multiple',
    rownames = FALSE
  )
})

output$shifting_regional_graph <- renderPlotly({
  # Use the reactive filtered data
  
  data_to_display <- cloud_v3 %>%
    select(BASIC.REGION, contains("SHIFTING.")) %>% 
    mutate(across(2:last_col(), as.numeric)) %>% 
    mutate(BASIC.REGION = as.character(BASIC.REGION)) %>%
    filter(BASIC.REGION != "") %>% 
    group_by(BASIC.REGION) %>%
    summarise(across(everything(), sum, na.rm = TRUE))  %>% 
    pivot_longer(
      cols = 2:last_col(), # Specifies the columns to pivot
      names_to = "Sections", # The new column to hold the original column names
      values_to = "Count" # The new column to hold the values
    )
  
  current_filtered_data <- data_to_display
  
  # --- Empty Data Handling ---
  if (nrow(current_filtered_data) == 0) {
    return(ggplotly(ggplot() +
                      annotate("text", x = 0.5, y = 0.5, label = "No data for selected regions/divisions") +
                      theme_void()))
  }
  
  # Prepare the data for plotting
  plot_data <- data_to_display
  
  # Create the ggplot
  p <- ggplot(plot_data,
              aes(x = BASIC.REGION, # Reorder regions based on overall total count for the region
                  y = Count,
                  fill = Sections, # Fill by Clustering.Status for coloring and consistent order
                  text = paste("Region: ", BASIC.REGION,
                               "<br>Section: ", Sections,
                               "<br>Count: ", scales::comma(Count)))) + # Custom tooltip text
    geom_bar(stat = "identity", position = "dodge", color = "black") + # Changed to position="dodge" for dodgeed bars
    labs(x = "",
         y = "") +
    scale_y_continuous(labels = scales::comma) + # Format y-axis labels as comma-separated numbers
    theme_minimal() +
    theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
          legend.position = "bottom", # Position legend at the bottom
          plot.title = element_text(hjust = 0.5)) # Center the plot title
  
  # Convert ggplot to plotly, ensuring custom text is used for hover
  ggplotly(p, tooltip = "text", source = "cloudenrolmentRegionPlot") %>%
    layout(hoverlabel = list(bgcolor = "white"),
           margin = list(b = 100)) # Increase bottom margin for x-axis labels
})

output$shifting_division_graph <- renderPlotly({
  # Use the reactive filtered data
  
  data_to_display <- filtered_cloud_region_v3() %>%
    select(BASIC.DIVISION, contains("SHIFTING.")) %>% 
    mutate(across(2:last_col(), as.numeric)) %>% 
    mutate(BASIC.DIVISION = as.character(BASIC.DIVISION)) %>%
    filter(BASIC.DIVISION != "") %>% 
    group_by(BASIC.DIVISION) %>%
    summarise(across(everything(), sum, na.rm = TRUE))  %>% 
    pivot_longer(
      cols = 2:last_col(), # Specifies the columns to pivot
      names_to = "Sections", # The new column to hold the original column names
      values_to = "Count" # The new column to hold the values
    )
  
  current_filtered_data <- data_to_display
  
  # --- Empty Data Handling ---
  if (nrow(current_filtered_data) == 0) {
    return(ggplotly(ggplot() +
                      annotate("text", x = 0.5, y = 0.5, label = "No data for selected regions/divisions") +
                      theme_void()))
  }
  
  # Prepare the data for plotting
  plot_data <- data_to_display
  
  # Create the ggplot
  p <- ggplot(plot_data,
              aes(x = Sections, # Reorder regions based on overall total count for the region
                  y = Count,
                  fill = Sections, # Fill by Clustering.Status for coloring and consistent order
                  text = paste("Region: ", BASIC.DIVISION,
                               "<br>Section: ", Sections,
                               "<br>Count: ", scales::comma(Count)))) + # Custom tooltip text
    geom_bar(stat = "identity", position = "dodge", color = "black") + # Changed to position="dodge" for dodgeed bars
    labs(x = "",
         y = "") +
    scale_y_continuous(labels = scales::comma) + # Format y-axis labels as comma-separated numbers
    theme_minimal() +
    theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
          legend.position = "bottom", # Position legend at the bottom
          plot.title = element_text(hjust = 0.5)) +
    facet_wrap(~BASIC.DIVISION) + coord_flip()+
    labs(x = NULL, y = NULL, fill = NULL) # Center the plot title # Center the plot title
  
  # Convert ggplot to plotly, ensuring custom text is used for hover
  ggplotly(p, tooltip = "text", source = "cloudenrolmentDivisionPlot") %>%
    layout(hoverlabel = list(bgcolor = "white"),
           margin = list(b = 100)) # Increase bottom margin for x-axis labels
})

output$ldm_data_table <- DT::renderDT({
  
  data_to_display <- filtered_cloud_region_v3() %>%
    select(BASIC.REGION,BASIC.DIVISION, contains("LDM.")) %>% 
    mutate(across(3:last_col(), as.numeric)) %>% 
    mutate(BASIC.DIVISION = as.character(BASIC.DIVISION)) %>%
    filter(BASIC.DIVISION != "") %>% 
    group_by(BASIC.REGION,BASIC.DIVISION) %>%
    summarise(across(everything(), sum, na.rm = TRUE))
  
  # You might want to add a check for NULL or empty data if filtered_school_data_division()
  # could return such states and you want to display a message or an empty table.
  if (is.null(data_to_display) || nrow(data_to_display) == 0) {
    return(DT::datatable(
      data.frame("Message" = "No data available based on current selection."),
      options = list(dom = 't'), # 't' hides all controls, showing only the table body
      rownames = FALSE
    ))
  }
  
  DT::datatable(
    data_to_display,
    options = list(pageLength = 10, scrollX = TRUE),
    filter = 'top',
    selection = 'multiple',
    rownames = FALSE
  )
})

output$ldm_regional_graph <- renderPlotly({
  # Use the reactive filtered data
  
  data_to_display <- cloud_v3 %>%
    select(BASIC.REGION, contains("LDM.")) %>% 
    mutate(across(2:last_col(), as.numeric)) %>% 
    mutate(BASIC.REGION = as.character(BASIC.REGION)) %>%
    filter(BASIC.REGION != "") %>% 
    group_by(BASIC.REGION) %>%
    summarise(across(everything(), sum, na.rm = TRUE))  %>% 
    pivot_longer(
      cols = 2:last_col(), # Specifies the columns to pivot
      names_to = "Sections", # The new column to hold the original column names
      values_to = "Count" # The new column to hold the values
    )
  
  current_filtered_data <- data_to_display
  
  # --- Empty Data Handling ---
  if (nrow(current_filtered_data) == 0) {
    return(ggplotly(ggplot() +
                      annotate("text", x = 0.5, y = 0.5, label = "No data for selected regions/divisions") +
                      theme_void()))
  }
  
  # Prepare the data for plotting
  plot_data <- data_to_display
  
  # Create the ggplot
  p <- ggplot(plot_data,
              aes(x = BASIC.REGION, # Reorder regions based on overall total count for the region
                  y = Count,
                  fill = Sections, # Fill by Clustering.Status for coloring and consistent order
                  text = paste("Region: ", BASIC.REGION,
                               "<br>Section: ", Sections,
                               "<br>Count: ", scales::comma(Count)))) + # Custom tooltip text
    geom_bar(stat = "identity", position = "dodge", color = "black") + # Changed to position="dodge" for dodgeed bars
    labs(x = "",
         y = "") +
    scale_y_continuous(labels = scales::comma) + # Format y-axis labels as comma-separated numbers
    theme_minimal() +
    theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
          legend.position = "bottom", # Position legend at the bottom
          plot.title = element_text(hjust = 0.5)) # Center the plot title
  
  # Convert ggplot to plotly, ensuring custom text is used for hover
  ggplotly(p, tooltip = "text", source = "cloudenrolmentRegionPlot") %>%
    layout(hoverlabel = list(bgcolor = "white"),
           margin = list(b = 100)) # Increase bottom margin for x-axis labels
})

output$ldm_division_graph <- renderPlotly({
  # Use the reactive filtered data
  
  data_to_display <- filtered_cloud_region_v3() %>%
    select(BASIC.DIVISION, contains("LDM.")) %>% 
    mutate(across(2:last_col(), as.numeric)) %>% 
    mutate(BASIC.DIVISION = as.character(BASIC.DIVISION)) %>%
    filter(BASIC.DIVISION != "") %>% 
    group_by(BASIC.DIVISION) %>%
    summarise(across(everything(), sum, na.rm = TRUE))  %>% 
    pivot_longer(
      cols = 2:last_col(), # Specifies the columns to pivot
      names_to = "Sections", # The new column to hold the original column names
      values_to = "Count" # The new column to hold the values
    )
  
  current_filtered_data <- data_to_display
  
  # --- Empty Data Handling ---
  if (nrow(current_filtered_data) == 0) {
    return(ggplotly(ggplot() +
                      annotate("text", x = 0.5, y = 0.5, label = "No data for selected regions/divisions") +
                      theme_void()))
  }
  
  # Prepare the data for plotting
  plot_data <- data_to_display
  
  # Create the ggplot
  p <- ggplot(plot_data,
              aes(x = Sections, # Reorder regions based on overall total count for the region
                  y = Count,
                  fill = Sections, # Fill by Clustering.Status for coloring and consistent order
                  text = paste("Region: ", BASIC.DIVISION,
                               "<br>Section: ", Sections,
                               "<br>Count: ", scales::comma(Count)))) + # Custom tooltip text
    geom_bar(stat = "identity", position = "dodge", color = "black") + # Changed to position="dodge" for dodgeed bars
    labs(x = "",
         y = "") +
    scale_y_continuous(labels = scales::comma) + # Format y-axis labels as comma-separated numbers
    theme_minimal() +
    theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
          legend.position = "bottom", # Position legend at the bottom
          plot.title = element_text(hjust = 0.5)) +
    facet_wrap(~BASIC.DIVISION) + coord_flip()+
    labs(x = NULL, y = NULL, fill = NULL)# Center the plot title # Center the plot title
  
  # Convert ggplot to plotly, ensuring custom text is used for hover
  ggplotly(p, tooltip = "text", source = "cloudenrolmentDivisionPlot") %>%
    layout(hoverlabel = list(bgcolor = "white"),
           margin = list(b = 100)) # Increase bottom margin for x-axis labels
})

output$adm_data_table <- DT::renderDT({
  
  data_to_display <- filtered_cloud_region_v2() %>%
    select(BASIC.REGION,BASIC.DIVISION, contains("ALTERNATIVE.ADM")) %>% 
    mutate(across(3:last_col(), as.numeric)) %>% 
    mutate(BASIC.DIVISION = as.character(BASIC.DIVISION)) %>%
    filter(BASIC.DIVISION != "") %>% 
    group_by(BASIC.REGION,BASIC.DIVISION) %>%
    summarise(across(everything(), sum, na.rm = TRUE))
  
  # You might want to add a check for NULL or empty data if filtered_school_data_division()
  # could return such states and you want to display a message or an empty table.
  if (is.null(data_to_display) || nrow(data_to_display) == 0) {
    return(DT::datatable(
      data.frame("Message" = "No data available based on current selection."),
      options = list(dom = 't'), # 't' hides all controls, showing only the table body
      rownames = FALSE
    ))
  }
  
  DT::datatable(
    data_to_display,
    options = list(pageLength = 10, scrollX = TRUE),
    filter = 'top',
    selection = 'multiple',
    rownames = FALSE
  )
})

output$adm_regional_graph <- renderPlotly({
  # Use the reactive filtered data
  
  data_to_display <- cloud_v2 %>%
    select(BASIC.REGION, contains("ALTERNATIVE.ADM")) %>% 
    mutate(across(2:last_col(), as.numeric)) %>% 
    mutate(BASIC.REGION = as.character(BASIC.REGION)) %>%
    filter(BASIC.REGION != "") %>% 
    group_by(BASIC.REGION) %>%
    summarise(across(everything(), sum, na.rm = TRUE))  %>% 
    pivot_longer(
      cols = 2:last_col(), # Specifies the columns to pivot
      names_to = "Sections", # The new column to hold the original column names
      values_to = "Count" # The new column to hold the values
    )
  
  current_filtered_data <- data_to_display
  
  # --- Empty Data Handling ---
  if (nrow(current_filtered_data) == 0) {
    return(ggplotly(ggplot() +
                      annotate("text", x = 0.5, y = 0.5, label = "No data for selected regions/divisions") +
                      theme_void()))
  }
  
  # Prepare the data for plotting
  plot_data <- data_to_display
  
  # Create the ggplot
  p <- ggplot(plot_data,
              aes(x = BASIC.REGION, # Reorder regions based on overall total count for the region
                  y = Count,
                  fill = Sections, # Fill by Clustering.Status for coloring and consistent order
                  text = paste("Region: ", BASIC.REGION,
                               "<br>Section: ", Sections,
                               "<br>Count: ", scales::comma(Count)))) + # Custom tooltip text
    geom_bar(stat = "identity", position = "dodge", color = "black") + # Changed to position="dodge" for dodgeed bars
    labs(x = "",
         y = "") +
    scale_y_continuous(labels = scales::comma) + # Format y-axis labels as comma-separated numbers
    theme_minimal() +
    theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
          legend.position = "bottom", # Position legend at the bottom
          plot.title = element_text(hjust = 0.5)) # Center the plot title
  
  # Convert ggplot to plotly, ensuring custom text is used for hover
  ggplotly(p, tooltip = "text", source = "cloudenrolmentRegionPlot") %>%
    layout(hoverlabel = list(bgcolor = "white"),
           margin = list(b = 100)) # Increase bottom margin for x-axis labels
})

output$adm_division_graph <- renderPlotly({
  # Use the reactive filtered data
  
  data_to_display <- filtered_cloud_region_v2() %>%
    select(BASIC.DIVISION, contains("ALTERNATIVE.ADM")) %>% 
    mutate(across(2:last_col(), as.numeric)) %>% 
    mutate(BASIC.DIVISION = as.character(BASIC.DIVISION)) %>%
    filter(BASIC.DIVISION != "") %>% 
    group_by(BASIC.DIVISION) %>%
    summarise(across(everything(), sum, na.rm = TRUE))  %>% 
    pivot_longer(
      cols = 2:last_col(), # Specifies the columns to pivot
      names_to = "Sections", # The new column to hold the original column names
      values_to = "Count" # The new column to hold the values
    )
  
  current_filtered_data <- data_to_display
  
  # --- Empty Data Handling ---
  if (nrow(current_filtered_data) == 0) {
    return(ggplotly(ggplot() +
                      annotate("text", x = 0.5, y = 0.5, label = "No data for selected regions/divisions") +
                      theme_void()))
  }
  
  # Prepare the data for plotting
  plot_data <- data_to_display
  
  # Create the ggplot
  p <- ggplot(plot_data,
              aes(x = Sections, # Reorder regions based on overall total count for the region
                  y = Count,
                  fill = Sections, # Fill by Clustering.Status for coloring and consistent order
                  text = paste("Region: ", BASIC.DIVISION,
                               "<br>Section: ", Sections,
                               "<br>Count: ", scales::comma(Count)))) + # Custom tooltip text
    geom_bar(stat = "identity", position = "dodge", color = "black") + # Changed to position="dodge" for dodgeed bars
    labs(x = "",
         y = "") +
    scale_y_continuous(labels = scales::comma) + # Format y-axis labels as comma-separated numbers
    theme_minimal() +
    theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
          legend.position = "bottom", # Position legend at the bottom
          plot.title = element_text(hjust = 0.5)) +
    facet_wrap(~BASIC.DIVISION) + coord_flip()+
    labs(x = NULL, y = NULL, fill = NULL)# Center the plot title # Center the plot title
  
  # Convert ggplot to plotly, ensuring custom text is used for hover
  ggplotly(p, tooltip = "text", source = "cloudenrolmentDivisionPlot") %>%
    layout(hoverlabel = list(bgcolor = "white"),
           margin = list(b = 100)) # Increase bottom margin for x-axis labels
})

output$nat_data_table <- DT::renderDT({
  
  data_to_display <- filtered_cloud_region_v2() %>%
    select(BASIC.REGION,BASIC.DIVISION, contains("NAT.K12.CURRICULUM.ALIGN"), -"NAT.K12.CURRICULUM.ALIGN_") %>% 
    mutate(across(3:last_col(), as.numeric)) %>% 
    mutate(BASIC.DIVISION = as.character(BASIC.DIVISION)) %>%
    filter(BASIC.DIVISION != "") %>% 
    group_by(BASIC.REGION,BASIC.DIVISION) %>%
    summarise(across(everything(), sum, na.rm = TRUE))
  
  # You might want to add a check for NULL or empty data if filtered_school_data_division()
  # could return such states and you want to display a message or an empty table.
  if (is.null(data_to_display) || nrow(data_to_display) == 0) {
    return(DT::datatable(
      data.frame("Message" = "No data available based on current selection."),
      options = list(dom = 't'), # 't' hides all controls, showing only the table body
      rownames = FALSE
    ))
  }
  
  DT::datatable(
    data_to_display,
    options = list(pageLength = 10, scrollX = TRUE),
    filter = 'top',
    selection = 'multiple',
    rownames = FALSE
  )
})

output$nat_regional_graph <- renderPlotly({
  # Use the reactive filtered data
  
  data_to_display <- cloud_v2 %>%
    select(BASIC.REGION, contains("NAT.K12.CURRICULUM.ALIGN"), -"NAT.K12.CURRICULUM.ALIGN_") %>% 
    mutate(across(2:last_col(), as.numeric)) %>% 
    mutate(BASIC.REGION = as.character(BASIC.REGION)) %>%
    filter(BASIC.REGION != "") %>% 
    group_by(BASIC.REGION) %>%
    summarise(across(everything(), sum, na.rm = TRUE))  %>% 
    pivot_longer(
      cols = 2:last_col(), # Specifies the columns to pivot
      names_to = "Sections", # The new column to hold the original column names
      values_to = "Count" # The new column to hold the values
    )
  
  current_filtered_data <- data_to_display
  
  # --- Empty Data Handling ---
  if (nrow(current_filtered_data) == 0) {
    return(ggplotly(ggplot() +
                      annotate("text", x = 0.5, y = 0.5, label = "No data for selected regions/divisions") +
                      theme_void()))
  }
  
  # Prepare the data for plotting
  plot_data <- data_to_display
  
  # Create the ggplot
  p <- ggplot(plot_data,
              aes(x = BASIC.REGION, # Reorder regions based on overall total count for the region
                  y = Count,
                  fill = Sections, # Fill by Clustering.Status for coloring and consistent order
                  text = paste("Region: ", BASIC.REGION,
                               "<br>Section: ", Sections,
                               "<br>Count: ", scales::comma(Count)))) + # Custom tooltip text
    geom_bar(stat = "identity", position = "dodge", color = "black") + # Changed to position="dodge" for dodgeed bars
    labs(x = "",
         y = "") +
    scale_y_continuous(labels = scales::comma) + # Format y-axis labels as comma-separated numbers
    theme_minimal() +
    theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
          legend.position = "bottom", # Position legend at the bottom
          plot.title = element_text(hjust = 0.5)) # Center the plot title
  
  # Convert ggplot to plotly, ensuring custom text is used for hover
  ggplotly(p, tooltip = "text", source = "cloudenrolmentRegionPlot") %>%
    layout(hoverlabel = list(bgcolor = "white"),
           margin = list(b = 100)) # Increase bottom margin for x-axis labels
})

output$nat_division_graph <- renderPlotly({
  # Use the reactive filtered data
  
  data_to_display <- filtered_cloud_region_v2() %>%
    select(BASIC.DIVISION, contains("NAT.K12.CURRICULUM.ALIGN"), -"NAT.K12.CURRICULUM.ALIGN_") %>% 
    mutate(across(2:last_col(), as.numeric)) %>% 
    mutate(BASIC.DIVISION = as.character(BASIC.DIVISION)) %>%
    filter(BASIC.DIVISION != "") %>% 
    group_by(BASIC.DIVISION) %>%
    summarise(across(everything(), sum, na.rm = TRUE))  %>% 
    pivot_longer(
      cols = 2:last_col(), # Specifies the columns to pivot
      names_to = "Sections", # The new column to hold the original column names
      values_to = "Count" # The new column to hold the values
    )
  
  current_filtered_data <- data_to_display
  
  # --- Empty Data Handling ---
  if (nrow(current_filtered_data) == 0) {
    return(ggplotly(ggplot() +
                      annotate("text", x = 0.5, y = 0.5, label = "No data for selected regions/divisions") +
                      theme_void()))
  }
  
  # Prepare the data for plotting
  plot_data <- data_to_display
  
  # Create the ggplot
  p <- ggplot(plot_data,
              aes(x = Sections, # Reorder regions based on overall total count for the region
                  y = Count,
                  fill = Sections, # Fill by Clustering.Status for coloring and consistent order
                  text = paste("Region: ", BASIC.DIVISION,
                               "<br>Section: ", Sections,
                               "<br>Count: ", scales::comma(Count)))) + # Custom tooltip text
    geom_bar(stat = "identity", position = "dodge", color = "black") + # Changed to position="dodge" for dodgeed bars
    labs(x = "",
         y = "") +
    scale_y_continuous(labels = scales::comma) + # Format y-axis labels as comma-separated numbers
    theme_minimal() +
    theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
          legend.position = "bottom", # Position legend at the bottom
          plot.title = element_text(hjust = 0.5)) +
    facet_wrap(~BASIC.DIVISION) + coord_flip()+
    labs(x = NULL, y = NULL, fill = NULL)# Center the plot title # Center the plot title
  
  # Convert ggplot to plotly, ensuring custom text is used for hover
  ggplotly(p, tooltip = "text", source = "cloudenrolmentDivisionPlot") %>%
    layout(hoverlabel = list(bgcolor = "white"),
           margin = list(b = 100)) # Increase bottom margin for x-axis labels
})

output$nat_sufficiency_data_table <- DT::renderDT({
  
  data_to_display <- filtered_cloud_region_v3() %>%
    select(BASIC.REGION,BASIC.DIVISION,COMPRE_YES_NAT,YES_NAT,NOT_ADEQ_NAT,NO_NAT) %>% 
    mutate(across(3:last_col(), as.numeric)) %>% 
    mutate(BASIC.DIVISION = as.character(BASIC.DIVISION)) %>%
    filter(BASIC.DIVISION != "") %>% 
    group_by(BASIC.REGION,BASIC.DIVISION) %>%
    summarise(across(everything(), sum, na.rm = TRUE))
  
  # You might want to add a check for NULL or empty data if filtered_school_data_division()
  # could return such states and you want to display a message or an empty table.
  if (is.null(data_to_display) || nrow(data_to_display) == 0) {
    return(DT::datatable(
      data.frame("Message" = "No data available based on current selection."),
      options = list(dom = 't'), # 't' hides all controls, showing only the table body
      rownames = FALSE
    ))
  }
  
  DT::datatable(
    data_to_display,
    options = list(pageLength = 10, scrollX = TRUE),
    filter = 'top',
    selection = 'multiple',
    rownames = FALSE
  )
})

output$nat_sufficiency_regional_graph <- renderPlotly({
  # Use the reactive filtered data
  
  data_to_display <- cloud_v3 %>%
    select(BASIC.REGION, COMPRE_YES_NAT,YES_NAT,NOT_ADEQ_NAT,NO_NAT) %>% 
    mutate(across(2:last_col(), as.numeric)) %>% 
    mutate(BASIC.REGION = as.character(BASIC.REGION)) %>%
    filter(BASIC.REGION != "") %>% 
    group_by(BASIC.REGION) %>%
    summarise(across(everything(), sum, na.rm = TRUE))  %>% 
    pivot_longer(
      cols = 2:last_col(), # Specifies the columns to pivot
      names_to = "Sections", # The new column to hold the original column names
      values_to = "Count" # The new column to hold the values
    )
  
  current_filtered_data <- data_to_display
  
  # --- Empty Data Handling ---
  if (nrow(current_filtered_data) == 0) {
    return(ggplotly(ggplot() +
                      annotate("text", x = 0.5, y = 0.5, label = "No data for selected regions/divisions") +
                      theme_void()))
  }
  
  # Prepare the data for plotting
  plot_data <- data_to_display
  
  # Create the ggplot
  p <- ggplot(plot_data,
              aes(x = BASIC.REGION, # Reorder regions based on overall total count for the region
                  y = Count,
                  fill = Sections, # Fill by Clustering.Status for coloring and consistent order
                  text = paste("Region: ", BASIC.REGION,
                               "<br>Section: ", Sections,
                               "<br>Count: ", scales::comma(Count)))) + # Custom tooltip text
    geom_bar(stat = "identity", position = "dodge", color = "black") + # Changed to position="dodge" for dodgeed bars
    labs(x = "",
         y = "") +
    scale_y_continuous(labels = scales::comma) + # Format y-axis labels as comma-separated numbers
    theme_minimal() +
    theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
          legend.position = "bottom", # Position legend at the bottom
          plot.title = element_text(hjust = 0.5)) # Center the plot title
  
  # Convert ggplot to plotly, ensuring custom text is used for hover
  ggplotly(p, tooltip = "text", source = "cloudenrolmentRegionPlot") %>%
    layout(hoverlabel = list(bgcolor = "white"),
           margin = list(b = 100)) # Increase bottom margin for x-axis labels
})

output$nat_sufficiency_division_graph <- renderPlotly({
  # Use the reactive filtered data
  
  data_to_display <- filtered_cloud_region_v3() %>%
    select(BASIC.DIVISION,COMPRE_YES_NAT,YES_NAT,NOT_ADEQ_NAT,NO_NAT) %>% 
    mutate(across(2:last_col(), as.numeric)) %>% 
    mutate(BASIC.DIVISION = as.character(BASIC.DIVISION)) %>%
    filter(BASIC.DIVISION != "") %>% 
    group_by(BASIC.DIVISION) %>%
    summarise(across(everything(), sum, na.rm = TRUE))  %>% 
    pivot_longer(
      cols = 2:last_col(), # Specifies the columns to pivot
      names_to = "Sections", # The new column to hold the original column names
      values_to = "Count" # The new column to hold the values
    )
  
  current_filtered_data <- data_to_display
  
  # --- Empty Data Handling ---
  if (nrow(current_filtered_data) == 0) {
    return(ggplotly(ggplot() +
                      annotate("text", x = 0.5, y = 0.5, label = "No data for selected regions/divisions") +
                      theme_void()))
  }
  
  # Prepare the data for plotting
  plot_data <- data_to_display
  
  # Create the ggplot
  p <- ggplot(plot_data,
              aes(x = Sections, # Reorder regions based on overall total count for the region
                  y = Count,
                  fill = Sections, # Fill by Clustering.Status for coloring and consistent order
                  text = paste("Region: ", BASIC.DIVISION,
                               "<br>Section: ", Sections,
                               "<br>Count: ", scales::comma(Count)))) + # Custom tooltip text
    geom_bar(stat = "identity", position = "dodge", color = "black") + # Changed to position="dodge" for dodgeed bars
    labs(x = "",
         y = "") +
    scale_y_continuous(labels = scales::comma) + # Format y-axis labels as comma-separated numbers
    theme_minimal() +
    theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
          legend.position = "bottom", # Position legend at the bottom
          plot.title = element_text(hjust = 0.5)) +
    facet_wrap(~BASIC.DIVISION) + coord_flip()+
    labs(x = NULL, y = NULL, fill = NULL)# Center the plot title # Center the plot title
  
  # Convert ggplot to plotly, ensuring custom text is used for hover
  ggplotly(p, tooltip = "text", source = "cloudenrolmentDivisionPlot") %>%
    layout(hoverlabel = list(bgcolor = "white"),
           margin = list(b = 100)) # Increase bottom margin for x-axis labels
})

output$elec_data_table <- DT::renderDT({
  
  data_to_display <- filtered_cloud_region_v3() %>%
    select(BASIC.REGION,BASIC.DIVISION,GRID_ELEC,OFFGRID_ELEC,COMBINED_ELEC,NO_ELEC_) %>% 
    mutate(across(3:last_col(), as.numeric)) %>% 
    mutate(BASIC.DIVISION = as.character(BASIC.DIVISION)) %>%
    filter(BASIC.DIVISION != "") %>% 
    group_by(BASIC.REGION,BASIC.DIVISION) %>%
    summarise(across(everything(), sum, na.rm = TRUE))
  
  # You might want to add a check for NULL or empty data if filtered_school_data_division()
  # could return such states and you want to display a message or an empty table.
  if (is.null(data_to_display) || nrow(data_to_display) == 0) {
    return(DT::datatable(
      data.frame("Message" = "No data available based on current selection."),
      options = list(dom = 't'), # 't' hides all controls, showing only the table body
      rownames = FALSE
    ))
  }
  
  DT::datatable(
    data_to_display,
    options = list(pageLength = 10, scrollX = TRUE),
    filter = 'top',
    selection = 'multiple',
    rownames = FALSE
  )
})

output$elec_regional_graph <- renderPlotly({
  # Use the reactive filtered data
  
  data_to_display <- cloud_v3 %>%
    select(BASIC.REGION,GRID_ELEC,OFFGRID_ELEC,COMBINED_ELEC,NO_ELEC_) %>% 
    mutate(across(2:last_col(), as.numeric)) %>% 
    mutate(BASIC.REGION = as.character(BASIC.REGION)) %>%
    filter(BASIC.REGION != "") %>% 
    group_by(BASIC.REGION) %>%
    summarise(across(everything(), sum, na.rm = TRUE))  %>% 
    pivot_longer(
      cols = 2:last_col(), # Specifies the columns to pivot
      names_to = "Sections", # The new column to hold the original column names
      values_to = "Count" # The new column to hold the values
    )
  
  current_filtered_data <- data_to_display
  
  # --- Empty Data Handling ---
  if (nrow(current_filtered_data) == 0) {
    return(ggplotly(ggplot() +
                      annotate("text", x = 0.5, y = 0.5, label = "No data for selected regions/divisions") +
                      theme_void()))
  }
  
  # Prepare the data for plotting
  plot_data <- data_to_display
  
  # Create the ggplot
  p <- ggplot(plot_data,
              aes(x = BASIC.REGION, # Reorder regions based on overall total count for the region
                  y = Count,
                  fill = Sections, # Fill by Clustering.Status for coloring and consistent order
                  text = paste("Region: ", BASIC.REGION,
                               "<br>Section: ", Sections,
                               "<br>Count: ", scales::comma(Count)))) + # Custom tooltip text
    geom_bar(stat = "identity", position = "dodge", color = "black") + # Changed to position="dodge" for dodgeed bars
    labs(x = "",
         y = "") +
    scale_y_continuous(labels = scales::comma) + # Format y-axis labels as comma-separated numbers
    theme_minimal() +
    theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
          legend.position = "bottom", # Position legend at the bottom
          plot.title = element_text(hjust = 0.5)) # Center the plot title
  
  # Convert ggplot to plotly, ensuring custom text is used for hover
  ggplotly(p, tooltip = "text", source = "cloudenrolmentRegionPlot") %>%
    layout(hoverlabel = list(bgcolor = "white"),
           margin = list(b = 100)) # Increase bottom margin for x-axis labels
})

output$elec_division_graph <- renderPlotly({
  # Use the reactive filtered data
  
  data_to_display <- filtered_cloud_region_v3() %>%
    select(BASIC.DIVISION,GRID_ELEC,OFFGRID_ELEC,COMBINED_ELEC,NO_ELEC_) %>% 
    mutate(across(2:last_col(), as.numeric)) %>% 
    mutate(BASIC.DIVISION = as.character(BASIC.DIVISION)) %>%
    filter(BASIC.DIVISION != "") %>% 
    group_by(BASIC.DIVISION) %>%
    summarise(across(everything(), sum, na.rm = TRUE))  %>% 
    pivot_longer(
      cols = 2:last_col(), # Specifies the columns to pivot
      names_to = "Sections", # The new column to hold the original column names
      values_to = "Count" # The new column to hold the values
    )
  
  current_filtered_data <- data_to_display
  
  # --- Empty Data Handling ---
  if (nrow(current_filtered_data) == 0) {
    return(ggplotly(ggplot() +
                      annotate("text", x = 0.5, y = 0.5, label = "No data for selected regions/divisions") +
                      theme_void()))
  }
  
  # Prepare the data for plotting
  plot_data <- data_to_display
  
  # Create the ggplot
  p <- ggplot(plot_data,
              aes(x = Sections, # Reorder regions based on overall total count for the region
                  y = Count,
                  fill = Sections, # Fill by Clustering.Status for coloring and consistent order
                  text = paste("Region: ", BASIC.DIVISION,
                               "<br>Section: ", Sections,
                               "<br>Count: ", scales::comma(Count)))) + # Custom tooltip text
    geom_bar(stat = "identity", position = "dodge", color = "black") + # Changed to position="dodge" for dodgeed bars
    labs(x = "",
         y = "") +
    scale_y_continuous(labels = scales::comma) + # Format y-axis labels as comma-separated numbers
    theme_minimal() +
    theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
          legend.position = "bottom", # Position legend at the bottom
          plot.title = element_text(hjust = 0.5)) +
    facet_wrap(~BASIC.DIVISION) + coord_flip()+
    labs(x = NULL, y = NULL, fill = NULL)# Center the plot title # Center the plot title
  
  # Convert ggplot to plotly, ensuring custom text is used for hover
  ggplotly(p, tooltip = "text", source = "cloudenrolmentDivisionPlot") %>%
    layout(hoverlabel = list(bgcolor = "white"),
           margin = list(b = 100)) # Increase bottom margin for x-axis labels
})

output$internet_data_table <- DT::renderDT({
  
  data_to_display <- filtered_cloud_region_v3() %>%
    select(BASIC.REGION,BASIC.DIVISION,Broadband_MOOE,WiFi_Plan_MOOE,Mobile_Data,No_Internet_Available) %>% 
    mutate(across(3:last_col(), as.numeric)) %>% 
    mutate(BASIC.DIVISION = as.character(BASIC.DIVISION)) %>%
    filter(BASIC.DIVISION != "") %>% 
    group_by(BASIC.REGION,BASIC.DIVISION) %>%
    summarise(across(everything(), sum, na.rm = TRUE))
  
  # You might want to add a check for NULL or empty data if filtered_school_data_division()
  # could return such states and you want to display a message or an empty table.
  if (is.null(data_to_display) || nrow(data_to_display) == 0) {
    return(DT::datatable(
      data.frame("Message" = "No data available based on current selection."),
      options = list(dom = 't'), # 't' hides all controls, showing only the table body
      rownames = FALSE
    ))
  }
  
  DT::datatable(
    data_to_display,
    options = list(pageLength = 10, scrollX = TRUE),
    filter = 'top',
    selection = 'multiple',
    rownames = FALSE
  )
})

output$internet_regional_graph <- renderPlotly({
  # Use the reactive filtered data
  
  data_to_display <- cloud_v3 %>%
    select(BASIC.REGION,Broadband_MOOE,WiFi_Plan_MOOE,Mobile_Data,No_Internet_Available) %>% 
    mutate(across(2:last_col(), as.numeric)) %>% 
    mutate(BASIC.REGION = as.character(BASIC.REGION)) %>%
    filter(BASIC.REGION != "") %>% 
    group_by(BASIC.REGION) %>%
    summarise(across(everything(), sum, na.rm = TRUE))  %>% 
    pivot_longer(
      cols = 2:last_col(), # Specifies the columns to pivot
      names_to = "Sections", # The new column to hold the original column names
      values_to = "Count" # The new column to hold the values
    )
  
  current_filtered_data <- data_to_display
  
  # --- Empty Data Handling ---
  if (nrow(current_filtered_data) == 0) {
    return(ggplotly(ggplot() +
                      annotate("text", x = 0.5, y = 0.5, label = "No data for selected regions/divisions") +
                      theme_void()))
  }
  
  # Prepare the data for plotting
  plot_data <- data_to_display
  
  # Create the ggplot
  p <- ggplot(plot_data,
              aes(x = BASIC.REGION, # Reorder regions based on overall total count for the region
                  y = Count,
                  fill = Sections, # Fill by Clustering.Status for coloring and consistent order
                  text = paste("Region: ", BASIC.REGION,
                               "<br>Section: ", Sections,
                               "<br>Count: ", scales::comma(Count)))) + # Custom tooltip text
    geom_bar(stat = "identity", position = "dodge", color = "black") + # Changed to position="dodge" for dodgeed bars
    labs(x = "",
         y = "") +
    scale_y_continuous(labels = scales::comma) + # Format y-axis labels as comma-separated numbers
    theme_minimal() +
    theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
          legend.position = "bottom", # Position legend at the bottom
          plot.title = element_text(hjust = 0.5)) # Center the plot title
  
  # Convert ggplot to plotly, ensuring custom text is used for hover
  ggplotly(p, tooltip = "text", source = "cloudenrolmentRegionPlot") %>%
    layout(hoverlabel = list(bgcolor = "white"),
           margin = list(b = 100)) # Increase bottom margin for x-axis labels
})

output$internet_division_graph <- renderPlotly({
  # Use the reactive filtered data
  
  data_to_display <- filtered_cloud_region_v3() %>%
    select(BASIC.DIVISION,Broadband_MOOE,WiFi_Plan_MOOE,Mobile_Data,No_Internet_Available) %>% 
    mutate(across(2:last_col(), as.numeric)) %>% 
    mutate(BASIC.DIVISION = as.character(BASIC.DIVISION)) %>%
    filter(BASIC.DIVISION != "") %>% 
    group_by(BASIC.DIVISION) %>%
    summarise(across(everything(), sum, na.rm = TRUE))  %>% 
    pivot_longer(
      cols = 2:last_col(), # Specifies the columns to pivot
      names_to = "Sections", # The new column to hold the original column names
      values_to = "Count" # The new column to hold the values
    )
  
  current_filtered_data <- data_to_display
  
  # --- Empty Data Handling ---
  if (nrow(current_filtered_data) == 0) {
    return(ggplotly(ggplot() +
                      annotate("text", x = 0.5, y = 0.5, label = "No data for selected regions/divisions") +
                      theme_void()))
  }
  
  # Prepare the data for plotting
  plot_data <- data_to_display
  
  # Create the ggplot
  p <- ggplot(plot_data,
              aes(x = Sections, # Reorder regions based on overall total count for the region
                  y = Count,
                  fill = Sections, # Fill by Clustering.Status for coloring and consistent order
                  text = paste("Region: ", BASIC.DIVISION,
                               "<br>Section: ", Sections,
                               "<br>Count: ", scales::comma(Count)))) + # Custom tooltip text
    geom_bar(stat = "identity", position = "dodge", color = "black") + # Changed to position="dodge" for dodgeed bars
    labs(x = "",
         y = "") +
    scale_y_continuous(labels = scales::comma) + # Format y-axis labels as comma-separated numbers
    theme_minimal() +
    theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
          legend.position = "bottom", # Position legend at the bottom
          plot.title = element_text(hjust = 0.5)) +
    facet_wrap(~BASIC.DIVISION) + coord_flip()+
    labs(x = NULL, y = NULL, fill = NULL)# Center the plot title # Center the plot title
  
  # Convert ggplot to plotly, ensuring custom text is used for hover
  ggplotly(p, tooltip = "text", source = "cloudenrolmentDivisionPlot") %>%
    layout(hoverlabel = list(bgcolor = "white"),
           margin = list(b = 100)) # Increase bottom margin for x-axis labels
})

output$internet_usage_data_table <- DT::renderDT({
  
  data_to_display <- filtered_cloud_region_v3() %>%
    select(BASIC.REGION,BASIC.DIVISION,Combination_Internet,Instructional_Internet,Administrative_Internet,None_Internet) %>% 
    mutate(across(3:last_col(), as.numeric)) %>% 
    mutate(BASIC.DIVISION = as.character(BASIC.DIVISION)) %>%
    filter(BASIC.DIVISION != "") %>% 
    group_by(BASIC.REGION,BASIC.DIVISION) %>%
    summarise(across(everything(), sum, na.rm = TRUE))
  
  # You might want to add a check for NULL or empty data if filtered_school_data_division()
  # could return such states and you want to display a message or an empty table.
  if (is.null(data_to_display) || nrow(data_to_display) == 0) {
    return(DT::datatable(
      data.frame("Message" = "No data available based on current selection."),
      options = list(dom = 't'), # 't' hides all controls, showing only the table body
      rownames = FALSE
    ))
  }
  
  DT::datatable(
    data_to_display,
    options = list(pageLength = 10, scrollX = TRUE),
    filter = 'top',
    selection = 'multiple',
    rownames = FALSE
  )
})

output$internet_usage_regional_graph <- renderPlotly({
  # Use the reactive filtered data
  
  data_to_display <- cloud_v3 %>%
    select(BASIC.REGION,Combination_Internet,Instructional_Internet,Administrative_Internet,None_Internet) %>% 
    mutate(across(2:last_col(), as.numeric)) %>% 
    mutate(BASIC.REGION = as.character(BASIC.REGION)) %>%
    filter(BASIC.REGION != "") %>% 
    group_by(BASIC.REGION) %>%
    summarise(across(everything(), sum, na.rm = TRUE))  %>% 
    pivot_longer(
      cols = 2:last_col(), # Specifies the columns to pivot
      names_to = "Sections", # The new column to hold the original column names
      values_to = "Count" # The new column to hold the values
    )
  
  current_filtered_data <- data_to_display
  
  # --- Empty Data Handling ---
  if (nrow(current_filtered_data) == 0) {
    return(ggplotly(ggplot() +
                      annotate("text", x = 0.5, y = 0.5, label = "No data for selected regions/divisions") +
                      theme_void()))
  }
  
  # Prepare the data for plotting
  plot_data <- data_to_display
  
  # Create the ggplot
  p <- ggplot(plot_data,
              aes(x = BASIC.REGION, # Reorder regions based on overall total count for the region
                  y = Count,
                  fill = Sections, # Fill by Clustering.Status for coloring and consistent order
                  text = paste("Region: ", BASIC.REGION,
                               "<br>Section: ", Sections,
                               "<br>Count: ", scales::comma(Count)))) + # Custom tooltip text
    geom_bar(stat = "identity", position = "dodge", color = "black") + # Changed to position="dodge" for dodgeed bars
    labs(x = "",
         y = "") +
    scale_y_continuous(labels = scales::comma) + # Format y-axis labels as comma-separated numbers
    theme_minimal() +
    theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
          legend.position = "bottom", # Position legend at the bottom
          plot.title = element_text(hjust = 0.5)) # Center the plot title
  
  # Convert ggplot to plotly, ensuring custom text is used for hover
  ggplotly(p, tooltip = "text", source = "cloudenrolmentRegionPlot") %>%
    layout(hoverlabel = list(bgcolor = "white"),
           margin = list(b = 100)) # Increase bottom margin for x-axis labels
})

output$internet_usage_division_graph <- renderPlotly({
  # Use the reactive filtered data
  
  data_to_display <- filtered_cloud_region_v3() %>%
    select(BASIC.DIVISION,Combination_Internet,Instructional_Internet,Administrative_Internet,None_Internet) %>% 
    mutate(across(2:last_col(), as.numeric)) %>% 
    mutate(BASIC.DIVISION = as.character(BASIC.DIVISION)) %>%
    filter(BASIC.DIVISION != "") %>% 
    group_by(BASIC.DIVISION) %>%
    summarise(across(everything(), sum, na.rm = TRUE))  %>% 
    pivot_longer(
      cols = 2:last_col(), # Specifies the columns to pivot
      names_to = "Sections", # The new column to hold the original column names
      values_to = "Count" # The new column to hold the values
    )
  
  current_filtered_data <- data_to_display
  
  # --- Empty Data Handling ---
  if (nrow(current_filtered_data) == 0) {
    return(ggplotly(ggplot() +
                      annotate("text", x = 0.5, y = 0.5, label = "No data for selected regions/divisions") +
                      theme_void()))
  }
  
  # Prepare the data for plotting
  plot_data <- data_to_display
  
  # Create the ggplot
  p <- ggplot(plot_data,
              aes(x = Sections, # Reorder regions based on overall total count for the region
                  y = Count,
                  fill = Sections, # Fill by Clustering.Status for coloring and consistent order
                  text = paste("Region: ", BASIC.DIVISION,
                               "<br>Section: ", Sections,
                               "<br>Count: ", scales::comma(Count)))) + # Custom tooltip text
    geom_bar(stat = "identity", position = "dodge", color = "black") + # Changed to position="dodge" for dodgeed bars
    labs(x = "",
         y = "") +
    scale_y_continuous(labels = scales::comma) + # Format y-axis labels as comma-separated numbers
    theme_minimal() +
    theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
          legend.position = "bottom", # Position legend at the bottom
          plot.title = element_text(hjust = 0.5)) +
    facet_wrap(~BASIC.DIVISION) + coord_flip()+
    labs(x = NULL, y = NULL, fill = NULL)# Center the plot title # Center the plot title
  
  # Convert ggplot to plotly, ensuring custom text is used for hover
  ggplotly(p, tooltip = "text", source = "cloudenrolmentDivisionPlot") %>%
    layout(hoverlabel = list(bgcolor = "white"),
           margin = list(b = 100)) # Increase bottom margin for x-axis labels
})

output$water_data_table <- DT::renderDT({
  
  data_to_display <- filtered_cloud_region_v3() %>%
    select(BASIC.REGION,BASIC.DIVISION,GROUNDWATER_WATER,BOTTLED_WATER,MWSS_WATER,WELL_WATER,LOCAL_WATER,SURFACE_WATER,PIPEDL3_WATER) %>% 
    mutate(across(3:last_col(), as.numeric)) %>% 
    mutate(BASIC.DIVISION = as.character(BASIC.DIVISION)) %>%
    filter(BASIC.DIVISION != "") %>% 
    group_by(BASIC.REGION,BASIC.DIVISION) %>%
    summarise(across(everything(), sum, na.rm = TRUE))
  
  # You might want to add a check for NULL or empty data if filtered_school_data_division()
  # could return such states and you want to display a message or an empty table.
  if (is.null(data_to_display) || nrow(data_to_display) == 0) {
    return(DT::datatable(
      data.frame("Message" = "No data available based on current selection."),
      options = list(dom = 't'), # 't' hides all controls, showing only the table body
      rownames = FALSE
    ))
  }
  
  DT::datatable(
    data_to_display,
    options = list(pageLength = 10, scrollX = TRUE),
    filter = 'top',
    selection = 'multiple',
    rownames = FALSE
  )
})

output$water_regional_graph <- renderPlotly({
  # Use the reactive filtered data
  
  data_to_display <- cloud_v3 %>%
    select(BASIC.REGION,GROUNDWATER_WATER,BOTTLED_WATER,MWSS_WATER,WELL_WATER,LOCAL_WATER,SURFACE_WATER,PIPEDL3_WATER) %>% 
    mutate(across(2:last_col(), as.numeric)) %>% 
    mutate(BASIC.REGION = as.character(BASIC.REGION)) %>%
    filter(BASIC.REGION != "") %>% 
    group_by(BASIC.REGION) %>%
    summarise(across(everything(), sum, na.rm = TRUE))  %>% 
    pivot_longer(
      cols = 2:last_col(), # Specifies the columns to pivot
      names_to = "Sections", # The new column to hold the original column names
      values_to = "Count" # The new column to hold the values
    )
  
  current_filtered_data <- data_to_display
  
  # --- Empty Data Handling ---
  if (nrow(current_filtered_data) == 0) {
    return(ggplotly(ggplot() +
                      annotate("text", x = 0.5, y = 0.5, label = "No data for selected regions/divisions") +
                      theme_void()))
  }
  
  # Prepare the data for plotting
  plot_data <- data_to_display
  
  # Create the ggplot
  p <- ggplot(plot_data,
              aes(x = BASIC.REGION, # Reorder regions based on overall total count for the region
                  y = Count,
                  fill = Sections, # Fill by Clustering.Status for coloring and consistent order
                  text = paste("Region: ", BASIC.REGION,
                               "<br>Section: ", Sections,
                               "<br>Count: ", scales::comma(Count)))) + # Custom tooltip text
    geom_bar(stat = "identity", position = "dodge", color = "black") + # Changed to position="dodge" for dodgeed bars
    labs(x = "",
         y = "") +
    scale_y_continuous(labels = scales::comma) + # Format y-axis labels as comma-separated numbers
    theme_minimal() +
    theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
          legend.position = "bottom", # Position legend at the bottom
          plot.title = element_text(hjust = 0.5)) # Center the plot title
  
  # Convert ggplot to plotly, ensuring custom text is used for hover
  ggplotly(p, tooltip = "text", source = "cloudenrolmentRegionPlot") %>%
    layout(hoverlabel = list(bgcolor = "white"),
           margin = list(b = 100)) # Increase bottom margin for x-axis labels
})

output$water_division_graph <- renderPlotly({
  # Use the reactive filtered data
  
  data_to_display <- filtered_cloud_region_v3() %>%
    select(BASIC.DIVISION,GROUNDWATER_WATER,BOTTLED_WATER,MWSS_WATER,WELL_WATER,LOCAL_WATER,SURFACE_WATER,PIPEDL3_WATER) %>% 
    mutate(across(2:last_col(), as.numeric)) %>% 
    mutate(BASIC.DIVISION = as.character(BASIC.DIVISION)) %>%
    filter(BASIC.DIVISION != "") %>% 
    group_by(BASIC.DIVISION) %>%
    summarise(across(everything(), sum, na.rm = TRUE))  %>% 
    pivot_longer(
      cols = 2:last_col(), # Specifies the columns to pivot
      names_to = "Sections", # The new column to hold the original column names
      values_to = "Count" # The new column to hold the values
    )
  
  current_filtered_data <- data_to_display
  
  # --- Empty Data Handling ---
  if (nrow(current_filtered_data) == 0) {
    return(ggplotly(ggplot() +
                      annotate("text", x = 0.5, y = 0.5, label = "No data for selected regions/divisions") +
                      theme_void()))
  }
  
  # Prepare the data for plotting
  plot_data <- data_to_display
  
  # Create the ggplot
  p <- ggplot(plot_data,
              aes(x = Sections, # Reorder regions based on overall total count for the region
                  y = Count,
                  fill = Sections, # Fill by Clustering.Status for coloring and consistent order
                  text = paste("Region: ", BASIC.DIVISION,
                               "<br>Section: ", Sections,
                               "<br>Count: ", scales::comma(Count)))) + # Custom tooltip text
    geom_bar(stat = "identity", position = "dodge", color = "black") + # Changed to position="dodge" for dodgeed bars
    labs(x = "",
         y = "") +
    scale_y_continuous(labels = scales::comma) + # Format y-axis labels as comma-separated numbers
    theme_minimal() +
    theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
          legend.position = "bottom", # Position legend at the bottom
          plot.title = element_text(hjust = 0.5)) +
    facet_wrap(~BASIC.DIVISION) + coord_flip()+
    labs(x = NULL, y = NULL, fill = NULL)# Center the plot title # Center the plot title
  
  # Convert ggplot to plotly, ensuring custom text is used for hover
  ggplotly(p, tooltip = "text", source = "cloudenrolmentDivisionPlot") %>%
    layout(hoverlabel = list(bgcolor = "white"),
           margin = list(b = 100)) # Increase bottom margin for x-axis labels
})

output$adm_data_table <- DT::renderDT({
  
  data_to_display <- filtered_cloud_region_v2() %>%
    select(BASIC.REGION,BASIC.DIVISION, contains("ALTERNATIVE.ADM")) %>% 
    mutate(across(3:last_col(), as.numeric)) %>% 
    mutate(BASIC.DIVISION = as.character(BASIC.DIVISION)) %>%
    filter(BASIC.DIVISION != "") %>% 
    group_by(BASIC.REGION,BASIC.DIVISION) %>%
    summarise(across(everything(), sum, na.rm = TRUE))
  
  # You might want to add a check for NULL or empty data if filtered_school_data_division()
  # could return such states and you want to display a message or an empty table.
  if (is.null(data_to_display) || nrow(data_to_display) == 0) {
    return(DT::datatable(
      data.frame("Message" = "No data available based on current selection."),
      options = list(dom = 't'), # 't' hides all controls, showing only the table body
      rownames = FALSE
    ))
  }
  
  DT::datatable(
    data_to_display,
    options = list(pageLength = 10, scrollX = TRUE),
    filter = 'top',
    selection = 'multiple',
    rownames = FALSE
  )
})

output$adm_regional_graph <- renderPlotly({
  # Use the reactive filtered data
  
  data_to_display <- cloud_v2 %>%
    select(BASIC.REGION, contains("ALTERNATIVE.ADM")) %>% 
    mutate(across(2:last_col(), as.numeric)) %>% 
    mutate(BASIC.REGION = as.character(BASIC.REGION)) %>%
    filter(BASIC.REGION != "") %>% 
    group_by(BASIC.REGION) %>%
    summarise(across(everything(), sum, na.rm = TRUE))  %>% 
    pivot_longer(
      cols = 2:last_col(), # Specifies the columns to pivot
      names_to = "Sections", # The new column to hold the original column names
      values_to = "Count" # The new column to hold the values
    )
  
  current_filtered_data <- data_to_display
  
  # --- Empty Data Handling ---
  if (nrow(current_filtered_data) == 0) {
    return(ggplotly(ggplot() +
                      annotate("text", x = 0.5, y = 0.5, label = "No data for selected regions/divisions") +
                      theme_void()))
  }
  
  # Prepare the data for plotting
  plot_data <- data_to_display
  
  # Create the ggplot
  p <- ggplot(plot_data,
              aes(x = BASIC.REGION, # Reorder regions based on overall total count for the region
                  y = Count,
                  fill = Sections, # Fill by Clustering.Status for coloring and consistent order
                  text = paste("Region: ", BASIC.REGION,
                               "<br>Section: ", Sections,
                               "<br>Count: ", scales::comma(Count)))) + # Custom tooltip text
    geom_bar(stat = "identity", position = "dodge", color = "black") + # Changed to position="dodge" for dodgeed bars
    labs(x = "",
         y = "") +
    scale_y_continuous(labels = scales::comma) + # Format y-axis labels as comma-separated numbers
    theme_minimal() +
    theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
          legend.position = "bottom", # Position legend at the bottom
          plot.title = element_text(hjust = 0.5)) # Center the plot title
  
  # Convert ggplot to plotly, ensuring custom text is used for hover
  ggplotly(p, tooltip = "text", source = "cloudenrolmentRegionPlot") %>%
    layout(hoverlabel = list(bgcolor = "white"),
           margin = list(b = 100)) # Increase bottom margin for x-axis labels
})

output$adm_division_graph <- renderPlotly({
  # Use the reactive filtered data
  
  data_to_display <- filtered_cloud_region_v2() %>%
    select(BASIC.DIVISION, contains("ALTERNATIVE.ADM")) %>% 
    mutate(across(2:last_col(), as.numeric)) %>% 
    mutate(BASIC.DIVISION = as.character(BASIC.DIVISION)) %>%
    filter(BASIC.DIVISION != "") %>% 
    group_by(BASIC.DIVISION) %>%
    summarise(across(everything(), sum, na.rm = TRUE))  %>% 
    pivot_longer(
      cols = 2:last_col(), # Specifies the columns to pivot
      names_to = "Sections", # The new column to hold the original column names
      values_to = "Count" # The new column to hold the values
    )
  
  current_filtered_data <- data_to_display
  
  # --- Empty Data Handling ---
  if (nrow(current_filtered_data) == 0) {
    return(ggplotly(ggplot() +
                      annotate("text", x = 0.5, y = 0.5, label = "No data for selected regions/divisions") +
                      theme_void()))
  }
  
  # Prepare the data for plotting
  plot_data <- data_to_display
  
  # Create the ggplot
  p <- ggplot(plot_data,
              aes(x = Sections, # Reorder regions based on overall total count for the region
                  y = Count,
                  fill = Sections, # Fill by Clustering.Status for coloring and consistent order
                  text = paste("Region: ", BASIC.DIVISION,
                               "<br>Section: ", Sections,
                               "<br>Count: ", scales::comma(Count)))) + # Custom tooltip text
    geom_bar(stat = "identity", position = "dodge", color = "black") + # Changed to position="dodge" for dodgeed bars
    labs(x = "",
         y = "") +
    scale_y_continuous(labels = scales::comma) + # Format y-axis labels as comma-separated numbers
    theme_minimal() +
    theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
          legend.position = "bottom", # Position legend at the bottom
          plot.title = element_text(hjust = 0.5)) +
    facet_wrap(~BASIC.DIVISION) + coord_flip()+
    labs(x = NULL, y = NULL, fill = NULL)# Center the plot title # Center the plot title
  
  # Convert ggplot to plotly, ensuring custom text is used for hover
  ggplotly(p, tooltip = "text", source = "cloudenrolmentDivisionPlot") %>%
    layout(hoverlabel = list(bgcolor = "white"),
           margin = list(b = 100)) # Increase bottom margin for x-axis labels
})

output$nat_data_table <- DT::renderDT({
  
  data_to_display <- filtered_cloud_region_v2() %>%
    select(BASIC.REGION,BASIC.DIVISION, contains("NAT.K12.CURRICULUM.ALIGN"), -"NAT.K12.CURRICULUM.ALIGN_") %>% 
    mutate(across(3:last_col(), as.numeric)) %>% 
    mutate(BASIC.DIVISION = as.character(BASIC.DIVISION)) %>%
    filter(BASIC.DIVISION != "") %>% 
    group_by(BASIC.REGION,BASIC.DIVISION) %>%
    summarise(across(everything(), sum, na.rm = TRUE))
  
  # You might want to add a check for NULL or empty data if filtered_school_data_division()
  # could return such states and you want to display a message or an empty table.
  if (is.null(data_to_display) || nrow(data_to_display) == 0) {
    return(DT::datatable(
      data.frame("Message" = "No data available based on current selection."),
      options = list(dom = 't'), # 't' hides all controls, showing only the table body
      rownames = FALSE
    ))
  }
  
  DT::datatable(
    data_to_display,
    options = list(pageLength = 10, scrollX = TRUE),
    filter = 'top',
    selection = 'multiple',
    rownames = FALSE
  )
})

output$nat_regional_graph <- renderPlotly({
  # Use the reactive filtered data
  
  data_to_display <- cloud_v2 %>%
    select(BASIC.REGION, contains("NAT.K12.CURRICULUM.ALIGN"), -"NAT.K12.CURRICULUM.ALIGN_") %>% 
    mutate(across(2:last_col(), as.numeric)) %>% 
    mutate(BASIC.REGION = as.character(BASIC.REGION)) %>%
    filter(BASIC.REGION != "") %>% 
    group_by(BASIC.REGION) %>%
    summarise(across(everything(), sum, na.rm = TRUE))  %>% 
    pivot_longer(
      cols = 2:last_col(), # Specifies the columns to pivot
      names_to = "Sections", # The new column to hold the original column names
      values_to = "Count" # The new column to hold the values
    )
  
  current_filtered_data <- data_to_display
  
  # --- Empty Data Handling ---
  if (nrow(current_filtered_data) == 0) {
    return(ggplotly(ggplot() +
                      annotate("text", x = 0.5, y = 0.5, label = "No data for selected regions/divisions") +
                      theme_void()))
  }
  
  # Prepare the data for plotting
  plot_data <- data_to_display
  
  # Create the ggplot
  p <- ggplot(plot_data,
              aes(x = BASIC.REGION, # Reorder regions based on overall total count for the region
                  y = Count,
                  fill = Sections, # Fill by Clustering.Status for coloring and consistent order
                  text = paste("Region: ", BASIC.REGION,
                               "<br>Section: ", Sections,
                               "<br>Count: ", scales::comma(Count)))) + # Custom tooltip text
    geom_bar(stat = "identity", position = "dodge", color = "black") + # Changed to position="dodge" for dodgeed bars
    labs(x = "",
         y = "") +
    scale_y_continuous(labels = scales::comma) + # Format y-axis labels as comma-separated numbers
    theme_minimal() +
    theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
          legend.position = "bottom", # Position legend at the bottom
          plot.title = element_text(hjust = 0.5)) # Center the plot title
  
  # Convert ggplot to plotly, ensuring custom text is used for hover
  ggplotly(p, tooltip = "text", source = "cloudenrolmentRegionPlot") %>%
    layout(hoverlabel = list(bgcolor = "white"),
           margin = list(b = 100)) # Increase bottom margin for x-axis labels
})

output$nat_division_graph <- renderPlotly({
  # Use the reactive filtered data
  
  data_to_display <- filtered_cloud_region_v2() %>%
    select(BASIC.DIVISION, contains("NAT.K12.CURRICULUM.ALIGN"), -"NAT.K12.CURRICULUM.ALIGN_") %>% 
    mutate(across(2:last_col(), as.numeric)) %>% 
    mutate(BASIC.DIVISION = as.character(BASIC.DIVISION)) %>%
    filter(BASIC.DIVISION != "") %>% 
    group_by(BASIC.DIVISION) %>%
    summarise(across(everything(), sum, na.rm = TRUE))  %>% 
    pivot_longer(
      cols = 2:last_col(), # Specifies the columns to pivot
      names_to = "Sections", # The new column to hold the original column names
      values_to = "Count" # The new column to hold the values
    )
  
  current_filtered_data <- data_to_display
  
  # --- Empty Data Handling ---
  if (nrow(current_filtered_data) == 0) {
    return(ggplotly(ggplot() +
                      annotate("text", x = 0.5, y = 0.5, label = "No data for selected regions/divisions") +
                      theme_void()))
  }
  
  # Prepare the data for plotting
  plot_data <- data_to_display
  
  # Create the ggplot
  p <- ggplot(plot_data,
              aes(x = Sections, # Reorder regions based on overall total count for the region
                  y = Count,
                  fill = Sections, # Fill by Clustering.Status for coloring and consistent order
                  text = paste("Region: ", BASIC.DIVISION,
                               "<br>Section: ", Sections,
                               "<br>Count: ", scales::comma(Count)))) + # Custom tooltip text
    geom_bar(stat = "identity", position = "dodge", color = "black") + # Changed to position="dodge" for dodgeed bars
    labs(x = "",
         y = "") +
    scale_y_continuous(labels = scales::comma) + # Format y-axis labels as comma-separated numbers
    theme_minimal() +
    theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
          legend.position = "bottom", # Position legend at the bottom
          plot.title = element_text(hjust = 0.5)) +
    facet_wrap(~BASIC.DIVISION) + coord_flip()+
    labs(x = NULL, y = NULL, fill = NULL)# Center the plot title # Center the plot title
  
  # Convert ggplot to plotly, ensuring custom text is used for hover
  ggplotly(p, tooltip = "text", source = "cloudenrolmentDivisionPlot") %>%
    layout(hoverlabel = list(bgcolor = "white"),
           margin = list(b = 100)) # Increase bottom margin for x-axis labels
})

output$adm_data_table <- DT::renderDT({
  
  data_to_display <- filtered_cloud_region_v2() %>%
    select(BASIC.REGION,BASIC.DIVISION, contains("ALTERNATIVE.ADM")) %>% 
    mutate(across(3:last_col(), as.numeric)) %>% 
    mutate(BASIC.DIVISION = as.character(BASIC.DIVISION)) %>%
    filter(BASIC.DIVISION != "") %>% 
    group_by(BASIC.REGION,BASIC.DIVISION) %>%
    summarise(across(everything(), sum, na.rm = TRUE))
  
  # You might want to add a check for NULL or empty data if filtered_school_data_division()
  # could return such states and you want to display a message or an empty table.
  if (is.null(data_to_display) || nrow(data_to_display) == 0) {
    return(DT::datatable(
      data.frame("Message" = "No data available based on current selection."),
      options = list(dom = 't'), # 't' hides all controls, showing only the table body
      rownames = FALSE
    ))
  }
  
  DT::datatable(
    data_to_display,
    options = list(pageLength = 10, scrollX = TRUE),
    filter = 'top',
    selection = 'multiple',
    rownames = FALSE
  )
})

output$adm_regional_graph <- renderPlotly({
  # Use the reactive filtered data
  
  data_to_display <- cloud_v2 %>%
    select(BASIC.REGION, contains("ALTERNATIVE.ADM")) %>% 
    mutate(across(2:last_col(), as.numeric)) %>% 
    mutate(BASIC.REGION = as.character(BASIC.REGION)) %>%
    filter(BASIC.REGION != "") %>% 
    group_by(BASIC.REGION) %>%
    summarise(across(everything(), sum, na.rm = TRUE))  %>% 
    pivot_longer(
      cols = 2:last_col(), # Specifies the columns to pivot
      names_to = "Sections", # The new column to hold the original column names
      values_to = "Count" # The new column to hold the values
    )
  
  current_filtered_data <- data_to_display
  
  # --- Empty Data Handling ---
  if (nrow(current_filtered_data) == 0) {
    return(ggplotly(ggplot() +
                      annotate("text", x = 0.5, y = 0.5, label = "No data for selected regions/divisions") +
                      theme_void()))
  }
  
  # Prepare the data for plotting
  plot_data <- data_to_display
  
  # Create the ggplot
  p <- ggplot(plot_data,
              aes(x = BASIC.REGION, # Reorder regions based on overall total count for the region
                  y = Count,
                  fill = Sections, # Fill by Clustering.Status for coloring and consistent order
                  text = paste("Region: ", BASIC.REGION,
                               "<br>Section: ", Sections,
                               "<br>Count: ", scales::comma(Count)))) + # Custom tooltip text
    geom_bar(stat = "identity", position = "dodge", color = "black") + # Changed to position="dodge" for dodgeed bars
    labs(x = "",
         y = "") +
    scale_y_continuous(labels = scales::comma) + # Format y-axis labels as comma-separated numbers
    theme_minimal() +
    theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
          legend.position = "bottom", # Position legend at the bottom
          plot.title = element_text(hjust = 0.5)) # Center the plot title
  
  # Convert ggplot to plotly, ensuring custom text is used for hover
  ggplotly(p, tooltip = "text", source = "cloudenrolmentRegionPlot") %>%
    layout(hoverlabel = list(bgcolor = "white"),
           margin = list(b = 100)) # Increase bottom margin for x-axis labels
})

output$adm_division_graph <- renderPlotly({
  # Use the reactive filtered data
  
  data_to_display <- filtered_cloud_region_v2() %>%
    select(BASIC.DIVISION, contains("ALTERNATIVE.ADM")) %>% 
    mutate(across(2:last_col(), as.numeric)) %>% 
    mutate(BASIC.DIVISION = as.character(BASIC.DIVISION)) %>%
    filter(BASIC.DIVISION != "") %>% 
    group_by(BASIC.DIVISION) %>%
    summarise(across(everything(), sum, na.rm = TRUE))  %>% 
    pivot_longer(
      cols = 2:last_col(), # Specifies the columns to pivot
      names_to = "Sections", # The new column to hold the original column names
      values_to = "Count" # The new column to hold the values
    )
  
  current_filtered_data <- data_to_display
  
  # --- Empty Data Handling ---
  if (nrow(current_filtered_data) == 0) {
    return(ggplotly(ggplot() +
                      annotate("text", x = 0.5, y = 0.5, label = "No data for selected regions/divisions") +
                      theme_void()))
  }
  
  # Prepare the data for plotting
  plot_data <- data_to_display
  
  # Create the ggplot
  p <- ggplot(plot_data,
              aes(x = Sections, # Reorder regions based on overall total count for the region
                  y = Count,
                  fill = Sections, # Fill by Clustering.Status for coloring and consistent order
                  text = paste("Region: ", BASIC.DIVISION,
                               "<br>Section: ", Sections,
                               "<br>Count: ", scales::comma(Count)))) + # Custom tooltip text
    geom_bar(stat = "identity", position = "dodge", color = "black") + # Changed to position="dodge" for dodgeed bars
    labs(x = "",
         y = "") +
    scale_y_continuous(labels = scales::comma) + # Format y-axis labels as comma-separated numbers
    theme_minimal() +
    theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
          legend.position = "bottom", # Position legend at the bottom
          plot.title = element_text(hjust = 0.5)) +
    facet_wrap(~BASIC.DIVISION) + coord_flip()+
    labs(x = NULL, y = NULL, fill = NULL)# Center the plot title # Center the plot title
  
  # Convert ggplot to plotly, ensuring custom text is used for hover
  ggplotly(p, tooltip = "text", source = "cloudenrolmentDivisionPlot") %>%
    layout(hoverlabel = list(bgcolor = "white"),
           margin = list(b = 100)) # Increase bottom margin for x-axis labels
})

output$rf_data_table <- DT::renderDT({
  
  data_to_display <- filtered_cloud_region_v2() %>%
    select(BASIC.REGION,BASIC.DIVISION, contains("QUALI.READING.PROFICIENCY.CHANGE")) %>% 
    mutate(across(3:last_col(), as.numeric)) %>% 
    mutate(BASIC.DIVISION = as.character(BASIC.DIVISION)) %>%
    filter(BASIC.DIVISION != "") %>% 
    group_by(BASIC.REGION,BASIC.DIVISION) %>%
    summarise(across(everything(), sum, na.rm = TRUE))
  
  # You might want to add a check for NULL or empty data if filtered_school_data_division()
  # could return such states and you want to display a message or an empty table.
  if (is.null(data_to_display) || nrow(data_to_display) == 0) {
    return(DT::datatable(
      data.frame("Message" = "No data available based on current selection."),
      options = list(dom = 't'), # 't' hides all controls, showing only the table body
      rownames = FALSE
    ))
  }
  
  DT::datatable(
    data_to_display,
    options = list(pageLength = 10, scrollX = TRUE),
    filter = 'top',
    selection = 'multiple',
    rownames = FALSE
  )
})

output$rf_regional_graph <- renderPlotly({
  # Use the reactive filtered data
  
  data_to_display <- cloud_v2 %>%
    select(BASIC.REGION, contains("QUALI.READING.PROFICIENCY.CHANGE")) %>% 
    mutate(across(2:last_col(), as.numeric)) %>% 
    mutate(BASIC.REGION = as.character(BASIC.REGION)) %>%
    filter(BASIC.REGION != "") %>% 
    group_by(BASIC.REGION) %>%
    summarise(across(everything(), sum, na.rm = TRUE))  %>% 
    pivot_longer(
      cols = 2:last_col(), # Specifies the columns to pivot
      names_to = "Sections", # The new column to hold the original column names
      values_to = "Count" # The new column to hold the values
    )
  
  current_filtered_data <- data_to_display
  
  # --- Empty Data Handling ---
  if (nrow(current_filtered_data) == 0) {
    return(ggplotly(ggplot() +
                      annotate("text", x = 0.5, y = 0.5, label = "No data for selected regions/divisions") +
                      theme_void()))
  }
  
  # Prepare the data for plotting
  plot_data <- data_to_display
  
  # Create the ggplot
  p <- ggplot(plot_data,
              aes(x = BASIC.REGION, # Reorder regions based on overall total count for the region
                  y = Count,
                  fill = Sections, # Fill by Clustering.Status for coloring and consistent order
                  text = paste("Region: ", BASIC.REGION,
                               "<br>Section: ", Sections,
                               "<br>Count: ", scales::comma(Count)))) + # Custom tooltip text
    geom_bar(stat = "identity", position = "dodge", color = "black") + # Changed to position="dodge" for dodgeed bars
    labs(x = "",
         y = "") +
    scale_y_continuous(labels = scales::comma) + # Format y-axis labels as comma-separated numbers
    theme_minimal() +
    theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
          legend.position = "bottom", # Position legend at the bottom
          plot.title = element_text(hjust = 0.5)) # Center the plot title
  
  # Convert ggplot to plotly, ensuring custom text is used for hover
  ggplotly(p, tooltip = "text", source = "cloudenrolmentRegionPlot") %>%
    layout(hoverlabel = list(bgcolor = "white"),
           margin = list(b = 100)) # Increase bottom margin for x-axis labels
})

output$rf_division_graph <- renderPlotly({
  # Use the reactive filtered data
  
  data_to_display <- filtered_cloud_region_v2() %>%
    select(BASIC.DIVISION, contains("QUALI.READING.PROFICIENCY.CHANGE")) %>% 
    mutate(across(2:last_col(), as.numeric)) %>% 
    mutate(BASIC.DIVISION = as.character(BASIC.DIVISION)) %>%
    filter(BASIC.DIVISION != "") %>% 
    group_by(BASIC.DIVISION) %>%
    summarise(across(everything(), sum, na.rm = TRUE))  %>% 
    pivot_longer(
      cols = 2:last_col(), # Specifies the columns to pivot
      names_to = "Sections", # The new column to hold the original column names
      values_to = "Count" # The new column to hold the values
    )
  
  current_filtered_data <- data_to_display
  
  # --- Empty Data Handling ---
  if (nrow(current_filtered_data) == 0) {
    return(ggplotly(ggplot() +
                      annotate("text", x = 0.5, y = 0.5, label = "No data for selected regions/divisions") +
                      theme_void()))
  }
  
  # Prepare the data for plotting
  plot_data <- data_to_display
  
  # Create the ggplot
  p <- ggplot(plot_data,
              aes(x = Sections, # Reorder regions based on overall total count for the region
                  y = Count,
                  fill = Sections, # Fill by Clustering.Status for coloring and consistent order
                  text = paste("Region: ", BASIC.DIVISION,
                               "<br>Section: ", Sections,
                               "<br>Count: ", scales::comma(Count)))) + # Custom tooltip text
    geom_bar(stat = "identity", position = "dodge", color = "black") + # Changed to position="dodge" for dodgeed bars
    labs(x = "",
         y = "") +
    scale_y_continuous(labels = scales::comma) + # Format y-axis labels as comma-separated numbers
    theme_minimal() +
    theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
          legend.position = "bottom", # Position legend at the bottom
          plot.title = element_text(hjust = 0.5)) +
    facet_wrap(~BASIC.DIVISION) + coord_flip()+
    labs(x = NULL, y = NULL, fill = NULL)# Center the plot title # Center the plot title
  
  # Convert ggplot to plotly, ensuring custom text is used for hover
  ggplotly(p, tooltip = "text", source = "cloudenrolmentDivisionPlot") %>%
    layout(hoverlabel = list(bgcolor = "white"),
           margin = list(b = 100)) # Increase bottom margin for x-axis labels
})

output$nat_data_table <- DT::renderDT({
  
  data_to_display <- filtered_cloud_region_v2() %>%
    select(BASIC.REGION,BASIC.DIVISION, contains("NAT.K12.CURRICULUM.ALIGN"), -"NAT.K12.CURRICULUM.ALIGN_") %>% 
    mutate(across(3:last_col(), as.numeric)) %>% 
    mutate(BASIC.DIVISION = as.character(BASIC.DIVISION)) %>%
    filter(BASIC.DIVISION != "") %>% 
    group_by(BASIC.REGION,BASIC.DIVISION) %>%
    summarise(across(everything(), sum, na.rm = TRUE))
  
  # You might want to add a check for NULL or empty data if filtered_school_data_division()
  # could return such states and you want to display a message or an empty table.
  if (is.null(data_to_display) || nrow(data_to_display) == 0) {
    return(DT::datatable(
      data.frame("Message" = "No data available based on current selection."),
      options = list(dom = 't'), # 't' hides all controls, showing only the table body
      rownames = FALSE
    ))
  }
  
  DT::datatable(
    data_to_display,
    options = list(pageLength = 10, scrollX = TRUE),
    filter = 'top',
    selection = 'multiple',
    rownames = FALSE
  )
})

output$nat_regional_graph <- renderPlotly({
  # Use the reactive filtered data
  
  data_to_display <- cloud_v2 %>%
    select(BASIC.REGION, contains("NAT.K12.CURRICULUM.ALIGN"), -"NAT.K12.CURRICULUM.ALIGN_") %>% 
    mutate(across(2:last_col(), as.numeric)) %>% 
    mutate(BASIC.REGION = as.character(BASIC.REGION)) %>%
    filter(BASIC.REGION != "") %>% 
    group_by(BASIC.REGION) %>%
    summarise(across(everything(), sum, na.rm = TRUE))  %>% 
    pivot_longer(
      cols = 2:last_col(), # Specifies the columns to pivot
      names_to = "Sections", # The new column to hold the original column names
      values_to = "Count" # The new column to hold the values
    )
  
  current_filtered_data <- data_to_display
  
  # --- Empty Data Handling ---
  if (nrow(current_filtered_data) == 0) {
    return(ggplotly(ggplot() +
                      annotate("text", x = 0.5, y = 0.5, label = "No data for selected regions/divisions") +
                      theme_void()))
  }
  
  # Prepare the data for plotting
  plot_data <- data_to_display
  
  # Create the ggplot
  p <- ggplot(plot_data,
              aes(x = BASIC.REGION, # Reorder regions based on overall total count for the region
                  y = Count,
                  fill = Sections, # Fill by Clustering.Status for coloring and consistent order
                  text = paste("Region: ", BASIC.REGION,
                               "<br>Section: ", Sections,
                               "<br>Count: ", scales::comma(Count)))) + # Custom tooltip text
    geom_bar(stat = "identity", position = "dodge", color = "black") + # Changed to position="dodge" for dodgeed bars
    labs(x = "",
         y = "") +
    scale_y_continuous(labels = scales::comma) + # Format y-axis labels as comma-separated numbers
    theme_minimal() +
    theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
          legend.position = "bottom", # Position legend at the bottom
          plot.title = element_text(hjust = 0.5)) # Center the plot title
  
  # Convert ggplot to plotly, ensuring custom text is used for hover
  ggplotly(p, tooltip = "text", source = "cloudenrolmentRegionPlot") %>%
    layout(hoverlabel = list(bgcolor = "white"),
           margin = list(b = 100)) # Increase bottom margin for x-axis labels
})

output$nat_division_graph <- renderPlotly({
  # Use the reactive filtered data
  
  data_to_display <- filtered_cloud_region_v2() %>%
    select(BASIC.DIVISION, contains("NAT.K12.CURRICULUM.ALIGN"), -"NAT.K12.CURRICULUM.ALIGN_") %>% 
    mutate(across(2:last_col(), as.numeric)) %>% 
    mutate(BASIC.DIVISION = as.character(BASIC.DIVISION)) %>%
    filter(BASIC.DIVISION != "") %>% 
    group_by(BASIC.DIVISION) %>%
    summarise(across(everything(), sum, na.rm = TRUE))  %>% 
    pivot_longer(
      cols = 2:last_col(), # Specifies the columns to pivot
      names_to = "Sections", # The new column to hold the original column names
      values_to = "Count" # The new column to hold the values
    )
  
  current_filtered_data <- data_to_display
  
  # --- Empty Data Handling ---
  if (nrow(current_filtered_data) == 0) {
    return(ggplotly(ggplot() +
                      annotate("text", x = 0.5, y = 0.5, label = "No data for selected regions/divisions") +
                      theme_void()))
  }
  
  # Prepare the data for plotting
  plot_data <- data_to_display
  
  # Create the ggplot
  p <- ggplot(plot_data,
              aes(x = Sections, # Reorder regions based on overall total count for the region
                  y = Count,
                  fill = Sections, # Fill by Clustering.Status for coloring and consistent order
                  text = paste("Region: ", BASIC.DIVISION,
                               "<br>Section: ", Sections,
                               "<br>Count: ", scales::comma(Count)))) + # Custom tooltip text
    geom_bar(stat = "identity", position = "dodge", color = "black") + # Changed to position="dodge" for dodgeed bars
    labs(x = "",
         y = "") +
    scale_y_continuous(labels = scales::comma) + # Format y-axis labels as comma-separated numbers
    theme_minimal() +
    theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
          legend.position = "bottom", # Position legend at the bottom
          plot.title = element_text(hjust = 0.5)) +
    facet_wrap(~BASIC.DIVISION) + coord_flip()+
    labs(x = NULL, y = NULL, fill = NULL)# Center the plot title # Center the plot title
  
  # Convert ggplot to plotly, ensuring custom text is used for hover
  ggplotly(p, tooltip = "text", source = "cloudenrolmentDivisionPlot") %>%
    layout(hoverlabel = list(bgcolor = "white"),
           margin = list(b = 100)) # Increase bottom margin for x-axis labels
})

output$aral_data_table <- DT::renderDT({
  
  data_to_display <- filtered_cloud_region() %>%
    select(BASIC.REGION,BASIC.DIVISION,ARAL.MATH,ARAL.READING,ARAL.SCIENCE) %>% 
    mutate(across(3:last_col(), as.numeric)) %>% 
    mutate(BASIC.DIVISION = as.character(BASIC.DIVISION)) %>%
    filter(BASIC.DIVISION != "") %>% 
    group_by(BASIC.REGION,BASIC.DIVISION) %>%
    summarise(across(everything(), sum, na.rm = TRUE)) %>% 
    rename(
      "REGION" = BASIC.REGION,
      "DIVISION" = BASIC.DIVISION,
      "PROSPECTIVE ARAL LEARNERS (MATH)" = ARAL.MATH,
      "PROSPECTIVE ARAL LEARNERS (READING)" = ARAL.READING,
      "PROSPECTIVE ARAL LEARNERS (SCIENCE)" = ARAL.SCIENCE)
  
  # You might want to add a check for NULL or empty data if filtered_school_data_division()
  # could return such states and you want to display a message or an empty table.
  if (is.null(data_to_display) || nrow(data_to_display) == 0) {
    return(DT::datatable(
      data.frame("Message" = "No data available based on current selection."),
      options = list(dom = 't'), # 't' hides all controls, showing only the table body
      rownames = FALSE
    ))
  }
  
  DT::datatable(
    data_to_display,
    options = list(pageLength = 10, scrollX = TRUE),
    filter = 'top',
    selection = 'multiple',
    rownames = FALSE
  )
})

output$aral_regional_graph <- renderPlotly({
  # Use the reactive filtered data
  
  data_to_display <- cloud %>%
    select(BASIC.REGION,ARAL.MATH,ARAL.READING,ARAL.SCIENCE) %>% 
    mutate(across(2:last_col(), as.numeric)) %>% 
    mutate(BASIC.REGION = as.character(BASIC.REGION)) %>%
    filter(BASIC.REGION != "") %>% 
    group_by(BASIC.REGION) %>%
    summarise(across(everything(), sum, na.rm = TRUE)) %>% 
    rename(
      "REGION" = BASIC.REGION,
      "PROSPECTIVE ARAL LEARNERS (MATH)" = ARAL.MATH,
      "PROSPECTIVE ARAL LEARNERS (READING)" = ARAL.READING,
      "PROSPECTIVE ARAL LEARNERS (SCIENCE)" = ARAL.SCIENCE)  %>% 
    pivot_longer(
      cols = 2:last_col(), # Specifies the columns to pivot
      names_to = "Sections", # The new column to hold the original column names
      values_to = "Count" # The new column to hold the values
    )
  
  current_filtered_data <- data_to_display
  
  # --- Empty Data Handling ---
  if (nrow(current_filtered_data) == 0) {
    return(ggplotly(ggplot() +
                      annotate("text", x = 0.5, y = 0.5, label = "No data for selected regions/divisions") +
                      theme_void()))
  }
  
  # Prepare the data for plotting
  plot_data <- data_to_display
  
  # Create the ggplot
  p <- ggplot(plot_data,
              aes(x = REGION, # Reorder regions based on overall total count for the region
                  y = Count,
                  fill = Sections, # Fill by Clustering.Status for coloring and consistent order
                  text = paste("Region: ", REGION,
                               "<br>Section: ", Sections,
                               "<br>Count: ", scales::comma(Count)))) + # Custom tooltip text
    geom_bar(stat = "identity", position = "dodge", color = "black") + # Changed to position="dodge" for dodgeed bars
    labs(x = "",
         y = "") +
    scale_y_continuous(labels = scales::comma) + # Format y-axis labels as comma-separated numbers
    theme_minimal() +
    theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
          legend.position = "bottom", # Position legend at the bottom
          plot.title = element_text(hjust = 0.5)) # Center the plot title
  
  # Convert ggplot to plotly, ensuring custom text is used for hover
  ggplotly(p, tooltip = "text", source = "cloudenrolmentRegionPlot") %>%
    layout(hoverlabel = list(bgcolor = "white"),
           margin = list(b = 100)) # Increase bottom margin for x-axis labels
})

output$aral_division_graph <- renderPlotly({
  # Use the reactive filtered data
  
  data_to_display <- filtered_cloud_region() %>%
    select(BASIC.DIVISION,ARAL.MATH,ARAL.READING,ARAL.SCIENCE) %>% 
    mutate(across(2:last_col(), as.numeric)) %>% 
    mutate(BASIC.DIVISION = as.character(BASIC.DIVISION)) %>%
    filter(BASIC.DIVISION != "") %>% 
    group_by(BASIC.DIVISION) %>%
    summarise(across(everything(), sum, na.rm = TRUE)) %>% 
    rename(
      "DIVISION" = BASIC.DIVISION,
      "PROSPECTIVE ARAL LEARNERS (MATH)" = ARAL.MATH,
      "PROSPECTIVE ARAL LEARNERS (READING)" = ARAL.READING,
      "PROSPECTIVE ARAL LEARNERS (SCIENCE)" = ARAL.SCIENCE)  %>% 
    pivot_longer(
      cols = 2:last_col(), # Specifies the columns to pivot
      names_to = "Sections", # The new column to hold the original column names
      values_to = "Count" # The new column to hold the values
    )
  
  current_filtered_data <- data_to_display
  
  # --- Empty Data Handling ---
  if (nrow(current_filtered_data) == 0) {
    return(ggplotly(ggplot() +
                      annotate("text", x = 0.5, y = 0.5, label = "No data for selected regions/divisions") +
                      theme_void()))
  }
  
  # Prepare the data for plotting
  plot_data <- data_to_display
  
  # Create the ggplot
  p <- ggplot(plot_data,
              aes(x = Sections, # Reorder regions based on overall total count for the region
                  y = Count,
                  fill = Sections, # Fill by Clustering.Status for coloring and consistent order
                  text = paste("Region: ", DIVISION,
                               "<br>Section: ", Sections,
                               "<br>Count: ", scales::comma(Count)))) + # Custom tooltip text
    geom_bar(stat = "identity", position = "dodge", color = "black") + # Changed to position="dodge" for dodgeed bars
    scale_y_continuous(labels = scales::comma) + # Format y-axis labels as comma-separated numbers
    theme_minimal() +
    theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
          legend.position = "bottom", # Position legend at the bottom
          plot.title = element_text(hjust = 0.5)) +
    facet_wrap(~DIVISION) + coord_flip()+
    labs(x = NULL, y = NULL, fill = NULL)
# Center the plot title # Center the plot title
  
  # Convert ggplot to plotly, ensuring custom text is used for hover
  ggplotly(p, tooltip = "text", source = "cloudenrolmentDivisionPlot") %>%
    layout(hoverlabel = list(bgcolor = "white"),
           margin = list(b = 100)) # Increase bottom margin for x-axis labels
})

output$bully_data_table <- DT::renderDT({
  
  data_to_display <- filtered_cloud_region() %>%
    select(BASIC.REGION,BASIC.DIVISION,MENTAL.BULLYING.COUNT,MENTAL.CHILDREN.RIGHT.ORIENTATION) %>% 
    mutate(across(3:last_col(), as.numeric)) %>% 
    mutate(BASIC.DIVISION = as.character(BASIC.DIVISION)) %>%
    filter(BASIC.DIVISION != "") %>% 
    group_by(BASIC.REGION,BASIC.DIVISION) %>%
    summarise(across(everything(), sum, na.rm = TRUE)) %>% 
    rename(
      "REGION" = BASIC.REGION,
      "DIVISION" = BASIC.DIVISION,
      "INCIDENCE OF BULLYING" = MENTAL.BULLYING.COUNT,
      "ATTENDANCE OF CHILDRENS' RIGHTS ORIENTATION" = MENTAL.CHILDREN.RIGHT.ORIENTATION)
  
  # You might want to add a check for NULL or empty data if filtered_school_data_division()
  # could return such states and you want to display a message or an empty table.
  if (is.null(data_to_display) || nrow(data_to_display) == 0) {
    return(DT::datatable(
      data.frame("Message" = "No data available based on current selection."),
      options = list(dom = 't'), # 't' hides all controls, showing only the table body
      rownames = FALSE
    ))
  }
  
  DT::datatable(
    data_to_display,
    options = list(pageLength = 10, scrollX = TRUE),
    filter = 'top',
    selection = 'multiple',
    rownames = FALSE
  )
})

output$bully_regional_graph <- renderPlotly({
  # Use the reactive filtered data
  
  data_to_display <- cloud %>%
    select(BASIC.REGION,MENTAL.BULLYING.COUNT,MENTAL.CHILDREN.RIGHT.ORIENTATION) %>% 
    mutate(across(2:last_col(), as.numeric)) %>% 
    mutate(BASIC.REGION = as.character(BASIC.REGION)) %>%
    filter(BASIC.REGION != "") %>% 
    group_by(BASIC.REGION) %>%
    summarise(across(everything(), sum, na.rm = TRUE)) %>% 
    rename(
      "REGION" = BASIC.REGION,
      "INCIDENCE OF BULLYING" = MENTAL.BULLYING.COUNT,
      "ATTENDANCE OF CHILDRENS' RIGHTS ORIENTATION" = MENTAL.CHILDREN.RIGHT.ORIENTATION)  %>% 
    pivot_longer(
      cols = 2:last_col(), # Specifies the columns to pivot
      names_to = "Sections", # The new column to hold the original column names
      values_to = "Count" # The new column to hold the values
    )
  
  current_filtered_data <- data_to_display
  
  # --- Empty Data Handling ---
  if (nrow(current_filtered_data) == 0) {
    return(ggplotly(ggplot() +
                      annotate("text", x = 0.5, y = 0.5, label = "No data for selected regions/divisions") +
                      theme_void()))
  }
  
  # Prepare the data for plotting
  plot_data <- data_to_display
  
  # Create the ggplot
  p <- ggplot(plot_data,
              aes(x = REGION, # Reorder regions based on overall total count for the region
                  y = Count,
                  fill = Sections, # Fill by Clustering.Status for coloring and consistent order
                  text = paste("Region: ", REGION,
                               "<br>Section: ", Sections,
                               "<br>Count: ", scales::comma(Count)))) + # Custom tooltip text
    geom_bar(stat = "identity", position = "dodge", color = "black") + # Changed to position="dodge" for dodgeed bars
    labs(x = "",
         y = "") +
    scale_y_continuous(labels = scales::comma) + # Format y-axis labels as comma-separated numbers
    theme_minimal() +
    theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
          legend.position = "bottom", # Position legend at the bottom
          plot.title = element_text(hjust = 0.5)) # Center the plot title
  
  # Convert ggplot to plotly, ensuring custom text is used for hover
  ggplotly(p, tooltip = "text", source = "cloudenrolmentRegionPlot") %>%
    layout(hoverlabel = list(bgcolor = "white"),
           margin = list(b = 100)) # Increase bottom margin for x-axis labels
})

output$bully_division_graph <- renderPlotly({
  # Use the reactive filtered data
  
  data_to_display <- filtered_cloud_region() %>%
    select(BASIC.DIVISION,MENTAL.BULLYING.COUNT,MENTAL.CHILDREN.RIGHT.ORIENTATION) %>% 
    mutate(across(2:last_col(), as.numeric)) %>% 
    mutate(BASIC.DIVISION = as.character(BASIC.DIVISION)) %>%
    filter(BASIC.DIVISION != "") %>% 
    group_by(BASIC.DIVISION) %>%
    summarise(across(everything(), sum, na.rm = TRUE)) %>% 
    rename(
      "DIVISION" = BASIC.DIVISION,
      "INCIDENCE OF BULLYING" = MENTAL.BULLYING.COUNT,
      "ATTENDANCE OF CHILDRENS' RIGHTS ORIENTATION" = MENTAL.CHILDREN.RIGHT.ORIENTATION)  %>% 
    pivot_longer(
      cols = 2:last_col(), # Specifies the columns to pivot
      names_to = "Sections", # The new column to hold the original column names
      values_to = "Count" # The new column to hold the values
    )
  
  current_filtered_data <- data_to_display
  
  # --- Empty Data Handling ---
  if (nrow(current_filtered_data) == 0) {
    return(ggplotly(ggplot() +
                      annotate("text", x = 0.5, y = 0.5, label = "No data for selected regions/divisions") +
                      theme_void()))
  }
  
  # Prepare the data for plotting
  plot_data <- data_to_display
  
  # Create the ggplot
  p <- ggplot(plot_data,
              aes(x = Sections, # Reorder regions based on overall total count for the region
                  y = Count,
                  fill = Sections, # Fill by Clustering.Status for coloring and consistent order
                  text = paste("Region: ", DIVISION,
                               "<br>Section: ", Sections,
                               "<br>Count: ", scales::comma(Count)))) + # Custom tooltip text
    geom_bar(stat = "identity", position = "dodge", color = "black") + # Changed to position="dodge" for dodgeed bars
    scale_y_continuous(labels = scales::comma) + # Format y-axis labels as comma-separated numbers
    theme_minimal() +
    theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
          legend.position = "bottom", # Position legend at the bottom
          plot.title = element_text(hjust = 0.5)) +
    facet_wrap(~DIVISION) + coord_flip()+
    labs(x = NULL, y = NULL, fill = NULL)
  # Center the plot title # Center the plot title
  
  # Convert ggplot to plotly, ensuring custom text is used for hover
  ggplotly(p, tooltip = "text", source = "cloudenrolmentDivisionPlot") %>%
    layout(hoverlabel = list(bgcolor = "white"),
           margin = list(b = 100)) # Increase bottom margin for x-axis labels
})

output$crla_data_table <- DT::renderDT({
  
  data_to_display <- filtered_cloud_region() %>%
    select(BASIC.REGION,BASIC.DIVISION, CRLA.DEVELOPING, CRLA.LOW, CRLA.TRANSITIONAL, CRLA.HIGH, CRLA.GRADELEVEL) %>% 
    mutate(across(3:last_col(), as.numeric)) %>% 
    mutate(BASIC.DIVISION = as.character(BASIC.DIVISION)) %>%
    filter(BASIC.DIVISION != "") %>% 
    group_by(BASIC.REGION,BASIC.DIVISION) %>%
    summarise(across(everything(), sum, na.rm = TRUE)) %>% 
    rename(
      "REGION" = BASIC.REGION,
      "DIVISION" = BASIC.DIVISION,
      "LOW" = CRLA.LOW,
      "DEVELOPING" = CRLA.DEVELOPING,
      "TRANSITIONAL" = CRLA.TRANSITIONAL,
      "HIGH" = CRLA.HIGH,
      "GRADE LEVEL" = CRLA.GRADELEVEL)
  
  # You might want to add a check for NULL or empty data if filtered_school_data_division()
  # could return such states and you want to display a message or an empty table.
  if (is.null(data_to_display) || nrow(data_to_display) == 0) {
    return(DT::datatable(
      data.frame("Message" = "No data available based on current selection."),
      options = list(dom = 't'), # 't' hides all controls, showing only the table body
      rownames = FALSE
    ))
  }
  
  DT::datatable(
    data_to_display,
    options = list(pageLength = 10, scrollX = TRUE),
    filter = 'top',
    selection = 'multiple',
    rownames = FALSE
  )
})

output$crla_regional_graph <- renderPlotly({
  # Use the reactive filtered data
  
  data_to_display <- cloud %>%
    select(BASIC.REGION, CRLA.DEVELOPING, CRLA.LOW, CRLA.TRANSITIONAL, CRLA.HIGH, CRLA.GRADELEVEL) %>% 
    mutate(across(2:last_col(), as.numeric)) %>% 
    mutate(BASIC.REGION = as.character(BASIC.REGION)) %>%
    filter(BASIC.REGION != "") %>% 
    group_by(BASIC.REGION) %>%
    summarise(across(everything(), sum, na.rm = TRUE)) %>% 
    rename(
      "REGION" = BASIC.REGION,
      "LOW" = CRLA.LOW,
      "DEVELOPING" = CRLA.DEVELOPING,
      "TRANSITIONAL" = CRLA.TRANSITIONAL,
      "HIGH" = CRLA.HIGH,
      "GRADE LEVEL" = CRLA.GRADELEVEL)  %>% 
    pivot_longer(
      cols = 2:last_col(), # Specifies the columns to pivot
      names_to = "Sections", # The new column to hold the original column names
      values_to = "Count" # The new column to hold the values
    )
  
  current_filtered_data <- data_to_display
  
  # --- Empty Data Handling ---
  if (nrow(current_filtered_data) == 0) {
    return(ggplotly(ggplot() +
                      annotate("text", x = 0.5, y = 0.5, label = "No data for selected regions/divisions") +
                      theme_void()))
  }
  
  # Prepare the data for plotting
  plot_data <- data_to_display
  
  # Create the ggplot
  p <- ggplot(plot_data,
              aes(x = REGION, # Reorder regions based on overall total count for the region
                  y = Count,
                  fill = Sections, # Fill by Clustering.Status for coloring and consistent order
                  text = paste("Region: ", REGION,
                               "<br>Section: ", Sections,
                               "<br>Count: ", scales::comma(Count)))) + # Custom tooltip text
    geom_bar(stat = "identity", position = "dodge", color = "black") + # Changed to position="dodge" for dodgeed bars
    labs(x = "",
         y = "") +
    scale_y_continuous(labels = scales::comma) + # Format y-axis labels as comma-separated numbers
    theme_minimal() +
    theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
          legend.position = "bottom", # Position legend at the bottom
          plot.title = element_text(hjust = 0.5)) # Center the plot title
  
  # Convert ggplot to plotly, ensuring custom text is used for hover
  ggplotly(p, tooltip = "text", source = "cloudenrolmentRegionPlot") %>%
    layout(hoverlabel = list(bgcolor = "white"),
           margin = list(b = 100)) # Increase bottom margin for x-axis labels
})

output$crla_division_graph <- renderPlotly({
  # Use the reactive filtered data
  
  data_to_display <- filtered_cloud_region() %>%
    select(BASIC.DIVISION, CRLA.DEVELOPING, CRLA.LOW, CRLA.TRANSITIONAL, CRLA.HIGH, CRLA.GRADELEVEL) %>% 
    mutate(across(2:last_col(), as.numeric)) %>% 
    mutate(BASIC.DIVISION = as.character(BASIC.DIVISION)) %>%
    filter(BASIC.DIVISION != "") %>% 
    group_by(BASIC.DIVISION) %>%
    summarise(across(everything(), sum, na.rm = TRUE)) %>% 
    rename(
      "DIVISION" = BASIC.DIVISION,
      "LOW" = CRLA.LOW,
      "DEVELOPING" = CRLA.DEVELOPING,
      "TRANSITIONAL" = CRLA.TRANSITIONAL,
      "HIGH" = CRLA.HIGH,
      "GRADE LEVEL" = CRLA.GRADELEVEL)  %>% 
    pivot_longer(
      cols = 2:last_col(), # Specifies the columns to pivot
      names_to = "Sections", # The new column to hold the original column names
      values_to = "Count" # The new column to hold the values
    )
  
  current_filtered_data <- data_to_display
  
  # --- Empty Data Handling ---
  if (nrow(current_filtered_data) == 0) {
    return(ggplotly(ggplot() +
                      annotate("text", x = 0.5, y = 0.5, label = "No data for selected regions/divisions") +
                      theme_void()))
  }
  
  # Prepare the data for plotting
  plot_data <- data_to_display
  
  # Create the ggplot
  p <- ggplot(plot_data,
              aes(x = Sections, # Reorder regions based on overall total count for the region
                  y = Count,
                  fill = Sections, # Fill by Clustering.Status for coloring and consistent order
                  text = paste("Region: ", DIVISION,
                               "<br>Section: ", Sections,
                               "<br>Count: ", scales::comma(Count)))) + # Custom tooltip text
    geom_bar(stat = "identity", position = "dodge", color = "black") + # Changed to position="dodge" for dodgeed bars
    scale_y_continuous(labels = scales::comma) + # Format y-axis labels as comma-separated numbers
    theme_minimal() +
    theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
          legend.position = "bottom", # Position legend at the bottom
          plot.title = element_text(hjust = 0.5)) +
    facet_wrap(~DIVISION) + coord_flip()+
    labs(x = NULL, y = NULL, fill = NULL)
# Center the plot title # Center the plot title
  
  # Convert ggplot to plotly, ensuring custom text is used for hover
  ggplotly(p, tooltip = "text", source = "cloudenrolmentDivisionPlot") %>%
    layout(hoverlabel = list(bgcolor = "white"),
           margin = list(b = 100)) # Increase bottom margin for x-axis labels
})

output$philiri_data_table <- DT::renderDT({
  
  data_to_display <- filtered_cloud_region() %>%
    select(BASIC.REGION,BASIC.DIVISION, PHILIRI.FRUSTRATION, PHILIRI.TRANSITIONAL, PHILIRI.INSTRUCTIONAL) %>% 
    mutate(across(3:last_col(), as.numeric)) %>% 
    mutate(BASIC.DIVISION = as.character(BASIC.DIVISION)) %>%
    filter(BASIC.DIVISION != "") %>% 
    group_by(BASIC.REGION,BASIC.DIVISION) %>%
    summarise(across(everything(), sum, na.rm = TRUE)) %>% 
    rename(
      "REGION" = BASIC.REGION,
      "DIVISION" = BASIC.DIVISION,
      "FRUSTRATION" = PHILIRI.FRUSTRATION,
      "TRANSITIONAL" = PHILIRI.TRANSITIONAL,
      "INSTRUCTIONAL" = PHILIRI.INSTRUCTIONAL)
  
  # You might want to add a check for NULL or empty data if filtered_school_data_division()
  # could return such states and you want to display a message or an empty table.
  if (is.null(data_to_display) || nrow(data_to_display) == 0) {
    return(DT::datatable(
      data.frame("Message" = "No data available based on current selection."),
      options = list(dom = 't'), # 't' hides all controls, showing only the table body
      rownames = FALSE
    ))
  }
  
  DT::datatable(
    data_to_display,
    options = list(pageLength = 10, scrollX = TRUE),
    filter = 'top',
    selection = 'multiple',
    rownames = FALSE
  )
})

output$philiri_regional_graph <- renderPlotly({
  # Use the reactive filtered data
  
  data_to_display <- cloud %>%
    select(BASIC.REGION, PHILIRI.FRUSTRATION, PHILIRI.TRANSITIONAL, PHILIRI.INSTRUCTIONAL) %>% 
    mutate(across(2:last_col(), as.numeric)) %>% 
    mutate(BASIC.REGION = as.character(BASIC.REGION)) %>%
    filter(BASIC.REGION != "") %>% 
    group_by(BASIC.REGION) %>%
    summarise(across(everything(), sum, na.rm = TRUE)) %>% 
    rename(
      "REGION" = BASIC.REGION,
      "FRUSTRATION" = PHILIRI.FRUSTRATION,
      "TRANSITIONAL" = PHILIRI.TRANSITIONAL,
      "INSTRUCTIONAL" = PHILIRI.INSTRUCTIONAL)  %>% 
    pivot_longer(
      cols = 2:last_col(), # Specifies the columns to pivot
      names_to = "Sections", # The new column to hold the original column names
      values_to = "Count" # The new column to hold the values
    )
  
  current_filtered_data <- data_to_display
  
  # --- Empty Data Handling ---
  if (nrow(current_filtered_data) == 0) {
    return(ggplotly(ggplot() +
                      annotate("text", x = 0.5, y = 0.5, label = "No data for selected regions/divisions") +
                      theme_void()))
  }
  
  # Prepare the data for plotting
  plot_data <- data_to_display
  
  # Create the ggplot
  p <- ggplot(plot_data,
              aes(x = REGION, # Reorder regions based on overall total count for the region
                  y = Count,
                  fill = Sections, # Fill by Clustering.Status for coloring and consistent order
                  text = paste("Region: ", REGION,
                               "<br>Section: ", Sections,
                               "<br>Count: ", scales::comma(Count)))) + # Custom tooltip text
    geom_bar(stat = "identity", position = "dodge", color = "black") + # Changed to position="dodge" for dodgeed bars
    labs(x = "",
         y = "") +
    scale_y_continuous(labels = scales::comma) + # Format y-axis labels as comma-separated numbers
    theme_minimal() +
    theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
          legend.position = "bottom", # Position legend at the bottom
          plot.title = element_text(hjust = 0.5)) # Center the plot title
  
  # Convert ggplot to plotly, ensuring custom text is used for hover
  ggplotly(p, tooltip = "text", source = "cloudenrolmentRegionPlot") %>%
    layout(hoverlabel = list(bgcolor = "white"),
           margin = list(b = 100)) # Increase bottom margin for x-axis labels
})

output$philiri_division_graph <- renderPlotly({
  # Use the reactive filtered data
  
  data_to_display <- filtered_cloud_region() %>%
    select(BASIC.DIVISION, PHILIRI.FRUSTRATION, PHILIRI.TRANSITIONAL, PHILIRI.INSTRUCTIONAL) %>% 
    mutate(across(2:last_col(), as.numeric)) %>% 
    mutate(BASIC.DIVISION = as.character(BASIC.DIVISION)) %>%
    filter(BASIC.DIVISION != "") %>% 
    group_by(BASIC.DIVISION) %>%
    summarise(across(everything(), sum, na.rm = TRUE)) %>% 
    rename(
      "DIVISION" = BASIC.DIVISION,
      "FRUSTRATION" = PHILIRI.FRUSTRATION,
      "TRANSITIONAL" = PHILIRI.TRANSITIONAL,
      "INSTRUCTIONAL" = PHILIRI.INSTRUCTIONAL)  %>% 
    pivot_longer(
      cols = 2:last_col(), # Specifies the columns to pivot
      names_to = "Sections", # The new column to hold the original column names
      values_to = "Count" # The new column to hold the values
    )
  
  current_filtered_data <- data_to_display
  
  # --- Empty Data Handling ---
  if (nrow(current_filtered_data) == 0) {
    return(ggplotly(ggplot() +
                      annotate("text", x = 0.5, y = 0.5, label = "No data for selected regions/divisions") +
                      theme_void()))
  }
  
  # Prepare the data for plotting
  plot_data <- data_to_display
  
  # Create the ggplot
  p <- ggplot(plot_data,
              aes(x = Sections, # Reorder regions based on overall total count for the region
                  y = Count,
                  fill = Sections, # Fill by Clustering.Status for coloring and consistent order
                  text = paste("Region: ", DIVISION,
                               "<br>Section: ", Sections,
                               "<br>Count: ", scales::comma(Count)))) + # Custom tooltip text
    geom_bar(stat = "identity", position = "dodge", color = "black") + # Changed to position="dodge" for dodgeed bars
    scale_y_continuous(labels = scales::comma) + # Format y-axis labels as comma-separated numbers
    theme_minimal() +
    theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
          legend.position = "bottom", # Position legend at the bottom
          plot.title = element_text(hjust = 0.5)) +
    facet_wrap(~DIVISION) + coord_flip()+
    labs(x = NULL, y = NULL, fill = NULL)
# Center the plot title # Center the plot title
  
  # Convert ggplot to plotly, ensuring custom text is used for hover
  ggplotly(p, tooltip = "text", source = "cloudenrolmentDivisionPlot") %>%
    layout(hoverlabel = list(bgcolor = "white"),
           margin = list(b = 100)) # Increase bottom margin for x-axis labels
})


output$sha_data_table <- DT::renderDT({
  
  data_to_display <- filtered_cloud_region_v2() %>%
    select(BASIC.REGION,BASIC.DIVISION, contains("SHA.CATEGORY")) %>% 
    mutate(across(3:last_col(), as.numeric)) %>% 
    mutate(BASIC.DIVISION = as.character(BASIC.DIVISION)) %>%
    filter(BASIC.DIVISION != "") %>% 
    group_by(BASIC.REGION,BASIC.DIVISION) %>%
    summarise(across(everything(), sum, na.rm = TRUE))
  
  # You might want to add a check for NULL or empty data if filtered_school_data_division()
  # could return such states and you want to display a message or an empty table.
  if (is.null(data_to_display) || nrow(data_to_display) == 0) {
    return(DT::datatable(
      data.frame("Message" = "No data available based on current selection."),
      options = list(dom = 't'), # 't' hides all controls, showing only the table body
      rownames = FALSE
    ))
  }
  
  DT::datatable(
    data_to_display,
    options = list(pageLength = 10, scrollX = TRUE),
    filter = 'top',
    selection = 'multiple',
    rownames = FALSE
  )
})

output$sha_regional_graph <- renderPlotly({
  # Use the reactive filtered data
  
  data_to_display <- cloud_v2 %>%
    select(BASIC.REGION, contains("SHA.CATEGORY")) %>% 
    mutate(across(2:last_col(), as.numeric)) %>% 
    mutate(BASIC.REGION = as.character(BASIC.REGION)) %>%
    filter(BASIC.REGION != "") %>% 
    group_by(BASIC.REGION) %>%
    summarise(across(everything(), sum, na.rm = TRUE))  %>% 
    pivot_longer(
      cols = 2:last_col(), # Specifies the columns to pivot
      names_to = "Sections", # The new column to hold the original column names
      values_to = "Count" # The new column to hold the values
    )
  
  current_filtered_data <- data_to_display
  
  # --- Empty Data Handling ---
  if (nrow(current_filtered_data) == 0) {
    return(ggplotly(ggplot() +
                      annotate("text", x = 0.5, y = 0.5, label = "No data for selected regions/divisions") +
                      theme_void()))
  }
  
  # Prepare the data for plotting
  plot_data <- data_to_display
  
  # Create the ggplot
  p <- ggplot(plot_data,
              aes(x = BASIC.REGION, # Reorder regions based on overall total count for the region
                  y = Count,
                  fill = Sections, # Fill by Clustering.Status for coloring and consistent order
                  text = paste("Region: ", BASIC.REGION,
                               "<br>Section: ", Sections,
                               "<br>Count: ", scales::comma(Count)))) + # Custom tooltip text
    geom_bar(stat = "identity", position = "dodge", color = "black") + # Changed to position="dodge" for dodgeed bars
    labs(x = "",
         y = "") +
    scale_y_continuous(labels = scales::comma) + # Format y-axis labels as comma-separated numbers
    theme_minimal() +
    theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
          legend.position = "bottom", # Position legend at the bottom
          plot.title = element_text(hjust = 0.5)) # Center the plot title
  
  # Convert ggplot to plotly, ensuring custom text is used for hover
  ggplotly(p, tooltip = "text", source = "cloudenrolmentRegionPlot") %>%
    layout(hoverlabel = list(bgcolor = "white"),
           margin = list(b = 100)) # Increase bottom margin for x-axis labels
})

output$sha_division_graph <- renderPlotly({
  # Use the reactive filtered data
  
  data_to_display <- filtered_cloud_region_v2() %>%
    select(BASIC.DIVISION, contains("SHA.CATEGORY")) %>% 
    mutate(across(2:last_col(), as.numeric)) %>% 
    mutate(BASIC.DIVISION = as.character(BASIC.DIVISION)) %>%
    filter(BASIC.DIVISION != "") %>% 
    group_by(BASIC.DIVISION) %>%
    summarise(across(everything(), sum, na.rm = TRUE))  %>% 
    pivot_longer(
      cols = 2:last_col(), # Specifies the columns to pivot
      names_to = "Sections", # The new column to hold the original column names
      values_to = "Count" # The new column to hold the values
    )
  
  current_filtered_data <- data_to_display
  
  # --- Empty Data Handling ---
  if (nrow(current_filtered_data) == 0) {
    return(ggplotly(ggplot() +
                      annotate("text", x = 0.5, y = 0.5, label = "No data for selected regions/divisions") +
                      theme_void()))
  }
  
  # Prepare the data for plotting
  plot_data <- data_to_display
  
  # Create the ggplot
  p <- ggplot(plot_data,
              aes(x = Sections, # Reorder regions based on overall total count for the region
                  y = Count,
                  fill = Sections, # Fill by Clustering.Status for coloring and consistent order
                  text = paste("Region: ", BASIC.DIVISION,
                               "<br>Section: ", Sections,
                               "<br>Count: ", scales::comma(Count)))) + # Custom tooltip text
    geom_bar(stat = "identity", position = "dodge", color = "black") + # Changed to position="dodge" for dodgeed bars
    labs(x = "",
         y = "") +
    scale_y_continuous(labels = scales::comma) + # Format y-axis labels as comma-separated numbers
    theme_minimal() +
    theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
          legend.position = "bottom", # Position legend at the bottom
          plot.title = element_text(hjust = 0.5)) +
    facet_wrap(~BASIC.DIVISION) + coord_flip()+
    labs(x = NULL, y = NULL, fill = NULL) # Center the plot title # Center the plot title
  
  # Convert ggplot to plotly, ensuring custom text is used for hover
  ggplotly(p, tooltip = "text", source = "cloudenrolmentDivisionPlot") %>%
    layout(hoverlabel = list(bgcolor = "white"),
           margin = list(b = 100)) # Increase bottom margin for x-axis labels
})


output$feeding_data_table <- DT::renderDT({
  
  data_to_display <- filtered_cloud_region() %>%
    select(BASIC.REGION,BASIC.DIVISION, contains("SBFP.")) %>% 
    mutate(across(3:last_col(), as.numeric)) %>% 
    mutate(BASIC.DIVISION = as.character(BASIC.DIVISION)) %>%
    filter(BASIC.DIVISION != "") %>% 
    group_by(BASIC.REGION,BASIC.DIVISION) %>%
    summarise(across(everything(), sum, na.rm = TRUE))
  
  # You might want to add a check for NULL or empty data if filtered_school_data_division()
  # could return such states and you want to display a message or an empty table.
  if (is.null(data_to_display) || nrow(data_to_display) == 0) {
    return(DT::datatable(
      data.frame("Message" = "No data available based on current selection."),
      options = list(dom = 't'), # 't' hides all controls, showing only the table body
      rownames = FALSE
    ))
  }
  
  DT::datatable(
    data_to_display,
    options = list(pageLength = 10, scrollX = TRUE),
    filter = 'top',
    selection = 'multiple',
    rownames = FALSE
  )
})

output$feeding_regional_graph <- renderPlotly({
  # Use the reactive filtered data
  
  data_to_display <- cloud %>%
    select(BASIC.REGION, contains("SBFP.")) %>% 
    mutate(across(2:last_col(), as.numeric)) %>% 
    mutate(BASIC.REGION = as.character(BASIC.REGION)) %>%
    filter(BASIC.REGION != "") %>% 
    group_by(BASIC.REGION) %>%
    summarise(across(everything(), sum, na.rm = TRUE))  %>% 
    pivot_longer(
      cols = 2:last_col(), # Specifies the columns to pivot
      names_to = "Sections", # The new column to hold the original column names
      values_to = "Count" # The new column to hold the values
    )
  
  current_filtered_data <- data_to_display
  
  # --- Empty Data Handling ---
  if (nrow(current_filtered_data) == 0) {
    return(ggplotly(ggplot() +
                      annotate("text", x = 0.5, y = 0.5, label = "No data for selected regions/divisions") +
                      theme_void()))
  }
  
  # Prepare the data for plotting
  plot_data <- data_to_display
  
  # Create the ggplot
  p <- ggplot(plot_data,
              aes(x = BASIC.REGION, # Reorder regions based on overall total count for the region
                  y = Count,
                  fill = Sections, # Fill by Clustering.Status for coloring and consistent order
                  text = paste("Region: ", BASIC.REGION,
                               "<br>Section: ", Sections,
                               "<br>Count: ", scales::comma(Count)))) + # Custom tooltip text
    geom_bar(stat = "identity", position = "dodge", color = "black") + # Changed to position="dodge" for dodgeed bars
    labs(x = "",
         y = "") +
    scale_y_continuous(labels = scales::comma) + # Format y-axis labels as comma-separated numbers
    theme_minimal() +
    theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
          legend.position = "bottom", # Position legend at the bottom
          plot.title = element_text(hjust = 0.5)) # Center the plot title
  
  # Convert ggplot to plotly, ensuring custom text is used for hover
  ggplotly(p, tooltip = "text", source = "cloudenrolmentRegionPlot") %>%
    layout(hoverlabel = list(bgcolor = "white"),
           margin = list(b = 100)) # Increase bottom margin for x-axis labels
})

output$feeding_division_graph <- renderPlotly({
  # Use the reactive filtered data
  
  data_to_display <- filtered_cloud_region() %>%
    select(BASIC.DIVISION, contains("SBFP.")) %>% 
    mutate(across(2:last_col(), as.numeric)) %>% 
    mutate(BASIC.DIVISION = as.character(BASIC.DIVISION)) %>%
    filter(BASIC.DIVISION != "") %>% 
    group_by(BASIC.DIVISION) %>%
    summarise(across(everything(), sum, na.rm = TRUE))  %>% 
    pivot_longer(
      cols = 2:last_col(), # Specifies the columns to pivot
      names_to = "Sections", # The new column to hold the original column names
      values_to = "Count" # The new column to hold the values
    )
  
  current_filtered_data <- data_to_display
  
  # --- Empty Data Handling ---
  if (nrow(current_filtered_data) == 0) {
    return(ggplotly(ggplot() +
                      annotate("text", x = 0.5, y = 0.5, label = "No data for selected regions/divisions") +
                      theme_void()))
  }
  
  # Prepare the data for plotting
  plot_data <- data_to_display
  
  # Create the ggplot
  p <- ggplot(plot_data,
              aes(x = Sections, # Reorder regions based on overall total count for the region
                  y = Count,
                  fill = Sections, # Fill by Clustering.Status for coloring and consistent order
                  text = paste("Region: ", BASIC.DIVISION,
                               "<br>Section: ", Sections,
                               "<br>Count: ", scales::comma(Count)))) + # Custom tooltip text
    geom_bar(stat = "identity", position = "dodge", color = "black") + # Changed to position="dodge" for dodgeed bars
    labs(x = "",
         y = "") +
    scale_y_continuous(labels = scales::comma) + # Format y-axis labels as comma-separated numbers
    theme_minimal() +
    theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
          legend.position = "bottom", # Position legend at the bottom
          plot.title = element_text(hjust = 0.5)) +
    facet_wrap(~BASIC.DIVISION)# Center the plot title # Center the plot title
  
  # Convert ggplot to plotly, ensuring custom text is used for hover
  ggplotly(p, tooltip = "text", source = "cloudenrolmentDivisionPlot") %>%
    layout(hoverlabel = list(bgcolor = "white"),
           margin = list(b = 100)) # Increase bottom margin for x-axis labels
})

output$years_data_table <- DT::renderDT({
  
  data_to_display <- filtered_cloud_region() %>%
    select(BASIC.REGION,BASIC.DIVISION, contains("SERVICE.")) %>% 
    mutate(across(3:last_col(), as.numeric)) %>% 
    mutate(BASIC.DIVISION = as.character(BASIC.DIVISION)) %>%
    filter(BASIC.DIVISION != "") %>% 
    group_by(BASIC.REGION,BASIC.DIVISION) %>%
    summarise(across(everything(), sum, na.rm = TRUE))
  
  # You might want to add a check for NULL or empty data if filtered_school_data_division()
  # could return such states and you want to display a message or an empty table.
  if (is.null(data_to_display) || nrow(data_to_display) == 0) {
    return(DT::datatable(
      data.frame("Message" = "No data available based on current selection."),
      options = list(dom = 't'), # 't' hides all controls, showing only the table body
      rownames = FALSE
    ))
  }
  
  DT::datatable(
    data_to_display,
    options = list(pageLength = 10, scrollX = TRUE),
    filter = 'top',
    selection = 'multiple',
    rownames = FALSE
  )
})

output$years_regional_graph <- renderPlotly({
  # Use the reactive filtered data
  
  data_to_display <- cloud %>%
    select(BASIC.REGION, contains("SERVICE.")) %>% 
    mutate(across(2:last_col(), as.numeric)) %>% 
    mutate(BASIC.REGION = as.character(BASIC.REGION)) %>%
    filter(BASIC.REGION != "") %>% 
    group_by(BASIC.REGION) %>%
    summarise(across(everything(), sum, na.rm = TRUE))  %>% 
    pivot_longer(
      cols = 2:last_col(), # Specifies the columns to pivot
      names_to = "Sections", # The new column to hold the original column names
      values_to = "Count" # The new column to hold the values
    )
  
  current_filtered_data <- data_to_display
  
  # --- Empty Data Handling ---
  if (nrow(current_filtered_data) == 0) {
    return(ggplotly(ggplot() +
                      annotate("text", x = 0.5, y = 0.5, label = "No data for selected regions/divisions") +
                      theme_void()))
  }
  
  # Prepare the data for plotting
  plot_data <- data_to_display
  
  # Create the ggplot
  p <- ggplot(plot_data,
              aes(x = BASIC.REGION, # Reorder regions based on overall total count for the region
                  y = Count,
                  fill = Sections, # Fill by Clustering.Status for coloring and consistent order
                  text = paste("Region: ", BASIC.REGION,
                               "<br>Section: ", Sections,
                               "<br>Count: ", scales::comma(Count)))) + # Custom tooltip text
    geom_bar(stat = "identity", position = "dodge", color = "black") + # Changed to position="dodge" for dodgeed bars
    labs(x = "",
         y = "") +
    scale_y_continuous(labels = scales::comma) + # Format y-axis labels as comma-separated numbers
    theme_minimal() +
    theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
          legend.position = "bottom", # Position legend at the bottom
          plot.title = element_text(hjust = 0.5)) # Center the plot title
  
  # Convert ggplot to plotly, ensuring custom text is used for hover
  ggplotly(p, tooltip = "text", source = "cloudenrolmentRegionPlot") %>%
    layout(hoverlabel = list(bgcolor = "white"),
           margin = list(b = 100)) # Increase bottom margin for x-axis labels
})

output$years_division_graph <- renderPlotly({
  # Use the reactive filtered data
  
  data_to_display <- filtered_cloud_region() %>%
    select(BASIC.DIVISION, contains("SERVICE.")) %>% 
    mutate(across(2:last_col(), as.numeric)) %>% 
    mutate(BASIC.DIVISION = as.character(BASIC.DIVISION)) %>%
    filter(BASIC.DIVISION != "") %>% 
    group_by(BASIC.DIVISION) %>%
    summarise(across(everything(), sum, na.rm = TRUE))  %>% 
    pivot_longer(
      cols = 2:last_col(), # Specifies the columns to pivot
      names_to = "Sections", # The new column to hold the original column names
      values_to = "Count" # The new column to hold the values
    )
  
  current_filtered_data <- data_to_display
  
  # --- Empty Data Handling ---
  if (nrow(current_filtered_data) == 0) {
    return(ggplotly(ggplot() +
                      annotate("text", x = 0.5, y = 0.5, label = "No data for selected regions/divisions") +
                      theme_void()))
  }
  
  # Prepare the data for plotting
  plot_data <- data_to_display
  
  # Create the ggplot
  p <- ggplot(plot_data,
              aes(x = Sections, # Reorder regions based on overall total count for the region
                  y = Count,
                  fill = Sections, # Fill by Clustering.Status for coloring and consistent order
                  text = paste("Region: ", BASIC.DIVISION,
                               "<br>Section: ", Sections,
                               "<br>Count: ", scales::comma(Count)))) + # Custom tooltip text
    geom_bar(stat = "identity", position = "dodge", color = "black") + # Changed to position="dodge" for dodgeed bars
    labs(x = "",
         y = "") +
    scale_y_continuous(labels = scales::comma) + # Format y-axis labels as comma-separated numbers
    theme_minimal() +
    theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
          legend.position = "bottom", # Position legend at the bottom
          plot.title = element_text(hjust = 0.5)) +
    facet_wrap(~BASIC.DIVISION) + coord_flip()+
    labs(x = NULL, y = NULL, fill = NULL) # Center the plot title # Center the plot title
  
  # Convert ggplot to plotly, ensuring custom text is used for hover
  ggplotly(p, tooltip = "text", source = "cloudenrolmentDivisionPlot") %>%
    layout(hoverlabel = list(bgcolor = "white"),
           margin = list(b = 100)) # Increase bottom margin for x-axis labels
})


output$lac_data_table <- DT::renderDT({
  
  data_to_display <- filtered_cloud_region() %>%
    select(BASIC.REGION,BASIC.DIVISION, contains("LAC.")) %>% 
    mutate(across(3:last_col(), as.numeric)) %>% 
    mutate(BASIC.DIVISION = as.character(BASIC.DIVISION)) %>%
    filter(BASIC.DIVISION != "") %>% 
    group_by(BASIC.REGION,BASIC.DIVISION) %>%
    summarise(across(everything(), sum, na.rm = TRUE))
  
  # You might want to add a check for NULL or empty data if filtered_school_data_division()
  # could return such states and you want to display a message or an empty table.
  if (is.null(data_to_display) || nrow(data_to_display) == 0) {
    return(DT::datatable(
      data.frame("Message" = "No data available based on current selection."),
      options = list(dom = 't'), # 't' hides all controls, showing only the table body
      rownames = FALSE
    ))
  }
  
  DT::datatable(
    data_to_display,
    options = list(pageLength = 10, scrollX = TRUE),
    filter = 'top',
    selection = 'multiple',
    rownames = FALSE
  )
})

output$feeding_regional_graph <- renderPlotly({
  # Use the reactive filtered data
  
  data_to_display <- cloud %>%
    select(BASIC.REGION, contains("SBFP.")) %>% 
    mutate(across(2:last_col(), as.numeric)) %>% 
    mutate(BASIC.REGION = as.character(BASIC.REGION)) %>%
    filter(BASIC.REGION != "") %>% 
    group_by(BASIC.REGION) %>%
    summarise(across(everything(), sum, na.rm = TRUE))  %>% 
    pivot_longer(
      cols = 2:last_col(), # Specifies the columns to pivot
      names_to = "Sections", # The new column to hold the original column names
      values_to = "Count" # The new column to hold the values
    )
  
  current_filtered_data <- data_to_display
  
  # --- Empty Data Handling ---
  if (nrow(current_filtered_data) == 0) {
    return(ggplotly(ggplot() +
                      annotate("text", x = 0.5, y = 0.5, label = "No data for selected regions/divisions") +
                      theme_void()))
  }
  
  # Prepare the data for plotting
  plot_data <- data_to_display
  
  # Create the ggplot
  p <- ggplot(plot_data,
              aes(x = BASIC.REGION, # Reorder regions based on overall total count for the region
                  y = Count,
                  fill = Sections, # Fill by Clustering.Status for coloring and consistent order
                  text = paste("Region: ", BASIC.REGION,
                               "<br>Section: ", Sections,
                               "<br>Count: ", scales::comma(Count)))) + # Custom tooltip text
    geom_bar(stat = "identity", position = "dodge", color = "black") + # Changed to position="dodge" for dodgeed bars
    labs(x = "",
         y = "") +
    scale_y_continuous(labels = scales::comma) + # Format y-axis labels as comma-separated numbers
    theme_minimal() +
    theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
          legend.position = "bottom", # Position legend at the bottom
          plot.title = element_text(hjust = 0.5)) # Center the plot title
  
  # Convert ggplot to plotly, ensuring custom text is used for hover
  ggplotly(p, tooltip = "text", source = "cloudenrolmentRegionPlot") %>%
    layout(hoverlabel = list(bgcolor = "white"),
           margin = list(b = 100)) # Increase bottom margin for x-axis labels
})

output$feeding_division_graph <- renderPlotly({
  # Use the reactive filtered data
  
  data_to_display <- filtered_cloud_region() %>%
    select(BASIC.DIVISION, contains("SBFP.")) %>% 
    mutate(across(2:last_col(), as.numeric)) %>% 
    mutate(BASIC.DIVISION = as.character(BASIC.DIVISION)) %>%
    filter(BASIC.DIVISION != "") %>% 
    group_by(BASIC.DIVISION) %>%
    summarise(across(everything(), sum, na.rm = TRUE))  %>% 
    pivot_longer(
      cols = 2:last_col(), # Specifies the columns to pivot
      names_to = "Sections", # The new column to hold the original column names
      values_to = "Count" # The new column to hold the values
    )
  
  current_filtered_data <- data_to_display
  
  # --- Empty Data Handling ---
  if (nrow(current_filtered_data) == 0) {
    return(ggplotly(ggplot() +
                      annotate("text", x = 0.5, y = 0.5, label = "No data for selected regions/divisions") +
                      theme_void()))
  }
  
  # Prepare the data for plotting
  plot_data <- data_to_display
  
  # Create the ggplot
  p <- ggplot(plot_data,
              aes(x = Sections, # Reorder regions based on overall total count for the region
                  y = Count,
                  fill = Sections, # Fill by Clustering.Status for coloring and consistent order
                  text = paste("Region: ", BASIC.DIVISION,
                               "<br>Section: ", Sections,
                               "<br>Count: ", scales::comma(Count)))) + # Custom tooltip text
    geom_bar(stat = "identity", position = "dodge", color = "black") + # Changed to position="dodge" for dodgeed bars
    labs(x = "",
         y = "") +
    scale_y_continuous(labels = scales::comma) + # Format y-axis labels as comma-separated numbers
    theme_minimal() +
    theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
          legend.position = "bottom", # Position legend at the bottom
          plot.title = element_text(hjust = 0.5)) +
    facet_wrap(~BASIC.DIVISION) + coord_flip()+
    labs(x = NULL, y = NULL, fill = NULL)# Center the plot title # Center the plot title
  
  # Convert ggplot to plotly, ensuring custom text is used for hover
  ggplotly(p, tooltip = "text", source = "cloudenrolmentDivisionPlot") %>%
    layout(hoverlabel = list(bgcolor = "white"),
           margin = list(b = 100)) # Increase bottom margin for x-axis labels
})

output$feeding_regional_graph <- renderPlotly({
  # Use the reactive filtered data
  
  data_to_display <- cloud %>%
    select(BASIC.REGION, contains("SBFP.")) %>% 
    mutate(across(2:last_col(), as.numeric)) %>% 
    mutate(BASIC.REGION = as.character(BASIC.REGION)) %>%
    filter(BASIC.REGION != "") %>% 
    group_by(BASIC.REGION) %>%
    summarise(across(everything(), sum, na.rm = TRUE))  %>% 
    pivot_longer(
      cols = 2:last_col(), # Specifies the columns to pivot
      names_to = "Sections", # The new column to hold the original column names
      values_to = "Count" # The new column to hold the values
    )
  
  current_filtered_data <- data_to_display
  
  # --- Empty Data Handling ---
  if (nrow(current_filtered_data) == 0) {
    return(ggplotly(ggplot() +
                      annotate("text", x = 0.5, y = 0.5, label = "No data for selected regions/divisions") +
                      theme_void()))
  }
  
  # Prepare the data for plotting
  plot_data <- data_to_display
  
  # Create the ggplot
  p <- ggplot(plot_data,
              aes(x = BASIC.REGION, # Reorder regions based on overall total count for the region
                  y = Count,
                  fill = Sections, # Fill by Clustering.Status for coloring and consistent order
                  text = paste("Region: ", BASIC.REGION,
                               "<br>Section: ", Sections,
                               "<br>Count: ", scales::comma(Count)))) + # Custom tooltip text
    geom_bar(stat = "identity", position = "dodge", color = "black") + # Changed to position="dodge" for dodgeed bars
    labs(x = "",
         y = "") +
    scale_y_continuous(labels = scales::comma) + # Format y-axis labels as comma-separated numbers
    theme_minimal() +
    theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
          legend.position = "bottom", # Position legend at the bottom
          plot.title = element_text(hjust = 0.5)) # Center the plot title
  
  # Convert ggplot to plotly, ensuring custom text is used for hover
  ggplotly(p, tooltip = "text", source = "cloudenrolmentRegionPlot") %>%
    layout(hoverlabel = list(bgcolor = "white"),
           margin = list(b = 100)) # Increase bottom margin for x-axis labels
})

output$feeding_division_graph <- renderPlotly({
  # Use the reactive filtered data
  
  data_to_display <- filtered_cloud_region() %>%
    select(BASIC.DIVISION, contains("SBFP.")) %>% 
    mutate(across(2:last_col(), as.numeric)) %>% 
    mutate(BASIC.DIVISION = as.character(BASIC.DIVISION)) %>%
    filter(BASIC.DIVISION != "") %>% 
    group_by(BASIC.DIVISION) %>%
    summarise(across(everything(), sum, na.rm = TRUE))  %>% 
    pivot_longer(
      cols = 2:last_col(), # Specifies the columns to pivot
      names_to = "Sections", # The new column to hold the original column names
      values_to = "Count" # The new column to hold the values
    )
  
  current_filtered_data <- data_to_display
  
  # --- Empty Data Handling ---
  if (nrow(current_filtered_data) == 0) {
    return(ggplotly(ggplot() +
                      annotate("text", x = 0.5, y = 0.5, label = "No data for selected regions/divisions") +
                      theme_void()))
  }
  
  # Prepare the data for plotting
  plot_data <- data_to_display
  
  # Create the ggplot
  p <- ggplot(plot_data,
              aes(x = Sections, # Reorder regions based on overall total count for the region
                  y = Count,
                  fill = Sections, # Fill by Clustering.Status for coloring and consistent order
                  text = paste("Region: ", BASIC.DIVISION,
                               "<br>Section: ", Sections,
                               "<br>Count: ", scales::comma(Count)))) + # Custom tooltip text
    geom_bar(stat = "identity", position = "dodge", color = "black") + # Changed to position="dodge" for dodgeed bars
    labs(x = "",
         y = "") +
    scale_y_continuous(labels = scales::comma) + # Format y-axis labels as comma-separated numbers
    theme_minimal() +
    theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
          legend.position = "bottom", # Position legend at the bottom
          plot.title = element_text(hjust = 0.5)) +
    facet_wrap(~BASIC.DIVISION)# Center the plot title # Center the plot title
  
  # Convert ggplot to plotly, ensuring custom text is used for hover
  ggplotly(p, tooltip = "text", source = "cloudenrolmentDivisionPlot") %>%
    layout(hoverlabel = list(bgcolor = "white"),
           margin = list(b = 100)) # Increase bottom margin for x-axis labels
})

output$resources_data_table <- DT::renderDT({
  
  data_to_display <- filtered_cloud_region_v2() %>%
    select(BASIC.REGION, BASIC.DIVISION, contains("RESOURCES.")) %>%
    mutate(across(3:last_col(), as.numeric)) %>%
    mutate(BASIC.DIVISION = as.character(BASIC.DIVISION)) %>%
    filter(BASIC.DIVISION != "") %>%
    group_by(BASIC.REGION, BASIC.DIVISION) %>%
    summarise(across(everything(), sum, na.rm = TRUE)) %>%
    mutate(RESOURCES.MATH = round((`RESOURCES.RECEIVED.SCIENCE.MATH.EQUIPMENT_Yes` / (`RESOURCES.RECEIVED.SCIENCE.MATH.EQUIPMENT_Yes` + `RESOURCES.RECEIVED.SCIENCE.MATH.EQUIPMENT_No`)) * 100, 2)) %>%
    mutate(RESOURCES.HANDWASHING = round((`RESOURCES.REGULAR.HANDWASHING_Yes` / (`RESOURCES.REGULAR.HANDWASHING_Yes` + `RESOURCES.REGULAR.HANDWASHING_No`)) * 100, 2)) %>%
    mutate(RESOURCES.SOAP = round((`RESOURCES.REGULAR.SOAP_Yes` / (`RESOURCES.REGULAR.SOAP_Yes` + `RESOURCES.REGULAR.SOAP_No`)) * 100, 2)) %>%
    mutate(RESOURCES.TVL = round((`RESOURCES.SCHOOL.WITH.TVL_Yes` / (`RESOURCES.SCHOOL.WITH.TVL_Yes` + `RESOURCES.SCHOOL.WITH.TVL_None`)) * 100, 2)) %>% 
    select(1:2,"RESOURCES.MATH","RESOURCES.HANDWASHING","RESOURCES.SOAP","RESOURCES.TVL")
  
  # You might want to add a check for NULL or empty data if filtered_school_data_division()
  # could return such states and you want to display a message or an empty table.
  if (is.null(data_to_display) || nrow(data_to_display) == 0) {
    return(DT::datatable(
      data.frame("Message" = "No data available based on current selection."),
      options = list(dom = 't'), # 't' hides all controls, showing only the table body
      rownames = FALSE
    ))
  }
  
  DT::datatable(
    data_to_display,
    options = list(pageLength = 10, scrollX = TRUE),
    filter = 'top',
    selection = 'multiple',
    rownames = FALSE
  )
})

output$resources_regional_graph <- renderPlotly({
  # Use the reactive filtered data
  
  data_to_display <- cloud_v2 %>%
    select(BASIC.REGION, contains("RESOURCES.")) %>% 
    mutate(across(2:last_col(), as.numeric)) %>% 
    mutate(BASIC.REGION = as.character(BASIC.REGION)) %>%
    filter(BASIC.REGION != "") %>% 
    group_by(BASIC.REGION) %>%
    summarise(across(everything(), sum, na.rm = TRUE)) %>%
    mutate(RESOURCES.MATH = round((`RESOURCES.RECEIVED.SCIENCE.MATH.EQUIPMENT_Yes` / (`RESOURCES.RECEIVED.SCIENCE.MATH.EQUIPMENT_Yes` + `RESOURCES.RECEIVED.SCIENCE.MATH.EQUIPMENT_No`)) * 100, 2)) %>%
    mutate(RESOURCES.HANDWASHING = round((`RESOURCES.REGULAR.HANDWASHING_Yes` / (`RESOURCES.REGULAR.HANDWASHING_Yes` + `RESOURCES.REGULAR.HANDWASHING_No`)) * 100, 2)) %>%
    mutate(RESOURCES.SOAP = round((`RESOURCES.REGULAR.SOAP_Yes` / (`RESOURCES.REGULAR.SOAP_Yes` + `RESOURCES.REGULAR.SOAP_No`)) * 100, 2)) %>%
    mutate(RESOURCES.TVL = round((`RESOURCES.SCHOOL.WITH.TVL_Yes` / (`RESOURCES.SCHOOL.WITH.TVL_Yes` + `RESOURCES.SCHOOL.WITH.TVL_None`)) * 100, 2)) %>%
    # mutate(RESOURCES.CHAIR = round((`RESOURCES.CHAIR.FUNCTIONAL` / `RESOURCES.CHAIR.INVENTORY`) * 100, 2)) %>%
    # mutate(RESOURCES.DESK = round((`RESOURCES.DESK.FUNCTIONAL` / `RESOURCES.DESK.INVENTORY`) * 100, 2)) %>%
    # mutate(RESOURCES.ECART = round((`RESOURCES.ECART.FUNCTIONAL` / `RESOURCES.ECART.INVENTORY`) * 100, 2)) %>%
    # mutate(RESOURCES.LAPTOP = round((`RESOURCES.LAPTOP.FUNCTIONAL` / `RESOURCES.LAPTOP.INVENTORY`) * 100, 2)) %>%
    # mutate(RESOURCES.TOILET = round((`RESOURCES.FUNCTIONAL.TOILET` / `RESOURCES.NON.FUNCTIONAL.TOILET`) * 100, 2)) %>%
    # mutate(RESOURCES.TV = round((`RESOURCES.TV.FUNCTIONAL` / `RESOURCES.TV.INVENTORY`) * 100, 2))  
    select(1,"RESOURCES.MATH","RESOURCES.HANDWASHING","RESOURCES.SOAP","RESOURCES.TVL") %>% 
    group_by(BASIC.REGION) %>%
    summarise(across(everything(), mean, na.rm = TRUE)) %>% 
    pivot_longer(
      cols = 2:last_col(), # Specifies the columns to pivot
      names_to = "Sections", # The new column to hold the original column names
      values_to = "Count" # The new column to hold the values
    ) 
  
  current_filtered_data <- data_to_display
  
  # --- Empty Data Handling ---
  if (nrow(current_filtered_data) == 0) {
    return(ggplotly(ggplot() +
                      annotate("text", x = 0.5, y = 0.5, label = "No data for selected regions/divisions") +
                      theme_void()))
  }
  
  # Prepare the data for plotting
  plot_data <- data_to_display
  
  # Create the ggplot
  p <- ggplot(plot_data,
              aes(x = BASIC.REGION, # Reorder regions based on overall total count for the region
                  y = Count,
                  fill = Sections, # Fill by Clustering.Status for coloring and consistent order
                  text = paste("Region: ", BASIC.REGION,
                               "<br>Section: ", Sections,
                               "<br>Count: ", scales::comma(Count)))) + # Custom tooltip text
    geom_bar(stat = "identity", position = "dodge", color = "black") + # Changed to position="dodge" for dodgeed bars
    labs(x = "",
         y = "") +
    scale_y_continuous(labels = scales::comma) + # Format y-axis labels as comma-separated numbers
    theme_minimal() +
    theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
          legend.position = "bottom", # Position legend at the bottom
          plot.title = element_text(hjust = 0.5)) # Center the plot title
  
  # Convert ggplot to plotly, ensuring custom text is used for hover
  ggplotly(p, tooltip = "text", source = "cloudenrolmentRegionPlot") %>%
    layout(hoverlabel = list(bgcolor = "white"),
           margin = list(b = 100)) # Increase bottom margin for x-axis labels
})

output$resources_division_graph <- renderPlotly({
  # Use the reactive filtered data
  
  data_to_display <- filtered_cloud_region_v2() %>%
    select(BASIC.DIVISION, contains("RESOURCES.")) %>% 
    mutate(across(2:last_col(), as.numeric)) %>% 
    mutate(BASIC.DIVISION = as.character(BASIC.DIVISION)) %>%
    filter(BASIC.DIVISION != "") %>% 
    group_by(BASIC.DIVISION) %>%
    summarise(across(everything(), sum, na.rm = TRUE)) %>%
    mutate(RESOURCES.MATH = round((`RESOURCES.RECEIVED.SCIENCE.MATH.EQUIPMENT_Yes` / (`RESOURCES.RECEIVED.SCIENCE.MATH.EQUIPMENT_Yes` + `RESOURCES.RECEIVED.SCIENCE.MATH.EQUIPMENT_No`)) * 100, 2)) %>%
    mutate(RESOURCES.HANDWASHING = round((`RESOURCES.REGULAR.HANDWASHING_Yes` / (`RESOURCES.REGULAR.HANDWASHING_Yes` + `RESOURCES.REGULAR.HANDWASHING_No`)) * 100, 2)) %>%
    mutate(RESOURCES.SOAP = round((`RESOURCES.REGULAR.SOAP_Yes` / (`RESOURCES.REGULAR.SOAP_Yes` + `RESOURCES.REGULAR.SOAP_No`)) * 100, 2)) %>%
    mutate(RESOURCES.TVL = round((`RESOURCES.SCHOOL.WITH.TVL_Yes` / (`RESOURCES.SCHOOL.WITH.TVL_Yes` + `RESOURCES.SCHOOL.WITH.TVL_None`)) * 100, 2)) %>%
    # mutate(RESOURCES.CHAIR = round((`RESOURCES.CHAIR.FUNCTIONAL` / `RESOURCES.CHAIR.INVENTORY`) * 100, 2)) %>%
    # mutate(RESOURCES.DESK = round((`RESOURCES.DESK.FUNCTIONAL` / `RESOURCES.DESK.INVENTORY`) * 100, 2)) %>%
    # mutate(RESOURCES.ECART = round((`RESOURCES.ECART.FUNCTIONAL` / `RESOURCES.ECART.INVENTORY`) * 100, 2)) %>%
    # mutate(RESOURCES.LAPTOP = round((`RESOURCES.LAPTOP.FUNCTIONAL` / `RESOURCES.LAPTOP.INVENTORY`) * 100, 2)) %>%
    # mutate(RESOURCES.TOILET = round((`RESOURCES.FUNCTIONAL.TOILET` / `RESOURCES.NON.FUNCTIONAL.TOILET`) * 100, 2)) %>%
    # mutate(RESOURCES.TV = round((`RESOURCES.TV.FUNCTIONAL` / `RESOURCES.TV.INVENTORY`) * 100, 2))  
  select(1,"RESOURCES.MATH","RESOURCES.HANDWASHING","RESOURCES.SOAP","RESOURCES.TVL") %>% 
    pivot_longer(
      cols = 2:last_col(), # Specifies the columns to pivot
      names_to = "Sections", # The new column to hold the original column names
      values_to = "Count" # The new column to hold the values
    )
  
  current_filtered_data <- data_to_display
  
  # --- Empty Data Handling ---
  if (nrow(current_filtered_data) == 0) {
    return(ggplotly(ggplot() +
                      annotate("text", x = 0.5, y = 0.5, label = "No data for selected regions/divisions") +
                      theme_void()))
  }
  
  # Prepare the data for plotting
  plot_data <- data_to_display
  
  # Create the ggplot
  p <- ggplot(plot_data,
              aes(x = Sections, # Reorder regions based on overall total count for the region
                  y = Count,
                  fill = Sections, # Fill by Clustering.Status for coloring and consistent order
                  text = paste("Region: ", BASIC.DIVISION,
                               "<br>Section: ", Sections,
                               "<br>Count: ", scales::comma(Count)))) + # Custom tooltip text
    geom_bar(stat = "identity", position = "dodge", color = "black") + # Changed to position="dodge" for dodgeed bars
    labs(x = "",
         y = "") +
    scale_y_continuous(labels = scales::comma) + # Format y-axis labels as comma-separated numbers
    theme_minimal() +
    theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
          legend.position = "bottom", # Position legend at the bottom
          plot.title = element_text(hjust = 0.5)) +
    facet_wrap(~BASIC.DIVISION) + coord_flip()+
    labs(x = NULL, y = NULL, fill = NULL)# Center the plot title # Center the plot title
  
  # Convert ggplot to plotly, ensuring custom text is used for hover
  ggplotly(p, tooltip = "text", source = "cloudenrolmentDivisionPlot") %>%
    layout(hoverlabel = list(bgcolor = "white"),
           margin = list(b = 100)) # Increase bottom margin for x-axis labels
})

output$overload_data_table <- DT::renderDT({
  
  data_to_display <- filtered_cloud_region() %>%
    select(BASIC.REGION,BASIC.DIVISION, contains("DO5.OVERLOAD")) %>% 
    mutate(across(3:last_col(), as.numeric)) %>% 
    mutate(BASIC.DIVISION = as.character(BASIC.DIVISION)) %>%
    filter(BASIC.DIVISION != "") %>% 
    group_by(BASIC.REGION,BASIC.DIVISION) %>%
    summarise(across(everything(), sum, na.rm = TRUE)) %>% 
    rename(
      "REGION" = BASIC.REGION,
      "DIVISION" = BASIC.DIVISION,
      "OVERLOAD PAY RECIPIENTS" = DO5.OVERLOAD.PAY.RECIPIENT)
  
  # You might want to add a check for NULL or empty data if filtered_school_data_division()
  # could return such states and you want to display a message or an empty table.
  if (is.null(data_to_display) || nrow(data_to_display) == 0) {
    return(DT::datatable(
      data.frame("Message" = "No data available based on current selection."),
      options = list(dom = 't'), # 't' hides all controls, showing only the table body
      rownames = FALSE
    ))
  }
  
  DT::datatable(
    data_to_display,
    options = list(pageLength = 10, scrollX = TRUE),
    filter = 'top',
    selection = 'multiple',
    rownames = FALSE
  )
})

output$overload_regional_graph <- renderPlotly({
  # Use the reactive filtered data
  
  data_to_display <- cloud %>%
    select(BASIC.REGION, contains("DO5.OVERLOAD")) %>% 
    mutate(across(2:last_col(), as.numeric)) %>% 
    mutate(BASIC.REGION = as.character(BASIC.REGION)) %>%
    filter(BASIC.REGION != "") %>% 
    group_by(BASIC.REGION) %>%
    summarise(across(everything(), sum, na.rm = TRUE)) %>% 
    rename(
      "REGION" = BASIC.REGION,
      "OVERLOAD PAY RECIPIENTS" = DO5.OVERLOAD.PAY.RECIPIENT)  %>% 
    pivot_longer(
      cols = 2:last_col(), # Specifies the columns to pivot
      names_to = "Sections", # The new column to hold the original column names
      values_to = "Count" # The new column to hold the values
    )
  
  current_filtered_data <- data_to_display
  
  # --- Empty Data Handling ---
  if (nrow(current_filtered_data) == 0) {
    return(ggplotly(ggplot() +
                      annotate("text", x = 0.5, y = 0.5, label = "No data for selected regions/divisions") +
                      theme_void()))
  }
  
  # Prepare the data for plotting
  plot_data <- data_to_display
  
  # Create the ggplot
  p <- ggplot(plot_data,
              aes(x = REGION, # Reorder regions based on overall total count for the region
                  y = Count,
                  fill = Sections, # Fill by Clustering.Status for coloring and consistent order
                  text = paste("Region: ", REGION,
                               "<br>Section: ", Sections,
                               "<br>Count: ", scales::comma(Count)))) + # Custom tooltip text
    geom_bar(stat = "identity", position = "dodge", color = "black") + # Changed to position="dodge" for dodgeed bars
    labs(x = "",
         y = "") +
    scale_y_continuous(labels = scales::comma) + # Format y-axis labels as comma-separated numbers
    theme_minimal() +
    theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
          legend.position = "bottom", # Position legend at the bottom
          plot.title = element_text(hjust = 0.5)) # Center the plot title
  
  # Convert ggplot to plotly, ensuring custom text is used for hover
  ggplotly(p, tooltip = "text", source = "cloudenrolmentRegionPlot") %>%
    layout(hoverlabel = list(bgcolor = "white"),
           margin = list(b = 100)) # Increase bottom margin for x-axis labels
})

output$overload_division_graph <- renderPlotly({
  # Use the reactive filtered data
  
  data_to_display <- filtered_cloud_region() %>%
    select(BASIC.DIVISION, contains("DO5.OVERLOAD")) %>% 
    mutate(across(2:last_col(), as.numeric)) %>% 
    mutate(BASIC.DIVISION = as.character(BASIC.DIVISION)) %>%
    filter(BASIC.DIVISION != "") %>% 
    group_by(BASIC.DIVISION) %>%
    summarise(across(everything(), sum, na.rm = TRUE)) %>% 
    rename(
      "DIVISION" = BASIC.DIVISION,
      "OVERLOAD PAY RECIPIENTS" = DO5.OVERLOAD.PAY.RECIPIENT) %>% 
    pivot_longer(
      cols = 2:last_col(), # Specifies the columns to pivot
      names_to = "Sections", # The new column to hold the original column names
      values_to = "Count" # The new column to hold the values
    )
  
  current_filtered_data <- data_to_display
  
  # --- Empty Data Handling ---
  if (nrow(current_filtered_data) == 0) {
    return(ggplotly(ggplot() +
                      annotate("text", x = 0.5, y = 0.5, label = "No data for selected regions/divisions") +
                      theme_void()))
  }
  
  # Prepare the data for plotting
  plot_data <- data_to_display
  
  # Create the ggplot
  p <- ggplot(plot_data,
              aes(x = Sections, # Reorder regions based on overall total count for the region
                  y = Count,
                  fill = Sections, # Fill by Clustering.Status for coloring and consistent order
                  text = paste("Region: ", DIVISION,
                               "<br>Section: ", Sections,
                               "<br>Count: ", scales::comma(Count)))) + # Custom tooltip text
    geom_bar(stat = "identity", position = "dodge", color = "black") + # Changed to position="dodge" for stacked bars
    scale_y_continuous(labels = scales::comma) + # Format y-axis labels as comma-separated numbers
    theme_minimal() +
    theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
          legend.position = "bottom", # Position legend at the bottom
          plot.title = element_text(hjust = 0.5)) +
    facet_wrap(~DIVISION) + coord_flip()+
    labs(x = NULL, y = NULL, fill = NULL)
# Center the plot title # Center the plot title
  
  # Convert ggplot to plotly, ensuring custom text is used for hover
  ggplotly(p, tooltip = "text", source = "cloudenrolmentDivisionPlot") %>%
    layout(hoverlabel = list(bgcolor = "white"),
           margin = list(b = 100)) # Increase bottom margin for x-axis labels
})

### CLOUD Regional Profile ###

output$enrolment_regional_profile_graph <- renderPlotly({
  # Use the reactive filtered data
  
  data_to_display <- cloud %>% filter(BASIC.REGION == input$cloud_region_profile_filter) %>%
    select(BASIC.REGION,ENROLLMENT.KINDER,ENROLLMENT.G1,ENROLLMENT.G2,ENROLLMENT.G3,ENROLLMENT.G4,ENROLLMENT.G5,ENROLLMENT.G6,ENROLLMENT.G7,ENROLLMENT.G8,ENROLLMENT.G9,ENROLLMENT.G10,ENROLMENT.G11,ENROLMENT.G12) %>% 
    mutate(across(2:last_col(), as.numeric)) %>% 
    mutate(BASIC.REGION = as.character(BASIC.REGION)) %>%
    filter(BASIC.REGION != "") %>% 
    group_by(BASIC.REGION) %>%
    summarise(across(everything(), sum, na.rm = TRUE)) %>%
    rename("REGION" = BASIC.REGION,
           "KINDER" = ENROLLMENT.KINDER,
           "GRADE 1" = ENROLLMENT.G1,
           "GRADE 2" = ENROLLMENT.G2,
           "GRADE 3" = ENROLLMENT.G3,
           "GRADE 4" = ENROLLMENT.G4,
           "GRADE 5" = ENROLLMENT.G5,
           "GRADE 6" = ENROLLMENT.G6,
           "GRADE 7" = ENROLLMENT.G7,
           "GRADE 8" = ENROLLMENT.G8,
           "GRADE 9" = ENROLLMENT.G9,
           "GRADE 10" = ENROLLMENT.G10,
           "GRADE 11" = ENROLMENT.G11,
           "GRADE 12" = ENROLMENT.G12) %>% 
    pivot_longer(
      cols = 2:last_col(), # Specifies the columns to pivot
      names_to = "Enrolment", # The new column to hold the original column names
      values_to = "Count" # The new column to hold the values
    ) %>%
    mutate(Enrolment = factor(Enrolment, levels = c(
      "KINDER", "GRADE 1", "GRADE 2", "GRADE 3",
      "GRADE 4", "GRADE 5", "GRADE 6",
      "GRADE 7", "GRADE 8", "GRADE 9",
      "GRADE 10", "GRADE 11", "GRADE 12"
    ))) 
  
  current_filtered_data <- data_to_display
  
  # --- Empty Data Handling ---
  if (nrow(current_filtered_data) == 0) {
    return(ggplotly(ggplot() +
                      annotate("text", x = 0.5, y = 0.5, label = "No data for selected regions/divisions") +
                      theme_void()))
  }
  
  # Prepare the data for plotting
  plot_data <- data_to_display
  
  # Create the ggplot
  p <- ggplot(plot_data,
              aes(x = REGION, # Reorder regions based on overall total count for the region
                  y = Count,
                  fill = Enrolment, # Fill by Clustering.Status for coloring and consistent order
                  text = paste("Region: ", REGION,
                               "<br>Section: ", Enrolment,
                               "<br>Count: ", scales::comma(Count)))) + # Custom tooltip text
    geom_bar(stat = "identity", position = "dodge", color = "black") + # Changed to position="stack" for stacked bars
    labs(x = "",
         y = "") +
    scale_y_continuous(labels = scales::comma) + # Format y-axis labels as comma-separated numbers
    theme_minimal() +
    theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
          legend.position = "bottom", # Position legend at the bottom
          plot.title = element_text(hjust = 0.5)) # Center the plot title
  
  # Convert ggplot to plotly, ensuring custom text is used for hover
  ggplotly(p, tooltip = "text", source = "cloudenrolmentRegionPlot") %>%
    layout(hoverlabel = list(bgcolor = "white"),
           margin = list(b = 100)) # Increase bottom margin for x-axis labels
})

output$sned_regional_profile_graph <- renderPlotly({
  # Use the reactive filtered data
  
  data_to_display <- cloud %>% filter(BASIC.REGION == input$cloud_region_profile_filter) %>%
    select(BASIC.REGION, ENROLMENT.KINDER.SNED, SPECIAL.NEEDS.G1, SPECIAL.NEEDS.G2, SPECIAL.NEEDS.G3, SPECIAL.NEEDS.G4, SPECIAL.NEEDS.G5, SPECIAL.NEEDS.G6, SPECIAL.NEEDS.G7, SPECIAL.NEEDS.G8, SPECIAL.NEEDS.G9, SPECIAL.NEEDS.G10) %>% 
    mutate(across(2:last_col(), as.numeric)) %>% 
    mutate(BASIC.REGION = as.character(BASIC.REGION)) %>%
    filter(BASIC.REGION != "") %>% 
    group_by(BASIC.REGION) %>%
    summarise(across(everything(), sum, na.rm = TRUE)) %>% 
    rename(
      "REGION" = BASIC.REGION,
      "KINDER" = ENROLMENT.KINDER.SNED,
      "GRADE 1" = SPECIAL.NEEDS.G1,
      "GRADE 2" = SPECIAL.NEEDS.G2,
      "GRADE 3" = SPECIAL.NEEDS.G3,
      "GRADE 4" = SPECIAL.NEEDS.G4,
      "GRADE 5" = SPECIAL.NEEDS.G5,
      "GRADE 6" = SPECIAL.NEEDS.G6,
      "GRADE 7" = SPECIAL.NEEDS.G7,
      "GRADE 8" = SPECIAL.NEEDS.G8,
      "GRADE 9" = SPECIAL.NEEDS.G9,
      "GRADE 10" = SPECIAL.NEEDS.G10
    )  %>% 
    pivot_longer(
      cols = 2:last_col(), # Specifies the columns to pivot
      names_to = "SNED_Learners", # The new column to hold the original column names
      values_to = "Count" # The new column to hold the values
    ) %>%
    mutate(SNED_Learners = factor(SNED_Learners, levels = c(
      "KINDER", "GRADE 1", "GRADE 2", "GRADE 3",
      "GRADE 4", "GRADE 5", "GRADE 6",
      "GRADE 7", "GRADE 8", "GRADE 9",
      "GRADE 10")))
  
  current_filtered_data <- data_to_display
  
  # --- Empty Data Handling ---
  if (nrow(current_filtered_data) == 0) {
    return(ggplotly(ggplot() +
                      annotate("text", x = 0.5, y = 0.5, label = "No data for selected regions/divisions") +
                      theme_void()))
  }
  
  # Prepare the data for plotting
  plot_data <- data_to_display
  
  # Create the ggplot
  p <- ggplot(plot_data,
              aes(x = REGION, # Reorder regions based on overall total count for the region
                  y = Count,
                  fill = SNED_Learners, # Fill by Clustering.Status for coloring and consistent order
                  text = paste("Region: ", REGION,
                               "<br>Section: ", SNED_Learners,
                               "<br>Count: ", scales::comma(Count)))) + # Custom tooltip text
    geom_bar(stat = "identity", position = "dodge", color = "black") + # Changed to position="stack" for stacked bars
    labs(x = "",
         y = "") +
    scale_y_continuous(labels = scales::comma) + # Format y-axis labels as comma-separated numbers
    theme_minimal() +
    theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
          legend.position = "bottom", # Position legend at the bottom
          plot.title = element_text(hjust = 0.5)) # Center the plot title
  
  # Convert ggplot to plotly, ensuring custom text is used for hover
  ggplotly(p, tooltip = "text", source = "cloudenrolmentRegionPlot") %>%
    layout(hoverlabel = list(bgcolor = "white"),
           margin = list(b = 100)) # Increase bottom margin for x-axis labels
})

output$ip_regional_profile_graph <- renderPlotly({
  # Use the reactive filtered data
  
  data_to_display <- cloud %>% filter(BASIC.REGION == input$cloud_region_profile_filter) %>%
    select(BASIC.REGION, IP.KINDER.IP, IP.G1.IP, IP.G2.IP, IP.G3.IP, IP.G4.IP, IP.G5.IP, IP.G6.IP, IP.G7.IP, IP.G8.IP, IP.G9.IP, IP.G10.IP, IP.G11, IP.G12) %>% 
    mutate(across(2:last_col(), as.numeric)) %>% 
    mutate(BASIC.REGION = as.character(BASIC.REGION)) %>%
    filter(BASIC.REGION != "") %>% 
    group_by(BASIC.REGION) %>%
    summarise(across(everything(), sum, na.rm = TRUE))  %>% 
    rename(
      "REGION" = BASIC.REGION,
      "KINDER" = IP.KINDER.IP,
      "GRADE 1" = IP.G1.IP,
      "GRADE 2" = IP.G2.IP,
      "GRADE 3" = IP.G3.IP,
      "GRADE 4" = IP.G4.IP,
      "GRADE 5" = IP.G5.IP,
      "GRADE 6" = IP.G6.IP,
      "GRADE 7" = IP.G7.IP,
      "GRADE 8" = IP.G8.IP,
      "GRADE 9" = IP.G9.IP,
      "GRADE 10" = IP.G10.IP,
      "GRADE 11" = IP.G11,
      "GRADE 12" = IP.G12
    ) %>% 
    pivot_longer(
      cols = 2:last_col(), # Specifies the columns to pivot
      names_to = "IP_Learners", # The new column to hold the original column names
      values_to = "Count" # The new column to hold the values
    )  %>%
    mutate(IP_Learners = factor(IP_Learners, levels = c(
      "KINDER", "GRADE 1", "GRADE 2", "GRADE 3",
      "GRADE 4", "GRADE 5", "GRADE 6",
      "GRADE 7", "GRADE 8", "GRADE 9",
      "GRADE 10", "GRADE 11", "GRADE 12"
    ))) 
  
  current_filtered_data <- data_to_display
  
  # --- Empty Data Handling ---
  if (nrow(current_filtered_data) == 0) {
    return(ggplotly(ggplot() +
                      annotate("text", x = 0.5, y = 0.5, label = "No data for selected regions/divisions") +
                      theme_void()))
  }
  
  # Prepare the data for plotting
  plot_data <- data_to_display
  
  # Create the ggplot
  p <- ggplot(plot_data,
              aes(x = REGION, # Reorder regions based on overall total count for the region
                  y = Count,
                  fill = IP_Learners, # Fill by Clustering.Status for coloring and consistent order
                  text = paste("Region: ", REGION,
                               "<br>Section: ", IP_Learners,
                               "<br>Count: ", scales::comma(Count)))) + # Custom tooltip text
    geom_bar(stat = "identity", position = "dodge", color = "black") + # Changed to position="stack" for stacked bars
    labs(x = "",
         y = "") +
    scale_y_continuous(labels = scales::comma) + # Format y-axis labels as comma-separated numbers
    theme_minimal() +
    theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
          legend.position = "bottom", # Position legend at the bottom
          plot.title = element_text(hjust = 0.5)) # Center the plot title
  
  # Convert ggplot to plotly, ensuring custom text is used for hover
  ggplotly(p, tooltip = "text", source = "cloudenrolmentRegionPlot") %>%
    layout(hoverlabel = list(bgcolor = "white"),
           margin = list(b = 100)) # Increase bottom margin for x-axis labels
})

output$muslim_regional_profile_graph <- renderPlotly({
  # Use the reactive filtered data
  
  data_to_display <- cloud %>% filter(BASIC.REGION == input$cloud_region_profile_filter) %>%
    select(BASIC.REGION, MUSLIM.KINDER.MUSLIM, MUSLIM.G1.MUSLIM, MUSLIM.G2.MUSLIM, MUSLIM.G3.MUSLIM, MUSLIM.G4.MUSLIM, MUSLIM.G5.MUSLIM, MUSLIM.G6.MUSLIM, MUSLIM.G7.MUSLIM, MUSLIM.G8.MUSLIM, MUSLIM.G9.MUSLIM, MUSLIM.G10.MUSLIM, MUSLIM.G11, MUSLIM.G12) %>% 
    mutate(across(2:last_col(), as.numeric)) %>% 
    mutate(BASIC.REGION = as.character(BASIC.REGION)) %>%
    filter(BASIC.REGION != "") %>% 
    group_by(BASIC.REGION) %>%
    summarise(across(everything(), sum, na.rm = TRUE)) %>% 
    rename(
      "REGION" = BASIC.REGION,
      'KINDER' = MUSLIM.KINDER.MUSLIM,
      "GRADE 1" = MUSLIM.G1.MUSLIM,
      "GRADE 2" = MUSLIM.G2.MUSLIM,
      "GRADE 3" = MUSLIM.G3.MUSLIM,
      "GRADE 4" = MUSLIM.G4.MUSLIM,
      "GRADE 5" = MUSLIM.G5.MUSLIM,
      "GRADE 6" = MUSLIM.G6.MUSLIM,
      "GRADE 7" = MUSLIM.G7.MUSLIM,
      "GRADE 8" = MUSLIM.G8.MUSLIM,
      "GRADE 9" = MUSLIM.G9.MUSLIM,
      "GRADE 10" = MUSLIM.G10.MUSLIM,
      "GRADE 11" = MUSLIM.G11,
      "GRADE 12" = MUSLIM.G12
    )  %>% 
    pivot_longer(
      cols = 2:last_col(), # Specifies the columns to pivot
      names_to = "Muslim_Learners", # The new column to hold the original column names
      values_to = "Count" # The new column to hold the values
    )  %>%
    mutate(Muslim_Learners = factor(Muslim_Learners, levels = c(
      "KINDER", "GRADE 1", "GRADE 2", "GRADE 3",
      "GRADE 4", "GRADE 5", "GRADE 6",
      "GRADE 7", "GRADE 8", "GRADE 9",
      "GRADE 10", "GRADE 11", "GRADE 12"
    ))) 
  
  current_filtered_data <- data_to_display
  
  # --- Empty Data Handling ---
  if (nrow(current_filtered_data) == 0) {
    return(ggplotly(ggplot() +
                      annotate("text", x = 0.5, y = 0.5, label = "No data for selected regions/divisions") +
                      theme_void()))
  }
  
  # Prepare the data for plotting
  plot_data <- data_to_display
  
  # Create the ggplot
  p <- ggplot(plot_data,
              aes(x = REGION, # Reorder regions based on overall total count for the region
                  y = Count,
                  fill = Muslim_Learners, # Fill by Clustering.Status for coloring and consistent order
                  text = paste("Region: ", REGION,
                               "<br>Section: ", Muslim_Learners,
                               "<br>Count: ", scales::comma(Count)))) + # Custom tooltip text
    geom_bar(stat = "identity", position = "dodge", color = "black") + # Changed to position="stack" for stacked bars
    labs(x = "",
         y = "") +
    scale_y_continuous(labels = scales::comma) + # Format y-axis labels as comma-separated numbers
    theme_minimal() +
    theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
          legend.position = "bottom", # Position legend at the bottom
          plot.title = element_text(hjust = 0.5)) # Center the plot title
  
  # Convert ggplot to plotly, ensuring custom text is used for hover
  ggplotly(p, tooltip = "text", source = "cloudenrolmentRegionPlot") %>%
    layout(hoverlabel = list(bgcolor = "white"),
           margin = list(b = 100)) # Increase bottom margin for x-axis labels
})

output$totalenrolment_regional_profile_graph <- renderPlotly({
  # Use the reactive filtered data
  t1 <- cloud  %>% filter(BASIC.REGION == input$cloud_region_profile_filter) %>%  
    select(BASIC.REGION,ENROLLMENT.KINDER,ENROLLMENT.G1,ENROLLMENT.G2,ENROLLMENT.G3,ENROLLMENT.G4,ENROLLMENT.G5,ENROLLMENT.G6,ENROLLMENT.G7,ENROLLMENT.G8,ENROLLMENT.G9,ENROLLMENT.G10,ENROLMENT.G11,ENROLMENT.G12) %>% mutate(Total.Enrollment = rowSums(select(., 2:ncol(.)))) %>% select(BASIC.REGION,Total.Enrollment)
  
  t2 <- cloud %>% filter(BASIC.REGION == input$cloud_region_profile_filter) %>%
    select(BASIC.REGION, ENROLMENT.KINDER.SNED, SPECIAL.NEEDS.G1, SPECIAL.NEEDS.G2, SPECIAL.NEEDS.G3, SPECIAL.NEEDS.G4, SPECIAL.NEEDS.G5, SPECIAL.NEEDS.G6, SPECIAL.NEEDS.G7, SPECIAL.NEEDS.G8, SPECIAL.NEEDS.G9, SPECIAL.NEEDS.G10) %>% mutate(Total.SNED = rowSums(select(., 2:ncol(.)))) %>% select(Total.SNED)
  
  t3 <- cloud %>% filter(BASIC.REGION == input$cloud_region_profile_filter) %>%
    select(BASIC.REGION, IP.KINDER.IP, IP.G1.IP, IP.G2.IP, IP.G3.IP, IP.G4.IP, IP.G5.IP, IP.G6.IP, IP.G7.IP, IP.G8.IP, IP.G9.IP, IP.G10.IP, IP.G11, IP.G12) %>% mutate(Total.IP = rowSums(select(., 2:ncol(.)))) %>% select(Total.IP)
  
  t4 <- cloud %>% filter(BASIC.REGION == input$cloud_region_profile_filter) %>%
    select(BASIC.REGION, MUSLIM.KINDER.MUSLIM, MUSLIM.G1.MUSLIM, MUSLIM.G2.MUSLIM, MUSLIM.G3.MUSLIM, MUSLIM.G4.MUSLIM, MUSLIM.G5.MUSLIM, MUSLIM.G6.MUSLIM, MUSLIM.G7.MUSLIM, MUSLIM.G8.MUSLIM, MUSLIM.G9.MUSLIM, MUSLIM.G10.MUSLIM, MUSLIM.G11, MUSLIM.G12) %>% mutate(Total.Muslim = rowSums(select(., 2:ncol(.)))) %>% select(Total.Muslim)
  
  t5 <- cloud %>% filter(BASIC.REGION == input$cloud_region_profile_filter) %>%
    select(BASIC.REGION, DISPLACED.KINDERGARTEN.DISPLACED.LEARNER, DISPLACED.GRADE1.DISPLACED.LEARNER, DISPLACED.GRADE2.DISPLACED.LEARNER, DISPLACED.GRADE3.DISPLACED.LEARNER, DISPLACED.GRADE4.DISPLACED.LEARNER, DISPLACED.GRADE5.DISPLACED.LEARNER, DISPLACED.GRADE6.DISPLACED.LEARNER,DISPLACED.G7.DISPLACED,DISPLACED.G8.DISPLACED,DISPLACED.G9.DISPLACED,DISPLACED.G10.DISPLACED,DISPLACED.G11,DISPLACED.G12) %>% mutate(Total.Displaced = rowSums(select(., 2:ncol(.)))) %>% select(Total.Displaced)
  
  tall <- cbind(t1,t2,t3,t4,t5)
  
  data_to_display <- tall %>% 
    mutate(across(2:last_col(), as.numeric)) %>% 
    mutate(BASIC.REGION = as.character(BASIC.REGION)) %>%
    filter(BASIC.REGION != "") %>% 
    group_by(BASIC.REGION) %>%
    summarise(across(everything(), sum, na.rm = TRUE)) %>% 
    rename(
      "REGION" = BASIC.REGION,
      "SNED LEARNERS" = Total.SNED,
      "MUSLIM LEARNERS" = Total.Muslim,
      "IP LEARNERS" = Total.IP,
      "DISPLACED LEARNERS" = Total.Displaced,
      "TOTAL LEARNERS" = Total.Enrollment
    )  %>% 
    pivot_longer(
      cols = 2:last_col(), # Specifies the columns to pivot
      names_to = "Learner_Type", # The new column to hold the original column names
      values_to = "Count" # The new column to hold the values
    )  %>%
    mutate(Learner_Type = factor(Learner_Type, levels = c(
      "SNED LEARNERS", "MUSLIM LEARNERS", "IP LEARNERS",
      "DISPLACED LEARNERS", "TOTAL LEARNERS"
    ))) 
  
  current_filtered_data <- data_to_display
  
  # --- Empty Data Handling ---
  if (nrow(current_filtered_data) == 0) {
    return(ggplotly(ggplot() +
                      annotate("text", x = 0.5, y = 0.5, label = "No data for selected regions/divisions") +
                      theme_void()))
  }
  
  # Prepare the data for plotting
  plot_data <- data_to_display
  
  # Create the ggplot
  p <- ggplot(plot_data,
              aes(x = REGION, # Reorder regions based on overall total count for the region
                  y = Count,
                  fill = Learner_Type, # Fill by Clustering.Status for coloring and consistent order
                  text = paste("Region: ", REGION,
                               "<br>Section: ", Learner_Type,
                               "<br>Count: ", scales::comma(Count)))) + # Custom tooltip text
    geom_bar(stat = "identity", position = "dodge", color = "black") + # Changed to position="dodge" for dodgeed bars
    labs(x = "Region",
         y = "") +
    scale_y_continuous(labels = scales::comma) + # Format y-axis labels as comma-separated numbers
    theme_minimal() +
    theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
          legend.position = "bottom", # Position legend at the bottom
          plot.title = element_text(hjust = 0.5)) # Center the plot title
  
  # Convert ggplot to plotly, ensuring custom text is used for hover
  ggplotly(p, tooltip = "text", source = "cloudenrolmentRegionPlot") %>%
    layout(hoverlabel = list(bgcolor = "white"),
           margin = list(b = 100)) # Increase bottom margin for x-axis labels
})

output$displaced_regional_profile_graph <- renderPlotly({
  # Use the reactive filtered data
  
  data_to_display <- cloud %>% filter(BASIC.REGION == input$cloud_region_profile_filter) %>%
    select(BASIC.REGION, DISPLACED.KINDERGARTEN.DISPLACED.LEARNER, DISPLACED.GRADE1.DISPLACED.LEARNER, DISPLACED.GRADE2.DISPLACED.LEARNER, DISPLACED.GRADE3.DISPLACED.LEARNER, DISPLACED.GRADE4.DISPLACED.LEARNER, DISPLACED.GRADE5.DISPLACED.LEARNER, DISPLACED.GRADE6.DISPLACED.LEARNER,DISPLACED.G7.DISPLACED,DISPLACED.G8.DISPLACED,DISPLACED.G9.DISPLACED,DISPLACED.G10.DISPLACED,DISPLACED.G11,DISPLACED.G12) %>% 
    mutate(across(2:last_col(), as.numeric)) %>% 
    mutate(BASIC.REGION = as.character(BASIC.REGION)) %>%
    filter(BASIC.REGION != "") %>% 
    group_by(BASIC.REGION) %>%
    summarise(across(everything(), sum, na.rm = TRUE)) %>% 
    rename(
      "REGION" = BASIC.REGION,
      "KINDER" = DISPLACED.KINDERGARTEN.DISPLACED.LEARNER,
      "GRADE 1" = DISPLACED.GRADE1.DISPLACED.LEARNER,
      "GRADE 2" = DISPLACED.GRADE2.DISPLACED.LEARNER,
      "GRADE 3" = DISPLACED.GRADE3.DISPLACED.LEARNER,
      "GRADE 4" = DISPLACED.GRADE4.DISPLACED.LEARNER,
      "GRADE 5" = DISPLACED.GRADE5.DISPLACED.LEARNER,
      "GRADE 6" = DISPLACED.GRADE6.DISPLACED.LEARNER,
      "GRADE 7" = DISPLACED.G7.DISPLACED,
      "GRADE 8" = DISPLACED.G8.DISPLACED,
      "GRADE 9" = DISPLACED.G9.DISPLACED,
      "GRADE 10" = DISPLACED.G10.DISPLACED,
      "GRADE 11" = DISPLACED.G11,
      "GRADE 12" = DISPLACED.G12
    )  %>% 
    pivot_longer(
      cols = 2:last_col(), # Specifies the columns to pivot
      names_to = "Displaced_Learners", # The new column to hold the original column names
      values_to = "Count" # The new column to hold the values
    )  %>%
    mutate(Displaced_Learners = factor(Displaced_Learners, levels = c(
      "KINDER", "GRADE 1", "GRADE 2", "GRADE 3",
      "GRADE 4", "GRADE 5", "GRADE 6",
      "GRADE 7", "GRADE 8", "GRADE 9",
      "GRADE 10", "GRADE 11", "GRADE 12"
    ))) 
  
  current_filtered_data <- data_to_display
  
  # --- Empty Data Handling ---
  if (nrow(current_filtered_data) == 0) {
    return(ggplotly(ggplot() +
                      annotate("text", x = 0.5, y = 0.5, label = "No data for selected regions/divisions") +
                      theme_void()))
  }
  
  # Prepare the data for plotting
  plot_data <- data_to_display
  
  # Create the ggplot
  p <- ggplot(plot_data,
              aes(x = REGION, # Reorder regions based on overall total count for the region
                  y = Count,
                  fill = Displaced_Learners, # Fill by Clustering.Status for coloring and consistent order
                  text = paste("Region: ", REGION,
                               "<br>Section: ", Displaced_Learners,
                               "<br>Count: ", scales::comma(Count)))) + # Custom tooltip text
    geom_bar(stat = "identity", position = "dodge", color = "black") + # Changed to position="dodge" for dodgeed bars
    labs(x = "Region",
         y = "") +
    scale_y_continuous(labels = scales::comma) + # Format y-axis labels as comma-separated numbers
    theme_minimal() +
    theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
          legend.position = "bottom", # Position legend at the bottom
          plot.title = element_text(hjust = 0.5)) # Center the plot title
  
  # Convert ggplot to plotly, ensuring custom text is used for hover
  ggplotly(p, tooltip = "text", source = "cloudenrolmentRegionPlot") %>%
    layout(hoverlabel = list(bgcolor = "white"),
           margin = list(b = 100)) # Increase bottom margin for x-axis labels
})


output$als_regional_profile_graph <- renderPlotly({
  # Use the reactive filtered data
  
  data_to_display <- cloud %>% filter(BASIC.REGION == input$cloud_region_profile_filter) %>%
    select(BASIC.REGION, contains("ENROLLEDALS.")) %>% 
    mutate(across(2:last_col(), as.numeric)) %>% 
    mutate(BASIC.REGION = as.character(BASIC.REGION)) %>%
    filter(BASIC.REGION != "") %>% 
    group_by(BASIC.REGION) %>%
    summarise(across(everything(), sum, na.rm = TRUE)) %>% 
    rename(
      "REGION" = BASIC.REGION,
      "ES" = ENROLLEDALS.ELEM.ALS,
      "JHS" = ENROLLEDALS.JHS.ALS,
      "SHS" = ENROLLEDALS.SHS.ALS
    )  %>% 
    pivot_longer(
      cols = 2:last_col(), # Specifies the columns to pivot
      names_to = "ALS_Learners", # The new column to hold the original column names
      values_to = "Count" # The new column to hold the values
    )
  
  current_filtered_data <- data_to_display
  
  # --- Empty Data Handling ---
  if (nrow(current_filtered_data) == 0) {
    return(ggplotly(ggplot() +
                      annotate("text", x = 0.5, y = 0.5, label = "No data for selected regions/divisions") +
                      theme_void()))
  }
  
  # Prepare the data for plotting
  plot_data <- data_to_display
  
  # Create the ggplot
  p <- ggplot(plot_data,
              aes(x = REGION, # Reorder regions based on overall total count for the region
                  y = Count,
                  fill = ALS_Learners, # Fill by Clustering.Status for coloring and consistent order
                  text = paste("Region: ", REGION,
                               "<br>Section: ", ALS_Learners,
                               "<br>Count: ", scales::comma(Count)))) + # Custom tooltip text
    geom_bar(stat = "identity", position = "dodge", color = "black") + # Changed to position="dodge" for dodgeed bars
    labs(x = "",
         y = "") +
    scale_y_continuous(labels = scales::comma) + # Format y-axis labels as comma-separated numbers
    theme_minimal() +
    theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
          legend.position = "bottom", # Position legend at the bottom
          plot.title = element_text(hjust = 0.5)) # Center the plot title
  
  # Convert ggplot to plotly, ensuring custom text is used for hover
  ggplotly(p, tooltip = "text", source = "cloudenrolmentRegionPlot") %>%
    layout(hoverlabel = list(bgcolor = "white"),
           margin = list(b = 100)) # Increase bottom margin for x-axis labels
})

output$dropout_regional_profile_graph <- renderPlotly({
  # Use the reactive filtered data
  
  data_to_display <- cloud %>% filter(BASIC.REGION == input$cloud_region_profile_filter) %>%
    select(BASIC.REGION, DROPOUT.PREVSY) %>% 
    mutate(across(2:last_col(), as.numeric)) %>% 
    mutate(BASIC.REGION = as.character(BASIC.REGION)) %>%
    filter(BASIC.REGION != "") %>% 
    group_by(BASIC.REGION) %>%
    summarise(across(everything(), sum, na.rm = TRUE)) %>% 
    rename(
      "REGION" = BASIC.REGION,
      "NUMBER OF DROPOUTS (PREVIOUS SY)" = DROPOUT.PREVSY) %>% 
    pivot_longer(
      cols = 2:last_col(), # Specifies the columns to pivot
      names_to = "Sections", # The new column to hold the original column names
      values_to = "Count" # The new column to hold the values
    )
  
  current_filtered_data <- data_to_display
  
  # --- Empty Data Handling ---
  if (nrow(current_filtered_data) == 0) {
    return(ggplotly(ggplot() +
                      annotate("text", x = 0.5, y = 0.5, label = "No data for selected regions/divisions") +
                      theme_void()))
  }
  
  # Prepare the data for plotting
  plot_data <- data_to_display
  
  # Create the ggplot
  p <- ggplot(plot_data,
              aes(x = REGION, # Reorder regions based on overall total count for the region
                  y = Count,
                  fill = Sections, # Fill by Clustering.Status for coloring and consistent order
                  text = paste("Region: ", REGION,
                               "<br>Section: ", Sections,
                               "<br>Count: ", scales::comma(Count)))) + # Custom tooltip text
    geom_bar(stat = "identity", position = "dodge", color = "black") + # Changed to position="dodge" for dodgeed bars
    labs(x = "",
         y = "") +
    scale_y_continuous(labels = scales::comma) + # Format y-axis labels as comma-separated numbers
    theme_minimal() +
    theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
          legend.position = "bottom", # Position legend at the bottom
          plot.title = element_text(hjust = 0.5)) # Center the plot title
  
  # Convert ggplot to plotly, ensuring custom text is used for hover
  ggplotly(p, tooltip = "text", source = "cloudenrolmentRegionPlot") %>%
    layout(hoverlabel = list(bgcolor = "white"),
           margin = list(b = 100)) # Increase bottom margin for x-axis labels
})


output$sosss_regional_profile_graph <- renderPlotly({
  # Use the reactive filtered data
  
  data_to_display <- cloud %>% filter(BASIC.REGION == input$cloud_region_profile_filter) %>%
    select(BASIC.REGION, contains("SOSSS.")) %>% 
    mutate(across(2:last_col(), as.numeric)) %>% 
    mutate(BASIC.REGION = as.character(BASIC.REGION)) %>%
    filter(BASIC.REGION != "") %>% 
    group_by(BASIC.REGION) %>%
    summarise(across(everything(), sum, na.rm = TRUE))  %>% 
    pivot_longer(
      cols = 2:last_col(), # Specifies the columns to pivot
      names_to = "Sections", # The new column to hold the original column names
      values_to = "Count" # The new column to hold the values
    )
  
  current_filtered_data <- data_to_display
  
  # --- Empty Data Handling ---
  if (nrow(current_filtered_data) == 0) {
    return(ggplotly(ggplot() +
                      annotate("text", x = 0.5, y = 0.5, label = "No data for selected regions/divisions") +
                      theme_void()))
  }
  
  # Prepare the data for plotting
  plot_data <- data_to_display
  
  # Create the ggplot
  p <- ggplot(plot_data,
              aes(x = BASIC.REGION, # Reorder regions based on overall total count for the region
                  y = Count,
                  fill = Sections, # Fill by Clustering.Status for coloring and consistent order
                  text = paste("Region: ", BASIC.REGION,
                               "<br>Section: ", Sections,
                               "<br>Count: ", scales::comma(Count)))) + # Custom tooltip text
    geom_bar(stat = "identity", position = "dodge", color = "black") + # Changed to position="dodge" for dodgeed bars
    labs(x = "",
         y = "") +
    scale_y_continuous(labels = scales::comma) + # Format y-axis labels as comma-separated numbers
    theme_minimal() +
    theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
          legend.position = "bottom", # Position legend at the bottom
          plot.title = element_text(hjust = 0.5)) # Center the plot title
  
  # Convert ggplot to plotly, ensuring custom text is used for hover
  ggplotly(p, tooltip = "text", source = "cloudenrolmentRegionPlot") %>%
    layout(hoverlabel = list(bgcolor = "white"),
           margin = list(b = 100)) # Increase bottom margin for x-axis labels
})

output$extension_regional_profile_graph <- renderPlotly({
  # Use the reactive filtered data
  
  data_to_display <- cloud %>% filter(BASIC.REGION == input$cloud_region_profile_filter) %>%
    select(BASIC.REGION, contains("EXTENSION.")) %>% 
    mutate(across(2:last_col(), as.numeric)) %>% 
    mutate(BASIC.REGION = as.character(BASIC.REGION)) %>%
    filter(BASIC.REGION != "") %>% 
    group_by(BASIC.REGION) %>%
    summarise(across(everything(), sum, na.rm = TRUE))  %>% 
    pivot_longer(
      cols = 2:last_col(), # Specifies the columns to pivot
      names_to = "Sections", # The new column to hold the original column names
      values_to = "Count" # The new column to hold the values
    )
  
  current_filtered_data <- data_to_display
  
  # --- Empty Data Handling ---
  if (nrow(current_filtered_data) == 0) {
    return(ggplotly(ggplot() +
                      annotate("text", x = 0.5, y = 0.5, label = "No data for selected regions/divisions") +
                      theme_void()))
  }
  
  # Prepare the data for plotting
  plot_data <- data_to_display
  
  # Create the ggplot
  p <- ggplot(plot_data,
              aes(x = BASIC.REGION, # Reorder regions based on overall total count for the region
                  y = Count,
                  fill = Sections, # Fill by Clustering.Status for coloring and consistent order
                  text = paste("Region: ", BASIC.REGION,
                               "<br>Section: ", Sections,
                               "<br>Count: ", scales::comma(Count)))) + # Custom tooltip text
    geom_bar(stat = "identity", position = "dodge", color = "black") + # Changed to position="dodge" for dodgeed bars
    labs(x = "",
         y = "") +
    scale_y_continuous(labels = scales::comma) + # Format y-axis labels as comma-separated numbers
    theme_minimal() +
    theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
          legend.position = "bottom", # Position legend at the bottom
          plot.title = element_text(hjust = 0.5)) # Center the plot title
  
  # Convert ggplot to plotly, ensuring custom text is used for hover
  ggplotly(p, tooltip = "text", source = "cloudenrolmentRegionPlot") %>%
    layout(hoverlabel = list(bgcolor = "white"),
           margin = list(b = 100)) # Increase bottom margin for x-axis labels
})

output$teacherinventory_regional_profile_graph <- renderPlotly({
  # Use the reactive filtered data
  
  data_to_display <- cloud %>% filter(BASIC.REGION == input$cloud_region_profile_filter) %>%
    select(BASIC.REGION, TEACHING.PERSONNEL.ELEM, TEACHING.PERSONNEL.JHS, TEACHING.PERSONNEL.SHS) %>% 
    mutate(across(2:last_col(), as.numeric)) %>% 
    mutate(BASIC.REGION = as.character(BASIC.REGION)) %>%
    filter(BASIC.REGION != "") %>% 
    group_by(BASIC.REGION) %>%
    summarise(across(everything(), sum, na.rm = TRUE)) %>% 
    rename(
      "REGION" = BASIC.REGION,
      "ES" = TEACHING.PERSONNEL.ELEM,
      "JHS" = TEACHING.PERSONNEL.JHS,
      "SHS" = TEACHING.PERSONNEL.SHS
    )  %>% 
    pivot_longer(
      cols = 2:last_col(), # Specifies the columns to pivot
      names_to = "Number_of_Teaching_Personnel", # The new column to hold the original column names
      values_to = "Count" # The new column to hold the values
    )
  
  current_filtered_data <- data_to_display
  
  # --- Empty Data Handling ---
  if (nrow(current_filtered_data) == 0) {
    return(ggplotly(ggplot() +
                      annotate("text", x = 0.5, y = 0.5, label = "No data for selected regions/divisions") +
                      theme_void()))
  }
  
  # Prepare the data for plotting
  plot_data <- data_to_display
  
  # Create the ggplot
  p <- ggplot(plot_data,
              aes(x = REGION, # Reorder regions based on overall total count for the region
                  y = Count,
                  fill = Number_of_Teaching_Personnel, # Fill by Clustering.Status for coloring and consistent order
                  text = paste("Region: ", REGION,
                               "<br>Section: ", Number_of_Teaching_Personnel,
                               "<br>Count: ", scales::comma(Count)))) + # Custom tooltip text
    geom_bar(stat = "identity", position = "dodge", color = "black") + # Changed to position="dodge" for dodgeed bars
    labs(x = "",
         y = "") +
    scale_y_continuous(labels = scales::comma) + # Format y-axis labels as comma-separated numbers
    theme_minimal() +
    theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
          legend.position = "bottom", # Position legend at the bottom
          plot.title = element_text(hjust = 0.5)) # Center the plot title
  
  # Convert ggplot to plotly, ensuring custom text is used for hover
  ggplotly(p, tooltip = "text", source = "cloudenrolmentRegionPlot") %>%
    layout(hoverlabel = list(bgcolor = "white"),
           margin = list(b = 100)) # Increase bottom margin for x-axis labels
})


output$classroom_regional_profile_graph <- renderPlotly({
  # Use the reactive filtered data
  
  data_to_display <- cloud %>% filter(BASIC.REGION == input$cloud_region_profile_filter) %>%
    select(BASIC.REGION, NO.CLASSROOM.ELEM, NO.CLASSROOM.SNED.ELEM, NO.CLASSROOM.JHS, NO.CLASSROOM.SNED.JHS, NO.CLASSROOM.SHS) %>% 
    mutate(across(2:last_col(), as.numeric)) %>% 
    mutate(BASIC.REGION = as.character(BASIC.REGION)) %>%
    filter(BASIC.REGION != "") %>% 
    group_by(BASIC.REGION) %>%
    summarise(across(everything(), sum, na.rm = TRUE)) %>% 
    rename(
      "REGION" = BASIC.REGION,
      "ES" = NO.CLASSROOM.ELEM,
      "JHS" = NO.CLASSROOM.JHS,
      "SHS" = NO.CLASSROOM.SHS,
      "SNED-ES" = NO.CLASSROOM.SNED.ELEM,
      "SNED-JHS" = NO.CLASSROOM.SNED.JHS)  %>% 
    pivot_longer(
      cols = 2:last_col(), # Specifies the columns to pivot
      names_to = "Number_of_Classrooms", # The new column to hold the original column names
      values_to = "Count" # The new column to hold the values
    )
  
  current_filtered_data <- data_to_display
  
  # --- Empty Data Handling ---
  if (nrow(current_filtered_data) == 0) {
    return(ggplotly(ggplot() +
                      annotate("text", x = 0.5, y = 0.5, label = "No data for selected regions/divisions") +
                      theme_void()))
  }
  
  # Prepare the data for plotting
  plot_data <- data_to_display
  
  # Create the ggplot
  p <- ggplot(plot_data,
              aes(x = REGION, # Reorder regions based on overall total count for the region
                  y = Count,
                  fill = Number_of_Classrooms, # Fill by Clustering.Status for coloring and consistent order
                  text = paste("Region: ", REGION,
                               "<br>Section: ", Number_of_Classrooms,
                               "<br>Count: ", scales::comma(Count)))) + # Custom tooltip text
    geom_bar(stat = "identity", position = "dodge", color = "black") + # Changed to position="dodge" for dodgeed bars
    labs(x = "",
         y = "") +
    scale_y_continuous(labels = scales::comma) + # Format y-axis labels as comma-separated numbers
    theme_minimal() +
    theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
          legend.position = "bottom", # Position legend at the bottom
          plot.title = element_text(hjust = 0.5))
  
  # Convert ggplot to plotly, ensuring custom text is used for hover
  ggplotly(p, tooltip = "text", source = "cloudenrolmentRegionPlot") %>%
    layout(hoverlabel = list(bgcolor = "white"),
           margin = list(b = 100)) # Increase bottom margin for x-axis labels
})

output$multigrade_regional_profile_graph <- renderPlotly({
  # Use the reactive filtered data
  
  data_to_display <- cloud %>% filter(BASIC.REGION == input$cloud_region_profile_filter) %>%
    select(BASIC.REGION, contains("MG.")) %>% 
    mutate(across(2:last_col(), as.numeric)) %>% 
    mutate(BASIC.REGION = as.character(BASIC.REGION)) %>%
    filter(BASIC.REGION != "") %>% 
    group_by(BASIC.REGION) %>%
    summarise(across(everything(), sum, na.rm = TRUE)) %>% 
    rename(
      "REGION" = BASIC.REGION,
      "G1 & G2" = MG.CLASSES.1.2,
      "G3 & G4" = MG.CLASSES.3.4,
      "G5 & G6" = MG.CLASSES.5.6
    )  %>% 
    pivot_longer(
      cols = 2:last_col(), # Specifies the columns to pivot
      names_to = "Multigrade", # The new column to hold the original column names
      values_to = "Count" # The new column to hold the values
    )
  
  current_filtered_data <- data_to_display
  
  # --- Empty Data Handling ---
  if (nrow(current_filtered_data) == 0) {
    return(ggplotly(ggplot() +
                      annotate("text", x = 0.5, y = 0.5, label = "No data for selected regions/divisions") +
                      theme_void()))
  }
  
  # Prepare the data for plotting
  plot_data <- data_to_display
  
  # Create the ggplot
  p <- ggplot(plot_data,
              aes(x = REGION, # Reorder regions based on overall total count for the region
                  y = Count,
                  fill = Multigrade, # Fill by Clustering.Status for coloring and consistent order
                  text = paste("Region: ", REGION,
                               "<br>Section: ", Multigrade,
                               "<br>Count: ", scales::comma(Count)))) + # Custom tooltip text
    geom_bar(stat = "identity", position = "dodge", color = "black") + # Changed to position="dodge" for dodgeed bars
    labs(x = "",
         y = "") +
    scale_y_continuous(labels = scales::comma) + # Format y-axis labels as comma-separated numbers
    theme_minimal() +
    theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
          legend.position = "bottom", # Position legend at the bottom
          plot.title = element_text(hjust = 0.5)) # Center the plot title
  
  # Convert ggplot to plotly, ensuring custom text is used for hover
  ggplotly(p, tooltip = "text", source = "cloudenrolmentRegionPlot") %>%
    layout(hoverlabel = list(bgcolor = "white"),
           margin = list(b = 100)) # Increase bottom margin for x-axis labels
})

output$organizedclass_regional_profile_graph <- renderPlotly({
  # Use the reactive filtered data
  
  data_to_display <- cloud %>% filter(BASIC.REGION == input$cloud_region_profile_filter) %>%
    select(BASIC.REGION, CLASS.ORG.BELOW, CLASS.ORG.ABOVE, CLASS.ORG.WITHIN) %>% 
    mutate(across(2:last_col(), as.numeric)) %>% 
    mutate(BASIC.REGION = as.character(BASIC.REGION)) %>%
    filter(BASIC.REGION != "") %>% 
    group_by(BASIC.REGION) %>%
    summarise(across(everything(), sum, na.rm = TRUE)) %>% 
    rename(
      "REGION" = BASIC.REGION,
      "BELOW THE STANDARD" = CLASS.ORG.BELOW,
      "ABOVE THE STANDARD" = CLASS.ORG.ABOVE,
      "WITHIN THE STANDARD" = CLASS.ORG.WITHIN
    )  %>% 
    pivot_longer(
      cols = 2:last_col(), # Specifies the columns to pivot
      names_to = "Classes_Organized", # The new column to hold the original column names
      values_to = "Count" # The new column to hold the values
    ) %>%
    # This is the key change: Convert 'Classes_Organized' to a factor with specific levels
    mutate(Classes_Organized = factor(Classes_Organized, levels = c(
      "BELOW THE STANDARD",
      "WITHIN THE STANDARD",
      "ABOVE THE STANDARD"
    )))
  
  current_filtered_data <- data_to_display
  
  # --- Empty Data Handling ---
  if (nrow(current_filtered_data) == 0) {
    return(ggplotly(ggplot() +
                      annotate("text", x = 0.5, y = 0.5, label = "No data for selected regions/divisions") +
                      theme_void()))
  }
  
  # Prepare the data for plotting
  plot_data <- data_to_display
  
  # Create the ggplot
  p <- ggplot(plot_data,
              aes(x = REGION, # Reorder regions based on overall total count for the region
                  y = Count,
                  fill = Classes_Organized, # Fill by Clustering.Status for coloring and consistent order
                  text = paste("Region: ", REGION,
                               "<br>Section: ", Classes_Organized,
                               "<br>Count: ", scales::comma(Count)))) + # Custom tooltip text
    geom_bar(stat = "identity", position = "dodge", color = "black") + # Changed to position="dodge" for dodgeed bars
    labs(x = "",
         y = "") +
    scale_y_continuous(labels = scales::comma) + # Format y-axis labels as comma-separated numbers
    scale_fill_manual(values = c(
      "BELOW THE STANDARD" = "blue",
      "WITHIN THE STANDARD" = "green", # Optional: you can set a color for this too
      "ABOVE THE STANDARD" = "red"
    )) +
    theme_minimal() +
    theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
          legend.position = "bottom", # Position legend at the bottom
          plot.title = element_text(hjust = 0.5)) # Center the plot title
  
  # Convert ggplot to plotly, ensuring custom text is used for hover
  ggplotly(p, tooltip = "text", source = "cloudenrolmentRegionPlot") %>%
    layout(hoverlabel = list(bgcolor = "white"),
           margin = list(b = 100)) # Increase bottom margin for x-axis labels
})

output$jhsdeployment_regional_profile_graph <- renderPlotly({
  # Use the reactive filtered data
  
  data_to_display <- cloud %>% filter(BASIC.REGION == input$cloud_region_profile_filter) %>%
    select(BASIC.REGION,Actual.Teacher.AFA,Actual.Teacher.FCS,Actual.Teacher.IA,Actual.Teacher.ICT) %>% 
    mutate(across(2:last_col(), as.numeric)) %>% 
    mutate(BASIC.REGION = as.character(BASIC.REGION)) %>%
    filter(BASIC.REGION != "") %>% 
    group_by(BASIC.REGION) %>%
    summarise(across(everything(), sum, na.rm = TRUE)) %>% 
    rename(
      "REGION" = BASIC.REGION,
      "AFA" = Actual.Teacher.AFA,
      "FCS" = Actual.Teacher.FCS,
      "IA" = Actual.Teacher.IA,
      "ICT" = Actual.Teacher.ICT) %>% 
    pivot_longer(
      cols = 2:last_col(), # Specifies the columns to pivot
      names_to = "Actual_JHS_Teachers_Deployed", # The new column to hold the original column names
      values_to = "Count" # The new column to hold the values
    )
  
  current_filtered_data <- data_to_display
  
  # --- Empty Data Handling ---
  if (nrow(current_filtered_data) == 0) {
    return(ggplotly(ggplot() +
                      annotate("text", x = 0.5, y = 0.5, label = "No data for selected regions/divisions") +
                      theme_void()))
  }
  
  # Prepare the data for plotting
  plot_data <- data_to_display
  
  # Create the ggplot
  p <- ggplot(plot_data,
              aes(x = REGION, # Reorder regions based on overall total count for the region
                  y = Count,
                  fill = Actual_JHS_Teachers_Deployed, # Fill by Clustering.Status for coloring and consistent order
                  text = paste("Region: ", REGION,
                               "<br>Section: ", Actual_JHS_Teachers_Deployed,
                               "<br>Count: ", scales::comma(Count)))) + # Custom tooltip text
    geom_bar(stat = "identity", position = "dodge", color = "black") + # Changed to position="dodge" for dodgeed bars
    labs(x = "",
         y = "") +
    scale_y_continuous(labels = scales::comma) + # Format y-axis labels as comma-separated numbers
    theme_minimal() +
    theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
          legend.position = "bottom", # Position legend at the bottom
          plot.title = element_text(hjust = 0.5)) # Center the plot title
  
  # Convert ggplot to plotly, ensuring custom text is used for hover
  ggplotly(p, tooltip = "text", source = "cloudenrolmentRegionPlot") %>%
    layout(hoverlabel = list(bgcolor = "white"),
           margin = list(b = 100)) # Increase bottom margin for x-axis labels
})

output$shifting_regional_profile_graph <- renderPlotly({
  # Use the reactive filtered data
  
  data_to_display <- cloud_v3 %>% filter(BASIC.REGION == input$cloud_region_profile_filter) %>%
    select(BASIC.REGION, contains("LAC.OFTEN")) %>% 
    mutate(across(2:last_col(), as.numeric)) %>% 
    mutate(BASIC.REGION = as.character(BASIC.REGION)) %>%
    filter(BASIC.REGION != "") %>% 
    group_by(BASIC.REGION) %>%
    summarise(across(everything(), sum, na.rm = TRUE))  %>% 
    rename("Annually" = LAC.OFTEN.ANNUALLY,
           "As need arises"= LAC.OFTEN.NEED.ARISES,
           "Once a month" = LAC.OFTEN.ONCE.A.MONTH,
           "Quarterly" = LAC.OFTEN.QUARTERLY,
           "Semi-annually" = LAC.OFTEN.SEMIANNUALLY,
           "Twice a month" = LAC.OFTEN.TWICE.A.MONTH
             ) %>% 
    pivot_longer(
      cols = 2:last_col(), # Specifies the columns to pivot
      names_to = "LAC_Sessions", # The new column to hold the original column names
      values_to = "Count" # The new column to hold the values
    )
  
  current_filtered_data <- data_to_display
  
  # --- Empty Data Handling ---
  if (nrow(current_filtered_data) == 0) {
    return(ggplotly(ggplot() +
                      annotate("text", x = 0.5, y = 0.5, label = "No data for selected regions/divisions") +
                      theme_void()))
  }
  
  # Prepare the data for plotting
  plot_data <- data_to_display
  
  # Create the ggplot
  p <- ggplot(plot_data,
              aes(x = BASIC.REGION, # Reorder regions based on overall total count for the region
                  y = Count,
                  fill = LAC_Sessions, # Fill by Clustering.Status for coloring and consistent order
                  text = paste("Region: ", BASIC.REGION,
                               "<br>Section: ", LAC_Sessions,
                               "<br>Count: ", scales::comma(Count)))) + # Custom tooltip text
    geom_bar(stat = "identity", position = "dodge", color = "black") + # Changed to position="dodge" for dodgeed bars
    labs(x = "",
         y = "") +
    scale_y_continuous(labels = scales::comma) + # Format y-axis labels as comma-separated numbers
    theme_minimal() +
    theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
          legend.position = "bottom", # Position legend at the bottom
          plot.title = element_text(hjust = 0.5)) # Center the plot title
  
  # Convert ggplot to plotly, ensuring custom text is used for hover
  ggplotly(p, tooltip = "text", source = "cloudenrolmentRegionPlot") %>%
    layout(hoverlabel = list(bgcolor = "white"),
           margin = list(b = 100)) # Increase bottom margin for x-axis labels
})

output$ldm_regional_profile_graph <- renderPlotly({
  # Use the reactive filtered data
  
  data_to_display <- cloud_v3 %>% filter(BASIC.REGION == input$cloud_region_profile_filter) %>%
    select(BASIC.REGION, contains("LDM.")) %>% 
    mutate(across(2:last_col(), as.numeric)) %>% 
    mutate(BASIC.REGION = as.character(BASIC.REGION)) %>%
    filter(BASIC.REGION != "") %>% 
    group_by(BASIC.REGION) %>%
    summarise(across(everything(), sum, na.rm = TRUE))  %>% 
    pivot_longer(
      cols = 2:last_col(), # Specifies the columns to pivot
      names_to = "Sections", # The new column to hold the original column names
      values_to = "Count" # The new column to hold the values
    )
  
  current_filtered_data <- data_to_display
  
  # --- Empty Data Handling ---
  if (nrow(current_filtered_data) == 0) {
    return(ggplotly(ggplot() +
                      annotate("text", x = 0.5, y = 0.5, label = "No data for selected regions/divisions") +
                      theme_void()))
  }
  
  # Prepare the data for plotting
  plot_data <- data_to_display
  
  # Create the ggplot
  p <- ggplot(plot_data,
              aes(x = BASIC.REGION, # Reorder regions based on overall total count for the region
                  y = Count,
                  fill = Sections, # Fill by Clustering.Status for coloring and consistent order
                  text = paste("Region: ", BASIC.REGION,
                               "<br>Section: ", Sections,
                               "<br>Count: ", scales::comma(Count)))) + # Custom tooltip text
    geom_bar(stat = "identity", position = "dodge", color = "black") + # Changed to position="dodge" for dodgeed bars
    labs(x = "",
         y = "") +
    scale_y_continuous(labels = scales::comma) + # Format y-axis labels as comma-separated numbers
    theme_minimal() +
    theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
          legend.position = "bottom", # Position legend at the bottom
          plot.title = element_text(hjust = 0.5)) # Center the plot title
  
  # Convert ggplot to plotly, ensuring custom text is used for hover
  ggplotly(p, tooltip = "text", source = "cloudenrolmentRegionPlot") %>%
    layout(hoverlabel = list(bgcolor = "white"),
           margin = list(b = 100)) # Increase bottom margin for x-axis labels
})

output$adm_regional_profile_graph <- renderPlotly({
  # Use the reactive filtered data
  
  data_to_display <- cloud_v2 %>% filter(BASIC.REGION == input$cloud_region_profile_filter) %>%
    select(BASIC.REGION, contains("ALTERNATIVE.ADM")) %>% 
    mutate(across(2:last_col(), as.numeric)) %>% 
    mutate(BASIC.REGION = as.character(BASIC.REGION)) %>%
    filter(BASIC.REGION != "") %>% 
    group_by(BASIC.REGION) %>%
    summarise(across(everything(), sum, na.rm = TRUE))  %>% 
    pivot_longer(
      cols = 2:last_col(), # Specifies the columns to pivot
      names_to = "Sections", # The new column to hold the original column names
      values_to = "Count" # The new column to hold the values
    )
  
  current_filtered_data <- data_to_display
  
  # --- Empty Data Handling ---
  if (nrow(current_filtered_data) == 0) {
    return(ggplotly(ggplot() +
                      annotate("text", x = 0.5, y = 0.5, label = "No data for selected regions/divisions") +
                      theme_void()))
  }
  
  # Prepare the data for plotting
  plot_data <- data_to_display
  
  # Create the ggplot
  p <- ggplot(plot_data,
              aes(x = BASIC.REGION, # Reorder regions based on overall total count for the region
                  y = Count,
                  fill = Sections, # Fill by Clustering.Status for coloring and consistent order
                  text = paste("Region: ", BASIC.REGION,
                               "<br>Section: ", Sections,
                               "<br>Count: ", scales::comma(Count)))) + # Custom tooltip text
    geom_bar(stat = "identity", position = "dodge", color = "black") + # Changed to position="dodge" for dodgeed bars
    labs(x = "",
         y = "") +
    scale_y_continuous(labels = scales::comma) + # Format y-axis labels as comma-separated numbers
    theme_minimal() +
    theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
          legend.position = "bottom", # Position legend at the bottom
          plot.title = element_text(hjust = 0.5)) # Center the plot title
  
  # Convert ggplot to plotly, ensuring custom text is used for hover
  ggplotly(p, tooltip = "text", source = "cloudenrolmentRegionPlot") %>%
    layout(hoverlabel = list(bgcolor = "white"),
           margin = list(b = 100)) # Increase bottom margin for x-axis labels
})

output$nat_regional_profile_graph <- renderPlotly({
  # Use the reactive filtered data
  
  data_to_display <- cloud_v2 %>% filter(BASIC.REGION == input$cloud_region_profile_filter) %>%
    select(BASIC.REGION, contains("NAT.K12.CURRICULUM.ALIGN"), -"NAT.K12.CURRICULUM.ALIGN_") %>% 
    mutate(across(2:last_col(), as.numeric)) %>% 
    mutate(BASIC.REGION = as.character(BASIC.REGION)) %>%
    filter(BASIC.REGION != "") %>% 
    group_by(BASIC.REGION) %>%
    summarise(across(everything(), sum, na.rm = TRUE))  %>% 
    rename(
      "Moderately Aligned" = NAT.K12.CURRICULUM.ALIGN_Moderately_aligned ,
      "Not at all aligned" = NAT.K12.CURRICULUM.ALIGN_Not_at_all_aligned,
      "Slightly Aligned" = NAT.K12.CURRICULUM.ALIGN_Slightly_aligned,
      "Very well Aligned" = NAT.K12.CURRICULUM.ALIGN_Very_well_aligned,
      "Well Aligned" = NAT.K12.CURRICULUM.ALIGN_Well_aligned
    ) %>% 
    pivot_longer(
      cols = 2:last_col(), # Specifies the columns to pivot
      names_to = "NAT_alignment_to_Curriculum", # The new column to hold the original column names
      values_to = "Count" # The new column to hold the values
    )
  
  current_filtered_data <- data_to_display
  
  # --- Empty Data Handling ---
  if (nrow(current_filtered_data) == 0) {
    return(ggplotly(ggplot() +
                      annotate("text", x = 0.5, y = 0.5, label = "No data for selected regions/divisions") +
                      theme_void()))
  }
  
  # Prepare the data for plotting
  plot_data <- data_to_display
  
  # Create the ggplot
  p <- ggplot(plot_data,
              aes(x = BASIC.REGION, # Reorder regions based on overall total count for the region
                  y = Count,
                  fill = NAT_alignment_to_Curriculum, # Fill by Clustering.Status for coloring and consistent order
                  text = paste("Region: ", BASIC.REGION,
                               "<br>Section: ", NAT_alignment_to_Curriculum,
                               "<br>Count: ", scales::comma(Count)))) + # Custom tooltip text
    geom_bar(stat = "identity", position = "dodge", color = "black") + # Changed to position="dodge" for dodgeed bars
    labs(x = "",
         y = "") +
    scale_y_continuous(labels = scales::comma) + # Format y-axis labels as comma-separated numbers
    theme_minimal() +
    theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
          legend.position = "bottom", # Position legend at the bottom
          plot.title = element_text(hjust = 0.5)) # Center the plot title
  
  # Convert ggplot to plotly, ensuring custom text is used for hover
  ggplotly(p, tooltip = "text", source = "cloudenrolmentRegionPlot") %>%
    layout(hoverlabel = list(bgcolor = "white"),
           margin = list(b = 100)) # Increase bottom margin for x-axis labels
})


output$nat_sufficiency_regional_profile_graph <- renderPlotly({
  # Use the reactive filtered data
  
  data_to_display <- cloud_v3 %>% filter(BASIC.REGION == input$cloud_region_profile_filter) %>%
    select(BASIC.REGION, COMPRE_YES_NAT,YES_NAT,NOT_ADEQ_NAT,NO_NAT) %>% 
    mutate(across(2:last_col(), as.numeric)) %>% 
    mutate(BASIC.REGION = as.character(BASIC.REGION)) %>%
    filter(BASIC.REGION != "") %>% 
    group_by(BASIC.REGION) %>%
    summarise(across(everything(), sum, na.rm = TRUE))  %>% 
    rename(
      ""
    )
    pivot_longer(
      cols = 2:last_col(), # Specifies the columns to pivot
      names_to = "Sections", # The new column to hold the original column names
      values_to = "Count" # The new column to hold the values
    )
  
  current_filtered_data <- data_to_display
  
  # --- Empty Data Handling ---
  if (nrow(current_filtered_data) == 0) {
    return(ggplotly(ggplot() +
                      annotate("text", x = 0.5, y = 0.5, label = "No data for selected regions/divisions") +
                      theme_void()))
  }
  
  # Prepare the data for plotting
  plot_data <- data_to_display
  
  # Create the ggplot
  p <- ggplot(plot_data,
              aes(x = BASIC.REGION, # Reorder regions based on overall total count for the region
                  y = Count,
                  fill = Sections, # Fill by Clustering.Status for coloring and consistent order
                  text = paste("Region: ", BASIC.REGION,
                               "<br>Section: ", Sections,
                               "<br>Count: ", scales::comma(Count)))) + # Custom tooltip text
    geom_bar(stat = "identity", position = "dodge", color = "black") + # Changed to position="dodge" for dodgeed bars
    labs(x = "",
         y = "") +
    scale_y_continuous(labels = scales::comma) + # Format y-axis labels as comma-separated numbers
    theme_minimal() +
    theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
          legend.position = "bottom", # Position legend at the bottom
          plot.title = element_text(hjust = 0.5)) # Center the plot title
  
  # Convert ggplot to plotly, ensuring custom text is used for hover
  ggplotly(p, tooltip = "text", source = "cloudenrolmentRegionPlot") %>%
    layout(hoverlabel = list(bgcolor = "white"),
           margin = list(b = 100)) # Increase bottom margin for x-axis labels
})

output$elec_regional_profile_graph <- renderPlotly({
  # Use the reactive filtered data
  
  data_to_display <- cloud_v3 %>% filter(BASIC.REGION == input$cloud_region_profile_filter) %>%
    select(BASIC.REGION,GRID_ELEC,OFFGRID_ELEC,COMBINED_ELEC,NO_ELEC_) %>% 
    mutate(across(2:last_col(), as.numeric)) %>% 
    mutate(BASIC.REGION = as.character(BASIC.REGION)) %>%
    filter(BASIC.REGION != "") %>% 
    group_by(BASIC.REGION) %>%
    summarise(across(everything(), sum, na.rm = TRUE))  %>% 
    rename(
      "GRID" = GRID_ELEC,
      "OFF-GRID" = OFFGRID_ELEC,
      "COMBINED" = COMBINED_ELEC,
      "NO ELECTRICITY" = NO_ELEC_
    ) %>% 
    pivot_longer(
      cols = 2:last_col(), # Specifies the columns to pivot
      names_to = "Electricity_Source", # The new column to hold the original column names
      values_to = "Count" # The new column to hold the values
    )
  
  current_filtered_data <- data_to_display
  
  # --- Empty Data Handling ---
  if (nrow(current_filtered_data) == 0) {
    return(ggplotly(ggplot() +
                      annotate("text", x = 0.5, y = 0.5, label = "No data for selected regions/divisions") +
                      theme_void()))
  }
  
  # Prepare the data for plotting
  plot_data <- data_to_display
  
  # Create the ggplot
  p <- ggplot(plot_data,
              aes(x = BASIC.REGION, # Reorder regions based on overall total count for the region
                  y = Count,
                  fill = Electricity_Source, # Fill by Clustering.Status for coloring and consistent order
                  text = paste("Region: ", BASIC.REGION,
                               "<br>Section: ", Electricity_Source,
                               "<br>Count: ", scales::comma(Count)))) + # Custom tooltip text
    geom_bar(stat = "identity", position = "dodge", color = "black") + # Changed to position="dodge" for dodgeed bars
    labs(x = "",
         y = "") +
    scale_y_continuous(labels = scales::comma) + # Format y-axis labels as comma-separated numbers
    theme_minimal() +
    theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
          legend.position = "bottom", # Position legend at the bottom
          plot.title = element_text(hjust = 0.5)) # Center the plot title
  
  # Convert ggplot to plotly, ensuring custom text is used for hover
  ggplotly(p, tooltip = "text", source = "cloudenrolmentRegionPlot") %>%
    layout(hoverlabel = list(bgcolor = "white"),
           margin = list(b = 100)) # Increase bottom margin for x-axis labels
})

output$internet_regional_profile_graph <- renderPlotly({
  # Use the reactive filtered data
  
  data_to_display <- cloud_v3 %>% filter(BASIC.REGION == input$cloud_region_profile_filter) %>%
    select(BASIC.REGION,Broadband_MOOE,WiFi_Plan_MOOE,Mobile_Data,No_Internet_Available) %>% 
    mutate(across(2:last_col(), as.numeric)) %>% 
    mutate(BASIC.REGION = as.character(BASIC.REGION)) %>%
    filter(BASIC.REGION != "") %>% 
    group_by(BASIC.REGION) %>%
    summarise(across(everything(), sum, na.rm = TRUE))  %>% 
    rename(
      "Broadband" = Broadband_MOOE,
      "WiFi Plan" = WiFi_Plan_MOOE,
      "Mobile Data" = Mobile_Data,
      "No Internet Available" = No_Internet_Available 
    ) %>% 
    pivot_longer(
      cols = 2:last_col(), # Specifies the columns to pivot
      names_to = "Internet_Source", # The new column to hold the original column names
      values_to = "Count" # The new column to hold the values
    )
  
  current_filtered_data <- data_to_display
  
  # --- Empty Data Handling ---
  if (nrow(current_filtered_data) == 0) {
    return(ggplotly(ggplot() +
                      annotate("text", x = 0.5, y = 0.5, label = "No data for selected regions/divisions") +
                      theme_void()))
  }
  
  # Prepare the data for plotting
  plot_data <- data_to_display
  
  # Create the ggplot
  p <- ggplot(plot_data,
              aes(x = BASIC.REGION, # Reorder regions based on overall total count for the region
                  y = Count,
                  fill = Internet_Source, # Fill by Clustering.Status for coloring and consistent order
                  text = paste("Region: ", BASIC.REGION,
                               "<br>Section: ", Internet_Source,
                               "<br>Count: ", scales::comma(Count)))) + # Custom tooltip text
    geom_bar(stat = "identity", position = "dodge", color = "black") + # Changed to position="dodge" for dodgeed bars
    labs(x = "",
         y = "") +
    scale_y_continuous(labels = scales::comma) + # Format y-axis labels as comma-separated numbers
    theme_minimal() +
    theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
          legend.position = "bottom", # Position legend at the bottom
          plot.title = element_text(hjust = 0.5)) # Center the plot title
  
  # Convert ggplot to plotly, ensuring custom text is used for hover
  ggplotly(p, tooltip = "text", source = "cloudenrolmentRegionPlot") %>%
    layout(hoverlabel = list(bgcolor = "white"),
           margin = list(b = 100)) # Increase bottom margin for x-axis labels
})


output$internet_usage_regional_profile_graph <- renderPlotly({
  # Use the reactive filtered data
  
  data_to_display <- cloud_v3 %>% filter(BASIC.REGION == input$cloud_region_profile_filter) %>%
    select(BASIC.REGION,Combination_Internet,Instructional_Internet,Administrative_Internet,None_Internet) %>% 
    mutate(across(2:last_col(), as.numeric)) %>% 
    mutate(BASIC.REGION = as.character(BASIC.REGION)) %>%
    filter(BASIC.REGION != "") %>% 
    group_by(BASIC.REGION) %>%
    summarise(across(everything(), sum, na.rm = TRUE))  %>% 
    rename(
      "Instructional" = Instructional_Internet,
      "Administrative" = Administrative_Internet,
      "Combination" = Combination_Internet,
      "None" = None_Internet
    ) %>% 
    pivot_longer(
      cols = 2:last_col(), # Specifies the columns to pivot
      names_to = "Internet_Usage", # The new column to hold the original column names
      values_to = "Count" # The new column to hold the values
    )
  
  current_filtered_data <- data_to_display
  
  # --- Empty Data Handling ---
  if (nrow(current_filtered_data) == 0) {
    return(ggplotly(ggplot() +
                      annotate("text", x = 0.5, y = 0.5, label = "No data for selected regions/divisions") +
                      theme_void()))
  }
  
  # Prepare the data for plotting
  plot_data <- data_to_display
  
  # Create the ggplot
  p <- ggplot(plot_data,
              aes(x = BASIC.REGION, # Reorder regions based on overall total count for the region
                  y = Count,
                  fill = Internet_Usage, # Fill by Clustering.Status for coloring and consistent order
                  text = paste("Region: ", BASIC.REGION,
                               "<br>Section: ", Internet_Usage,
                               "<br>Count: ", scales::comma(Count)))) + # Custom tooltip text
    geom_bar(stat = "identity", position = "dodge", color = "black") + # Changed to position="dodge" for dodgeed bars
    labs(x = "",
         y = "") +
    scale_y_continuous(labels = scales::comma) + # Format y-axis labels as comma-separated numbers
    theme_minimal() +
    theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
          legend.position = "bottom", # Position legend at the bottom
          plot.title = element_text(hjust = 0.5)) # Center the plot title
  
  # Convert ggplot to plotly, ensuring custom text is used for hover
  ggplotly(p, tooltip = "text", source = "cloudenrolmentRegionPlot") %>%
    layout(hoverlabel = list(bgcolor = "white"),
           margin = list(b = 100)) # Increase bottom margin for x-axis labels
})


output$water_regional_profile_graph <- renderPlotly({
  # Use the reactive filtered data
  
  data_to_display <- cloud_v3 %>% filter(BASIC.REGION == input$cloud_region_profile_filter) %>%
    select(BASIC.REGION,GROUNDWATER_WATER,BOTTLED_WATER,MWSS_WATER,WELL_WATER,LOCAL_WATER,SURFACE_WATER,PIPEDL3_WATER) %>% 
    mutate(across(2:last_col(), as.numeric)) %>% 
    mutate(BASIC.REGION = as.character(BASIC.REGION)) %>%
    filter(BASIC.REGION != "") %>% 
    group_by(BASIC.REGION) %>%
    summarise(across(everything(), sum, na.rm = TRUE))  %>% 
    rename(
      "Bottled Water" = BOTTLED_WATER,
      "Groundwater" = GROUNDWATER_WATER,
      "Local Water" = LOCAL_WATER,
      "MWSS" = MWSS_WATER,
      "Piped Water" = PIPEDL3_WATER,
      "Surface Water" = SURFACE_WATER,
      "Well Water" = WELL_WATER
    ) %>% 
    pivot_longer(
      cols = 2:last_col(), # Specifies the columns to pivot
      names_to = "Water_Source", # The new column to hold the original column names
      values_to = "Count" # The new column to hold the values
    )
  
  current_filtered_data <- data_to_display
  
  # --- Empty Data Handling ---
  if (nrow(current_filtered_data) == 0) {
    return(ggplotly(ggplot() +
                      annotate("text", x = 0.5, y = 0.5, label = "No data for selected regions/divisions") +
                      theme_void()))
  }
  
  # Prepare the data for plotting
  plot_data <- data_to_display
  
  # Create the ggplot
  p <- ggplot(plot_data,
              aes(x = BASIC.REGION, # Reorder regions based on overall total count for the region
                  y = Count,
                  fill = Water_Source, # Fill by Clustering.Status for coloring and consistent order
                  text = paste("Region: ", BASIC.REGION,
                               "<br>Section: ", Water_Source,
                               "<br>Count: ", scales::comma(Count)))) + # Custom tooltip text
    geom_bar(stat = "identity", position = "dodge", color = "black") + # Changed to position="dodge" for dodgeed bars
    labs(x = "",
         y = "") +
    scale_y_continuous(labels = scales::comma) + # Format y-axis labels as comma-separated numbers
    theme_minimal() +
    theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
          legend.position = "bottom", # Position legend at the bottom
          plot.title = element_text(hjust = 0.5)) # Center the plot title
  
  # Convert ggplot to plotly, ensuring custom text is used for hover
  ggplotly(p, tooltip = "text", source = "cloudenrolmentRegionPlot") %>%
    layout(hoverlabel = list(bgcolor = "white"),
           margin = list(b = 100)) # Increase bottom margin for x-axis labels
})

output$adm_regional_profile_graph <- renderPlotly({
  # Use the reactive filtered data
  
  data_to_display <- cloud_v2 %>% filter(BASIC.REGION == input$cloud_region_profile_filter) %>%
    select(BASIC.REGION, contains("ALTERNATIVE.ADM")) %>% 
    mutate(across(2:last_col(), as.numeric)) %>% 
    mutate(BASIC.REGION = as.character(BASIC.REGION)) %>%
    filter(BASIC.REGION != "") %>% 
    group_by(BASIC.REGION) %>%
    summarise(across(everything(), sum, na.rm = TRUE))  %>% 
    pivot_longer(
      cols = 2:last_col(), # Specifies the columns to pivot
      names_to = "Sections", # The new column to hold the original column names
      values_to = "Count" # The new column to hold the values
    )
  
  current_filtered_data <- data_to_display
  
  # --- Empty Data Handling ---
  if (nrow(current_filtered_data) == 0) {
    return(ggplotly(ggplot() +
                      annotate("text", x = 0.5, y = 0.5, label = "No data for selected regions/divisions") +
                      theme_void()))
  }
  
  # Prepare the data for plotting
  plot_data <- data_to_display
  
  # Create the ggplot
  p <- ggplot(plot_data,
              aes(x = BASIC.REGION, # Reorder regions based on overall total count for the region
                  y = Count,
                  fill = Sections, # Fill by Clustering.Status for coloring and consistent order
                  text = paste("Region: ", BASIC.REGION,
                               "<br>Section: ", Sections,
                               "<br>Count: ", scales::comma(Count)))) + # Custom tooltip text
    geom_bar(stat = "identity", position = "dodge", color = "black") + # Changed to position="dodge" for dodgeed bars
    labs(x = "",
         y = "") +
    scale_y_continuous(labels = scales::comma) + # Format y-axis labels as comma-separated numbers
    theme_minimal() +
    theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
          legend.position = "bottom", # Position legend at the bottom
          plot.title = element_text(hjust = 0.5)) # Center the plot title
  
  # Convert ggplot to plotly, ensuring custom text is used for hover
  ggplotly(p, tooltip = "text", source = "cloudenrolmentRegionPlot") %>%
    layout(hoverlabel = list(bgcolor = "white"),
           margin = list(b = 100)) # Increase bottom margin for x-axis labels
})

output$adm_regional_profile_graph <- renderPlotly({
  # Use the reactive filtered data
  
  data_to_display <- cloud_v2 %>% filter(BASIC.REGION == input$cloud_region_profile_filter) %>%
    select(BASIC.REGION, contains("ALTERNATIVE.ADM")) %>% 
    mutate(across(2:last_col(), as.numeric)) %>% 
    mutate(BASIC.REGION = as.character(BASIC.REGION)) %>%
    filter(BASIC.REGION != "") %>% 
    group_by(BASIC.REGION) %>%
    summarise(across(everything(), sum, na.rm = TRUE))  %>% 
    pivot_longer(
      cols = 2:last_col(), # Specifies the columns to pivot
      names_to = "Sections", # The new column to hold the original column names
      values_to = "Count" # The new column to hold the values
    )
  
  current_filtered_data <- data_to_display
  
  # --- Empty Data Handling ---
  if (nrow(current_filtered_data) == 0) {
    return(ggplotly(ggplot() +
                      annotate("text", x = 0.5, y = 0.5, label = "No data for selected regions/divisions") +
                      theme_void()))
  }
  
  # Prepare the data for plotting
  plot_data <- data_to_display
  
  # Create the ggplot
  p <- ggplot(plot_data,
              aes(x = BASIC.REGION, # Reorder regions based on overall total count for the region
                  y = Count,
                  fill = Sections, # Fill by Clustering.Status for coloring and consistent order
                  text = paste("Region: ", BASIC.REGION,
                               "<br>Section: ", Sections,
                               "<br>Count: ", scales::comma(Count)))) + # Custom tooltip text
    geom_bar(stat = "identity", position = "dodge", color = "black") + # Changed to position="dodge" for dodgeed bars
    labs(x = "",
         y = "") +
    scale_y_continuous(labels = scales::comma) + # Format y-axis labels as comma-separated numbers
    theme_minimal() +
    theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
          legend.position = "bottom", # Position legend at the bottom
          plot.title = element_text(hjust = 0.5)) # Center the plot title
  
  # Convert ggplot to plotly, ensuring custom text is used for hover
  ggplotly(p, tooltip = "text", source = "cloudenrolmentRegionPlot") %>%
    layout(hoverlabel = list(bgcolor = "white"),
           margin = list(b = 100)) # Increase bottom margin for x-axis labels
})

output$rf_regional_profile_graph <- renderPlotly({
  # Use the reactive filtered data
  
  data_to_display <- cloud_v2 %>% filter(BASIC.REGION == input$cloud_region_profile_filter) %>%
    select(BASIC.REGION, contains("QUALI.READING.PROFICIENCY.CHANGE")) %>% 
    mutate(across(2:last_col(), as.numeric)) %>% 
    mutate(BASIC.REGION = as.character(BASIC.REGION)) %>%
    filter(BASIC.REGION != "") %>% 
    group_by(BASIC.REGION) %>%
    summarise(across(everything(), sum, na.rm = TRUE))  %>% 
    rename(
      "Declined" = QUALI.READING.PROFICIENCY.CHANGE_Declined,
      "Improved" = QUALI.READING.PROFICIENCY.CHANGE_Improved,
      "Stagnant" = QUALI.READING.PROFICIENCY.CHANGE_Stagnant
    ) %>% 
    pivot_longer(
      cols = 2:last_col(), # Specifies the columns to pivot
      names_to = "Reading_Proficiency", # The new column to hold the original column names
      values_to = "Count" # The new column to hold the values
    )
  
  current_filtered_data <- data_to_display
  
  # --- Empty Data Handling ---
  if (nrow(current_filtered_data) == 0) {
    return(ggplotly(ggplot() +
                      annotate("text", x = 0.5, y = 0.5, label = "No data for selected regions/divisions") +
                      theme_void()))
  }
  
  # Prepare the data for plotting
  plot_data <- data_to_display
  
  # Create the ggplot
  p <- ggplot(plot_data,
              aes(x = BASIC.REGION, # Reorder regions based on overall total count for the region
                  y = Count,
                  fill = Reading_Proficiency, # Fill by Clustering.Status for coloring and consistent order
                  text = paste("Region: ", BASIC.REGION,
                               "<br>Section: ", Reading_Proficiency,
                               "<br>Count: ", scales::comma(Count)))) + # Custom tooltip text
    geom_bar(stat = "identity", position = "dodge", color = "black") + # Changed to position="dodge" for dodgeed bars
    labs(x = "",
         y = "") +
    scale_y_continuous(labels = scales::comma) + # Format y-axis labels as comma-separated numbers
    theme_minimal() +
    theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
          legend.position = "bottom", # Position legend at the bottom
          plot.title = element_text(hjust = 0.5)) # Center the plot title
  
  # Convert ggplot to plotly, ensuring custom text is used for hover
  ggplotly(p, tooltip = "text", source = "cloudenrolmentRegionPlot") %>%
    layout(hoverlabel = list(bgcolor = "white"),
           margin = list(b = 100)) # Increase bottom margin for x-axis labels
})

output$aral_regional_profile_graph <- renderPlotly({
  # Use the reactive filtered data
  
  data_to_display <- cloud %>% filter(BASIC.REGION == input$cloud_region_profile_filter) %>%
    select(BASIC.REGION,ARAL.MATH,ARAL.READING,ARAL.SCIENCE) %>% 
    mutate(across(2:last_col(), as.numeric)) %>% 
    mutate(BASIC.REGION = as.character(BASIC.REGION)) %>%
    filter(BASIC.REGION != "") %>% 
    group_by(BASIC.REGION) %>%
    summarise(across(everything(), sum, na.rm = TRUE)) %>% 
    rename(
      "REGION" = BASIC.REGION,
      "MATH" = ARAL.MATH,
      "READING" = ARAL.READING,
      "SCIENCE" = ARAL.SCIENCE)  %>% 
    pivot_longer(
      cols = 2:last_col(), # Specifies the columns to pivot
      names_to = "Prospective_ARAL_Learners", # The new column to hold the original column names
      values_to = "Count" # The new column to hold the values
    )
  
  current_filtered_data <- data_to_display
  
  # --- Empty Data Handling ---
  if (nrow(current_filtered_data) == 0) {
    return(ggplotly(ggplot() +
                      annotate("text", x = 0.5, y = 0.5, label = "No data for selected regions/divisions") +
                      theme_void()))
  }
  
  # Prepare the data for plotting
  plot_data <- data_to_display
  
  # Create the ggplot
  p <- ggplot(plot_data,
              aes(x = REGION, # Reorder regions based on overall total count for the region
                  y = Count,
                  fill = Prospective_ARAL_Learners, # Fill by Clustering.Status for coloring and consistent order
                  text = paste("Region: ", REGION,
                               "<br>Section: ", Prospective_ARAL_Learners,
                               "<br>Count: ", scales::comma(Count)))) + # Custom tooltip text
    geom_bar(stat = "identity", position = "dodge", color = "black") + # Changed to position="dodge" for dodgeed bars
    labs(x = "",
         y = "") +
    scale_y_continuous(labels = scales::comma) + # Format y-axis labels as comma-separated numbers
    theme_minimal() +
    theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
          legend.position = "bottom", # Position legend at the bottom
          plot.title = element_text(hjust = 0.5)) # Center the plot title
  
  # Convert ggplot to plotly, ensuring custom text is used for hover
  ggplotly(p, tooltip = "text", source = "cloudenrolmentRegionPlot") %>%
    layout(hoverlabel = list(bgcolor = "white"),
           margin = list(b = 100)) # Increase bottom margin for x-axis labels
})

output$bully_regional_profile_graph <- renderPlotly({
  # Use the reactive filtered data
  
  data_to_display <- cloud %>% filter(BASIC.REGION == input$cloud_region_profile_filter) %>%
    select(BASIC.REGION,MENTAL.BULLYING.COUNT,MENTAL.CHILDREN.RIGHT.ORIENTATION) %>% 
    mutate(across(2:last_col(), as.numeric)) %>% 
    mutate(BASIC.REGION = as.character(BASIC.REGION)) %>%
    filter(BASIC.REGION != "") %>% 
    group_by(BASIC.REGION) %>%
    summarise(across(everything(), sum, na.rm = TRUE)) %>% 
    rename(
      "REGION" = BASIC.REGION,
      "INCIDENCE OF BULLYING" = MENTAL.BULLYING.COUNT,
      "ATTENDANCE TO CHILDRENS' RIGHTS ORIENTATION" = MENTAL.CHILDREN.RIGHT.ORIENTATION)  %>% 
    pivot_longer(
      cols = 2:last_col(), # Specifies the columns to pivot
      names_to = "Sections", # The new column to hold the original column names
      values_to = "Count" # The new column to hold the values
    )
  
  current_filtered_data <- data_to_display
  
  # --- Empty Data Handling ---
  if (nrow(current_filtered_data) == 0) {
    return(ggplotly(ggplot() +
                      annotate("text", x = 0.5, y = 0.5, label = "No data for selected regions/divisions") +
                      theme_void()))
  }
  
  # Prepare the data for plotting
  plot_data <- data_to_display
  
  # Create the ggplot
  p <- ggplot(plot_data,
              aes(x = REGION, # Reorder regions based on overall total count for the region
                  y = Count,
                  fill = Sections, # Fill by Clustering.Status for coloring and consistent order
                  text = paste("Region: ", REGION,
                               "<br>Section: ", Sections,
                               "<br>Count: ", scales::comma(Count)))) + # Custom tooltip text
    geom_bar(stat = "identity", position = "dodge", color = "black") + # Changed to position="dodge" for dodgeed bars
    labs(x = "",
         y = "") +
    scale_y_continuous(labels = scales::comma) + # Format y-axis labels as comma-separated numbers
    theme_minimal() +
    theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
          legend.position = "bottom", # Position legend at the bottom
          plot.title = element_text(hjust = 0.5)) # Center the plot title
  
  # Convert ggplot to plotly, ensuring custom text is used for hover
  ggplotly(p, tooltip = "text", source = "cloudenrolmentRegionPlot") %>%
    layout(hoverlabel = list(bgcolor = "white"),
           margin = list(b = 100)) # Increase bottom margin for x-axis labels
})

output$crla_regional_profile_graph <- renderPlotly({
  # Use the reactive filtered data
  
  data_to_display <- cloud %>% filter(BASIC.REGION == input$cloud_region_profile_filter) %>%
    select(BASIC.REGION, CRLA.DEVELOPING, CRLA.LOW, CRLA.TRANSITIONAL, CRLA.HIGH, CRLA.GRADELEVEL) %>% 
    mutate(across(2:last_col(), as.numeric)) %>% 
    mutate(BASIC.REGION = as.character(BASIC.REGION)) %>%
    filter(BASIC.REGION != "") %>% 
    group_by(BASIC.REGION) %>%
    summarise(across(everything(), sum, na.rm = TRUE)) %>% 
    rename(
      "REGION" = BASIC.REGION,
      "LOW" = CRLA.LOW,
      "DEVELOPING" = CRLA.DEVELOPING,
      "TRANSITIONAL" = CRLA.TRANSITIONAL,
      "HIGH" = CRLA.HIGH,
      "GRADE LEVEL" = CRLA.GRADELEVEL)  %>% 
    pivot_longer(
      cols = 2:last_col(), # Specifies the columns to pivot
      names_to = "CRLA_Levels", # The new column to hold the original column names
      values_to = "Count" # The new column to hold the values
    )
  
  current_filtered_data <- data_to_display
  
  # --- Empty Data Handling ---
  if (nrow(current_filtered_data) == 0) {
    return(ggplotly(ggplot() +
                      annotate("text", x = 0.5, y = 0.5, label = "No data for selected regions/divisions") +
                      theme_void()))
  }
  
  # Prepare the data for plotting
  plot_data <- data_to_display
  
  # Create the ggplot
  p <- ggplot(plot_data,
              aes(x = REGION, # Reorder regions based on overall total count for the region
                  y = Count,
                  fill = CRLA_Levels, # Fill by Clustering.Status for coloring and consistent order
                  text = paste("Region: ", REGION,
                               "<br>Section: ", CRLA_Levels,
                               "<br>Count: ", scales::comma(Count)))) + # Custom tooltip text
    geom_bar(stat = "identity", position = "dodge", color = "black") + # Changed to position="dodge" for dodgeed bars
    labs(x = "",
         y = "") +
    scale_y_continuous(labels = scales::comma) + # Format y-axis labels as comma-separated numbers
    theme_minimal() +
    theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
          legend.position = "bottom", # Position legend at the bottom
          plot.title = element_text(hjust = 0.5)) # Center the plot title
  
  # Convert ggplot to plotly, ensuring custom text is used for hover
  ggplotly(p, tooltip = "text", source = "cloudenrolmentRegionPlot") %>%
    layout(hoverlabel = list(bgcolor = "white"),
           margin = list(b = 100)) # Increase bottom margin for x-axis labels
})

output$philiri_regional_profile_graph <- renderPlotly({
  # Use the reactive filtered data
  
  data_to_display <- cloud %>% filter(BASIC.REGION == input$cloud_region_profile_filter) %>%
    select(BASIC.REGION, PHILIRI.FRUSTRATION, PHILIRI.TRANSITIONAL, PHILIRI.INSTRUCTIONAL) %>% 
    mutate(across(2:last_col(), as.numeric)) %>% 
    mutate(BASIC.REGION = as.character(BASIC.REGION)) %>%
    filter(BASIC.REGION != "") %>% 
    group_by(BASIC.REGION) %>%
    summarise(across(everything(), sum, na.rm = TRUE)) %>% 
    rename(
      "REGION" = BASIC.REGION,
      "FRUSTRATION" = PHILIRI.FRUSTRATION,
      "TRANSITIONAL" = PHILIRI.TRANSITIONAL,
      "INSTRUCTIONAL" = PHILIRI.INSTRUCTIONAL)  %>% 
    pivot_longer(
      cols = 2:last_col(), # Specifies the columns to pivot
      names_to = "PhilIRI_Levels", # The new column to hold the original column names
      values_to = "Count" # The new column to hold the values
    )
  
  current_filtered_data <- data_to_display
  
  # --- Empty Data Handling ---
  if (nrow(current_filtered_data) == 0) {
    return(ggplotly(ggplot() +
                      annotate("text", x = 0.5, y = 0.5, label = "No data for selected regions/divisions") +
                      theme_void()))
  }
  
  # Prepare the data for plotting
  plot_data <- data_to_display
  
  # Create the ggplot
  p <- ggplot(plot_data,
              aes(x = REGION, # Reorder regions based on overall total count for the region
                  y = Count,
                  fill = PhilIRI_Levels, # Fill by Clustering.Status for coloring and consistent order
                  text = paste("Region: ", REGION,
                               "<br>Section: ", PhilIRI_Levels,
                               "<br>Count: ", scales::comma(Count)))) + # Custom tooltip text
    geom_bar(stat = "identity", position = "dodge", color = "black") + # Changed to position="dodge" for dodgeed bars
    labs(x = "",
         y = "") +
    scale_y_continuous(labels = scales::comma) + # Format y-axis labels as comma-separated numbers
    theme_minimal() +
    theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
          legend.position = "bottom", # Position legend at the bottom
          plot.title = element_text(hjust = 0.5)) # Center the plot title
  
  # Convert ggplot to plotly, ensuring custom text is used for hover
  ggplotly(p, tooltip = "text", source = "cloudenrolmentRegionPlot") %>%
    layout(hoverlabel = list(bgcolor = "white"),
           margin = list(b = 100)) # Increase bottom margin for x-axis labels
})

output$sha_regional_profile_graph <- renderPlotly({
  # Use the reactive filtered data
  
  data_to_display <- cloud_v2 %>% filter(BASIC.REGION == input$cloud_region_profile_filter) %>%
    select(BASIC.REGION, contains("SHA.CATEGORY")) %>% 
    mutate(across(2:last_col(), as.numeric)) %>% 
    mutate(BASIC.REGION = as.character(BASIC.REGION)) %>%
    filter(BASIC.REGION != "") %>% 
    group_by(BASIC.REGION) %>%
    summarise(across(everything(), sum, na.rm = TRUE))  %>% 
    pivot_longer(
      cols = 2:last_col(), # Specifies the columns to pivot
      names_to = "Sections", # The new column to hold the original column names
      values_to = "Count" # The new column to hold the values
    )
  
  current_filtered_data <- data_to_display
  
  # --- Empty Data Handling ---
  if (nrow(current_filtered_data) == 0) {
    return(ggplotly(ggplot() +
                      annotate("text", x = 0.5, y = 0.5, label = "No data for selected regions/divisions") +
                      theme_void()))
  }
  
  # Prepare the data for plotting
  plot_data <- data_to_display
  
  # Create the ggplot
  p <- ggplot(plot_data,
              aes(x = BASIC.REGION, # Reorder regions based on overall total count for the region
                  y = Count,
                  fill = Sections, # Fill by Clustering.Status for coloring and consistent order
                  text = paste("Region: ", BASIC.REGION,
                               "<br>Section: ", Sections,
                               "<br>Count: ", scales::comma(Count)))) + # Custom tooltip text
    geom_bar(stat = "identity", position = "dodge", color = "black") + # Changed to position="dodge" for dodgeed bars
    labs(x = "",
         y = "") +
    scale_y_continuous(labels = scales::comma) + # Format y-axis labels as comma-separated numbers
    theme_minimal() +
    theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
          legend.position = "bottom", # Position legend at the bottom
          plot.title = element_text(hjust = 0.5)) # Center the plot title
  
  # Convert ggplot to plotly, ensuring custom text is used for hover
  ggplotly(p, tooltip = "text", source = "cloudenrolmentRegionPlot") %>%
    layout(hoverlabel = list(bgcolor = "white"),
           margin = list(b = 100)) # Increase bottom margin for x-axis labels
})

output$feeding_regional_profile_graph <- renderPlotly({
  # Use the reactive filtered data
  
  data_to_display <- cloud %>% filter(BASIC.REGION == input$cloud_region_profile_filter) %>%
    select(BASIC.REGION, contains("SBFP.")) %>% 
    mutate(across(2:last_col(), as.numeric)) %>% 
    mutate(BASIC.REGION = as.character(BASIC.REGION)) %>%
    filter(BASIC.REGION != "") %>% 
    group_by(BASIC.REGION) %>%
    summarise(across(everything(), sum, na.rm = TRUE))  %>% 
    rename(
      "DepEd Beneficiary" = SBFP.BENEFICIARY,
      "Non-DepEd Beneficiary" = SBFP.NO.OF.BENEFICIARIES.NOT.SBFP
    ) %>% 
    pivot_longer(
      cols = 2:last_col(), # Specifies the columns to pivot
      names_to = "Feeding_Program_Beneficiaries", # The new column to hold the original column names
      values_to = "Count" # The new column to hold the values
    )
  
  current_filtered_data <- data_to_display
  
  # --- Empty Data Handling ---
  if (nrow(current_filtered_data) == 0) {
    return(ggplotly(ggplot() +
                      annotate("text", x = 0.5, y = 0.5, label = "No data for selected regions/divisions") +
                      theme_void()))
  }
  
  # Prepare the data for plotting
  plot_data <- data_to_display
  
  # Create the ggplot
  p <- ggplot(plot_data,
              aes(x = BASIC.REGION, # Reorder regions based on overall total count for the region
                  y = Count,
                  fill = Feeding_Program_Beneficiaries, # Fill by Clustering.Status for coloring and consistent order
                  text = paste("Region: ", BASIC.REGION,
                               "<br>Section: ", Feeding_Program_Beneficiaries,
                               "<br>Count: ", scales::comma(Count)))) + # Custom tooltip text
    geom_bar(stat = "identity", position = "dodge", color = "black") + # Changed to position="dodge" for dodgeed bars
    labs(x = "",
         y = "") +
    scale_y_continuous(labels = scales::comma) + # Format y-axis labels as comma-separated numbers
    theme_minimal() +
    theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
          legend.position = "bottom", # Position legend at the bottom
          plot.title = element_text(hjust = 0.5)) # Center the plot title
  
  # Convert ggplot to plotly, ensuring custom text is used for hover
  ggplotly(p, tooltip = "text", source = "cloudenrolmentRegionPlot") %>%
    layout(hoverlabel = list(bgcolor = "white"),
           margin = list(b = 100)) # Increase bottom margin for x-axis labels
})

output$years_regional_profile_graph <- renderPlotly({
  # Use the reactive filtered data
  
  data_to_display <- cloud %>% filter(BASIC.REGION == input$cloud_region_profile_filter) %>%
    select(BASIC.REGION, contains("SERVICE.")) %>% 
    mutate(across(2:last_col(), as.numeric)) %>% 
    mutate(BASIC.REGION = as.character(BASIC.REGION)) %>%
    filter(BASIC.REGION != "") %>% 
    group_by(BASIC.REGION) %>%
    summarise(across(everything(), sum, na.rm = TRUE))  %>%
    rename("Less than 1 year" = SERVICE.X0.TO.1,
           "2-5 years" = SERVICE.X2.TO.5,
           "6-10 years" = SERVICE.X6.TO.10,
           "11-15 years" = SERVICE.X11.TO.15,
           "16-20 years" = SERVICE.X16.TO.20,
           "21-25 years" = SERVICE.X21.TO.25,
           "26-30 years" = SERVICE.X26.TO.30,
           "31-35 years" = SERVICE.X31.TO.35,
           "36-40 years" = SERVICE.X36.TO.40,
           "41-45 years" = SERVICE.X41.TO.45) %>% 
    pivot_longer(
      cols = 2:last_col(), # Specifies the columns to pivot
      names_to = "Years_in_Service", # The new column to hold the original column names
      values_to = "Count" # The new column to hold the values
    )
  
  current_filtered_data <- data_to_display
  
  # --- Empty Data Handling ---
  if (nrow(current_filtered_data) == 0) {
    return(ggplotly(ggplot() +
                      annotate("text", x = 0.5, y = 0.5, label = "No data for selected regions/divisions") +
                      theme_void()))
  }
  
  # Prepare the data for plotting
  plot_data <- data_to_display
  
  # Create the ggplot
  p <- ggplot(plot_data,
              aes(x = BASIC.REGION, # Reorder regions based on overall total count for the region
                  y = Count,
                  fill = Years_in_Service, # Fill by Clustering.Status for coloring and consistent order
                  text = paste("Region: ", BASIC.REGION,
                               "<br>Section: ", Years_in_Service,
                               "<br>Count: ", scales::comma(Count)))) + # Custom tooltip text
    geom_bar(stat = "identity", position = "dodge", color = "black") + # Changed to position="dodge" for dodgeed bars
    labs(x = "",
         y = "") +
    scale_y_continuous(labels = scales::comma) + # Format y-axis labels as comma-separated numbers
    theme_minimal() +
    theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
          legend.position = "bottom", # Position legend at the bottom
          plot.title = element_text(hjust = 0.5)) # Center the plot title
  
  # Convert ggplot to plotly, ensuring custom text is used for hover
  ggplotly(p, tooltip = "text", source = "cloudenrolmentRegionPlot") %>%
    layout(hoverlabel = list(bgcolor = "white"),
           margin = list(b = 100)) # Increase bottom margin for x-axis labels
})

output$resources_regional_profile_graph <- renderPlotly({
  # Use the reactive filtered data
  
  data_to_display <- cloud_v2 %>% filter(BASIC.REGION == input$cloud_region_profile_filter) %>%
    select(BASIC.REGION, contains("RESOURCES.")) %>% 
    mutate(across(2:last_col(), as.numeric)) %>% 
    mutate(BASIC.REGION = as.character(BASIC.REGION)) %>%
    filter(BASIC.REGION != "") %>% 
    group_by(BASIC.REGION) %>%
    summarise(across(everything(), sum, na.rm = TRUE)) %>%
    mutate(RESOURCES.MATH = round((`RESOURCES.RECEIVED.SCIENCE.MATH.EQUIPMENT_Yes` / (`RESOURCES.RECEIVED.SCIENCE.MATH.EQUIPMENT_Yes` + `RESOURCES.RECEIVED.SCIENCE.MATH.EQUIPMENT_No`)) * 100, 2)) %>%
    mutate(RESOURCES.HANDWASHING = round((`RESOURCES.REGULAR.HANDWASHING_Yes` / (`RESOURCES.REGULAR.HANDWASHING_Yes` + `RESOURCES.REGULAR.HANDWASHING_No`)) * 100, 2)) %>%
    mutate(RESOURCES.SOAP = round((`RESOURCES.REGULAR.SOAP_Yes` / (`RESOURCES.REGULAR.SOAP_Yes` + `RESOURCES.REGULAR.SOAP_No`)) * 100, 2)) %>%
    mutate(RESOURCES.TVL = round((`RESOURCES.SCHOOL.WITH.TVL_Yes` / (`RESOURCES.SCHOOL.WITH.TVL_Yes` + `RESOURCES.SCHOOL.WITH.TVL_None`)) * 100, 2)) %>%
    # mutate(RESOURCES.CHAIR = round((`RESOURCES.CHAIR.FUNCTIONAL` / `RESOURCES.CHAIR.INVENTORY`) * 100, 2)) %>%
    # mutate(RESOURCES.DESK = round((`RESOURCES.DESK.FUNCTIONAL` / `RESOURCES.DESK.INVENTORY`) * 100, 2)) %>%
    # mutate(RESOURCES.ECART = round((`RESOURCES.ECART.FUNCTIONAL` / `RESOURCES.ECART.INVENTORY`) * 100, 2)) %>%
    # mutate(RESOURCES.LAPTOP = round((`RESOURCES.LAPTOP.FUNCTIONAL` / `RESOURCES.LAPTOP.INVENTORY`) * 100, 2)) %>%
    # mutate(RESOURCES.TOILET = round((`RESOURCES.FUNCTIONAL.TOILET` / `RESOURCES.NON.FUNCTIONAL.TOILET`) * 100, 2)) %>%
    # mutate(RESOURCES.TV = round((`RESOURCES.TV.FUNCTIONAL` / `RESOURCES.TV.INVENTORY`) * 100, 2))  
    select(1,"RESOURCES.MATH","RESOURCES.HANDWASHING","RESOURCES.SOAP","RESOURCES.TVL") %>% 
    group_by(BASIC.REGION) %>%
    summarise(across(everything(), mean, na.rm = TRUE)) %>% 
    rename("Handwashing" = RESOURCES.HANDWASHING,
           "Soap" = RESOURCES.SOAP,
           "Math" = RESOURCES.MATH,
           "TVL" = RESOURCES.TVL) %>% 
    pivot_longer(
      cols = 2:last_col(), # Specifies the columns to pivot
      names_to = "Resources_Data", # The new column to hold the original column names
      values_to = "Count" # The new column to hold the values
    ) 
  
  current_filtered_data <- data_to_display
  
  # --- Empty Data Handling ---
  if (nrow(current_filtered_data) == 0) {
    return(ggplotly(ggplot() +
                      annotate("text", x = 0.5, y = 0.5, label = "No data for selected regions/divisions") +
                      theme_void()))
  }
  
  # Prepare the data for plotting
  plot_data <- data_to_display
  
  # Create the ggplot
  p <- ggplot(plot_data,
              aes(x = BASIC.REGION, # Reorder regions based on overall total count for the region
                  y = Count,
                  fill = Resources_Data, # Fill by Clustering.Status for coloring and consistent order
                  text = paste("Region: ", BASIC.REGION,
                               "<br>Section: ", Resources_Data,
                               "<br>Count: ", scales::comma(Count)))) + # Custom tooltip text
    geom_bar(stat = "identity", position = "dodge", color = "black") + # Changed to position="dodge" for dodgeed bars
    labs(x = "",
         y = "") +
    scale_y_continuous(labels = scales::comma) + # Format y-axis labels as comma-separated numbers
    theme_minimal() +
    theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
          legend.position = "bottom", # Position legend at the bottom
          plot.title = element_text(hjust = 0.5)) # Center the plot title
  
  # Convert ggplot to plotly, ensuring custom text is used for hover
  ggplotly(p, tooltip = "text", source = "cloudenrolmentRegionPlot") %>%
    layout(hoverlabel = list(bgcolor = "white"),
           margin = list(b = 100)) # Increase bottom margin for x-axis labels
})

output$overload_regional_profile_graph <- renderPlotly({
  # Use the reactive filtered data
  
  data_to_display <- cloud %>% filter(BASIC.REGION == input$cloud_region_profile_filter) %>%
    select(BASIC.REGION, contains("DO5.OVERLOAD")) %>% 
    mutate(across(2:last_col(), as.numeric)) %>% 
    mutate(BASIC.REGION = as.character(BASIC.REGION)) %>%
    filter(BASIC.REGION != "") %>% 
    group_by(BASIC.REGION) %>%
    summarise(across(everything(), sum, na.rm = TRUE)) %>% 
    rename(
      "REGION" = BASIC.REGION,
      "OVERLOAD PAY RECIPIENTS" = DO5.OVERLOAD.PAY.RECIPIENT)  %>% 
    pivot_longer(
      cols = 2:last_col(), # Specifies the columns to pivot
      names_to = "Sections", # The new column to hold the original column names
      values_to = "Count" # The new column to hold the values
    )
  
  current_filtered_data <- data_to_display
  
  # --- Empty Data Handling ---
  if (nrow(current_filtered_data) == 0) {
    return(ggplotly(ggplot() +
                      annotate("text", x = 0.5, y = 0.5, label = "No data for selected regions/divisions") +
                      theme_void()))
  }
  
  # Prepare the data for plotting
  plot_data <- data_to_display
  
  # Create the ggplot
  p <- ggplot(plot_data,
              aes(x = REGION, # Reorder regions based on overall total count for the region
                  y = Count,
                  fill = Sections, # Fill by Clustering.Status for coloring and consistent order
                  text = paste("Region: ", REGION,
                               "<br>Section: ", Sections,
                               "<br>Count: ", scales::comma(Count)))) + # Custom tooltip text
    geom_bar(stat = "identity", position = "dodge", color = "black") + # Changed to position="dodge" for dodgeed bars
    labs(x = "",
         y = "") +
    scale_y_continuous(labels = scales::comma) + # Format y-axis labels as comma-separated numbers
    theme_minimal() +
    theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
          legend.position = "bottom", # Position legend at the bottom
          plot.title = element_text(hjust = 0.5)) # Center the plot title
  
  # Convert ggplot to plotly, ensuring custom text is used for hover
  ggplotly(p, tooltip = "text", source = "cloudenrolmentRegionPlot") %>%
    layout(hoverlabel = list(bgcolor = "white"),
           margin = list(b = 100)) # Increase bottom margin for x-axis labels
})
  
}

shinyApp(ui, server)