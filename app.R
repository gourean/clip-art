# ==============================================================================
# 0. SETUP & PACKAGE INSTALLATION
# ==============================================================================

library(shiny)

library(bslib)
library(ggplot2)
library(dplyr)
library(DT)
library(colourpicker)
library(ggthemes)
library(glue)
library(svglite)
library(readxl)
library(haven)
library(ggdist)
library(scales) # Added for explicit dependency
library(RColorBrewer) # Added for explicit dependency
library(munsell) # Explicitly needed for Shinylive
library(labeling) # Explicitly needed for Shinylive

# ==============================================================================
# 0. SETUP & THEME
# ==============================================================================
my_theme <- bs_theme(bootswatch = "minty", version = 5) %>%
  bs_add_rules("
    .accordion-button { background-color: transparent !important; box-shadow: none !important; }
    .accordion-item { border: none !important; background-color: transparent !important; }
    .accordion-button:not(.collapsed) { background-color: rgba(0,0,0,0.05) !important; }
    .form-control, .form-select { border-radius: 0.5rem; }
    .btn { border-radius: 0.5rem; }
    
    /* Full Screen Icon Placement (Bottom Right) */
    .bslib-full-screen-enter {
      position: absolute;
      bottom: 10px !important;
      right: 10px !important;
      top: auto !important;
      z-index: 1000;
      opacity: 0.5;
    }
    .bslib-full-screen-enter:hover {
        opacity: 1;
    }
    
    /* Fix Layering Issues in Expanded View */
    .modal-backdrop { z-index: 10000 !important; } 
    .modal { z-index: 10001 !important; }
    #shiny-notification-panel { z-index: 10002 !important; }
    .selectize-dropdown { z-index: 10005 !important; }
  ")

# ==============================================================================
# 1. UI DEFINITION
# ==============================================================================
ui <- page_sidebar(
  tags$head(tags$link(rel = "icon", type = "image/svg+xml", href = "icon.svg")),
  theme = my_theme,
  title = div(
    style = "display: flex; align-items: center;",
    img(src = "icon.svg", height = "40px", style = "margin-right: 15px;"), 
    span("CLIP-ART: Code-less Interactive Plot-Automated R Tool", style = "font-weight: normal; font-size: 1.2em; margin-right: 5px;"),
  ),
  
  # --- LEFT SIDEBAR: DATA & PLOT TYPE ---
  sidebar = sidebar(
    width = 350,
    title = "1. Data & Type",
    
    # A. Data Source
    fileInput("file", "Upload Data (.csv, .xlsx, .txt, .sav)", accept = c(".csv", ".xlsx", ".xls", ".txt", ".sav")),
    actionLink("demo_data", "Load Dummy Data", icon = icon("database"), style = "margin-top: -10px; margin-bottom: 20px;"),
    
    hr(),
    
    
    # B. Plot Type Selection
    selectInput("plot_type", "Select Plot Type", 
                choices = c(
                  "Scatter Plot" = "point",
                  "Bar Chart" = "bar",
                  "Box Plot" = "box",
                  "Line Plot" = "line",
                  "Histogram" = "hist",
                  "Violin Plot" = "violin",
                  "Density Plot" = "density",
                  "Dot Plot" = "dot",
                  "Raincloud Plot" = "rain"
                )),
    
    hr(),
    
    # C. Variable Mappings (Dynamic)
    uiOutput("var_selectors"),
    
    # Footer Note
    div(style = "margin-top: auto; padding-top: 20px; text-align: center; color: #777; font-size: 0.8em; width: 100%; display: block;",
        "Built by Wong GR. v1.0.0. Available on ", 
        a(href="https://github.com/gourean/clip-art", "Github", target="_blank")
    )
  ),
  
  # --- MAIN CONTENT & RIGHT SIDEBAR ---
  layout_sidebar(
    id = "right_sidebar_container",
    fillable = TRUE,
    border = FALSE,
    
    # --- RIGHT SIDEBAR: SETTINGS ---
    sidebar = sidebar(
      position = "right",
      width = 350,
      title = "2. Settings",
      open = "open", # Open by default for visibility
      
      accordion(
        open = FALSE, # Close all by default to save space
        
        # 1. Labels & Title
        accordion_panel(
          "Labels & Titles",
          icon = icon("heading"),
          textInput("plot_title", "Main Title", placeholder = "Auto"),
          textInput("plot_subtitle", "Subtitle", placeholder = "Optional"),
          textInput("x_label", "X Axis Label", placeholder = "Auto"),
          textInput("y_label", "Y Axis Label", placeholder = "Auto"),
          textInput("plot_caption", "Caption", placeholder = "Data Source...")
        ),
        
        # 2. Appearance (Theme & Color)
        accordion_panel(
          "Appearance",
          icon = icon("palette"),
          selectInput("theme_choice", "Theme", 
                      choices = c("Minimal" = "minimal", "Classic" = "classic", 
                                  "Black/White" = "bw", "Light" = "light", 
                                  "Dark" = "dark", "Void" = "void",
                                  "Economist" = "economist",
                                  "FiveThirtyEight" = "fivethirtyeight",
                                  "Solarized" = "solarized"),
                      selected = "classic"),
          
          h6("Text Settings"),
          numericInput("font_size", "Font Size (pt)", value = 14, min = 8, step = 1),
          selectInput("font_family", "Font Type", choices = c("Arial/Helvetica (Sans)"="sans", "Times/Georgia (Serif)"="serif", "Courier/Mono (Mono)"="mono"), selected = "sans"),
          selectInput("font_style", "Font Style", choices = c("Plain"="plain", "Bold"="bold", "Italic"="italic", "Bold Italic"="bold.italic"), selected = "plain"),
          
          
          h6("Color Settings"),
          radioButtons("color_mode", "Coloring Mode", 
                       choices = c("Single Color" = "single", "By Group/Variable" = "group"), 
                       inline = TRUE),
          
          conditionalPanel(
            condition = "input.color_mode == 'single'",
            colourInput("main_color", "Pick Color", value = "#5DA5DA")
          ),
          
          conditionalPanel(
            condition = "input.color_mode == 'group'",
            checkboxInput("use_palette", "Use Color Palette", value = TRUE),
            
            conditionalPanel(
              condition = "input.use_palette == true",
              selectInput("palette_choice", "Palette", 
                          choices = c("Pastel1", "Set1", "Set2", "Dark2", "Paired", "Viridis", "Magma"),
                          selected = "Pastel1")
            ),
            conditionalPanel(
              condition = "input.use_palette == false",
              # Improved Manual Color UI
              uiOutput("manual_color_ui")
            )
          ),
          sliderInput("plot_alpha", "Transparency (Alpha)", 0, 1, 1.0, step = 0.1),
        ),
        
        # 3. Geometry Options (Dynamic based on plot type)
        accordion_panel(
          "Plot Options",
          icon = icon("sliders"),
          uiOutput("geom_settings"), # Dynamic: Bin width, Point size, Flip coords
          checkboxInput("flip_coords", "Flip Coordinates", value = FALSE),
          checkboxInput("show_legend", "Show Legend", value = TRUE),
          
          hr(),
          h6("Scatter Options"),
          conditionalPanel(
            condition = "input.plot_type == 'point'",
            checkboxInput("dev_trendline", "Add Trendline (Linear)", value = FALSE),
            conditionalPanel(
              condition = "input.dev_trendline == true",
              checkboxInput("dev_equation", "Show Equation & R2", value = FALSE),
              checkboxInput("dev_intercept0", "Force Intercept at 0", value = FALSE),
              colourInput("trend_color", "Trendline Color", value = "red")
            )
          ),
          
          h6("Reference Lines"),
          checkboxInput("show_vline", "Add Vertical Line (x)", value = FALSE),
          conditionalPanel(
            condition = "input.show_vline == true",
            numericInput("vline_int", "x Intercept", value = 0)
          ),
          checkboxInput("show_hline", "Add Horizontal Line (y)", value = FALSE),
          conditionalPanel(
            condition = "input.show_hline == true",
            numericInput("hline_int", "y Intercept", value = 0)
          ),
          colourInput("ref_color_val", "Ref Line Color", "black"),
          
          h6("Stroke & Limits"),
          div(style="display: flex; gap: 10px; align-items: flex-end;",
              numericInput("stroke_size", "Stroke Size", value = 0.0, min = 0, step = 0.1, width = "120px"),
              colourInput("stroke_color", "Stroke Color", value = "black", showColour = "both", palette = "square")
          ),
          div(style="display: flex; gap: 10px;",
              checkboxInput("remove_gap_x", "Remove Gap X", value = TRUE),
              checkboxInput("remove_gap_y", "Remove Gap Y", value = FALSE)
          ),
          
          h6("Chart Options"),
          conditionalPanel(
            condition = "input.plot_type == 'bar'",
            selectInput("bar_pos", "Bar Position", 
                        choices = c("Dodge (Side-by-side)" = "dodge", "Stack (Stacked)" = "stack", "Fill (Percent)" = "fill"), 
                        selected = "dodge"),
            selectInput("error_bar_type", "Error Bars (SD)", 
                        choices = c("None", "Both (+/-)", "Positive (+)", "Negative (-)"),
                        selected = "None"),
            helpText("Note: Error bars only work with 'Dodge' position.")
          ),
          div(style="display: flex; gap: 5px;",
              textInput("x_min", "X Min", placeholder = "Auto", width = "33%"),
              textInput("x_max", "X Max", placeholder = "Auto", width = "33%"),
              textInput("x_step", "X Step", placeholder = "Auto", width = "33%")
          ),
          div(style="display: flex; gap: 5px;",
              textInput("y_min", "Y Min", placeholder = "Auto", width = "33%"),
              textInput("y_max", "Y Max", placeholder = "Auto", width = "33%"),
              textInput("y_step", "Y Step", placeholder = "Auto", width = "33%")
          )
        ),
        
        # 4. Facets
        accordion_panel(
          "Faceting",
          icon = icon("border-all"),
          selectInput("facet_var", "Split by (Facet)", choices = NULL, selected = "None"),
          checkboxInput("facet_scales", "Free Scales", value = FALSE)
        ),
        
        # 5. Dimensions & Export
        accordion_panel(
          "Export",
          icon = icon("download"),
          # DO NOT REMOVE THIS DIV: It controls layout and ensures correct syntax (closing parentheses)
          div(style="display: flex; gap: 10px; align-items: flex-end;",
              numericInput("export_width", "Width (cm)", 20, min = 1, max = 50),
              numericInput("export_height", "Height (cm)", 13.3, min = 1, max = 50)
          ),
          numericInput("export_dpi", "DPI", 300, min = 72, max = 600),
          
          div(class = "d-grid gap-2",
              div(style="display: flex; gap: 5px;",
                  downloadButton("dl_png", "PNG", class = "btn-primary w-100"),
                  downloadButton("dl_jpeg", "JPEG", class = "btn-primary w-100")
              ),
              div(style="display: flex; gap: 5px;",
                  downloadButton("dl_pdf", "PDF", class = "btn-outline-primary w-100"),
                  downloadButton("dl_svg", "SVG", class = "btn-outline-primary w-100")
              )
          )
        ),
        
        div(style = "padding: 10px;",
            actionButton("reset_all", "Reset All Settings", icon = icon("rotate-left"), class = "btn-warning w-100")
        )
      )
    ),
    
    # --- MAIN VISUALIZATION AREA ---
    navset_card_underline(
      id = "main_tabs",
      full_screen = TRUE,
      
      # Tab 1: Visualization
      nav_panel(
        title = "Visualization",
        icon = icon("chart-line"),
        plotOutput("main_plot", height = "600px")
      ),
      
      # Tab 2: Data Editor
      nav_panel(
        title = "Data Editor",
        icon = icon("table"),
        div(
          class = "d-flex justify-content-between align-items-center mb-3",
          div(
            h5("Edit Dataset", style="margin-bottom: 0;"),
            span(" (Double-click cells to edit)", style="font-size: 0.9em; color: #666;")
          ),
          div(class = "d-flex gap-2 align-items-center",
              div(style = "margin-right: 15px;", 
                  actionButton("new_data_btn", "New", icon = icon("file"), class = "btn-outline-secondary btn-sm")
              ),
              div(class = "btn-group btn-group-sm", role = "group",
                  actionButton("add_row_btn", "Row", icon = icon("plus"), class = "btn-secondary"),
                  actionButton("add_col_btn", "Col", icon = icon("plus"), class = "btn-secondary")
              ),
              div(class = "btn-group btn-group-sm", role = "group",
                  actionButton("rename_col_btn", "Col", icon = icon("pen"), class = "btn-secondary"),
                  actionButton("reorder_col_btn", "Cols", icon = icon("sliders"), class = "btn-secondary"),
                  actionButton("reorder_levels_btn", "Group", icon = icon("sort"), class = "btn-secondary"),
                  actionButton("delete_row_btn", "Row", icon = icon("trash"), class = "btn-danger")
              ),
              div(class = "btn-group btn-group-sm", role = "group",
                  downloadButton("download_csv", "CSV", class = "btn-info")
              )
          )
        ),
        uiOutput("data_table_ui")
      )
    )
  )
)

# ==============================================================================
# 2. SERVER LOGIC
# ==============================================================================
server <- function(input, output, session) {
  
  # --- A. DATA HANDLING ---
  vals <- reactiveValues(data = NULL, editing_data = NULL, dt_trigger = 0)
  
  # Load File (CSV, Excel, TXT, SPSS)
  observeEvent(input$file, {
    req(input$file)
    tryCatch({
      ext <- tools::file_ext(input$file$name)
      df <- switch(ext,
                   csv = read.csv(input$file$datapath, stringsAsFactors = TRUE),
                   xlsx = read_excel(input$file$datapath),
                   xls = read_excel(input$file$datapath),
                   txt = read.table(input$file$datapath, header = TRUE, sep = "\t"),
                   sav = read_sav(input$file$datapath),
                   read.csv(input$file$datapath, stringsAsFactors = TRUE) # Default
      )
      
      # Convert to vanilla data frame and handle factors if needed
      df <- as.data.frame(df)
      if(ext %in% c("xlsx", "xls", "sav")) {
        # naive factor detection for strings
        df[] <- lapply(df, function(x) if(is.character(x)) as.factor(x) else x)
      }
      
      vals$data <- df
      vals$editing_data <- df
      vals$dt_trigger <- vals$dt_trigger + 1
      showNotification("Data loaded successfully!", type = "message")
    }, error = function(e) {
      showNotification(paste("Error loading file:", e$message), type = "error")
    })
  })
  
  # Load Demo Data
  observeEvent(input$demo_data, {
    set.seed(123)
    n <- 200
    df <- data.frame(
      ID = 1:n,
      Group = sample(c("Control", "Treatment A", "Treatment B"), n, replace = TRUE),
      Age = round(rnorm(n, 45, 10)),
      Gender = sample(c("Male", "Female"), n, replace = TRUE),
      Score_Pre = round(rnorm(n, 60, 15)),
      Score_Post = round(rnorm(n, 70, 15)),
      Category = sample(c("Low", "Medium", "High"), n, replace = TRUE)
    )
    # Add some correlation
    df$Score_Post <- round(df$Score_Pre + ifelse(df$Group == "Control", 0, 10) + rnorm(n, 0, 5))
    
    val <- as.data.frame(df) # Ensure it is a data frame
    vals$data <- val
    vals$editing_data <- val
    vals$dt_trigger <- vals$dt_trigger + 1
    showNotification("Dummy dataset loaded.", type = "message")
  })
  
  # --- DATA EDITOR ENGINE ---
  
  # Render DT for Editing
  # Render DT for Editing
  # Wrap in renderUI to force complete teardown/rebuild on heavy resets if needed
  output$data_table_ui <- renderUI({
    vals$dt_trigger
    DTOutput("data_table")
  })
  
  output$data_table <- renderDT({
    req(vals$editing_data)
    
    isolate({
      datatable(vals$editing_data, 
                editable = TRUE, 
                options = list(
                  pageLength = 10, 
                  scrollX = TRUE,
                  keys = TRUE
                ),
                callback = JS("
                    table.on('key', function(e, datatable, key, cell, originalEvent){
                      var targetName = originalEvent.target.localName;
                      if(key == 13 && targetName == 'body'){
                        $(cell.node()).trigger('dblclick.dt');
                      }
                    });
                    // Global listener for input field to blur on Enter
                    $(document).on('keydown', '.dataTable input', function(e) {
                      if(e.which === 13) {
                         e.preventDefault();
                         $(this).blur();
                      }
                    });
                 ")
      )
    })
  })
  
  proxy_dt <- dataTableProxy("data_table")
  
  # Capture Edits
  observeEvent(input$data_table_cell_edit, {
    info <- input$data_table_cell_edit
    
    i <- info$row
    j <- info$col # Correct index mapping for rownames=TRUE
    
    v <- info$value
    
    # Update the editing_data safely
    # Use DT::coerceValue for better type handling
    current_val <- vals$editing_data[i, j]
    
    new_val <- tryCatch({
      DT::coerceValue(v, current_val)
    }, error = function(e) {
      # Fallback or just return v (which might be character)
      v 
    })
    
    # Handle factor levels dynamic addition
    if(is.factor(current_val)) {
      if(!(new_val %in% levels(vals$editing_data[[j]]))) {
        # Add level or convert to char
        vals$editing_data[[j]] <- as.character(vals$editing_data[[j]])
      }
    }
    
    vals$editing_data[i, j] <- new_val
    
    # Important: Do NOT trigger a re-render of DT
    # We don't need to do anything else because editable=TRUE updates the client side automatically
    # AND we isolated the renderDT, so updating vals$editing_data here won't cause redraw.
  })
  
  # Live Sync: Update Plot automatically when data changes
  observe({
    req(vals$editing_data)
    # Sync editing data to main data for plotting
    vals$data <- vals$editing_data
  })
  
  # New Empty Data
  observeEvent(input$new_data_btn, {
    # Create a small empty placeholder
    df <- data.frame(
      Category = c("A", "B", "C"),
      Value1 = c(10, 20, 30),
      Value2 = c(5.5, 6.2, 7.1)
    )
    vals$editing_data <- df
    vals$dt_trigger <- vals$dt_trigger + 1
    # Do NOT update vals$data (Plot) until they save
    # Do NOT update vals$data (Plot) until they save
    showNotification("New template created. Edit and Save to visualize.", type = "message")
  })
  
  # --- EXTENDED EDITOR FUNCTIONS ---
  
  # Download CSV
  output$download_csv <- downloadHandler(
    filename = function() { paste0("edited-data-", Sys.Date(), ".csv") },
    content = function(file) {
      req(vals$editing_data)
      write.csv(vals$editing_data, file, row.names = FALSE)
    }
  )
  
  # Add Row
  observeEvent(input$add_row_btn, {
    req(vals$editing_data)
    df <- vals$editing_data
    
    # Create a new row with appropriate NA/defaults
    new_row <- df[1, ] # Copy structure
    # Reset values to NA or empty
    for(j in 1:ncol(new_row)) {
      if(is.numeric(new_row[[j]])) new_row[[j]] <- NA # Use NA for empty
      else if(is.factor(new_row[[j]])) new_row[[j]] <- levels(new_row[[j]])[1] # pick first level
      else new_row[[j]] <- ""
    }
    
    # Bind
    vals$editing_data <- rbind(df, new_row)
    vals$dt_trigger <- vals$dt_trigger + 1
    showNotification("Row added.", type = "message")
  })
  
  # Add Column
  observeEvent(input$add_col_btn, {
    showModal(modalDialog(
      title = "Add New Column",
      textInput("new_col_name", "Column Name", placeholder = "NewVar"),
      selectInput("new_col_type", "Type", choices = c("Numeric", "Text")),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_add_col", "Add", class = "btn-primary")
      )
    ))
  })
  
  observeEvent(input$confirm_add_col, {
    req(input$new_col_name)
    removeModal()
    
    col_name <- input$new_col_name
    # Check duplicate
    if(col_name %in% names(vals$editing_data)) {
      showNotification("Column name already exists.", type = "error")
      return()
    }
    
    df <- vals$editing_data
    n <- nrow(df)
    
    new_col <- if(input$new_col_type == "Numeric") rep(0, n) else rep("", n)
    df[[col_name]] <- new_col
    
    vals$editing_data <- df
    vals$dt_trigger <- vals$dt_trigger + 1
    showNotification(paste("Column", col_name, "added."), type = "message")
  })
  
  # Rename Column
  observeEvent(input$rename_col_btn, {
    req(vals$editing_data)
    showModal(modalDialog(
      title = "Rename Column",
      selectInput("rename_target", "Select Column", choices = names(vals$editing_data)),
      textInput("rename_new_name", "New Name"),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_rename_col", "Rename", class = "btn-primary")
      )
    ))
  })
  
  observeEvent(input$confirm_rename_col, {
    req(input$rename_new_name)
    removeModal()
    
    old <- input$rename_target
    new <- input$rename_new_name
    
    if(new %in% names(vals$editing_data)) {
      showNotification("Name already exists.", type = "error")
      return()
    }
    
    df <- vals$editing_data
    names(df)[names(df) == old] <- new
    
    vals$editing_data <- df
    vals$dt_trigger <- vals$dt_trigger + 1
    showNotification(paste("Renamed", old, "to", new), type = "message")
  })
  
  # Reorder/Delete Columns
  observeEvent(input$reorder_col_btn, {
    req(vals$editing_data)
    cols <- names(vals$editing_data)
    showModal(modalDialog(
      title = "Reorder or Delete Columns",
      selectizeInput("reorder_cols_list", "Drag to Reorder (Remove to Delete)", choices = cols, selected = cols, multiple = TRUE, options = list(plugins = list('drag_drop'))),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_reorder_col", "Confirm", class = "btn-primary")
      )
    ))
  })
  
  observeEvent(input$confirm_reorder_col, {
    # If NULL (all removed), we might want to prevent or allow clearing table?
    # Let's prevent clearing all columns for safety, or just warn.
    new_order <- input$reorder_cols_list
    
    if(length(new_order) == 0) {
      showNotification("Error: You must keep at least one column.", type = "error")
      return()
    }
    
    removeModal()
    
    # Subset columns (this handles reordering AND deletion)
    vals$editing_data <- vals$editing_data[, new_order, drop = FALSE]
    vals$dt_trigger <- vals$dt_trigger + 1
    showNotification("Columns updated.", type = "message")
  })
  
  # Delete Row
  observeEvent(input$delete_row_btn, {
    req(vals$editing_data)
    sel_rows <- input$data_table_rows_selected
    
    if(is.null(sel_rows) || length(sel_rows) == 0) {
      showNotification("Please select row(s) in the table to delete.", type = "warning")
      return()
    }
    
    # Remove rows
    # Note: DT indices are 1-based if strictly configured, let's verify.
    # We are using basic DT (server side processing default is TRUE for large data, 
    # but here we load entire DF to memory so it's effectively client-side 
    # UNLESS strict server processing is on. default datatable() is intelligent.
    # Standard shiny input$rows_selected matches the data indices usually.
    
    vals$editing_data <- vals$editing_data[-sel_rows, , drop = FALSE]
    vals$dt_trigger <- vals$dt_trigger + 1
    showNotification("Row(s) deleted.", type = "message")
  })
  
  # Reorder Levels (Step 1: Select Variable)
  observeEvent(input$reorder_levels_btn, {
    req(vals$editing_data)
    # Get categorical variables only
    vars <- get_vars()
    cats <- vars$cats
    
    if(length(cats) == 0) {
      showNotification("No categorical variables found to reorder.", type = "warning")
      return()
    }
    
    showModal(modalDialog(
      title = "Reorder Groups",
      selectInput("reorder_lvl_target", "Select Categorical Variable", choices = cats),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_lvl_first_step", "Next", class = "btn-primary")
      )
    ))
  })
  
  # Reorder Levels (Step 2: Reorder)
  observeEvent(input$confirm_lvl_first_step, {
    req(input$reorder_lvl_target)
    removeModal()
    
    target_col <- input$reorder_lvl_target
    # Get current levels
    current_vals <- vals$editing_data[[target_col]]
    
    # If it's already a factor, get levels. If character, get unique.
    if(is.factor(current_vals)) {
      lvls <- levels(current_vals)
    } else {
      lvls <- unique(as.character(current_vals))
    }
    
    showModal(modalDialog(
      title = paste("Reorder Levels for:", target_col),
      p("Drag and drop to reorder the levels. The top item will be the first group."),
      selectizeInput("reorder_lvl_order", "Order", choices = lvls, selected = lvls, multiple = TRUE, options = list(plugins = list('drag_drop'))),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_reorder_lvl", "Apply Order", class = "btn-primary")
      )
    ))
  })
  
  # Reorder Levels (Step 3: Apply)
  observeEvent(input$confirm_reorder_lvl, {
    req(input$reorder_lvl_order)
    new_order <- input$reorder_lvl_order
    target_col <- input$reorder_lvl_target # Should still be accessible from input if we didn't destroy it? 
    # Actually inputs persist until overwritten.
    
    removeModal()
    
    df <- vals$editing_data
    
    # Modify the column to be a factor with specific levels
    # Note: We must ensure all current values are in the new order list
    # The selectize input with drag_drop on the full set of levels should ensure this,
    # unless the user deleted one from the box.
    
    # Check completeness
    current_vals <- df[[target_col]]
    if(is.factor(current_vals)) existing_lvls <- levels(current_vals)
    else existing_lvls <- unique(as.character(current_vals))
    
    if(!all(existing_lvls %in% new_order)) {
      showNotification("Error: You must include all levels.", type = "error")
      return()
    }
    
    # Apply
    df[[target_col]] <- factor(df[[target_col]], levels = new_order)
    
    vals$editing_data <- df
    vals$dt_trigger <- vals$dt_trigger + 1
    showNotification(paste("Levels reordered for", target_col), type = "message")
  })
  
  # Helper to get numeric/categorical columns
  get_vars <- reactive({
    req(vals$data)
    df <- vals$data
    nums <- names(df)[sapply(df, is.numeric)]
    cats <- names(df)[sapply(df, function(x) is.factor(x) || is.character(x))]
    list(nums = nums, cats = cats, all = names(df))
  })
  
  # --- SMART LABEL RESET ---
  # Simplified: Always reset manual label when variable changes to prevent confusion
  observeEvent(input$var_x, {
    updateTextInput(session, "x_label", value = "")
  }, ignoreInit = TRUE)
  
  observeEvent(input$var_y, {
    updateTextInput(session, "y_label", value = "")
  }, ignoreInit = TRUE)
  
  # --- RESET ALL SETTINGS ---
  observeEvent(input$reset_all, {
    showModal(modalDialog(
      title = "Reset All Settings",
      "Are you sure you want to reset all plot settings to their defaults? This action cannot be undone.",
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_reset_all", "Yes, Reset Everything", class = "btn-danger")
      )
    ))
  })
  
  observeEvent(input$confirm_reset_all, {
    removeModal()
    
    # 1. Labels
    updateTextInput(session, "plot_title", value = "")
    updateTextInput(session, "plot_subtitle", value = "")
    updateTextInput(session, "x_label", value = "")
    updateTextInput(session, "y_label", value = "")
    updateTextInput(session, "plot_caption", value = "")
    
    # 2. Appearance
    updateSelectInput(session, "theme_choice", selected = "classic")
    updateNumericInput(session, "font_size", value = 14)
    updateSelectInput(session, "font_family", selected = "sans")
    updateSelectInput(session, "font_style", selected = "plain")
    
    updateRadioButtons(session, "color_mode", selected = "single") # Default back to single
    updateColourInput(session, "main_color", value = "#5DA5DA")
    updateCheckboxInput(session, "use_palette", value = TRUE)
    updateSelectInput(session, "palette_choice", selected = "Pastel1")
    updateSliderInput(session, "plot_alpha", value = 1.0)
    
    # 3. Geometry / Options
    updateCheckboxInput(session, "flip_coords", value = FALSE)
    updateCheckboxInput(session, "show_legend", value = TRUE)
    updateCheckboxInput(session, "dev_trendline", value = FALSE)
    updateCheckboxInput(session, "show_vline", value = FALSE)
    updateCheckboxInput(session, "show_hline", value = FALSE)
    
    # Stroke & Limits (Reset based on plot type default if needed, or just standard 0.5/black)
    # Resetting to standard defaults
    # Need to match the logic in the auto-update observer
    val <- if(input$plot_type == "point") 0.0 else 0.5
    updateNumericInput(session, "stroke_size", value = val)
    updateColourInput(session, "stroke_color", value = "black")
    
    updateCheckboxInput(session, "remove_gap_x", value = TRUE)
    updateCheckboxInput(session, "remove_gap_y", value = FALSE)
    
    updateTextInput(session, "x_min", value = "")
    updateTextInput(session, "x_max", value = "")
    updateTextInput(session, "x_step", value = "")
    updateTextInput(session, "y_min", value = "")
    updateTextInput(session, "y_max", value = "")
    updateTextInput(session, "y_step", value = "")
    
    # Facets
    updateSelectInput(session, "facet_var", selected = "None")
    updateCheckboxInput(session, "facet_scales", value = FALSE)
    
    # Exports
    updateNumericInput(session, "export_width", value = 20)
    updateNumericInput(session, "export_height", value = 13.3)
    
    updateNumericInput(session, "export_dpi", value = 300)
    
    showNotification("All plot settings have been reset.", type = "message")
  })
  
  # --- B. DYNAMIC UI UPDATES ---
  
  # 1. Variable Selectors (Left Panel)
  output$var_selectors <- renderUI({
    req(vals$data)
    vars <- get_vars()
    ptype <- input$plot_type
    
    # --- PERSISTENCE LOGIC ---
    # Capture current selections safely
    curr_x <- isolate(input$var_x)
    curr_y <- isolate(input$var_y)
    
    # 1. Determine X Selection
    # Default fallback
    selected_x <- vars$all[1]
    
    # If we have a previous selection, check validity for new plot type
    if (!is.null(curr_x) && curr_x %in% vars$all) {
      if (ptype %in% c("hist", "density")) {
        # Must be numeric
        if (curr_x %in% vars$nums) selected_x <- curr_x
        else if (length(vars$nums) > 0) selected_x <- vars$nums[1]
      } else {
        # Can be any
        selected_x <- curr_x
      }
    } else {
      # Initial load or invalid previous
      if (ptype %in% c("hist", "density") && length(vars$nums) > 0) {
        selected_x <- vars$nums[1]
      }
    }
    
    # 2. Determine Y Selection
    # Only needed for non-univariate plots
    selected_y <- "None"
    
    if (!ptype %in% c("hist", "density", "dot")) {
      # Valid choices for Y
      y_choices <- if(ptype == "bar") c("None", vars$all) else vars$all
      
      # Default fallback
      default_y <- if(length(vars$nums) > 1) vars$nums[2] else vars$all[1]
      if(ptype == "bar") default_y <- "None"
      
      selected_y <- default_y
      
      # Persistence check
      if (!is.null(curr_y) && curr_y %in% y_choices) {
        selected_y <- curr_y
      }
    }
    
    tagList(
      # X-Axis
      selectInput("var_x", "X Variable", choices = vars$all, selected = selected_x),
      
      # Y-Axis
      if(!ptype %in% c("hist", "density", "dot")) {
        y_choices <- if(ptype == "bar") c("None", vars$all) else vars$all
        selectInput("var_y", "Y Variable", choices = y_choices, selected = selected_y)
      },
      
      # Color/Fill (Grouping)
      selectInput("var_color", "Color/Group By", choices = c("None", vars$cats), selected = "None"),
      
      # Size (Scatter)
      if(ptype == "point")
        selectInput("var_size", "Size By", choices = c("None", vars$nums), selected = "None")
    )
  })
  
  # --- SCALE RESET LOGIC ---
  observeEvent(input$plot_type, {
    updateTextInput(session, "x_min", value = "")
    updateTextInput(session, "x_max", value = "")
    updateTextInput(session, "x_step", value = "")
    updateTextInput(session, "y_min", value = "")
    updateTextInput(session, "y_max", value = "")
    updateTextInput(session, "y_step", value = "")
  })
  
  # 2. Facet Options (Right Panel)
  observe({
    req(vals$data)
    vars <- get_vars()
    updateSelectInput(session, "facet_var", choices = c("None", vars$cats))
  })
  
  # 3. Geometry Options (Right Panel)
  output$geom_settings <- renderUI({
    ptype <- input$plot_type
    
    tagList(
      if(ptype == "point") sliderInput("point_size", "Point Size", 1, 10, 3),
      if(ptype %in% c("hist", "density")) sliderInput("bins", "Bins (Hist Only)", 5, 100, 30),
      if(ptype == "box") checkboxInput("box_notch", "Notched Boxplot", FALSE),
      if(ptype == "line") sliderInput("line_width", "Line Width", 0.5, 5, 1),
      if(ptype == "dot") radioButtons("dot_layout", "Dot Plot Layout", choices = c("Center" = "center", "Histogram" = "hist"), selected = "center", inline = TRUE),
      if(ptype == "rain") sliderInput("rain_jitter", "Jitter Width", 0.05, 0.4, 0.15, step = 0.05)
    )
  })
  
  # 3b. Manual Color Pickers (Dynamic)
  # 3b. Manual Color Pickers (Dynamic)
  output$manual_color_ui <- renderUI({
    req(vals$data, input$var_color)
    if(input$var_color == "None" || input$use_palette) return(NULL)
    
    df <- vals$data
    # Get levels
    if(is.factor(df[[input$var_color]])) {
      lvls <- levels(df[[input$var_color]])
    } else {
      lvls <- sort(unique(as.character(df[[input$var_color]])))
    }
    
    n <- length(lvls)
    # Determine default colors from current palette selection
    pal_name <- input$palette_choice
    
    def_cols <- tryCatch({
      if(pal_name %in% c("Viridis", "Magma")) {
        scales::viridis_pal(option = tolower(pal_name))(n)
      } else {
        # Brewer Palettes
        # Handle case where n > palette max
        # scales::brewer_pal returns a function, but we can just use colorRampPalette on the brewer colors directly for safety
        # Getting max colors for the palette
        max_n <- RColorBrewer::brewer.pal.info[pal_name, "maxcolors"]
        if(n <= max_n) {
          RColorBrewer::brewer.pal(n, pal_name)
        } else {
          colorRampPalette(RColorBrewer::brewer.pal(max_n, pal_name))(n)
        }
      }
    }, error = function(e) {
      # Fallback
      scales::hue_pal()(n)
    })
    
    lapply(seq_along(lvls), function(i) {
      lvl <- lvls[i]
      id <- paste0("col_", i)
      
      # Positional Persistence:
      # If input[[id]] exists, use it. Otherwise default.
      curr_val <- input[[id]]
      init_val <- if(!is.null(curr_val)) curr_val else def_cols[i]
      
      div(style="margin-bottom: 5px;",
          colourInput(id, label = lvl, value = init_val)
      )
    })
  })
  
  # 4. Auto-Switch Color Mode
  observeEvent(input$var_color, {
    req(input$var_color)
    if(input$var_color != "None") {
      updateRadioButtons(session, "color_mode", selected = "group")
    }
  })
  
  # 5. Auto-Update Stroke Size Default
  observeEvent(input$plot_type, {
    # Axis line size in theme_classic() is 0.5 pts (~0.176 mm).
    # For points, stroke is in pts. 0.5 matches axis.
    # For others (bar/box/line), linewidth is in mm. ~0.2 mm matches axis.
    # We default points to 0.0 (no border) for cleanliness, but others to 0.2 to match axis visual.
    val <- if(input$plot_type == "point") 0.0 else 0.5
    updateNumericInput(session, "stroke_size", value = val)
  })
  
  # --- C. PLOT GENERATION ---
  
  final_plot <- reactive({
    req(vals$data, input$var_x)
    
    df <- vals$data
    p <- ggplot(df, aes(x = .data[[input$var_x]]))
    
    # Y-Axis
    has_y <- !is.null(input$var_y) && input$var_y != "None" && !input$plot_type %in% c("hist", "density", "dot")
    if(has_y) {
      p <- p + aes(y = .data[[input$var_y]])
    }
    
    # Color Mapping
    if(input$color_mode == "group" && input$var_color != "None") {
      p <- p + aes(color = .data[[input$var_color]], fill = .data[[input$var_color]])
    }
    
    # Size Mapping (Scatter)
    if(!is.null(input$var_size) && input$var_size != "None" && input$plot_type == "point") {
      p <- p + aes(size = .data[[input$var_size]])
    }
    
    # 2. Geometries
    ptype <- input$plot_type
    col <- input$main_color
    alpha <- input$plot_alpha
    stroke <- input$stroke_size
    stroke_col <- input$stroke_color
    
    # Helper to clean up "group" color mode if user didn't select a variable
    # Fix: If mode is "group" but var is "None", revert to single color behavior
    effective_mode <- if(input$color_mode == "group" && input$var_color == "None") "single" else input$color_mode
    
    use_fill <- if(effective_mode == "single") list(fill = col, color = col) else list()
    use_alpha <- list(alpha = alpha)
    
    # Merge fixed aesthetics
    # Add stroke to params (color is often border, except for points where we need specific shapes)
    
    # Determine Geom Params
    geom_params <- use_alpha
    
    if(effective_mode == "single") {
      geom_params <- c(geom_params, list(fill = col, color = col))
    }
    
    # If using manual colors or palette, we handle scale_... separate
    # Here we just set size/width stuff
    
    if(ptype == "point") {
      sz <- if(is.null(input$point_size)) 3 else input$point_size
      
      # For points, if we want stroke (border), we need shape 21 (fillable)
      # If single color mode, fill=col, color=col.
      # If group mode, both are mapped.
      
      if(stroke > 0) {
        # Shape 21 allows fill and color (stroke)
        p <- p + geom_point(shape = 21, size = sz, stroke = stroke, color = stroke_col, alpha = alpha)
        
        if(effective_mode == "single") {
          # Optimization: overwrite the previous geom call with specific fill
          p <- ggplot(df, aes(x = .data[[input$var_x]])) 
          if(has_y) p <- p + aes(y = .data[[input$var_y]])
          p <- p + geom_point(shape = 21, size = sz, stroke = stroke, color = stroke_col, fill = col, alpha = alpha)
        }
      } else {
        # Standard solid points (shape 19)
        if(effective_mode == "single") geom_params$fill <- NULL # Points take color only
        p <- p + do.call(geom_point, c(geom_params, list(size = sz)))
      }
      
      # Trendline
      if(isTRUE(input$dev_trendline) && has_y) {
        formula_val <- if(isTRUE(input$dev_intercept0)) y ~ x + 0 else y ~ x
        p <- p + geom_smooth(method = "lm", formula = formula_val, se = FALSE, color = input$trend_color, linetype = "dashed")
        
        if(isTRUE(input$dev_equation)) {
          # Calculate LM manually to get equation
          try({
            if(isTRUE(input$dev_intercept0)) m <- lm(df[[input$var_y]] ~ df[[input$var_x]] + 0)
            else m <- lm(df[[input$var_y]] ~ df[[input$var_x]])
            
            cf <- coef(m)
            r2 <- summary(m)$r.squared
            
            eq_text <- if(isTRUE(input$dev_intercept0)) {
              sprintf("y = %.3fx, R2 = %.3f", cf[1], r2)
            } else {
              sprintf("y = %.3fx + %.3f, R2 = %.3f", cf[2], cf[1], r2)
            }
            
            p <- p + annotate("text", x = -Inf, y = Inf, label = eq_text, hjust = -0.1, vjust = 1.5, size = 5)
          }, silent=TRUE)
        }
      }
      
    } 
    else if(ptype == "line") {
      wd <- if(is.null(input$line_width)) 1 else input$line_width
      if(effective_mode == "single") geom_params$fill <- NULL
      p <- p + do.call(geom_line, c(geom_params, list(linewidth = wd)))
    }
    else if(ptype == "bar") {
      if(has_y) {
        # Mean Bar Plot
        sum_params <- list(fun = "mean", geom = "bar", position = input$bar_pos)
        
        # Fix Color Bug for Bar: Use effective_mode
        if(effective_mode == "single") {
          sum_params$fill <- col
          sum_params$color <- if(stroke > 0) stroke_col else col 
        } else {
          if(stroke > 0) sum_params$color <- stroke_col
        }
        
        if(stroke > 0) sum_params$linewidth <- stroke
        sum_params$alpha <- alpha
        
        p <- p + do.call(stat_summary, sum_params)
        
        # Error Bars
        if(input$error_bar_type != "None" && input$bar_pos == "dodge") {
          # Custom function generator
          get_err <- function(x) {
            m <- mean(x, na.rm=TRUE)
            s <- sd(x, na.rm=TRUE)
            lower <- if(input$error_bar_type == "Positive (+)") m else m - s
            upper <- if(input$error_bar_type == "Negative (-)") m else m + s
            data.frame(y = m, ymin = lower, ymax = upper)
          }
          
          err_params <- list(fun.data = get_err, 
                             geom = "errorbar", width = 0.2, position = position_dodge(0.9))
          err_params$color <- "black"
          err_params$linewidth <- if(stroke > 0) stroke else 0.5
          
          p <- p + do.call(stat_summary, err_params)
        }
        
      } else {
        # Count Plot
        bar_params <- list(position = input$bar_pos)
        if(effective_mode == "single") {
          bar_params$fill <- col
          bar_params$color <- if(stroke > 0) stroke_col else col
        } else {
          if(stroke > 0) bar_params$color <- stroke_col
        }
        if(stroke > 0) bar_params$linewidth <- stroke
        bar_params$alpha <- alpha
        p <- p + do.call(geom_bar, bar_params)
      }
    }
    else if(ptype == "hist") {
      bins <- if(is.null(input$bins)) 30 else input$bins
      extra_args <- list(bins = bins, position = "identity")
      if(stroke > 0) { extra_args$color <- stroke_col; extra_args$linewidth <- stroke }
      
      # Use effective_mode for fallback
      if(effective_mode == "single" && stroke == 0) {
        # Remove border color if single mode and no stroke, so it doesn't default to weird lines
        geom_params$color <- NULL
      }
      # If effective mode is single, ensure fill is passed if not in geom_params?
      # geom_params already has fill=col if effective_mode==single.
      
      p <- p + do.call(geom_histogram, c(geom_params, extra_args))
    }
    else if(ptype == "density") {
      p <- p + do.call(geom_density, c(geom_params))
    }
    else if(ptype == "box") {
      if(isTRUE(input$box_notch)) geom_params <- c(geom_params, list(notch = TRUE))
      if(stroke > 0) {
        geom_params$linewidth <- stroke # For boxplot, size controls line thickness
        geom_params$color <- stroke_col
      }
      p <- p + do.call(geom_boxplot, geom_params)
    }
    else if(ptype == "violin") {
      if(stroke > 0) {
        geom_params$linewidth <- stroke
        geom_params$color <- stroke_col
      }
      p <- p + do.call(geom_violin, geom_params)
    }
    else if(ptype == "dot") {
      # Map selection to stackdir
      # Default to center
      s_dir <- "center"
      if(!is.null(input$dot_layout)) {
        if(input$dot_layout == "hist") s_dir <- "up"
        else s_dir <- "center"
      }
      p <- p + do.call(geom_dotplot, c(geom_params, list(binaxis = "x", stackdir = s_dir)))
    }
    else if(ptype == "rain") {
      # FAST Raincloud: Slab (Density) + Boxplot + Jitter (Rain)
      # Replaced stat_halfeye/stat_dots with stat_slab/geom_jitter for performance
      
      # 1. Cloud (Density)
      cloud_args <- list(
        side = "right", adjust = .5, scale = 0.6, justification = -0.2, 
        alpha = alpha, colour = NA # No border for cloud by default
      )
      if(effective_mode == "single") {
        cloud_args$fill <- col
      }
      # If group mode, fill is inherited from p
      
      p <- p + do.call(ggdist::stat_slab, cloud_args) +
        # 2. Umbrella (Boxplot) - High Contrast (White Fill)
        geom_boxplot(
          width = .12, outlier.shape = NA, alpha = 0.8,
          fill = "white", color = stroke_col, linewidth = stroke
        )
      
      # 3. Rain (Jittered Points)
      pt_args <- list(
        position = position_jitter(width = input$rain_jitter, height = 0),
        size = 1.5, alpha = alpha, shape = 19 # Solid circle
      )
      
      if(effective_mode == "single") {
        pt_args$color <- col # Use main fill color for points in single mode
      } else {
        # Group mode: color aesthetic is already mapped in `p`
        # No need to set `color` here, it will inherit from `aes(color = .data[[input$var_color]])`
      }
      
      p <- p + do.call(geom_point, pt_args)
      
      # Boxplot needs to be on top? geom_point usually below? 
      # Actually standard raincloud has points below density. Jitter is "Rain".
      # Layout: Boxplot (Center), Cloud (Right/Up), Rain (Left/Down)
      # stat_slab(side="right") goes up/right. 
      # Jitter should be to the left of the boxplot?
      # standard geom_jitter centers around the x-axis tick.
      # To offset jitter, we might need 'nudge_x' but geom_point doesn't support nudge AND jitter easily composed without extra pkg.
      # Simplified: Just centered jitter under the boxplot is often acceptable "Rain".
      # Or, we can use side="left" for density? 
      # Let's keep it simple: Overlapping jitter is standard for "PointCloud".
      # If user wants "Offset", we'd need position_nudge(). 
      # Optimization priority: Speed. geom_jitter is fastest.
    }
    
    # 3. Faceting
    if(input$facet_var != "None") {
      scales_val <- if(input$facet_scales) "free" else "fixed"
      p <- p + facet_wrap(as.formula(paste("~", input$facet_var)), scales = scales_val)
    }
    
    # Reference Lines
    if(isTRUE(input$show_vline)) {
      p <- p + geom_vline(xintercept = input$vline_int, color = input$ref_color_val, linetype = "dashed")
    }
    if(isTRUE(input$show_hline)) {
      p <- p + geom_hline(yintercept = input$hline_int, color = input$ref_color_val, linetype = "dashed")
    }
    
    # 4. Appearance
    # A. Labels
    title_txt <- if(input$plot_title != "") input$plot_title else NULL
    sub_txt <- if(input$plot_subtitle != "") input$plot_subtitle else NULL
    x_txt <- if(input$x_label != "") input$x_label else input$var_x
    y_txt <- if(input$y_label != "") {
      input$y_label 
    } else if(ptype == "bar" && !has_y) {
      "Count"
    } else if(ptype == "hist") { # For histograms without pre-computation
      "Count"
    } else if(ptype == "density") {
      "Density"
    } else if(ptype == "dot") {
      "Count" # Default geom_dotplot is density
    } else {
      input$var_y
    }
    cap_txt <- if(input$plot_caption != "") input$plot_caption else NULL
    
    p <- p + labs(title = title_txt, subtitle = sub_txt, x = x_txt, y = y_txt, caption = cap_txt)
    
    # B. Theme
    thm <- switch(input$theme_choice,
                  "minimal" = theme_minimal(),
                  "classic" = theme_classic(),
                  "bw" = theme_bw(),
                  "light" = theme_light(),
                  "dark" = theme_dark(),
                  "void" = theme_void(),
                  "economist" = ggthemes::theme_economist(),
                  "fivethirtyeight" = ggthemes::theme_fivethirtyeight(),
                  "solarized" = ggthemes::theme_solarized()
    )
    p <- p + thm
    
    # C. Font Size (Base adjustment) & Centered Titles
    fs <- input$font_size %||% 14
    ff <- input$font_family %||% "sans"
    fst <- input$font_style %||% "plain"
    
    p <- p + theme(
      text = element_text(size = fs, family = ff, face = fst),
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5)
    )
    
    # C. Color Logic (Palette vs Manual)
    if(input$color_mode == "group" && input$var_color != "None") {
      if(input$use_palette) {
        if(input$palette_choice %in% c("Viridis", "Magma")) {
          p <- p + scale_color_viridis_d(option = tolower(input$palette_choice)) + 
            scale_fill_viridis_d(option = tolower(input$palette_choice))
        } else {
          p <- p + scale_color_brewer(palette = input$palette_choice) + 
            scale_fill_brewer(palette = input$palette_choice)
        }
      } else {
        # Manual Color Logic
        req(vals$data[[input$var_color]])
        # Fix: Match logic in manual_color_ui to prevent swapping
        col_var <- vals$data[[input$var_color]]
        if(is.factor(col_var)) {
          lvls <- levels(col_var)
        } else {
          lvls <- sort(unique(as.character(col_var)))
        }
        
        # Construct colors from inputs (Positional)
        man_cols <- sapply(seq_along(lvls), function(i) {
          # Positional ID: col_1, col_2...
          id <- paste0("col_", i)
          col <- input[[id]]
          if(is.null(col)) "grey50" else col
        })
        names(man_cols) <- lvls
        
        p <- p + scale_color_manual(values = man_cols) + 
          scale_fill_manual(values = man_cols)
      }
    }
    
    # D. Axes Limits & Gap
    # Helper to parse text inputs
    parse_lim <- function(x) if(x == "" || is.null(x)) NA else as.numeric(x)
    
    xlims <- c(parse_lim(input$x_min), parse_lim(input$x_max))
    ylims <- c(parse_lim(input$y_min), parse_lim(input$y_max))
    
    # Gap removal: expand = c(0,0)
    
    is_num_x <- is.numeric(df[[input$var_x]])
    is_num_y <- if(has_y) is.numeric(df[[input$var_y]]) else TRUE
    
    if(is_num_x) {
      # Construct arguments
      args_x <- list()
      if(!all(is.na(xlims))) args_x$limits <- xlims
      if(input$remove_gap_x) args_x$expand <- c(0,0)
      
      # Breaks (Step)
      step_x <- parse_lim(input$x_step)
      if(!is.na(step_x)) {
        if(!any(is.na(xlims))) {
          # All defined: strict sequence from min to max
          args_x$breaks <- seq(xlims[1], xlims[2], by = step_x)
        } else {
          # Partial/Auto limits: use breaks_width
          args_x$breaks <- scales::breaks_width(step_x)
        }
      }
      
      if(length(args_x) > 0) p <- p + do.call(scale_x_continuous, args_x)
    } else {
      # Discrete X
      if(input$remove_gap_x) p <- p + scale_x_discrete(expand = c(0,0))
    }
    
    if(is_num_y) {
      args_y <- list()
      if(!all(is.na(ylims))) args_y$limits <- ylims
      if(input$remove_gap_y) args_y$expand <- c(0,0)
      
      step_y <- parse_lim(input$y_step)
      if(!is.na(step_y)) {
        if(!any(is.na(ylims))) {
          args_y$breaks <- seq(ylims[1], ylims[2], by = step_y)
        } else {
          args_y$breaks <- scales::breaks_width(step_y)
        }
      }
      
      if(length(args_y) > 0) p <- p + do.call(scale_y_continuous, args_y)
    }
    
    # E. Flip
    if(input$flip_coords) p <- p + coord_flip()
    
    # F. Legend
    if(!input$show_legend) p <- p + theme(legend.position = "none")
    
    p
  })
  
  # --- D. OUTPUTS ---
  
  output$main_plot <- renderPlot({
    final_plot()
  })
  
  # Exports
  # Exports
  output$dl_png <- downloadHandler(
    filename = function() { paste0("clip-art-plot-", Sys.Date(), ".png") },
    content = function(file) {
      ggsave(file, plot = final_plot(), device = "png", 
             width = input$export_width, height = input$export_height, 
             dpi = input$export_dpi, units = "cm")
    }
  )
  
  output$dl_jpeg <- downloadHandler(
    filename = function() { paste0("clip-art-plot-", Sys.Date(), ".jpg") },
    content = function(file) {
      ggsave(file, plot = final_plot(), device = "jpeg", 
             width = input$export_width, height = input$export_height, 
             dpi = input$export_dpi, units = "cm")
    }
  )
  
  output$dl_pdf <- downloadHandler(
    filename = function() { paste0("clip-art-plot-", Sys.Date(), ".pdf") },
    content = function(file) {
      ggsave(file, plot = final_plot(), device = "pdf", 
             width = input$export_width, height = input$export_height,
             units = "cm")
    }
  )
  
  output$dl_svg <- downloadHandler(
    filename = function() { paste0("clip-art-plot-", Sys.Date(), ".svg") },
    content = function(file) {
      ggsave(file, plot = final_plot(), device = "svg", 
             width = input$export_width, height = input$export_height,
             units = "cm")
    }
  )
}


shinyApp(ui, server)