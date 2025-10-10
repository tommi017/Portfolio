library(shiny)
library(reactable)
library(openxlsx)
library(shinyjs)
library(RSQLite)
library(DBI)
library(jsonlite)
library(readr)

# Connect to the SQLite database (or create it if it doesn't exist)
conn <- dbConnect(RSQLite::SQLite(), "code_groups.db")

# Create a table to store code groups
dbExecute(conn, "
  CREATE TABLE IF NOT EXISTS code_groups (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    name TEXT NOT NULL,
    description TEXT,
    codes TEXT NOT NULL
  )
")

groups <- dbGetQuery(conn, "SELECT * FROM code_groups")


# Close the connection
dbDisconnect(conn)



# Expanded sample data frame with ICD-10 codes
 icd_codes <- read_csv("icd10_dict.csv")


ui <- fluidPage(
  useShinyjs(),  
  titlePanel("Codelist Manager"),
  
  sidebarLayout(
    sidebarPanel(
      textInput("group_name", "Group Name"),
      textInput("group_description", "Group Description"),
      actionButton("create_group", "Create New Group"),
      
      hr(),
      h4("Saved Groups"),
      selectInput("group_select", "Select Group", choices = NULL),
      uiOutput("selected_group_details"),
      actionButton("edit_group", "Edit Group"),
      actionButton("update_group", "Update Group"),
      actionButton("delete_group", "Delete Group"),
      
      hr(),
      h4("Export Codelist"),
      actionButton("add_to_export", "Add to Export List"),
      uiOutput("export_list"),
      downloadButton("download_export", "Download Export List"),
      
      hr(),
      h4("Import Codelist"),
      fileInput("file_upload", "Upload Excel File", accept = c(".xlsx")),
      actionButton("import_file", "Import File")
      
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("ICD Table", reactableOutput("icd_table")),
        tabPanel("Selected Codes", reactableOutput("selected_table"))
      )
      ,verbatimTextOutput("testing")
    )
  
  )
)




server <- function(input, output, session) {
  data <- reactive({
    icd_codes
  })
  
  groups_data <- reactiveVal(data.frame(groups))
  
  update_groups_data <- function() {
    conn <- dbConnect(RSQLite::SQLite(), "code_groups.db")
    groups <- dbGetQuery(conn, "SELECT * FROM code_groups")
    dbDisconnect(conn)
    groups_data(groups)
  }
  
  output$icd_table <- renderReactable({
    reactable(data(),
              searchable = TRUE,
              filterable = TRUE,
              selection = "multiple",
              theme = reactableTheme(
                rowSelectedStyle = list(backgroundColor = "#eee", boxShadow = "inset 2px 0 0 0 #ffa62d")
              ),
              onClick = "select",
              defaultPageSize = 100,
              striped= TRUE,
              highlight = TRUE,
              columns = list(
                CODE=colDef(name="CODE")
                ,DESCRIPTION=colDef(name="DESCRIPTION")
                ,TYPE=colDef(name="TYPE")
              )
              
    )
  })  
  
  
  selected_data <- reactive({
    selected_rows <- getReactableState("icd_table", "selected")
    icd_codes[selected_rows, ]
  })
  
  
  output$selected_table <- renderReactable({
    reactable(selected_data(),
              searchable = FALSE
    )
  })
  
  output$testing <- renderPrint({
    list(
      selected = getReactableState("icd_table", "selected")
  # ,
  #     groups = groups_data(),
  #     export = export_list(),
  #     codes_to_export = 
  #     all_codes <- lapply(export_list()$codes, function(codes_json) {
  #       codes <- jsonlite::fromJSON(codes_json)
  #       codes
      # })
    )
  })
  
  observe({
    groups <- groups_data()
    updateSelectInput(session, "group_select", choices = setNames(groups$id, groups$description))
  })
  
  output$selected_group_details <- renderUI({
    req(input$group_select)
    groups <- groups_data()
    selected_group <- groups[groups$id == input$group_select, ]

    tagList(
      h4("Group Details"),
      p(strong("Name: "), selected_group$name),
      p(strong("Description: "), selected_group$description)
    )
  })
  
  observeEvent(input$create_group, {
    group_name <- input$group_name
    group_description <- input$group_description
    
    # Check for duplicate group names
    groups <- groups_data()
    if (group_name %in% groups$name) {
      showNotification("Group name already exists. Please choose a different name.", type = "error")
      return()
    }
    
    selected_rows <- getReactableState("icd_table", "selected")
    selected_codes <- icd_codes[selected_rows, ]
    
    codes_json <- jsonlite::toJSON(selected_codes)
    
    conn <- dbConnect(RSQLite::SQLite(), "code_groups.db")
    dbExecute(conn, "INSERT INTO code_groups (name, description, codes) VALUES (?, ?, ?)",
              params = list(group_name, group_description, codes_json))
    dbDisconnect(conn)
    
    update_groups_data()  # Trigger reactive update
  })
  
  
  observeEvent(input$edit_group, {
    req(input$group_select)
    groups <- groups_data()
    selected_group <- groups[groups$id == input$group_select, ]
    
    updateTextInput(session, "group_name", value = selected_group$name)
    updateTextInput(session, "group_description", value = selected_group$description)
    
    selected_codes <- jsonlite::fromJSON(selected_group$codes)
    selected_rows <- which(icd_codes$CODE %in% selected_codes$CODE)
    updateReactable("icd_table", selected = selected_rows)
  })
  
  observeEvent(input$update_group, {
    shinyjs::runjs('if (confirm("Are you sure you want to update group?")) { Shiny.setInputValue("update_group_confirmed", true, {priority: "event"}); }')
  })
  
  observeEvent(input$update_group_confirmed, {
    req(input$group_select)
    group_id <- input$group_select
    group_name <- input$group_name
    group_description <- input$group_description
    selected_rows <- getReactableState("icd_table", "selected")
    selected_codes <- icd_codes[selected_rows, ]
    
    codes_json <- jsonlite::toJSON(selected_codes)
    
    conn <- dbConnect(RSQLite::SQLite(), "code_groups.db")
    dbExecute(conn, "UPDATE code_groups SET name = ?, description = ?, codes = ? WHERE id = ?",
              params = list(group_name, group_description, codes_json, group_id))
    dbDisconnect(conn)
    
    update_groups_data()  # Trigger reactive update
  })
  
  
  observeEvent(input$delete_group, {
    req(input$group_select)
    group_id <- input$group_select
    
    conn <- dbConnect(RSQLite::SQLite(), "code_groups.db")
    dbExecute(conn, "DELETE FROM code_groups WHERE id = ?", params = list(group_id))
    dbDisconnect(conn)
    
    update_groups_data()  # Trigger reactive update
  })
  
  export_list <- reactiveVal(data.frame())
  
  observeEvent(input$add_to_export, {
    req(input$group_select)
    groups <- groups_data()
    selected_group <- groups[groups$id == input$group_select, ]
    
    current_export_list <- export_list()
    if (!selected_group$id %in% current_export_list$id) {
      export_list(rbind(current_export_list, selected_group))
    }
  })
  
  output$export_list <- renderUI({
    export_groups <- export_list()
    if (nrow(export_groups) == 0) {
      return(NULL)
    }
    
    tagList(
      h4("Export List"),
      lapply(1:nrow(export_groups), function(i) {
        group <- export_groups[i, ]
        p(strong(group$name))
      })
    )
  })
  
#   output$download_export <- downloadHandler(
#     filename = function() {
#       paste("exported_groups", Sys.Date(), ".xlsx", sep = "")
#     },
#     content = function(file) {
#       export_groups <- export_list()
#       if (nrow(export_groups) == 0) {
#         return(NULL)
#       }
#       
#       all_codes <- unique(unlist(lapply(export_groups$codes, function(codes_json) {
#         codes <- jsonlite::fromJSON(codes_json)
#         codes$code
#       })))
#       
#       export_data <- data.frame(
#         code = all_codes$code,
#         description = sapply(all_codes, function(code) {
#           icd_codes$description[icd_codes$code == code]
#         }),
#         type = all_codes$type
#       )
#       
#       for (i in 1:nrow(export_groups)) {
#         group <- export_groups[i, ]
#         group_codes <- jsonlite::fromJSON(group$codes)$code
#         export_data[[group$name]] <- ifelse(export_data$code %in% group_codes, 1, 0)
#       }
#       
#       write.xlsx(export_data, file)
#     }


  output$download_export <- downloadHandler(
    
    filename = function() {
      paste("exported_groups", Sys.Date(), ".xlsx", sep = "")
    },
    
    content = function(file) {
      
      export_groups <- export_list()
      if (nrow(export_groups) == 0) {
        return(NULL)
      }
      
      # Create a new workbook
      wb <- createWorkbook()
      
      for (i in 1:nrow(export_groups)) {
        # Extract the name and codes
        tab_name <- export_groups$name[i]
        codes_json <- export_groups$codes[i]
        
        # Convert JSON to dataframe
        codes_df <- fromJSON(codes_json, flatten = TRUE)
        
        # Add a new sheet to the workbook
        addWorksheet(wb, tab_name)
        # Write the dataframe to the sheet
        writeData(wb, tab_name, codes_df)
      }
      
      # Save the workbook to the specified file
      saveWorkbook(wb, file, overwrite = TRUE)
      
    }
  )
  
  
  observeEvent(input$import_file, {
    req(input$file_upload)
    
    # Read the uploaded Excel file
    imported_data <- read.xlsx(input$file_upload$datapath)
    
    # Extract the CODE and TYPE variables
    codes <- imported_data$CODE
    types <- imported_data$TYPE
    
    # Identify the group variables (0/1 variables)
    group_vars <- setdiff(names(imported_data), c("CODE", "TYPE"))
    
    conn <- dbConnect(RSQLite::SQLite(), "code_groups.db")
    
    for (group_var in group_vars) {
      group_codes <- imported_data[imported_data[[group_var]] == 1, c("CODE", "TYPE")]
      codes_json <- jsonlite::toJSON(group_codes)
      
      dbExecute(conn, "INSERT INTO code_groups (name, description, codes) VALUES (?, ?, ?)",
                params = list(group_var, paste("Imported group:", group_var), codes_json))
    }
    
    dbDisconnect(conn)
    
    update_groups_data()  # Trigger reactive update
  })
  
  
  
}

shinyApp(ui, server)

