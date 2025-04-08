
server <- function(input, output, session) {
  
  observe({
    req(input$tabs)
    if (input$tabs == "homeTab") {
      js$hideHeader()
    } else {
      js$showHeader()
    }
  })
  
  # Get Clean Data ----------------------------------------------------------------
  
  clean <- importDataServer("dataImport")
  
  # Determine if full sidebar should be shown -------------------------------
  showMenu <- reactiveVal(FALSE)
  dataPresent <- reactive({
    !is.null(clean()) && nrow(clean()) > 0
  })
  observe({
    showMenu(dataPresent())
  })
  output$menu <- renderUI({
    if (dataPresent()) {
      sidebarMenu(
        id = "tabs",
        menuItem("Antibiogram", tabName = "abTab", icon = icon("braille", class = "nav-icon"))
      )
    } else {
      tagList(
        sidebarMenu(id = "tabs"),
        h6(em("Please import or select a data source to access additional tabs."), style = "color: #a7b6d4; margin:25px; text-align: center;")
      )
    }
  })
  
  observeEvent(input$info, {
    showModal(
      modalDialog(
        title = div(
          style = "text-align: center;",
          tags$img(
            src = "logoDark.png",
            height = "100px",
            style = "vertical-align: middle;"
          )
        ),
        switch(
          input$tabs,
          importTab   = includeMarkdown("./Documentation/import.rmd"),
          abTab       = includeMarkdown("./Documentation/antibiogram.rmd"),
          "Documentation Coming Soon."
        ),
        
        easyClose = TRUE
      )
    )
  })
  
  # Antibiogram Page --------------------------------------------------------
  output$antibiogramUI <- renderUI({
    req(clean())
    abPageUI("antibiogramModule", clean())
  })
  
  observe({
    req(clean())
    abPageServer("antibiogramModule", clean())
  })
  
  # Close server ------------------------------------------------------------
}