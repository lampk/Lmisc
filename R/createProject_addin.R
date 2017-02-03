#' @export
createProject_addin <- function() {
  require(shiny)
  require(miniUI)

  ## ui
  ui <- miniPage(
    gadgetTitleBar("Create Project",
                   right = miniTitleBarButton("done", "Done", primary = TRUE)),
    miniContentPanel(
      textInput("path", "Directory:"),
      actionButton("data_browse", "Directory browse"),
      textInput("name", "Project name:"),
      selectInput(inputId = "structure",
                  label = "Structure",
                  choices = c("project", "project_analysis", "simple_analysis",
                              "OUCRU_RCT", "journal_club", "presentation", "proposal"))
    )
  )

  ## server
  server <- function(input, output, session){
    observe({
      if (input$data_browse == 0) return()
      updateTextInput(session, "path",  value = tcltk::tk_choose.dir())
    })

    create_Project <- reactive({
      fullname <- file.path(input$path, input$name)
      code <- paste("createProject2(dir = '", fullname, "', structure = '", input$structure, "')", sep = "")
      eval(parse(text = code))
    })

    ## stop app when click "Done"
    observeEvent(input$done, {
      create_Project()
      stopApp()
    })
  }

  ## run
  runGadget(ui, server)
}
