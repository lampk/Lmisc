extract_task <- function(file){
  tmp <- readLines(file, warn = FALSE)
  return(tmp[grepl(pattern = "[:][[:alnum:]_]", x = tmp)])
}

extract_todo <- function(object) {
  tmp <- grep(pattern = ":TODO:", x = object)
  tmp2 <- rep(1:length(tmp), times = diff(c(tmp, length(object) + 1)))
  if (tmp[1] > 1){object <- object[-c(1:(tmp[1] - 1))]}
  return(split(x = object, f = tmp2))
}

extract_info <- function(object,
                         info = c("proj_tags", "proj_close", "TODO", "info", "tags",
                                  "personnel", "cost", "deadline", "priority", "start", "end")) {
  pattern <- paste(":", info, ":", sep = "")
  tmp <- grepl(pattern = pattern, x = object)
  if (any(tmp)) {
    out <- gsub("^\\s+|\\s+$", "", gsub(pattern = pattern, replacement = "", x = object[tmp]))
  } else {out <- ""}
  return(out)
}

get_task <- function(file) {
  ## extract information related to task
  all_info <- extract_task(file = file)

  ## get all tasks
  all_task <- extract_todo(all_info)

  ## extract todo
  todo <- sapply(all_task, extract_info, info = "TODO")

  ## extract task info
  todo_info <- sapply(all_task, extract_info, info = "info")

  ## extract tags
  proj_tags <- extract_info(all_info, info = "proj_tags")
  tmp <- paste(proj_tags, sapply(all_task, extract_info, info = "tags"), sep = "|")
  todo_tags <- sapply(tmp, function(x) paste(unique(unlist(strsplit(tmp, split = "[|]"))), collapse = "|"))

  ## extract personnel
  todo_person <- sapply(all_task, extract_info, info = "personnel")

  ## extract cost
  todo_cost <- sapply(all_task, extract_info, info = "cost")

  ## extract deadline
  todo_deadline <- sapply(all_task, extract_info, info = "deadline")

  ## extract priority
  todo_priority <- sapply(all_task, extract_info, info = "priority")

  ## extract task start
  todo_start <- sapply(all_task, extract_info, info = "start")

  ## extract task end
  todo_end <- sapply(all_task, extract_info, info = "end")

  ## output
  return(data.frame(task = todo,
                    info = todo_info,
                    tags = todo_tags,
                    personnel = todo_person,
                    cost = todo_cost,
                    deadline = todo_deadline,
                    priority = todo_priority,
                    start = todo_start,
                    end = todo_end))
}

summarize_task <- function(task_info){
  #browser()
  require(lubridate)
  cutoff <- ymd_hms(Sys.time())
  task_start <- ymd_hm(task_info$start)
  task_end <- ymd_hm(task_info$end)
  task_due <- ymd_hm(task_info$deadline)
  started <- ifelse(!is.na(task_start) & task_start < cutoff, "Yes", "No")
  completed <- ifelse(!is.na(task_end) & task_end < cutoff, "Yes", "No")
  time_left <- paste(round(ifelse(completed == "Yes", difftime(task_due, task_end, units = "day"),
                      difftime(task_due, cutoff, units = "day")), digits = 1), "days")
  status <- ifelse(started == "No", "Not started yet",
                   ifelse(completed == "Yes", "Completed", "Ongoing"))
  ## output
  return(data.frame(task_info,
                    started = started,
                    completed = completed,
                    time_left = time_left,
                    status = status))
}

#' Function to display summary of tasks
#' @export
manage_task_addin <- function() {
  require(shiny)
  require(miniUI)

  ## ui
  ui <- miniPage(
    gadgetTitleBar("Task View",
                   right = miniTitleBarButton("done", "Done", primary = TRUE)),
    miniContentPanel(
      tableOutput("table")
    )
  )

  ## server
  server <- function(input, output, session){
    context <- rstudioapi::getActiveDocumentContext()
    path <- context$path
    task <- get_task(path)
    output$table <- renderTable(summarize_task(task)[, c("task", "tags", "priority", "deadline",
                                             "status", "time_left")])
    ## stop app when click "Done"
    observeEvent(input$done, {
      stopApp()
    })
  }

  ## run
  runGadget(ui, server)
}
