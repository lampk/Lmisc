################################################################################
# to create project directory
# by: Lam PK
# version: 23 July 2015
################################################################################

#' @export
createProject <- function(dir, structure = c("default", "journalclub", "presentation")){
  ## to create project directory
  ## initial version: 23 July 2015

  ## read project template
  template <- switch(structure,
                     default = file.path(system.file(package = "Lmisc"), "structure", "default.txt"),
                     journalclub = file.path(system.file(package = "Lmisc"), "structure", "journalclub.txt"),
                     presentation = file.path(system.file(package = "Lmisc"), "structure", "presentation.txt"))
  .str <- readLines(template)

  ## create folder structure
  sapply(.str, FUN= function(x){dir.create(path = file.path(dir, x), recursive = TRUE)})

  # inform
  cat(paste("Project", basename(dir), "was created in", dirname(dir), "\n", sep = " "))
}

