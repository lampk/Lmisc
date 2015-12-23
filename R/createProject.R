################################################################################
# to create project directory
# by: Lam PK
# version: 23 July 2015
################################################################################

#' @export
createProject <- function(dir, structure = c("default", "journalclub", "presentation", "proposal")){
  ## to create project directory
  ## initial version: 23 July 2015

  if (length(structure) > 1) {structure <- "default"}

  ## read project template
  template <- switch(structure,
                     default = file.path(system.file(package = "Lmisc"), "structure", "default.txt"),
                     journalclub = file.path(system.file(package = "Lmisc"), "structure", "journalclub.txt"),
                     presentation = file.path(system.file(package = "Lmisc"), "structure", "presentation.txt"),
                     proposal = file.path(system.file(package = "Lmisc"), "structure", "proposal.txt"))
  .str <- readLines(template)

  ## create folder structure
  sapply(.str, FUN= function(x){dir.create(path = file.path(dir, x), recursive = TRUE)})

  # inform
  cat(paste("Project", basename(dir), "was created in", dirname(dir), "\n", sep = " "))
}

#' @export
createProject2 <- function(dir, structure = c("simple_analysis", "OUCRU_RCT", "journal_club", "presentation", "proposal")){
  ## to create project directory
  ## initial version: 23 July 2015

  if (length(structure) > 1) {structure <- "simple_analysis"}

  file.copy(from = file.path(system.file(package = "Lmisc"), "structure", structure),
            to = dir, recursive = TRUE)

  # inform
  cat(paste("Project", basename(dir), "was created in", dirname(dir), "\n", sep = " "))
}

