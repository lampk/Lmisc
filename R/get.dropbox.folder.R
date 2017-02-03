################################################################################
# to get Dropbox location
# by: Lam PK, modified from original: http://applyr.blogspot.com/2012/08/get-path-to-your-dropbox-folder.html
# version: 23 July 2015
################################################################################


# misc ------------------------------------------------------

#' @export
get.dropbox.folder <- function(type = c("personal", "business"), file = "info.json") {
  require("rjson")
  db_file <- ifelse(.Platform$OS.type == "windows",
                    file.path(Sys.getenv("LOCALAPPDATA"), "Dropbox", file),
                    file.path("~", ".dropbox", file))

  tmp <- fromJSON(file = db_file)
  if (length(type) > 1) type <- "personal"
  out <- switch(type,
                "personal" = tmp$personal$path,
                "business" = tmp$business$path)
  return(out)
}

# get.dropbox.folder <- function() {
#   ## to get Dropbox location
#   ## modified from original: http://applyr.blogspot.com/2012/08/get-path-to-your-dropbox-folder.html
#   ## initial version: 23 July 2015
#
#   if (requireNamespace("RCurl", quietly = TRUE)) {
#     ## get path to host.db file
#     db.file <- ifelse(.Platform$OS.type == "windows",
#                       file.path(Sys.getenv("APPDATA"), "Dropbox", "host.db"),
#                       file.path(Sys.getenv("HOME"), ".dropbox", "host.db"))
#
#     ## get Dropbox path
#     base64coded <- readLines(db.file, warn = FALSE)[2]
#     RCurl::base64(base64coded, encode = FALSE)
#   } else {
#     stop ("You need to install RCurl package.")
#   }
# }