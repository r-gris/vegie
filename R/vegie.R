vrtemplate <- function() {
  '<OGRVRTDataSource>
  <OGRVRTLayer name="%s">
  <SrcDataSource>%s</SrcDataSource>
  <SrcSQL>SELECT * FROM %s WHERE FID = 0</SrcSQL>
  </OGRVRTLayer>
  </OGRVRTDataSource>'
}

updateFIDText <- function(x, fid = 0) {
  stopifnot(fid >= 0)
  gsub("FID = 0", sprintf("FID = %i", as.integer(fid)))
}

writeTmp <- function(x) {
  tmp <- sprintf("%s.vrt", tempfile())
  writeLines(x, tmp)
  tmp
}

buildVRT <- function(dsn, layer) {
  txt <- vrtemplate()
  sprintf(txt, layer, dsn, layer)
}
#' Build database from legacy format.
#'
#' @param dsn data source name
#' @param layer layer name
#' @param dbfile SQLite file name
#'
#' @return
#' @export
#'
#' @examples
#' @importFrom rgdal readOGR
#' @importFrom dplyr copy_to src_sqlite
#' @importFrom spbabel mtable
buildDB <- function(dsn, layer, dbfile) {
  db <- dplyr::src_sqlite(dbfile, create = TRUE)
  ## build VRT
  vrt <- buildVRT(dsn, layer)
  ## write temp VRT
  sfile <- writeTmp(vrt)
  ## the actual data
  x <- rgdal::readOGR(sfile, layer, verbose = FALSE)

  ## decompose to tables
  tabs <- spbabel::mtable(x)
  for (i in seq_along(tabs)) {
    dplyr::copy_to(db, tabs[[i]], name = names(tabs)[i])
  }
  oldids <- data.frame(name = c("object_", "branch_", "vertex_"),
                     maxval = c(max(tabs$o$object_), max(tabs$b))
  # ## do the remaining FIDs
   fid <- 1
  # while(TRUE) {
     vrt <- updateFIDText(vrt, fid)
     sfile <- writeTmp(vrt)
     x <- try(rgdal::readOGR(vrt, layer, verbose = FALSE), silent = TRUE)
     if (inherits(x), "try-error") break;
  #
  # ## decompose to tables
   tabs <- spbabel::mtable(x)
  # for (i in seq_along(tabs)) {
     tabs <- update_tabs(tabs)
     dplyr::db_insert_into( con = db, table = names(tabs)[i], values = tabs[[i]])
  }

  db
}
