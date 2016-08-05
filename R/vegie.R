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
  gsub("FID = 0", sprintf("FID = %i", as.integer(fid)), x)
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
# @importFrom spbabel mtable
buildDB <- function(dsn, layer, dbfile) {
  db <- dplyr::src_sqlite(dbfile, create = TRUE)
  ## build VRT
  vrt <- buildVRT(dsn, layer)
  ## write temp VRT
  sfile <- writeTmp(vrt)
  ## the actual data
  x <- rgdal::readOGR(sfile, layer, verbose = FALSE)

  ## decompose to tables
  tabs <- spbabel:::mtable.Spatial(x)
  indexes <- list(list("object_"),
                  list(c("object_", "branch_")),
                  list(c("vertex_", "branch_")),
                  list("x_", "y_", "vertex_"))

  print(names(tabs))
  for (i in seq_along(tabs)) {
    dplyr::copy_to(db, tabs[[i]], name = names(tabs)[i], indexes = indexes[[i]],
                   temporary = FALSE)
  }
  # ## do the remaining FIDs
  fid <- 1
  while(TRUE) {
    vrt <- updateFIDText(vrt, fid)
    sfile <- writeTmp(vrt)
    x <- try(rgdal::readOGR(vrt, layer, verbose = FALSE), silent = FALSE)
    if (inherits(x, "try-error")) break;
    fid <- fid + 1
    # ## decompose to tables
    tabs <- spbabel:::mtable.Spatial(x)
    print(fid)
    for (i in seq_along(tabs)) {

      dplyr::db_insert_into( con = db$con, table = names(tabs)[i], values = tabs[[i]], indexes = indexes[[i]])
    }
  }
  db
}
