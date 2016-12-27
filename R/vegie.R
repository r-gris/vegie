vrtemplate <- function() {
  '<OGRVRTDataSource>
  <OGRVRTLayer name="%s">
  <SrcDataSource>%s</SrcDataSource>
  <SrcSQL>SELECT * FROM %s WHERE FID = %s</SrcSQL>
<FeatureCount>1</FeatureCount>
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

buildVRT <- function(dsn, layer, fidoffset) {
  txt <- vrtemplate()
  sprintf(txt, layer, dsn, layer, fidoffset)
}
#' Build database from legacy format.
#'
#' @param dsn data source name
#' @param layer layer name
#' @param dbfile SQLite file name
#' @param slurp if TRUE read all and write all in one go
#' @param verbose if TRUE report on progress while building DB
#' @fidoffset a hack in case the driver is 1-based (i.e MIF or TAB)
#' @return
#' @export
#'
#' @examples
#' @importFrom rgdal readOGR
#' @importFrom dplyr copy_to src_sqlite
#' @importFrom progress progress_bar
buildDB <- function(dsn, layer, dbfile, fidoffset = 0, slurp = FALSE, verbose = TRUE) {
 db <- dplyr::src_sqlite(dbfile, create = TRUE)
  RSQLite::dbGetQuery(db$con, "PRAGMA synchronous = OFF")
  RSQLite::dbGetQuery(db$con, "PRAGMA journal_mode = OFF")

  ## build VRT
 vrt0 <- buildVRT(dsn, layer)
  ## write temp VRT
  sfile <- writeTmp(vrt0)

  ##info
  info <- rgdal::ogrInfo(dsn, layer)
  pb <- progress_bar$new(total = info$nrows)
  if (verbose) pb$tick(0)

  if (slurp) {
    x <- rgdal::readOGR(dsn, layer)
  } else {
  ## the actual data
  x <- rgdal::readOGR(sfile, layer, verbose = FALSE)
}
  ## decompose to tables
  tabs <- spbabel:::mtable.Spatial(x)
  for (i in seq_along(tabs)) {
    dplyr::copy_to(db, tabs[[i]], name = names(tabs)[i], temporary = FALSE)
  }
  if (slurp) return(db)
  vrt0 <- buildVRT(dsn, layer, fidoffset = fidoffset)
  ## write temp VRT
  sfile <- writeTmp(vrt0)
  ## the actual data
  x <- rgdal::readOGR(sfile, layer, verbose = FALSE)
  meta <- tibble(crs = proj4string(x), layername = layer, dsn = dsn)
  ## decompose to tables
  tabs <- spbabel::map_table(x)
  indexes <- list(list("object_"),
                  list(c("object_", "branch_")),
                  list(c("vertex_", "branch_")),
                  list("x_", "y_", "vertex_"))

  print(names(tabs))
  for (i in seq_along(tabs)) {
    dplyr::copy_to(db, tabs[[i]], name = names(tabs)[i], indexes = indexes[[i]],
                   temporary = FALSE)
  }
  dplyr::copy_to(db, meta, temporary = FALSE)
 # ## do the remaining FIDs
  fid <- 1

  while(TRUE) {
  vrt <- updateFIDText(vrt0, fid)
    sfile <- writeTmp(vrt)
    x <- try(rgdal::readOGR(vrt, layer, verbose = FALSE), silent = TRUE)
    if (inherits(x, "try-error")) break;
    fid <- fid + 1
    # ## decompose to tables
    tabs <- spbabel:::mtable.Spatial(x)
 vrt <- updateFIDText(vrt0, fid + fidoffset)
    sfile <- writeTmp(vrt)

    x <- try(rgdal::readOGR(vrt, layer, verbose = FALSE), silent = TRUE)
#browser()
    #print(x)
    if (inherits(x, "try-error")) break;
    fid <- fid + 1
    # ## decompose to tables
    tabs <- spbabel::map_table(x)
    print(fid)
 for (i in seq_along(tabs)) {

      dplyr::db_insert_into( con = db$con, table = names(tabs)[i], values = tabs[[i]], indexes = indexes[[i]])
    }
    if (verbose) pb$tick()
  }
  db
}
