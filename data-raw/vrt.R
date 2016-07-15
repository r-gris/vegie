dsn <- file.path(getOption("default.datadir"), "data/listdata.thelist.tas.gov.au/opendata/data")
layer <- "list_parcels_hobart"
#system(sprintf("ogr2ogr -f VRT -fid 0 data-raw/%s.vrt %s %s", basename(srcfile), dirname(srcfile), basename(srcfile)))

vrtemplate <- '<OGRVRTDataSource>
  <OGRVRTLayer name="%s">
  <SrcDataSource>%s</SrcDataSource>
  <SrcSQL>SELECT * FROM %s WHERE FID = 0</SrcSQL>
  </OGRVRTLayer>
  </OGRVRTDataSource>'

writeLines(sprintf(vrtemplate, layer, dsn, layer), "data-raw/vegie.vrt")
