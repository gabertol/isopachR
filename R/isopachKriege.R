#' Isopachs with Kriege methods
#'
#' @param variables
#'
#' @return a raster map of isopachs
#' @export
#'
#' @examples
#' isopachKriege(XXXX)
#'

isopachKriege <- function(database,
                          X_name="X",
                          Y_name="Y",
                          well_name="well",
                          thick_name="thick",
                          crs=4326,
                          bbox_dir=NULL,
                          DIR=NULL,
                          cell_size=5000) {
# Column names


  BD<-
    database %>%
    st_as_sf(coords=c("X","Y"),crs=crs) %>%
    st_transform(crs=3857) %>%
    cbind(., st_coordinates(.)) %>%
    st_drop_geometry() %>%
    dplyr::select(X=X_name,
                  Y=Y_name,
                  thick=thick_name,
                  well=well_name)

# convert to SP

BD_SP<-as(BD %>% st_as_sf(coords=c("X","Y"),crs=3857), "Spatial")

# Bounding box

if(isTRUE(is.character(bbox_dir))){

  bbox<-read_sf(bbox_dir) %>%
    st_transform(crs=3857) %>%
    st_union() %>%
    st_convex_hull()
}
else{

  bbox<-BD %>%
    st_as_sf(coords=c("X","Y"),crs=3857) %>%
    st_union() %>%
    st_convex_hull()

}

# Grid

grd_sf <- bbox %>%
  st_bbox() %>%
  st_as_sfc() %>%
  st_make_grid(
    cellsize = c(cell_size, cell_size),
    what = "centers"
  ) %>%
  st_as_sf() %>%
  cbind(., st_coordinates(.))

grd <- as(grd_sf, "Spatial") # converting to {sp} format
gridded(grd) <- TRUE             # informing the object that it is a grid
grd <- as(grd, "SpatialPixels") # specifying what kind of grid



# Geostatistics
kriege<-automap::autoKrige(thick~1,BD_SP,grd)$krige_output %>%
  st_as_sf() %>%
  st_intersection(.,bbox) %>%
  dplyr::select(var1.pred) %>%
  cbind(.,st_coordinates(.)) #%>%
  #dplyr::select(X,Y,Z=var1.pred) %>%
#  st_transform(crs=4326) %>%
 # st_drop_geometry() #%>%
  #terra::rast(.,type="xyz")



# ISO1<-raster::raster(raster)
#
# crs(ISO1)<-CRS('+init=EPSG:3857')
# ISO2<-projectRaster(ISO1, crs = 4326)
# KKK<-rasterToPoints(ISO2) %>% as_tibble() %>% dplyr::select(X=x,Y=y,Z=3)


# Saving raster

if(isTRUE(is.character(DIR))){

  terra::writeRaster(kriege,DIR,overwrite=TRUE)
}
else{}

# return
return(kriege)

}
