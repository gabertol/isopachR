#' Isopachs with inverse distance weight methods
#'
#' @param variables
#'
#' @return
#' @export
#'
#' @examples
isopachKriege <- function(database,
                          X_name="X",
                          Y_name="Y",
                          well_name="well",
                          thick_name="thick",
                          crs=4326,
                          bbox_dir=NULL,
                          DIR=NULL,
                          cell_size=5000,
                          GGPLOT_READY=FALSE) {
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
      st_transform(crs=3857)
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
  kriege<-gstat::idw(thick~1,BD_SP,grd) %>%
    st_as_sf() %>%
    st_intersection(.,bbox) %>% d
    dplyr::select(var1.pred)%>%
    cbind(.,st_coordinates(.)) %>%
    dplyr::select(X,Y,Z=var1.pred) %>%
    st_drop_geometry() %>%
    terra::rast(.,type="xyz")

  # Reconvert to CRS as raster

  ISO<-raster::raster(kriege)
  crs(ISO)<-CRS('+init=EPSG:3857')
  ISO_reproject<-projectRaster(ISO, crs = crs)

  # As dataframe
  RASTER<-rasterToPoints(ISO_reproject) %>%
    as_tibble() %>%
    dplyr::select(X=x,Y=y,Z=3)


  # Saving raster

  if(isTRUE(is.character(DIR))){

    terra::writeRaster(ISO_reproject,DIR,overwrite=TRUE)
  }
  else{
    # return as DF for ggplot
    if(isTRUE(GGPLOT_READY)){
      return(RASTER)
    }
    # return as raster
    else{
      return(ISO_reproject)}

  }
