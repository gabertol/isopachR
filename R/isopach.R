#' Isopach via kriege or inverse distance weight methods of spatial intepolation
#'
#' @param a dataframe
#' @param method methods of spatial interpolation, kriege as default. Currently includes: kriege and idw (inverse distance weight)
#' @param X_name name of the column containing longitude coordinates
#' @param Y_name name of the column containing latitude coordinates
#' @param well_name name of a particular well or sample
#' @param thick_name name of the column containing a thickness or other feature to interpolate
#' @param crs coordinate reference system of the coordinates and final output
#' @param bbox_dir direction of the bounding box (shapefile) used for the grid size and croping the final output. (Empty means using the limits of the dataset to create the grid)
#' @param DIR direction to save a tif of the output (empty means no saving and returning as dataframe ou raster (see GGPLOT_READY))
#' @param cell_size the size of each cell for the grid in m (default=5000)
#' @param GGPLOT_READY a logical gate for the return, TRUE means a raster and the default FALSE means a dataframe data which runs smoother for ggplots
#'
#' @return an spatial interpolation as raster or dataframe or tiff file
#' @export
#'
#' @examples
isopach <- function(database,
                    method="kriege",
                          X_name="X",
                          Y_name="Y",
                          well_name="well",
                          thick_name="thick",
                          crs=4326,
                          bbox_dir=NULL,
                          DIR=NULL,
                          cell_size=5000,
                          GGPLOT_READY=FALSE) {# Column names


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
      what = "centers") %>%
    st_as_sf() %>%
    cbind(., st_coordinates(.))

  grd <- as(grd_sf, "Spatial") # converting to {sp} format
  gridded(grd) <- TRUE             # informing the object that it is a grid
  grd <- as(grd, "SpatialPixels") # specifying what kind of grid



  # Geostatistics


  if(method=="kriege"){
    interpolator<-automap::autoKrige(thick~1,BD_SP,grd)$krige_output
  }
  else{
    interpolator<-gstat::idw(thick~1,BD_SP,grd)
  }

interpolation<-interpolator %>%
    st_as_sf() %>%
    st_intersection(.,bbox) %>%
    dplyr::select(var1.pred) %>%
    cbind(.,st_coordinates(.)) %>%
    dplyr::select(X,Y,Z=var1.pred) %>%
    st_drop_geometry() %>%
    terra::rast(.,type="xyz")

  # Reconvert to CRS as raster

  ISO<-raster::raster(interpolation)
  crs(ISO)<-CRS('+init=EPSG:3857')
  ISO_reproject<-projectRaster(ISO, crs = crs)

  # As dataframe
  RASTER<-rasterToPoints(ISO_reproject) %>%
    as_tibble() %>%
    dplyr::select(X=x,Y=y,Z=3)

  # Logical for return

  if(isTRUE(GGPLOT_READY)){
    #as DF for ggplot
    RETURN<-(RASTER)

  }
  else{
    # return as raster
    RETURN<-(ISO_reproject)
  }

  # Return

  if(isTRUE(is.character(DIR))){
    # Saving raster
    terra::writeRaster(ISO_reproject,DIR,overwrite=TRUE)
  }
  else{

    return(RETURN)}

}
