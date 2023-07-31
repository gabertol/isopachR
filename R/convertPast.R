#' convert to past
#'
#' @param database a dataframe containing X and Y coordinates.
#' @param crs coordinate reference system epsg code (eg. 4326 for WGS84)
#' @param Y name of the column containing latitude as character (default = "Y")
#' @param X name of the column containing longitude (default = "Y")
#' @param age the age to convert coordinates
#'
#' @return
#' @export
#'
#' @examples
#' convertPast(DF,
#' 4326,
#' Y,
#' X,
#' 125)
#'
convertPast <- function(database,
                 crs=4326,
                 latitude="Y",
                 longitude="X",
                 age) {

# Preparing
  df_1<- database %>%
    dplyr::select(longitude,
                  latitude)

# Aux names
X_name<-paste("paleolong_",age,sep="")
Y_name<-paste("paleolat_",age,sep="")

# convert to past

    BD_1_coor <- df_1 %>%
     dplyr::select(X,Y) %>%
     st_as_sf(coords=c("X","Y"),crs=crs) %>%
     st_transform(crs=4326) %>%
     cbind(., st_coordinates(.)) %>%
     st_drop_geometry() %>%
     round(.,digits=2) %>%
     reconstruct(age=age) %>%
     as_tibble() %>%
     st_as_sf(coords=c("paleolong","paleolat"),crs=4326) %>%
     st_transform(crs=crs) %>%
     cbind(., st_coordinates(.)) %>%
     st_drop_geometry() %>%
     dplyr::select(!!X_name:="X",
                   !!Y_name:="Y")

# Append

    return(cbind(database,BD_1_coor))

}
