#' GBNG.to.Lat.Long
#'
#' This function take a GB national Grid reference an return Lat Long
#'
#'
#' @param GBNG Must be a spatial object with the data to plot.
#'
#'
#'
#' @return Latitiude and Longitude
#'
#' @export GBNG.to.Lat.Long
#' @importFrom "magrittr" "set_colnames"
#' @importFrom "graphics" "filled.contour" "par"
#' @importFrom "grDevices" "colorRampPalette"
#' @importFrom "stats" "na.omit"
#' @importFrom "ggplot2" "ggplot" "aes" "geom_point" "geom_line" "fortify" "geom_polygon"
#' @importFrom "rgeos" "plot"
#' @importFrom "rnrfa" "osg_parse"
#' @importFrom "MASS" "kde2d"
#'
      GBNG.to.Lat.Long <- function(GBNG ) {
        # Read previously saved map
        #my.leics <- readRDS(file = "data/my.leics.Rds")
        #my.leics <- fortify(map)

      # Taxon spatial data is a list of grid references

      # If required, remove spaces in the NGR
        GBNG = gsub(" ", "", GBNG, fixed=T)
      # Remove any rows with NA
      data.for.spatial <- na.omit(GBNG)

      # Rough check for sensible Grid reference format
      #rows.with.GR <- grepl("([A-Z]{2})(\\d{4}|\\d{6}|\\d{8}|\\d{10})$",data.for.spatial )
      # Make a list of those rows
      #rows.to.keep <- which(rows.with.GR)
      # Only keep those rows
      #data.for.spatial <- data.for.spatial[rows.to.keep]

      # Convert NGR to easting and northing
      x <-  rnrfa::osg_parse(data.for.spatial)
      x <- na.omit(x)

      coords <- NULL
      coords$x <- x[[1]]
      coords$y <- x[[2]]


      # To convert the list of coords to spatial points
      # Need to take extra care about NAs creeping in
      # Also make sure the coordinates are numeric
      df.coords <- as.data.frame(coords)
      df.coords[[1]] %>% as.numeric
      df.coords[[2]] %>% as.numeric

      # Now create the spatial object
      # We happen to know that the data is UK national grid so use the proj4string = CRS("+init=epsg:27700")
      sp.coords <- sp::SpatialPoints(df.coords, sp::CRS("+init=epsg:27700"))
      # Need to check the proj4string for my.leics. This is WGS84.
      # proj4string(my.leics)
      # So now we reproject the data in WGS84
      sp.coords.84 <- sp::spTransform(sp.coords, sp::CRS("+init=epsg:4326"))
      sp.coord.df <- as.data.frame(sp.coords.84)
      sp.coords.output <- set_colnames(sp.coord.df, c("Long","Lat"))
      #output.coords <- colnames(as.data.frame(sp.coords.84), c("Long","Lat"))
      # Need to plot using the rgeos package otherwise it does not understnd the geospatial object.
      #my.plot <- MASS::kde2d(sp.coords.84@coords[,1], sp.coords.84@coords[,2], n =100)
      #filled.contour(my.plot,color.palette=colorRampPalette(c('white','blue','yellow','red','darkred')))
      #par(new=FALSE)
     # my.plot <- rgeos::plot(map,main=paste("Distribution ",taxon_name, sep=""))
      #my.plot <- rgeos::plot(sp.coords.84, pch = 19, col="red", add=TRUE)



      #my.plot <- ggplot(my.leics, aes(x = my.leics$long , y = my.leics$lat ) ) + geom_polygon(fill = "white", colour = "black")



}


