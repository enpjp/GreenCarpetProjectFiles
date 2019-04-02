#' Plot Maps
#'
#' @param taxon_name Name of taxon used in map description.
#' @param taxon_spatial_data Must be a spatial object with the data to plot.
#' @param map A map used as the underlay for the plot.
#'
#'
#' @return A map
#'
#' @export Plot.Maps
#'
#' @importFrom "stats" "na.omit"
#' @importFrom "rgeos" "plot"
#' @importFrom "rnrfa" "osg_parse"
#'
      Plot.Maps <- function(taxon_name,taxon_spatial_data, map ) {
        # Read previously saved map
        #my.leics <- readRDS(file = "data/my.leics.Rds")
        my.leics <- map

      # Taxon spatial data is a list of grid references

      # If required, remove spaces in the NGR
      taxon_spatial_data = gsub(" ", "", taxon_spatial_data, fixed=T)
      # Remove any rows with NA
      data.for.spatial <- na.omit(taxon_spatial_data)

      # Rough check for sensible Grid reference format
      rows.with.GR <- grepl("([A-Z]{2})(\\d{4}|\\d{6}|\\d{8}|\\d{10})$",data.for.spatial )
      # Make a list of those rows
      rows.to.keep <- which(rows.with.GR)
      # Only keep those rows
      data.for.spatial <- data.for.spatial[rows.to.keep]

      # Convert NGR to easting and northing
      x <-  rnrfa::osg_parse(data.for.spatial)
      x <- na.omit(x)

      coords <- NULL
      coords$x <- x[[1]]
      coords$y <- x[[2]]


      # To convert the list of coords to spatial points
      # Need to take extra care about NAs creeping in
      # Also make sure the coordinates are numeric
      df.coords <- as.data.frame(coords) %>% na.omit
      df.coords[[1]] %>% as.numeric
      df.coords[[2]] %>% as.numeric

      # Now create the spatial object
      # We happen to know that the data is UK national grid so use the proj4string = CRS("+init=epsg:27700")
      sp.coords <- sp::SpatialPoints(df.coords, sp::CRS("+init=epsg:27700"))
      # Need to check the proj4string for my.leics. This is WGS84.
      # proj4string(my.leics)
      # So now we reproject the data in WGS84
      sp.coords.84 <- sp::spTransform(sp.coords, sp::CRS("+init=epsg:4326"))

      # Need to plot using the rgeos package otherwise it does not understnd the geospatial object.
      my.plot <- rgeos::plot(map,main=paste("Distribution ",taxon_name, sep=""))
      my.plot <- rgeos::plot(sp.coords.84, pch = 19, col="red", add=TRUE)


}


