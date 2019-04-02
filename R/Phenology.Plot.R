#' Phenology.Plot
#'
#' Plots a phenology chart for a taxon. If more than one point is supplied
#' the a circular density plot is generated
#'
#' @param taxon_name Name used as the taxon the label the chart.
#' @param taxon_data A vector of day of year numbers. Can be strings.
#'
#'
#' @return A chart object
#'
#'
#' @export Phenology.Plot
#'
#' @importFrom  magrittr %>%
#' @importFrom grDevices  "dev.off" "graphics.off" "png"
#' @importFrom "stats" "density"
#' @importFrom "graphics" "axis" "plot" "polygon"
#' @importFrom "stats" "density"
#' @importFrom circular density.circular circular
#'
#'
#'
#'
Phenology.Plot <- function(taxon_name,taxon_data ) {
  # Now generate a density plot for the current taxon
  # Check for 1 to 3 digits in taxon_data
  rows.with.DOY <- grepl("[0-9]{1,3}",taxon_data )
  # Make a list of those rows
  rows.to.keep <- which(rows.with.DOY)
  # Only keep those rows
  taxon_data <- taxon_data[rows.to.keep]

  # Now continue to generate plot.


  # The month labels are left here for clarity
  bandwidth <- 1

  # Add a test option. If full name is test_function go into test mode and produce a chart.
 # if(full_name != "test_function" ) {png(taxon_name)}
  #png(full_name)
  x_labels <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")

  # If there is only one row of data we cannot do a circular density plot
  if (length(taxon_data) == 1) {
    #if(full_name != "test_function" ) {png(taxon_name)}
    my.plot <- density(as.numeric(taxon_data), bw=bandwidth)
    plot(my.plot,xlim=c(1, 366), main=paste("Phenology ",taxon_name, sep=""), xlab= "Month", xaxt="n", yaxs = "i", xaxs="i")
    #polygon(my.plot, col="red")
    x_coords <- my.plot["x"] %>% unlist %>% as.numeric %>% append(360)
    y_coords <- my.plot["y"] %>% unlist %>% as.numeric %>% append(0)
    polygon(c(0,x_coords),c(0,y_coords), col="red")
    axis(1,at=c(1:12)*30.5-25, labels = x_labels )

    #if(full_name != "test_function" ) {dev.off()}

  } else {
    #if(full_name != "test_function" ) {png(taxon_name)}
    my.circular.data <- circular(as.numeric(taxon_data)*360/366, units="degrees")
    my.plot <- density.circular(my.circular.data, bw= bandwidth*100, units="degrees")


    plot(my.plot, plot.type = "line", join = TRUE, xlim=c(0, 360), main=paste("Phenology ",taxon_name, sep=""), xaxt="n", xlab= "Month",yaxs = "i", xaxs="i")
    x_coords <- my.plot["x"] %>% unlist %>% as.numeric %>% append(360)
    y_coords <- my.plot["y"] %>% unlist %>% as.numeric %>% append(0)
    polygon(c(0,x_coords),c(0,y_coords), col="red")
    axis(1,at=c(1:12)*30.5-30, labels = x_labels )
    #if(full_name != "test_function" ) {dev.off()}
  }

  #if(full_name != "test_function" ) {graphics.off()}
 my.plot
}
