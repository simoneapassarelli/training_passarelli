airtemps <- c(212, 30.3, 78, 32)

# Without functions, you copy and paste a lot


# Insert Roxygen Skeleton here to add documentation that the help file uses
#' Convert temperature data from Fahrenheit to Celsius
#'
#' @param fahr Temperature data in degrees Fahrenheit to be converted
#' @return temperature value in degrees Celsius
#' @keywords conversion
#' @export
#' @examples
#' fahr_to_celsius(32)
#' fahr_to_celsius(c(32, 212, 72))
fahr_to_celsius <- function(fahr) {
  celsius <- (fahr-32)*5/9
  return(celsius)
}


celsius1 <- (airtemps[1] - 32 * 5/9)
celsius2 <- (airtemps[2] - 32 * 5/9)
celsius3 <- (airtemps[3] - 32 * 5/9)

# Make a function that converts farenheit to celsius

fahr_to_celsius <- function(fahr) {
  (fahr - 32) * 5/9
}

celsius4 <- fahr_to_celsius(airtemps[1])
celsius1==celsius4

fahr_to_celsius(airtemps)


# Now try to convert celsius to fahrenheit


cels_to_fahr <- function(cels) {
  (cels * 9/5 + 32)
}

cels_to_fahr(0) #32
fahr_to_celsius(32)
cels_to_fahr(100) #212
fahr_to_celsius(212)


convert_temps <- function(fahr) {
  celsius <- (fahr-32)*5/9
  kelvin <- celsius + 273.15
  return(list(fahr=fahr, celsius=celsius, kelvin=kelvin))
}

temps_df <- data.frame(convert_temps(seq(-100,100,10)))


# Make our own theme for plots

# Define the custom theme
# Color code the points by their value in kelvin
custom_theme <- function(base_size = 9) {
  ggplot2::theme(
    axis.ticks       = ggplot2::element_blank(),
    text             = ggplot2::element_text(family = 'Helvetica', color = 'gray30', size = base_size),
    plot.title       = ggplot2::element_text(size = ggplot2::rel(1.25), hjust = 0.5, face = 'bold'),
    panel.background = ggplot2::element_blank(),
    legend.position  = 'right',
    panel.border     = ggplot2::element_blank(),
    panel.grid.minor = ggplot2::element_blank(),
    panel.grid.major = ggplot2::element_line(colour = 'grey90', size = .25),
    legend.key       = ggplot2::element_rect(colour = NA, fill = NA),
    axis.line        = ggplot2::element_blank()
  )
}

ggplot(temps_df, mapping=aes(x=fahr, y=celsius, color=kelvin)) +
  geom_point() +
  custom_theme(20)  #this changes the text size

# You can also define a graph as a function if you reuse the same types of graphs
scatterplot <- function(df, point_size = 2, font_size=9) {
  ggplot(df, mapping=aes(x=fahr, y=celsius, color=kelvin)) +
    geom_point(size=point_size) +
    custom_theme(font_size)
}

scatterplot(temps_df, point_size=3, font_size = 16)

scatterplot(temps_df, point_size=5, font_size = 20)
