##Based on:
##https://drsimonj.svbtle.com/creating-corporate-colour-palettes-for-ggplot2
##https://ourcodingclub.github.io/tutorials/writing-r-package/
install.packages("tidyverse", repos = "http://cran.us.r-project.org")
library(tidyverse)
library(scales)
library(ggplot2)
theme_set(theme_minimal())

##Create vector attaching colours to names
fairy<-c(
  `light blue`="#C6EAFA",
  `blue`      ="#B1D5FB",
  `lilac`     ="#D9D0F1",
  `pink`      ="#FFC2E2",
  `blush`     ="#FEDFD7",
  `yellow`    ="#FCEDC5",
  `green`     ="#C2FFC8",
  `mint`      ="#B4E4D4"
)

#'Function to extract fairy colours as hex codes
#'
#'@param ... Character names of  fairy colours 
#' @export
fairycolours<-function(...){
  colours<-c(...)
  if(is.null(colours))
    return(fairy)
  fairy[colours]  
}

##Can call function to view the hex codes of colours
fairycolours("blush","yellow")

##Create palettes of various combinations(subroutines) of the colours
fairypalettes<-list(
  `primary`  =fairycolours("blue","pink","yellow","green"),
  `secondary`=fairycolours("light blue", "lilac", "blush","mint"),
  `full`     =fairycolours("light blue","blue","lilac","pink",
                           "blush","yellow","green","mint"),
  `blue`     =fairycolours("light blue", "blue"),
  `pink`     =fairycolours("lilac","pink","blush"),
  `green`    =fairycolours("green", "mint"),
  `contrast1` =fairycolours("pink","mint"),
  `contrast2` =fairycolours("pink","blue"),
  `contrast3` =fairycolours("light blue", "lilac"),
  `contrast4` =fairycolours("blue", "green"),
  `contrast5` =fairycolours("pink","yellow")
)

#'Return function to interpolate a fairy colour palette
#'
#'@param palette Character name of palette in fairy_palettes
#'@param reverse Boolean indicating whether the palette should be reversed
#'@param ... Additional arguments to pass to colorRampPalette()
#' @export
fairypal <- function(palette = "primary", reverse = FALSE, ...) {
  pal <- fairypalettes[[palette]]
  if (reverse) pal <- rev(pal)
  colorRampPalette(pal, ...)
}

#'Colour scale for fairy kei inspired coloured palette
#'
#'Colour refers to point and line colour on scatter/line plots
#'@param palette Choose a palette by character name in fairypalettes
#'@param discrete Boolean indicating whether color aesthetic is discrete or not
#'@param reverse Boolean indicating whether the palette should be reversed
#'@param ... Additional arguments passed to discrete_scale() or scale_color_gradientn(), used respectively when discrete is TRUE or FALSE
#'@examples
#'Colour by discrete variable using default palette
#'ggplot(data, aes(x, y, color = type)) +
#'  geom_point(size = 4) +
#'  scale_colour_fairy()
#'Colour by numeric variable with secondary palette
#'ggplot(data, aes(x, y, color = type)) +
#'  geom_point(size = 4, alpha = 0.8) +
#'  scale_colour_fairy(discrete = FALSE, palette = "secondary")
#' @export
scale_colour_fairy <- function(palette = "primary", discrete = TRUE, reverse = FALSE, ...) {
  pal <- fairypal(palette = palette, reverse = reverse)
  
  if (discrete) {
    discrete_scale("colour", paste0("fairy", palette), palette = pal, ...)
  } else {
    scale_color_gradientn(colours = pal(256), ...)
  }
}

#'Fill scale for fairy kei inspired coloured palette
#'
#'Fill refers to bar or density fill for bar plots
#'@param palette Choose a palette by character name in fairypalettes
#'@param discrete Boolean indicating whether color aesthetic is discrete or not
#'@param reverse Boolean indicating whether the palette should be reversed
#'@param ... Additional arguments passed to discrete_scale() or scale_fill_gradientn(), used respectively when discrete is TRUE or FALSE
#'@examples
#'##Fill by discrete variable with different palette + remove legend (guide)
#'ggplot(data, aes(x, fill = x)) +
#'  geom_bar(colour="black") +
#'  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
#'  scale_fill_fairy(palette = "green", guide = "none")
#' @export
scale_fill_fairy <- function(palette = "primary", discrete = TRUE, reverse = FALSE, ...) {
  pal <- fairypal(palette = palette, reverse = reverse)
  
  if (discrete) {
    discrete_scale("fill", paste0("fairy", palette), palette = pal, ...)
  } else {
    scale_fill_gradientn(colours = pal(256), ...)
  }
}
