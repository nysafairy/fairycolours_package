##Based on:
##https://drsimonj.svbtle.com/creating-corporate-colour-palettes-for-ggplot2

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

##Create function to extract fairy colours as hex codes
##The (...) allows the function to take arguments that weren't pre-defined
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
  `green`    =fairycolours("green", "mint")
)

##Create function to access these new palettes, "primary" as default
##Boolean condition to reverse
fairypal <- function(palette = "primary", reverse = FALSE, ...) {
  pal <- fairypalettes[[palette]]
  if (reverse) pal <- rev(pal)
  colorRampPalette(pal, ...)
}

##Return function to interpolate palette colours to create shades between originals
fairypal("secondary")
##To see secondary palette with 4 original colours and 3 additional shades
fairypal("secondary")(7)

##Create functions for colour and fill
scale_colour_fairy <- function(palette = "primary", discrete = TRUE, reverse = FALSE, ...) {
  pal <- fairypal(palette = palette, reverse = reverse)
  
  if (discrete) {
    discrete_scale("colour", paste0("fairy", palette), palette = pal, ...)
  } else {
    scale_color_gradientn(colours = pal(256), ...)
  }
}

scale_fill_fairy <- function(palette = "primary", discrete = TRUE, reverse = FALSE, ...) {
  pal <- fairypal(palette = palette, reverse = reverse)
  
  if (discrete) {
    discrete_scale("fill", paste0("fairy", palette), palette = pal, ...)
  } else {
    scale_fill_gradientn(colours = pal(256), ...)
  }
}

##Test them
library(datasets)
data(iris)

# Color by discrete variable using default palette
ggplot(iris, aes(Sepal.Width, Sepal.Length, color = Species)) +
  geom_point(size = 4) +
  scale_colour_fairy()

# Color by numeric variable with secondary palette
ggplot(iris, aes(Sepal.Width, Sepal.Length, color = Sepal.Length)) +
  geom_point(size = 4, alpha = 1) +
  scale_colour_fairy(discrete = FALSE, palette = "secondary")

# Fill by discrete variable with different palette + remove legend (guide)
data(cars)
ggplot(mpg, aes(manufacturer, fill = manufacturer)) +
  geom_bar(colour="black") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_fairy(palette = "green", guide = "none")






