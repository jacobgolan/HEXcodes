get_colorPal <- function(im, n=x, cs="RGB"){
  #print(cs) 
  library(tidyverse) ## I love ggplot and tidy data.... so this is a must for anything. 
  library(magick) ## Hello magick!!! 
  library(scales) ## I find rescale function so useful!  and i love show_col function :)
  library(imager) ## i don't know how else to convert image to data frame at the moment. 
  tmp <-im %>% image_resize("100") %>% 
    image_quantize(max=n, colorspace=cs) %>%  ## reducing colours! different colorspace gives you different result
    magick2cimg() %>%  ## I'm converting, becauase I want to use as.data.frame function in imager package.
    RGBtoHSV() %>% ## i like sorting colour by hue rather than RGB (red green blue)
    as.data.frame(wide="c") %>%  #3 making it wide makes it easier to output hex colour
    mutate(hex=hsv(rescale(c.1, from=c(0,360)),c.2,c.3),
           hue = c.1,
           sat = c.2,
           value = c.3) %>%
    count(hex, hue, sat,value, sort=T) %>% 
    mutate(colorspace = cs)
  
  return(tmp %>% select(colorspace,hex,hue,sat,value,n)) ## I want data frame as a result.
  
}

