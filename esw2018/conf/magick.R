
#install.packages("magick")
#install.packages("rsvg")

library(magick)

tiger <- image_read_svg('http://jeroen.github.io/images/tiger.svg', width = 400)
print(tiger)

#image_write(tiger, path = "tiger.png", format = "png")

tiger_png <- image_convert(tiger, "png")
image_info(tiger_png)

frink <- image_read("https://jeroen.github.io/images/frink.png")
print(frink)

# Add 20px left/right and 10px top/bottom
image_border(image_background(frink, "hotpink"), "#000080", "20x10")
