if (!require("jpeg")) install.packages("jpeg", repos="http://cran.us.r-project.org")
if (!require("imager")) install.packages("imager", repos="http://cran.us.r-project.org")


library('imager')
require(jpeg)
require(ReadImages)

library(png)
library(grid)


rootfolder <- setwd("/home/Project/fromOthers/Reference/it21/Preprocessing/Pics")
#rootfolder <- setwd("/home/Project/fromOthers/Reference/it21/Preprocessing/Pics")
getwd()

#Show the original image
im <- load.image("African_Grey_Parrot-macro2.jpg")
plot(im)

#Blurry
im.blurry <- isoblur(im,10) #Blurry parrots!
plot(im.blurry)

#Edge

layout(t(1:2))

im.xedges <- deriche(im,2,order=2,axis="x") #Edge detector along x-axis
plot(im.xedges)

im.yedges <- deriche(im,2,order=2,axis="y") #Edge detector along x-axis
plot(im.yedges)



thmb <- resize(im,round(width(im)/10),round(height(im)/10))
plot(thmb,main="Thumbnail") #Pixellated parrots

thmb <- resize(im,-10,-10)

imrotate(im,30) %>% plot(main="Rotating")

imshift(im,40,20) %>% plot(main="Shifting")
imshift(im,100,100,boundary=1) %>% plot(main="Shifting (Neumann boundaries)")

#Filtering

A = matrix( 
 c(1, 0, -1,  0, 1, 0, -1, 0, 1), # the data elements 
 nrow=3,              # number of rows 
 ncol=3,              # number of columns 
 byrow = TRUE) 

B = matrix( 
  c(-50, -50, -50,  -50, -50, 
    -50, -50, -50,  -50, -50,
    100, 100, 100, 100, 100,
    -50, -50, -50,  -50, -50,
    -50, -50,-50, -50, -50), # the data elements 
  nrow=5,              # number of rows 
  ncol=5,              # number of columns 
  byrow = TRUE) 

flt <- as.cimg(B) #4x4 box filter
#flt <- as.cimg(matrix(1,4,4)) #4x4 box filter

grayscale(im) %>% correlate(flt) %>% plot(main="Filtering with box filter")



layout(t(1:3))
threshold(im,"20%") %>% plot
threshold(im,"15%") %>% plot
threshold(im,"10%") %>% plot

#morphological:
#http://dahtah.github.io/imager/morphology.html


im <- load.image("Leonardo_flight_of_bird.jpg")
plot(im)

im.g <- grayscale(im)
plot(im.g)

layout(t(1:3))
threshold(im.g,"20%") %>% plot
threshold(im.g,"15%") %>% plot
threshold(im.g,"10%") %>% plot

df <- as.data.frame(im.g)
head(df,5)

m <- lm(value ~ x + y,data=df) #linear trend
summary(m)

#We can extract fitted values to remove the luminance trend:
layout(t(1:2))
im.f <- im.g-fitted(m)
plot(im.g,main="Before")
plot(im.f,main="After trend removal")

layout(t(1:3))
l_ply(paste0(c(20,15,10),"%"),function(v) threshold(im.f,v) %>% plot(main=v))

im.t <- threshold(im.f,"16%")

layout(t(1:2))
plot(im.t,main="Before")
mclosing_square(im.t,4) %>% plot(main="After morphological closing")
# closing = dilation + erosion