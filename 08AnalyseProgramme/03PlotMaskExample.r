#03PlotMaskExample.r
Disk="H"
path=paste0(gsub("C",Disk,SubversionDirectory()),"PRO/MCT/Deeplearning4ObjectDetection/09Originale")
setwd(path)

img01=jpeg::readJPEG("img/img1.jpg")

load("ObjectMasks.rda")

Woman=ObjectMasks$img_01_woman


WomanArray=ConvertSparseList2Arry(Woman)
plot.new()
rasterImage(WomanArray[,,1],   0, 0, 1, 1)
rasterImage(img01, 0, 0, 1, 1)

WomanArray=ConvertSparseList2Arry(Woman)
plot.new()
WomanArray=abind::abind(WomanArray,WomanArray[,,1],WomanArray[,,1])
WomanArray[,,3]=0.2
WomanArray[which(WomanArray==1,arr.ind = T)]=0.6
rasterImage(img01, 0, 0, 1, 1)
rasterImage(WomanArray,   0, 0, 1, 1)

PlotMaskOnImg=function(Img,Mask,TransparencyFactor=0.6){

  dims=dim(Mask)
  third=dims[3]
  
  #Make Sure that given Mask is four dim array
    if(is.na(third)){
      MaskArray=abind::abind(Mask,Mask,Mask,Mask)
    }
    if(third==1){
      MaskArray=abind::abind(Mask[,,1],Mask[,,1],Mask[,,1],Mask[,,1])
    }
    if(third==2){
      MaskArray=abind::abind(Mask,Mask[,,1],Mask[,,1])
    }
    if(third==3){
      MaskArray=abind::abind(Mask,Mask[,,1])
    }
    if(third>4){
      MaskArray=Mask[,,1:4]
    }

  MaskArray[,,3]=0.2
  MaskArray[which(MaskArray==1,arr.ind = T)]=TransparencyFactor
  plot.new()
  rasterImage(img01, 0, 0, 1, 1)
  rasterImage(MaskArray,   0, 0, 1, 1)
}

ObjectMask=ObjectMasks$img_01_sidewalk
ObjectMaskArray=ConvertSparseList2Arry(ObjectMask)
PlotMaskOnImg(img01,ObjectMaskArray)

##PlotPolygons
Disk="H"
path=paste0(gsub("C",Disk,SubversionDirectory()),"PRO/MCT/Deeplearning4ObjectDetection/09Originale")
setwd(path)

img01=jpeg::readJPEG("img/img1.jpg")
img01_alt2=imager::load.image("img/img1.jpg")

load("ObjectNamesAndPolygons.rda")

plot.new()
rasterImage(img01, 0, 0, 1, 1)
Pols=ObjectNamesAndPolygons$img1_woman$Polygons

#img=imager::load.image("img/img1.jpg")
library(imager)

img01_cobverted=imager::as.cimg(array(dim = c(512,384,3)))
img01_temp=imager::as.cimg(img01)
for(z in 1:3){
  img01_cobverted[,,z]=t(img01_temp[,,z])
}

plot(img01_cobverted)
xy=rbind(Pols[[1]])
#resolution conversion
xy[,2]=xy[,2]/1024*512 
xy[,1]=xy[,1]/768*384   
lines(xy,lwd=5,col="darkred")

plot(img01_alt2)
xy=Pols[[1]]
xy[,2]=xy[,2]/1024*512 
xy[,1]=xy[,1]/768*384   
lines(xy,lwd=2)

