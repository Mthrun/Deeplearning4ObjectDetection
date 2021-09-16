#01InvestigatePcitures.R
library(R.matlab)
path="H:/Subversion/PRO/MCT/Deeplearning4ObjectDetection/99RawData/data"
setwd(path)
imgNr = readMat("imgNr.mat")
max(imgNr$imgNr)
Objects = readMat("Objects.mat")

path="H:/Subversion/PRO/MCT/Deeplearning4ObjectDetection/99RawData/imgOutline"
setwd(path)

ann_woman_048 = readMat("ann_woman_048.mat")

path="H:/Subversion/PRO/MCT/Deeplearning4ObjectDetection/99RawData/img_Part1"
setwd(path)

library(imager)
img=imager::load.image("img1.jpg")
plot(img)
dim(img)
xy=t(ann_woman_048$pointList[[1]][[1]])
xy[,2]=xy[,2]/1024*512 
xy[,1]=xy[,1]/768*384   
lines(xy,lwd=2)

devtools::install_github("bnosac/image", subdir = "image.darknet")
library(image.darknet)

yolo_tiny_voc <- image_darknet_model(type = 'detect', 
                                     model = "tiny-yolo-voc.cfg", 
                                     weights = system.file(package="image.darknet", "models", "tiny-yolo-voc.weights"), 
                                     labels = system.file(package="image.darknet", "include", "darknet", "data", "voc.names"))

f <- system.file("include", "darknet", "data", "dog.jpg", package="image.darknet")

file="H:/Subversion/PRO/MCT/Deeplearning4ObjectDetection/99RawData/img_Part2/img1.jpg"
notfilled <- image_darknet_detect(file = file, 
                                  object = yolo_tiny_voc,
                                  threshold = 0.19)

file="H:/Subversion/PRO/MCT/Deeplearning4ObjectDetection/99RawData/img_Part2/img2.jpg"
notfilled <- image_darknet_detect(file = file, 
                          object = yolo_tiny_voc,
                          threshold = 0.19)

file="H:/Subversion/PRO/MCT/Deeplearning4ObjectDetection/99RawData/img_Part1/cww/img2.png"
notfilled <- image_darknet_detect(file = file, 
                                  object = yolo_tiny_voc,
                                  threshold = 0.19)


file="H:/Subversion/PRO/MCT/Deeplearning4ObjectDetection/99RawData/img_Part2/img41.jpg"
notfilled <- image_darknet_detect(file = file, 
                                  object = yolo_tiny_voc,
                                  threshold = 0.19)

file="H:/Subversion/PRO/MCT/Deeplearning4ObjectDetection/99RawData/img_Part2/cw/img41.png"
notfilled <- image_darknet_detect(file = file, 
                                  object = yolo_tiny_voc,
                                  threshold = 0.19)


InDirectory="H:/Subversion/PRO/MCT/Deeplearning4ObjectDetection/99RawData/img_Part2/"
Filename="img1.jpg"
OutDirectory="H:/Subversion/PRO/MCT/Deeplearning4ObjectDetection/01Predictions/"

PredictObject_YOLO=function(Filename,InDirectory,Darknet_Model,Threshold=0.19,OutDirectory=getwd(),PlotIt=FALSE){
  requireNamespace("image.darknet")
  
  if(missing(Darknet_Model))
    Darknet_Model <- image.darknet::image_darknet_model(type = 'detect', 
                                         model = "tiny-yolo-voc.cfg", 
                                         weights = system.file(package="image.darknet", "models", "tiny-yolo-voc.weights"), 
                                         labels = system.file(package="image.darknet", "include", "darknet", "data", "voc.names"))

  curpath=getwd()
  file=paste0(InDirectory,Filename)
  if(!file.exists(file)){
    txt=paste0("PredictObject_YOLO: file does not exist:",file)
    warning(txt)
    return(txt)
  }

  setwd(OutDirectory)

  notfilled <- image.darknet::image_darknet_detect(file = file, 
                                    object = Darknet_Model,
                                    threshold = Threshold)

  setwd(curpath)
  
  filepred=paste0(OutDirectory,"predictions.png")
  print(filepred)
  img_cur=imager::load.image(filepred)

  if(isTRUE(PlotIt)){

    plot(img_cur)
  }
  return(PredictedImage=img_cur)
  
}

result=PredictObject_YOLO(Filename,InDirectory,OutDirectory = OutDirectory,PlotIt = T)

result=PredictObject_YOLO("img41.jpg",InDirectory,OutDirectory = OutDirectory,PlotIt = T)
InDirectory="H:/Subversion/PRO/MCT/Deeplearning4ObjectDetection/09Originale/img_Shuffled/"

result=PredictObject_YOLO("img2.jpg",InDirectory,OutDirectory = OutDirectory,PlotIt = T)

result=PredictObject_YOLO("cww/img2.png",InDirectory,OutDirectory = OutDirectory,PlotIt = T)
