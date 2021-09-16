#02PrepareImages.R
library(R.matlab)
path="H:/Subversion/PRO/MCT/Deeplearning4ObjectDetection/99RawData/data"
setwd(path)
imgNr = readMat("imgNr.mat")
max(imgNr$imgNr)
Objects = readMat("Objects.mat")
str(Objects)

path="H:/Subversion/PRO/MCT/Deeplearning4ObjectDetection/99RawData/imgOutline"
setwd(path)

files=list.files(pattern = ".mat")

V=strsplit(x = files,split  = "_")
Ende=strsplit(sapply(V, "[[",3),split  = "\\.")

ObjectNames=sapply(V, "[[",2)
ObjectNumberOld=sapply(Ende, "[[",1)
mode(ObjectNumberOld)="numeric"

ind=which(!is.finite(ObjectNumberOld))
files[ind]

Anns=lapply(files, function(x){
  path="H:/Subversion/PRO/MCT/Deeplearning4ObjectDetection/99RawData/imgOutline"
  setwd(path)
  return(readMat(x))
})
names(Anns)=ObjectNames
ind=match(ObjectNumberOld,imgNr$imgNr[1,],nomatch = 0)
indRel=which(ind!=0)
Rel_ObjectNumberOld=ObjectNumberOld[indRel]
ind2=match(Rel_ObjectNumberOld,imgNr$imgNr[1,],nomatch = 0)
View(cbind(Rel_ObjectNumberOld,imgNr$imgNr[1,ind2],imgNr$imgNr[2,ind2]))
Anns_Rel=Anns[indRel]

Anns_Re_Files=lapply(Anns_Rel, "[[",1)
Anns_Re_Ploygons=lapply(Anns_Rel, "[[",2)
Anns_Rel_renamed=lapply(1:length(Anns_Rel), function(i,x,y){
  z=paste0("img",y[i],".jpg")
  return(z)
},Anns_Re_Files,imgNr$imgNr[2,ind2])
names(Anns_Rel_renamed)=names(Anns_Rel)

Anns_Re_Ploygons2=list()
for(i in 1:length(Anns_Re_Ploygons)){
  x=Anns_Re_Ploygons[[i]]
  print(i)
  #do nothing if strcutre changes
  #Anns_Re_Ploygons2[i]=list(x)
  try({
  x=unlist(x,recursive = F)
  x=lapply(x, function(x) {
    x=t(x)
  })
  for(j in 1:length(x))
    colnames(x[[j]])=c("X","Y")
    
  
  })
  
  Anns_Re_Ploygons2[i]=list(x)
}

FullInfo=lapply(1:length(Anns_Rel_renamed), function(i,x,y,z){
  #ann=strsplit(x=gsub('([[:upper:]])', ' \\1', z[[i]]), split="[[:upper:]]",perl = T)
  ann=gsub('([[:upper:]])', ' \\1', z[[i]])
  z=list(Filename=x[[i]],Annotation=ann,Polygons=y[[i]])
},Anns_Rel_renamed,Anns_Re_Ploygons2,ObjectNames)

ObjectNamesAndPolygons=FullInfo

ff=sapply(strsplit(unlist(Anns_Rel_renamed),"\\."),"[[",1)

names(ObjectNamesAndPolygons)=paste0(ff,"_",names(Anns_Rel))
  
save(file = "ObjectNamesAndPolygons.rda",ObjectNamesAndPolygons)

##Nun masken ----


library(R.matlab)
path="H:/Subversion/PRO/MCT/Deeplearning4ObjectDetection/99RawData/masks_Part1"
setwd(path)

WomanMask = readMat("mask_01_woman.mat")

files=list.files(pattern = ".mat")

Names=gsub(files,pattern = "mask",replacement = "img")


Names=gsub(Names,pattern = ".mat",replacement = "")

MasksTemp=lapply(files, function(x){
  path="H:/Subversion/PRO/MCT/Deeplearning4ObjectDetection/99RawData/masks_Part1"
  setwd(path)
  return(readMat(x))
})
names(MasksTemp)=Names

Masks=c()
for(i in 1:length(MasksTemp)){
  x=MasksTemp[[i]]
  x=x$mask
  Masks[i]=list(x)
}
names(Masks)=names(MasksTemp)

x=Masks$img_01_woman
image(x[,,1])
mat_sparse <- Matrix::Matrix(x[,,1], sparse=TRUE)
y=as.matrix(mat_sparse)
image(y[,,1])

library(Matrix)

ObjectMasks=c()
for(i in 1:length(Masks)){
  x=Masks[[i]]
  ThirdDim=c()
  if(!is.na(dim(x)[3])){
    for(j in 1:dim(x)[3]){
      ThirdDim[j]=list(Matrix::Matrix(x[,,j], sparse=TRUE))
    }
  }else{
    ThirdDim[1]=list(Matrix::Matrix(x, sparse=TRUE))
  }
  ObjectMasks[i]=list(ThirdDim)
  print(i)
}
names(ObjectMasks)=names(Masks)

ConvertSparseList2Array=function(OneObject){
  templist=c()
  for(i in 1:length(OneObject)){
    templist[i]=list(as.matrix(OneObject[[i]]))
  }
  return(simplify2array(templist))
}

info="Sparse Masks of Objects per Image with Conversation function back to array"
save(file="ObjectMasks.rda",ObjectMasks,ConvertSparseList2Array,info,compress = "xz")

