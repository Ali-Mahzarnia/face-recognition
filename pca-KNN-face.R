
# Get the data 

install.packages('RnavGraphImageData')

library('RnavGraphImageData')

data(faces)  # 64 X 64 pixel image  X 400 photos each face maps to column vector of 4096=64*64 
dim(faces)  # 4096 X 400 vector. faces[,1] = vectorized  64 X 64 matrix. 400 faces
head(matrix(faces[,1],64,64))  # the number in each cell represents the intensity of a pixel as a grayscale.


# Using "EBImage"
############################################################
#Install EBImage packages
source("http://bioconductor.org/biocLite.R")

biocLite("EBImage")

faces=(faces-min(faces))/(max(faces)-min(faces))


install.packages('yaml')
library("EBImage")  # once EBImage installed, just call the library
tmp=Image(matrix(faces[,64],64,64,byrow=TRUE)) # lets see picture 64
display(tmp)# if the intensity >1, it considers white
tmp2=normalize(tmp)   # make the intensity between [0,1]
display(tmp2)


######################################
# show the images
######################################
n = dim(faces)[2]
n # number of pcitures
each.img=NULL # tag all pciture together each person has 10 pic and we have 40 people
img_all1=NULL
img_all2 =NULL
count=0
for(i in 1:20){   # 10 photos X 20 people  (40 people in total)
  assign(paste0('img',i),NULL)   # img1 : 10 photos of 1st person
  for(j in 1:10){
    count = count+1
    tmp=normalize(Image(matrix(faces[,count],64,64,byrow=TRUE)))    # load ONE image
    each.img[[count]] = tmp
    assign(paste0('img',i), abind(eval(as.symbol(paste0('img',i))), tmp, along=1))  # combine images for each people // along=1 horizontally, default: vertically
  }
  img_all1 = abind(img_all1, eval(as.symbol(paste0('img',i))))
}
for(i in 1:20){   # 10 photos X 20 people  (40 people in total)
  assign(paste0('img',i+20),NULL)   # img1 : 10 photos of 1st person
  for(j in 1:10){
    count = count+1    # using the count after the above for-loop
    each.img[[count]] = tmp
    tmp=normalize(Image(matrix(faces[,count],64,64,byrow=TRUE)))    # load ONE image
    assign(paste0('img',i+20), abind(eval(as.symbol(paste0('img',i+20))), tmp, along=1))  # combine images for each people // along=1 horizontally, default: vertically
  }
  img_all2 = abind(img_all2, eval(as.symbol(paste0('img',i+20))))
}
img_all = abind(img_all1,img_all2,along=1)
display(img1)
display(img2)
display(img40)
display(img_all1)
display(img_all2)
display(img_all)



# pca
######################################
x=t(as.matrix(faces))
head(x)
center.x = t(t(x) - apply(x,2,mean))
pca.face = prcomp(x)
loadings = pca.face$rotation
pcs = pca.face$x
ppl = rep(1:40, each=10)   # people in different area by pca1 and pca2
plot(pcs[,1:2], pch=as.character(ppl))
point.images = function(tmp, xlab=NULL, ylab=NULL){
  plot(tmp,pch="", xlab=xlab, ylab=ylab)
  n=nrow(tmp)
  offset=apply(tmp,2,sd)/10
  for(i in 1:n){
    rasterImage(each.img[[i]],tmp[i,1]-offset[1],tmp[i,2]-offset[2],tmp[i,1]+offset[1],tmp[i,2]+offset[2])  
  }
}


point.images(pcs[,1:2], xlab="1st PC", ylab="2nd PC")
point.images(pcs[,c(1,3)], xlab="1st PC", ylab="3rd PC")


## Centered Image
display(normalize(Image(matrix(apply(x,2,mean),64,64,byrow=T)))) # mean-face mean of face
display(normalize(Image(matrix(x[50,],64,64,byrow=T)))) # 50th photo
display(normalize(Image(matrix(center.x[50,],64,64,byrow=T))))  # centered image of 50st photo
dim(loadings)

## Reconstruction 
### For the first photo  face[,1]
portion = cumsum(pca.face$sdev^2)/sum(pca.face$sdev^2)
portion
d=which(portion>0.95)[1] # the pc dimention that catches 95 % of cariablity in covariance
d


# 50~~ 100 PCs were Okay to identify a person. and we can chose d=27 which is catching 80 %
# Remember that the original data have 4096 (64 X 64) variables for each photo (observation)

plot(pca.face, type='l', main='Scree Plot')


tmp = pcs[1,1:d] %*% t(loadings[,1:d]) + apply(x,2,mean)  # use first d-PCs + mean face to reconstrcut the first pic in with d eigenfaces
display(normalize(Image(matrix(tmp,64,64,byrow=T)))) 


# reconstruct all ghost or eigenfaces with d number of PCA 
redu.faces=faces
for( i in 1:400){
#i = 1 ## i-th photo
# recustruction using first dims[..] PCs
img.reconst = NULL # Image reconstruction
  tmp = pcs[i,1:d] %*% t(loadings[,1:d]) + apply(x,2,mean)  # use first d-PCs + mean face
  #tmp = normalize(Image(matrix(tmp,64,64,byrow=T)))
  #img.reconst = abind(img.reconst, tmp, along=1)
redu.faces[,i]=t(tmp)
}



#show all reconstructed pciture at once


each.img.redu=NULL
img_all1.redu=NULL
img_all2.redu=NULL
count=0
for(i in 1:20){   # 10 photos X 20 people  (40 people in total)
  assign(paste0('img.redu',i),NULL)   # img1 : 10 photos of 1st person
  for(j in 1:10){
    count = count+1
    tmp=normalize(Image(matrix(redu.faces[,count],64,64,byrow=TRUE)))    # load ONE image
    each.img[[count]] = tmp
    assign(paste0('img.redu',i), abind(eval(as.symbol(paste0('img.redu',i))), tmp, along=1))  # combine images for each people // along=1 horizontally, default: vertically
  }
  img_all1.redu = abind(img_all1.redu, eval(as.symbol(paste0('img.redu',i))))
}
for(i in 1:20){   # 10 photos X 20 people  (40 people in total)
  assign(paste0('img.redu',i+20),NULL)   # img1 : 10 photos of 1st person
  for(j in 1:10){
    count = count+1    # using the count after the above for-loop
    each.img[[count]] = tmp
    tmp=normalize(Image(matrix(redu.faces[,count],64,64,byrow=TRUE)))    # load ONE image
    assign(paste0('img.redu',i+20), abind(eval(as.symbol(paste0('img.redu',i+20))), tmp, along=1))  # combine images for each people // along=1 horizontally, default: vertically
  }
  img_all2.redu = abind(img_all2.redu, eval(as.symbol(paste0('img.redu',i+20))))
}
img_all.redu = abind(img_all1.redu,img_all2.redu,along=1)
display(img.redu1)
display(img.redu2)
display(img.redu40)
display(img_all1.redu)
display(img_all2.redu)
display(img_all.redu)




#### weights of all sample image . each weight is a vector of d we store them to use themk later for euclidian distance  
weight= matrix(data=NA, nrow=d, ncol=400)
for( i in 1:400){
  tmp =t(loadings[,1:d]) %*% t(x[i,]-t(apply(x,2,mean))) # 
  weight[,i]=t(tmp)
}
weight


# test different picture 
# change i for different picture testing

i=75 # see if we can find the pciture i with lower dimension analysis 
temp=x[i,]
display(normalize(Image(matrix(temp,64,64,byrow=T)))) 
new.weight=t(loadings[,1:d]) %*% t(temp-t(apply(x,2,mean))) # use first d-PCs + mean face
new.recons=loadings[,1:d]%*%new.weight+apply(x,2,mean) # this is the other way of reconstruction that can be used for out of sample mapping to space
display(normalize(Image(matrix(new.recons,64,64,byrow=T)))) 
new.weight=t(loadings[,1:d]) %*% t(temp-t(apply(x,2,mean))) # find the weieght of this new pic 
new.weight=unname(new.weight, force = TRUE)
dif=sweep(weight,1,new.weight) # we want to find the minimum euclidian distance btw new face's weight and all other 400 pic and delcare the winner 
dif=dif**2
dif=colSums(dif) # sum squared 
dif
find=as.numeric(which(dif==min(dif))) # which pic has minimum distance weigh with the whight of new face ?
tmp=Image(matrix(faces[,find],64,64,byrow=TRUE)) # let us project the winner here 
tmp2=normalize(tmp)   # make the intensity between [0,1]
display(tmp2)




######## lets change the i for another one 
i=265 # see if we can find the pciture i with lower dimension analysis 
temp=x[i,]
display(normalize(Image(matrix(temp,64,64,byrow=T)))) 
new.weight=t(loadings[,1:d]) %*% t(temp-t(apply(x,2,mean))) # use first d-PCs + mean face
new.recons=loadings[,1:d]%*%new.weight+apply(x,2,mean) # this is the other way of reconstruction that can be used for out of sample mapping to space
display(normalize(Image(matrix(new.recons,64,64,byrow=T)))) 
new.weight=t(loadings[,1:d]) %*% t(temp-t(apply(x,2,mean))) # find the weieght of this new pic 
new.weight=unname(new.weight, force = TRUE)
dif=sweep(weight,1,new.weight) # we want to find the minimum euclidian distance btw new face's weight and all other 400 pic and delcare the winner 
dif=dif**2
dif=colSums(dif) # sum squared 
find=as.numeric(which(dif==min(dif))) # which pic has minimum distance weigh with the whight of new face ?
tmp=Image(matrix(faces[,find],64,64,byrow=TRUE)) # let us project the winner here 
tmp2=normalize(tmp)   # make the intensity between [0,1]
display(tmp2)




#insample missrate 




missrate=0


for  (i in 400 ){
  temp=x[i,]
  temp.weight=t(loadings[,1:d]) %*% t(t(temp)-t(apply(x,2,mean))) 
  temp.weight=unname(temp.weight, force = TRUE)
  dif=sweep(weight,1,temp.weight)
  dif=dif**2
  dif=colSums(dif) 
  find=as.numeric(which(dif==min(dif)))
  if(i!=find) { missrate=missrate+1}
}
missrate/400
#################################

# denormalize the new coming pictyure to the scale of x, the factor is range of x denoted as m
#m=max(x)-min(x)

#install.packages("png")
library(png)
ali<-readPNG('D:/gitprojects/Face recognition via PCA and KNN/ali.PNG')
grid::grid.raster(ali)
dim(ali)

temp=(as.vector(ali))
#temp=temp*m+mean(temp) # denormalize

dim(temp) # just to make sure picture is grayscale and 64*64 
length(temp)
display(normalize(Image(matrix(temp,64,64,byrow=T)))) 

temp.weight=t(loadings[,1:d]) %*% t(temp-t(apply(x,2,mean))) # ali has a weight a column of d 
temp.recons=loadings[,1:d]%*%temp.weight+apply(x,2,mean) # reconstruct ali into space 
display(normalize(Image(matrix(temp.recons,64,64,byrow=T)))) 

temp.weight=unname(temp.weight, force = TRUE)
dif=sweep(weight,1,temp.weight)
dif=dif**2
dif=colSums(dif) 
find=as.numeric(which(dif==min(dif))) # which pic has minimum distance weigh with the whight of new face ?

tmp=Image(matrix(faces[,find],64,64,byrow=TRUE))
tmp2=normalize(tmp)   # make the intensity between [0,1]
tmp1=normalize(Image(matrix(temp.recons,64,64,byrow=T)))
tmp0=normalize(Image(matrix(temp,64,64,byrow=T)))
display(tmp2)
a=rbind(tmp0,tmp1,tmp2)
display(a)


#make it as function

triple <- function(temp) {
  #temp=temp*m+mean(temp) #denormalize
  # dim(temp) # just to make sure picture is grayscale and 64*64 
  #length(temp)
  #display(normalize(Image(matrix(temp,64,64,byrow=T)))) 
  
  temp.weight=t(loadings[,1:d]) %*% t(temp-t(apply(x,2,mean))) # ali has a weight a column of d 
  temp.recons=loadings[,1:d]%*%temp.weight+apply(x,2,mean) # reconstruct ali into space 
  #display(normalize(Image(matrix(temp.recons,64,64,byrow=T)))) 
  
  temp.weight=unname(temp.weight, force = TRUE)
  dif=sweep(weight,1,temp.weight)
  dif=dif**2
  dif=colSums(dif) 
  find=as.numeric(which(dif==min(dif))) # which pic has minimum distance weigh with the whight of new face ?
  
  tmp=Image(matrix(faces[,find],64,64,byrow=TRUE))
  tmp2=normalize(tmp)   # make the intensity between [0,1]
  tmp1=normalize(Image(matrix(temp.recons,64,64,byrow=T)))
  tmp0=normalize(Image(matrix(temp,64,64,byrow=T)))

  
  return(rbind(tmp0,tmp1,tmp2))
}

setwd("D:/gitprojects/Face recognition via PCA and KNN")
#################### set the directory to source file manually
i=1
l= paste0(i,".png")
face<-readPNG(l)
temp=(as.vector(face))
a=triple(temp)
for(i in 2:17){
  l= paste0(i,".png")
face<-readPNG(l)
temp=(as.vector(face))
a=abind(a,eval(triple(temp)))
}
display(a)






# now probability 

# having a picture we count sort the miimum euclidian distance to see who is the most frequent one 
# find 41st nearest face and report the three one are most frequent  among 41

# example last pic 
setwd("D:/gitprojects/Face recognition via PCA and KNN")
#################### set the directory to source file manually

i=1
l= paste0(i,".png")
face<-readPNG(l)
temp=(as.vector(face))
display(triple(temp))
# 
#temp=temp*m+mean(temp) # denormalize
temp.weight=t(loadings[,1:d]) %*% t(temp-t(apply(x,2,mean))) # ali has a weight a column of d 
temp.weight=unname(temp.weight, force = TRUE)
dif=sweep(weight,1,temp.weight)
dif=dif**2
dif=colSums(dif)
#k nearest face
k=41
index_sort=NULL
for (j in 1:k){
a=which(dif==sort(dif,partial=j)[j])
index_sort[j]=a
}
index_sort=floor(index_sort/10)
find_prob=as.numeric(names(sort(summary(as.factor(index_sort)), decreasing=T)[1:3]))
#find_prob=as.numeric(tail(names(sort(table(index_sort))), 3))
first_prob=find_prob[1]*10+1
second_prob=find_prob[2]*10+1
third_prob=find_prob[3]*10+1
tmp0=normalize(Image(matrix(temp,64,64,byrow=T)))
tmp=Image(matrix(faces[,first_prob],64,64,byrow=TRUE))
tmp1=normalize(tmp)   # make the intensity between [0,1]
tmp=Image(matrix(faces[,second_prob],64,64,byrow=TRUE))
tmp2=normalize(tmp)   # make the intensity between [0,1]
tmp=Image(matrix(faces[,third_prob],64,64,byrow=TRUE))
tmp3=normalize(tmp)   # make the intensity between [0,1]

display(rbind(tmp0,tmp1,tmp2,tmp3))


# let us have a function of it 

triple_prob <- function(temp) {
  #temp=temp*m+mean(temp) # denormalized
  temp.weight=t(loadings[,1:d]) %*% t(temp-t(apply(x,2,mean))) # ali has a weight a column of d 
  temp.weight=unname(temp.weight, force = TRUE)
  dif=sweep(weight,1,temp.weight)
  dif=dif**2
  dif=colSums(dif)
  #k nearest face
  k=41
  index_sort=NULL
  for (j in 1:k){
    a=which(dif==sort(dif,partial=j)[j])
    index_sort[j]=a
  }
  index_sort=floor(index_sort/10)
find_prob=as.numeric(names(sort(summary(as.factor(index_sort)), decreasing=T)[1:3]))
  #find_prob=as.numeric(tail(names(sort(table(index_sort))), 3))
    first_prob=find_prob[1]*10+1
  second_prob=find_prob[2]*10+1
  third_prob=find_prob[3]*10+1
  tmp0=normalize(Image(matrix(temp,64,64,byrow=T)))
  tmp=Image(matrix(faces[,first_prob],64,64,byrow=TRUE))
  tmp1=normalize(tmp)   # make the intensity between [0,1]
  tmp=Image(matrix(faces[,second_prob],64,64,byrow=TRUE))
  tmp2=normalize(tmp)   # make the intensity between [0,1]
  tmp=Image(matrix(faces[,third_prob],64,64,byrow=TRUE))
  tmp3=normalize(tmp)   # make the intensity between [0,1]
  return(rbind(tmp0,tmp1,tmp2,tmp3))
}
temp=(as.vector(face))
display(triple_prob(temp))


####### probability result

setwd("D:/gitprojects/Face recognition via PCA and KNN")
#################### set the directory to source file manually

i=1
l= paste0(i,".png")
face<-readPNG(l)
temp=(as.vector(face))
a=triple_prob(temp)
for(i in 2:17){
  l= paste0(i,".png")
  face<-readPNG(l)
  temp=(as.vector(face))
  a=abind(a,eval(triple_prob(temp)))
}
display(a)

