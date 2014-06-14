rm( list=ls())
library('ripa')
library('png')

reduce <- function(A,rank) {
      #Calculates the SVD
      s <- svd(A)
      
      #Approximate each result of SVD with the given dimension
      u<-as.matrix(s$u[, 1:rank])
      v<-as.matrix(s$v[, 1:rank])
      d<-as.matrix(diag(s$d)[1:rank, 1:rank])
      
      #Create the new approximated matrix
      m <- u%*%d%*%t(v)
      return(m)
}

# read png file
mat <- readPNG("lady.png" , native=F, )
mat <- matrix(mat ,  ncol=dim(mat)[1], nrow=dim(mat)[2])
lady <- imagematrix(mat , type="grey")
leng <- length(svd(lady)$d)
png("image.png"  ,  width = 255, height = 255)
p <-  seq(from=leng , to=10 , by = -(leng/10))
layout(matrix(c(1:10), 5, 2, byrow = TRUE) )
for(i in p){
      m <- reduce(lady , i)
      m <- imagematrix(m)
      plot(m , main=paste("K =",i) )
}

dev.off()
getwd()


