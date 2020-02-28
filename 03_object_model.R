#библиотеки
library(imager)

#конструктор
wall.create <- function(m,n){
     r <- cimg(array(1,c(m,n,1,3)))
     attr(r,"class") <- c("wall",class(r))
     return(r)
}

wall.clean <- function(w){
     w[,,,] <- 1
     attr(w,"class") <- c("wall",class(w))
     return(w)
}

wall.addBar <- function(wall,x,y,w,h,col){
        r <- draw_rect(wall,x,y,x+w,y+h,color=col,filled = T)
        return(r)
}

wall.addPicture <- function(wall,x,y,w,h,b,col){
        
}
        
######ТЕСТЫ#########
ww <- wall.create(100,200)
ww <- wall.clean(ww)
ww <- wall.addBar(ww,12,12,70,70,col="red")
save.image(ww,"test.jpg")
