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

wall.addRect <- function(wall,dx,dy,dw,dh,db,col){

        t <- wall[(dx+db):(dx + dw - db),(dy + db):(dy + dh - db),,]
        r <- draw_rect(wall,dx,dy,dx+dw,dy+dh,color=col,filled = T)
        r[(dx+db):(dx + dw - db),(dy + db):(dy + dh - db),,] <- t

        return(r)
} 
        
######ТЕСТЫ#########
ww <- wall.create(100,200)
ww <- wall.clean(ww)
ww <- wall.addBar(ww,12,15,70,45,col="red")
ww <- wall.addRect(ww,12,35,70,45,3,col="green")
save.image(ww,"test.png")
ww1 <- imsub(ww, x > 10, x < 75, y > 10, y < 75)
