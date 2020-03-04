#библиотеки
library(imager)

#параметры
fone <- "Data//a38606.jpg"



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

wall.grid.mm <- function(sc) {
        # sc - scale, масштаб
        stX <- 10 #отступ слева
        stY <- 50 #отступ сверху
        s <- 10 #ширина между картинами
        Xgen <- stX+(297+s)*(0:3)
        Ygen <- stY+(297+s)*(0:3)
        m <- expand.grid(Xgen,Ygen)
        m <- sapply(m,function(x){x*sc})
        return(round(m,0))
}

######скрипт########
coords <- wall.grid.mm(800/2500)
ww <- load.image(fone)
ww <- wall.addRect(ww,coords$Var1,coords$Var2,297,210,3,"red")


        
######ТЕСТЫ#########
ww <- wall.create(800,600)
ww <- wall.clean(ww)
ww <- wall.addBar(ww,12,15,70,45,col="red")
ww <- wall.addRect(ww,12,35,70,45,3,col="green")
save.image(ww,"test.png")
ww1 <- imsub(ww, x > 10, x < 75, y > 10, y < 75)
