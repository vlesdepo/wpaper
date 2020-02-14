#поиск по кратинке повторяющихся строк методом евклидова расстояния
#побочный эффект: если в картинке строк нет, то в результате будут выданы соседние номера линий (например 23,24) 

#библиотеки
library(OpenImageR)

#параметры
folderInput <- "Data" #она же и для выходных файлов

#функции
dupRows <- function(imag){
     #v2 ищет строки/столбцы вматрице изображения сумма ркостей которых совпадает
 
     dfm <- as.data.frame(imag)
     di <- dist(dfm,method = "euclidean")
     di <- as.matrix(di)
     di[which(di==0)] <- NA
     m <- which(di==min(di,na.rm = T),arr.ind = T)
     m <- as.data.frame(m)
     return(m$row)
}

addLine <- function(imag, veclines, byrow=1){
     #добавляет красную линию на рисунок
     #byrow=1 - горизонтальная, иначе вертикальная
     if (byrow==1) {
          imag[veclines,,1] <- 1
     } else {
          imag[,veclines,1] <- 1
     }
     return(imag)
}

# читаем список файлов
fls <- list.files(folderInput,recursive = T,include.dirs = T,pattern = "*.jpg*",full.names = T)

#обработка
for (f in fls){
     img <- readImage(f)
     imgG <- rgb_2gray(img)
     dr <- dupRows(imgG)
     cat("Горизонтальная обработка файла: ",f,". Найдены строки: ",dr,"\n")
     img <- addLine(img,dr)
     dr <- dupRows(t(imgG))
     cat("Вертикальная обработка файла: ",f,". Найдены строки: ",dr,"\n")
     img <- addLine(img,dr,byrow = 0)
     writeImage(img,paste0(f,"_find_texture.jpg"))

}




