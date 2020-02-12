#библиотеки
library(OpenImageR)

#параметры
folderInput <- "Data"

#функции
dupRows <- function(imag, byrow=1){
     #ищет строки/столбцы вматрице изображения сумма ркостей которых совпадает
     # byrow = 1 или 2, согласно нотации функции apply 
          
     hashrow <- apply(img,byrow,sum)
     df <- as.data.frame(table(hashrow))
     return(which(df$Freq > 1))
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
for (f in fls[1]){
     img <- readImage(f)
     dr <- dupRows(img)
     img1 <- addLine(img,dr)
     writeImage(img1,paste0(f,"_01_test.jpg"))

}
