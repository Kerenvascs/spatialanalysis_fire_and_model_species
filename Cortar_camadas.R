library(raster)

current.list <- list.files(path="/Users/Keren Vasconcelos/Desktop/Keren/ITV/Análises Espaciais/2 semana/Base/wc10", pattern =".bil$", full.names=TRUE)
current.list
Bioclimate <- stack(current.list)
names(Bioclimate)

## Cropar raster
E <- extent(-82, -45, -13, 13)

amazonia <- crop(Bioclimate, E)

plot(amazonia)

## Salvar raster
writeRaster(amazonia$bio10, "bio10.img", format = "HFA", overwrite=T)
