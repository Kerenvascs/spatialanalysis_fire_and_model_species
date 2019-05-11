setwd("C:/Users/Keren Vasconcelos/Desktop/Keren/ITV/Análises Espaciais/2 semana/Aula 5 Marcelo")

library(raster)
library(sp)
#gerando dados para grupos taxonomicos
tax1=c(rpois(64000,8),rep(0,769))         #grupos taxonomicos diferentes ex anfibios, aves e mamiferos onde oito e a media para  gerar numeros aleatorios
tax2=c(rpois(64000,40),rep(0,769))        #64000 e 769 e o numero de celulas do raster
tax3=c(rpois(64000,13),rep(0,769))

mapa=raster("Raster_exercicio_Criterios_p960.tif")
mapa

n.cells=length(c(which(getValues(mapa)==8),which(getValues(mapa)==10))) #quantas celulas sao de mata


vals1=sample(tax1,n.cells,T)               #T = com reposiçao
ind1=c(which(getValues(mapa)==8),which(getValues(mapa)==10))    #me diga quais numeros de pixels sao demata nesse raster, ou seja qual a posiçao deles, quais sao os pixels de mata
mtax1=matrix(0,239,271) #matriz do tax 1 e igual a todos os outros, metax2, mtax3
r.tax1=raster(mtax1,xmn=-55.2599,xmx=-52.92225,ymn=-4.43018,ymx=-2.367427,crs="+proj=longlat +ellps=GRS67 +towgs84=-57,1,-41,0,0,0,0 +no_defs")  #faz raster usando a matriz criada e com as mesmas caracteristicas do mapa, xmin, xmax, ymin, ymax.
r.tax1[ind1]=vals1/max(tax1)   #coloque os valores do tax1 dividindo pelo max do tax1 para escalonar ou seja padronizar os valores de 0 a 1
plot(r.tax1)

vals2=sample(tax2,n.cells,T)  #mesma coisa do tax1
ind2=c(which(getValues(mapa)==8),which(getValues(mapa)==10))
mtax2=matrix(0,239,271)
r.tax2=raster(mtax2,xmn=-55.2599,xmx=-52.92225,ymn=-4.43018,ymx=-2.367427,crs="+proj=longlat +ellps=GRS67 +towgs84=-57,1,-41,0,0,0,0 +no_defs")
r.tax2[ind2]=vals2/max(tax2)
plot(r.tax2)

vals3=sample(tax3,n.cells,T)
ind3=c(which(getValues(mapa)==8),which(getValues(mapa)==10))
mtax3=matrix(0,239,271)
r.tax3=raster(mtax3,xmn=-55.2599,xmx=-52.92225,ymn=-4.43018,ymx=-2.367427,crs="+proj=longlat +ellps=GRS67 +towgs84=-57,1,-41,0,0,0,0 +no_defs")
r.tax3[ind3]=vals3/max(tax3)
plot(r.tax3)

mapa=calc(mapa, fun=function(x) { x[x == 8|x==4|x==10] <- 100; return(x) } ) #refaz o mapa de forma binaria e incluiu o 4 que e a hidrografia
mapa=calc(mapa, fun=function(x) { x[x != 100] <- 0; return(x) } ) # o mapa virou binario para dar mais valor pro que for mata porem os rios tambem sao considerados. Nesse caso se for rio e mata e 1 e se nao for e 0
mapa=calc(mapa, fun=function(x) { x[x == 100] <- 1; return(x) } )

soma=(r.tax1+r.tax2+r.tax3+mapa)/4 #soma as camadas e os pixels que tiverem mais valore sao importantes pq sao de rio e de mata, quando dive por 4 ele ta tirando a media dos valores das 4 camadas para cada pixel. Mantendo assim os valores de pixel para cada 1
plot(soma)
#Reescalonando
soma[which(getValues(soma)>0.6)]=1 
soma[which(getValues(soma)>0.45&getValues(soma)<=0.6)]=0.6
soma[which(getValues(soma)>0.3&getValues(soma)<=0.45)]=0.35
soma[which(getValues(soma)>0.15&getValues(soma)<=0.3)]=0.2
soma[which(getValues(soma)<=0.15)]=0
plot(soma)

low.res <- aggregate(soma, fact=2, fun=mean, expand=FALSE, na.rm=TRUE)  #diminuir a resolucao do raster
maxValue(low.res)
low.res.sc=low.res/maxValue(low.res)
plot(low.res.sc)
# dados=stack(list(mapa,r.tax1,r.tax2,r.tax3))
# dados
# 
# dados2=extract(dados,1:64769)
# norm.dados2=apply(dados2,1,sum)
# 
# mat.f=matrix(norm.dados2,239,271)
# plot(raster(mat.f,xmn=-55.2599,xmx=-52.92225,ymn=-4.43018,ymx=-2.367427,crs="+proj=longlat +ellps=GRS67 +towgs84=-57,1,-41,0,0,0,0 +no_defs"))
# 
# plot(raster(matrix(dados[,1],239,271),xmn=-55.2599,xmx=-52.92225,ymn=-4.43018,ymx=-2.367427,crs="+proj=longlat +ellps=GRS67 +towgs84=-57,1,-41,0,0,0,0 +no_defs"))

