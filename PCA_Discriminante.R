

#---------2. ANALISIS DE COMPONENTES PRINCIPALES-------
#ind-> individuos
#var-> variables
#biplot-> individuos y variables

library(factoextra)
#Para graficar se usa FactoExtra:
#Draw the graph of individuals/variables from the output of Principal Component Analysis (PCA).

#The following functions, from factoextra package are use:

#fviz_pca_ind(): Graph of individuals
#fviz_pca_var(): Graph of variables
#fviz_pca_biplot() (or fviz_pca()): Biplot of individuals and variables


# PCA ESTRUCTURA 

attach(PCA_TODOS)

pca=prcomp(PCA_TODOS[3:12],scale=TRUE,scores=T) #En corchetes se ponen las columnas que quieres incluir en el PCA
pca #Nombre del PCA

pca$scale
get_pca_var(pca)
A3=get_pca_ind(pca)# Contribucion por individuos -parcelas/fragmentos
A3=get_pca_var(pca)#Contribucion por variables
A3$contrib
scores(pca, choices=c(1,2))
eig.val <- get_eigenvalue(pca)
eig.val

#Resumen del PCA
summary(pca)
resultados_pca=data.frame(summary(pca)$importance)
resultados_pca

factores_variables=get_pca_var(pca)
contri_var_pca=factores_variables$contrib
contri_var_pca


s=as.table(contri_var_pca) # Convierte todos los datos en tabla

write.xlsx(s,"var.xlsx")

#Exportar tabla a Excel

#Paquetes xlxs, readxl,writexl,openxlxs-> Para guardar tablas y exportar en Excel

library(openxlsx)
library(xlxs)
library(readxl)
library(writexl)

write.xlsx(contri_var_pca, "Contribucion Variables pca datos normales.xlsx")


#Grafica del PCA 

pcagrafica=fviz_pca_biplot(pca,  label=c("var","Ind"),title=" ",labelsize = 4,habillage = PCA_TODOS$Parcela,
                           pointsize = 2.5)
pcagrafica

PCAE=pcagrafica+theme_classic()+ theme (text = element_text(family = "Times New Roman",face = "bold",size=12),
                                        panel.grid.major = element_blank(),
                                        panel.grid.minor = element_blank(),
                                        panel.background = element_rect(fill = NA, color = "black",size = 1),
                                        legend.background=element_rect(fill="white", colour="white"),
                                        legend.key=element_rect(colour="black",size = 0.1),
                                        legend.key.size=unit(1.8, "lines"),
                                        legend.position=("bottom"),
                                        legend.title = element_blank(),
                                        axis.text.x = element_text(size=12,face="bold"),
                                        axis.text.y = element_text(size=12,face="bold",family = "Times New Roman"))
PCAE
#Guardar la grafica

ggsave("PCAEstructura.png",plot=PCAE,dpi=400,width=16,height = 16,units = "cm")

#PCA con promedios de fragmentos (un promedio por fragmento)
attach(PCA_prom)

pca_frag=prcomp(PCA_prom[2:11],scale=TRUE,scores=T)
pca_frag

pca2=fviz_pca_biplot(pca_frag,  label=c("var","Ind"),title=" ",labelsize = 4,habillage = PCA_prom$Fragmento,
                     pointsize = 2.5)

pca2

#Grafica de contribucion de las variables
fviz_pca_var(pca_frag, col.var = "cos2",
             gradient.cols = c("green", "yellow", "red"), 
             repel = TRUE)

#Porcentajes contribucion de las variables
factores_variables=get_pca_var(pca_frag)
contri_var_pca_frag=factores_variables$contrib
contri_var_pca_frag


f=as.table(contri_var_pca_frag) # Convierte todos los datos en tabla
f
write.xlsx(f,"var_frag.xlsx")


#Analisis No Metrico


#----------4. PCA PARA ANALISIS DISCRIMINANTE Y REGRESION DE VARIABLES POR DIMENSIONES -------

attach(AD_TODOS)

Fragmentos=as.data.frame(AD_TODOS)# Convertir la tabla que exporto desde Excel como un DataFrame para no tener problemas

Fragmentos

#Pruebas de normalidad

normal<-function(vec){
  shapiro.test(vec)$p
}

Normalidad=aggregate(Fragmentos[,2:11],list(AD_TODOS$Fragmento),normal)
Normalidad

install.packages(MVN)
library(MVN)


pca_AD=prcomp(Fragmentos[2:11],scale=TRUE,scores=T)
pca_AD
summary(pca_AD)

pca_AD=fviz_pca_biplot(pca_AD,  label=c("var","Ind"),title=" ",labelsize = 6,habillage = AD_TODOS$Fragmento,
                       pointsize = 2.5)

pca_AD

#PCA con los valores o porcentajes de contribucion de las variables
#funcion-> fviz_pca_var

#Grafica de contribucion de las variables
fviz_pca_var(pca_frag, col.var = "cos2",
             gradient.cols = c("green", "yellow", "red"), 
             repel = TRUE)

#Porcentajes contribucion de las variables
factores_variables=get_pca_var(pca_frag)
contri_var_pca_frag=factores_variables$contrib
contri_var_pca_frag


f=as.table(contri_var_pca_frag) # Convierte todos los datos en tabla
f
write.xlsx(f,"var_frag.xlsx") #-> Esta es la tabla con los valores de contribucion de las variables del PCA por fragmentos.

attach(AD_TODOS)

Fragmentos=as.data.frame(AD_TODOS)
Fragmentos

normal<-function(vec){
  shapiro.test(vec)$p
}

Normalidad=aggregate(Fragmentos[,2:11],list(AD_TODOS$Fragmento),normal)

Normalidad

Normalidad_Multi=mvn(AD_TODOS[,c(2:11)],mvnTest = "hz")
Normalidad_Multi

pca_datos_normales=prcomp(Fragmentos[2:11])
summary(pca_datos_normales)
resultados_pca=data.frame(summary(pca_datos_normales)$importance)
resultados_pca


grafica_pca=fviz_pca_var(pca_datos_normales, col.var = "contrib")

#Paquetes:
#Factoextra, Factorminer, ggplot, ggalt, caret, tidyverse, mvn, readxls, xlsx, Mass

attach(AD_TODOS)

Fragmentos=as.data.frame(AD_TODOS)#->ASI SE LLAMAN TODOS LOS DATOS GUARDADOS COMO DATAFRAME
Fragmentos

normal<-function(vec){
  shapiro.test(vec)$p
}

Normalidad=aggregate(Fragmentos[,2:11],list(AD_TODOS$Fragmento),normal)

Normalidad

N=as.table(Normalidad) # Convierte todos los datos en tabla
N
write.xlsx(Normalidad,"Normalidad2.xlsx") #-> no me quiere guardar como una tabla

Normalidad_Multi=mvn(AD_TODOS[,c(2:11)],mvnTest = "hz")
Normalidad_Multi

#el paquete MVN no me quiere cargar me pide la version 3.5

pca_datos_normales=prcomp(Fragmentos[2:10])
summary(pca_datos_normales)
resultados_pca=data.frame(summary(pca_datos_normales)$importance)
resultados_pca


grafica_pca=fviz_pca_var(pca_datos_normales, col.var = "contrib",
                         gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                         repel = TRUE,title=" ", legend.title="Contribución")+ 
  theme_classic()+
  theme(text = element_text(family = "Times New Roman",face = "bold",size=12),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(colour = "black",size = 12),
        axis.text.y = element_text(colour = "black",size = 12),
        panel.background = element_blank())

grafica_pca

pca_AD=prcomp(Fragmentos[2:11],scale=TRUE,scores=T)
pca_AD
summary(pca_AD)

pca_AD=fviz_pca_biplot(pca_AD,  label=c("var","Ind"),title=" ",labelsize = 6,habillage = AD_TODOS$Fragmento,
                       pointsize = 2.5)

pca_AD

factores_variables=get_pca_var(pca_datos_normales)
contri_var_pca=factores_variables$contrib
contri_var_pca

AD=as.table(contri_var_pca) # Convierte todos los datos en tabla

write.xlsx(AD,"contri_var_pca4.xlsx")


factores_individuos_pca=get_pca_ind(pca_datos_normales)
factores_individuos_pca

write.xlsx(factores_individuos_pca,"factores_individuos_pca4.xlsx")

dimensiones_pca=as.data.frame(factores_individuos_pca$contrib)
dimensiones_pca

write.xlsx(contri_var_pca,"dimensiones_pca.xlsx")

#Se realiza un lm por cada variable respecto al componente 1 del PCA por fragmentos (Dim1)

HOJ=lm(Fragmentos$`Hojarasca (%)`~dimensiones_pca$Dim.1)
SD=lm(Fragmentos$`Suelo Desnudo (%)`~dimensiones_pca$Dim.1)
HER=lm(Fragmentos$`Herbaceas(%)`~dimensiones_pca$Dim.1)
TM=lm(Fragmentos$`Temperatura(ºC)`~dimensiones_pca$Dim.1)
LM=lm(Fragmentos$`Luminosidad(LUX)`~dimensiones_pca$Dim.1)
HT=lm(Fragmentos$`Humedad Relativa(%)`~dimensiones_pca$Dim.1)
CD=lm(Fragmentos$`Cobertura dosell`~dimensiones_pca$Dim.1)
JI1=lm(Fragmentos$`Juv1 (inds)`~dimensiones_pca$Dim.1)
JI2=lm(Fragmentos$`Juv2 (inds)`~dimensiones_pca$Dim.1)
AI=lm(Fragmentos$`Árboles (inds)`~dimensiones_pca$Dim.1)

#Se extraen los residuales de cada lm realizado

RHOJ=as.data.frame(HOJ$residuals)
RSD=as.data.frame(SD$residuals)
RHER=as.data.frame(HER$residuals)
RTM=as.data.frame(TM$residuals)
RLM=as.data.frame(LM$residuals)
RHT=as.data.frame(HT$residuals)
RCD=as.data.frame(CD$residuals)
RJI1=as.data.frame(JI1$residuals)
RJI2=as.data.frame(JI2$residuals)
RAI=as.data.frame(AI$residuals)

#se unen todos los residuales en una sola tabla llamada->
residuales_regresiones=cbind(RHOJ,RSD,RHER,RTM,RLM,RHT,RCD,RJI1,RJI2,RAI)
residuales_regresiones

#Se les cambia el nombre a las columnas de residuales
colnames(residuales_regresiones)=c("HOJ","SD","HER","TM","LM","HT","CD","JI1","JI2",
                                   "AI")
residuales_regresiones

FRG=Fragmentos$Fragmento
FRG

#Se construye un dataframe con los residuales de las regresiones para cada punto (parcela) de cada uno de los 4 fragmentos
residuales_datos_normales=as.data.frame(cbind(FRG,residuales_regresiones))
residuales_datos_normales

write.xlsx(residuales_datos_normales,"residuales_datos_normales.xlsx")

require (RMarkdown) 

require (dbplyr)

set.seed(123)
install.packages("caret")

residuales_datos_normales$FRG

training.samples=residuales_datos_normales$FRG %>%
  createDataPartition(p = 0.9, list = FALSE)
train.data=residuales_datos_normales[training.samples, ]
test.data=residuales_datos_normales[-training.samples, ]


preproc.param <- train.data %>% 
  preProcess(method = c("center", "scale"))
train.transformed <- preproc.param %>% predict(train.data)
test.transformed <- preproc.param %>% predict(test.data)

require(MASS)

modelo=lda(FRG~., data = train.transformed)
modelo

contri_var_lda=modelo$scaling
contri_var_lda

predicciones=modelo %>% predict(test.transformed)
predicciones

mean(predicciones$class==test.transformed$FRG)

lda.data=cbind(train.transformed, predict(modelo)$x)
lda.data

grafica_discriminante=ggplot(lda.data, aes(LD1, LD2)) +
  geom_point(aes(shape=FRG,color = FRG),size=2)+
  scale_shape_manual(values = c(0,1,2,3))+
  scale_color_manual(values=c("chartreuse4","cyan3",
                              "darkgoldenrod1", "orangered"))+
  geom_encircle(aes(fill = FRG), s_shape = 1, expand = 0,
                alpha = 0.2, color = "black", show.legend = FALSE)+
  scale_fill_manual(values=c("chartreuse4","cyan3",
                             "darkgoldenrod1", "orangered"))+
  theme_classic()+
  theme(text = element_text(family = "Times New Roman",face = "bold",size=20),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(colour = "black",size = 20),
        axis.text.y = element_text(colour = "black",size = 20),
        panel.background = element_blank())

#points(nmds1$points, pch = as.numeric(env$Forest.type), col = as.numeric(env$Forest.type),  cex = 1.5)

#for (i in unique (lda.data$FRG)) ordihull (nmds1, groups = as.numeric(lda.data$FRG, show.group = i, col = i), draw = 'polygon', label = T)
#COMO PONERLE EL CODIOGOA CADA PARCELA

grafica_discriminante  

ordiplot (pca2, display = 'sites', type = 'n')
points (pca2, col = as.numeric(env$Forest.type), pch = as.numeric(env$Forest.type))

for (i in unique (env$Forest.type)) ordihull (nmds1, groups = as.numeric(env$Forest.type, show.group = i, col = i), draw = 'polygon', label = T)

#----INSTALACION DE PAQUETES
#Con el paquete MVN se peuden correr pruebas de normalidad
#MVN solicita versiones actualizadas de R y paquetes como rmarkdown
install.packages("rmarkdown", dependencies = TRUE, type="binary",binary="TRUE")

install.packages("~/Downloads/rmarkdown_1.16.tar.gz")
library(MVN)

install.packages("robCompositions")

#Para hacer las elipses en graficos multivariados se utiliza Vegan y Vegan3d.
#Estos requieren el XQuartz o OpenGL.
#Ya descargue e instale XQuartz pero funciona en sistemas operativos MacOs 10.6 y el mio es 10.6.3

install.packages("rgl", type="binary",binary="TRUE")
library(rgl)

library(devtools)

install.packages("~/Downloads/rgl_0.100.54.tar")

install.packages("~/Downloads/rgl_0.100.19.tar",dependencies = TRUE, type="binary",binary="TRUE")

install.packages("nat", dependencies=TRUE)

install.packages("dartR")

install.packages("~/Downloads/adegenet_2.1.3.tar", repos = NULL,type="binary")
library(adegenet)

#dep = TRUE
#Ya me lee MVN pero me pide el paquete rmarkdown y no me deja inStalarlo<- SOUCIONADO

# "Error in install.packages : type ==“both” cannot be used with 'repos =NULL'"->SOLUCION ->add type="binary"

#-------------CLUSTER-------

attach(NMDS_spp)
Cluster=data.frame(NMDS_spp,row.names =1)

ID=vegdist(Cluster,method="bray")
ID
C=hclust(ID,method =  "average")
C$height
CPlantas=fviz_dend(C, k = 4,horiz=T, cex = 1,
                   color_labels_by_k = F, rect = TRUE,
                   rect_fill = F,ylab = " ",main=" ") +
  theme (text = element_text(family = "Times New Roman",face = "bold",size=20),
         panel.grid.major = element_blank(),
         panel.grid.minor = element_blank(),
         panel.background = element_rect(fill = NA, color = "black",size = 1),
         legend.background=element_rect(fill="white", colour="white"),
         legend.key=element_rect(colour="black",size = 0.1),
         legend.key.size=unit(1.8, "lines"),
         legend.position=("bottom"),
         legend.title=element_blank(),
         axis.text.x = element_text(size=20,face="bold"),
         axis.ticks.y = element_blank())
CPlantas
ggsave("cluster plantas ruizi2.png",plot=CPlantas,dpi=400,width=16,height = 16,units = "cm")

fviz_nbclust(Cluster, kmeans, method = "gap_stat", k.max = 19, 
             diss = get_dist(my_data, method = "euclidean"), nstart = 50)+
  labs(title= "Numero optimo de cluster") + 
  xlab("k ") +
  ylab("Estadisticas Gap")

#Correlacion cofenetica mide la similitud entre las similitudes/distancias
#originales y las estimadas da partir del arbol

coph=cophenetic(C)
coph
cor(ID,coph)







