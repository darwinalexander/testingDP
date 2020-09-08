#' -------
#' title: "Evaluacion de especies maderables alternativas, para disminuir la sobreexplotacion 
#  a especies de alto valor comercial en las provincias de Loja y Zamora Chinchipe."
#' subtitle: "Analisis de clasificacion de datos (PCA)"
#' author: "Keyla Cartuche"
#  date: "21.ago.2020"
#' -------
# Codificado: Darwin Pucha C.

#################### ANALISIS DE COMPONENTES PRINCIPALES ##########################

rm(list=ls())     # Limpiar espacio de trabajo
graphics.off()    # Limpiar area de graficos
#  Ctrl + l       # Limpiar consola


## 1. IMPORTACION, VISUALIZACION, ESTADISTICOS BASICOS Y EXPORTACION DE DATOS
# Fijar la carpeta de trabajo

setwd("M:/Documents/ACADEMICA/TESIS - MADERAS/r/PCA_KC")

# importar archivo CSV

db <- read.csv("KC_Base_datos_maderas_PCA.csv", sep = ";")


# Subtabla

db2.1  <- db[, c("fam",  "Name", "T.bosque", "P_alt") ]   # Categorias
db2.2 <-  db[, sapply(db, class) == "numeric"]  # solo numericos
#db2.3 <-  db[, sapply(db, class) == "integer"]  # solo enteros


# nueva tabla

db2 <-  cbind(db2.1, db2.2)


# Eliminar manualmente variables
#db2$DAP_cm <- NULL
#db2$HT_m <- NULL
#db2$C_long <- NULL
#db2$R_tan_rad <- NULL
db2[, c("Temp", "C_x", "C_y","fam",
        "DAP","HT")] <- NULL




# Filtro para dejar solo celdas completas

db3 <- db2[complete.cases(db2), ]

rownames(db3) <-  db3$Name # poner nombres a las filas

db3$Name <- NULL



## ANALISIS DE DATOS

# 2. DENDROGRAMAS


###############################
# Heat Maps


db4 <- data.matrix(db3) # to matrix

##db4 <- t(db4)  # Transponer datos


db4 <- scale(db4, center=T, scale=T) # data standarization



###############################################
# 3. PCA (Analisis de componentes principales)
#      
###############################################



## PCA
# Crear un objeto PCA

#TODAS
db4.pca <- prcomp(db4[ , 3:71] , scale = T, center = T)  # menos primeras 4 columnas
db4.pca.fis <- prcomp(db4[ , 16:21 ], scale = T, center = T)  #  solo propiedades fisicas
db4.pca.org <- prcomp(db4[ , 3:15 ], scale = T, center = T)  #  solo propiedades Organolepticas
db4.pca.anat <- prcomp(db4[ , 22:71 ], scale = T, center = T)  #  solo Caracteristicas Anatomicas



# 3.1. VISUALIZACION DE DATOS

# 3.1.1. Datos INDIVIDUALES
library(factoextra)  # Abrir libreria para realizar los graficos


jpeg(filename="pca.ind_all_P_alt.jpg", width = 350, height = 200, units = "mm", res=500)

fviz_pca_ind(db4.pca,
             title = expression("PCA - Ind: MADERAS DEL SUR DEL ECUADOR - (Piso altitudinal)" * italic(" TODAS ")),
             legend.title = "Variables:",
             habillage = db3$P_alt,
             addEllipses = T,
             ellipse.level = 0.99,
             label = "n",  # "all" es para ver todas las etiquetas, para ninguno "n"
             repel = TRUE,
             labelsize = 3.5,
             pointsize = "contrib" )  
dev.off()



# 3.1.2. Datos por VARIABLES

####fviz_pca_var(db4.pca,
    #         title = expression("PCA (Variables) MADERAS DEL SUR DEL ECUADOR - TODAS"),
     #        col.var = "contrib",
      #       repel = T,
       #      label = "all",
        #     legend.title = "Contribucion:")

############## 3.1.3. PCA IND  ######
###  Por tipo de bosque y piso altitudinal

#### A. TODOS 

jpeg(filename="pca.ind_all_P_alt.jpg", width = 350, height = 200, units = "mm", res=500)

fviz_pca_ind(db4.pca,
             title = expression("PCA - Ind: MADERAS DEL SUR DEL ECUADOR - (Piso altitudinal)" * italic(" TODAS ")),
             legend.title = "Variables:",
             habillage = db3$P_alt,
             addEllipses = T,
             ellipse.level = 0.99,
             label = "n",  # "all" es para ver todas las etiquetas, para ninguno "n"
             repel = TRUE,
             labelsize = 3.5,
             pointsize = "contrib" )  
dev.off()

#### B. CAR. ORGANOLEPTICAS

jpeg(filename="pca.ind_org_T_bosque(etiquetas).jpg", width = 300, height = 200, units = "mm", res=500)

fviz_pca_ind(db4.pca.org,
             title = expression("PCA - Ind: MADERAS DEL SUR DEL ECUADOR - (Tipo de bosque)" * italic(" Características organolépticas ")),
             legend.title = "Variables:",
             habillage = db3$T.bosque,
             addEllipses = T,
             ellipse.level = 0.99,
             label = "all",  # "all" es para ver todas las etiquetas, para ninguno "n"
             repel = TRUE,
             labelsize = 3.5,
             pointsize = "contrib" )  

dev.off()

####   C. cAR. FÍSICAS 

jpeg(filename="pca.ind_fisica_P_alt(etiquetas).jpg", width = 300, height = 200, units = "mm", res=500)

fviz_pca_ind(db4.pca.fis,
             title = expression("PCA - Ind: MADERAS DEL SUR DEL ECUADOR - (Piso altitudinal)" * italic(" Características físicas")),
             legend.title = "Variables:",
             habillage = db3$P_alt,
             addEllipses = T,
             ellipse.level = 0.99,
             label = "all",  # "all" es para ver todas las etiquetas, para ninguno "n"
             repel = TRUE,
             labelsize = 3.5,
             pointsize = "contrib" )  

dev.off()

##### D. CAR. ANATOMICAS

jpeg(filename="pca.ind_anat_T_bosque (etiquetas).jpg", width = 300, height = 200, units = "mm", res=500)

fviz_pca_ind(db4.pca.anat,
             title = expression("PCA - Ind: MADERAS DEL SUR DEL ECUADOR - (Tipo de bosque)" * italic(" Características anatómicas")),
             legend.title = "Variables:",
             habillage = db3$T.bosque,
             addEllipses = T,
             ellipse.level = 0.99,
             label = "all",  # "all" es para ver todas las etiquetas, para ninguno "n"
             repel = TRUE,
             labelsize = 3.5,
             pointsize = "contrib" ) 
dev.off()

####### 3.1.4. PCA BIPLOT #########

# A. TODOS

jpeg(filename="pca.biplot_all_P_alt(etiquetas).jpg", width = 350, height = 200, units = "mm", res=500)

fviz_pca_biplot(db4.pca,
                title = expression("PCA - Biplot: MADERAS DEL SUR DEL ECUADOR - (Piso altitudinal)" * italic(" TODAS")),
                col.var = "dodgerblue",
                #col.ind = "forestgreen",  # "grey70",  # Individuals color
                addEllipses = T,
                habillage = db3$P_alt,
                legend.title = "Variabilidad",
                label = "all",
                repel = T) 
dev.off()


####### B. Propiedades  fisicas
# Pisos altitudinales

jpeg(filename="pca.biplot_fisicas_P_alt(etiquetadas).jpg", width = 300, height = 200, units = "mm", res=500)

fviz_pca_biplot(db4.pca.fis,
                title = expression("PCA - Biplot: MADERAS DEL SUR DEL ECUADOR - (Piso altitudinal)" * italic(" Características físicas")),
                col.var = "dodgerblue",
                #col.ind = "forestgreen",  # "grey70",  # Individuals color
                addEllipses = T,
                habillage = db3$P_alt,
                legend.title = "Variabilidad",
                label = "all",
                repel = T) 
dev.off()

# Tipo de bosque

jpeg(filename="pca.biplot_fisicas_T_bosque(etiquetas).jpg", width = 300, height = 200, units = "mm", res=500)

fviz_pca_biplot(db4.pca.fis,
                title = expression("PCA - Biplot: MADERAS DEL SUR DEL ECUADOR - (Tipo de Bosque)" * italic(" Características físicas")),
                col.var = "dodgerblue",
                #col.ind = "forestgreen",  # "grey70",  # Individuals color
                addEllipses = T,
                habillage = db3$T.bosque,
                legend.title = "Variabilidad",
                label = "all",
                repel = T) 
dev.off()



####### C. Propiedades  organolepticas
# Pisos altitudinales

jpeg(filename="pca.biplot_org_P_alt.jpg", width = 300, height = 200, units = "mm", res=500)

fviz_pca_biplot(db4.pca.org,
                title = expression("PCA - Biplot: MADERAS DEL SUR DEL ECUADOR - (Piso altitudinal)" * italic(" Características organolépticas")),
                col.var = "dodgerblue",
                #col.ind = "forestgreen",  # "grey70",  # Individuals color
                addEllipses = T,
                habillage = db3$P_alt,
                legend.title = "Variabilidad",
                label = "n",
                repel = T) 
dev.off()

# Tipo de bosque

jpeg(filename="pca.biplot_org_T_bosque(etiquetas).jpg", width = 300, height = 200, units = "mm", res=500)

fviz_pca_biplot(db4.pca.org,
                title = expression("PCA - Biplot: MADERAS DEL SUR DEL ECUADOR - (Tipo de bosque)" * italic(" Características organolépticas")),
                col.var = "dodgerblue",
                #col.ind = "forestgreen",  # "grey70",  # Individuals color
                addEllipses = T,
                habillage = db3$T.bosque,
                legend.title = "Variabilidad",
                label = "all",
                repel = T) 
dev.off()




####### D. Caracteristicas anatomicas
# Pisos altitudinales

jpeg(filename="pca.biplot_anatomicas_P_alt.jpg", width = 300, height = 200, units = "mm", res=500)

fviz_pca_biplot(db4.pca.anat,
                title = expression("PCA - Biplot: MADERAS DEL SUR DEL ECUADOR - (Piso altitudinal)" * italic(" Características anatómicas")),
                col.var = "dodgerblue",
                #col.ind = "forestgreen",  # "grey70",  # Individuals color
                addEllipses = T,
                habillage = db3$P_alt,
                legend.title = "Variabilidad",
                label = "n",
                repel = T) 
dev.off()


# Tipo de bosque

jpeg(filename="pca.biplot_anatomicas_T_bosque.jpg", width = 300, height = 200, units = "mm", res=500)

fviz_pca_biplot(db4.pca.anat,
                title = expression("PCA - Biplot: MADERAS DEL SUR DEL ECUADOR - (Tipo de bosque)" * italic(" Características anatómicas")),
                col.var = "dodgerblue",
                #col.ind = "forestgreen",  # "grey70",  # Individuals color
                addEllipses = T,
                habillage = db3$T.bosque,
                legend.title = "Variabilidad",
                label = "n",
                repel = T) 
dev.off()
