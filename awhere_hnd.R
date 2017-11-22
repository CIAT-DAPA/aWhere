#####  A-Where Data

# descarga (revisar si hay alguna forma de que el paquete funcione)


######## Guardado de archivos diarios y mensuales + depuracion de la base de datos
# Setwd
library(readxl)
library(ggplot2)
library(grid)
library(gridExtra)
library(epiR)
library(dplyr)
library(randtests)
library(reshape)


# Directorio de trabajo 
setwd("C:/Users/AESQUIVEL/Google Drive/Informes/A_Where/Honduras")
getwd()
Honduras <- "C:/Users/AESQUIVEL/Google Drive/Informes/A_Where/Honduras" 

# liste los directorios en la carpeta donde se encuentran los datos de AWhere
folders <- list.dirs(path = "C:/Users/AESQUIVEL/Google Drive/Informes/A_Where/Honduras/app")
folders<-folders[-1] # elimine el nombre del directorio principal



# Esta funcion unifica los archivos de AWhere para que sean datos diarios en formato .csv
# path es la carpeta donde se encuentran los archivos de los sitios
daily_data <-function(Path){
  
  setwd(Path) #ubiquese en la carpeta del sitio de interes
  Test1<-NA # cree un objeto NA
  Test1<-list.files(Path, pattern = ".xlsx") # liste los archivos .xlsx de la carpeta
  tables <- lapply(Test1, read_excel) # lea todas las direcciones de esos archivos en una lista
  tables <- lapply(tables, as.data.frame) # convierta los objetos de la lista a data.frame
  combined.df <- do.call(rbind , tables)
  
  # coversion de la radiacion solar a las unidades del SI
  combined.df <- mutate(.data = combined.df, solar_24= solar/24, solarC= (solar/24)*0.0864)
  
  
  Split <- strsplit(Path, "/") # divida los nombres de las rutas (/)
  LastElement <- Split[[1]][length(Split[[1]])] # extraiga solo el ultimo elemento (nombre del sitio)
  
  # Extraiga las fechas que se encuentran en este formato ("%Y-%m-%d")
  combined.df$date<-as.Date(combined.df$date, format = "%Y-%m-%d")
  
  # Cree tres variables que corresponden al dia, mes y año
  day	<- as.numeric(format(combined.df$date,"%d"))
  month <- as.numeric(format(combined.df$date,"%m"))
  year <- as.numeric(format(combined.df$date,"%Y"))
  
  # imprima las coordenadas de la estacion en la consola
  print((combined.df[1, 2:3]))
  
  combined.df <- combined.df[, -(1:3)] # elimine las 3 primeras columnas del data.frame
  combined.df <- cbind(day = day, month = month, year= year, combined.df) # adicione las neuvas variables
  write.csv(combined.df, paste(LastElement, "_daily.csv", sep=""), row.names = FALSE) # guarde el archivo mensual del sitio
  
  # monthly data
  # Agregue el archivo de forma que los datos queden mensuales, usando la funcion promedio
  test2<-aggregate(x = combined.df, by = list(combined.df$month, combined.df$year) , FUN = mean )
  # haga lo mismo pero en el caso de la precipitacion agreguelos mediante la funcion suma
  precip <- aggregate(x = combined.df$precipitation , by = list(combined.df$month, combined.df$year) , FUN = sum )
  test2$precipitation <- precip$x
  
  # asinele nombres a las variables 
  Null_names<-c("Group.1", "Group.2",  "day", "wind_avg",  "solar", "solar_24" )
  test2<-test2[,  !(names(test2) %in% Null_names)] # elimine las variables con los nombres especificados
  names(test2) <- c("month", "year", "tmax",	"tmin",	"precip","humid_max",  "humid_min", "srad")
  
  write.csv(test2, paste(LastElement, "_monthly.csv", sep=""), row.names = FALSE) # cree el archivo mensual del sitio

}

# Corra para todos los sitios guardados en folders 
sapply(X= folders, FUN = daily_data)






####

setwd(Honduras)
getwd() # muestre el directorio de trabajo

folders

files_ind <- function(folders){
  
  combined.df <- list()
  
  for(i in 1:length(folders)){
    Path <- folders[i]
    setwd(Path) #ubiquese en la carpeta del sitio de interes
    Test1<-list.files(Path, pattern = "_monthly.csv") # liste los archivos .xlsx de la carpeta
    Split <- paste0(strsplit(Test1, "_")[[1]][1:2], collapse = "_") # divida los nombres de las rutas (/)
    
    
    Muni<-read.csv(Test1) %>% mutate(Sitio =  Split)
    combined.df [[i]] <- Muni
  }
  
  combined.df <- do.call(rbind , combined.df)
 
  
  names(combined.df)
  
  setwd(paste(Honduras, "/app/", sep=""))
  ind <- c("tmax", "tmin", "precip") 
  
  
  for(i in 1:3){
    dataInd<- combined.df[,  c("Sitio", "month", "year", ind[i])]
    name<-paste(ind[i], ".csv", sep="")
    write.csv(dataInd, name)
  }
        
  
  
   
}

files_ind(folders)


######################################
######################################
#### Datos obs
######################################

### Para Temp

setwd(Honduras)
getwd() # muestre el directorio de trabajo
folders <- list.dirs(path = "C:/Users/AESQUIVEL/Google Drive/Informes/A_Where/Honduras/app")
folders<-folders[-1] # elimine el nombre del directorio principal

Split <- lapply(folders, strsplit, "/")
Split1<-0
for(j in 1:length(Split)){Split1[j] <- Split[[j]][[1]][9]}  



Path <- "C:/Users/AESQUIVEL/Google Drive/Informes/A_Where/Honduras/obs/Tmax_Tmin/"
setwd(Path) #ubiquese en la carpeta del sitio de interes
Test1<-list.files(Path, pattern = ".csv") # liste los archivos .xlsx de la carpeta
tables <- lapply(Test1, read.csv) # lea todas las direcciones de esos archivos en una lista
tables <- lapply(tables, as.data.frame) # convierta los objetos de la lista a data.frame


names(tables) <- c("Tmax", "Tmin")
monthly<-function(x){table<-aggregate(x, by = list(x$month, x$year), FUN = mean)
table <- table[, -(1:3)]
Col<- c(1:2, which(names(table) %in% Split1))
Rows<-which(table$year<2005)
table<- table[-Rows, Col]
return(table)}
Men<- lapply(tables, monthly)
Men<-lapply(Men, melt, 1:2)
Tmax<-Men[[1]] ; Tmin<-Men[[2]]
names(Tmax) = c( "month", "year", "Est", "Tmax" ) ; names(Tmin) = c( "month", "year", "Est", "Tmin" )

setwd(Honduras)


write.csv(Tmax, file = "obs/Tmax_O.csv")
write.csv(Tmin, file = "obs/Tmin_O.csv")





##############################################
############### Para precipitación obs
##############################################



Path <- "C:/Users/AESQUIVEL/Google Drive/Informes/A_Where/Honduras/obs/Prec/"
setwd(Path) #ubiquese en la carpeta del sitio de interes
Test1<-list.files(Path, pattern = ".csv") # liste los archivos .xlsx de la carpeta
Test1<-list.files(Path, pattern = ".csv") # liste los archivos .xlsx de la carpeta
tables <- lapply(Test1, read.csv) # lea todas las direcciones de esos archivos en una lista

precip<-merge(tables[[1]], tables[[2]])
rows<-which(precip$year<2005)
Col <-  c(1,2,which(names(precip) %in% Split1))
precip<-precip[-rows, ]
precip <- precip[, Col]

precip<-melt(precip, 1:2)
precip<-precip[order(precip$variable, precip$year, precip$month),]

setwd(Honduras)
write.csv(precip, file = "obs/precip_O.csv")



obsM_all <- list(precip = precip , tmax = Tmax, tmin = Tmin)

str(obsM_all)





##### Lea los datos mensuales de aWhere, guardelos en una lista y cortelos por el numero
##### de filas que tengan los obs


getwd() # muestre el directorio de trabajo
# liste los archivos en la carpteta donde se encuentran los archivos de AWhere
files_app <- list.files(path = "C:/Users/AESQUIVEL/Google Drive/Informes/A_Where/Honduras/app",pattern = ".csv")
app_routes <- paste("C:/Users/AESQUIVEL/Google Drive/Informes/A_Where/Honduras/app/", files_app, sep="")


app_all<-lapply(X= app_routes, FUN=read.csv)
Split <- strsplit(files_app, ".csv") # fragmente los nombres de los achivos (_)
Element <- unique(rapply(Split, function(x) head(x, 1))) # Extraiga el nombre de sitio
names(app_all)<-Element #


precs<- c("X78724_Choluteca", "X25087_SAN.JERONIMO", "X25114_ULAPA", 
          "X25142_EL.NISPERO", "X25075_GRACIASLEMPIRA")
rows <- which(app_all[[1]]$Sitio  %in% precs)
app_all[[1]] <- app_all[[1]][rows,]



temps<-c("X78724_Choluteca", "X25087_SAN.JERONIMO", "X25114_ULAPA", 
         "X25142_EL.NISPERO", "X78718_NuevaOcotepeque","X25202_LAS.LAJAS", "X25232_MAPULACA"  )

app_all[[2]] <- app_all[[2]][-which(app_all[[2]]$year>2015),]
rows <- which(app_all[[2]]$Sitio  %in% temps)
app_all[[2]] <- app_all[[2]][rows,]

app_all[[3]] <- app_all[[3]][-which(app_all[[3]]$year>2015),]
rows <- which(app_all[[3]]$Sitio  %in% temps)
app_all[[3]] <- app_all[[3]][rows,]


str(app_all)



######Validación Cruzada

# cree un directorio que se llame cross_Validacion 
dir.create("cross_validation")



summary(obsM_all$precip)
summary(app_all$precip)




names(obsM_all$precip)[ncol(obsM_all$precip)] <- names(app_all$precip) [ncol(app_all$precip)] 
names(obsM_all$tmax)[ncol(obsM_all$tmax)] <- names(app_all$tmax) [ncol(app_all$tmax)] 
names(obsM_all$tmin)[ncol(obsM_all$tmin)] <- names(app_all$tmin) [ncol(app_all$tmin)] 




# ORGANICE LA BASE DE DATOS CON OTRO ORDEN DE COLUMNAS
obsM_all<-lapply(obsM_all, function(x) x[,names(obsM_all[[3]])])
app<- lapply(app_all, function(x) x[,names(obsM_all[[3]])])
str(app) # MUESTRE UN RESUMEN DE LA BASE DE DATOS

# Cree dos matrices de indicadores
cor.cv.all = matrix(NA,51,1) # 51 número de veces que se hizo
rmse.cv.all = matrix(NA,51,1)# la validacion 

# Esta funcion hace la cross_v y guarda indicadores
cross_v <- function(obs, app, name){
  
  # guarde una imagen en .tiff
  tiff(paste0("cross_validation/",name, "_", names(app)[4],"_cv.tiff"),compression = 'lzw',height = 5,width = 7,units="in", res=100)
  
  for(j in 1:50){ # repita 50 veces el proceso
    data.model = as.data.frame(cbind("y"= obs[,4],"x"= app[,4])) # extraiga los datos
    na.sample = nrow(data.model)*sample(seq(0.01,0.2,0.01),1,replace = T) # seleccione el total de información a eliminar
    na.sample = round(na.sample,0) # redondee el número a uno entero 
    
    pos.na = sample(1:nrow(data.model),na.sample,replace=F) # extraiga las posiciones de las filas a eliminar
    
    data.model$y[pos.na] = NA # convierta esas filas en NA
    model = lm(data=data.model,formula = y~x)
    
    x = app[pos.na,4]
    data_predict = predict(model,as.data.frame(x))
    data_predict[data_predict<0] = 0
    
    
    if(j==1){
      
      plot(obs[pos.na,4],data_predict,xlab="Observed_stations",ylab="Fitted", main=names(app)[4])
      cor.cv = round(cor(obs[pos.na,4],data_predict,  use="complete.obs"),3)
      rmse.cv <- round(sqrt(mean((obs[pos.na,4]-data_predict)^2,na.rm=T)), 2)
      
    }
    
    points(obs[pos.na,4],data_predict, pch=j, col="blue")
    cor.cv <- rbind(cor.cv, round(cor(obs[pos.na,4],data_predict, use="complete.obs" ),3))
    rmse.cv <- rbind(rmse.cv, round(sqrt(mean((obs[pos.na,4]-data_predict)^2,na.rm=T)), 2))
    
    
  }
  
  cor.cv.all = cor.cv
  rmse.cv.all= rmse.cv
  eqn <- bquote(r == .(round(mean(na.omit(cor.cv.all)),3)) * "," ~~ RMSE == .(round(na.omit(mean(rmse.cv.all)),3)))
  legend('bottomright', legend = eqn, bty = 'n',cex=2)
  dev.off()
  
  assign("cor.cv.all",cor.cv.all,.GlobalEnv)
  assign("rmse.cv.all",rmse.cv.all,.GlobalEnv)
  
  print(eqn)
} 
cvD<-function(f, y){
  stations <- levels(f[,3])
  data<-0
  
  for( i in 1:length(stations)){
    station <- stations[i]
    app <- filter(.data = y, y$Sitio==station)[,-1]
    obs <- filter(.data = f, f[,3]==station)
    cross_v(obs = obs, app = app, name = station) 
    
    write.csv(cor.cv.all, paste("cross_validation/r_", station, "_",names(app)[4], ".csv",sep=""))
    write.csv(rmse.cv.all, paste("cross_validation/rmse_", station, "_",names(app)[4], ".csv",sep=""))
    datac<-cbind.data.frame(Station = station, ind =  names(app)[4]  , mean_r = round(mean(na.omit(cor.cv.all) ),2)   ,   rmse = round(mean(na.omit(rmse.cv.all) ),2))
    data<-rbind.data.frame(data, datac) 
  }
  data <- data[-1,]
  write.csv(data, paste("cv_",names(app)[4],".csv", sep=""))
return(data)}
  
cv<-list()
for(i in 1:3){
  cv[[i]]<-cvD(f = obsM_all[[i]], y = app_all[[i]])
}


cv <- Reduce(function(x, y) merge(x, y, all = TRUE),cv)
write.csv(cv, "cv.csv")







# x11()
data1<-cv[-which(cv$ind=="precip"),]
T<- ggplot(data1, aes(x = Station, y = mean_r)) + geom_point(colour ="red") + 
    theme_bw() +  facet_grid(~ ind) + labs(x="Sitio", y= "Coef. Correlación - Validación Cruzada") +
    theme(axis.text=element_text(size=8, angle = 90, hjust = 1)) + geom_hline(yintercept = 0, colour="gray")

data2<-cv[-which(cv$ind!="precip"),]
P<-ggplot(data2, aes(x = Station, y = mean_r)) + geom_point(colour ="red") + 
  theme_bw() +  facet_grid(~ ind) + labs(x="Sitio", y= "Coef. Correlación - Validación Cruzada") +
  theme(axis.text=element_text(size=8, angle = 90, hjust = 1)) + geom_hline(yintercept = 0, colour="gray")

grid.arrange(T,P, layout_matrix = cbind(2, 1,1,1))



p1<-ggplot(data1, aes(x = Station, y = rmse)) + geom_point(colour ="red") + 
  theme_bw() +  facet_grid(~ ind) + 
  labs(x="Sitio", y= "RMSE") +
  theme(axis.text=element_text(size=8, angle = 90, hjust = 1)) + 
  geom_hline(yintercept = 0, colour="gray")

p2<-ggplot(data2, aes(x = Station, y = rmse)) + geom_point(colour ="red") + 
  theme_bw() +  facet_grid(~ ind) + 
  labs(x="Sitio", y= "RMSE") +
  theme(axis.text=element_text(size=8, angle = 90, hjust = 1)) + 
  geom_hline(yintercept = 0, colour="gray")

grid.arrange(p1,p2, layout_matrix = cbind(2, 1,1,1))









###########################################
# Ahora si los analisis de concordancia


dir.create("Concordance")
inf<-0



 

  



for(k in 1:length(obsM_all)){
  
  
  stations <- levels(obsM_all[[k]][,3])
  
  for(i in 1:length(stations)){
    
    station <-stations[i]
    app <- filter(.data = app_all[[k]], app_all[[k]][,2]==station)[,-1]
    obs <- filter(.data = obsM_all[[k]], obsM_all[[k]][,3]==station)
    
    
    
   tmp.ccc <- epi.ccc(app[,4], obs[,4], ci = "z-transform", 
                       conf.level = 0.95, rep.measure = FALSE)
    
    tmp.lab <- data.frame(lab = paste("CCC: ", 
                                      round(tmp.ccc$rho.c[,1], digits = 2), " (95% CI ", 
                                      round(tmp.ccc$rho.c[,2], digits = 2), " - ",
                                      round(tmp.ccc$rho.c[,3], digits = 2), ")", sep = ""))
    
    
    z <- lm(obs[,4] ~ app[,4])
    alpha <- summary(z)$coefficients[1,1]
    beta <-  summary(z)$coefficients[2,1]
    tmp.lm <- data.frame(alpha, beta)
    
    
    tmp.m <- data.frame( mod =  paste("Model: ",    round(tmp.lm$alpha, digits = 2), "+", 
                                      round(tmp.lm$beta, digits = 3), "*x ; " , expression(R^2), "=", 
                                      round(summary(z)$r.squared, digits = 2),sep = ""))
    
    
   cr<-cbind.data.frame(app= app[,4], obs=obs[,4])
    
    coef<- data.frame(pearson =cor(x = cr$app, y = cr$obs, method = "pearson"),
                      spearman = cor(x = cr$app, y = cr$obs, method = "spearman"),
                      kendall =cor(x = cr$app, y = cr$obs, method = "kendall"))
    
    cr<-list( aWhere = data.frame(name="aWhere", j=app[,4]), obs = data.frame(name="obs", j=obs[,4]))
    f <- merge( x = cr$aWhere, y =cr$obs, all = TRUE)
    
    
    runs<-runs.test(x = f$j, alternative =  "left.sided" , plot = TRUE)$p.value
    
    SALIDA<-list(coef_cor= coef, Model = tmp.m, CCC_test = tmp.lab, test_Runs = runs)
    
    
    write.csv(SALIDA, paste("Concordance/con_",names(obsM_all)[k], "_", station,".csv",sep=""))
    
    
    if(k == 1 ){
      label1 <- "Precipitación (mm) aWhere" ; label2 <- "Precipitación (mm) Observada" 
      lb <- "Precipitación (mm)"
    }else if(k == 2 ){
      label1 <- expression("Temperatura Máxima (" * degree * C *") aWhere") ; label2 <- expression("Temperatura Máxima (" * degree * C *") Observada")
      lb<-expression("Temperatura Máxima (" * degree * C *")")
    }else if(k == 3){
      label <- expression("Temperatura Mínima (" * degree * C *") aWhere") ; label2 <- expression("Temperatura Máxima (" * degree * C *") Observada")
      lb<-expression("Temperatura Mínima (" * degree * C *")")
  }
    
    
    
    den<-ggplot(f , aes(x = j, colour = name)) +
      geom_density(position="identity", fill = NA, size = 1)  +
      scale_y_continuous(name = "Density") +
      theme_bw() + #ggtitle(label = paste("WW.test valor-p = ", round(runs,5), sep="")) +
      theme(plot.title = element_text(size = 10, face = "italic"),
            text = element_text(size = 12, family = "Tahoma")) +
      scale_colour_brewer(palette="Accent") + labs(x= lb, colour="Datos")
    
    
    
    tiff(paste("Concordance/density_",names(obsM_all)[k], "_", station,".tif",sep=""), height=450,width=800,res=80,
         compression="lzw") # height=1280, width=2048, pointsize=2, res=200,
    print(den)
    dev.off()
    
    
    p <- ggplot(data.frame(), aes(x = app[,4], y = obs[,4])) + 
      geom_point(colour="red") +  geom_abline(intercept = 0, slope = 1,  linetype = "dashed") +
      geom_abline(data = tmp.lm, aes(intercept = alpha, slope = beta), 
                  linetype = "dashed", colour="blue") + 
      xlab(label1) +
      ylab(label2) +
      theme_bw() + coord_fixed(ratio = 1 / 1) + ggtitle(tmp.m$mod,tmp.lab$lab) +
      theme(plot.title = element_text(size = 10, face = "italic"), plot.subtitle = 
              element_text(size = 10, face = "italic"))
    
    tiff(paste("Concordance/ccc_",names(obsM_all)[k], "_", station,".tif",sep=""), height=450,width=800,res=80,
         compression="lzw") # height=1280, width=2048, pointsize=2, res=200,
    print(p)
    dev.off()
    
    q<-  ggplot(tmp.ccc$blalt, aes(x = mean, y = delta)) + 
      geom_point(colour="red") +
      geom_hline(data = tmp.ccc$sblalt, aes(yintercept = lower), linetype = 2) +
      geom_hline(data = tmp.ccc$sblalt, aes(yintercept = upper), linetype = 2) +
      geom_hline(data = tmp.ccc$sblalt, aes(yintercept = est), linetype = 1) +
      xlab(names(app[[k]])[i]) +
      ylab(names(obsM_all[[k]])[i]) +
      theme_bw()   + xlab("Mean of Measurements") + ylab("Differences")
    
    tiff(paste("Concordance/BA_",names(obsM_all)[k], "_", station,".tif",sep=""), height=450,width=800,res=80,
         compression="lzw") # height=1280, width=2048, pointsize=2, res=200,
    print(q)
    dev.off()
    
    
    sal<- cbind.data.frame(sitio = station, var= names(obsM_all)[k], coef, R2 = round(summary(z)$r.squared, digits = 2), CCC = tmp.ccc$rho.c, runs = round(runs,5))
    inf<-rbind.data.frame(inf, sal)
  } 
}



inf <- inf[-1, ]
write.csv(x = inf, file = "Concordance.csv")





data1<-inf[-which(inf$var=="precip"),]
T<- ggplot(data1, aes(x = sitio, y = R2)) + geom_bar(stat = "identity") + 
  facet_grid(~var) + theme_bw()  + labs(x= "", y= expression(R^2), colour="Datos") +
  theme(axis.text=element_text(size=9, angle = 90, hjust = 0.9)) 
data2<-inf[-which(inf$var!="precip"),]
P<-ggplot(data2, aes(x = sitio, y = R2)) + geom_bar(stat = "identity") + 
  facet_grid(~var) + theme_bw()  + labs(x= "", y= expression(R^2), colour="Datos") +
  theme(axis.text=element_text(size=9, angle = 90, hjust = 0.9)) 
grid.arrange(T,P, layout_matrix = cbind(2, 1,1,1))






melt(inf, inf$pearson, inf$spearman, inf$kendall)

corI<-melt(inf[, 1:5], id=1:2)
names(corI) = c("sitio", "var", "Coeficiente", "Corr.")



corI1<-corI[-which(corI$var=="precip"),]
T<- ggplot(corI1, aes(x = sitio, y = Corr., fill=Coeficiente)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  facet_grid(~var) + theme_bw() +   
  theme(axis.text=element_text(size=9, angle = 90, hjust = 0.9), legend.position = "top") + 
  labs(x="", y="Correlación") + geom_hline(yintercept = 0, colour="gray") +
  scale_fill_brewer(palette="Accent") 
corI2<-corI[-which(corI$var!="precip"),]
P<-ggplot(corI2, aes(x = sitio, y = Corr., fill=Coeficiente)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  facet_grid(~var) + theme_bw() +   
  theme(axis.text=element_text(size=9, angle = 90, hjust = 0.9), legend.position = "top") + 
  labs(x="", y="Correlación") + geom_hline(yintercept = 0, colour="gray") +
  scale_fill_brewer(palette="Accent") 

grid.arrange(T,P, layout_matrix = cbind(2, 1,1,1))






pd <- position_dodge(0.1)



data1<-inf[-which(inf$var=="precip"),]
T<- ggplot(data1, aes(x=sitio, y=CCC.est)) + 
  geom_errorbar(aes(ymin=CCC.lower, ymax=CCC.upper), colour="firebrick4", width=.1, position=pd) +
  geom_point(position=pd, size=3, shape=21, fill="deepskyblue") + # 21 is filled circle
  theme_bw() + facet_grid(~var) +
  theme(axis.text=element_text(size=9, angle = 90, hjust = 0.9), legend.position = "top") + 
  labs(x="", y="Concordancia") + geom_hline(yintercept = c(0, 0.3) , colour="#990000", linetype="dashed")
data2<-inf[-which(inf$var!="precip"),]
P<-ggplot(data2, aes(x=sitio, y=CCC.est)) + 
  geom_errorbar(aes(ymin=CCC.lower, ymax=CCC.upper), colour="firebrick4", width=.1, position=pd) +
  geom_point(position=pd, size=3, shape=21, fill="deepskyblue") + # 21 is filled circle
  theme_bw() + facet_grid(~var) +
  theme(axis.text=element_text(size=9, angle = 90, hjust = 0.9), legend.position = "top") + 
  labs(x="", y="Concordancia") + geom_hline(yintercept = c(0, 0.3) , colour="#990000", linetype="dashed")
grid.arrange(T,P, layout_matrix = cbind(2, 1,1,1))





ggsave(filename = "ccc1.png",  width = 7, height = 4.5 ,units = "in")











#



# 








View(app_all[[1]])
View(obsM_all[[1]])

names(obsM_all[[1]])[4] = names(app_all[[1]])[5]
names(obsM_all[[1]])[3]  = names(app_all[[1]])[2]
names(obsM_all$tmax)[[3]] = names(app_all$tmax)[2]

require(zoo)

names(obsM_all$tmin)[[3]]= names(app_all$tmin)[2]

app <- lapply(app_all, function(x){x <- mutate(x, id = "aWhere", date = as.yearmon(paste0(x$month,"-" , x$year ), format="%m-%Y"))})
obsM_all <- lapply(obsM_all, function(x){x <- mutate(x, id = "Obs.", date = as.yearmon(paste0(x$month,"-" , x$year ), format="%m-%Y"))})


app$precip<- app$precip[,-1]
app$precip

test <- mapply(FUN = function(x,y){merge(x, y, all=TRUE)}, app, obsM_all, SIMPLIFY = FALSE)

ggplot(test$tmax, aes(as.yearmon(date),  tmax, colour=id, linetype = id)) + geom_line() +
  facet_grid(~Sitio) + 
  theme_bw() + labs(x="year", y= expression("Temperatura Máxima (" * degree * C *")"),colour="", linetype = "")+
  scale_colour_manual(values = c("black", "red")) + theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggsave("Temp_max_Hon.png", width = 15, height = 4 )



ggplot(test$tmin, aes(as.yearmon(date),  tmin, colour=id, linetype = id)) + geom_line() +
  facet_grid(~Sitio) + 
  theme_bw() + labs(x="year", y= expression("Temperatura Mínima (" * degree * C *")"),colour="", linetype = "")+
  scale_colour_manual(values = c("black", "red")) + theme(axis.text.x = element_text(angle = 90, hjust = 1))


ggsave("Temp_min_Hon.png", width = 15, height = 4 )






ggplot(test$precip, aes(date,  precip, colour=id, linetype = id)) +  geom_line() +
  facet_grid(~Sitio) + # ylim(0,600)+
  theme_bw() + labs(x="year", y= "Precipitación (mm)", colour="", linetype = "")+
  scale_colour_manual(values = c("black", "red")) +  scale_x_yearmon(format = "%Y") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))



ggsave("Precip_Hon.png", width = 15, height = 4 )















