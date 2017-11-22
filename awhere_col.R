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
setwd("C:/Users/AESQUIVEL/Google Drive/Informes/A_Where/Colombia")
getwd()

# liste los directorios en la carpeta donde se encuentran los datos de AWhere
folders <- list.dirs(path = "C:/Users/AESQUIVEL/Google Drive/Informes/A_Where/Colombia/app")
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
  
  setwd("C:/Users/AESQUIVEL/Google Drive/Informes/A_Where/Colombia/app") # ubiquese en la carpeta app
  write.csv(test2, paste(LastElement, "_monthly.csv", sep=""), row.names = FALSE) # cree el archivo mensual del sitio
  
  }

# Corra para todos los sitios guardados en folders 
sapply(X= folders, FUN = daily_data)








####

getwd() # muestre el directorio de trabajo
# liste los archivos en la carpteta donde se encuentran los archivos de AWhere
files_app <- list.files(path = "C:/Users/AESQUIVEL/Google Drive/Informes/A_Where/Colombia/app",pattern = ".csv")
app_routes <- paste("C:/Users/AESQUIVEL/Google Drive/Informes/A_Where/Colombia/app/", files_app, sep="")

# liste los archivos en la carpteta donde se encuentran los archivos de obsevados
files_obs <- list.files(path = "C:/Users/AESQUIVEL/Google Drive/Informes/A_Where/Colombia/obs", pattern = ".csv")
obs_routes <- paste("C:/Users/AESQUIVEL/Google Drive/Informes/A_Where/Colombia/obs/", files_obs, sep="")




# Lea los arhivos
app_all<-lapply(X= app_routes, FUN=read.csv)
Split <- strsplit(files_app, "_") # fragmente los nombres de los achivos (_)
Element <- unique(rapply(Split, function(x) head(x, 1))) # Extraiga el nombre de sitio
names(app_all)<-Element #

# De las posiciones de esos años 
rows1<-which(app_all[[1]]$year>2014)
rows2<-which(app_all[[1]]$year>2015)

# c("Cerete",  "Espinal", "Ibague" , "LaUnion" ,"Lorica" , "Yopal")

# Elimine de acuerdo al sitio las fechas que no se encuentran en los datos obs
for(i in 1:length(app_all)){
  if(names(app_all)[i]=="Cerete" |names(app_all)[i]== "Lorica" |names(app_all)[i]== "LaUnion"){
    app_all[[i]] <- app_all[[i]][-rows1,]
  } else if(names(app_all)[i]=="Espinal" |names(app_all)[i]== "Ibague" |names(app_all)[i]== "Yopal"){
    app_all[[i]] <- app_all[[i]][-rows2,]
  } 
}


#app_all<-lapply(app_all, function(x, rows) x[-rows,])






#### daily data obs

# Lea los datos observados
obs_all<-lapply(X= obs_routes, FUN=read.csv)
Split <- strsplit(files_obs, "\\.") # Fails because of 'Trailing backslash'
Element <- unique(rapply(Split, function(x) head(x, 1))) # EXTRAIGA LOS NOMBRES DE LOS SITIOS
names(obs_all)<-Element #asigne los nombres a los objetos de la lista
names(app_all) == names(obs_all) # revise que los nombres de las dos listas sean iguales


#### monthly data obs 

monthly<- function(base){
  # Agregue los datos de los sitios observados mensualmente por la media
  test2<-aggregate(x = base, by = list(base$month, base$year) , FUN = mean )
  # Agregue los datos de los sitios observados mensualmente sumando
  precip <- aggregate(x =  base , by = list(base$month, base$year) , FUN = sum )
  precip <- precip$precip # guarde solo los valores de precip 
  
  Null_names<-c("Group.1", "Group.2",  "day") # cree un vector de nombres
  test2<-test2[,  !(names(test2) %in% Null_names)] # Elimine las columnas con esos nombres
  test2$precip <- precip
  
  base <- test2
return(base)}

obsM_all<-lapply(X = obs_all, FUN=monthly)

rows<-which(obsM_all[[1]]$year<2005) # posciones de los años menores del 2005
obsM_all<-lapply(obsM_all, function(x) x[-rows,]) # elimine la informacion antes del 2005


names(obsM_all) = names(obs_all) # asigne a amabas listas los mismos nombres



boxplot(obsM_all[[1]])



#for(i in 1:length(obsM_all)){obsM_all[[i]]<-cbind.data.frame(Sitio= names(obsM_all)[[i]], obsM_all[[i]])}
#obs<-Reduce(function(x,y){merge(x,y, all=TRUE)}, obsM_all)
#obs <- mutate(obs, Set = "obs")




#for(i in 1:length(app)){app[[i]]<-cbind.data.frame(Sitio= names(app)[[i]], app[[i]])}
#app<-Reduce(function(x,y){merge(x,y, all=TRUE)}, app)
#app <- mutate(obs, Set = "aWhere")

#COMP<-merge(obs,app, all=TRUE)

#a<- ggplot(COMP, aes(x=Sitio, y=precip, fill=Set)) + geom_boxplot()  + 
#  labs(x="", y="Precipitación (mm)", fill="") + geom_hline(yintercept = 0, colour="gray") +
#  scale_fill_brewer(palette="Accent") + theme_bw() +
#  ggtitle(" ", "Precipitación")  + theme(axis.text=element_text(size=9, angle = 90, hjust = 0.9), legend.position = "top")


#b<- ggplot(COMP, aes(x=Sitio, y=tmax, fill=Set)) + geom_boxplot()  + 
#  labs(x="",  fill="") +
#  scale_fill_brewer(palette="Accent") + theme_bw() +
#  ggtitle(" ", "Temperatura Máxima") + 
#  ylab(expression("Temperatura (" * degree * C *")"))+ 
#  theme(axis.text=element_text(size=9, angle = 90, hjust = 0.9), legend.position = "top")


#c<- ggplot(COMP, aes(x=Sitio, y=tmin, fill=Set)) + geom_boxplot() + 
#  labs(x="",  fill="") +
#  scale_fill_brewer(palette="Accent") + theme_bw() +
#  ggtitle(" ", "Temperatura Mínima") + 
#  ylab(expression("Temperatura (" * degree * C *")")) + 
#  theme(axis.text=element_text(size=9, angle = 90, hjust = 0.9), legend.position = "top") 


#d<- ggplot(COMP, aes(x=Sitio, y=tmin, fill=Set)) + geom_boxplot() + 
#  labs(x="",  fill="") +
#  scale_fill_brewer(palette="Accent") + theme_bw() +
#  ggtitle(" ", "Radiación Solar") + 
#  ylab(expression("Radiación Solar ( MJ" * m^-1 * day^-1 *")")) + 
#  theme(axis.text=element_text(size=9, angle = -90, hjust = 1), legend.position = "top") 


#grid.arrange(a, b, c, d  ,ncol=4)



######Validación Cruzada

# cree un directorio que se llame cross_Validacion 
dir.create("cross_validation")
 
# ORGANICE LA BASE DE DATOS CON OTRO ORDEN DE COLUMNAS
 obsM_all<-lapply(obsM_all, function(x) x[,names(obsM_all[[3]])])
 app<- lapply(app_all, function(x) x[,names(obsM_all[[3]])])
 str(app) # MUESTRE UN RESUMEN DE LA BASE DE DATOS

 # Cree dos matrices de indicadores
 cor.cv.all = matrix(NA,51,length(obsM_all[[1]])) # 51 número de veces que se hizo
 rmse.cv.all = matrix(NA,51,length(obsM_all[[1]])) # la validacion 

# Esta funcion hace la cross_v y guarda indicadores
cross_v = function(k,i){
  # guarde una imagen en .tiff
   tiff(paste0("cross_validation/",names(obsM_all)[k], "_",names(obsM_all[[k]])[i] ,"_cv.tiff"),compression = 'lzw',height = 5,width = 7,units="in", res=100)
   
   for(j in 1:50){ # repita 50 veces el proceso
     data.model = as.data.frame(cbind("y"=obsM_all[[k]][,i],"x"= app[[k]][,i])) # extraiga los datos
     na.sample = nrow(data.model)*sample(seq(0.01,0.2,0.01),1,replace = T) # seleccione el total de información a eliminar
     na.sample = round(na.sample,0) # redondee el número a uno entero 
     
     pos.na = sample(1:nrow(data.model),na.sample,replace=F) # extraiga las posiciones de las filas a eliminar
     
     data.model$y[pos.na] = NA # convierta esas filas en NA
     model = lm(data=data.model,formula = y~x)
     
     x = app[[k]][pos.na,i]
     data_predict = predict(model,as.data.frame(x))
     data_predict[data_predict<0] = 0
     
     
     if(j==1){
       
       plot(obsM_all[[k]][pos.na,i],data_predict,xlab="Observed_stations",ylab="Fitted", main=names(obsM_all[[k]])[i])
       cor.cv = round(cor(obsM_all[[k]][pos.na,i],data_predict,  use="complete.obs"),3)
       rmse.cv <- round(sqrt(mean((obsM_all[[k]][pos.na,i]-data_predict)^2,na.rm=T)), 2)
       
     }
     
     points(obsM_all[[k]][pos.na,i],data_predict, pch=j, col="blue")
     cor.cv <- rbind(cor.cv, round(cor(obsM_all[[k]][pos.na,i],data_predict, use="complete.obs" ),3))
     rmse.cv <- rbind(rmse.cv, round(sqrt(mean((obsM_all[[k]][pos.na,i]-data_predict)^2,na.rm=T)), 2))
     
     
   }
   
   cor.cv.all[,i] = cor.cv
   rmse.cv.all[,i] = rmse.cv
   eqn <- bquote(r == .(round(mean(na.omit(cor.cv.all[,i])),3)) * "," ~~ RMSE == .(round(na.omit(mean(rmse.cv.all[,i])),3)))
   legend('bottomright', legend = eqn, bty = 'n',cex=2)
   dev.off()
   
   assign("cor.cv.all",cor.cv.all,.GlobalEnv)
   assign("rmse.cv.all",rmse.cv.all,.GlobalEnv)
   
   print(eqn)
 }  

data<-0
for(k in 1:length(obsM_all)){
   for(i in 3:ncol(obsM_all[[k]])){
     cross_v(k,i)    
     write.csv(cor.cv.all[,i], paste("r_", names(obsM_all)[k], "_",names(obsM_all[[k]])[i], ".csv",sep=""))
     write.csv(rmse.cv.all[,i], paste("rmse_", names(obsM_all)[k], "_",names(obsM_all[[k]])[i], ".csv",sep=""))
     datac<-cbind.data.frame(Station = names(obsM_all)[k], ind =  names(obsM_all[[k]])[i]  , mean_r = round(mean(na.omit(cor.cv.all[,i]) ),2)   ,   rmse = round(mean(na.omit(rmse.cv.all[,i]) ),2))
     data<-rbind.data.frame(data, datac) 
   } 
}

 
data <- data[-1,]

write.csv(data, "cv.csv")

# x11()
lab <- as_labeller(c("tmax" = "T.Max" ,  "tmin" = "T.Min", "precip" = "Prec.",   "srad"= "S.Rad"))
ggplot(data, aes(x = Station, y = mean_r)) + geom_point(colour ="red") + 
  theme_bw() +  facet_grid(~ ind, labeller = labeller(ind = lab)) + 
  ylim(-0.25,1) + labs(x="Sitio", y= "Coef. Correlación - Validación Cruzada") +
  theme(axis.text=element_text(size=9, angle = 90, hjust = 1)) + geom_hline(yintercept = 0, colour="gray")

ggsave("Cv.cor.png", width = 8.5, height = 3.5 ,units = "in")



data1<-data[-which(data$ind=="precip"),]
p1<-ggplot(data1, aes(x = Station, y = rmse)) + geom_point(colour ="red") + 
  theme_bw() +  facet_grid(~ ind, labeller = labeller(ind = lab)) + 
  labs(x="Sitio", y= "RMSE") +
  theme(axis.text=element_text(size=9, angle = 90, hjust = 1)) + 
  geom_hline(yintercept = 0, colour="gray")

data2<-data[-which(data$ind!="precip"),]
p2<-ggplot(data2, aes(x = Station, y = rmse)) + geom_point(colour ="red") + 
  theme_bw() +  facet_grid(~ ind, labeller = labeller(ind = lab)) + 
  labs(x="Sitio", y= "RMSE") +
  theme(axis.text=element_text(size=9, angle = 90, hjust = 1)) + 
  geom_hline(yintercept = 0, colour="gray")

grid.arrange(p1,p2, layout_matrix = cbind(2, 1,1,1))













###########################################
# Ahora si los analisis de concordancia


dir.create("Concordance")
inf<-0

for(k in 1:length(obsM_all)){
  for(i in 3:dim(obsM_all[[k]])[2]){
    tmp.ccc <- epi.ccc(app[[k]][,i], obsM_all[[k]][,i], ci = "z-transform", 
                       conf.level = 0.95, rep.measure = FALSE)
    
    tmp.lab <- data.frame(lab = paste("CCC: ", 
                                      round(tmp.ccc$rho.c[,1], digits = 2), " (95% CI ", 
                                      round(tmp.ccc$rho.c[,2], digits = 2), " - ",
                                      round(tmp.ccc$rho.c[,3], digits = 2), ")", sep = ""))
    
    
    z <- lm(obsM_all[[k]][,i] ~ app[[k]][,i])
    alpha <- summary(z)$coefficients[1,1]
    beta <-  summary(z)$coefficients[2,1]
    tmp.lm <- data.frame(alpha, beta)
    
    
    tmp.m <- data.frame( mod =  paste("Model: ",    round(tmp.lm$alpha, digits = 2), "+", 
                                                          round(tmp.lm$beta, digits = 3), "*x ; " , expression(R^2), "=", 
                                                          round(summary(z)$r.squared, digits = 2),sep = ""))
    
    
    cr<-cbind.data.frame(app= app[[k]][,i], obs=obsM_all[[k]][,i])
    
    coef<- data.frame(pearson =cor(x = cr$app, y = cr$obs, method = "pearson"),
                      spearman = cor(x = cr$app, y = cr$obs, method = "spearman"),
                      kendall =cor(x = cr$app, y = cr$obs, method = "kendall"))
  
    cr<-list( aWhere = data.frame(name="aWhere", j=app[[k]][,i]), obs = data.frame(name="obs", j=obsM_all[[k]][,i]))
    f <- merge( x = cr$aWhere, y =cr$obs, all=T)
 
    
    runs<-runs.test(x = f$j, alternative =  "left.sided" , plot = TRUE)$p.value
    
    SALIDA<-list(coef_cor= coef, Model = tmp.m, CCC_test = tmp.lab, test_Runs = runs)
    
    
    write.csv(SALIDA, paste("Concordance/con_",names(obsM_all)[k], "_", names(obsM_all[[k]])[i],".csv",sep=""))
    
    
    if(i == 3 ){
      label1 <- expression("Temperatura Máxima (" * degree * C *") aWhere") ; label2 <- expression("Temperatura Máxima (" * degree * C *") Observada")
      lb<-expression("Temperatura Máxima (" * degree * C *")")
    }else if(i == 4 ){
      label <- expression("Temperatura Mínima (" * degree * C *") aWhere") ; label2 <- expression("Temperatura Máxima (" * degree * C *") Observada")
      lb<-expression("Temperatura Mínima (" * degree * C *")")
    }else if(i == 5){
      label1 <- "Precipitación (mm) aWhere" ; label2 <- "Precipitación (mm) Observada" 
      lb <- "Precipitación (mm)"
    }else if(i == 6){
      label1 <- "Radiación Solar () aWhere"  ; label2 <- "Radiación Solar () Observada"
      lb<-expression("Radiación Solar ( MJ" * m^-1 * day^-1 *")")
    }
    
    
   

    den<-ggplot(f , aes(x = j, colour = name)) +
      geom_density(position="identity", fill = NA, size = 1)  +
      scale_y_continuous(name = "Density") +
      theme_bw() + #ggtitle(label = paste("WW.test valor-p = ", round(runs,5), sep="")) +
      theme(plot.title = element_text(size = 10, face = "italic"),
            text = element_text(size = 12, family = "Tahoma")) +
      scale_colour_brewer(palette="Accent") + labs(x= lb, colour="Datos")
    
    
    
    tiff(paste("Concordance/density_",names(obsM_all)[k], "_", names(obsM_all[[k]])[i],".tif",sep=""), height=450,width=800,res=80,
         compression="lzw") # height=1280, width=2048, pointsize=2, res=200,
    print(den)
    dev.off()
    
    
   p <- ggplot(data.frame(), aes(x = app[[k]][,i], y = obsM_all[[k]][,i])) + 
      geom_point(colour="red") +  geom_abline(intercept = 0, slope = 1,  linetype = "dashed") +
      geom_abline(data = tmp.lm, aes(intercept = alpha, slope = beta), 
                  linetype = "dashed", colour="blue") + 
      xlab(label1) +
      ylab(label2) +
      theme_bw() + coord_fixed(ratio = 1 / 1) + ggtitle(tmp.m$mod,tmp.lab$lab) +
      theme(plot.title = element_text(size = 10, face = "italic"), plot.subtitle = 
              element_text(size = 10, face = "italic"))
    
    tiff(paste("Concordance/ccc_",names(obsM_all)[k], "_", names(obsM_all[[k]])[i],".tif",sep=""), height=450,width=800,res=80,
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
   
   tiff(paste("Concordance/BA_",names(obsM_all)[k], "_", names(obsM_all[[k]])[i],".tif",sep=""), height=450,width=800,res=80,
        compression="lzw") # height=1280, width=2048, pointsize=2, res=200,
   print(q)
   dev.off()
   
   
   sal<- cbind.data.frame(sitio = names(obsM_all)[k], var= names(obsM_all[[k]])[i], coef, R2 = round(summary(z)$r.squared, digits = 2), CCC = tmp.ccc$rho.c, runs = round(runs,5))
   inf<-rbind.data.frame(inf, sal)
  } 
}


inf <- inf[-1, ]
write.csv(x = inf, file = "Concordance.csv")

lab <- as_labeller(c("tmax" = "T.Max" ,  "tmin" = "T.Min", "precip" = "Prec.",   "srad"= "S.Rad"))
ggplot(inf, aes(x = sitio, y = R2)) + geom_bar(stat = "identity") + 
  facet_grid(~var, labeller =  labeller(var=lab)) + theme_bw()  + labs(x= "", y= expression(R^2), colour="Datos") +
  theme(axis.text=element_text(size=9, angle = 90, hjust = 0.9)) 


ggsave(filename = "r2.png",  width = 7, height = 3 ,units = "in")





melt(inf, inf$pearson, inf$spearman, inf$kendall)

corI<-melt(inf[, 1:5], id=1:2)
names(corI) = c("sitio", "var", "Coeficiente", "Corr.")

ggplot(corI, aes(x = sitio, y = Corr., fill=Coeficiente)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  facet_grid(~var, labeller = labeller(var=lab)) + theme_bw() +   
  theme(axis.text=element_text(size=9, angle = 90, hjust = 0.9), legend.position = "top") + 
  labs(x="", y="Correlación") + geom_hline(yintercept = 0, colour="gray") +
  scale_fill_brewer(palette="Accent") 


ggsave(filename = "corI.png",  width = 7, height = 4.5 ,units = "in")





pd <- position_dodge(0.1)


ggplot(inf, aes(x=sitio, y=CCC.est)) + 
  geom_errorbar(aes(ymin=CCC.lower, ymax=CCC.upper), colour="firebrick4", width=.1, position=pd) +
  geom_point(position=pd, size=3, shape=21, fill="deepskyblue") + # 21 is filled circle
  theme_bw() + facet_grid(~var, labeller = labeller(var=lab)) +
  theme(axis.text=element_text(size=9, angle = 90, hjust = 0.9), legend.position = "top") + 
  labs(x="", y="Concordancia") + geom_hline(yintercept = c(0, 0.3) , colour="#990000", linetype="dashed")


ggsave(filename = "ccc1.png",  width = 7, height = 4.5 ,units = "in")


   







# 

View(app[[1]])
View(obsM_all[[1]])



merge(app[[1]], obsM_all[[1]])


app <- lapply(app, function(x){x <- mutate(x, id = "aWhere", date = as.yearmon(paste0(x$month,"-" , x$year ), format="%m-%Y"))})
obsM_all <- lapply(obsM_all, function(x){x <- mutate(x, id = "Obs.", date = as.yearmon(paste0(x$month,"-" , x$year ), format="%m-%Y"))})

test <- mapply(FUN = function(x,y){merge(x, y, all=TRUE)}, app, obsM_all, SIMPLIFY = FALSE)
for(i in 1:length(test)){ test[[i]]<- data.frame(zone = names(test)[i], test[[i]])}
test <-  do.call("rbind.data.frame", test)

  





#test[[1]]$date = as.Date(test[[1]]$date, "%M-%Y")
ggplot(test, aes(as.yearmon(date),  tmax, colour=id, linetype = id)) + geom_line() +
 facet_grid(~zone) + 
   theme_bw() + labs(x="year", y= expression("Temperatura Máxima (" * degree * C *")"),colour="", linetype = "")+
  scale_colour_manual(values = c("black", "red")) + theme(axis.text.x = element_text(angle = 90, hjust = 1))


ggsave("Temp_max_Col.png", width = 15, height = 4 )



ggplot(test, aes(as.yearmon(date),  tmin, colour=id, linetype = id)) + geom_line() +
  facet_grid(~zone) + 
  theme_bw() + labs(x="year", y= expression("Temperatura Mínima (" * degree * C *")"),colour="", linetype = "")+
  scale_colour_manual(values = c("black", "red")) + theme(axis.text.x = element_text(angle = 90, hjust = 1))


ggsave("Temp_min_Col.png", width = 15, height = 4 )



ggplot(test, aes(as.yearmon(date),  precip, colour=id, linetype = id)) + geom_line() +
  facet_grid(~zone) + 
  theme_bw() + labs(x="year", y= "Precipitación (mm)", colour="", linetype = "")+
  scale_colour_manual(values = c("black", "red")) + theme(axis.text.x = element_text(angle = 90, hjust = 1))


ggsave("Precip_Col.png", width = 15, height = 4 )






ggplot(test, aes(as.yearmon(date),  srad, colour=id, linetype = id)) + geom_line() +
  facet_grid(~zone) + 
  theme_bw() + labs(x="year", y= expression("Radiación Solar ( MJ" * m^-1 * day^-1 *")"), colour="", linetype = "")+
  scale_colour_manual(values = c("black", "red")) + theme(axis.text.x = element_text(angle = 90, hjust = 1))


ggsave("Rad_Col.png", width = 15, height = 4 )








