#Initialisation du projet

station<-read.table("station.txt",header=T)
attach(station)
plot(station[,2:4])


#Mise en place de l'ACP

#install.packages("PCAmixdata")
#library(PCAmixdata)
require(PCAmixdata)

resACP <- PCAmix(station)
resACP
round(resACP$eig,digits=2)

plot(resACP,axes=c(1,2),choice="ind")
plot(resACP,axes=c(1,2),choice="cor")
plot(resACP,axes=c(1,2),choice="sqload")

resACP$ind
round(resACP$ind$cos2,digits=3)

#Régression linéaire multiple

res<-lm(ventes~.,data = station[,1:4])
summary(res)
step(res,trace=TRUE)

resfinal<-lm(ventes~nbpompes+trafic)
summary(resfinal)

shapiro.test(resfinal$residuals)
plot(resfinal$fitted,resfinal$residuals)
abline(h=0)

outliers<-which(resfinal$residuals < -5)
outliers
resfinal[outliers]
resfinal$residuals[outliers]
resfinal2 <- lm(ventes~nbpompes+trafic, data=station[-outliers,1:4])
summary(resfinal2)
plot(resfinal2$fitted,resfinal2$residuals)
abline(h=0)

#Prédiction du modèle

nbp<-seq(from=3,to=21,by=1) #Estimation des valeurs grâce au summary
nbp
traf<-seq(from=8,to=28,by=1) #Estimation des valeurs grâce au summary
traf

for(k in nbp){
  res.conf <- predict(resfinal2,data.frame(nbpompes=k,trafic=traf),interval="conf",level=0.95)
  print(round(cbind(k,traf,res.conf),digits=1))
  
  res.pred <- predict(resfinal2,data.frame(nbpompes=k,trafic=traf),interval="pred",level=0.95)
  print(round(cbind(k,traf,res.pred),digits=1))
}

#Régression linéaire à une variable

res1 <- lm(ventes~nbpompes)
summary(res1)
res2 <- lm(ventes~nbconc)
summary(res2)
res3 <- lm(ventes~trafic)
summary(res3)