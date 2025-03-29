#Ouvrir le fichier et les librairies 


cyclisme = read.csv('cyclismePSY6002.csv')

#Créer une copie de notreZbanque 
cyclisme -> cyclismeBrute

#Vérifier la classe des variables
lapply(cyclisme, class)


#1 : Vérifier l’entrée des données
library(sjmisc)
frq(cyclismeBrute,'age','sexe','pays','langue','langsou','scolar','statut','enfant','velo','coutvelo', "estim1", "estim2", "estim3", "estim4", 
    "equil1", "equil2", "equil3", "equil4",
    "engsoc1", "engsoc2", "engsoc3", "engsoc4",
    "sociab1", "sociab2", "sociab3", "sociab4",
    "soi1", "soi2", "soi3", "soi4",
    "bonheu1", "bonheu2", "bonheu3", "bonheu4", "bonheu5")


#2 : Créer des variables 
# sexe
cyclisme$sexeNUM<-as.numeric(unlist(cyclisme$sexe))
cyclisme$sexe<-factor(cyclisme$sexeNUM, levels = c(1,2), labels = c("Homme","Femme"))

# pays
cyclisme$paysNUM<-as.numeric(unlist(cyclisme$pays))
cyclisme$pays<-factor(cyclisme$paysNUM, levels = c(1,2,3,4,5,6,7,8,9,10), 
                      labels = c("Canada","Etats-Unis", "Mexique", "France", "Belgique", "Grande-Bretagne", "Algérie", "Maroc", "Vietnam", "Chine"))
# langue
cyclisme$langueNUM<-as.numeric(unlist(cyclisme$langue))
cyclisme$langue<-factor(cyclisme$langueNUM, levels = c(1,2,3,4,5), labels = c("Français","Anglais", "Portugais", "Espagnol", "Arabe"))

# langsou
cyclisme$langsouNUM<-as.numeric(unlist(cyclisme$langsou))
cyclisme$langsou<-factor(cyclisme$langsouNUM, levels = c(1,2,3,4,5), labels = c("Français","Anglais", "Portugais", "Espagnol", "Arabe"))

# scolar
cyclisme$scolarNUM<-as.numeric(unlist(cyclisme$scolar))
cyclisme$scolar<-factor(cyclisme$scolarNUM, levels = c(1,2,3,4,5,6,7,8,9,10), labels = c("Primaire","Secondaire", "Secondaire général", "Secondaire professionnel", "Collégial général", "Collégial professionnel", "Certificat", "Universitaire, 1er cycle", "Universitaire, 2e cycle", "Universitaire, 3e cycle"))

# statut
cyclisme$statutNUM<-as.numeric(unlist(cyclisme$statut))
cyclisme$statut<-factor(cyclisme$statutNUM, levels = c(1,2,3,4), labels = c("Célibataire","Marié ou vivant avec une personne", "Séparé et vivant seul", "Séparé et vivant avec une personne"))

# enfant
cyclisme$enfantNUM<-as.numeric(unlist(cyclisme$enfant))
cyclisme$enfant<-factor(cyclisme$enfantNUM, levels = c(1,2,3,4,5,6), labels = c("Non","1 enfant", "2 enfants", "3 enfants", "4 enfants", "5 enfants ou plus"))

# velo
cyclisme$veloNUM<-as.numeric(unlist(cyclisme$velo))
cyclisme$velo<-factor(cyclisme$veloNUM, levels = c(1,2), labels = c("Vélo de route","Vélo de montagne"))

# coutvelo
cyclisme$coutveloNUM<-as.numeric(unlist(cyclisme$coutvelo))
cyclisme$coutvelo<-factor(cyclisme$coutveloNUM, levels = c(1,2,3), labels = c("0 à 499","500 à 999", "1000 +"))



#Recoder les items inversés : Pas d'item à inverser

#Créer des variables
cyclisme$estim <- rowMeans(cyclisme[c("estim1","estim2","estim3","estim4")], n=.75)
cyclisme$equil <- rowMeans(cyclisme[c("equil1","equil2","equil3","equil4")], n=.75)
cyclisme$engsoc <- rowMeans(cyclisme[c("engsoc1","engsoc2","engsoc3","engsoc4")], n=.75)
cyclisme$sociab <- rowMeans(cyclisme[c("sociab1","sociab2","sociab3","sociab4")], n=.75)
cyclisme$soi <- rowMeans(cyclisme[c("soi1","soi2","soi3","soi4")], n=.75)
cyclisme$bonheu <- rowMeans(cyclisme[c("bonheu1","bonheu2","bonheu3","bonheu4")], n=.75)

library(labelled)
var_label(cyclisme$estim) <- 'Moyenne estim'
var_label(cyclisme$equil) <- 'Moyenne equil'
var_label(cyclisme$engsoc) <- 'Moyenne engsoc'
var_label(cyclisme$sociab) <- 'Moyenne sociab'
var_label(cyclisme$soi) <- 'Moyenne soi'
var_label(cyclisme$bonheu) <- 'Moyenne bonheu'


#3 : Vérifier les données manquantes
mean(is.na(cyclisme$estim))
mean(is.na(cyclisme$equil))
mean(is.na(cyclisme$engsoc))
mean(is.na(cyclisme$sociab))
mean(is.na(cyclisme$soi))
mean(is.na(cyclisme$bonheu))
                              


#4Vérifier la variabilité des variables catégorielles
sjmisc::frq(cyclisme, "sexe", "pays", "langue", "langsou", "scolar", "statut", "enfant", "velo", "coutvelo")

#5: Variabilité des variable continues
sjmisc::frq(cyclisme, estim, equil, engsoc, sociab, soi, bonheu)

#6. Vérifier normalité
jmv::descriptives(cyclisme,
                  vars = vars(estim, equil, engsoc, sociab, soi, bonheu),
                  sd = TRUE, range = TRUE, 
                  skew = TRUE, kurt = TRUE, hist = TRUE)



#Scores extrêmes
cyclisme$age.z<-scale(cyclisme$age)
cyclisme$age.z <-as.numeric(unlist(cyclisme$age.z))
sjmisc::frq(cyclisme, age.z)

cyclisme$estim.z<-scale(cyclisme$estim)
cyclisme$estim.z <-as.numeric(unlist(cyclisme$estim.z))
sjmisc::frq(cyclisme,estim.z)

cyclisme$equil.z<-scale(cyclisme$equil)
cyclisme$equil.z <-as.numeric(unlist(cyclisme$equil.z))
sjmisc::frq(cyclisme,equil.z)

cyclisme$engsoc.z<-scale(cyclisme$engsoc)
cyclisme$engsoc.z <-as.numeric(unlist(cyclisme$engsoc.z))
sjmisc::frq(cyclisme,engsoc.z)

cyclisme$sociab.z<-scale(cyclisme$sociab)
cyclisme$sociab.z <-as.numeric(unlist(cyclisme$sociab.z))
sjmisc::frq(cyclisme,sociab.z)

cyclisme$soi.z<-scale(cyclisme$soi)
cyclisme$soi.z <-as.numeric(unlist(cyclisme$soi.z))
sjmisc::frq(cyclisme,soi.z)

cyclisme$bonheu.z<-scale(cyclisme$bonheu)
cyclisme$bonheu.z <-as.numeric(unlist(cyclisme$bonheu.z))
sjmisc::frq(cyclisme,bonheu.z)


cyclisme$estim.z <- ifelse(abs(cyclisme$estim.z)>3.29,NA,cyclisme$estim.z)
cyclisme$equil.z <- ifelse(abs(cyclisme$equil.z )>3.29,NA,cyclisme$equil.z)
cyclisme$sociab.z <- ifelse(abs(cyclisme$sociab.z )>3.29,NA,cyclisme$sociab.z)
cyclisme$bonheu.z <- ifelse(abs(cyclisme$bonheu.z )>3.29,NA,cyclisme$bonheu.z)

mean(is.na(cyclisme$estim.z))
mean(is.na(cyclisme$equil.z))
mean(is.na(cyclisme$sociab.z))
mean(is.na(cyclisme$bonheu.z))


##Facteurs
#Présence des items importants  
efa <- subset(cyclisme, select=c("estim1", "estim2", "estim3", "estim4", "equil1", 
                                 "equil2", "equil3", "equil4",
                                 "engsoc1", "engsoc2", "engsoc3", "engsoc4",
                                 "sociab1", "sociab2", "sociab3", "sociab4",
                                 "soi1", "soi2", "soi3", "soi4",
                                 "bonheu1", "bonheu2", "bonheu3", "bonheu4", "bonheu5"))

mean(psych::smc(efa))



#Taille de l’échantillon et données manquantes
library(sjmisc)
frq(cyclismeBrute,'age','sexe','pays','langue','langsou','scolar','statut','enfant','velo','coutvelo', "estim1", "estim2", "estim3", "estim4", 
    "equil1", "equil2", "equil3", "equil4",
    "engsoc1", "engsoc2", "engsoc3", "engsoc4",
    "sociab1", "sociab2", "sociab3", "sociab4",
    "soi1", "soi2", "soi3", "soi4",
    "bonheu1", "bonheu2", "bonheu3", "bonheu4", "bonheu5")

#Étendue des items : les corrélations pourraient être surestimées ou sous-estimées


#Scores extrêmes au niveau des items

qchisq(.001, df=25 , lower.tail=FALSE)
sort(psych::outlier(mahal <- subset(cyclisme, select =c("estim1", "estim2", "estim3", "estim4","equil1", "equil2", 
                                                        "equil3", "equil4", "engsoc1", "engsoc2", "engsoc3", "engsoc4",   
                                                        "sociab1", "sociab2", "sociab3", "sociab4", "soi1", "soi2", "soi3", "soi4",
                                                        "bonheu1", "bonheu2", "bonheu3", "bonheu4", "bonheu5"))))
                                    
##Postulats de base
#Indépendance des scores

#Normalité des items
library(ggplot2)
ggplot(cyclisme, aes(x=estim.z))+ geom_histogram()+ labs(title="Estime de soi")
ggplot(cyclisme, aes(x=equil.z))+ geom_histogram()+ labs(title="Équilibre")
ggplot(cyclisme, aes(x=engsoc.z))+ geom_histogram()+ labs(title="Engagement social")
ggplot(cyclisme, aes(x=sociab.z))+ geom_histogram()+ labs(title="Sociabilité")
ggplot(cyclisme, aes(x=soi.z))+ geom_histogram()+ labs(title="Contrôle de soi et des évènements")
ggplot(cyclisme, aes(x=bonheu.z))+ geom_histogram()+ labs(title="Bonheur")


#Examen de la normalité des résiduels et de l’homoscédasticité
library('olsrr')
ols_regress(m <- lm(id~estim+equil+engsoc+sociab+soi+bonheu,cyclisme))

res<-data.frame(residu=residuals(m))

ggplot(res,aes(x=residu))+geom_histogram()

jmv::descriptives(res,vars=residu,skew=T,kurt=T)

ggplot(data=subset(cyclisme,aes(x=id, y=m,colour=grpas))+geom_point()+scale_colour_hue(l=60))



#Homogénéité de la variance des résiduels

homosd <- na.omit(cyclisme[,c("id")])

homosd$res <- residuals(m)

ggplot(homosd,
       aes(x=total_mint,y=res))+
  geom_point()

#Caractéristique de la matrice de corrélation
#Multicolinéarité


##Analyse primaire

jmv::efa(cyclisme, vars=vars("estim1", "estim2", "estim3", "estim4",
                             "equil1", "equil2", "equil3", "equil4",
                             "engsoc1", "engsoc2", "engsoc3", "engsoc4",
                             "sociab1", "sociab2", "sociab3", "sociab4",
                             "soi1", "soi2", "soi3", "soi4",
                             "bonheu1", "bonheu2", "bonheu3", "bonheu4", "bonheu5"),
nFactorMethod="fixed",
nFactors = 6,
rotation = "oblimin",
screePlot = TRUE,
eigen = TRUE,
modelFit = TRUE,
kmo = TRUE, 
bartlett = TRUE,
factorSummary = TRUE,
hideLoadings = 0.3,
sortLoadings = TRUE,
factorCor = TRUE)

mean(psych::smc(efa))

corefa<- cor(efa, use = "complete.obs")
det(corefa)

efaresults<- psych::fa(efa,
                       nfactors = 6,
                       fm = "ml",
                       rotate = "oblimin")

crnr <- efaresults$residual[lower.tri(efaresults$residual)]

sort(efaresults$residual[lower.tri(efaresults$residual)])

sum(abs(crnr) > .05) 
length(crnr)

# Analyses exploratoires

# KMO, Bartlett, et analyse factorielle exploratoire

# Trouver le nombre de facteurs avec le test parallèle


jmv::efa(cyclisme,
         vars = vars("estim1", "estim2", "estim3", "estim4", 
                     "equil1", "equil2", "equil3", "equil4",
                     "engsoc1", "engsoc2", "engsoc3", "engsoc4",
                     "sociab1", "sociab2", "sociab3", "sociab4",
                     "soi1", "soi2", "soi3", "soi4",
                     "bonheu1", "bonheu2", "bonheu3", "bonheu4", "bonheu5"), 
         nFactorMethod = "parallel",
         extraction = "ml",
         rotation = "oblimin",
         screePlot = TRUE,
         eigen = TRUE,
         modelFit = TRUE,
         kmo = TRUE, 
         bartlett = TRUE,
         factorSummary = TRUE,
         hideLoadings = 0.3,
         sortLoadings = TRUE,
         factorCor = TRUE)


#1 : Enlever engsoc3 : Cette étape est définitive

jmv::efa(cyclisme,
         vars = vars("estim1", "estim2", "estim3", "estim4", 
                     "equil1", "equil2", "equil3", "equil4",
                     "engsoc1", "engsoc2", "engsoc4",
                     "sociab1", "sociab2", "sociab3", "sociab4",
                     "soi1", "soi2", "soi3", "soi4",
                     "bonheu1", "bonheu2", "bonheu3", "bonheu4", "bonheu5"), 
         nFactorMethod = "parallel",
         extraction = "ml",
         rotation = "oblimin",
         screePlot = TRUE,
         eigen = TRUE,
         modelFit = TRUE,
         kmo = TRUE, 
         bartlett = TRUE,
         factorSummary = TRUE,
         hideLoadings = 0.3,
         sortLoadings = TRUE,
         factorCor = TRUE)


#2 : Enlever l'échelle equil (étape test)
jmv::efa(cyclisme,
         vars = vars("estim1", "estim2", "estim3", "estim4", 
                  
                     "engsoc1", "engsoc2",  "engsoc4",
                     "sociab1", "sociab2", "sociab3", "sociab4",
                     "soi1", "soi2", "soi3", "soi4",
                     "bonheu1", "bonheu2", "bonheu3", "bonheu4", "bonheu5"), 
         nFactorMethod = "parallel",
         extraction = "ml",
         rotation = "oblimin",
         screePlot = TRUE,
         eigen = TRUE,
         modelFit = TRUE,
         kmo = TRUE, 
         bartlett = TRUE,
         factorSummary = TRUE,
         hideLoadings = 0.3,
         sortLoadings = TRUE,
         factorCor = TRUE)


#3 : Enlever soi2

jmv::efa(cyclisme,
         vars = vars("estim1", "estim2", "estim3", "estim4", 
                     
                     "engsoc1", "engsoc2", "engsoc4",
                     "sociab1", "sociab2", "sociab3", "sociab4",
                     "soi1", "soi3", "soi4",
                     "bonheu1", "bonheu2", "bonheu3", "bonheu4", "bonheu5"), 
         nFactorMethod = "parallel",
         extraction = "ml",
         rotation = "oblimin",
         screePlot = TRUE,
         eigen = TRUE,
         modelFit = TRUE,
         kmo = TRUE, 
         bartlett = TRUE,
         factorSummary = TRUE,
         hideLoadings = 0.3,
         sortLoadings = TRUE,
         factorCor = TRUE)


#4 : enlever engsoc1

jmv::efa(cyclisme,
         vars = vars("estim1", "estim2", "estim3", "estim4", 
                     
                      "engsoc2", "engsoc4",
                     "sociab1", "sociab2", "sociab3", "sociab4",
                     "soi1","soi2", "soi3", "soi4",
                     "bonheu1", "bonheu2", "bonheu3", "bonheu4", "bonheu5"), 
         nFactorMethod = "parallel",
         extraction = "ml",
         rotation = "oblimin",
         screePlot = TRUE,
         eigen = TRUE,
         modelFit = TRUE,
         kmo = TRUE, 
         bartlett = TRUE,
         factorSummary = TRUE,
         hideLoadings = 0.3,
         sortLoadings = TRUE,
         factorCor = TRUE)

#5 : Enlever, estim3, engsoc1 et soi2
jmv::efa(cyclisme,
         vars = vars("estim1", "estim2", "estim4", 
                     
                      "engsoc2", "engsoc4",
                     "sociab1", "sociab2", "sociab3", "sociab4",
                     "soi1", "soi3", "soi4",
                     "bonheu1", "bonheu2", "bonheu3", "bonheu4", "bonheu5"), 
         nFactorMethod = "parallel",
         extraction = "ml",
         rotation = "oblimin",
         screePlot = TRUE,
         eigen = TRUE,
         modelFit = TRUE,
         kmo = TRUE, 
         bartlett = TRUE,
         factorSummary = TRUE,
         hideLoadings = 0.3,
         sortLoadings = TRUE,
         factorCor = TRUE)


jmv::efa(cyclisme,
         vars = vars("estim1", "estim2", "estim4", 
                     
                     "engsoc2", "engsoc4",
                     "sociab1", "sociab2", "sociab3", "sociab4",
                     "soi1", "soi3", "soi4",
                     "bonheu1", "bonheu2", "bonheu3", "bonheu4", "bonheu5"), 
         nFactorMethod = "parallel",
         extraction = "ml",
         rotation = "oblimin",
         screePlot = TRUE,
         eigen = TRUE,
         modelFit = TRUE,
         kmo = TRUE, 
         bartlett = TRUE,
         factorSummary = TRUE,
         hideLoadings = 0.3,
         sortLoadings = TRUE,
         factorCor = TRUE)



#6 : Enlever l'échelle engsoc

jmv::efa(cyclisme,
         vars = vars("estim1", "estim2","estim4", 
                     
                  
                     "sociab1", "sociab2", "sociab3", "sociab4",
                     "soi1", "soi3", "soi4",
                     "bonheu1", "bonheu2", "bonheu3", "bonheu4", "bonheu5"), 
         nFactorMethod = "fixed",
         nFactor = 4,
         extraction = "ml",
         rotation = "oblimin",
         screePlot = TRUE,
         eigen = TRUE,
         modelFit = TRUE,
         kmo = TRUE, 
         bartlett = TRUE,
         factorSummary = TRUE,
         hideLoadings = 0.3,
         sortLoadings = TRUE,
         factorCor = TRUE)


#7 : Enlever bonheu5
jmv::efa(cyclisme,
         vars = vars("estim1", "estim2","estim4", 
                     
                     
                     "sociab1", "sociab2","sociab4", "sociab3",
                     "soi1", "soi3", "soi4",
                     "bonheu1", "bonheu2", "bonheu3", "bonheu4"), 
         nFactorMethod = "fixed",
         nFactor = 4,
         extraction = "ml",
         rotation = "oblimin",
         screePlot = TRUE,
         eigen = TRUE,
         modelFit = TRUE,
         kmo = TRUE, 
         bartlett = TRUE,
         factorSummary = TRUE,
         hideLoadings = 0.3,
         sortLoadings = TRUE,
         factorCor = TRUE)


#Enlever sociab 4
  
  
  jmv::efa(cyclisme,
         vars = vars("estim1", "estim2","estim4", 
                     
                     
                     "sociab1", "sociab2", "sociab3",
                     "soi1", "soi3", "soi4",
                     "bonheu1", "bonheu2", "bonheu3", "bonheu4","bonheu5"), 
         nFactorMethod = "fixed",
         nFactor = 4,
         extraction = "ml",
         rotation = "oblimin",
         screePlot = TRUE,
         eigen = TRUE,
         modelFit = TRUE,
         kmo = TRUE, 
         bartlett = TRUE,
         factorSummary = TRUE,
         hideLoadings = 0.3,
         sortLoadings = TRUE,
         factorCor = TRUE)



#Résultat final de l'analyse exploratoire  
  jmv::efa(cyclisme,
           vars = vars("estim1", "estim2","estim4", 
                       
                       
                       "sociab1", "sociab2", "sociab3",
                       "soi1", "soi3", "soi4",
                       "bonheu1", "bonheu2", "bonheu3", "bonheu4","bonheu5"), 
           nFactorMethod = "fixed",
           nFactor = 4,
           extraction = "ml",
           rotation = "oblimin",
           screePlot = TRUE,
           eigen = TRUE,
           modelFit = TRUE,
           kmo = TRUE, 
           bartlett = TRUE,
           factorSummary = TRUE,
           hideLoadings = 0.3,
           sortLoadings = TRUE,
           factorCor = TRUE)
  
  explo <- subset(cyclisme, select=c("estim1", "estim2","estim4", 
                                     
                                     
                                     "sociab1", "sociab2", "sociab3",
                                     "soi1", "soi3", "soi4",
                                     "bonheu1", "bonheu2", "bonheu3", "bonheu4","bonheu5"))
  
mean(psych::smc(explo))    
  

corefa2<- cor(explo, use = "complete.obs")
det(corefa2)

exploresults<- psych::fa(explo,
                       nfactors = 4,
                       fm = "ml",
                       rotate = "oblimin")

crnr <- exploresults$residual[lower.tri(exploresults$residual)]

sort(exploresults$residual[lower.tri(exploresults$residual)])

sum(abs(crnr) > .05) 
length(crnr)



























