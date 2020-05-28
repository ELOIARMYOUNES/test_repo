### Importer le fichier
RAP <- read.csv2("D:/mooc/RAP.csv")
## Analyse basique :
summary(RAP)
library(prettyR)
describe(RAP)
## La régression lin?aire multiple :
### Inspection graphique :
newdata<-cbind(RAP$Y,RAP$X1,RAP$X2,RAP$X3)
pairs(newdata, main = "Répresentation graphique des variables",pch = 21, bg = c("red", "green3", "blue")[unclass(iris$Species)])
"L'objectif de ce diagramme est double. il permet de visualiser la relation entre la variable ? expliquer (var1=RAP$Y) et chacune des variables explicatives (var2=RAP$X1,var3=RAP$X2,var3=RAP$X3), mais aussi de juger de la correlation entre les variables explicatives"
## Estimation des param?tre :
### Test sur les param?tres :
modele1<-lm(Y~X1+X2+X3,data = RAP)
modele1
summary(modele1)
"Les r?sultats fournis par summary() se pr?sentent de fa?on identique ? ceux de r?gression lin?aire simple. On y retrouve les estimations des param?tres de r?gression dans la colonne Estimate."
"Les valeurs r?alis?s des statistiques des tests de Student associ?s aux hypoth?ses (H0:Bi=0,H1:B1<>0) se Trouvent dans la colonne t value, les valeurs-p associ?es dans la colonne Pr(>|t|). Residual standard error fournit l'estimation d'?cart-type ainsi que le nombre de ddl associ?es n-p-1. On trouve enfin le coefficient de d?termination r? (Multiple R-squared) ainsi qu'une version ajust?e (Adjusted R-squared). Enfin, on trouve la r?alisation du test de Fisher global (F-statistic) ainsi que sa (p-value) associ?e"
### Tableau d'analyse de la variance :
anova(modele1)
* Interpr?tation des résultats:
  "Généralement lorsque les p-values des variables explicatives inf?rieur ? 5% simmeltaniment variable par variable. On dit que les toutes variable ont des p-values inf?rieur ? 5% sont des variables qui expliqu? le variable Y. "
"Alors on peut dire la signification par le test du Fisher ou le test du Student que la variable X1 n'est pas un variable explicatif"
### Intervalle de confiance :
confint(modele1)
"Malgr? que la coefficient de r?gression de X1 est tr?s grand que X2 et X3 mais la p-value de X1 supp?rier ? 5% signifi? que le risque d'estimation de cet coefficient est tr?s grand ? obtenu le B1=0, c'est-?-dire ont accepter l'hypoth?se H0,sachant que l'intervalle de confiance de B1 entre -0.34 et 1.33"
## Choisir un bon mod?le
"il existe plusieurs m?thode pour choisir un bon mod?le de la g?gression lin?aire multiple."
"Nous avons utiliser deux m?thodes tr?s signifi?e :"
### La m?thode pas ? pas descendante (backward selection)
"Cette m?thode est aussi appel?e r?gression par ?liminations successives. On part cette fois du mod?le complet et on ?limine ? chaque ?tape la variable ayant la plus petite valeur pour la statistique du test de Student(p-value la plus qrande)." 
drop1(lm(RAP$Y~RAP$X1+RAP$X2+RAP$X3),test = "F")
"On retire la variable X1, lorsque (plus-valeur>0.5)"
drop1(lm(RAP$Y~RAP$X2+RAP$X3),test = "F")
"La m?thode s'arr?te donc sur le mod?le contennat deux variables explicatives (X2 et X3)"
### La m?thode pas ? pas (stepwise)
"Cet algorithme est un perfectionnement de la m?thode ascendante il consiste ? effectuer en plus, ? chaque ?tape, des tests du type Student ou fisher pour ne pas introduire une variable non significative et pour ?liminer ?ventuellement des variables d?ja introduites qui ne seraient plus informatives compte tenu de la derni?re variable s?lectionn?e. L'algorithme s'arr?t? quand on ne peut plus ajoute ni retrancher de variables."
step(lm(RAP$Y~1),RAP$Y~RAP$X1+RAP$X2+RAP$X3,direction="both")
modfinal<-lm(Y~X2+X3,data = RAP)
"La m?thode s'arr?te donc sur le mod?le contenant les variables explicatives (X2 et X3)"
### Test le mod?le final :
modfinal<-lm(Y~X2+X3,data = RAP)
summary(modfinal)
anova(modfinal)
"Nous observons que Adjusted R-squared toujour constant par raport le mod?le initial et aussi le mod?le final, donc la signification de R? ajust? ?gal ? 0.4997 est comme suit, 49.97% les donn?es des variables explicatis qui s'int?resse le mod?le lin?aire donc les autres part des donn?es qui ?gal ? 48.97% est s'int?resse le mod?le non lin?aire.    "
### La probl?me de la colin?arit? :
library(car)
vif(modele1)
"Le crit?re utilis? pour juger de la colin?arit? entre les variables explicatives est le facteur d'inflation de la variance VIF"
"Le VIF joue un r?le fondamental dans la variance des estimateurs, alors la colin?arit? entre r?gresseurs se r?percute in?vitablement dans la pr?cision des estimateurs. On estime qu'il y a une forte colin?arit? lorsque VIF>10 c'est-?-dire r?>0.9"
"Alors les r?sultats obtenus pour X2 et X2, aucun probl?me de la colin?arit? pour les deux. "

## Analyse des r?sidus :
### Test statistique d'autocorrelation des erreurs :

"Test de Durbin-Watson :"
library(lmtest)
dwtest(lm(RAP$Y~RAP$X2+RAP$X3))

### Test de normalit? des erreurs :
"Test de shapiro.test:"
shapiro.test(resid(modfinal))
par(mfrow=c(1:2))
plot(modfinal,1:2,col.smooth="red")

### Test d'h?t?rosc?dasticit? des r?sidus :
"Test Breusch-Pagan"
library(lmtest)
bptest(modfinal)

"Test de wthite"
library(car)
R2<-modfinal$residuals^2
Y<-fitted(modfinal)
Ru<-summary(lm(R2~RAP$Y+I(RAP$Y^2)))$r.squared
LM<-nrow(RAP)*Ru
p.value<-1-pchisq(LM,2)
p.value
plot(modfinal)

### La distance de cook:
plot(cooks.distance(modfinal),type="h")


### La v?rification des 6 hypotheses de la mod?le :
"Les deux premi?res hypoth?ses totalement existe sans v?rification"
* H1: "Le mod?le est lin?aire en Xi et Yi"
* H2: "Les valeurs Xi sont observ?es sans erreur c'est-?-dire Xi non al?atoire"
"Les deux premi?res hypoth?ses totalement existe sans v?rification"
* H3: " l'esp?rance math?matique de l'erreur est nulle "
mean(modfinal$residuals,na.rm = TRUE)
"Nous observons que l'esp?rance math?matique de l'erreur est tr?s proche ? 0" 
* H4: "Le risque de l'amplitude de l'erreur est le m?me quelle que soit la p?riode :"
var(modfinal$residuals,na.rm = TRUE)
sd(modfinal$residuals,na.rm = TRUE)
"Nous observons que la variance de l'erreur est constant"
* H5: "quelque soit t,t' les erreurs sont non corr?l?es ou encore ind?pendantes"
"Apr?s les tests de normalit? et d'autocorr?lation et d'h?t?rosc?dasticit? des erreurs, on peut dire l'hypoth?se H5 totalement exacte"
* H6: " l'erreur est independante de la variable explicative "
res<-residuals(modfinal)
par(mfrow=c(1,2))
plot(res~X2,col="blue",main="point du nuage X2 par rapport les r?sidus",data = RAP)
abline(lm(res~X2,data = RAP),col="red")
plot(res~X3,col="red",main="point du nuage X1 par rapport les r?sidus",data = RAP)
abline(lm(res~X3,data = RAP),col="blue")
cor(res,RAP$X2)
cor(res,RAP$X3)
"Nous observons qu'aucun corr?lation entre les variables explicatives et les erreurs"
H7:" les erreurs suit loi normal centr? et de variance constant "
summary(modfinal$residuals)
summary(modfinal$residuals^2)
"Sachant que H3 et H4 ce sont existes donc H7 est aussi existe"

### La pr?diction pour un nouvelle valeur:
newRAP<-data.frame(X2=64,X3=23)
predict(modfinal,newRAP,interval = "prediction",se.fit = FALSE)
library(effects)
plot(predictorEffects(modfinal))




res.stud<-rstudent(modfinal)
seuil.stud<-qt(0.975,47~47~47)
cond<-res.stud<-(seuil.stud)|res.stud>seuil.stud
val.ajust<-fitted(modfinal)
plot(res.stud~val.ajust,xlab="Valeurs ajustées",ylab="Résidus studentises")


