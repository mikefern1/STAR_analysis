#FERNANDES MICHAEL N24005750

options(scipen=9) #drop scientific notation
#pacchetti necessari
library(skimr) #summary of data like function skim
library(psych) #summary of data - describe()
library(corrplot) #corrplot
library(sfsmisc) #boxplot

#function to calculate the most important descriptive indexes
descriptive <- function(x) {
  mean <- mean(x)
  var <- var(x)*7/8
  sd <- sqrt(var)
  vc <- sd/abs(mean)
  min_max <- range(x)
  results <- round(c(mean, var, sd, vc, min_max), digits=2)
  labels <- c("mean","variance","standard deviation","variation coefficient",
              "minimum and maximum" )
  list <- list(labels,results)
  list
}

#set up dataset
setwd("~/Desktop/Esame Data Analytics") #valutare di fare una destinazione github
#https://www.youtube.com/watch?v=JW5Ug6NQexg
star <- read.csv("STAR.csv",sep=";") #importa file
#Data understanding
View(star)
sum(is.na(star)) #missing data?
dim(star) #numero di righe e colonne
summary(star) #some have character values

#ADAPTING variables
#trasforming all the character values in numeric
{
  star$teachers <- gsub(",", ".", star$teachers)
  star$calw_pct <- gsub(",", ".", star$calw_pct)
  star$meal_pct <- gsub(",", ".", star$meal_pct)
  star$testscr <- gsub(",", ".", star$testscr)
  star$comp_stu <- gsub(",", ".", star$comp_stu)
  star$expn_stu <- gsub(",", ".", star$expn_stu)
  star$str <- gsub(",", ".", star$str)
  star$avginc <- gsub(",", ".", star$avginc)
  star$el_pct <- gsub(",", ".", star$el_pct)
  star$read_scr <- gsub(",", ".", star$read_scr)
  star$math_scr <- gsub(",", ".", star$math_scr)
  
  star$obs <- as.numeric(star$obs)
  star$enrl_tot <- as.numeric(star$enrl_tot)
  star$teachers <- as.numeric(star$teachers) #
  star$calw_pct <- as.numeric(star$calw_pct)#
  star$meal_pct <- as.numeric(star$meal_pct) #
  star$computer <- as.numeric(star$computer)
  star$testscr <- as.numeric(star$testscr) #
  star$comp_stu <- as.numeric(star$comp_stu)#
  star$expn_stu <- as.numeric(star$expn_stu)#
  star$str <- as.numeric(star$str)#
  star$avginc <- as.numeric(star$avginc) #
  star$el_pct <- as.numeric(star$el_pct) #
  star$read_scr <- as.numeric(star$read_scr)#
  star$math_scr <- as.numeric(star$math_scr)#
}

#Descriptive description (look at the pdf)
{
  #indexes
  {
    summary(star) 
    describe(star)
    
    descriptive(star$enrl_tot)
    descriptive(star$teachers)
    descriptive(star$calw_pct)
    descriptive(star$meal_pct)
    descriptive(star$computer)
    descriptive(star$testscr)
    descriptive(star$comp_stu)
    descriptive(star$expn_stu)
    descriptive(star$str)
    descriptive(star$avginc)
    descriptive(star$el_pct)
    descriptive(star$read_scr)
    descriptive(star$math_scr)
  }

  #histogram and boxplot of the variables
  {
  par(mfrow=c(3,2))
  histBxp(star$enrl_tot, main="Enrollment's Distribution", xlab= "Enrollment", col="dark green") 
  histBxp(star$teachers, main="Teachers' Distribution", xlab= "Teachers", col="dark green")
  histBxp(star$calw_pct, main="Calwork's Distribution", xlab= "Calwork Percentage", col="dark green")
  histBxp(star$meal_pct, main="Meal_pct's Distribution", xlab= "Meal Percentage", col="dark green")
  histBxp(star$computer, main="Computer's Distribution", xlab= "Computer", col="dark green")
  histBxp(star$comp_stu, main="Comp_stu's Distribution", xlab= "Computer per student", col="dark green")
  histBxp(star$expn_stu, main="Expn_stu's Distribution", xlab= "Expenditure per student", col="dark green")
  histBxp(star$str, main="Str's Distribution", xlab= "Student/Teacher", col="dark green")
  histBxp(star$avginc, main="Average Income's Distribution", xlab= "Avg Income", col="dark green")
  histBxp(star$el_pct, main="English Learners Distribution", xlab= "English Learners Percentage", col="dark green")
  histBxp(star$read_scr, main="Reading Score Distribution", xlab= "Reading Score", col="dark green")
  histBxp(star$math_scr, main="Math Score Distribution", xlab= "Math Score", col="dark green")
  par(mfrow=c(1,1))
  histBxp(star$testscr, main="Test_Scr Distribution", xlab= "Test Score", col="dark green")
  }
}

#understanding CalWorks
{
  {
  summary(star$calw_pct) 
  hist(star$calw_pct) #plot
  hist(star$calw_pct,breaks=20) #plot with more breaks
  boxplot(star$calw_pct) #boxplot
  plot(star$calw_pct,star$testscr) #scatterplot with testscr
  star$calw_pct1 <- star$calw_pct #setting a new variable
  star$calw_pct1 <- round(star$calw_pct1, digits=0) #rounding to unit
  summary(star$calw_pct1) #make a summary to check that everything is alright
  breaks = seq(0,81, by=3) #make a sequence of 3 units-intervals
  star$calw_pct1 = cut(star$calw_pct1, breaks, right=FALSE) 
  star$calw_pct1 #it is discrete now
  cw.freq = table(star$calw_pct1)
  cw.cumfreq = cumsum(cw.freq)
  cw.cumrelfreq = round(cw.cumfreq / nrow(star),digits=3)
  cbind(cw.freq,cw.cumfreq,cw.cumrelfreq) #table with absolute, relative and cumulative frequencies
  boxplot(star$testscr ~ star$calw_pct1, notch=T) #taglio a 15
  }

#dummyvariable
star$calw_cat <- star$calw_pct #nuova variabile da dummyzare
star$calw_cat<- ifelse(star$calw_cat<=15,"0","1")
star$calw_cat <- factor(star$calw_cat, levels =c("0","1"))
class(star$calw_cat) #0 non usufruiscono, #1 usufruiscono
}

#corrmatrix
{
  star1 <- star[,c(-1,-15,-16)] #new dataset removing the dummy variable and obs
  X <- cor(star1, method = "pearson") #correlation matrix with pearson method
  X <-round(X,3)
  upper.tri(X, diag = FALSE) 
  upper<-X
  upper[upper.tri(X)]<-""
  upper<-as.data.frame(upper)
  cor_mat <- upper #now I have just half of the correlation matrix to see better the relationships
  View(cor_mat)
  complete <- corrplot(X) #correlation plot
  number <- corrplot(X, method = "number", type= "lower") #correlation plot with numbers
  ycor <- X[-c(1:5, 7:13),]
  ycor #correlations of the testscr
}


#plot rispetto a testscr
{
  plot(star$enrl_tot,star$testscr) #punti abbastanza casuali
  abline(h=654, lty=2, col="red")
  cor.test(star$enrl_tot,star$testscr) #0,0015
  
  plot(star$teachers,star$testscr) #punti casuali
  abline(h=654, lty=2, col="red")
  cor.test(star$teachers,star$testscr)#pvalue: 0.003
  
  plot(star$calw_pct,star$testscr) #alta correlazione
  abline(h=654, lty=2, col="red")
  abline(v=3,lty=2,col="red")
  cor.test(star$calw_pct,star$testscr) #0.000
  
  plot(star$meal_pct,star$testscr) #sembrano esserci due gruppi, fino al 3% e il resto
  abline(h=654, lty=2, col="red")
  cor.test(star$meal_pct,star$testscr) #0
  
  plot(star$computer,star$testscr)#i punti sembrano molto casuali
  abline(h=654, lty=2, col="red")
  cor.test(star$computer,star$testscr) #pvalue=0.131
  
  plot(star$comp_stu,star$testscr) 
  abline(h=640, lty=2, col="dark green")
  abline(v=0.255,lty=2, col="dark green")
  abline(v=0.04,lty=2,col="dark green")
  cor.test(star$comp_stu,star$testscr) #p-value=0
  
  plot(star$expn_stu,star$testscr) 
  abline(h=654, lty=2, col="red")
  abline(v=6500,lty=2, col="dark green")
  cor.test(star$expn_stu,star$testscr) #0.0
  
  plot(star$str,star$testscr) #abbastanza casuali
  abline(h=654, lty=2, col="red")
  cor.test(star$str,star$testscr) #0, corr=-0,226
  
  
  plot(star$avginc,star$testscr) #molto correlato
  abline(h=654, lty=2, col="red")
  abline(v=16,lty=2,col="red")
  cor.test(star$avginc,star$testscr) #0
  
  plot(star$el_pct,star$testscr) #correlazione negativa netta
  abline(h=654, lty=2, col="red")
  abline(v=25,lty=2,col="red")
  cor.test(star$el_pct,star$testscr) #0
  
  plot(star$read_scr,star$testscr)#super correlato
  abline(h=654, lty=2, col="red")
  cor.test(star$read_scr,star$testscr) #0
  
  plot(star$math_scr,star$testscr) #super correlato
  abline(h=654, lty=2, col="red")
  cor.test(star$math_scr,star$testscr) #0
  
  plot(star$math_scr,star$read_scr)
  plot(star$read_scr,star$math_scr)
  cor.test(star$math_scr,star$read_scr) #0
}

#scatter plots with testscr and calwork highlithing
{
  plot(star$calw_pct,star$testscr, col= star$calw_cat) 
  plot(star$computer,star$testscr, col= star$calw_cat)
  plot(star$comp_stu,star$testscr,col= star$calw_cat) 
  plot(star$avginc,star$testscr,col= star$calw_cat) 
  plot(star$expn_stu,star$testscr,col= star$calw_cat) 
  plot(star$str,star$testscr,col= star$calw_cat) 
  plot(star$avginc,star$testscr,col= star$calw_cat) 
  plot(star$el_pct,star$testscr,col= star$calw_cat) 
  plot(star$read_scr,star$testscr,col= star$calw_cat)
  plot(star$math_scr,star$testscr,col= star$calw_cat) 
  plot(star$math_scr,star$read_scr,col= star$calw_cat)
  plot(star$read_scr,star$math_scr,col= star$calw_cat)
  plot(star$enrl_tot,star$testscr,col= star$calw_cat)
  plot(star$meal_pct,star$testscr,col= star$calw_cat) 
}

# 1)testscr~meal_pct
{
  #simple linear regression
  plot(star$meal_pct,star$testscr)
  simple_fm <-lm(testscr~meal_pct,data=star)
  simple_fm$coefficients
  abline(simple_fm$coefficients[1],simple_fm$coefficients[2],col="dark green")
  summary(simple_fm)
  
  #same slope different intercepts
  plot(star$meal_pct,star$testscr,col=star$calw_cat)
  mod1 <- lm(testscr~calw_cat+meal_pct,data=star)
  summary(mod1)
  mod1$coefficients
  no_calw <- c(mod1$coefficients[1],mod1$coefficients[3])
  yes_calw <- c(mod1$coefficients[1]+mod1$coefficients[2],mod1$coefficients[3])
  abline(no_calw[1],no_calw[2],col="black")
  abline(yes_calw[1],yes_calw[2],col="red")
  
  
  #we do with same intercept and different slopes
  plot(star$meal_pct, star$testscr, col=star$calw_cat)
  mod4 <- lm(testscr~meal_pct+meal_pct:calw_cat,data=star) #A:B==A*B
  summary(mod4)
  no_calw <- c(mod4$coefficients[1],mod4$coefficients[2])
  yes_calw <- c(mod4$coefficients[1],mod4$coefficients[2]+mod4$coefficients[3])
  abline(no_calw[1],no_calw[2],col="black")
  abline(yes_calw[1],yes_calw[2],col="red")
  
  #different slope different intercepts
  plot(star$meal_pct,star$testscr,col=star$calw_cat)
  mod5 <- lm(testscr~meal_pct*calw_cat,data=star) #A*B==A+B+A*B
  summary(mod5)
  mod5$coefficients
  no_calw <- c(mod5$coefficients[1],mod5$coefficients[2])
  yes_calw <- c(mod5$coefficients[1]+mod5$coefficients[3]
                ,mod5$coefficients[2]+mod5$coefficients[4])
  abline(no_calw[1],no_calw[2],col="black")
  abline(yes_calw[1],yes_calw[2],col="red")
  
  anova(simple_fm,mod1,mod4,mod5)
  summary(mod4)
}

"l'intercetta è quasi la stessa, la pendenza è sempre negativa e in no_calw è maggiore
in termini assoluti, indicando che ad un aumento unitario del meal_pct ci sarà una
diminuzione maggiore nel testscr rispetto a yes_calw. implementando il test ANOVA,
il test migliore risulta mod4 con diversa pendenza. discriminare rispetto a calw
non porta grandi risultati vista anche l'alta correlazione tra i due. Abbiamo un R^2 elevato
0,757 e il test sui coefficienti porta buoni risultati"

# 2) testscr~enrollement
{
  #simple linear regression
  plot(star$enrl_tot,star$testscr)
  simple_fm <-lm(testscr~enrl_tot,data=star)
  simple_fm$coefficients
  abline(simple_fm$coefficients[1],simple_fm$coefficients[2],col="dark green")
  summary(simple_fm)
  
  #same slope different intercepts
  plot(star$enrl_tot,star$testscr,col=star$calw_cat)
  mod1 <- lm(testscr~calw_cat+enrl_tot,data=star)
  summary(mod1)
  mod1$coefficients
  no_calw <- c(mod1$coefficients[1],mod1$coefficients[3])
  yes_calw <- c(mod1$coefficients[1]+mod1$coefficients[2],mod1$coefficients[3])
  abline(no_calw[1],no_calw[2],col="black")
  abline(yes_calw[1],yes_calw[2],col="red")
  
  #we do with same intercept and different slopes
  plot(star$enrl_tot, star$testscr, col=star$calw_cat)
  mod4 <- lm(testscr~enrl_tot+enrl_tot:calw_cat,data=star) #A:B==A*B
  summary(mod4)
  no_calw <- c(mod4$coefficients[1],mod4$coefficients[2])
  yes_calw <- c(mod4$coefficients[1],mod4$coefficients[2]+mod4$coefficients[3])
  abline(no_calw[1],no_calw[2],col="black")
  abline(yes_calw[1],yes_calw[2],col="red")
  
  #different slope different intercepts
  plot(star$enrl_tot,star$testscr,col=star$calw_cat)
  mod5 <- lm(testscr~enrl_tot*calw_cat,data=star) #A*B==A+B+A*B
  summary(mod5)
  mod5$coefficients
  no_calw <- c(mod5$coefficients[1],mod5$coefficients[2])
  yes_calw <- c(mod5$coefficients[1]+mod5$coefficients[3]
                ,mod5$coefficients[2]+mod5$coefficients[4])
  abline(no_calw[1],no_calw[2],col="black")
  abline(yes_calw[1],yes_calw[2],col="red")
  abline(prova$coefficients[1],prova$coefficients[2],col="dark green")
  
  
  anova(simple_fm,mod1,mod4,mod5)
  summary(simple_fm)
  summary(mod1)
}
"enrollment:il modello preferibile secondo il test anova sembra il mod1, same slope
different intercept. Categorizzare per calw_ sembra una buona idea, vista la riduzione
nei residui. tuttavia l'R^2 è molto piccolo, pari solo a 0,314. Tuttavia si può osservare come
migliora rispetto al modello semplice pari a 0,024"

# 3) tescr~str
{
  #simple linear regression
  plot(star$str,star$testscr)
  simple_fm <-lm(testscr~str,data=star)
  simple_fm$coefficients
  abline(simple_fm$coefficients[1],simple_fm$coefficients[2],col="green")
  summary(simple_fm)
  
  #same slope different intercepts
  plot(star$str,star$testscr,col=star$calw_cat)
  mod1 <- lm(testscr~calw_cat+str,data=star)
  summary(mod1)
  mod1$coefficients
  no_calw <- c(mod1$coefficients[1],mod1$coefficients[3])
  yes_calw <- c(mod1$coefficients[1]+mod1$coefficients[2],mod1$coefficients[3])
  abline(no_calw[1],no_calw[2],col="black")
  abline(yes_calw[1],yes_calw[2],col="red")
  
  #we do with same intercept and different slopes
  plot(star$str, star$testscr, col=star$calw_cat)
  mod4 <- lm(testscr~enrl_tot_pct+str:calw_cat,data=star) #A:B==A*B
  summary(mod4)
  no_calw <- c(mod4$coefficients[1],mod4$coefficients[2])
  yes_calw <- c(mod4$coefficients[1],mod4$coefficients[2]+mod4$coefficients[3])
  abline(no_calw[1],no_calw[2],col="black")
  abline(yes_calw[1],yes_calw[2],col="red")
  
  #different slope different intercepts
  plot(star$str,star$testscr,col=star$calw_cat)
  mod5 <- lm(testscr~str*calw_cat,data=star) #A*B==A+B+A*B
  summary(mod5)
  mod5$coefficients
  no_calw <- c(mod5$coefficients[1],mod5$coefficients[2])
  yes_calw <- c(mod5$coefficients[1]+mod5$coefficients[3]
                ,mod5$coefficients[2]+mod5$coefficients[4])
  abline(no_calw[1],no_calw[2],col="black")
  abline(yes_calw[1],yes_calw[2],col="red")
  
  anova(simple_fm,mod1,mod4,mod5)
  summary(simple_fm)
  summary(mod1)
}
"Il modello preferito è same slope, different intercept. Nel grafico mod4 infatti
le due rette,essendo calcolate con la same intercept e different slope, collassano
sulla stessa retta e sono identificate dalle stesse coordinate. come si vede in
mod5m infatti le due rette sono quasi parallele. Il test anova, indica infatti sia come
la codifica dummy porti ad una considerevole riduzione degli errori, ma anche come
effettivamente il modello preferito sia mod1, con same slope e different intercept,
tenendo conto degli errori e dei gradi di libertà. L'introduzione della dummy 
porta l'indice R^2 da 0.051 a 0.0351"

#4) testscr-computer
{
  #simple linear regression
  plot(star$computer,star$testscr)
  simple_fm <-lm(testscr~computer,data=star)
  simple_fm$coefficients
  abline(simple_fm$coefficients[1],simple_fm$coefficients[2],col="green")
  summary(simple_fm)
  
  #same slope different intercepts
  plot(star$computer,star$testscr, col= star$calw_cat)
  mod1 <- lm(testscr~calw_cat+computer,data=star)
  summary(mod1)
  mod1$coefficients
  no_calw <- c(mod1$coefficients[1],mod1$coefficients[3])
  yes_calw <- c(mod1$coefficients[1]+mod1$coefficients[2],mod1$coefficients[3])
  abline(no_calw[1],no_calw[2],col="black")
  abline(yes_calw[1],yes_calw[2],col="red")
  
  #we do with same intercept and different slopes
  plot(star$computer,star$testscr, col= star$calw_cat)
  mod4 <- lm(testscr~enrl_tot_pct+computer:calw_cat,data=star) #A:B==A*B
  summary(mod4)
  no_calw <- c(mod4$coefficients[1],mod4$coefficients[2])
  yes_calw <- c(mod4$coefficients[1],mod4$coefficients[2]+mod4$coefficients[3])
  abline(no_calw[1],no_calw[2],col="black")
  abline(yes_calw[1],yes_calw[2],col="red")
  
  #different slope different intercepts
  plot(star$computer,star$testscr, col= star$calw_cat)
  mod5 <- lm(testscr~computer*calw_cat,data=star) #A*B==A+B+A*B
  summary(mod5)
  mod5$coefficients
  no_calw <- c(mod5$coefficients[1],mod5$coefficients[2])
  yes_calw <- c(mod5$coefficients[1]+mod5$coefficients[3]
                ,mod5$coefficients[2]+mod5$coefficients[4])
  abline(no_calw[1],no_calw[2],col="black")
  abline(yes_calw[1],yes_calw[2],col="red")
  
  anova(simple_fm,mod1,mod5,mod4)
  summary(simple_fm)
  summary(mod1)
}
"modello migliore è il mod1. Rsq_adj da 0.003 a 0.303"

#5) testscr - teacher
{
  #simple linear regression
  plot(star$teachers,star$testscr)
  simple_fm <-lm(testscr~teachers,data=star)
  simple_fm$coefficients
  abline(simple_fm$coefficients[1],simple_fm$coefficients[2],col="dark green")
  summary(simple_fm)
  
  #same slope different intercepts
  plot(star$teachers,star$testscr, col= star$calw_cat)
  mod1 <- lm(testscr~calw_cat+teachers,data=star)
  summary(mod1)
  mod1$coefficients
  no_calw <- c(mod1$coefficients[1],mod1$coefficients[3])
  yes_calw <- c(mod1$coefficients[1]+mod1$coefficients[2],mod1$coefficients[3])
  abline(no_calw[1],no_calw[2],col="black")
  abline(yes_calw[1],yes_calw[2],col="red")
  
  #we do with same intercept and different slopes
  plot(star$teachers,star$testscr, col= star$calw_cat)
  mod4 <- lm(testscr~enrl_tot_pct+teachers:calw_cat,data=star) #A:B==A*B
  summary(mod4)
  no_calw <- c(mod4$coefficients[1],mod4$coefficients[2])
  yes_calw <- c(mod4$coefficients[1],mod4$coefficients[2]+mod4$coefficients[3])
  abline(no_calw[1],no_calw[2],col="black")
  abline(yes_calw[1],yes_calw[2],col="red")
  
  #different slope different intercepts
  plot(star$teachers,star$testscr, col= star$calw_cat)
  mod5 <- lm(testscr~teachers*calw_cat,data=star) #A*B==A+B+A*B
  summary(mod5)
  mod5$coefficients
  no_calw <- c(mod5$coefficients[1],mod5$coefficients[2])
  yes_calw <- c(mod5$coefficients[1]+mod5$coefficients[3]
                ,mod5$coefficients[2]+mod5$coefficients[4])
  abline(no_calw[1],no_calw[2],col="black")
  abline(yes_calw[1],yes_calw[2],col="red")
  
  anova(simple_fm,mod1,mod4,mod5)
  summary(simple_fm)
  summary(mod1)
}
"la pendenza cambia poco, scelgo sempre mod1. L'Rsq_adj passa da 0.019 a 0.312 "

#6) testscr-calw_pct
{
  #simple linear regression
  plot(star$calw_pct,star$testscr, col= star$calw_cat)
  simple_fm <-lm(testscr~calw_pct,data=star)
  simple_fm$coefficients
  abline(simple_fm$coefficients[1],simple_fm$coefficients[2],col="dark green")
  summary(simple_fm)
  
  #same slope different intercepts
  plot(star$calw_pct,star$testscr, col= star$calw_cat)
  mod1 <- lm(testscr~calw_cat+calw_pct,data=star)
  summary(mod1)
  mod1$coefficients
  no_calw <- c(mod1$coefficients[1],mod1$coefficients[3])
  yes_calw <- c(mod1$coefficients[1]+mod1$coefficients[2],mod1$coefficients[3])
  abline(no_calw[1],no_calw[2],col="black")
  abline(yes_calw[1],yes_calw[2],col="red")
  
  #we do with same intercept and different slopes
  plot(star$calw_pct,star$testscr, col= star$calw_cat)
  mod4 <- lm(testscr~enrl_tot_pct+calw_pct:calw_cat,data=star) #A:B==A*B
  summary(mod4)
  no_calw <- c(mod4$coefficients[1],mod4$coefficients[2])
  yes_calw <- c(mod4$coefficients[1],mod4$coefficients[2]+mod4$coefficients[3])
  abline(no_calw[1],no_calw[2],col="black")
  abline(yes_calw[1],yes_calw[2],col="red")
  
  #different slope different intercepts
  plot(star$calw_pct,star$testscr, col= star$calw_cat)
  mod5 <- lm(testscr~calw_pct*calw_cat,data=star) #A*B==A+B+A*B
  summary(mod5)
  mod5$coefficients
  no_calw <- c(mod5$coefficients[1],mod5$coefficients[2])
  yes_calw <- c(mod5$coefficients[1]+mod5$coefficients[3]
                ,mod5$coefficients[2]+mod5$coefficients[4])
  abline(no_calw[1],no_calw[2],col="black")
  abline(yes_calw[1],yes_calw[2],col="red")
  abline(simple_fm$coefficients[1],simple_fm$coefficients[2],col="dark green")
  
  
  anova(simple_fm,mod1,mod4,mod5)
  summary(simple_fm)
  summary(mod5)
} 
"il modello mod4 con same intercept e different slope sembra quasi sempre inutile, poichè
la differenza non è nello slope ma nel valore intercetta. Per quanto riguarda gli errori,
in questo caso il modello da utilizzare sarebbe il mod5, poichè una modifica del calw_pct
nel gruppo di coloro che ne usufruiscono in maniera inferiore al 15% incide molto sul rendimento al test.
questo probabilmente potrebbe comportare un'ulteriore modifica se si fosse diviso in 3 gruppi
come si vede nei boxplot. Simple_fm ha un r^2_adj di 0,391, che aumenta a 0,520 nel
modello con la dummy"

#7) testscr - cmp_stu
{
  #simple linear regression
  plot(star$comp_stu,star$testscr)
  simple_fm <-lm(testscr~comp_stu,data=star)
  simple_fm$coefficients
  abline(simple_fm$coefficients[1],simple_fm$coefficients[2],col="dark green")
  summary(simple_fm)
  
  #same slope different intercepts
  plot(star$comp_stu,star$testscr, col= star$calw_cat)
  mod1 <- lm(testscr~calw_cat+comp_stu,data=star)
  summary(mod1)
  mod1$coefficients
  no_calw <- c(mod1$coefficients[1],mod1$coefficients[3])
  yes_calw <- c(mod1$coefficients[1]+mod1$coefficients[2],mod1$coefficients[3])
  abline(no_calw[1],no_calw[2],col="black")
  abline(yes_calw[1],yes_calw[2],col="red")
  
  #we do with same intercept and different slopes
  plot(star$comp_stu,star$testscr, col= star$calw_cat)
  mod4 <- lm(testscr~enrl_tot_pct+comp_stu:calw_cat,data=star) #A:B==A*B
  summary(mod4)
  no_calw <- c(mod4$coefficients[1],mod4$coefficients[2])
  yes_calw <- c(mod4$coefficients[1],mod4$coefficients[2]+mod4$coefficients[3])
  abline(no_calw[1],no_calw[2],col="black")
  abline(yes_calw[1],yes_calw[2],col="red")
  
  #different slope different intercepts
  plot(star$comp_stu,star$testscr, col= star$calw_cat)
  mod5 <- lm(testscr~comp_stu*calw_cat,data=star) #A*B==A+B+A*B
  summary(mod5)
  mod5$coefficients
  no_calw <- c(mod5$coefficients[1],mod5$coefficients[2])
  yes_calw <- c(mod5$coefficients[1]+mod5$coefficients[3]
                ,mod5$coefficients[2]+mod5$coefficients[4])
  abline(no_calw[1],no_calw[2],col="black")
  abline(yes_calw[1],yes_calw[2],col="red")
  
  anova(simple_fm,mod1,mod4,mod5)
  summary(simple_fm)
  summary(mod1)
}
"mod4, gli assi corrispondono. le rette sono parallele e si preferisce il modello mod1.
La codifica dummy diminuisce gli errori. R^2_adj passa da 0.071 a 0.347"
#8) testscr- expn_stu
{
  #simple linear regression
  plot(star$expn_stu,star$testscr)
  simple_fm <-lm(testscr~expn_stu,data=star)
  simple_fm$coefficients
  abline(simple_fm$coefficients[1],simple_fm$coefficients[2],col="dark green")
  summary(simple_fm)
  
  #same slope different intercepts
  plot(star$expn_stu,star$testscr, col= star$calw_cat)
  mod1 <- lm(testscr~calw_cat+expn_stu,data=star)
  summary(mod1)
  mod1$coefficients
  no_calw <- c(mod1$coefficients[1],mod1$coefficients[3])
  yes_calw <- c(mod1$coefficients[1]+mod1$coefficients[2],mod1$coefficients[3])
  abline(no_calw[1],no_calw[2],col="black")
  abline(yes_calw[1],yes_calw[2],col="red")
  
  #we do with same intercept and different slopes
  plot(star$expn_stu,star$testscr, col= star$calw_cat)
  mod4 <- lm(testscr~enrl_tot_pct+expn_stu:calw_cat,data=star) #A:B==A*B
  summary(mod4)
  no_calw <- c(mod4$coefficients[1],mod4$coefficients[2])
  yes_calw <- c(mod4$coefficients[1],mod4$coefficients[2]+mod4$coefficients[3])
  abline(no_calw[1],no_calw[2],col="black")
  abline(yes_calw[1],yes_calw[2],col="red")
  
  #different slope different intercepts
  plot(star$expn_stu,star$testscr, col= star$calw_cat)
  mod5 <- lm(testscr~expn_stu*calw_cat,data=star) #A*B==A+B+A*B
  summary(mod5)
  mod5$coefficients
  no_calw <- c(mod5$coefficients[1],mod5$coefficients[2])
  yes_calw <- c(mod5$coefficients[1]+mod5$coefficients[3]
                ,mod5$coefficients[2]+mod5$coefficients[4])
  abline(no_calw[1],no_calw[2],col="black")
  abline(yes_calw[1],yes_calw[2],col="red")
  
  anova(simple_fm,mod1,mod4,mod5)
  summary(simple_fm)
  summary(mod1)
}
"scegliamo sempre mod1, nonostante si noti che nel mod5 vi sia una differenza di intercetta,
ma anche una differenza di slope. la pendenza, sebbene diminuisca di circa il 5/1000 rispetto
a coloro che non hanno il calw, diventa significativa se rapportata rispetto all'unità di misura,
portando notevoli differenze nel testscr nel momento in cui la spesa per studente aumenta
significatrivamente. Infatti si notava come nello scatterplot, oltre un certo valore,
tutti gli studenti si pongano oltre la media e la mediana. L'R^2 adj è del 0,034 vs 0.347"

#9) testscr-avginc
{
  #simple linear regression
  plot(star$avginc,star$testscr)
  simple_fm <-lm(testscr~avginc,data=star)
  simple_fm$coefficients
  abline(simple_fm$coefficients[1],simple_fm$coefficients[2],col="dark green")
  summary(simple_fm)
  #same slope different intercepts
  plot(star$avginc,star$testscr, col= star$calw_cat)
  mod1 <- lm(testscr~calw_cat+avginc,data=star)
  summary(mod1)
  mod1$coefficients
  no_calw <- c(mod1$coefficients[1],mod1$coefficients[3])
  yes_calw <- c(mod1$coefficients[1]+mod1$coefficients[2],mod1$coefficients[3])
  abline(no_calw[1],no_calw[2],col="black")
  abline(yes_calw[1],yes_calw[2],col="red")
  
  #we do with same intercept and different slopes
  plot(star$avginc,star$testscr, col= star$calw_cat)
  mod4 <- lm(testscr~enrl_tot_pct+avginc:calw_cat,data=star) #A:B==A*B
  summary(mod4)
  no_calw <- c(mod4$coefficients[1],mod4$coefficients[2])
  yes_calw <- c(mod4$coefficients[1],mod4$coefficients[2]+mod4$coefficients[3])
  abline(no_calw[1],no_calw[2],col="black")
  abline(yes_calw[1],yes_calw[2],col="red")
  
  #different slope different intercepts
  plot(star$avginc,star$testscr, col= star$calw_cat)
  mod5 <- lm(testscr~avginc*calw_cat,data=star) #A*B==A+B+A*B
  summary(mod5)
  mod5$coefficients
  no_calw <- c(mod5$coefficients[1],mod5$coefficients[2])
  yes_calw <- c(mod5$coefficients[1]+mod5$coefficients[3]
                ,mod5$coefficients[2]+mod5$coefficients[4])
  abline(no_calw[1],no_calw[2],col="black")
  abline(yes_calw[1],yes_calw[2],col="red")
  
  anova(simple_fm,mod1,mod4,mod5)
  summary(simple_fm)
  summary(mod1)
}
"essendo avginc molto correlata a calwcat anch'esso non produce una notevole riduzione degli errori,
tuttavia è maggiore poichè l'rcorr è di -0,513. Si preferirà sicuramente il mod1.
R^2_adj da 0.506 a 0.580"

#10) testscr-elpct
{
  #simple linear regression
  plot(star$el_pct,star$testscr)
  simple_fm <-lm(testscr~el_pct,data=star)
  simple_fm$coefficients
  abline(simple_fm$coefficients[1],simple_fm$coefficients[2],col="dark green")
  summary(simple_fm)
  
  #same slope different intercepts
  plot(star$el_pct,star$testscr, col= star$calw_cat)
  mod1 <- lm(testscr~calw_cat+el_pct,data=star)
  summary(mod1)
  mod1$coefficients
  no_calw <- c(mod1$coefficients[1],mod1$coefficients[3])
  yes_calw <- c(mod1$coefficients[1]+mod1$coefficients[2],mod1$coefficients[3])
  abline(no_calw[1],no_calw[2],col="black")
  abline(yes_calw[1],yes_calw[2],col="red")
  
  #we do with same intercept and different slopes
  plot(star$el_pct,star$testscr, col= star$calw_cat)
  mod4 <- lm(testscr~enrl_tot_pct+el_pct:calw_cat,data=star) #A:B==A*B
  summary(mod4)
  no_calw <- c(mod4$coefficients[1],mod4$coefficients[2])
  yes_calw <- c(mod4$coefficients[1],mod4$coefficients[2]+mod4$coefficients[3])
  abline(no_calw[1],no_calw[2],col="black")
  abline(yes_calw[1],yes_calw[2],col="red")
  
  #different slope different intercepts
  plot(star$el_pct,star$testscr, col= star$calw_cat)
  mod5 <- lm(testscr~el_pct*calw_cat,data=star) #A*B==A+B+A*B
  summary(mod5)
  mod5$coefficients
  no_calw <- c(mod5$coefficients[1],mod5$coefficients[2])
  yes_calw <- c(mod5$coefficients[1]+mod5$coefficients[3]
                ,mod5$coefficients[2]+mod5$coefficients[4])
  abline(no_calw[1],no_calw[2],col="black")
  abline(yes_calw[1],yes_calw[2],col="red")
  
  anova(simple_fm,mod1,mod4,mod5)
  anova(mod1,mod5)
  summary(simple_fm)
  summary(mod5)
}
"riduzione degli errori non sostanziale. Anche qui vi è una correlazione di 0,312.
Comunque si preferisce il modello 5 poichè il p-value è sotto lo 0,001. R^2 da 0.413 a
0.567"

#11) testscr- rdscr
{
  #simple linear regression
  plot(star$read_scr,star$testscr, col= star$calw_cat)
  simple_fm <-lm(testscr~read_scr,data=star)
  simple_fm$coefficients
  abline(simple_fm$coefficients[1],simple_fm$coefficients[2],col="dark green")
  summary(simple_fm)
  
  #same slope different intercepts
  plot(star$read_scr,star$testscr, col= star$calw_cat)
  mod1 <- lm(testscr~calw_cat+read_scr,data=star)
  summary(mod1)
  mod1$coefficients
  no_calw <- c(mod1$coefficients[1],mod1$coefficients[3])
  yes_calw <- c(mod1$coefficients[1]+mod1$coefficients[2],mod1$coefficients[3])
  abline(no_calw[1],no_calw[2],col="black")
  abline(yes_calw[1],yes_calw[2],col="red")
  
  #we do with same intercept and different slopes
  plot(star$read_scr,star$testscr, col= star$calw_cat)
  mod4 <- lm(testscr~enrl_tot_pct+read_scr:calw_cat,data=star) #A:B==A*B
  summary(mod4)
  no_calw <- c(mod4$coefficients[1],mod4$coefficients[2])
  yes_calw <- c(mod4$coefficients[1],mod4$coefficients[2]+mod4$coefficients[3])
  abline(no_calw[1],no_calw[2],col="black")
  abline(yes_calw[1],yes_calw[2],col="red")
  
  #different slope different intercepts
  plot(star$read_scr,star$testscr, col= star$calw_cat)
  mod5 <- lm(testscr~read_scr*calw_cat,data=star) #A*B==A+B+A*B
  summary(mod5)
  mod5$coefficients
  no_calw <- c(mod5$coefficients[1],mod5$coefficients[2])
  yes_calw <- c(mod5$coefficients[1]+mod5$coefficients[3]
                ,mod5$coefficients[2]+mod5$coefficients[4])
  abline(no_calw[1],no_calw[2],col="black")
  abline(yes_calw[1],yes_calw[2],col="red")
  
  anova(simple_fm,mod1,mod4,mod5)
  
}

#12) testscr - mathscr
{
  #simple linear regression
  plot(star$el_pct,star$testscr, col= star$calw_cat)
  simple_fm <-lm(testscr~el_pct,data=star)
  simple_fm$coefficients
  abline(simple_fm$coefficients[1],simple_fm$coefficients[2],col="dark green")
  summary(simple_fm)
  #same slope different intercepts
  plot(star$math_scr,star$testscr, col= star$calw_cat)
  mod1 <- lm(testscr~calw_cat+math_scr,data=star)
  summary(mod1)
  mod1$coefficients
  no_calw <- c(mod1$coefficients[1],mod1$coefficients[3])
  yes_calw <- c(mod1$coefficients[1]+mod1$coefficients[2],mod1$coefficients[3])
  abline(no_calw[1],no_calw[2],col="black")
  abline(yes_calw[1],yes_calw[2],col="red")
  
  #we do with same intercept and different slopes
  plot(star$math_scr,star$testscr, col= star$calw_cat)
  mod4 <- lm(testscr~enrl_tot_pct+math_scr:calw_cat,data=star) #A:B==A*B
  summary(mod4)
  no_calw <- c(mod4$coefficients[1],mod4$coefficients[2])
  yes_calw <- c(mod4$coefficients[1],mod4$coefficients[2]+mod4$coefficients[3])
  abline(no_calw[1],no_calw[2],col="black")
  abline(yes_calw[1],yes_calw[2],col="red")
  
  #different slope different intercepts
  plot(star$math_scr,star$testscr, col= star$calw_cat)
  mod5 <- lm(testscr~math_scr*calw_cat,data=star) #A*B==A+B+A*B
  summary(mod5)
  mod5$coefficients
  no_calw <- c(mod5$coefficients[1],mod5$coefficients[2])
  yes_calw <- c(mod5$coefficients[1]+mod5$coefficients[3]
                ,mod5$coefficients[2]+mod5$coefficients[4])
  abline(no_calw[1],no_calw[2],col="black")
  abline(yes_calw[1],yes_calw[2],col="red")
  
  anova(simple_fm,mod1,mod4,mod5)
  summary(simple_fm)
  summary(mod1)
}

#Regressione Multipla
{
  #initialization
  {
  index0 <- which(star$calw_cat == "0")
  index1 <- which(star$calw_cat == "1")
  star2 <- star[,c(-1,-13,-14,-15)]
  subset0 <- star2[index0,]
  subset1 <- star2[index1,]
  }
  #multiple regression model
  {
  fit_all = lm(testscr ~ ., data=star2)
  in_m <- lm(testscr ~ 1,data=star2)
  summary(in_m)
  step(in_m,direction="forward",scope=formula(fit_all))
  fm <- lm(formula = testscr ~ meal_pct + avginc + el_pct + expn_stu + 
             comp_stu, data = star2)
  }
  
  #Multiple Regression with backward elimination
  {
  fit_rev = lm(testscr ~ .,data=star2[,-12])
  summary(fit_rev) #la meno significativa è teachers
  
  fit_rev <- update(fit_rev, . ~ .-teachers)
  summary(fit_rev)
  
  fit_rev <- update(fit_rev, . ~ .-enrl_tot) 
  summary(fit_rev)
  
  fit_rev <- update(fit_rev, . ~ .-computer) 
  summary(fit_rev)
  
  fit_rev <- update(fit_rev, . ~ .-str) 
  summary(fit_rev)
  
  fit_rev <- update(fit_rev, . ~ .-calw_pct) 
  summary(fit_rev)
  
fit_rev1 <- lm(formula = testscr ~ meal_pct + comp_stu + expn_stu + avginc + 
       el_pct, data = star2[, -12]) #corrisponde al metodo stepwise
summary(fit_rev1)

  }
  
  #let's check if an additional dummy variable could improve our model
  {
  fm_int <- update(fm,. ~ . + calw_cat) #adding dummy_var
  summary(fm_int) #p-value is 0.715 so it's not convenient
  fm_slp <- update(fm,. ~ . :calw_cat )
  summary(fm_slp)
  anova(fm, fm_slp) #we loose 5df and the model is more complicated. However, F
                    #is not significant and we won't add the dummy variable
  }
  
  #confronto tra modelli
  {
    fm1 <- lm(formula = testscr ~ meal_pct, data = star2)
    fm2 <- lm(formula = testscr ~ meal_pct + avginc, data = star2)  
    fm3 <- lm(formula = testscr ~ meal_pct + avginc + el_pct, data = star2)  
    fm4 <- lm(formula = testscr ~ meal_pct + avginc + el_pct + expn_stu, data = star2)  
    fm5 <- lm(formula = testscr ~ meal_pct + avginc + el_pct + expn_stu + 
                comp_stu, data = star2)    
    fm6 <- lm(formula= testscr ~ ., data=star2)
    anova(fm1,fm2,fm3,fm4,fm5,fm6) 
    #fm5 is the best model considering a 0.05 significance level
    summary(fm5) #R^2 adj=0.806
    }
    
  #diagnostica
    {
    summary(fm5)
    plot(fm5) 
    
    residui<-fm5$res
    yfit<-fitted(fm5)
    #linearità - residui vs fitted
    par(mfrow=c(3,2))
    plot(star2[,4],residui,xlab=names(star2)[4])
    plot(star2[,10],residui,xlab=names(star2)[10])
    plot(star2[,11],residui,xlab=names(star2)[11])  
    plot(star2[,8],residui,xlab=names(star2)[8])
    plot(star2[,7],residui,xlab=names(star2)[7])    
  
    bptest(fm5)
    
    }
  
}
