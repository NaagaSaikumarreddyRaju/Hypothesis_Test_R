pdf("visualization.pdf")

df<- read.csv("master.csv")

# Our Data consists of Nominal Data as Independent Variable(Sex) and Ordinal/Interval Data as Dependent Variable(suicides.100k.pop)
x<- df$sex
y<- df$suicides.100k.pop
# Most of the Data is  present in between the Range of (0,120) remaining are more likely the unique Data up to 200 So we Plotted our Box Plot in Range of (0,120)

boxplot(y ~ x,
        data = df,
        xlab = "Sex(Gender)",
        ylab = "No of Suicides/100K_Pop",
        main = "Boxplot of Sex(Gender) Vs Count of Suicides/100K_Pop",
        ylim = c(0,120),
        col=c("gold3","royal blue"),
        border="black",
        las=1)

# Drawing Barplot Using aggregate Function
a <- aggregate(y~x,FUN=mean)
barplot(a$y,
        names=a$x,
        main=" Barplot of Male and Females SuicideCount/100K.Pop Mean",
        xlab = "Sex(Gender)",
        ylab = "mean of suicides/100k.pop",col=c("mistyrose","lightBlue"),
        args.legend = list(x="topleft"),
        legend.text = a$x,
        las=1)

#Stacked Barplot to show the Comparison of Proportions
# To do Stacked Barplot we will be using the ggplot2 Package
library(ggplot2)
Gender <- df$sex
Count  <- df$suicides.100k.pop
Age    <- df$age
Mydata <- data.frame(Age, Gender,Count)
p <- ggplot(Mydata,aes(Age,Count,fill=Gender))
p +geom_bar(stat="identity",position = "fill") + 
  xlab("Age")+ylab("Frequency of Male and Female Deaths") + 
  ggtitle("Frequency Proportions of Suicides Rates of Male and Female(1985-2015)") +
  theme_bw() +
  theme(legend.position = c(0.8,0.2))

#Grouped Barplot
p +geom_bar(stat="identity",position = "dodge") + 
  xlab("Age")+ylab("Frequency of Male and Female Deaths/100K Population") + 
  ggtitle(" Grouped Barplot of Suicides Rates of Male and Female(1985-2015)")+
  theme_bw()+theme(legend.position = c(0.1,0.8))

# If the Dependent Variable is Ordinal/Interval we need to Draw the Histogram for that Variable

hist(y,50,prob=T,xlim = c(0,150),ylim = c(0,0.1),main="suicides_rate/100K Population Frequency",xlab="suicides_rate/100K population",ylab = "Frequency",col="azure",las=1)

# If we Adjust the curve to 2
lines(density(y,adjust = 2),col="Red",lwd=2)

#Overlay Normal Curve Using Random Normal Distribution
y<- df$suicides.100k.pop
mn=mean(y)
stdD <- sd(y)
x <- seq(0,150,50)
y_norm <- rnorm(length(y),mn,stdD)

#Overlay Normal Curve
lines(density(y_norm,adjust = 2),col="Green",lwd=2)

# We've Performed the T test to Check dependency of Suicides Rate on Gender
Ho = "Gender has no effect on suicides"   # Stating the Null Hypothesis
Ha = "Gender has an effect on suicides"   # Stating the Alternate Hypothesis

males_suicides=df[df['sex'] =='male','suicides.100k.pop']
females_suicides=df[df['sex'] =='female','suicides.100k.pop']
res <- t.test(x=males_suicides,y=females_suicides, data =df, var.equal = TRUE)
res

dev.off()
