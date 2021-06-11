library(readxl)
library(tidyr)
library(dplyr)
library(data.table)

data<-read.csv("Desktop/UMN MSBA/Stats/WPI_5Species (1).csv")
##Data preparation and cleaning - filter data for atlantic cod and herring
##Created unique ID using station, stratum(can use depth as  well), year
##splitted the data for atlantic cod and herring(two separate dataframes)
##joined them by ID(station, stratum(can use depth as  well), year)
##we need to imporve this 

data<-data%>%filter(SEASON=="FALL")
data<-unite_(data,"ID",c('STATION','YEAR'),remove = FALSE)

data_hake<-data%>%filter(SPECIES == 'Silver Hake')
data_herr<-data%>%filter(SPECIES == 'Atlantic Herring')
data_cod<-data%>%filter(SPECIES == 'Atlantic Cod')

data_hake<-data_hake%>%select("ID","SPECIES","biomass")
data_herr<-data_herr%>%select("ID","SPECIES","biomass")

colnames(data_hake)[colnames(data_hake)=="biomass"] <- "biomass_hake"
colnames(data_herr)[colnames(data_herr)=="biomass"] <- "biomass_herring"

data_join<-left_join(data_cod,data_hake, by='ID') %>%
  left_join(., data_herr, by='ID')

data_join<-data_join%>%select(biomass,biomass_herring,biomass_hake,
                              ID, SURFTEMP, BOTTEMP, DEPTH)

data_clean<-data_join%>%drop_na()
str(data_clean)
#data_clean<-data_clean%>%filter(biomass < 11 & biomass_herring < 4)
data_final<-data_clean
setnames(data_final, old=c("biomass","biomass_herring","biomass_hake",
                           "SURFTEMP", "BOTTEMP","DEPTH"), 
         new=c("mass_c", "mass_h","mass_ha","s_temp","b_temp","depth"))
summary(data_final)
str(data_final)
#=====================DATA CLEANING COMPLETED===============================


#=============================Model Fitting==================================

##Once we are done with finding correlation next step is to fit the model
#fit model between vairables 
fit1 <- lm(mass_c ~ s_temp + b_temp + depth + mass_ha + mass_h, data = data_final)
summary(fit1)


fit2 <- lm(mass_c ~ s_temp + b_temp + depth + mass_h, data = data_final)
summary(fit2)
#plot relationship between biomass od cod and herring
#plot(data_clean$biomass_herring, data_clean$biomass, pch = 16 ,  main = "Atlantic Cod",  xlab = "biomass_herring",  ylab = "Biomass Cod")
#abline(lm(data_clean$biomass ~ data_clean$biomass_herring), lty=   2,  col=   "red")
library(car)
avPlots(fit1)

pairs(~  mass_c + b_temp+ depth+ mass_h, main="Simple Scatterplot Matrix")

#==============================================================
fit.stres <- rstandard(fit1)
plot(fit$fitted.values, fit.stres, pch=16, 
     main="Standardized Residual Plot", xlab="Fitted Cod biomass", 
     ylab = "Standardized Residuals")
abline(0, 0, lty=2, col="red")

h <- hist(fit.stres)
x <- fit.stres
xfit <- seq(min(x), max(x), length = 50 )
yfit <- dnorm(xfit, mean=mean(x), sd = sd(x))
yfit <- yfit*diff(h$mids[1:2])*length(x)
lines(xfit, yfit, col= "blue")

qqnorm(fit.stres, main = "Normal Probability Plot",  xlab = "Normal Scores",  ylab = "Standardized Residuals", xlim = c(-2, 2), ylim = c(-2,2))
qqline(fit.stres, col = "red")

shapiro.test(fit.stres)
#++========================Try Panel Method======================

#======================Exploratory Data Analysis=============================
##Once data has been cleaned next step is to find correlation between variables
##I have chosen the variables mentioned in the proposal
#correlation heatmap 

###run the following line for correlation matrix
mydata <- data_final%>%select("mass_c", "mass_h","mass_ha","s_temp","b_temp","depth")
cormat <- round(cor(mydata),2)
cormat
#Below code line 41 to 100 is for plotting the heatmap of correlation, ignore it. Skip to line 97
library(reshape2)
melted_cormat <- melt(cormat)
library(ggplot2)
ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile()

# Get lower triangle of the correlation matrix
get_lower_tri<-function(cormat){
  cormat[upper.tri(cormat)] <- NA
  return(cormat)
}
# Get upper triangle of the correlation matrix
get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}
upper_tri <- get_upper_tri(cormat)
upper_tri

# Melt the correlation matrix
library(reshape2)
melted_cormat <- melt(upper_tri, na.rm = TRUE)
# Heatmap
library(ggplot2)
ggplot(data = melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "white", high = "red", mid = "blue", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()

reorder_cormat <- function(cormat){
  # Use correlation between variables as distance
  dd <- as.dist((1-cormat)/2)
  hc <- hclust(dd)
  cormat <-cormat[hc$order, hc$order]
}
# Reorder the correlation matrix
cormat <- reorder_cormat(cormat)
upper_tri <- get_upper_tri(cormat)
# Melt the correlation matrix
melted_cormat <- melt(upper_tri, na.rm = TRUE)
# Create a ggheatmap
ggheatmap <- ggplot(melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "yellow", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ # minimal theme
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()
# Print the heatmap
print(ggheatmap)
#Plot the relation between varialbles
library(ggplot2)
library(plotly)

data123<-data_clean%>%select(YEAR,biomass)
data456<-data_clean%>%select(YEAR,biomass_herring)
ggplot(data123, aes(x=YEAR, y=biomass)) + geom_point()
data789<-data_clean%>%select(biomass,biomass_herring)
ggplot(data789, aes(x=biomass_herring, y=biomass)) + geom_point()



library(plm)

fit2<-plm(biomass ~ BOTTEMP+ DEPTH+
           SURFTEMP+ biomass_herring,data =data_final
         ,index = c("YEAR"))
summary(fit2)
