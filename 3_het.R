library(data.table)
library(ggplot2)
library(magrittr)

astra_h_whole_listings<-fread("data/astra_h_ads_20190912_cleaned.csv",encoding = "UTF-8")
View(head(astra_h_whole_listings))
names(astra_h_whole_listings)

astra_h_whole_listings

#histogram
ggplot(astra_h_whole_listings,aes(x=`Vételár`))+geom_histogram()+theme_minimal()
ggplot(astra_h_whole_listings)+geom_histogram(aes(x=`Kilométeróra állása`))

#scatterplot
ggplot(astra_h_whole_listings[Kivitel %in% c("Ferdehátú","Kombi")],aes(y=`Vételár`,x=`Kilométeróra állása`))+geom_point(aes(col=Kivitel))+
    scale_x_continuous(labels=scales::comma)+scale_y_continuous(labels=scales::comma)+theme_minimal()+geom_smooth(method = "lm")+
    labs(title = "Valami cím",y="Vételár(Ft)",x="Kilométeróra állása(km)",subtitle = "Valami alcím",caption = "blabla")+
    theme(plot.title = element_text(color="pink"),legend.position = "top")+facet_grid(Kivitel~`Évjárat`)


astra_h_whole_listings[,.N,by=`Kivitel`]  

ggplot(astra_h_whole_listings,aes(y=`Vételár`,x=`Kivitel`))+geom_boxplot()

ggplot(astra_h_whole_listings,aes(y=`Vételár`,x=`Üzemanyag`))+geom_boxplot()


# csak x-et irtuk at, ctrlc ctrlv helyett inkabb irjunk fuggvenyt

plotPricevsSelectedvariable<-function(car_ads,selected_variable){
  ggplot(car_ads,aes_string(y="Vételár",x=selected_variable))+geom_boxplot()
}
plotPricevsSelectedvariable(astra_h_whole_listings,"Kivitel")
plotPricevsSelectedvariable(astra_h_whole_listings,"Üzemanyag")
