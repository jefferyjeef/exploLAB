library(dplyr)
library(ggplot2)
install.packages("SmarterPoland")
library(SmarterPoland)
install.packages("forcats")
library(forcats)

countries[countries$country == "Niue",]
sum(countries$population) 
# `population` jest w tysiącach

## Zadanie 1
# 1. Ograniczyć zbiór krajów do tych, których nazwa jest krótsza niż 8 znaków (nchar).
# 2. Stworzyć zmienną logarytm populacji (*1000) 
#    i posortować względem niej poziomy zmiennej kraju (forcats::fct_reorder).
# 3. Zrobić wykres słupkowy pokazujący logarytm populacji w poszczególnych krajach 
#    i zaznaczyć kolorem kontynent (wykres poziomy).


### Skale (scale)
#1.
p<-countries%>%
  filter(nchar(country)<8)%>%
  mutate(logPop = 3+log(population))%>%
  mutate(country = fct_reorder(country,logPop))%>%
  ggplot(aes(y=country,x=logPop,fill=continent))+
  geom_col()


## Osie (x & y) 

p+scale_y_discrete(position = "right")

p+scale_y_discrete(guide = guide_axis(n.dodge = 3,title="country",angle=5))

p_point <- ggplot(countries,aes(x = birth.rate,y = death.rate,color = continent))+geom_point()
p_point

p_point + scale_y_continuous(expand = c(0,0))
p_point + scale_y_reverse()

p_point+scale_y_log10()
p_point+scale_color_manual(values=c("black","orange","green","grey","red"))
## Kolor (color & fill)


# color brewer http://colorbrewer2.org/#type=sequential&scheme=BuGn&n=3
# install.packages("RColorBrewer")
library(RColorBrewer)
RColorBrewer::brewer.pal(n = 5, name = "Blues")
p_point + scale_color_manual(values = RColorBrewer::brewer.pal(n=5,name = "Blues"))

ggplot(countries,aes(x = log(population)+3,y = death.rate, color = birth.rate))+
  geom_point(size=2)+scale_color_gradient(low = "lightblue",high = "red")

## Zadanie 2
# 1. Ograniczyć zbiór krajów, do tych z Azji i Europy (można wykorzystać dane z Zad1).
# 2. Policzyć stosunek współczynnika zgonów do współczynnika urodzeń.
# 3. Zrobić wykres słupkowy pokazujący logarytm populacji w poszczególnych krajach 
#    i zaznaczyć kolorem wskaźnik.

zad2<-countries%>%
  filter(continent %in% c("Asia","Europe"))%>%
  mutate(ratio = death.rate/birth.rate,logPop = log(population)+3)%>%
  mutate(country = fct_reorder(country,logPop))
  
ggplot(zad2,aes(y = country,x=logPop,fill = ratio))+
  geom_col()+scale_fill_gradient(low = "purple",high = "green")


### Legenda (theme & legend)
p_point + theme(legend.position = "bottom")
p_point + theme(legend.position = "none")
p_point + theme(legend.title = element_blank())

p_point + theme(legend.title = element_text(color = "green",size = 15),
                legend.text = element_text(color = "red", face = "bold"))

p_point + labs(title = "wykres",x = "wskaznik narodzin",y = "wskaznik smierci" , color = "black")
p_point + coord_flip()
p_point + coord_polar()+coord_cartesian()
tmp <- data.frame(table(countries$continent))
ggplot(tmp,aes(x="",y = Frew,fill - var1))+
  geom_col(width=1)+coord_polar("y",start=0)
p_point + facet_wrap(-continent)
p_point + facet_wrap(-continent,scales="free_x")

p_point + facet_wrap(size-continent)
### Koordynaty (coord)


# wykres kołowy (DANGER ZONE)
ggplot2::geom_pie()


## Zadanie 3
# 1. Stworzyć zmienną wielkość kraju, która przyjmuje K wartości w zależności
#    od podziału zmiennej populacji (np. K = 3, można wykorzystać dane z Zad1).
# 2. Zrobić wykres punktowy pokazujący zależność death.rate od birth.rate 
#    i zaznaczyć kolorem wielkość kraju.


### Panele (facet)


### How to plot? --->>> https://www.r-graph-gallery.com