library(tidyverse)
library(highcharter)
library(readxl)
library(showtext)


full_table <- read_excel("C:/Users/nagib/Desktop/creances_sonelgaz_hamid/creances_sadeg/full_table.xlsx",na = "0")
full_table = data.frame(full_table)
for(i in 1:194) {
  for(j in c(3,4,5,7)){
    if (is.na(as.numeric(full_table[i,j]))==TRUE){
      full_table[i,j]=0
    }
  }
}

font_add_google("Merriweather Sans", "merri")
topgi <- full_table %>% 
  filter(Maitre.Ouvrage=="OPGI") %>% 
  mutate("Creances Réclames par SADEG"=as.numeric(Creances.Reclames.par.SADEG...Energie)+
           as.numeric(Creances.Reclames.par.SADEG...TRAVAUX)) %>% 
  mutate("Creances Déclares par MO"=as.numeric(Creances.declares.par.MO...Energie)+
           as.numeric(Creances.declares.par.MO...Travaux)
  ) %>% select(1,9,10) %>% gather("reclames_declares","creances",2:3) %>% 
  filter(creances>100000) %>% 
  arrange(desc(creances))


font_add_google("Mukta Mahee", "muktama")
showtext_auto()

mytheme = hc_theme(
  title = list(fontFamily = "Erica One")
)

mytheme <- hc_theme(
  chart = list(
    #backgroundColor = "#f4fcfe", 
    style = list(
      fontFamily = "Mukta Mahee",fontSize=30
    )
  )
)

hchart(topgi,
       "bar",
       hcaes(x=Wilaya,y=creances,group=reclames_declares)
       ) %>% 
  
  hc_title(text = "<div style='
  margin-top: -45px;
  height: 74px;
  border-left: 10px solid #77a4cc;
  /*  position: absolute;
  */
    left: 50%;
    padding-left:6px;
    '
    
  class='verticalLine'>
                    <p style='display:block;
                    font-size: 36px;
                    margin-bottom: -27px;
                    '>Etat des Créances détenues par SADEG</p>
                    <p style='font-size:20px;'>Maitre d'Ouvrage : OPGI</p>
                  </div>
           ",align="left",
           #family="muktama",
           useHTML=TRUE
           ) %>% 
  #hc_subtitle(text = "Maitre d'Ouvrage : OPGI ",align="left") %>% 
  hc_tooltip(
    crosshairs = TRUE,
    backgroundColor = "#F0F0F0",
    borderColor="#212121",
    shared = TRUE, 
    borderWidth = 3,
    sort=TRUE,
    valueSuffix=" DA"
  ) %>% 
  hc_plotOptions(
    series = list(
      pointWidth=15,
      #groupPadding=0,
      pointPadding=0.9
    )
  ) %>% 
  hc_xAxis(
    title=list(text = ""),
    labels=list(style=list(fontSize= "18px"))
    
  ) %>% 
  hc_yAxis(max=max(topgi$creances),
           #tickPositions=c(0, 10000000, 50000000, 100000000, 300000000),
    labels=list(style=list(fontSize= "18px"))
    
  ) %>% 
  hc_add_theme(
    mytheme
  ) %>% 
  hc_size(height=2600,width=1200
          )

  

