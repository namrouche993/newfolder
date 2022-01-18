library(tidyverse)
library(reactable)
library(readxl)
library(showtext)
library(grDevices)


full_table <- read_excel("full_table.xlsx")
full_table$`Creances Reclames par SADEG - Energie`=as.numeric(full_table$`Creances Reclames par SADEG - Energie`)


for(i in 1:194) {
  for(j in c(3,4,5,7)){
    if (is.na(as.numeric(full_table[i,j]))==TRUE){
      full_table[i,j]=0
    }
  }
}

# 
# font_add_google("Merriweather Sans", "merri")
# font_add_google("Mukta Mahee", "muktama")
# showtext_auto()


sadeg_energie_pal <- function(x) rgb(colorRamp(c("#ffe4cc", "#ffb54d"),interpolate = "spline")(x), maxColorValue = 255)
sadeg_energie_pal2 <- function(x) rgb(colorRamp(c("#e5a245", "#66481e"),interpolate = "spline")(x), maxColorValue = 255)


sadeg_travaux_pal <- function(x) rgb(colorRamp(c("#dbc3a5", "#995500"),interpolate = "spline")(x), maxColorValue = 255)
sadeg_travaux_pal2 <- function(x) rgb(colorRamp(c("#995500", "#6b3b00"),interpolate = "spline")(x), maxColorValue = 255)
sadeg_travaux_pal0 <- function(x) rgb(colorRamp(c("#f7f3ed", "#f7f3ed"),interpolate = "spline")(x), maxColorValue = 255)

sadeg_total_pal <- function(x) rgb(colorRamp(c("#af6666", "#cc0000"),interpolate = "spline")(x), maxColorValue = 255)
sadeg_total_pal2 <- function(x) rgb(colorRamp(c("#cc0000", "#a30000"),interpolate = "spline")(x), maxColorValue = 255)
sadeg_total_pal0 <- function(x) rgb(colorRamp(c("#f7f3ed", "#c79393"),interpolate = "spline")(x), maxColorValue = 255)


diff_energie_pal <- function(x) rgb(colorRamp(c("#e5cccc", "#aa5555"),interpolate = "spline")(x), maxColorValue = 255)





# sadeg_travaux_pal <- function(x) rgb(colorRamp(c("#ffe4cc", "#ffb54d"),interpolate = "spline")(x), maxColorValue = 255)
# sadeg_travaux_pal2 <- function(x) rgb(colorRamp(c("#e5a245", "#66481e"),interpolate = "spline")(x), maxColorValue = 255)
# 


topgi=full_table %>% 
  filter(`Maitre Ouvrage`=="OPGI") %>% 
  mutate(creances_reclames_sadeg=`Creances Reclames par SADEG - Energie`
         +`Creances Reclames par SADEG - TRAVAUX`,
         .after=`Creances Reclames par SADEG - TRAVAUX`
         ) %>% 
  mutate(creances_declares_mo=`Creances declares par MO - Energie`+`Creances declares par MO - Travaux`,
         .after=`Creances declares par MO - Travaux`
         ) %>% 
  mutate(diff_energie=`Creances Reclames par SADEG - Energie`- `Creances declares par MO - Energie`,
         diff_travaux=`Creances Reclames par SADEG - TRAVAUX`- `Creances declares par MO - Travaux`,
         diff_total=creances_reclames_sadeg- creances_declares_mo
         ) %>% 
  select(1,3,4,5,6,8,9,11,12,13)

for(i in 1:50){
  topgi$id_wilaya[i]=as.numeric(unlist(str_split(topgi$Wilaya[i],"-"))[1])
}



topgi %>% select(-11) %>% 
  reactable(
    highlight=TRUE,pagination =FALSE,fullWidth = TRUE,
    #defaultSorted = c("Species", "Petal.Length")
    defaultColDef = colDef(
      format = colFormat(separators = TRUE,digits = 2,locales = "fr-FR",suffix = " DA"),
      sortNALast = TRUE,
      style = list(fontFamily="Mukta Mahee")
      ,footerStyle = list(fontWeight = "bold")
    ),
    columns=list(
      Wilaya=colDef(
        name="",
        format=colFormat(suffix="")
        ,footer = "Total"
        
      ),
      `Creances Reclames par SADEG - Energie`=colDef(
        name="Energie",
        
        footer=paste(format(sum(topgi$`Creances Reclames par SADEG - Energie`,na.rm = TRUE)
                      , digits=3,scientific = FALSE,big.mark=" ",nsmall = 2),"DA"),
        headerStyle =list(
          borderLeft="55px inset transparent"
        ),
        style = function(value) {
          normalized <- (value - min(topgi$`Creances Reclames par SADEG - Energie`)) / 
            
            (max(
              topgi %>%
                filter(!id_wilaya %in% c(15,21,25,09,31,26,7,6,23)) %>% 
                select(y=`Creances Reclames par SADEG - Energie`) %>% .$y
              )
             
             - min(topgi$`Creances Reclames par SADEG - Energie`))
          if(normalized>1){
            normalized2 <- (value - min(
              topgi %>%
                filter(id_wilaya %in% c(15,21,25,09,31,26,7,6,23)) %>% 
                select(y=`Creances Reclames par SADEG - Energie`) %>% .$y
            )) / 
              
              (max(
                topgi %>%
                  filter(id_wilaya %in% c(15,21,25,09,31,26,7,6,23)) %>% 
                  select(y=`Creances Reclames par SADEG - Energie`) %>% .$y
              )
              
              - min(
                
                topgi %>%
                  filter(id_wilaya %in% c(15,21,25,09,31,26,7,6,23)) %>% 
                  select(y=`Creances Reclames par SADEG - Energie`) %>% .$y
              ))
            
            
            color <- sadeg_energie_pal2(normalized2)
            list(background = color)
          } else {
            color <- sadeg_energie_pal(normalized)
            list(background = color)
          }
          
          }
      ),
      `Creances Reclames par SADEG - TRAVAUX`=colDef(
        name="Travaux",
        footer=paste(format(sum(topgi$`Creances Reclames par SADEG - TRAVAUX`,na.rm = TRUE)
                            , digits=3,scientific = FALSE,big.mark=" ",nsmall = 2),"DA"),
        style = function(value) {
          normalized <- (value - min(topgi$`Creances Reclames par SADEG - TRAVAUX`)) / 
            
            (max(
              topgi %>%
                filter(id_wilaya %in% c(26,15,42)) %>% 
                select(y=`Creances Reclames par SADEG - TRAVAUX`) %>% .$y
            )
            
            - min(topgi$`Creances Reclames par SADEG - TRAVAUX`))
          
          if(value<100000){
            color <- sadeg_travaux_pal0(normalized)
            list(background = color)
          } else if (normalized>1){
            normalized2 <- (value - min(
              topgi %>%
                filter(id_wilaya %in% c(26,15,42)) %>% 
                select(y=`Creances Reclames par SADEG - TRAVAUX`) %>% .$y
            )) / 
              
              (max(
                topgi %>%
                  filter(id_wilaya %in% c(26,15,42)) %>% 
                  select(y=`Creances Reclames par SADEG - TRAVAUX`) %>% .$y
              )
              
              - min(
                
                topgi %>%
                  filter(id_wilaya %in% c(26,15,42)) %>% 
                  select(y=`Creances Reclames par SADEG - TRAVAUX`) %>% .$y
              ))
            
            
            color <- sadeg_travaux_pal2(normalized2)
            list(background = color)
          } else {
            color <- sadeg_travaux_pal(normalized)
            list(background = color)
          }
          
        }
        
      ),
      creances_reclames_sadeg=colDef(
        name="Total",
        footer=paste(format(sum(topgi$creances_reclames_sadeg,na.rm = TRUE)
                            , digits=3,scientific = FALSE,big.mark=" ",nsmall = 2),"DA"),
        headerStyle =list(
          borderRight="35px inset transparent"
        ),
        
        style = function(value) {
          normalized <- (value - min(topgi$creances_reclames_sadeg)) / 
            
            (max(
              topgi %>%
                filter(!id_wilaya %in% c(26,15,42)) %>% 
                select(y=creances_reclames_sadeg) %>% .$y
            )
            
            - min(topgi$creances_reclames_sadeg))
          
          if(value<350000){
            color <- sadeg_total_pal0(
              
              (value - min(topgi$creances_reclames_sadeg)) /
              (max(
                topgi %>%
                  filter(id_wilaya %in% c(topgi$id_wilaya[topgi$creances_reclames_sadeg<350000])) %>% 
                  select(y=creances_reclames_sadeg) %>% .$y
              )
              
              - min(topgi$creances_reclames_sadeg))
              
            )
            list(background = color)
          } else if (normalized>1){
            normalized2 <- (value - min(
              topgi %>%
                filter(id_wilaya %in% c(26,15,42)) %>% 
                select(y=creances_reclames_sadeg) %>% .$y
            )) / 
              
              (max(
                topgi %>%
                  filter(id_wilaya %in% c(26,15,42)) %>% 
                  select(y=creances_reclames_sadeg) %>% .$y
              )
              
              - min(
                
                topgi %>%
                  filter(id_wilaya %in% c(26,15,42)) %>% 
                  select(y=creances_reclames_sadeg) %>% .$y
              ))
            
            
            color <- sadeg_total_pal2(normalized2)
            list(background = color)
          } else {
            color <- sadeg_total_pal(normalized)
            list(background = color)
          }
          
        }
        
        
      ),
      
      `Creances declares par MO - Energie`=colDef(
        name="Energie",
        footer=paste(format(sum(topgi$`Creances declares par MO - Energie`,na.rm = TRUE)
                            , digits=3,scientific = FALSE,big.mark=" ",nsmall = 2),"DA"),
        headerStyle =list(
          borderLeft="35px inset transparent"
        ),
        style = function(value) {
          
          normalized <- (value - min(topgi$`Creances Reclames par SADEG - Energie`)) / 
            
            (max(
              topgi %>%
                filter(!id_wilaya %in% c(15,21,25,09,31,26,7,6,23)) %>% 
                select(y=`Creances Reclames par SADEG - Energie`) %>% .$y
            )
            
            - min(topgi$`Creances Reclames par SADEG - Energie`))
          if(normalized>1){
            normalized2 <- (value - min(
              topgi %>%
                filter(id_wilaya %in% c(15,21,25,09,31,26,7,6,23)) %>% 
                select(y=`Creances Reclames par SADEG - Energie`) %>% .$y
            )) / 
              
              (max(
                topgi %>%
                  filter(id_wilaya %in% c(15,21,25,09,31,26,7,6,23)) %>% 
                  select(y=`Creances Reclames par SADEG - Energie`) %>% .$y
              )
              
              - min(
                
                topgi %>%
                  filter(id_wilaya %in% c(15,21,25,09,31,26,7,6,23)) %>% 
                  select(y=`Creances Reclames par SADEG - Energie`) %>% .$y
              ))
            
            
            color <- sadeg_energie_pal2(normalized2)
            list(background = color,marginLeft="15px")
            
          } else {
            color <- sadeg_energie_pal(normalized)
            list(background = color,marginLeft="15px")
          }
          
        }
        
      ),
      `Creances declares par MO - Travaux`=colDef(
        name="Travaux",
        footer=paste(format(sum(topgi$`Creances declares par MO - Travaux`,na.rm = TRUE)
                            , digits=3,scientific = FALSE,big.mark=" ",nsmall = 2),"DA"),
        style = function(value) {
          normalized <- (value - min(topgi$`Creances Reclames par SADEG - TRAVAUX`)) / 
            
            (max(
              topgi %>%
                filter(id_wilaya %in% c(26,15,42)) %>% 
                select(y=`Creances Reclames par SADEG - TRAVAUX`) %>% .$y
            )
            
            - min(topgi$`Creances Reclames par SADEG - TRAVAUX`))
          
          if(value<100000){
            color <- sadeg_travaux_pal0(normalized)
            list(background = color)
          } else if (normalized>1){
            normalized2 <- (value - min(
              topgi %>%
                filter(id_wilaya %in% c(26,15,42)) %>% 
                select(y=`Creances Reclames par SADEG - TRAVAUX`) %>% .$y
            )) / 
              
              (max(
                topgi %>%
                  filter(id_wilaya %in% c(26,15,42)) %>% 
                  select(y=`Creances Reclames par SADEG - TRAVAUX`) %>% .$y
              )
              
              - min(
                
                topgi %>%
                  filter(id_wilaya %in% c(26,15,42)) %>% 
                  select(y=`Creances Reclames par SADEG - TRAVAUX`) %>% .$y
              ))
            
            
            color <- sadeg_travaux_pal2(normalized2)
            list(background = color)
          } else {
            color <- sadeg_travaux_pal(normalized)
            list(background = color)
          }
          
        }
        
      ),
      creances_declares_mo=colDef(
        name="Total",
        footer=paste(format(sum(topgi$creances_declares_mo,na.rm = TRUE)
                            , digits=3,scientific = FALSE,big.mark=" ",nsmall = 2),"DA"),
        
        headerStyle =list(
          borderRight="35px inset transparent"
        ),
        
        style = function(value) {
          normalized <- (value - min(topgi$creances_reclames_sadeg)) / 
            
            (max(
              topgi %>%
                filter(!id_wilaya %in% c(26,15,42)) %>% 
                select(y=creances_reclames_sadeg) %>% .$y
            )
            
            - min(topgi$creances_reclames_sadeg))
          
          if(value<350000){
            color <- sadeg_total_pal0(
              
              (value - min(topgi$creances_reclames_sadeg)) /
                (max(
                  topgi %>%
                    filter(id_wilaya %in% c(topgi$id_wilaya[topgi$creances_reclames_sadeg<350000])) %>% 
                    select(y=creances_reclames_sadeg) %>% .$y
                )
                
                - min(topgi$creances_reclames_sadeg))
              
            )
            list(background = color)
          } else if (normalized>1){
            normalized2 <- (value - min(
              topgi %>%
                filter(id_wilaya %in% c(26,15,42)) %>% 
                select(y=creances_reclames_sadeg) %>% .$y
            )) / 
              
              (max(
                topgi %>%
                  filter(id_wilaya %in% c(26,15,42)) %>% 
                  select(y=creances_reclames_sadeg) %>% .$y
              )
              
              - min(
                
                topgi %>%
                  filter(id_wilaya %in% c(26,15,42)) %>% 
                  select(y=creances_reclames_sadeg) %>% .$y
              ))
            
            
            color <- sadeg_total_pal2(normalized2)
            list(background = color)
          } else {
            color <- sadeg_total_pal(normalized)
            list(background = color)
          }
          
        }
        
      ),
      diff_energie=colDef(
        name="Energie",
        footer=paste(format(sum(topgi$diff_energie,na.rm = TRUE)
                            , digits=3,scientific = FALSE,big.mark=" ",nsmall = 2),"DA"),
        headerStyle =list(
          borderLeft="35px inset transparent"
        )
        # ,
        # 
        # style = function(value) {
        #   normalized <- (value - min(topgi$diff_energie)) / 
        #     
        #     (max(
        #       topgi %>%
        #         filter(!id_wilaya %in% c(21)) %>% 
        #         select(y=diff_energie) %>% .$y
        #     )
        #     
        #     - min(topgi$diff_energie))
        #   
        #   if(value<0){
        #     color <- "#018370"
        #     list(background = color,marginLeft="15px")
        #   } else if(value==0) {
        #     color <- "#f6eeee"
        #     list(background = color,marginLeft="15px")
        #   } else if (normalized>1){
        #     color <- "#763b3b"
        #     list(background = color,marginLeft="15px")
        #   } else {
        #     color <- diff_energie_pal(normalized)
        #     list(background = color,marginLeft="15px")
        #   }
        #   
        # }
        # 
        
      ),
      
      diff_travaux=colDef(
        name="Travaux",
        footer=paste(format(sum(topgi$diff_travaux,na.rm = TRUE)
                            , digits=3,scientific = FALSE,big.mark=" ",nsmall = 2),"DA")
      ),
      
      diff_total=colDef(
        name="Total",
        footer=paste(format(sum(topgi$diff_total,na.rm = TRUE)
                            , digits=3,scientific = FALSE,big.mark=" ",nsmall = 2),"DA")
      )
      
    ),
    columnGroups = list(
      colGroup(name = "Créances Réclamés par SADEG",
               headerStyle =list(
                 borderRight="25px inset transparent",
                 borderLeft="35px inset transparent"
                 
               ),
               columns = c("Creances Reclames par SADEG - Energie","Creances Reclames par SADEG - TRAVAUX","creances_reclames_sadeg")),
      
      colGroup(name = "Créances Déclarés par OPGI",
               headerStyle =list(
                 borderRight="25px inset transparent",
                 borderLeft="25px inset transparent"
               ),
               columns = c("Creances declares par MO - Energie","Creances declares par MO - Travaux","creances_declares_mo")
               ),
      
      
      colGroup(name = "Différence",
               headerStyle =list(
                 borderRight="25px inset transparent",
                 borderLeft="25px inset transparent"
               ),
               columns = c("diff_energie","diff_travaux","diff_total")
      )
    )
    
            #bordered = TRUE,
              #highlight = TRUE
    )
