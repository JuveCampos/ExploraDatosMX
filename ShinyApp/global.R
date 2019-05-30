# Librerias y fincuones
library(dplyr)
library(readr)
library(scales)
library(ggplot2)
library(treemapify)
library(highcharter)
library(plotly)
niveles <- function(x) levels(as.factor(x))


# 1. Leemos las bases de datos 
bd_completa <- read_csv("www/presupuesto_ddhh_2013_2019_juan.csv")

# 2. Armamos los treemaps
# Bases
pat <- c("Comisión Nacional de los Derechos Humanos" , 
         "Instituto Nacional de Transparencia, Acceso a la Información y Protección de Datos Personales", 
         "Procuraduría General de la República"
)

bd_completa$DESC_RAMO <-  stringr::str_replace(bd_completa$DESC_RAMO, pattern = pat[1], replacement = c("CNDH", "INAI", "PGR")[1])
bd_completa$DESC_RAMO <-  stringr::str_replace(bd_completa$DESC_RAMO, pattern = pat[2], replacement = c("CNDH", "INAI", "PGR")[2])
bd_completa$DESC_RAMO <-  stringr::str_replace(bd_completa$DESC_RAMO, pattern = pat[3], replacement = c("CNDH", "INAI", "PGR")[3])

#base treemap completo
bd_tree_completa<-bd_completa %>% 
  group_by(CICLO,DESC_RAMO,DESC_PP) %>% 
  summarise(montos=sum(MONTO_APROBADO)) %>% 
  mutate(ll=paste("$",comma(montos),sep=""),
         descl=paste(DESC_PP,ll,sep=": "))

#base treemap ramos
tree_ramos<-bd_tree_completa%>% 
  group_by(CICLO,DESC_RAMO) %>% 
  summarise(monto_ramo=sum(montos)) %>% 
  mutate(ll=paste("$",comma(monto_ramo),sep=""),
         label_ramo=paste(DESC_RAMO,ll,sep=": "))

# Bases Nueva 
b1_ramo_year <- read.csv("www/presup_ramo_year.csv")
b2_tipo_gasto <- read.csv("www/presup_tipogasto_year.csv")
b3_top_10 <- read.csv("www/presup_top10_concepto_ramo_year.csv")

# Caso n = 1
niveles(b1_ramo_year$DESC_RAMO)
rammo <- "Defensa Nacional"

b1 <- b1_ramo_year %>%
  filter(DESC_RAMO == "Defensa Nacional")

b2 <- b2_tipo_gasto %>%
  filter(DESC_RAMO == "Comisión Nacional de los Derechos Humanos")

ggplot(b2, aes(x = CICLO, y = aprobado, color = DESC_TIPOGASTO)) + geom_line()

p <- ggplot(b1, aes(x = CICLO, y = aprobado, color = DESC_TIPOGASTO)) + geom_bar(stat = 'identity', fill = 'purple') +
  labs(title = "Evolución del presupuesto",
       x = "Año",
       y = "Presupuesto Aprobado (millones de Pesos)") + theme_bw()


# Temas de graficar 
tema_juve <- theme_bw() + theme(text = element_text(family = "Asap-Bold", color = "#25636e"), 
                                panel.grid.major = element_blank(),
                                panel.grid.minor = element_blank(), 
                                plot.caption=element_text(hjust=1,size=9,colour="grey30"),
                                plot.subtitle=element_text(face="italic",size=12,colour="grey40"),
                                plot.title=element_text(size=18,face="bold"),
                                axis.text.x = element_text(family = "Asap-Bold", color = "#25636e"),
                                axis.text.y = element_text(family = "Asap-Bold", color = "#25636e"))








