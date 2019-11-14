library(openxlsx)
library(dplyr)
library(hgchmagic)
library(DT)
library(htmlwidgets)

tm_l <- list (
  font_family = "Open Sans",
  font_color = '#758988',
  stylesLabelY_fontSize = '15px',
  labsData_familyLabel = "Open Sans",
  colors = c('#21F4A7', '#284F4F', '#DCF763', '#A06CD5', '#DF57BC', '#1C66EF', '#FF8552', '#FF4365', '#39A0ED', '#CCCCCC')
         )

# BDA POSITIVA
bda_positiva <- read.xlsx('data/Reporte_BDA_Asomovil.xlsx', 1)

bda_positiva <- bda_positiva %>% 
                  gather("Año", "Número total de IMEI",  `2011`:`2019`)

bda_positiva <- bda_positiva %>%
                  drop_na(`Número total de IMEI`)
bda_positiva$Fecha <- paste0(bda_positiva$fecha, '-', bda_positiva$Año)
bda_positiva <- bda_positiva[,c(4,3)]

opts <- list( order = unique(bda_positiva$Fecha),
              agg_text = " ",
              spline =  T,
              horLabel = " ",
              theme = tma(custom = c(tm_l, showText = F)))
h1 <- hgch_line_CatNum(bda_positiva, opts = opts) %>%
  hc_yAxis( 
    title = list(
  style = list(
    fontSize = '18px'
  )
))

saveWidget(h1, 'uno.html')
# BDA NEGATIVA
bda_negativa <- read.xlsx('data/Reporte_BDA_Asomovil.xlsx', 2)
bda_negativa <- bda_negativa %>% separate("fecha", c("dia", "mes", "Año"), sep = "-")


bda_ngt <- bda_negativa %>% 
                 select(-dia, -mes) %>% 
                 gather("Tipo Reporte", "Número total de IMEI", -Año)
bda_ngt <- bda_ngt %>% 
             select(`Tipo Reporte`, everything()) %>% 
                drop_na(`Número total de IMEI`)
bda_ngt$`Tipo Reporte` <- gsub('\\.', ' ', bda_ngt$`Tipo Reporte`)

opts <- list( order = unique(bda_ngt$Fecha),
              agg_text = " ",
              spline =  T,
              horLabel = " ",
              theme = tma(custom = c(tm_l, showText = F)))

h2 <- hgch_line_CatCatNum(bda_ngt, opts = opts) %>%
  hc_yAxis( 
    title = list(
      style = list(
        fontSize = '18px'
      )
    ))%>%
  hc_xAxis( 
    labels = list(
      style = list(
        fontSize = '15px'
      )
    ))%>% 
  hc_legend(
    title = list(
      text = 'Puedes hacer click en una categoría para ocultarla',
      style = list(
        color = '#758988'
      )
    )
  )
saveWidget(h2, 'dos.html')
bda_ngt <- bda_ngt %>% 
             group_by(Año, `Tipo Reporte`) %>% 
               summarise(Total = sum(`Número total de IMEI`))
bda_ngt$Total <- as.character(bda_ngt$Total)
h3 <- datatable(bda_ngt,
          rownames = F,
          options = list(scrollX = TRUE,
                         scrollY = TRUE,
                         pageLength = 7, 
                         language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'),
                         lengthChange = F,
                         initComplete = JS(
                           "function(settings, json) {",
                           "$(this.api().table().header()).css({'background-color': '#213133', 'color': '#fff', 'font-family': 'Open Sans', 'text-align': 'center !important'});",
                           "}"),
                         searching = FALSE
          ))%>% 
  formatStyle( 0 , target= 'row',color = '#213133', fontSize ='15px', lineHeight='19px', background = '#fff', fontFamily = 'Open Sans', textAlign = 'center') %>% 
  formatStyle(c(1:dim(bda_ngt)[2]), border = '1px solid #213133', textAlign = 'center')
saveWidget(h3, 'tabla_1.html')
# BDA NEG SOLO ROBOS
bda_robos <- bda_negativa %>% unite("Fecha", mes:Año, sep = "-")
bda_robos <- bda_robos %>% 
              select(Fecha, `Denuncias por hurto` = Robo) %>% 
                drop_na(`Denuncias por hurto`)
opts <- list( order = unique(bda_robos$Fecha),
              agg_text = " ",
              spline =  T,
              horLabel = " ",
              theme = tma(custom = c(tm_l, showText = F)))
h4 <- hgch_line_CatNum(bda_robos, opts = opts)%>%
  hc_yAxis( 
    title = list(
      style = list(
        fontSize = '18px'
      )
    ))

saveWidget(h4, 'tres.html')
# CELULARES

data_2003_2010 <- map_df(c('2003', '2004', '2005', '2006', '2007', '2008', '2009', '2010'), function(i){
  d <- read.xlsx('data/Hurto celular 2003-2010.xlsx', i)
dt <- d %>% summarise(Total = n())
dt$Año <- i
dt
})
data_2011_2014 <- map_df(c( '2011', '2012', '2013', '2014'), function(i){
  d <- read.xlsx('data/Hurto Celulares 2011-2014.xlsx', i)
  dt <- d %>% summarise(Total = n())
  dt$Año <- i
  dt
})


data_2015_2016 <- map_df(c( '2015', '2016'), function(i){
  d <- read.xlsx('data/Hurto Celulares 2015-2016.xlsx', i)
  dt <- d %>% summarise(Total = n())
  dt$Año <- i
  dt
})

data_2017 <- read.xlsx('data/Hurto celulares 2017 .xlsx')
data_2017 <- data_2017 %>% summarise(Total = n())
data_2017$Año <-  '2017'

df <- bind_rows(data_2003_2010, data_2011_2014, data_2015_2016, data_2017)
df <- df %>% select(Año,  `Total denuncias de celulares robados` = Total)
opts <- list( #order = unique(bda_ngt$Fecha),
              agg_text = " ",
              spline =  T,
              horLabel = " ",
              theme = tma(custom = c(tm_l, showText = F)))
h5 <- hgch_area_CatNum(df, opts = opts) %>%
  hc_yAxis( 
    title = list(
      style = list(
        fontSize = '18px'
      )
    ))%>%
  hc_xAxis( 
    labels = list(
      style = list(
        fontSize = '15px'
      )
    ))

saveWidget(h5, 'cinco.html')
# comparacación
bda_robos <- bda_robos %>% 
               separate(Fecha, c('mes', 'Año'), sep = '-')
bda_robos <- bda_robos %>% group_by(Año) %>% summarise('BDA' = sum(`Denuncias por hurto`))

dt <- df %>% left_join(bda_robos)
names(dt) <- c('Año', 'Policia Nacional', 'Base de datos administrativa')
dt <- dt %>% drop_na(`Base de datos administrativa`)
dt <- dt %>% gather('Fuente', 'Total denuncias', -Año)
dt <- dt %>% select(Fuente, everything())
opts <- list( #order = unique(bda_ngt$Fecha),
  agg_text = " ",
  spline =  T,
  horLabel = " ",
  theme = tma(custom = c(tm_l, showText = F)))
h6 <- hgch_line_CatCatNum(dt, opts = opts) %>%
  hc_yAxis( 
    title = list(
      style = list(
        fontSize = '18px'
      )
    ))%>%
  hc_xAxis( 
    labels = list(
      style = list(
        fontSize = '15px'
      )
    ))
saveWidget(h6, 'seis.html')


dt_es <- data.frame('Año' = as.character(2013:2017), 
                    'Subreporte' = c("96.3%", "96.6%", "95.2%", "95.1%", "90.4%")
                   )


h7 <- datatable(dt_es,
                rownames = F,
                options = list( dom = 't',
                               pageLength = 5, 
                               language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'),
                               lengthChange = F,
                               initComplete = JS(
                                 "function(settings, json) {",
                                 "$(this.api().table().header()).css({'background-color': '#213133', 'color': '#fff', 'font-family': 'Open Sans', 'text-align': 'center !important'});",
                                 "}"),
                               searching = FALSE
                ))%>% 
  formatStyle( 0 , target= 'row', textAlign = 'center',color = '#213133', fontSize ='15px', lineHeight='19px', background = '#fff', fontFamily = 'Open Sans') %>% 
  formatStyle(c(1:dim(dt_es)[2]), border = '1px solid #213133', textAlign = 'center')

saveWidget(h7, 'tabla_2.html')
