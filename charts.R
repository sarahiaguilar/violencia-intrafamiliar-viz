library(data.table)
library(ggplot2)
library("mxmaps")
library(lubridate)

##### Chart 1
df <- fread("./datos/prevalencia_total_por_estado.csv") 

data("df_mxstate")

df_map <- merge(df_mxstate, df, by.x = "state_name_official", by.y = "Estado", all.x = TRUE)
setnames(df_map, old = "Prevalencia_total", new = "value") 

mxhexbin_choropleth(df_map, num_colors = 1) +
  scale_fill_gradient(
    low = "#faeaedff",
    high = "#912236ff",
    guide = "colourbar"
  )

##### Chart 2
df <- fread("./datos/prevalencia_por_tipo.csv") 

ggplot(df, aes(x = reorder(Tipo, -Prevalencia), y = Prevalencia, label = scales::percent(Prevalencia))) + 
  geom_bar(stat = "identity", width = 0.5, fill = "#912236ff") + 
  geom_text(color = "white",  size = 4, position = position_stack(vjust = .55)) +
  scale_y_continuous(labels = scales::percent) +  
  labs(x = "Tipo de violencia", y = "Prevalencia") +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14)) 


##### Chart 3
df <- fread("./datos/prevalencia_de_agresor_por_tipo.csv") 

ggplot(df, aes(x = Prevalencia, y = reorder(Agresor, Prevalencia))) +
  geom_bar(stat = "identity", width = 0.5, fill = "#912236ff") +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 0.4)) +
  labs(x = "Prevalencia", y = "Agresor") +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14)) +
  facet_grid(~Tipo)



##### Chart 4
df <- fread("./datos/prevalencia_razon_no_denuncia.csv") 

ggplot(df, aes(x = reorder(Razon, Prevalencia), y = Prevalencia, label = scales::percent(Prevalencia, accuracy = 1.1))) + 
  geom_bar(stat = "identity", width = 0.5, fill = "#912236ff") + 
  geom_text(color = "white",  size = 4, position = position_stack(vjust = 0.50)) +
  scale_y_continuous(labels = scales::percent) +  
  labs(x = "Razón", y = "Prevalencia") +
  coord_flip() + 
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14)) 


##### Chart 5
df <- fread("./datos/violencia_familiar_carpetas_investigacion.csv")

df$fecha <- mdy(df$fecha)

df <- df[df$fecha < "2020-04-30"]

ggplot(df, aes(x = fecha, y = viol_fam_nac)) +
  geom_line(color = "#912236ff", size = 1.3) +
  geom_area(fill = "#912236ff", alpha=0.4) +
  scale_x_date(labels = scales::date_format("%b %y"), breaks = scales::date_breaks("2 months")) + 
  scale_y_continuous(labels = scales::comma, limits = c(0, 25000)) + 
  labs(x = "Fecha", y = "Carpetas de investigación por violencia familiar") 








