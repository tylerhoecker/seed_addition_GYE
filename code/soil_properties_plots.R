soil_properties = read_csv('data/soil_properties.csv')

# One method, works, kinda ugly
library(plotrix)
data(soils)
soil.texture(main="NO DATA")
soil.texture(soils, main="DEFAULT", pch=2)
soil.texture(soils, main="LINES AND NAMES", show.lines=TRUE,
             show.names=TRUE, pch=3)
soiltex.return<-soil.texture(soils[1:6,], main="GRID AND LEGEND",
                             show.grid=TRUE, pch=4, col.symbols=1:6, show.legend=TRUE)
par(soiltex.return$oldpar)



soil.texture(soil_properties[2:4]/100, main="DEFAULT", pch=2)
soil.texture(soil_properties[2:4], main="LINES AND NAMES", show.lines=TRUE,
             show.names=TRUE, pch=3)
soiltex.return<-soil.texture(soil_properties[2:4], 
                             show.grid=TRUE, 
                             pch=19, 
                             #col.symbols=as.character(''), 
                             show.legend=TRUE, 
                             point.labels = seq(1,12), label.points = T)
text(x, y, labels=1:12, font=2)


# Load the required libraries
# ggplot method, less ugly
library(ggtern)
library(ggrepel)

# Load the Data. (Available in ggtern 1.0.3.0 next version)
data(USDA)

# Put tile labels at the midpoint of each tile.
USDA.LAB = plyr::ddply(USDA, 'Label', function(df) {
  apply(df[, 1:3], 2, mean)
})

# Tweak
USDA.LAB$Angle = 0
USDA.LAB$Angle[which(USDA.LAB$Label == 'Loamy Sand')] = -35
USDA.LAB$Clay[which(USDA.LAB$Label == 'Loam')] = 0.19


# Construct the plot.
# NOTE aes(color=Label,fill=Label) in 3rd line below
ggplot(data = USDA, aes(y=Clay, x=Sand, z=Silt)) +
  coord_tern(L="x",T="y",R="z") +
  geom_polygon(alpha = 0.75, size = 0.5, color = 'black',aes(color=Label,fill=Label), show.legend = F) +
  geom_text(data = USDA.LAB,
            aes(label = Label, angle = Angle),
            color = 'black',
            size = 3.5) +
  #theme_rgbw() +
  #theme_showsecondary() +
  theme_showarrows() +
  custom_percent("Percent") +
  #geom_point(data = soil_properties, size=3) 
  geom_text(data = soil_properties, aes(label = seq(1:12)), fontface = 'bold' ) 
#  geom_text(data = soil_properties, aes(label=site, x=Inf, y=y_values, hjust=0)) 


