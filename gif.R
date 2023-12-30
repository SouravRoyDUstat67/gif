library(ggplot2)
library(gapminder)
library(gganimate)
library(gifski)

gapminder
names(gapminder)

p=ggplot(gapminder, aes(gdpPercap, lifeExp, size= pop, col=country))+
  geom_point(alpha=.7)+
  scale_color_manual(values = country_colors)+
  scale_size(range = c(2, 15))+
  scale_x_log10()+
  facet_wrap(~continent)+
  theme(legend.position = "none")+
  theme(axis.text = element_text(size = 10),
        axis.title = element_text(size = 10, face = "bold"),
        strip.text = element_text(size = 10))+
  transition_time(year)+
  theme(plot.title = element_text(size = 30, face="bold"))+
  labs(title = 'Year:{frame_time}', x="GDP per capital", y="Life expentency")

animate(p, nframes=20, render= gifski_renderer("percapita_gdp.gif"),
        height= 700, width= 1000)
