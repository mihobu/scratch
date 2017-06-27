require(ggplot2)

fake_inf = rep(900, 14)

breaks_y = c(1.8, 2, 2.4, 2.8, 3.3, 4, 4.8, 5.6, 6.7, 8, 9.5, 11, 13, 16)
labels_y = c('f/1.8', 'f/2', 'f/2.4', 'f/2.8', 'f/3.3',
             'f/4', 'f/4.8', 'f/5.6', 'f/6.7', 'f/8',
             'f/9.5', 'f/11', 'f/13', 'f/16')

hd = c(665.45, 592.88, 498.60, 419.31, 352.64,
       296.58, 249.44, 209.80, 176.46, 148.43,
       124.86, 105.04, 88.37, 74.35)
hn = c(332.73, 296.44, 249.30, 209.66, 176.32,
       148.29, 124.72, 104.90, 88.23, 74.21,
       62.43, 52.52, 44.18, 37.18)

#y1 = c(1.8, 2, 2.4, 2.8, 3.3, 4, 4.8, 5.6, 6.7, 8, 9.5, 11, 13, 16)
#y2 = c(2, 2.4, 2.8, 3.3, 4, 4.8, 5.6, 6.7, 8, 9.5, 11, 13, 16, 19)

y1 = seq(1, 14)
y2 = seq(2, 15)

df1 = data.frame(x1=hd, y1=y1, x2=fake_inf, y2=y2)
df2 = data.frame(x1=hn, y1=y1, x2=hd, y2=y2)

ggplot() +
  geom_rect(data=df1, mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2), color=NA, fill="blue", alpha=0.25) +
  geom_rect(data=df2, mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2), color=NA, fill="blue", alpha=0.5) +
  #scale_x_continuous(name='distance (feet)',breaks=c(seq(100,701,100),900), labels=c(seq(100,701,100), 'INF'), minor_breaks=NULL, limits=c(-50,900)) +
  scale_x_continuous(name='DISTANCE (FEET)',breaks=NULL, labels=NULL, minor_breaks=NULL, limits=c(-50,900), expand=c(0,0)) +
  scale_y_continuous(name='', breaks=y1+0.5, labels=labels_y, minor_breaks=NULL, expand=c(0,0)) +
  #ggtitle('Hyperfocal distance chart: APS-C/85mm') +
  geom_text(data=df1, mapping=aes(x=x1+10, y=y1+0.5, label=x1), hjust=0, size=5.5, fontface='bold') +
  geom_text(data=df2, mapping=aes(x=x1-10, y=y1+0.5, label=x1), hjust=1, size=5.5, fontface='bold') +
  theme(
    axis.text=element_text(size=12, face='bold', color='black'),
    axis.title=element_text(size=12, face='bold'),
    panel.background=element_rect(fill=NA, colour=NA),
    axis.ticks=element_blank(),
    panel.grid.major.x=element_line(colour='black')
  ) +
  ggtitle('85mm')
  

