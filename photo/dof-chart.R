require(ggplot2)
require(reshape2)
require(RColorBrewer)

# --------------------------------------------------------
# THIS SCRIPT GENERATES A DEPTH OF FIELD PLOT
# (X-AXIS = SUBJECT DISTANCE IN FEET, Y-AXIS = DEPTH OF FIELD IN FEET)
# FOR A SINGLE FOCAL LENGTH AT APERTURE VALUES OF YOUR CHOOSING

# --------------------------------------------------------
# CONFIGURE CHART HERE:
c <- 0.020 # circle of confusion (0.020 for Nikon APS-C cameras)

# Which f-stops do you want to plot?
# Hack alert: use '12' for the 1/3-stop f/13 (AV=7.333)
#             use '13' for the 1/2-stop f/13 (AV=7.5)

# My 85mm lens
f <- 85
myfstops <- c(1.8, 2, 2.8, 4, 5.6, 8, 11, 16) # for 85mm
s.min <- 10 # minimum subject distance in feet
s.max <- 50 # maximum subject distsance in feet
dof.min <- 0
dof.max <- 30

# My 24mm lens
#f <- 24
#myfstops <- c(2.8, 4, 5.6, 8, 11, 16, 22) # for 24mm
#s.min <- 0 # minimum subject distance in feet
#s.max <- 10 # maximum subject distsance in feet
#dof.min <- 0
#dof.max <- 20

# --------------------------------------------------------
# CALCULATE LIST OF TRUE F-NUMBERS
av <- c()
for ( i in seq(1,11) ) {
  av <- c(av, i, i+1/3, i+0.5, i+2/3)
}
av <- c(av,12)
fstop.true <- sapply(av, function(x) {return(2^(x/2))})

# These are the common/standard f-numbers:
fstop.label <- c(
  '1.4', '1.6', '1.7', '1.8',
  '2',   '2.2', '2.4', '2.5',
  '2.8', '3.2', '3.3', '3.5',
  '4',   '4.5', '4.8', '5',
  '5.6', '6.3', '6.7', '7.1',
  '8',   '9',   '9.5', '10',
  '11',  '12',  '13',  '14',
  '16',  '18',  '19',  '20',
  '22',  '25',  '27',  '29',
  '32',  '36',  '38',  '40',
  '45',  '51',  '54',  '57',
  '64'
)
aperture <- data.frame(cbind(fstop.true), row.names=fstop.label)

# --------------------------------------------------------
# params: f = focal length (mm)
#         c = circle of confusion (mm)
#         N = f/Number
# --------------------------------------------------------
hyperfocal.distance <- function(f, c, N) {
  # N = f/number
  return(f + f^2 / (N*c))
}

# --------------------------------------------------------
# --------------------------------------------------------
feet.to.mm <- function(ft) {
  return(ft / 0.00328084)
}

# --------------------------------------------------------
# --------------------------------------------------------
mm.to.feet <- function(mm) {
  return(mm * 0.00328084)
}

# --------------------------------------------------------
# params: f = focal length (mm)
#         c = circle of confusion (mm)
#         N = f/Number
#         s = subject distance (mm)
# --------------------------------------------------------
dof.nearlimit <- function(f, c, N, s) {
  h <- hyperfocal.distance(f, c, N)
  return( (h * s) / (h + s) )
}

# --------------------------------------------------------
# params: f = focal length (mm)
#         c = circle of confusion (mm)
#         N = f/Number
#         s = subject distance (mm)
# --------------------------------------------------------
dof.farlimit <- function(f, c, N, s) {
  h <- hyperfocal.distance(f, c, N)
  return( (h * s) / (h - s) )
}

# --------------------------------------------------------
# params: f = focal length (mm)
#         c = circle of confusion (mm)
#         N = f/Number
#         s = subject distance (mm)
# --------------------------------------------------------
dof <- function(f, c, N, s) {
  if ( s > hyperfocal.distance(f, c, N) ) {
    retval = NA
  }
  else {
    dfar <- dof.farlimit(f, c, N, s)
    dnear <- dof.nearlimit(f, c, N, s)
    retval <- dfar - dnear    
  }
  return(retval)
}

# --------------------------------------------------------
# Wrap the dof function using the globally assigned values
# for f, c, and N, as well as handling the mm/ft conversions
# --------------------------------------------------------
dof.wrapper <- function(x) {
  return(mm.to.feet(dof(f, c, N, feet.to.mm(x))))
}

# --------------------------------------------------------
# Wrap the hyperfocal distance using the globally assigned values
# for f and c, as well as handling the mm/ft conversions
# --------------------------------------------------------
hd.wrapper <- function(x) {
  return(mm.to.feet(hyperfocal.distance(f, c, x)))
}

# --------------------------------------------------------
# --------------------------------------------------------
nl.wrapper <- function(r) {
  return(
    mm.to.feet(dof.nearlimit(f, c, aperture[toString(r[1]),], feet.to.mm(r[2])))
  )
}

# --------------------------------------------------------
# Here's where all the work happens
# --------------------------------------------------------
linesize <- 2
textsize <- 8

# Build data frame for plotting
chart.x <- seq(s.min, s.max, length.out=1000)
df.plot <- data.frame(cbind(chart.x))
for ( myN in myfstops ) {
  N <- unlist(aperture[toString(myN),])
  d <- sapply(chart.x, function(x) {dof.wrapper(x)})
  column.names <- c(colnames(df.plot), paste('f/', myN, sep=''))
  df.plot <- data.frame(cbind(df.plot, d))
  colnames(df.plot) <- column.names
}

# Reshape the data for easier plotting
df.melt <- melt(df.plot, id.vars='chart.x')

ggplot(df.melt, aes(chart.x, value, color=variable)) +
  geom_line(size=linesize) +
  scale_x_continuous(name='SUBJECT DISTANCE (feet)', limits=c(s.min,s.max), breaks=seq(s.min,s.max,10), expand=c(0,0)) +
  scale_y_continuous(name='DEPTH OF FIELD (FEET)', limits=c(dof.min,dof.max), breaks=seq(dof.min,dof.max,10), expand=c(0,0)) +
  theme(
    axis.text=element_text(size=textsize*2, face='bold', color='black'),
    axis.title=element_text(size=textsize*2, face='bold'),
    panel.grid.major=element_line(colour='white', size=1),
    panel.grid.minor=element_line(colour='white', size=0.5),
    axis.ticks=element_blank(),
    panel.background=element_rect(fill='#D0D0D0', colour=NA),
    plot.margin = unit(c(0.125,0.125,0.125,0.125), "in")
  ) +
  scale_fill_brewer(palette="Spectral") +
  ggtitle(paste(f, 'mm', sep=''))


# Get the hyperfocal distances at each of my preferred f-stops

# 85mm
f <- 85
myfstops <- c(1.8, 2, 2.4, 2.8, 3.3, 4, 4.8, 5.6, 6.7, 8, 9.5, 11, 13, 16) # for 85mm

# 24mm
f <- 24
myfstops <- c(2.8, 3.3, 4, 4.8, 5.6, 6.7, 8, 9.5, 11, 13, 16, 22)

df.hd <- data.frame(cbind(myfstops))
colnames(df.hd) <- c('N')
hd <- sapply(myfstops, hd.wrapper)
df.hd <- data.frame(cbind(df.hd, hd))
nl <- apply(df.hd, 1, nl.wrapper)
df.hd <- data.frame(cbind(df.hd, nl))

df2.melt <- melt(df.hd, id.vars=c('hd', 'nl'))

# Add lo and hi columns (for plotting our rectangles)
df.hd <- data.frame(cbind(df.hd, seq(1,length(nl))))
df.hd <- data.frame(cbind(df.hd, seq(2,1+length(nl))))
colnames(df.hd) <- c('N', 'hd', 'nl', 'lo', 'hi')

getPalette <- colorRampPalette(brewer.pal(11, "Spectral"))

ggplot(df.hd, aes(fill=N)) +
  geom_rect(aes(xmin=nl, xmax=hd, ymin=lo, ymax=hi), fill=getPalette(length(nl))) +
  geom_text(aes(x=nl-3, y=lo+0.5, label=round(nl,0)), hjust=1, size=8) +
  geom_text(aes(x=hd+3, y=lo+0.5, label=round(hd,0)), hjust=0, size=8, fontface='bold') +
  geom_text(aes(x=-10, y=lo+0.5, label=N), hjust=0.5, size=8, fontface='italic') +
  ggtitle(paste('NEAR LIMIT / HYPERFOCAL DISTANCE FOR', f, 'mm', 'lens', sep=' ')) +
  guides(fill=FALSE) +
  scale_x_continuous(name='SUBJECT DISTANCE (FEET)') +
  scale_y_continuous(name='F-NUMBER') +
  theme(
    axis.ticks=element_blank(),
    panel.background=element_rect(fill='#D0D0D0', colour=NA),
    plot.margin = unit(c(0.125,0.250,0.125,0.125), "in")
  )

