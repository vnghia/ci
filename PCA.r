library(ggplot2)
library(dplyr)
library(gganimate)
library(tidyr)
library(magick)

set.seed(42)

X <- matrix(rnorm(200), 100)
X <- X %*% chol(matrix(c(1, 0.6, 0.6, 0.6), 2))
X <- X - mean(X)

X.eig = eigen(cov(X))
a = X.eig$vectors

X.df = data.frame(X)
colnames(X.df) <- c("x", "y")

X.plot <- X.df %>% ggplot() + 
  geom_point(aes(x=x, y=y), color="#0561f5", size=2) + 
  coord_fixed() +
  xlim(-3, 3) +
  ylim(-3, 3) +
  theme(axis.line=element_line(),axis.text.x=element_blank(),
          axis.text.y=element_blank(),axis.ticks=element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),legend.position="none",panel.border=element_blank(),panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),plot.background = element_rect(fill = "transparent", color = NA), panel.background = element_rect(fill = "transparent"),
          axis.line.y = element_line(arrow = grid::arrow(length = unit(0.3, "cm"))), axis.line.x = element_line(arrow = grid::arrow(length = unit(0.3, "cm"))))
ggsave(X.plot, filename = "plot.svg",  bg = "#F8F8FF")

X.df.rotate <-  data.frame(c(0:359) * pi / 180) %>%  crossing(X.df)
colnames(X.df.rotate) <- c("angle", "x", "y")

X.plot.rotate <- X.df.rotate %>% ggplot() + 
  geom_point(aes(x=x, y=y), color="#0561f5",  size=2) + 
  coord_fixed() +
  xlim(-3, 3) +
  ylim(-3, 3) +
  theme(axis.line=element_line(),axis.text.x=element_blank(),
          axis.text.y=element_blank(),axis.ticks=element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),legend.position="none",panel.border=element_blank(),panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),plot.background = element_rect(fill = "#F8F8FF", color = NA), panel.background = element_rect(fill = "transparent"),
          axis.line.y = element_line(arrow = grid::arrow(length = unit(0.3, "cm"))), axis.line.x = element_line(arrow = grid::arrow(length = unit(0.3, "cm")))) +
          transition_states(angle, transition_length = 0.5, state_length = 0.5) +
geom_abline(aes(intercept=0, slope=tan(angle)), size=0.5)

anim_save("plot.gif", X.plot.rotate, renderer=magick_renderer(), nframes=360, fps=50, device = "svg")

# for(x in 0:359) {
#   alpha <- 2*x*pi/360
#   p <- X.plot + geom_abline(slope=tan(alpha))
#   ggsave(p, filename=paste(".local/", x, ".png", sep=""), bg = "white")
# }
