max <- 1
min <- -1
size_pic <- 256
num_iterates <- 1000

roots <- matrix(nrow=3,ncol=2)
roots[1,] <- c(cos(0), sin(0))
roots[2,] <- c(cos(2*pi/3), sin(2*pi/3))
roots[3,] <- c(cos(4*pi/3), sin(4*pi/3))

roots_text <- c('1', 'sqrt(3)/2+i/2', 'sqrt(3)/2-i/2')
colors <- c('red', 'green', 'blue')

newton_function <- function(z) {
  c((2/3)*z[1]+(1/3)*((z[1]^2-z[2]^2)/((z[1]^2-z[2]^2)^2+4*z[1]^2*z[2]^2)),
    (2/3)*z[2]-(1/3)*((2*z[1]*z[2])/((z[1]^2-z[2]^2)^2+4*z[1]^2*z[2]^2)))
}

distance_function <- function(theta1, theta2) {
  sqrt((theta1[1]-theta2[1])^2+(theta1[2]-theta2[2])^2)
}

color_by_min_distance <- function(z) {
  distances <- c(distance_function(z, roots[1,]),
                 distance_function(z, roots[2,]),
                 distance_function(z, roots[3,]))
  colors[which(distances == min(distances))]
}

a <- seq(min, max, (max-min)/size_pic)
b <- seq(min, max, (max-min)/size_pic)

plot(0,0, xlim=c(min,max), ylim=c(min,max), xlab=NA, ylab=NA, main='z^3-1')

for (i in a) {
  for (j in b) {
    z <- c(i,j)
    k = 1
    while((z[1]!=0 || z[2]!=0) && k<=num_iterates) {
      z <- newton_function(z)
      k <- k+1
    }
    l <- color_by_min_distance(z)
    if (z[1]!=0 || z[2]!=0) {
      points(i, j, col=l)
    }
  }
}