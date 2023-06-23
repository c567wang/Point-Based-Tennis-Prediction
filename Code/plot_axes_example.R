marks <- function(a,m,f){
  return (20*m + f*(20/3*(1-m)+40)+c(4,6,6,9,15)%*%a)
}

m <- rep(seq(0.5,0.9,length.out=50),50)
f <- rep(seq(0.5,0.9,length.out=50),each=50)
a <- c(0.967,0.891,1,0.9,0.9)
z <- rep(0,2500)
for(i in 1:2500){
  z[i] <- marks(a,m[i],f[i])
}

axes <- list(
  range = c(0.5,0.9)
)

library(plotly)
fig <- plot_ly(x=~m,y=~f,z=~z,type='mesh3d')
fig <- fig %>% layout(scene = list(xaxis=axes,yaxis=axes))
fig

# new
m <- 0.59223
w <- rep(0,2500)
for (i in 1:2500){
  w[i] <- marks(a,m,f[i])
}
plot(f,w,'l')
