library(GA)

# function optimisation in one dimension
f <- function(x)  (x^2+x)*cos(x)
lbound <- -10; ubound <- 10
curve(f, from = lbound, to = ubound, n = 1000)


GA <- ga(type = "real-valued", fitness = f, lower = c(th = lbound), upper = ubound)
summary(GA)
plot(GA)
curve(f, from = lbound, to = ubound, n = 1000)
points(GA@solution, GA@fitnessValue, col = 2, pch = 19)
# found maximum


# function optomisation in two dimensions
# rastrigin function
Rastrigin <- function(x1, x2)
{
  20 + x1^2 + x2^2 - 10*(cos(2*pi*x1) + cos(2*pi*x2))
}

x1 <- x2 <- seq(-5.12, 5.12, by = 0.1)
f <- outer(x1, x2, Rastrigin)
persp3D(x1, x2, f, theta = 50, phi = 20, col.palette = bl2gr.colors)
filled.contour(x1, x2, f, color.palette = bl2gr.colors)
GA <- ga(type = "real-valued", 
         fitness =  function(x) -Rastrigin(x[1], x[2]),
         lower = c(-5.12, -5.12), upper = c(5.12, 5.12), 
         popSize = 50, maxiter = 1000, run = 100)
summary(GA)
plot(GA)
filled.contour(x1, x2, f, color.palette = bl2gr.colors, 
               plot.axes = { axis(1); axis(2); 
                 points(GA@solution[,1], GA@solution[,2], 
                        pch = 3, cex = 2, col = "white", lwd = 2) }
)
monitor <- function(obj) 
{ 
  contour(x1, x2, f, drawlabels = FALSE, col = grey(0.5))
  title(paste("iteration =", obj@iter), font.main = 1)
  points(obj@population, pch = 20, col = 2)
  Sys.sleep(0.2)
}

GA <- ga(type = "real-valued", 
         fitness =  function(x) -Rastrigin(x[1], x[2]),
         lower = c(-5.12, -5.12), upper = c(5.12, 5.12), 
         popSize = 50, maxiter = 100, 
         monitor = monitor)
 #show process for finind the minimum




# using parallel computing
library(parallel)
library(doParallel)

library(GA)
fitness <- function(x)
{
  Sys.sleep(0.01)
  x*runif(1)
}

library(rbenchmark)
out <- benchmark(
  GA1 = ga(type = "real-valued", 
           fitness = fitness, lower = 0, upper = 1,
           popSize = 50, maxiter = 100, monitor = FALSE,
           seed = 12345),
  GA2 = ga(type = "real-valued", 
           fitness = fitness, lower = 0, upper = 1,
           popSize = 50, maxiter = 100, monitor = FALSE,
           seed = 12345, parallel = TRUE),
  GA3 = ga(type = "real-valued", 
           fitness = fitness, lower = 0, upper = 1,
           popSize = 50, maxiter = 100, monitor = FALSE,
           seed = 12345, parallel = 2),
  GA4 = ga(type = "real-valued", 
           fitness = fitness, lower = 0, upper = 1,
           popSize = 50, maxiter = 100, monitor = FALSE,
           seed = 12345, parallel = "snow"),
  columns = c("test", "replications", "elapsed", "relative"),
  order = "test", 
  replications = 10)
out$average <- with(out, average <- elapsed/replications)
out[,c(1:3,5,4)]