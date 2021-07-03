rm(list = ls()) ;  gc(); cat("\014")

# fix seed
set.seed(1)

# initialize
n       <- 100
alpha.y <- 0
alpha.m <- 0
theta.y <- 1
theta.m <- 1
beta    <- 1
true.indirect.effect <- theta.m * beta

# initialize contamination
R             <- 100 # number of repetitions
contam.levels <- 0:20
x.contam.arr  <- rnorm(max(contam.levels), mean = -8, sd = 1)

# initialize arrays for performance measures
coverage.arr <- array(NA_real_, dim = c(R, length(contam.levels), 2), 
                      dimnames = list(1:R, paste0("n.contam.", contam.levels), c("OLS", "MM")))
power.arr <- coverage.arr

for(r in 1:R){
  
  # sample variables
  x     <- rnorm(n, mean = 0, sd = 1)
  eps.y <- rnorm(n, mean = 0, sd = 1)
  eps.m <- rnorm(n, mean = 0, sd = 1)
  
  # data generating process
  m <- alpha.m + theta.m * x + eps.m
  y <- alpha.y + theta.y * x + beta * m + eps.y


  ## fit model on contaminated data 
  # contaminate data in x-dimension (goal: classical test shall make type 2 error)
  
  for(i in contam.levels){
    
    if(i == 0){
      x.contam <- x
    } else{
      x.contam      <- x
      x.contam[1:i] <- x.contam.arr[1:i]
    } # IF
    
    # non-robust fit
    mod.ols_contam <- robmed::fit_mediation(data.frame(y = y, m = m, x = x.contam),
                                            x = "x", y = "y", m = "m", 
                                            method = "regression", 
                                            robust = FALSE)
    
    test.ols.boot_contam  <- robmed::test_mediation(mod.ols_contam, test = "boot", R = 2000)

    # robust fit
    mod.mm_contam <- robmed::fit_mediation(data.frame(y = y, m = m, x = x.contam),
                                           x = "x", y = "y", m = "m", 
                                           method = "regression", 
                                           robust = TRUE)
    
    test.mm.boot_contam  <- robmed::test_mediation(mod.mm_contam, test = "boot", R = 2000)
    
    # evaluate performance measures
    coverage.arr[r, i+1, "OLS"] <- 
      test.ols.boot_contam$ci[1] < true.indirect.effect & true.indirect.effect < test.ols.boot_contam$ci[2]
    
    coverage.arr[r, i+1, "MM"] <- 
      test.mm.boot_contam$ci[1] < true.indirect.effect & true.indirect.effect < test.mm.boot_contam$ci[2]
    
    power.arr[r, i+1, "OLS"] <- 
      !(test.ols.boot_contam$ci[1] < 0 & 0 < test.ols.boot_contam$ci[2]) 
    
    power.arr[r, i+1, "MM"] <- 
      !(test.mm.boot_contam$ci[1] < 0 & 0 < test.mm.boot_contam$ci[2])
    
  } # FOR contam.levels
} # FOR r

# save results
type2.list <- list(coverage.arr = coverage.arr, power.arr = power.arr)
save(type2.list, file = paste0(getwd(), "/type2-error/simulation_type2.Rdata"))
