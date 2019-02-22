## Change Log
## Jay
21 Feb 2019
I added three new functions:
  1) slmfit_jay.R
  2) estcovparm_jay.R
  3) m2LL_jay
These make slmfit much faster.  On my computer, using the new functions takes 
about 1 second for a sample size of 300, whereas the old code takes about 
40 seconds. I used a profiled likelihood to optimize over 2 covariance
parameters rather than 3, and used the QR decompostion, qr(), rather than 
solve(). The QR decomposition allows quick inverses, solves, and determinants
from a single stable set of matrix operations.
22 Feb 2019
I added two new functions:
  1) AIC.slmfit.R
  2) logLik.slmfit.R
These are generics for other linear model functions such as lm(), etc.
