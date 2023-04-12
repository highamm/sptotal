# Summary output does not change

    Code
      summary(slmobj)
    Output
      
      Call:
      counts ~ pred1 + pred2
      
      Residuals:
          Min      1Q  Median      3Q     Max 
      -16.086  -9.120  -4.596   4.963  28.594 
      
      Coefficients:
                  Estimate Std. Error t value Pr(>|t|)    
      (Intercept)   26.104      4.465   5.847   <2e-16 ***
      pred1          2.055      6.065   0.339    0.737    
      pred2          0.214      1.913   0.112    0.912    
      ---
      Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
      
      Covariance Parameters:
                   Exponential Model
      Nugget            2.323211e-04
      Partial Sill      1.543624e+02
      Range             8.240060e-01
      
      Generalized R-squared: 0.00413886 

