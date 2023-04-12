# Stratification summary output does not change

    Code
      summary(stratamod)
    Output
      $A
      
      Call:
      counts ~ pred1
      
      Residuals:
         Min     1Q Median     3Q    Max 
      -6.647 -5.768 -1.715  4.578  9.552 
      
      Coefficients:
                  Estimate Std. Error t value Pr(>|t|)  
      (Intercept)   21.974      5.537   3.969   0.0286 *
      pred1         -2.554     11.072  -0.231   0.8324  
      ---
      Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
      
      Covariance Parameters:
                   Exponential Model
      Nugget             63.63648285
      Partial Sill        0.55829824
      Range               0.02203231
      
      Generalized R-squared: 0.01742682 
      
      $B
      
      Call:
      counts ~ pred1
      
      Residuals:
         Min     1Q Median     3Q    Max 
      -8.290 -3.753  0.127  3.153  8.983 
      
      Coefficients:
                  Estimate Std. Error t value Pr(>|t|)    
      (Intercept)   20.956      2.946   7.114    1e-05 ***
      pred1         -3.068      4.799  -0.639    0.534    
      ---
      Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
      
      Covariance Parameters:
                   Exponential Model
      Nugget             0.006356148
      Partial Sill      24.816378626
      Range              0.557269210
      
      Generalized R-squared: 0.03049108 
      
      $C
      
      Call:
      counts ~ pred1
      
      Residuals:
          Min      1Q  Median      3Q     Max 
      -9.4279 -4.4230  0.3513  2.8862 13.7064 
      
      Coefficients:
                  Estimate Std. Error t value Pr(>|t|)    
      (Intercept)   19.968      2.623   7.611   <2e-16 ***
      pred1         -2.322      4.968  -0.467    0.648    
      ---
      Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
      
      Covariance Parameters:
                   Exponential Model
      Nugget            3.888537e+01
      Partial Sill      4.187629e-13
      Range             4.621396e+03
      
      Generalized R-squared: 0.0165178 
      

