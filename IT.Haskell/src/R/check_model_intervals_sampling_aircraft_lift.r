check_model_intervals_sampling_aircraft_lift <- function(model_dir, model_file, x) {
  
  # change directory to the directory containing the models
  setwd(model_dir);
  
  # identified models and their partial derivatives
  source(model_file);
  
  # f: the expressions to check
  # direction: 1 for comparison f(x) < threshold
  #           -1 for comparison f(x) > threshold
  # threshold: the value to compare to
  constraints <- c(lift_model,          -1, 0,  "lift > 0",
                   lift_model,           1, 20, "lift < 20",
                   lift_partial_C_La,   -1, 0,  "d lift / d C_La > 0",
                   lift_partial_a,      -1, 0,  "d lift / d a > 0",
                   lift_partial_C_Ld_e, -1, 0,  "d lift / d C_Ld_e > 0",
                   lift_partial_d_e,    -1, 0,  "d lift / d d_e > 0",
                   lift_partial_S_HT,   -1, 0,  "d lift / d S_HT > 0",
                   lift_partial_S_ref,   1, 0,  "d lift / d S_ref < 0"
  );
  
  constraintCols <- 4;
  
  # evaluate all expressions, the model and its partials 
  for(i in 1:(length(constraints) %/% constraintCols)) {
    violations <- 0;
    baseIdx <- (i-1)*constraintCols;
    f  <- constraints[[baseIdx+1]];
    d  <- constraints[[baseIdx+2]];
    th <- constraints[[baseIdx+3]];
    expr <- constraints[[baseIdx+4]];
    print(paste("Check constraint #",i,expr));
    m <- f(x); # eval function
    m[is.na(m)] <- Inf; # undefined outputs also violate constraints
    for(modelIdx in 1:ncol(m)) {
      if(max(d*m[,modelIdx]) > th) {
        violations <- violations + 1;
        warning(paste("Model",modelIdx,"violates constraint", "range:", min(m[,modelIdx]),max(m[,modelIdx])), immediate.=TRUE)
        print(paste("Model",modelIdx,"violates constraint", "range:", min(m[,modelIdx]),max(m[,modelIdx])))
      } else 
        print(paste("Model",modelIdx, " min:", min(m[,modelIdx])," max:", max(m[,modelIdx])))
    }
    print(paste("Violations: ", violations));
    print('~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~');
  }
}