rm(list=ls());

#setwd(".");
source("hl_functions.R");
source("check_model_intervals_sampling_aircraft_lift.r");
#source("check_model_intervals_sampling_F05128_dyn.r");
#source("check_model_intervals_sampling_F05128_stat.r");
#source("check_model_intervals_sampling_flow_psi.r");
#source("check_model_intervals_sampling_flow_stress.r");
#source("check_model_intervals_sampling_rocket_fuel.r");

# set repo dir containing the models
modelDirs <- c(
  "."
);

N <- 1000000;

  # extrapolation range definitions for all problem instances
  #C_Lα in [0.3 .. 0.9]
  #α in [2 .. 12] 
  #C_Lδ_e in [0.3 .. 0.9]
  #δ_e in [0 .. 12]
  #S_HT in [0.5 .. 2]
  #S_ref in [3 .. 10]
  aircraft_lift_data_extrapolation <- data.frame(
    C_La = runif(N, 0.3, 0.9),
    a = runif(N, 2, 12),
    C_Ld_e = runif(N, 0.3, 0.9),
    d_e = runif(N, 0, 12),
    S_HT = runif(N, 0.5, 2),
    S_ref = runif(N, 3, 10)
    );
	
  #T in     [250 .. 600]
  #φ in     [0 .. 1]
  #φ_dot in [0.001 .. 10] 
  flow_stress_data_extrapolation <- data.frame(
    T = runif(N, 250, 600),
    phi = runif(N, 0, 1), 
    phi_dot = runif(N, 0.001, 10)
    );

  # V_inf in [30 .. 100]
  # theta in [10 .. 90]   
  # Gamma in [2 .. 15]    
  # R in [0.1 .. 0.5] 
  # r in [0.5 .. 1.5] 
  flow_psi_data_extrapolation <- data.frame(
    V_inf = runif(N, 30, 100), 
    theta = runif(N, 10, 90), 
    Gamma = runif(N, 2, 15), 
    R = runif(N, 0.1, 0.5), 
    r = runif(N, 0.5, 1.5)
    );
  
  #p in [0.1 .. 15]
  #v in [0.01 .. 3]   // only for μ_dyn
  #T in [-50 .. 250]
  F05128_static_data_extrapolation <- data.frame(
    p = runif(N, 0.1, 15),
    v = runif(N, 0.07, 0.5), # same as data distribution for mu_stat
    T = runif(N, -50, 250)
    );
  
  #p in [0.1 .. 15]
  #v in [0.01 .. 3]   // only for μ_dyn
  #T in [-50 .. 250]
  F05128_dynamic_data_extrapolation <- data.frame(
    p = runif(N, 0.1, 15),
    v = runif(N, 0.01, 3), 
    T = runif(N, -50, 250)
    );

  #p0 in [300000 .. 700000]
  #A in [0.2 .. 2] 
  #T0 in [200 .. 400]
  rocket_fuel_data_extrapolation <- data.frame(
    p0 = runif(N, 300000, 700000),
    A = runif(N, 0.2, 2),
    T0 = runif(N, 200, 400)
    );
  
for(modelDir in modelDirs) {
  
  sink(paste(modelDir,"check_model_intervals_result.txt", sep="/")); # send output to file
  
  
  check_model_intervals_sampling_aircraft_lift(modelDir, "aircraft_lift.R", aircraft_lift_data_extrapolation)
  #check_model_intervals_sampling_aircraft_lift(modelDir, "aircraft_lift_noisy.R", aircraft_lift_data_extrapolation)
  #check_model_intervals_sampling_F05128_dyn(modelDir, "F05128_dynamic.R", F05128_dynamic_data_extrapolation)
  #check_model_intervals_sampling_F05128_stat(modelDir, "F05128_static.R", F05128_static_data_extrapolation)
  #check_model_intervals_sampling_flow_psi(modelDir, "flow_psi.R", flow_psi_data_extrapolation)
  #check_model_intervals_sampling_flow_psi(modelDir, "flow_psi_noisy.R", flow_psi_data_extrapolation)
  #check_model_intervals_sampling_flow_stress(modelDir, "flow_stress.R", flow_stress_data_extrapolation)
  #check_model_intervals_sampling_rocket_fuel(modelDir, "rocket_fuel.R", rocket_fuel_data_extrapolation)
  #check_model_intervals_sampling_rocket_fuel(modelDir, "rocket_fuel_noisy.R", rocket_fuel_data_extrapolation)
  sink()
}

