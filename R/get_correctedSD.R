### Get stDev to calculate corrected RR
GetStDevRR <- function(RR, LB, UB){
  # RR=2.82
  # LB=2.35
  # UB=3.38
  SE=exp(((UB-LB)/3.92))
  # stDevRR=exp(sqrt(exp((2*log(RR) + 2*((UB-LB)/3.92)*(log(RR))^2)) - exp((2*log(RR) + ((UB-LB)/3.92)*(log(RR))^2))))
  return(SE) ### Belen to check calculation 
}                   

