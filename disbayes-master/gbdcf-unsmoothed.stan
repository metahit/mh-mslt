functions {

    // Annual transition probabilities between states, in terms of intensities
    // From discrete-time solution to the Kolmogorov forward equation given in DISMOD2 paper 
    // 1: healthy;  2: disease;  3: dead from disease
    // i: incidence, f: case fatality, r: remission 
    // TODO handle i=f.
    
    matrix trans_probs(real i, real f, real r)  {
	real l = i + r + f;
	real q = sqrt(i*i + 2*i*r -  2*i*f  + r*r + 2*f*r + f*f);
	real w = exp(-(l + q) / 2);
	real v = exp(-(l - q) / 2);
	matrix[3,3] P;
	P[1,1] = (2*(v-w)*(f+r) + v*(q-l) + w*(q+l)) / (2*q);
	P[2,1] = 2*(v-w)*r/(2*q);
	P[3,1] = 0; 

	P[1,2] = -2*(f + r - l)*(v-w)/(2*q);
	P[2,2] = -((2*(f+r) - l)*(v-w) - q*(v+w)) / (2*q);
	P[3,2] = 0;

	P[1,3] = (-l*(v-w) - q*(v+w))/(2*q) + 1;
	P[2,3] = ((v-w)*(2*f - l) - q*(v+w))/(2*q) + 1;
	P[3,3] = 1;
	return P;
    }

}

// Declare the data 
data {
    int<lower=0> nage;
    int<lower=0> pop[nage];
    int<lower=0> ndieddis[nage];
    int<lower=0> prevdenom[nage];
    int<lower=0> prevn[nage];
    vector<lower=0>[nage] inc;
    vector<lower=0>[nage] rem; // remission rates assumed to be constant 0 here
}

// Declare the unknown parameters
parameters {
    vector<lower=0>[nage] cf;
}

// Functions of parameters 
transformed parameters {
    //// range constraints on these cause problems due to floating point fuzz
    matrix[nage+1,3] state_probs; 
    row_vector[3] tmp;
    matrix[3,3] P;
    real prev[nage];
    real p_die_disease[nage];
    
    state_probs[1,1] = 1;
    state_probs[1,2] = 0;
    state_probs[1,3] = 0;

    for (a in 1:nage){ 
	P = trans_probs(inc[a], cf[a], rem[a]);
	// Calculate state occupancy probabilities at each age by multiplying by the transition probabilities
	tmp = state_probs[a,1:3] * P;  // temp variable to avoid warning
	state_probs[a+1,1:3] = tmp;
	// Deduce the prevalence and disease-specific mortality 
	prev[a] = state_probs[a,2] / (state_probs[a,1] + state_probs[a,2]);
	p_die_disease[a] = P[1,3]*(1 - prev[a]) + P[2,3]*prev[a];
	//// work around floating point fuzz
	if (p_die_disease[a] < 0) p_die_disease[a] = 0;
	if (p_die_disease[a] > 1) p_die_disease[a] = 1;
    }
}

model {
    // Prior for case fatality 
    for (a in 1:nage){
	cf[a] ~ exponential(1); // gamma(0.1, 10); // Why does it not converge with the gamma prior?// BZ: changed to expoential
    }

    // Statistical model for number of people dying from IHD at each age
    ndieddis ~ binomial(pop, p_die_disease);

    // Statistical model for prevalence of IHD
    // Doesn't make much difference whether prevalence data are included or not
    // They're very similar to prevalences forward-calculated from incidence anyway
    prevn ~ binomial(prevdenom, prev);
}