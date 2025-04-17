// define endogenous variables
var y, c, w, l, k, i, r;

// define exogenous variable
varexo z;

// define parameters
parameters alpha, beta, delta, psi, rho, epsilon;
alpha = 0.33;
beta = 0.99;
delta = 0.025;
psi = 1.75;
rho = 0.95;
epsilon = 10;

//define model
model;
1/c = beta * ((1 / c(+1)) * (1 + r(+1) - delta));
psi * (c / (1 - l)) = w;
c + i = y;
y = (k(-1)^alpha) * ((exp(z) * l)^(1 - alpha));
w = y * ((epsilon - 1) / epsilon) * ((1 - alpha) / l);
r = y * ((epsilon - 1) / epsilon) * (alpha / k(-1));
i = k - (1 - delta) * k(-1);
end;

//establish initial values for steady state calc
initval;
z = 0;
k = 10;
l = 0.3;
y = k^alpha * (l)^(1 - alpha);
w = y * ((epsilon - 1) / epsilon) * ((1 - alpha)/l);
r = y * ((epsilon - 1) / epsilon) * alpha / k;
i = delta * k;
c = y - i;
end;

steady;

check;

//implement schock in periods 1-5
shocks;
var z;
periods 1:5;
values 0.1;
end;

perfect_foresight_setup(periods=100);
perfect_foresight_solver;
