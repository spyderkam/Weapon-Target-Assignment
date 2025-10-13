function [p,q] = calc_rice_cdf_asymp(v, sig, b)

% Evaluate cdf of Rice distribution, with offset v, at x = b
% Compare to exact value

% Rescale inputs wrt sigma
b = b/sig;
v = v/sig;
z = b*v;

% Compute pdf and cdf of Gaussian (easier to just use a lookup table!)
norm_coeff = sqrt(1/(2*pi));
pdf_norm = norm_coeff*exp(-0.5*(b - v)^2);
cdf_norm = norm_coeff*integral(@(x) exp(-0.5*(x - v).^2), -inf, b, 'ArrayValued', true);

% Asymptotic calculation
p = cdf_norm - pdf_norm/(2*sqrt(z))*(1 + 1/(8*z));

% Exact calculation
q = integral(@(x) x.*exp(-(x.^2 + v^2)/2).*besseli(0, v*x), 0, b);
end
