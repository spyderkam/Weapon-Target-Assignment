function main
F = fopen("./riceTable.csv", "w");
rice_cdf_table(F);
fclose(F);
end

function p = calc_rice_cdf_asymp(v, sig, b)
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
end

function rice_cdf_table(f)

% Create lookup table for smaller offsets (relative to sigma)
% Note: v is the offset from the center, b is the fuel budget (budget is for divert)
v_vals = linspace(2, 150, 150);
b_vals = linspace(48.6846, 150, 150);
T = zeros(200);

counter = 1;
fprintf("working...")
for i = 2:150
    fprintf(".");
    for j = 2:150
        fprintf(".");
        T(i,j) = calc_rice_cdf_asymp(v_vals(i), 10, b_vals(j));
        fprintf(f, T(i,j) + ",");
        if mod (counter, 149) == 0
            fprintf(f, "\n");
        end;
        if mod (counter, 15000) == 0
            fprintf("\x1b[2J");
        end;
        counter = counter + 1;
    end
end

save('./rice_lookup_table.csv', '-ASCII');
end
