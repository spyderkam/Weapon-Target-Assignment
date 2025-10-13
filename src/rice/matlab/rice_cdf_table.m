function rice_cdf_table()

% Create lookup table for smaller offsets and fuel budgets (relative to sigma)

v_vals = linspace(0, 4, 200);
b_vals = v_vals;
T = zeros(200);

for i = 1:200
    for j = 1:200
        [~, T(i,j)] = calc_rice_cdf_asymp(v_vals(i), 1, b_vals(j));
    end
end

table_name = 'rice_lookup';
save([table_name '.mat'], 'T');
writetable(table(T), ([table_name '.csv']));
