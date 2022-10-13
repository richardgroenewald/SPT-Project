tic

format longg

%load data
grid = readmatrix('C:/Users/richa/Downloads/Project/1.3 - grid.csv');
p_total = readmatrix('C:/Users/richa/Downloads/p_mtx.csv', 'NumHeaderLines', 1);
q_total = readmatrix('C:/Users/richa/Downloads/q_mtx.csv', 'NumHeaderLines', 1);

%compute dimensions
dims = size(p_total);
%time period to use during fit, prop = proportion of data used to train
prop = 0.75;
start = 1;
finish = dims(2);
T = floor((finish-start)*prop);

%end fitting period at December 31st, 2002 instead:
T = 11580;

%number of stocks (= number of ranks)
n = dims(1);

%split off training set
p = p_total(:, start:(start+T));
q = q_total(:, start:(start+T));
p_test = p_total(:, (start+T+1):finish);
q_test = q_total(:, (start+T+1):finish);

%number of points to evaluate function, obtain intervals
d = length(grid);
int_lengths = diff(grid);

%parameters
beta = 1000000;
alpha = 100;

%number of classes in partition
J = 3;

%categories - 4 of equal size for this toy example (need to correspond with the categories in the data cleaning file, in the non ranked based case)
r = [0 33 66 100];
cellcutoffs = cell(J);
for j = 1:J
    cellcutoffs{j} = (r(j)+1):r(j+1);
end

%construct vector of categories corresponding to stocks
category = 1:n;
for i = 1:n
    for j = 1:J
        if ismember(i, cellcutoffs{j}) == 1
            category(i) = j;
        end
    end
end

%one step forward difference of market capitalizations for market weights at time t
delta_pq = q - p;

%indices for function evaluation - need to account for excess terms in dl
indices = arrayfun(@(x) find(x < grid,1) - 1, p);
%need to adjust for optimizing J different functions in a single vector in optimization problem below
adj = d*(category' - 1);
phi_indices = indices + adj;

%fix lambda, assigned for each category
lambda = repmat(1/n, J, 1);

cvx_solver mosek;

%optimization problem
cvx_begin
    variable phi(J*d)
    expression dlogv(T);
    expression dphi(J*d - 1)
    dphi = phi(2:(d*J)) - phi(1:(d*J-1));

    %fix lambda, assigned for each index
    lambdas = lambda(category);
    if J == 1
        lambdas = lambda(category)';
    end

    %build objective function - multiplying lambdas by function derivatives by delta_pq and summing by column
    dlogv = log(1 + sum(delta_pq .* lambdas .*  (phi(phi_indices) + (dphi(phi_indices) ./ int_lengths(indices)).*(p - grid(indices))), 1));
    obj = sum(dlogv);

    maximize(obj)
    subject to
        for j = 1:J
            for i = 1:(d-2)
                %constraint (3.9) - exponential concavity approximation
                dphi((j-1)*d + i) + (phi((j-1)*d + i+1))^2 <= 0;

                %constraint (3.11) - alpha Lipschitz second derivatives approximation
                %dphi((j-1)*d + i+1)*int_lengths(i+1) - dphi((j-1)*d + i)*int_lengths(i) <= alpha*(int_lengths(i+1)+int_lengths(i))/2;
                %dphi((j-1)*d + i+1)*int_lengths(i+1) - dphi((j-1)*d + i)*int_lengths(i) >= -alpha*(int_lengths(i+1)+int_lengths(i))/2;
            end
            %only did to d-2 for (3.9) in above loop
            dphi((j-1)*d + d-1) + (phi((j-1)*d + d))^2 <= 0;

            %constraint (3.10) - boundedness of derivatives
            phi((j-1)*d + d) >= 0;
            phi((j-1)*d + 1) <= beta;

        end
cvx_end

%test results
delta_pq = q_test - p_test;
indices = arrayfun(@(x) find(x < grid,1) - 1, p_test);
phi_indices = indices + adj;
lambda = repmat(1/n, J, 1);
z = log(1 + sum(delta_pq .* lambdas .*  (phi(phi_indices) + (dphi(phi_indices) ./ int_lengths(indices)).*(p_test - grid(indices))), 1));
test_my_wealth = sum(z);

plot(cumsum(z));

writematrix(phi, "phi.csv");

toc