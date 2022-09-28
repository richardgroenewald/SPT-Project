tic
format longg
%helper function
subindex = @(A, r, c) A(r,c);

grid = readmatrix('C:/Users/richa/Downloads/Project/grid2.csv');
p = readmatrix('C:/Users/richa/Downloads/p_mtx.csv');
q = readmatrix('C:/Users/richa/Downloads/q_mtx.csv');

p = p(:, 1:100);
q = q(:, 1:100);

%number of points to evaluate function, obtain intervals
d = length(grid);
int_lengths = diff(grid);

%parameters
beta = 1000000;
alpha = 100;

%number of classes in partition
J = 3;

%categories - 3 of equal size for this toy example (need to correspond with the categories in the data cleaning file, in the non ranked based case)
r = [0 33 66 100];
cellcutoffs = cell(J);
for j = 1:J
    cellcutoffs{j} = (r(j)+1):r(j+1);
end

dims = size(p);
%number of time periods
T = dims(2);
%number of stocks (= number of ranks)
n = dims(1);

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
                dphi((j-1)*d + i) + (phi(i+1))^2 <= 0;

                %constraint (3.11) - alpha Lipschitz second derivatives approximation
                dphi((j-1)*d + i+1)*int_lengths(i+1) - dphi((j-1)*d + i)*int_lengths(i) <= alpha*(int_lengths(i+1)+int_lengths(i))/2;
                dphi((j-1)*d + i+1)*int_lengths(i+1) - dphi((j-1)*d + i)*int_lengths(i) >= -alpha*(int_lengths(i+1)+int_lengths(i))/2;
            end
            %only did to d-2 for (3.9) in above loop
            dphi((j-1)*d + d-1) + (phi(d))^2 <= 0;

            %constraint (3.10) - boundedness of derivatives
            phi((j-1)*d + d) >= 0;
            phi((j-1)*d + 1) <= beta;

        end
cvx_end

toc