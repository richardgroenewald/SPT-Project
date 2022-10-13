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

%end fitting period at December 31st, 2003 instead:
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

%parameter
beta = 1000000;

%number of rank based classes
J = 3;

%categories - 3 of equal size for this toy example
r = [0 33 66 100];
cellcutoffs = cell(J);
for j = 1:J
    cellcutoffs{j} = (r(j)+1):r(j+1);
end

%index of the zero value for the functions l_j
zero_ind = find(0.5 == grid,1);

category = 1:n;
%vector to add to indices below
for i = 1:n
    for j = 1:J
        if ismember(i, cellcutoffs{j}) == 1
            category(i) = j;
        end
    end
end
adj = d*(category' - 1);
%disp(adj);

%one step forward difference of market capitalizations for market weights at time t
delta_pq = q - p;

%indices for function evaluation - need to account for excess terms in dl
indices = arrayfun(@(x) find(x < grid,1) - 1, p);
dl_indices = indices + adj;

%initialize lambda, assigned for each category
lambda = repmat(1/n, J, 1);

cvx_solver mosek;

%optimization problem for exponentially concave functions l
cvx_begin
    variable l(J*d)
    expression dlogv(T);
    expression dl(J*d - 1)
    dl = l(2:(d*J)) - l(1:(d*J-1));

    %fix lambda, assigned for each index
    lambdas = lambda(category);
    %single category case - lambda = 1/n
    if J == 1
        lambdas = lambda(category)';
    end

    %build objective function - multiplying lambdas by function derivatives by delta_pq and summing by column
    obj = sum(log(1 + sum(delta_pq .* lambdas .* dl(dl_indices) ./ int_lengths(indices), 1)));

    maximize(obj)
    subject to
        for j = 1:J
            for i = 1:(d-2)
                %constraint (3.1) - exponential concavity approximation
                w = int_lengths(i)/(int_lengths(i+1) + int_lengths(i));
                -l((j-1)*d + i+1) + log (w*exp(l((j-1)*d + i+2)) + (1-w)*exp(l((j-1)*d + i)) ) <= 0;

                %constraint (3.2) - beta Lipschitz derivatives approximation
                dl((j-1)*d + i+1)/int_lengths(i+1) - dl((j-1)*d + i)/int_lengths(i) + beta*(int_lengths(i+1)+int_lengths(i))/2 >= 0;
            end

            %constraint (3.3) - to ensure compactness of optimization region
            dl((j-1)*d + 1) <= sqrt(beta)*int_lengths(1);
            dl((j-1)*d + d-1) >= -sqrt(beta)*int_lengths(d-1);

            %constraint (3.4) - enforcing a function value of 0 at x = 1/2
            l((j-1)*d + zero_ind) == 0;
        end
cvx_end

%test results and plot
delta_pq = q_test - p_test;
indices = arrayfun(@(x) find(x < grid,1) - 1, p_test);
dl_indices = indices + adj;
lambda = repmat(1/n, J, 1);
z = log(1 + sum(delta_pq .* lambdas .* dl(dl_indices) ./ int_lengths(indices), 1));
test_cw_wealth = sum(z);

plot(cumsum(z));

writematrix(l, "ell.csv");

toc