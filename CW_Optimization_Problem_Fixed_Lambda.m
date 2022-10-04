tic

format longg

grid = readmatrix('C:/Users/richa/Downloads/Project/grid.csv');
p = readmatrix('C:/Users/richa/Downloads/p_mtx.csv');
q = readmatrix('C:/Users/richa/Downloads/q_mtx.csv');

%number of points to evaluate function, obtain intervals
d = length(grid);
int_lengths = diff(grid);
beta = 1000000;

%number of rank based classes
J = 1;

%categories - 3 of equal size for this toy example
r = [0 100];
cellcutoffs = cell(J);
for j = 1:J
    cellcutoffs{j} = (r(j)+1):r(j+1);
end

dims = size(p);
%number of time periods
T = dims(2);
%T = 1;
%number of stocks (= number of ranks)
n = dims(1);
%index of the zero value for the functions l_j
zero_ind = find(0.5 == grid,1);

%checking with print statements
%fprintf('n is %d\n ', n);
%fprintf('T is %d\n ', T);

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
                dl((j-1)*d + i+1)/int_lengths(i+1) - dl((j-1)*d + i)/int_lengths(i) >= -beta*(int_lengths(i+1)+int_lengths(i))/2;
            end

            %constraint (3.3) - to ensure compactness of optimization region
            dl((j-1)*d + 1) <= sqrt(beta)*int_lengths(1);
            dl((j-1)*d + d-1) >= -sqrt(beta)*int_lengths(d-1);

            %constraint (3.4) - enforcing a function value of 0 at x = 1/2
            l((j-1)*d + zero_ind) == 0;
        end
cvx_end

toc