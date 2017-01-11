function decoded_text = hw8(cipher)
    %% Load data 
    % Make sure that `cipher.mat` is in the working directory.
    load('cipher.mat');

    [last_r, opt_pi] = run_MH(cipher, pi_0, T, 100);
    decoded_text = decode(cipher,opt_pi);
end

%% Compute Log-likelihood
function ll = compute_log_likelihood(cipher, pi, pi_0, T)
    eng_char =   {'a', 'b', 'c', 'd','e','f','g','h','i','j','k','l','m','n',...
        'o','p','q','r','s','t','u','v','w','x','y','z','.',' '};
    eng_num = [1:28];
    char_to_num = containers.Map(eng_char,eng_num);
    target = decode(cipher,pi);
    number = zeros(1,length(cipher));
    for i = 1:length(cipher)
        number(i) = char_to_num(target(i));
    end

    ll = pi_0(number(1));
    for j = 1:(length(number)-1)
        ll = ll+T(number(j),number(j+1));
    end 
end

%% Compute Initial Permutation
function pi = compute_initial_permutation(cipher, pi_0)
    target = cipher;
    reference = 'abcdefghijklmnopqrstuvwxyz. ';
    [~, ord_english] = sort(pi_0, 'descend');
    % Count the number of occurrences for each of 28 characters
    counts = zeros(1,length(pi_0));
    for i=1:length(reference)
        counts(i) = length(strfind(target,reference(i)));
    end
    [~, ord_counts] = sort(counts, 'descend');

    % Compute the initial permutation using the ordering of frequencies
    char_map = [ord_english transpose(ord_counts)];
    char_map_sort = sortrows(char_map,1);
    pi = transpose(char_map_sort(:,2));
end

%% Propose A Local Move
function proposal = propose(pi)
    target = pi;
    index = randi([1 length(target)],1,2);
    while index(1) == index(2)
        index  = randi([1 length(target)],1,2);
    end
    temp = target(index(1));
    target(index(1)) = target(index(2));
    target(index(2)) = temp;
    proposal = target;
end

%% Run MCMC
function [r, pi] = run_MH(cipher, pi_0, T, n_steps)
    init_permu = compute_initial_permutation(cipher,pi_0);
    best_permu = zeros(1,length(init_permu));
    best_loglike = 0;
    for k = 1:n_steps
        display(k)
        propose_permu = propose(init_permu);
        old_loglike = compute_log_likelihood(cipher, init_permu, pi_0, T);
        new_loglike = compute_log_likelihood(cipher, propose_permu, pi_0, T);
        if old_loglike >= best_loglike
            best_permu = init_permu;
            best_loglike = old_loglike;
        end
        if new_loglike >= best_loglike
            best_permu = propose_permu;
            best_loglike = new_loglike;
        end
        r = exp(new_loglike-old_loglike);
        if r >= 1
            init_permu = propose_permu;
        else
            if rand < r
                init_permu = propose_permu;
            end 
        end
    end
    pi = best_permu;

end

%% Decoder
function decoded_text = decode(cipher, pi)
    reference = 'abcdefghijklmnopqrstuvwxyz. ';
    result = char(zeros(1,length(cipher)));
    for i=1:length(cipher)
       result(i) = reference(pi(reference==cipher(i)));
    end
    decoded_text = result;
end
