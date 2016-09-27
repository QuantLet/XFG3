
function[mh] = regxest(x,h,K)
    [n,m] = size(x);
    if m ~= 2
        disp('regxest: data matrix must contain 2 columns');
        disp('Please input the data matrix again');
        dat = input('x=');
    end
    eh = exist('h');
    if (eh == 0)
        %h=(max(x(:,1))-min(x(:,1)))/5;
        h = 2.42*std(x(:,1))*n^(-0.2);
    end
    eK = exist('K');
    if (eK == 0)
        K=1;        %quartic kernel
    else
        if K == 0 || K >4
            disp('Type of the kernel must be "1", "2", "3" or "4"');
            disp('The quartic kernel will be used');
            K = 1;
        end  
    end
    tmp = sortrows(x);
    x   = tmp(:,1);
    y   = tmp(:,2);
    %if (exist(v)==0)
        %v = x;
    %else
        %v = sort(v);
    %endif
    %require nw (Nadaraya-Watson) function / toolbox
    for i = 1:n  
        s(i,1)= nw(x(i),x,y,h,K)./nw(x(i),x,ones(n,1),h,K);
    end
    %for i = 1:n  % Gaussian kernel
        %s(i,1)= nw(x(i),x,y,5*h,K)./nw(x(i),x,ones(n,1),5*h,K);
    %end
    mh=[x s];