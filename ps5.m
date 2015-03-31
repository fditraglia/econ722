% ECON722 PS5 Q3(d) replication of Table I and II in Hurvich and Tsai (1993)
% Qiusha Peng
% March 26, 2015

clear all;

%% VAR(1)

% Initialization

Sigma1 = [1,0;0,1];
Sigma12 = chol(Sigma1);
Phi1 = [-1,.96;-1.5,1.4]';

% Simulation

N = 40;
Nsimu = 1000;
epsi1 = randn(N,2,Nsimu);

Y = zeros(N,2,Nsimu);
Y(1,:,:) = permute(permute(epsi1(1,:,:),[3 2 1])*Sigma12,[3 2 1]);

for i = 2:N
    Y(i,:,:) = permute(permute(Y(i-1,:,:),[3 2 1])*Phi1,[3 2 1])...
                + permute(permute(epsi1(i,:,:),[3 2 1])*Sigma12,[3 2 1]);
end

% Model Selection

P = 6;
AIC1 = zeros(P,Nsimu);
BIC1 = zeros(P,Nsimu);
AICC1 = zeros(P,Nsimu);

for p = 1:P
    T = N-p;
    Y1 = Y(p+1:end,:,:); 
    X1 = zeros(T,2*p,Nsimu);
    for i = 1:p
        X1(:,2*(i-1)+1:2*i,:) = Y(p+1-i:end-i,:,:);
    end
    for j = 1:Nsimu
        hat_Phi = inv(X1(:,:,j)'*X1(:,:,j))*(X1(:,:,j)'*Y1(:,:,j));
        hat_Sigma = 1/T*(Y1(:,:,j)-X1(:,:,j)*hat_Phi)'*(Y1(:,:,j)-X1(:,:,j)*hat_Phi);
        AIC1(p,j) = log(det(hat_Sigma))+(2*p*(2^2)+2*(2+1))/T;
        BIC1(p,j) = log(det(hat_Sigma))+(log(T)*(p*(2^2)+2*(2+1)/2))/T;
        AICC1(p,j) = log(det(hat_Sigma))+(T+2*p)*2/(T-2*p-2-1);
    end
end

[fval,I1] = min(AIC1);
[fval,I2] = min(BIC1);
[fval,I3] = min(AICC1);

Table = zeros(3,P);
for p = 1:P
    Table(1,p) = sum(I1==p);
    Table(2,p) = sum(I2==p);
    Table(3,p) = sum(I3==p);
end

%% VAR(2)

% Initialization

Sigma2 = [1,-.08;-.08,1];
Sigma22 = chol(Sigma1);
Phi21 = [.5,-.3;.2,.65]';
Phi22 = [-.5,.3;0,-.4]';

% Simulation

N = 40;
Nsimu = 1000;
epsi1 = randn(N+1,2,Nsimu);

Y = zeros(N,2,Nsimu);
Y(1,:,:) = permute(permute(epsi1(1,:,:),[3 2 1])*Sigma22,[3 2 1]);
Y(2,:,:) = permute(permute(Y(1,:,:),[3 2 1])*Phi22,[3 2 1])...
            + permute(permute(epsi1(2,:,:),[3 2 1])*Sigma22,[3 2 1]);

for i = 3:N
    Y(i,:,:) = permute(permute(Y(i-1,:,:),[3 2 1])*Phi21,[3 2 1])...
                + permute(permute(Y(i-2,:,:),[3 2 1])*Phi22,[3 2 1])...
                + permute(permute(epsi1(i,:,:),[3 2 1])*Sigma22,[3 2 1]);
end

% Model Selection

P = 6;
AIC2 = zeros(P,Nsimu);
BIC2 = zeros(P,Nsimu);
AICC2 = zeros(P,Nsimu);

for p = 1:P
    T = N-p;
    Y2 = Y(p+1:end,:,:); 
    X2 = zeros(T,2*p,Nsimu);
    for i = 1:p
        X2(:,2*(i-1)+1:2*i,:) = Y(p+1-i:end-i,:,:);
    end
    for j = 1:Nsimu
        hat_Phi = inv(X2(:,:,j)'*X2(:,:,j))*(X2(:,:,j)'*Y2(:,:,j));
        hat_Sigma = 1/T*(Y2(:,:,j)-X2(:,:,j)*hat_Phi)'*(Y2(:,:,j)-X2(:,:,j)*hat_Phi);
        AIC2(p,j) = log(det(hat_Sigma))+(2*p*(2^2)+2*(2+1))/T;
        BIC2(p,j) = log(det(hat_Sigma))+(log(T)*(p*(2^2)+2*(2+1)/2))/T;
        AICC2(p,j) = log(det(hat_Sigma))+(T+2*p)*2/(T-2*p-2-1);
    end
end

[fval,I1] = min(AIC2);
[fval,I2] = min(BIC2);
[fval,I3] = min(AICC2);

Table2 = zeros(3,P);
for p = 1:P
    Table2(1,p) = sum(I1==p);
    Table2(2,p) = sum(I2==p);
    Table2(3,p) = sum(I3==p);
end