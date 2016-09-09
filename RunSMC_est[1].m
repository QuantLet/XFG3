% % Parameter estimation by the density-tempered expanding-data
% sequential Monte Carlo
% Paper: Estimating Distance-to-Default with a Sector-Specific
% Liability Adjustment via Sequential Monte Carlo (Draft: August 12, 2016)

clear all
randn('state',1);
rand('state',1);
p = parpool(4); % open parralete computing: Maximun 4 workers in Matlab R2013b
addpath('Subfunction')

SimuFlag = 0; % 0: real data/ 1: simulation data
yearI    = 2014;   % [2009 2014]
SecCode  = 20018; % [20018 20082 20051 20055]
 
load(['Input\Y' num2str(yearI) 'G' num2str(SecCode,'%06d') '_c.mat'])
Fname_org  = fieldnames(YearData);
Nfiled_org = length(Fname_org);

% The random selected 40 firms in paper
if SecCode ==2 0051 ||  SecCode == 20055
   load(['Input\RandFirm_' num2str(SecCode) '.mat'],'FirmCode')
   [Irand]  = ismember(YearData.FirmCode,FirmCode);
   for Iname=1:Nfiled_org
       YearData.(Fname_org{Iname})=YearData.(Fname_org{Iname})(:,Irand);
   end
end

% Check data with Missing and sorting correspondingly
SunMiss = sum(isnan(YearData.Equity));
[Mseq, Miss_sort] = sort(SunMiss);
% delete firms with 250 time series missing data
NaNInd     = find(Mseq==250);
StoreDelete= YearData.FirmCode(1,Miss_sort(NaNInd));
for Iname=1:Nfiled_org
    Y.(Fname_org{Iname}) = YearData.(Fname_org{Iname})(:,Miss_sort);
    Y.(Fname_org{Iname})(:,NaNInd) = [];
end
[Ydays, NtotFirm] = size( Y.Equity);

SectorN = size( Y.Equity,2); 
% SMCsettingbyFirm: Initilization parameters setting 
[ ModelSetting,int_ComParam, int_Param, smcsettings ] = SMCsettingbyFirm( SectorN );

% Nrep: # of simulations --for Excel output
Nrep   = 1;
ElapsT = zeros(1,Nrep,'double');
OE     = 2;
n0     = 0;

% Step1. Initialization sampling: Common variables 
for i=smcsettings.Com_Idx
theta.(smcsettings.Fname{i}) = InitSim_theta(smcsettings.Nsim, ...
    int_ComParam.m, int_ComParam.s.^(1/2), int_ComParam.dist, int_ComParam.bound);
%     density of initialization sampler for common parmameter
    [ lnpdf_int(:,1)]        = oglikelihood_density( theta.(smcsettings.Fname{i}),...
    int_ComParam.m, diag(int_ComParam.s),1,0);
end

Int_mean = zeros(ModelSetting.Ni); 
Int_var  = zeros(ModelSetting.Ni); 
Nf       = 0;

for CountG = 1:smcsettings.Gnum

    begT = tic; 
    
    % add Firms from Nf to Ns
    Nf   = smcsettings.MoveBlocks(CountG+1).moveindex(1)-1; % # of firms Finish SMC 
    Ns   = smcsettings.MoveBlocks(CountG+1).moveindex(end);
    disp(['add company to ' num2str( Ns )])
    Y_exp = ExtractData( Y, 1, Ns, ModelSetting.N );
    
    % Step1. Initialization sampling: Individual variables  
    if CountG == 1
        
        Int_data = zeros(smcsettings.Nsim, ModelSetting.Ni, Ns);
        %  only sample first Group partameters  
        for Gi = smcsettings.MoveBlocks(CountG+1).moveindex
            Int_data(:,:,Gi)     = InitSim_theta(smcsettings.Nsim, int_Param(Gi).m' ,...
                (int_Param(Gi).s).^(1/2)', int_Param(Gi).dist, int_Param(Gi).bound );
            % log likelihood of initialization sampler 
            [ lnpdf_int(:,Gi+1)] = loglikelihood_density(...
                Int_data(:,:,Gi),int_Param(Gi).m', diag(int_Param(Gi).s), 1,1);
        end
        %   Initial theta enviroment
        for i=1:ModelSetting.Ni
            Iname                = smcsettings.Ind_Idx(i);
            theta.(smcsettings.Fname{Iname}) = ...
                squeeze(Int_data(:,i,smcsettings.MoveBlocks(CountG+1).moveindex ));
        end

    else % Step5. Adding more firms
        Int_mean = zeros(ModelSetting.Ni,Nf);
        Int_var  = zeros(ModelSetting.Ni,Nf);

        for i=1:ModelSetting.Ni
            Iname = smcsettings.Ind_Idx(i);
            Int_mean(i,:)   = mean( theta.(smcsettings.Fname{Iname})(:,1:Nf)) ;
            Int_var(i,:)    = var( theta.(smcsettings.Fname{Iname})(:,1:Nf) ) ;
            Int_data(:,i,:) = squeeze( theta.(smcsettings.Fname{Iname}) );
            % Theta(i) regress on delta
            for Gi=2:Nf
                TmpReg      = ols(theta.(smcsettings.Fname{Iname})(:,Gi), [ones(smcsettings.Nsim,1)...
                    theta.(smcsettings.Fname{smcsettings.Com_Idx})]);
                int_Param(Gi).Coeff(i,:)= TmpReg.beta;
            end
        end
        
        % Re-Initialization: Resample Common Variable : 
        for i= smcsettings.Com_Idx  
            % Mix density funciton
            % I_s ~ N(m,s)
            int_ComParam.m(1,2) = mean(theta.(smcsettings.Fname{i}),1);
            int_ComParam.s(1,2) = var(theta.(smcsettings.Fname{i}) );

            IndM = random('bino',1,1-smcsettings.MixW,[smcsettings.Nsim ,1])==1;
            % ( smcsettings.MixW )% generate from CountG result
            theta.(smcsettings.Fname{i})(IndM==0) = InitSim_theta(sum(IndM==0), ...
                    int_ComParam.m(2),  int_ComParam.s(2)^(1/2),  int_ComParam.dist,...
                    int_ComParam.bound); 
            % ( 1-smcsettings.MixW )% generate from Initial setting I_0
            theta.(smcsettings.Fname{i})(IndM)    = InitSim_theta(sum(IndM), ...
                    int_ComParam.m(1),  int_ComParam.s(1)^(1/2),  int_ComParam.dist,...
                    int_ComParam.bound); 
            
        [ lnpdf_int(:,i),int_ComParam.tc] = loglikelihood_density(...
            theta.(smcsettings.Fname{i}),int_ComParam.m,...
            int_ComParam.s , 1,1, smcsettings.MixW, []);
        
        end
        % adding firms
        Int_data = cat(3,Int_data,zeros(smcsettings.Nsim, ModelSetting.Ni, Ns-Nf) );
        
        for Gi=1:Ns 
            % Re-Initialization: Resample Individual variables      
            if Gi<=Nf  
                if Gi==1 % (1) Initialization
                int_Param(Gi).m = Int_mean(:,Gi)';    
                Mean_Fi         = repmat(int_Param(Gi).m,smcsettings.Nsim,1);
                int_Param(Gi).s = cov(bsxfun(@minus,Int_data(:,:,Gi),Mean_Fi));
                % Resample Initial theta from Trucated MultiNormal
                % (beta1>0)
                Int_data(:,:,Gi) = SimProposalMN2(Mean_Fi,int_Param(Gi).s,[], 1,1,...
                    int_Param(Gi).bound(1),int_Param(Gi).bound(2));
                else % (2:Nf) Initialization
                Mean_Fi =[ones(smcsettings.Nsim,1) theta.(smcsettings.Fname{smcsettings.Com_Idx})]...
                  *int_Param(Gi).Coeff';
                int_Param(Gi).m  =  mean(Mean_Fi);
                int_Param(Gi).s  = cov(bsxfun(@minus,Int_data(:,:,Gi),Mean_Fi )); %* ModelSetting.adjK;
                % Resample Initial theta from MultiNormal
                Int_data(:,:,Gi) = SimProposalMN2(Mean_Fi,int_Param(Gi).s,[], 1,0,[],[]);
                end
                [ lnpdf_int(:,Gi+1)] = loglikelihood_density(...
                    Int_data(:,:,Gi),int_Param(Gi).m', int_Param(Gi).s, 0,1);
            else % Expanding data (Nf+1:Ns) Initialization

                % Generate Initial theta from iid Normal
                Int_data(:,:,Gi) = InitSim_theta(smcsettings.Nsim, int_Param(Gi).m' ,...
                    int_Param(Gi).s'.^(1/2), int_Param(Gi).dist, int_Param(Gi).bound );

                % log likelihood of initialization sampler 
                [ lnpdf_int(:,Gi+1)] = loglikelihood_density(...
                    Int_data(:,:,Gi),int_Param(Gi).m', diag(int_Param(Gi).s), 1,1);
            end
        end
        
        for i=1:ModelSetting.Ni
            Iname = smcsettings.Ind_Idx(i);
            theta.(smcsettings.Fname{Iname}) = squeeze(Int_data(:,i,:));
        end
             
    end
    
    % evaluate log likelihood at initial parameters: smcsettings.fun
    %%%%   initial likelihood
    [lnCL_addD] = feval(smcsettings.fun, theta, Y_exp,1  );
    
    gamma=0;
    lnw = zeros(smcsettings.Nsim,1);
    t=1;
    reslt(CountG).runtime(t)=toc(begT);
    
    while gamma < 1
        
        begT = tic;
        %%%%%% SMC step %%%%%%%
        [reslt(CountG).ESS(t),reslt(CountG).normfac(t),reslt(CountG).AcceptRate(t,:),...
            theta,lnCL_addD,lnw,lnpdf_int,gamma,smcsettings] = SMC_Step_MixW(theta,...
            lnCL_addD,lnw,lnpdf_int,int_ComParam, int_Param,gamma,...
            Y_exp,smcsettings,CountG);
        
        Xeven = theta;
        
        for Iname = 1:smcsettings.nameN
            if ismember(Iname,smcsettings.Com_Idx);
                reslt(CountG).(smcsettings.Fname{Iname})(t,:)=...
            [mean(Xeven.(smcsettings.Fname{Iname})) min(Xeven.(smcsettings.Fname{Iname}))...
             max(Xeven.(smcsettings.Fname{Iname})) prctile(Xeven.(smcsettings.Fname{Iname}),5) ...
             prctile(Xeven.(smcsettings.Fname{Iname}),95) ...
             mean(Xeven.(smcsettings.Fname{Iname})) - std(Xeven.(smcsettings.Fname{Iname}))*1.96 ...
             mean(Xeven.(smcsettings.Fname{Iname})) + std(Xeven.(smcsettings.Fname{Iname}))*1.96];
            else
                % Uncommon parameters
                reslt(CountG).(smcsettings.Fname{Iname}).m(t,:)=...
                    [mean(Xeven.(smcsettings.Fname{Iname})) ...
                    min(Xeven.(smcsettings.Fname{Iname})) ...
                    max(Xeven.(smcsettings.Fname{Iname})) ];
                reslt(CountG).(smcsettings.Fname{Iname}).prc(t,:)=...
                    [prctile(Xeven.(smcsettings.Fname{Iname}),5) prctile(Xeven.(smcsettings.Fname{Iname}),95)];
                reslt(CountG).(smcsettings.Fname{Iname}).ci(t,:)=...
                    [mean(Xeven.(smcsettings.Fname{Iname})) - std(Xeven.(smcsettings.Fname{Iname}))*1.96 ...
                     mean(Xeven.(smcsettings.Fname{Iname})) + std(Xeven.(smcsettings.Fname{Iname}))*1.96];
            end
        end
        
        reslt(CountG).runtime(t+1) = toc(begT);
            
        t=t+1;
        disp([ 'gamma steps:' num2str(t-1)]);
    end
    
    %store full-sample posterior
    reslt(CountG).Xeven=Xeven;
    
    bridgeN(1,CountG) = t-1;
    
    outExcel=[reslt(CountG).weight(t-1,:);reshape([reslt(CountG).mu.m(t-1,:)  reslt(CountG).mu.prc(t-1,:)  reslt(CountG).mu.ci(t-1,:)],Ns,7);...
        reshape([reslt(CountG).beta.m(t-1,:)  reslt(CountG).beta.prc(t-1,:)  reslt(CountG).beta.ci(t-1,:)],Ns,7);...
        reshape(exp([reslt(CountG).nu.m(t-1,:)  reslt(CountG).nu.prc(t-1,:)  reslt(CountG).nu.ci(t-1,:)]),Ns,7)];
    xlswrite(['Examples\OutY' num2str(yearI) '_Sec' num2str(SecCode) '.xls'],outExcel,1,sprintf('B%d',OE))
    xlswrite(['Examples\OutY' num2str(yearI) '_Sec' num2str(SecCode) '.xls'],reslt(CountG).runtime,1,sprintf('M%d',CountG+1))
    OE = OE+size(outExcel,1)+1;
    
end
save(['Examples\Y' num2str(yearI) '_Sec' num2str(SecCode) '.mat'])
disp(['Firms without any data available: ' num2str(StoreDelete)])

delete(p)
