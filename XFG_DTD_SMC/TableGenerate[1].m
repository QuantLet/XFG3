% To generate Table 1 to Table 4 of the paper
% "Estimating Distance-to-Default with a Sector-Specific
% Liability Adjustment via Sequential Monte Carlo"
% Section 4  Empirical implementation
clear all
OutTable2=zeros(7,8);

for k=1:8
switch k
    case 1
    Year=2009;
    IndustryCode=20018;
    case 2
    Year=2014;
    IndustryCode=20018;
    case 3
    Year=2009;
    IndustryCode=20082;
    case 4
    Year=2014;
    IndustryCode=20082;
    case 5
    Year=2009;
    IndustryCode=20051;
    case 6
    Year=2014;
    IndustryCode=20051;
    case 7
    Year=2009;
    IndustryCode=20055;
    case 8
    Year=2014;
    IndustryCode=20055;
end

% loading raw data
load(['Input\Y' num2str(Year) 'G' num2str(IndustryCode,'%06d') '_c.mat'])
load(['Output\Y' num2str(Year) '_Sec' num2str(IndustryCode,'%05d') '.mat'], 'Y','theta','outExcel') %SMC result
% financial statement according to our critiera:
% 22 days (1 month) consuctive data available
Fname_org = fieldnames(YearData);
Nfiled_org = length(Fname_org);

%% Table 1
% Fname_org={'Date','FirmCode', 'Equity', 'ShortD','LongD', 'otherL','TotalA','Rf'}';
OutTable1(1,k)=size(YearData.FirmCode,2);
for i=3:Nfiled_org-1
OutTable1(i-1,k)=nanmean(YearData.(Fname_org{i})(250,:));
end
TotalL=YearData.(Fname_org{4})+YearData.(Fname_org{5})+YearData.(Fname_org{6});
OutTable1(7,k)= nanmean(TotalL(250,:));
Dratio=YearData.otherL./TotalL;
OutTable1(8,k)= nanmean(Dratio(250,:));

%% Table 2
% summary statistic of CRI data / 
% CRI data critiera:
% more than 10 months and 250 daily data available within 24 months

% estimation result from DTD MLE step1
load('CRI_data\deltaSD.mat', 'fulldata')  
indY = find(floor(fulldata(:,2)/100)==(Year*100+12));
    Ydata=fulldata(indY,:);      % year end data
    [im, ic] = ismember(Ydata(:,1),Y.FirmCode);
    
tmpY=Ydata(im,:);
[ia, iR]=ismember(Y.FirmCode,tmpY);
CRIparam{4,k}=tmpY(iR(ia),:);
OutTable2(3,k) = nanmean(CRIparam{4,k}(:,7) );

% estimation result from DTD MLE step2
load('CRI_data\deltaSD_step3.mat', 'fulldata')  

indY = find(floor(fulldata(:,2)/100)==(Year*100+12));
    Ydata=fulldata(indY,:);      % year end data
    [im, ic] = ismember(Ydata(:,1),Y.FirmCode);
    
CRIparam{1,k}=IndustryCode;
CRIparam{2,k}=Year;
tmpY=Ydata(im,:);
[ia, iR]=ismember(Y.FirmCode,tmpY);
CRIparam{3,k}=tmpY(iR(ia),:);
% delta :CRIparam{3,k}(:,7) -- Table 2
% mu :CRIparam{3,k}(:,5)    -- Table 3
% sigma :CRIparam{3,k}(:,6) -- Table 3
OutTable2(4,k) = nanmean(CRIparam{3,k}(:,7) );

% estimation result from SMC method
weight = outExcel(1,1);
OutTable2(5:7,k) = [weight; prctile(theta.weight,2.5);  prctile(theta.weight,97.5)];
Nq=size(Y.FirmCode,2); 
MissR=sum(sum(isnan(Y.Equity)))/(250*Nq);
OutTable2(1:2,k) = [Nq; MissR];

OutTable3_a(1:2,k)=[nanmean(CRIparam{3,k}(:,5))*250;nanmean(CRIparam{3,k}(:,6))*sqrt(250)];

% Table 3_b: Firm-specific parameters for four industry sectors in 2009 and 2014
sigma=sqrt(outExcel(Nq+2:2*Nq+1,1).^2+outExcel(2*Nq+2:3*Nq+1,1).^2);
OutTable3_b(1:16,k)= [nanmean(outExcel(2:Nq+1,1));  nanmedian(outExcel(2:Nq+1,1))  ;nanmin(outExcel(2:Nq+1,1) ) ;nanmax(outExcel(2:Nq+1,1)  );
    nanmean(outExcel(Nq+2:2*Nq+1,1));   nanmedian(outExcel(Nq+2:2*Nq+1,1))   ;nanmin(outExcel(Nq+2:2*Nq+1,1))   ;nanmax(outExcel(Nq+2:2*Nq+1,1));
    nanmean(outExcel(2*Nq+2:3*Nq+1,1)); nanmedian(outExcel(2*Nq+2:3*Nq+1,1));nanmin(outExcel(2*Nq+2:3*Nq+1,1));nanmax(outExcel(2*Nq+2:3*Nq+1,1));
    nanmean(sigma);  nanmedian(sigma)  ;nanmin(sigma ) ;nanmax(sigma   )];
    CV2= diag( outExcel(2*Nq+2:3*Nq+1,1).^2 )+ outExcel(Nq+2:2*Nq+1,1)*outExcel(Nq+2:2*Nq+1,1)';
    CR=corrcov(CV2);
    tmpCR=reshape(tril(CR,-1),1,Nq*Nq);
    dataCR=tmpCR(tmpCR~=0);
OutTable3_b(17:20,k)=[ mean(dataCR);median(dataCR);min(dataCR);max(dataCR)];
    
% Table 4: DTD comparison for four industry sectors in 2009 and 2014.   
addpath('Subfunction')

tdays=250;
liability_valid = Y.ShortD + 0.5 * Y.LongD +  weight * Y.otherL;
[evalue ] = impvalue_annual(Y.Equity(:,1:Nq), liability_valid(:,1:Nq), Y.Rf(:,1:Nq)/(100), sigma', 250/tdays, []);
%     mu = reslt(end).mu.m(end,1:Nq)-sig.* sig /2;
DTD=((log(evalue(end,1:Nq)./liability_valid(end,1:Nq)))./(sigma'))';
DTDtmp=DTD(ia); %to consistance with CRI database
DTDnanI=~isnan( DTDtmp); 
    % compare CRI and SMC
OutTable4(1:10,k)= [nanmean(CRIparam{3,k}(:,3));  nanmedian(CRIparam{3,k}(:,3))  ;nanmin(CRIparam{3,k}(:,3) ) ;nanmax(CRIparam{3,k}(:,3)   );...
nanmean(DTD);  nanmedian(DTD)  ;nanmin(DTD ) ;nanmax(DTD   );...
corr( CRIparam{3,k}(DTDnanI,3) , DTDtmp(DTDnanI),'type','Kendall');corr(CRIparam{3,k}(DTDnanI,3),DTDtmp(DTDnanI),'type','Pearson')];

end
