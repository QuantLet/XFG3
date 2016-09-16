% clear variables and close graphics
clear all
close all
clc

x = [0.88, 0.91, 0.93, 0.98, 1.06, 1.96, 2.34, 2.43, 2.85,  3.54,  3.66, 3.86, 4.35, 5.78, 7.02, 10.39, 12.43, 23.21, 23.52, 25.85];
barh(x,0.2,'k')
set(gca,'YTick',1:20);
set(gca,'YTickLabel',{'Pharmaceuticals' 'Sovereign' 'Commercial Services' 'Telecom' 'Food&Staple Retl' 'Media' 'Consumer Services' 'Hardware&Equipment' 'Energy' 'Insurance' 'Food&Bev&Tobacco' 'Durables&Apparel' 'Automobile&Compo' 'Utilities' 'Diversified Fin' 'Materials'  'Transportation'  'Banks'  'Capital Goods'  'Retailing'})
set(gca,'FontSize',12,'FontWeight','Bold')
title('R^2','FontSize',16,'FontWeight','Bold')
box on
set(gca,'LineWidth',1.6,'FontSize',12,'FontWeight','Bold')
xlim([0 28])
ylim([0 21])

% to save plot please uncomment following lines 
% print -painters -dpng -r600 XFGRsquared.png
% print -painters -dpdf -r600 XFGRsquared.pdf
