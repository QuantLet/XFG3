% clear variables and close graphics
clear all
close all
clc

barh([0.1, 0.2, 1.9, 2.6, 4.4, 7.6, 83.2 ],0.2,'k')
set(gca,'YTick',1:7);
set(gca,'YTickLabel',{'Eastern Europe' 'Pacific' 'Latin America' 'Europe&Mid East&Africa' 'Asia' 'North America' 'Europe' })
set(gca,'FontSize',12,'FontWeight','Bold')
title('MSCI Regions','FontSize',16,'FontWeight','Bold')
xlabel('Percent','FontSize',12,'FontWeight','Bold')
box on
set(gca,'LineWidth',1.6,'FontSize',12,'FontWeight','Bold')
ylim([0 8])

% to save plot please uncomment following lines
% print -painters -dpng -r600 XFGRegionsBreakdown.png
% print -painters -dpdf -r600 XFGRegionsBreakdown.pdf
