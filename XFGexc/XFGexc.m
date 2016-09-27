close all
clc

% load data
  PL = load('XFGPL.dat');
  MMPL = load('XFGMMPL.dat');

% main computation
  [m n] = size(PL);              % m = number of rows
  t = 1;
  Exc = 0;
  while (t<=m)
    q = quantile(PL(t,:)',0.99); % 99% quantile of PL
    if (q<MMPL(t,1))             % counts exceeding values of q in MMPL
      Exc = Exc+1;
    end
    t = t+1;
  end
  Exc = Exc/m



