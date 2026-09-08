function[u] = ut(c,g)
if g< 1 | g>1 
    u = (1/(1-g))*c.^(1-g); 
else 
    u = log(c);
end

