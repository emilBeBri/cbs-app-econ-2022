
1=DO 
2=HS 
3=SC 
4=CO
5=AD

dtdesc(f1)


View(f1)

colc(f1, 'dmfwg')


f1[, .(mean=mean(dmfwg28)), edgrp] %>% round(4)



f1[, .N, edgrp]


f1[, .(mean(dmfwg28)  ), edgrp]


f1[, .(mean(dmfwg28)  )]
