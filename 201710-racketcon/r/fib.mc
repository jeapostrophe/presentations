START:
mar := sp; d := (1) + sp; wr;
f := (1) + (1); wr;
mbr := (1); b := (1); mar := d; wr;
e := f + (1); wr;
LOOP:
a := a + b;
d := f + sp;
mar :=  d; mbr := a; wr; if n then goto DONE;
sp := sp + e; wr;
b := b + a;
mar := sp; mbr := b; wr; if n then goto DONE;
sp := d; goto LOOP; wr;
DONE:
wr; rd; goto START;
