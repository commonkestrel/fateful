add:
    li
    rba | ro | alp | pci
.reg:
    rpa | als
.imm:
    po | als
.both:
    aa | rba | ri | cr | pci

sub:
    li
    rba | ro | alp | pci
.reg:
    rpa | als
.imm:
    po | als
.both:
    aa | aol | rba | ri | cr | pci

adc:
    li
    rba | ro | alp | pci
.reg:
    rpa | als
.imm:
    po | als
.both:
    aa | aom | rba | ri | cr | pci

sbc:
    li
    rba | ro | alp | pci
.reg:
    rpa | als
.imm:
    po | als
.both:
    aa | aol | aom | rba | ri | cr | pci

nand:
    li
    rba | ro | alp | pci
.reg:
    rpa | als
.imm:
    po | als
.both:
    aa | aoh | rba | ri | cr | pci

or:
    li
    rba | ro | alp | pci
.reg:
    rpa | als
.imm:
    po | als
.both:
    aa | aoh | aol | rba | ri | cr | pci

cmp:
    li
    rba | ro | alp | pci
.reg:
    rpa | als
.imm:
    po | als
.both:
    aa | aoh | aom | cr | pci

mv:
.reg:
    li
    rba | ro | st | pci
    rpa | ri | st | cr | pci
.imm:
    li | pci
    rba | ri | po | cr | pci

ld:
.reg:
    li
    thl | ro
    la | rba | ri | cr | pci
.imm:
    li | pci
    ah | po | pci
    al | po
    la | rba | ri | cr | pci

st:
.reg:
    li
    thl | ro
    sa | rba | ro | cr | pci
.imm:
    li | pci
    ah | po | pci
    al | po
    sa | rba | ro | cr | pci

lda:
.imm:
    li | pci
    po | ah | pci
    po | al
    thl | ri | cr | pci

push:
    li
.reg:
    lsp
    rba | ro | sa
.imm:
    lsp | pci
    po | sa
.both:
    spi | cr | pci

pop:
.reg:
    li
    spd
    lsp
    rba | ro | sa | cr | pci

jnz:
.reg:
    li
    rba | ro | alp
    aol | aom | aoh
    jnz | cr
.imm:
    li | pci
    po | alp
    aol | aom | aoh
    jnz | cr

in:
    ; TODO: figure out `in` and `out`

out:
    ; TODO: figure out `in` and `out`
