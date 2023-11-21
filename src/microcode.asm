add:
    li
    rsb | rbo | aom | aol | pci
.reg:
    rsp | aoh
.imm:
    po | aoh
.both:
    ao | rsb | rbi | cr | pci

sub:
    li
    rsb | rbo | aom | aol | pci
.reg:
    rsp | aoh
.imm:
    po | aoh
.both:
    ao | aol | rsb | rbi | cr | pci

adc:
    li
    rsb | rbo | aom | aol | pci
.reg:
    rsp | aoh
.imm:
    po | aoh
.both:
    ao | aom | rsb | rbi | cr | pci

sbc:
    li
    rsb | rbo | aom | aol | pci
.reg:
    rsp | aoh
.imm:
    po | aoh
.both:
    ao | aol | aom | rsb | rbi | cr | pci

nand:
    li
    rsb | rbo | aom | aol | pci
.reg:
    rsp | aoh
.imm:
    po | aoh
.both:
    ao | aoh | rsb | rbi | cr | pci

or:
    li
    rsb | rbo | aom | aol | pci
.reg:
    rsp | aoh
.imm:
    po | aoh
.both:
    ao | aoh | aol | rsb | rbi | cr | pci

cmp:
    li
    rsb | rbo | aom | aol | pci
.reg:
    rsp | aoh
.imm:
    po | aoh
.both:
    ao | aoh | aom | cr | pci

mv:
.reg:
    li
    sr | cr | pci
.imm:
    li
    pci
    rsb | rbi | po | cr | pci

ld:
.reg:
    li
    thl | rbo
    la | rsb | rbi | cr | pci
.imm:
    li
    pci
    ahi | po | pci
    ali | po
    la | rsb | rbi | cr | pci

st:
.reg:
    li
    thl | rbo
    sa | rsb | rbo | cr | pci
.imm:
    li
    pci
    ahi | po | pci
    ali | po
    sa | rsb | rbo | cr | pci

lda:
.imm:
    li
    pci
    po | ahi | pci
    po | ali
    thl | rbi | cr | pci

push:
    li
.reg:
    lsp
    rsb | rbo | sa
.imm:
    lsp | pci
    po | sa
.both:
    spd | cr | pci

pop:
.reg:
    li
    spi
    lsp
    rsb | rbo | sa | cr | pci

jnz:
.reg:
    li
    rsb | rbo | aom | aol
    aol | aom | aoh
    jnz | cr
.imm:
    li
    pci
    po | aom | aol
    aol | aom | aoh
    jnz | cr

halt:
    li
    sh
