#ifndef LIBZ80_H
#define LIBZ80_H

#include <stdint.h>
#include <stdlib.h>

#include "parity.h"

#define ROM_START   0x0000
#define ROM_END     0x3FFF
#define RAM_START   0x4000

#define SF 128
#define ZF  64
#define YF  32
#define HF  16
#define XF   8
#define PF   4
#define NF   2
#define CF   1

#define SZPCF (SF | ZF | PF | CF)
#define SZPF  (SF | ZF | PF	)
#define SZCF  (SF | ZF | CF	)
#define SYXF  (SF | YF | XF	)
#define ZPF   (ZF | PF		)
#define YXCF  (YF | XF | CF	)
#define YXF   (YF | XF		)
#define HCF   (HF | CF		)

#define PC cpu->pc
#define SP cpu->sp

#define BC (cpu->B << 8) | cpu->C
#define DE (cpu->D << 8) | cpu->E
#define HL (cpu->H << 8) | cpu->L

#define CYCLES cpu->cycles

typedef struct {
    uint8_t A, F, B, C, D, E, H, L;
    uint8_t A_, F_, B_, C_, D_, E_, H_, L_;
    uint8_t I, R;
    uint16_t pc, sp;
    uint16_t IX, IY;
    uint8_t *memory;
    uint8_t cycles;
} __attribute__((packed)) Z80;

Z80 *init_cpu(size_t ram_size);
void execute_cycle(Z80 *cpu);

#endif
