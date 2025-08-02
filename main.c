#include <stdio.h>
#include <stdint.h>
#include "src/libz80.h"
#include "src/rom_loader.h"

#include "src/parity.h"

int main(int argc, char *argv[]) {
    // Init a new CPU
    Z80 *cpu = init_cpu(64 * 1024); // 64kb of address space
    // Loads OS and BASIC into ROM
    //load_rom("48.rom", cpu->memory);
    cpu->memory[0x0000] = 0x11;
    cpu->memory[0x0001] = 0xfe;
    cpu->memory[0x0002] = 0xff;
    cpu->memory[0x0003] = 0x21;
    cpu->memory[0x0004] = 0x01;
    cpu->memory[0x0005] = 0x00;
    cpu->memory[0x0006] = 0x19;
    execute_cycle(cpu);
    execute_cycle(cpu);
    execute_cycle(cpu);
    printf("HL: %04x DE: %04x F: %02x\n", HL, DE, cpu->F);
}
