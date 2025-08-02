#include "parity.h"

uint8_t parity_table[256];

static int check_parity(uint8_t num) {
    int count = 0;
    while (num > 0) {
        if (num & 1) {
            count++;
        }
        num >>= 1;
    }
    return !(count & 1);
}

void generate_parity_table() {
    for (int num = 0; num <= 0xFF; num++) {
        parity_table[num] = check_parity(num);
    }
}
