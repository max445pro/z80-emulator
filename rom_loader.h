#ifndef ROM_LOADER_H
#define ROM_LOADER_H

#include <stdint.h>
#include <stdio.h>

void load_rom(const char *filename, uint8_t *location);

#endif
