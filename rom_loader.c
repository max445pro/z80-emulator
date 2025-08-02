#include "rom_loader.h"
#include <stdlib.h>

void load_rom(const char *filename, uint8_t *location) {
    FILE *file = fopen(filename, "rb");

    // Get ROM size
    fseek(file, 0, SEEK_END);
    size_t file_size = ftell(file);

    rewind(file);

    // Read the content and stores it at locations
    fread(location, 1, file_size, file);

    fclose(file);
}
