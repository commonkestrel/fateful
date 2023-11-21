#include "peripheral.h"
#include<stdio.h>
#include <stdint.h>

uint8_t STATE = 0x00;

__declspec(dllexport) void init(uint8_t n) {
    printf("C init\n");
}

__declspec(dllexport) uint8_t read(uint8_t n) {
    printf("C read\n");
    return STATE;
}

__declspec(dllexport) void write(uint8_t n, uint8_t data) {
    printf("C write\n");
    STATE = data;
}

__declspec(dllexport) const char* name(void) {
    return "C Example";
}
