#include <stdint.h>

__declspec(dllexport) void init(void);
__declspec(dllexport) uint8_t read(void);
__declspec(dllexport) void write(uint8_t data);
__declspec(dllexport) const char* name(void);
