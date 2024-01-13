#include <stdio.h>
#include "femto_llvm.h"

int main(void) {
    struct Context *context = fm_context_init();
    printf("Hello, world\n");
    fm_context_deinit(context);
}
