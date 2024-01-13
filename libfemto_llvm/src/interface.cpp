#include "Context.h"

extern "C" {

struct Context *fm_context_init(void) {
    auto *context = new femto::Context();
    struct Context *handle = reinterpret_cast<struct Context *>(context);
    return handle;
}

void fm_context_deinit(struct Context *handle) {
    auto *context = reinterpret_cast<femto::Context *>(handle);
    context->~Context();
    // delete context; // TODO: why is this crashing
}

// void fm_context_dump(struct Context *handle) {
//     femto::Context *context = reinterpret_cast<femto::Context *>(handle);
//     context->
// }

};
