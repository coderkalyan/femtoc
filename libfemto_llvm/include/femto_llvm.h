// #ifndef _FEMTO_LLVM_H
// #define _FEMTO_LLVM_H
#pragma once

struct Context;

struct Context *fm_context_init(void);
void fm_context_deinit(struct Context *handle);
void fm_context_dump(struct Context *handle);

// #endif /* _FEMTO_LLVM_H */
