// Copyright MMXXI Arthur A. Gleckler.
//
// See MIT license in "LICENSE" file.

#ifndef REPL_H
#define REPL_H

#include "nanovg.h"

#ifdef __cplusplus
extern "C" {
#endif

void errorExit(const char *format, ...);
void repl(NVGcontext *vg, GLFWwindow* window);

#ifdef __cplusplus
}
#endif

#endif // REPL_H
