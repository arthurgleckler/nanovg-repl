// Copyright MMXXI Arthur A. Gleckler.
//
// See MIT license in "LICENSE" file.

#include <stdio.h>
#ifdef NANOVG_GLEW
#	include <GL/glew.h>
#endif
#ifdef __APPLE__
#	define GLFW_INCLUDE_GLCOREARB
#endif
#define GLFW_INCLUDE_GLEXT
#include <GLFW/glfw3.h>
#include "nanovg.h"
#define NANOVG_GL3_IMPLEMENTATION
#include "nanovg_gl.h"
#include "repl.h"

extern FILE *events;

void reportGLFWerror(int error, const char* desc) {
  errorExit("GLFW error %d: %s\n", error, desc);
}

void usageExit() {
  errorExit("Usage: <width> <height> <title> <events-named-pipe>");
}

int main(int argc, char **argv) {
  if (argc != 5) {
    usageExit();
  }

  int initialWindowWidth;
  int initialWindowHeight;
  char *windowTitle = argv[3];
  char *eventsFilename = argv[4];

  if (1 != sscanf(argv[1], "%d ", &initialWindowWidth)) {
    usageExit();
  }
  if (1 != sscanf(argv[1], "%d ", &initialWindowHeight)) {
    usageExit();
  }
  events = fopen(eventsFilename, "r+");
  if (events == NULL) {
    errorExit("Failed to open named pipe for events.");
  }

  GLFWwindow* window;
  NVGcontext* vg = NULL;

  if (!glfwInit()) {
    errorExit("Failed to initialize GLFW.");
  }
  glfwSetErrorCallback(reportGLFWerror);
#ifndef _WIN32 // don't require this on win32, and works with more cards
  glfwWindowHint(GLFW_CONTEXT_VERSION_MAJOR, 3);
  glfwWindowHint(GLFW_CONTEXT_VERSION_MINOR, 2);
  glfwWindowHint(GLFW_OPENGL_FORWARD_COMPAT, GL_TRUE);
  glfwWindowHint(GLFW_OPENGL_PROFILE, GLFW_OPENGL_CORE_PROFILE);
#endif
  glfwWindowHint(GLFW_OPENGL_DEBUG_CONTEXT, 1);
  window = glfwCreateWindow(initialWindowWidth, initialWindowHeight,
			    windowTitle, NULL, NULL);
  if (!window) {
    glfwTerminate();
    errorExit("Failed to create window.");
    return -1;
  }
  glfwMakeContextCurrent(window);
#ifdef NANOVG_GLEW
  glewExperimental = GL_TRUE;
  if(glewInit() != GLEW_OK) {
    errorExit("Failed to initialize GLEW.");
    return -1;
  }
  glGetError();
#endif
  vg = nvgCreateGL3(NVG_ANTIALIAS | NVG_STENCIL_STROKES | NVG_DEBUG);
  if (vg == NULL) {
    errorExit("Failed to initialize NanoVG.");
  }
  glfwSwapInterval(0);
  repl(vg, window);
  nvgDeleteGL3(vg);
  glfwTerminate();
  return 0;
}