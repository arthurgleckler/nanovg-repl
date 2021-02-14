// Copyright MMXXI Arthur A. Gleckler.
//
// See MIT license in "LICENSE" file.

#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#ifdef NANOVG_GLEW
#  include <GL/glew.h>
#endif
#include <GLFW/glfw3.h>
#include "repl.h"
#include "nanovg.h"

FILE *events = NULL;

// This must match "fontstash.h".
#define FONS_INVALID -1

// The registry holds allocated memory objects that have been returned to the
// client as IDs.  <registryCount> contains the number of entries currently
// allocated.  <registryHead> contains the index of a free slot in <registry>.
// Unused slots are linked into a list.  Elements of the list encode their next
// pointer as a signed offset from the following index.  That way, the
// registry's memory can be initialized simply by zeroing it, then storing -1 in
// the last element.  In this encoding, -1 is the offset of the current entry.
// That is used to signify the end of the list.

#define REGISTRY_SIZE 3
static void **registry = NULL;
static size_t registryCount = 0;
static size_t registryHead = 0;

void errorExit(const char *format, ...) {
  va_list argp;

  va_start(argp, format);
  vfprintf(stderr, format, argp);
  fputc('\n', stderr);
  exit(1);
  va_end(argp);
}

void keyInput(GLFWwindow* window, int key, int code, int action, int mods) {
  NVG_NOTUSED(code);
  NVG_NOTUSED(mods);
  if (action == GLFW_PRESS) {
    fprintf(events, "key-input %d %d %d\n", key, code, mods);
    fflush(events);
  }
}

void mouseButton(GLFWwindow* window, int button, int action, int mods) {
  fprintf(events, "mouse-button %d %d %d\n", button, action, mods);
  fflush(events);
}

void mousePosition(GLFWwindow* window, double xpos, double ypos) {
  fprintf(events, "mouse-position %f %f\n", xpos, ypos);
  fflush(events);
}

void textInput(GLFWwindow* window, unsigned int codepoint) {
  fprintf(events, "text-input %d\n", codepoint);
  fflush(events);
}

void windowSizeChanged(GLFWwindow* window, int width, int height) {
  fprintf(events, "window-size-changed %d %d\n", width, height);
  fflush(events);
}

void dumpRegistry() {
  printf("R: %ld > ", (size_t) registryHead);
  for (size_t i = 0; i < registryCount; i++) {
    printf("%ld ", (size_t) registry[i]);
  }
  printf("\n");
}

void initializeRegistry() {
  if (registry == NULL) {
    size_t unit = sizeof(void *);
    size_t bytes = REGISTRY_SIZE * unit;

    registry = malloc(bytes);
    if (registry == NULL) {
      errorExit("Failed to initialize registry.");
    }
    memset(registry, 0, bytes - unit);
    registry[REGISTRY_SIZE - 1] = (void *) -1;
    registryCount = REGISTRY_SIZE;
  }
}

int registerObject(void *object) {
  size_t index = registryHead;
  long next = (long) registry[registryHead];
  size_t unit = sizeof(void *);

  registry[registryHead] = object;
  if (next != -1) {
    registryHead = registryHead + next + 1;
  } else {
    size_t old_count = registryCount;

    registryCount += REGISTRY_SIZE;
    registry = realloc(registry, registryCount * unit);
    if (registry == NULL) {
      errorExit("realloc failed.");
    }
    memset(&registry[old_count], 0, (REGISTRY_SIZE - 1) * unit);
    registry[registryCount - 1] = (void *) -1;
    registryHead = old_count;
  }
  return index;
}

void unregisterObject(int id) {
  registry[id] = (void *) (registryHead - id - 1);
  registryHead = id;
}

void unregisterAndFreeObject(int id) {
  free(registry[id]);
  unregisterObject(id);
}

#define RETURN_OBJECT(type, object) {			\
    type *local_p = (type *) malloc(sizeof(type));	\
							\
    if (local_p == NULL) {				\
      errorExit("malloc failed.");			\
    }							\
    memcpy(local_p, &object, sizeof(type));		\
							\
    int id = registerObject(local_p);			\
							\
    printf("%d\n", id);					\
  }

#define RETURN_POINTER(pointer) {		\
    int id = registerObject(pointer);		\
						\
    printf("%d\n", id);				\
  }

#define LOOK_UP_OBJECT(type, id) ((type *) registry[id])

unsigned char readByte() {
  int byte;

  if (1 == fscanf(stdin, "%d", &byte)
      && byte >= 0
      && byte <= 255) {
    return (unsigned char) byte;
  }
  return 0;
}

float readFloat() {
  float number;

  if (1 == fscanf(stdin, "%f", &number)) {
    return number;
  }
  return 0.0;
}

int readInt() {
  int number;

  if (1 == fscanf(stdin, "%d", &number)) {
    return number;
  }
  return 0;
}

char *readString() {
  size_t length;

  if (1 == fscanf(stdin, "%lu ", &length)) {
    char *buffer = malloc(length + 1);

    if (buffer == NULL) {
      errorExit("malloc failed.");
    }

    size_t bytes = fread(buffer, 1, length, stdin);

    if (bytes < length) {
      errorExit("Too few bytes read for string.  Excepted %d, but got %d.",
		length, bytes);
    }
    buffer[length] = 0;
    return buffer;
  }
  return NULL;
}

void addFallbackFont(NVGcontext *vg, GLFWwindow *window) {
  char *baseFont = readString();
  char *fallbackFont = readString();

  if (!nvgAddFallbackFont(vg, baseFont, fallbackFont)) {
    errorExit("Failed to find fallback fonts by name (baseFont: \"%s\", fallbackFont: \"%s\".",
	      baseFont, fallbackFont);
  }
  free(baseFont);
  free(fallbackFont);
}

void addFallbackFontId(NVGcontext *vg, GLFWwindow *window) {
  int baseFont = readInt();
  int fallbackFont = readInt();

  if (!nvgAddFallbackFontId(vg, baseFont, fallbackFont)) {
    errorExit("Failed to find fallback fonts by name (baseFont ID: \"%d\", fallbackFont ID: \"%d\".",
	      baseFont, fallbackFont);
  }
}

void arc(NVGcontext *vg, GLFWwindow *window) {
  float cx = readFloat();
  float cy = readFloat();
  float r = readFloat();
  float a0 = readFloat();
  float a1 = readFloat();
  int dir = readInt();

  nvgArc(vg, cx, cy, r, a0, a1, dir);
}

void arcTo(NVGcontext *vg, GLFWwindow *window) {
  float x1 = readFloat();
  float y1 = readFloat();
  float x2 = readFloat();
  float y2 = readFloat();
  float radius = readFloat();

  nvgArcTo(vg, x1, y1, x2, y2, radius);
}

void beginFrame(NVGcontext *vg, GLFWwindow *window) {
  float windowWidth = readFloat();
  float windowHeight = readFloat();
  float devicePixelRatio = readFloat();

  nvgBeginFrame(vg, windowWidth, windowHeight, devicePixelRatio);
}

void beginPath(NVGcontext *vg, GLFWwindow *window) {
  nvgBeginPath(vg);
}

void bezierTo(NVGcontext *vg, GLFWwindow *window) {
  float c1x = readFloat();
  float c1y = readFloat();
  float c2x = readFloat();
  float c2y = readFloat();
  float x = readFloat();
  float y = readFloat();

  nvgBezierTo(vg, c1x, c1y, c2x, c2y, x, y);
}

void boxGradient(NVGcontext *vg, GLFWwindow *window) {
  float x = readFloat();
  float y = readFloat();
  float w = readFloat();
  float h = readFloat();
  float r = readFloat();
  float f = readFloat();
  unsigned char icolr = readByte();
  unsigned char icolg = readByte();
  unsigned char icolb = readByte();
  unsigned char icola = readByte();
  unsigned char ocolr = readByte();
  unsigned char ocolg = readByte();
  unsigned char ocolb = readByte();
  unsigned char ocola = readByte();
  NVGpaint p = nvgBoxGradient(vg, x, y, w, h, r, f,
			      nvgRGBA(icolr, icolg, icolb, icola),
			      nvgRGBA(ocolr, ocolg, ocolb, ocola));

  RETURN_OBJECT(NVGpaint, p);
}

void circle(NVGcontext *vg, GLFWwindow *window) {
  float cx = readFloat();
  float cy = readFloat();
  float r = readFloat();

  nvgCircle(vg, cx, cy, r);
}

void clear(NVGcontext *vg, GLFWwindow *window) {
  unsigned char colorBufferBit = readByte();
  unsigned char depthBufferBit = readByte();
  unsigned char stencilBufferBit = readByte();

  glClear((colorBufferBit ? GL_COLOR_BUFFER_BIT : 0)
	  | (depthBufferBit ? GL_COLOR_BUFFER_BIT : 0)
	  | (stencilBufferBit ? GL_COLOR_BUFFER_BIT : 0));
}

// <> Unlike all the other color operations, this one takes floats, not bytes,
// for the components.  Should that change?
void clearColor(NVGcontext *vg, GLFWwindow *window) {
  float r = readFloat();
  float g = readFloat();
  float b = readFloat();
  float a = readFloat();

  glClearColor(r, g, b, a);
}

void closePath(NVGcontext *vg, GLFWwindow *window) {
  nvgClosePath(vg);
}

void closeWindow(NVGcontext *vg, GLFWwindow *window) {
  glfwSetWindowShouldClose(window, GL_TRUE);
}

void finishCreateFont(int id, char *name, char *filename) {
  if (id == FONS_INVALID) {
    errorExit("Failed to create font by name (name: \"%s\", filename: \"%s\").",
	      name, filename);
  }
  printf("%d\n", id);
  free(name);
  free(filename);
}

void createFont(NVGcontext *vg, GLFWwindow *window) {
  char *name = readString();
  char *filename = readString();
  int id = nvgCreateFont(vg, name, filename);

  finishCreateFont(id, name, filename);
}

void createFontAtIndex(NVGcontext *vg, GLFWwindow *window) {
  char *name = readString();
  char *filename = readString();
  int index = readInt();
  int id = nvgCreateFontAtIndex(vg, name, filename, index);

  finishCreateFont(id, name, filename);
}

void createImage(NVGcontext *vg, GLFWwindow *window) {
  char *filename = readString();
  int imageFlags = readInt();
  int result = nvgCreateImage(vg, filename, imageFlags);

  if (result == 0) {
    errorExit("Failed to load image (filename: \"%s\").", filename);
  }
  free(filename);
  printf("%d\n", result);
}

void currentTransform(NVGcontext *vg, GLFWwindow *window) {
  float transform[6];

  nvgCurrentTransform(vg, transform);
  for (int i = 0; i < 6; i++) {
    printf("%f ", transform[i]);
  }
  printf("\n");
}

void deleteImage(NVGcontext *vg, GLFWwindow *window) {
  int image = readInt();

  nvgDeleteImage(vg, image);
}

void ellipse(NVGcontext *vg, GLFWwindow *window) {
  float cx = readFloat();
  float cy = readFloat();
  float rx = readFloat();
  float ry = readFloat();

  nvgEllipse(vg, cx, cy, rx, ry);
}

void endFrame(NVGcontext *vg, GLFWwindow *window) {
  nvgEndFrame(vg);
}

void fill(NVGcontext *vg, GLFWwindow *window) {
  nvgFill(vg);
}

void fillColor(NVGcontext *vg, GLFWwindow *window) {
  unsigned char r = readByte();
  unsigned char g = readByte();
  unsigned char b = readByte();
  unsigned char a = readByte();

  nvgFillColor(vg, nvgRGBA(r, g, b, a));
}

void fillPaint(NVGcontext *vg, GLFWwindow *window) {
  int paintId = readInt();

  nvgFillPaint(vg, *LOOK_UP_OBJECT(NVGpaint, paintId));
}

void findFont(NVGcontext *vg, GLFWwindow *window) {
  char *name = readString();
  int id = nvgFindFont(vg, name);

  if (id == FONS_INVALID) {
    errorExit("Failed to find font by name (name: \"%s\").", name);
  }
  printf("%d\n", id);
  free(name);
}

void fontBlur(NVGcontext *vg, GLFWwindow *window) {
  float blur = readFloat();

  nvgFontBlur(vg, blur);
}

void fontFace(NVGcontext *vg, GLFWwindow *window) {
  char *font = readString();

  nvgFontFace(vg, font);
  free(font);
}

void fontFaceId(NVGcontext *vg, GLFWwindow *window) {
  int font = readInt();

  nvgFontFaceId(vg, font);
}

void fontSize(NVGcontext *vg, GLFWwindow *window) {
  float size = readFloat();

  nvgFontSize(vg, size);
}

void frameBufferSize(NVGcontext *vg, GLFWwindow *window) {
  int fbWidth;
  int fbHeight;

  glfwGetFramebufferSize(window, &fbWidth, &fbHeight);
  printf("%d %d\n", fbWidth, fbHeight);
}

void globalAlpha(NVGcontext *vg, GLFWwindow *window) {
  float alpha = readFloat();

  nvgGlobalAlpha(vg, alpha);
}

void imagePattern(NVGcontext *vg, GLFWwindow *window) {
  float cx = readFloat();
  float cy = readFloat();
  float w = readFloat();
  float h = readFloat();
  float angle = readFloat();
  int image = readInt();
  float alpha = readFloat();

  NVGpaint p =
    nvgImagePattern(vg, cx, cy, w, h, angle, image, alpha);

  RETURN_OBJECT(NVGpaint, p);
}

void imageSize(NVGcontext *vg, GLFWwindow *window) {
  int image = readInt();
  int w;
  int h;

  nvgImageSize(vg, image, &w, &h);
  printf("%d %d\n", w, h);
}

void intersectScissor(NVGcontext *vg, GLFWwindow *window) {
  float x = readFloat();
  float y = readFloat();
  float w = readFloat();
  float h = readFloat();

  nvgIntersectScissor(vg, x, y, w, h);
}

void keyInputEvents(NVGcontext *vg, GLFWwindow *window) {
  unsigned char on = readByte();

  glfwSetKeyCallback(window, on ? keyInput : NULL);
}

void linearGradient(NVGcontext *vg, GLFWwindow *window) {
  float sx = readFloat();
  float sy = readFloat();
  float ex = readFloat();
  float ey = readFloat();
  unsigned char icolr = readByte();
  unsigned char icolg = readByte();
  unsigned char icolb = readByte();
  unsigned char icola = readByte();
  unsigned char ocolr = readByte();
  unsigned char ocolg = readByte();
  unsigned char ocolb = readByte();
  unsigned char ocola = readByte();
  NVGpaint p = nvgLinearGradient(vg, sx, sy, ex, ey,
				 nvgRGBA(icolr, icolg, icolb, icola),
				 nvgRGBA(ocolr, ocolg, ocolb, ocola));

  RETURN_OBJECT(NVGpaint, p);
}

void lineCap(NVGcontext *vg, GLFWwindow *window) {
  int cap = readInt();

  nvgLineCap(vg, cap);
}

void lineJoin(NVGcontext *vg, GLFWwindow *window) {
  int join = readInt();

  nvgLineJoin(vg, join);
}

void lineTo(NVGcontext *vg, GLFWwindow *window) {
  float x = readFloat();
  float y = readFloat();

  nvgLineTo(vg, x, y);
}

void miterLimit(NVGcontext *vg, GLFWwindow *window) {
  float limit = readFloat();

  nvgMiterLimit(vg, limit);
}

void mouseButtonEvents(NVGcontext *vg, GLFWwindow *window) {
  unsigned char on = readByte();

  glfwSetMouseButtonCallback(window, on ? mouseButton : NULL);
}

void mousePositionEvents(NVGcontext *vg, GLFWwindow *window) {
  unsigned char on = readByte();

  glfwSetCursorPosCallback(window, on ? mousePosition : NULL);
}

void moveTo(NVGcontext *vg, GLFWwindow *window) {
  float x = readFloat();
  float y = readFloat();

  nvgMoveTo(vg, x, y);
}

void pathWinding(NVGcontext *vg, GLFWwindow *window) {
  int dir = readInt();

  nvgPathWinding(vg, dir);
}

void pollEvents(NVGcontext *vg, GLFWwindow *window) {
  glfwPollEvents();
}

void quadTo(NVGcontext *vg, GLFWwindow *window) {
  float cx = readFloat();
  float cy = readFloat();
  float x = readFloat();
  float y = readFloat();

  nvgQuadTo(vg, cx, cy, x, y);
}

void radialGradient(NVGcontext *vg, GLFWwindow *window) {
  float cx = readFloat();
  float cy = readFloat();
  float inr = readFloat();
  float outr = readFloat();
  unsigned char icolr = readByte();
  unsigned char icolg = readByte();
  unsigned char icolb = readByte();
  unsigned char icola = readByte();
  unsigned char ocolr = readByte();
  unsigned char ocolg = readByte();
  unsigned char ocolb = readByte();
  unsigned char ocola = readByte();
  NVGpaint p = nvgRadialGradient(vg, cx, cy, inr, outr,
				 nvgRGBA(icolr, icolg, icolb, icola),
				 nvgRGBA(ocolr, ocolg, ocolb, ocola));

  RETURN_OBJECT(NVGpaint, p);
}

void rect(NVGcontext *vg, GLFWwindow *window) {
  float x = readFloat();
  float y = readFloat();
  float w = readFloat();
  float h = readFloat();

  nvgRect(vg, x, y, w, h);
}

void reset(NVGcontext *vg, GLFWwindow *window) {
  nvgReset(vg);
}

void resetFallbackFonts(NVGcontext *vg, GLFWwindow *window) {
  char *baseFont = readString();

  nvgResetFallbackFonts(vg, baseFont);
  free(baseFont);
}

void resetFallbackFontsId(NVGcontext *vg, GLFWwindow *window) {
  int baseFont = readInt();

  nvgResetFallbackFontsId(vg, baseFont);
}

void resetScissor(NVGcontext *vg, GLFWwindow *window) {
  nvgResetScissor(vg);
}

void resetTransform(NVGcontext *vg, GLFWwindow *window) {
  nvgResetTransform(vg);
}

void restore(NVGcontext *vg, GLFWwindow *window) {
  nvgRestore(vg);
}

void rotate(NVGcontext *vg, GLFWwindow *window) {
  float angle = readFloat();

  nvgRotate(vg, angle);
}

void roundedRect(NVGcontext *vg, GLFWwindow *window) {
  float x = readFloat();
  float y = readFloat();
  float w = readFloat();
  float h = readFloat();
  float r = readFloat();

  nvgRoundedRect(vg, x, y, w, h, r);
}

void roundedRectVarying(NVGcontext *vg, GLFWwindow *window) {
  float x = readFloat();
  float y = readFloat();
  float w = readFloat();
  float h = readFloat();
  float radTopLeft = readFloat();
  float radTopRight = readFloat();
  float radBottomRight = readFloat();
  float radBottomLeft = readFloat();

  nvgRoundedRectVarying(vg, x, y, w, h, radTopLeft, radTopRight, radBottomRight,
			radBottomLeft);
}

void save(NVGcontext *vg, GLFWwindow *window) {
  nvgSave(vg);
}

void scale(NVGcontext *vg, GLFWwindow *window) {
  float x = readFloat();
  float y = readFloat();

  nvgScale(vg, x, y);
}

void scissor(NVGcontext *vg, GLFWwindow *window) {
  float x = readFloat();
  float y = readFloat();
  float w = readFloat();
  float h = readFloat();

  nvgScissor(vg, x, y, w, h);
}

void shapeAntiAlias(NVGcontext *vg, GLFWwindow *window) {
  int enabled = readInt();

  nvgShapeAntiAlias(vg, enabled);
}

void skewX(NVGcontext *vg, GLFWwindow *window) {
  float angle = readFloat();

  nvgSkewX(vg, angle);
}

void skewY(NVGcontext *vg, GLFWwindow *window) {
  float angle = readFloat();

  nvgSkewY(vg, angle);
}

void stroke(NVGcontext *vg, GLFWwindow *window) {
  nvgStroke(vg);
}

void strokeColor(NVGcontext *vg, GLFWwindow *window) {
  unsigned char r = readByte();
  unsigned char g = readByte();
  unsigned char b = readByte();
  unsigned char a = readByte();

  nvgStrokeColor(vg, nvgRGBA(r, g, b, a));
}

void strokePaint(NVGcontext *vg, GLFWwindow *window) {
  int paintId = readInt();

  nvgStrokePaint(vg, *LOOK_UP_OBJECT(NVGpaint, paintId));
}

void strokeWidth(NVGcontext *vg, GLFWwindow *window) {
  float width = readFloat();

  nvgStrokeWidth(vg, width);
}

void swapBuffers(NVGcontext *vg, GLFWwindow *window) {
  glfwSwapBuffers(window);
}

void text(NVGcontext *vg, GLFWwindow *window) {
  float x = readFloat();
  float y = readFloat();
  char *string = readString();
  float result = nvgText(vg, x, y, string, NULL);

  printf("%f\n", result);
  free(string);
}

void textAlign(NVGcontext *vg, GLFWwindow *window) {
  int align = readInt();

  nvgTextAlign(vg, align);
}

void textBounds(NVGcontext *vg, GLFWwindow *window) {
  float x = readFloat();
  float y = readFloat();
  char *string = readString();
  float bounds[4];
  float result = nvgTextBounds(vg, x, y, string, NULL, bounds);

  printf("%f ", result);
  for (int i = 0; i < 4; i++) {
    printf("%f ", bounds[i]);
  }
  printf("\n");
  free(string);
}

void textBox(NVGcontext *vg, GLFWwindow *window) {
  float x = readFloat();
  float y = readFloat();
  float breakRowWidth = readFloat();
  char *string = readString();

  nvgTextBox(vg, x, y, breakRowWidth, string, NULL);
  free(string);
}

void textBoxBounds(NVGcontext *vg, GLFWwindow *window) {
  float x = readFloat();
  float y = readFloat();
  float breakRowWidth = readFloat();
  char *string = readString();
  float bounds[4];

  nvgTextBoxBounds(vg, x, y, breakRowWidth, string, NULL, bounds);
  for (int i = 0; i < 4; i++) {
    printf("%f ", bounds[i]);
  }
  printf("\n");
  free(string);
}

void textInputEvents(NVGcontext *vg, GLFWwindow *window) {
  unsigned char on = readByte();

  glfwSetCharCallback(window, on ? textInput : NULL);
}

void textLetterSpacing(NVGcontext *vg, GLFWwindow *window) {
  float spacing = readFloat();

  nvgTextLetterSpacing(vg, spacing);
}

void textMetrics(NVGcontext *vg, GLFWwindow *window) {
  float ascender;
  float descender;
  float lineh;

  nvgTextMetrics(vg, &ascender, &descender, &lineh);
  printf("%f ", ascender);
  printf("%f ", descender);
  printf("%f\n", lineh);
}

void textLineHeight(NVGcontext *vg, GLFWwindow *window) {
  float lineHeight = readFloat();

  nvgTextLineHeight(vg, lineHeight);
}

void transform(NVGcontext *vg, GLFWwindow *window) {
  float a = readFloat();
  float b = readFloat();
  float c = readFloat();
  float d = readFloat();
  float e = readFloat();
  float f = readFloat();

  nvgTransform(vg, a, b, c, d, e, f);
}

void translate(NVGcontext *vg, GLFWwindow *window) {
  float x = readFloat();
  float y = readFloat();

  nvgTranslate(vg, x, y);
}

void unregister(NVGcontext *vg, GLFWwindow *window) {
  int id = readInt();

  unregisterAndFreeObject(id);
  NVG_NOTUSED(vg);
}

void viewport(NVGcontext *vg, GLFWwindow *window) {
  int x = readInt();
  int y = readInt();
  int w = readInt();
  int h = readInt();

  glViewport(x, y, w, h);
}

void windowSize(NVGcontext *vg, GLFWwindow *window) {
  int winWidth;
  int winHeight;

  glfwGetWindowSize(window, &winWidth, &winHeight);
  printf("%d %d\n", winWidth, winHeight);
}

void windowShouldClose(NVGcontext *vg, GLFWwindow *window) {
  printf("%d\n", glfwWindowShouldClose(window));
}

void windowSizeChangeEvents(NVGcontext *vg, GLFWwindow *window) {
  unsigned char on = readByte();

  glfwSetWindowSizeCallback(window, on ? windowSizeChanged : NULL);
}

void ping(NVGcontext *vg, GLFWwindow *window) {
  char *string = readString();

  printf("%s\n", string);
  free(string);
}

struct command {
  char *name;
  void (*function)(NVGcontext *, GLFWwindow *);
};

struct command commands[] =
  {{"add-fallback-font", addFallbackFont},
   {"add-fallback-font-id", addFallbackFontId},
   {"arc", arc},
   {"arc-to", arcTo},
   {"begin-frame", beginFrame},
   {"begin-path", beginPath},
   {"bezier-to", bezierTo},
   {"box-gradient", boxGradient},
   {"circle", circle},
   {"clear", clear},
   {"clear-color", clearColor},
   {"close-path", closePath},
   {"close-window", closeWindow},
   {"create-font", createFont},
   {"create-font-at-index", createFontAtIndex},
   {"create-image", createImage},
   {"current-transform", currentTransform},
   {"delete-image", deleteImage},
   {"ellipse", ellipse},
   {"end-frame", endFrame},
   {"fill", fill},
   {"fill-color", fillColor},
   {"fill-paint", fillPaint},
   {"find-font", findFont},
   {"font-blur", fontBlur},
   {"font-face", fontFace},
   {"font-face-id", fontFaceId},
   {"font-size", fontSize},
   {"frame-buffer-size", frameBufferSize},
   {"global-alpha", globalAlpha},
   {"image-pattern", imagePattern},
   {"image-size", imageSize},
   {"intersect-scissor", intersectScissor},
   {"key-input-events", keyInputEvents},
   {"linear-gradient", linearGradient},
   {"line-cap", lineCap},
   {"line-join", lineJoin},
   {"line-to", lineTo},
   {"miter-limit", miterLimit},
   {"mouse-button-events", mouseButtonEvents},
   {"mouse-position-events", mousePositionEvents},
   {"move-to", moveTo},
   {"path-winding", pathWinding},
   {"ping", ping},
   {"poll-events", pollEvents},
   {"quad-to", quadTo},
   {"radial-gradient", radialGradient},
   {"rect", rect},
   {"reset", reset},
   {"reset-fallback-fonts", resetFallbackFonts},
   {"reset-fallback-fonts-id", resetFallbackFontsId},
   {"reset-scissor", resetScissor},
   {"reset-transform", resetTransform},
   {"restore", restore},
   {"rotate", rotate},
   {"rounded-rect", roundedRect},
   {"rounded-rect-varying", roundedRectVarying},
   {"save", save},
   {"scale", scale},
   {"scissor", scissor},
   {"shape-anti-alias", shapeAntiAlias},
   {"skew-x", skewX},
   {"skew-y", skewY},
   {"stroke", stroke},
   {"stroke-color", strokeColor},
   {"stroke-paint", strokePaint},
   {"stroke-width", strokeWidth},
   {"swap-buffers", swapBuffers},
   {"text", text},
   {"text-align", textAlign},
   {"text-bounds", textBounds},
   {"text-box", textBox},
   {"text-box-bounds", textBoxBounds},
   {"text-input-events", textInputEvents},
   {"text-letter-spacing", textLetterSpacing},
   {"text-line-height", textLineHeight},
   {"text-metrics", textMetrics},
   {"transform", transform},
   {"translate", translate},
   {"unregister", unregister},
   {"viewport", viewport},
   {"window-size", windowSize},
   {"window-should-close?", windowShouldClose},
   {"window-size-change-events", windowSizeChangeEvents}};

void repl(NVGcontext *vg, GLFWwindow *window) {
  static char buffer[1024];
  static int count = sizeof(commands) / sizeof(struct command);

  initializeRegistry();
  while(1) {
    int result = fscanf(stdin, "%1024s", buffer);

    if (result == 1) {
      if (!strcmp(buffer, "shutdown")) {
	return;
      }
      for (int i = 0; i < count; i++) { // Perhaps use binary search.
	if (!strcmp(buffer, commands[i].name)) {
	  commands[i].function(vg, window);
	  fflush(NULL);
	}
      }
    }
  }
}