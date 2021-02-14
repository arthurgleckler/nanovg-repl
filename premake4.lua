local action = _ACTION or ""

solution "nanovg"
  configurations { "Debug", "Release" }
  location ( "build" )
  libdirs { "../nanovg/build" }
  platforms { "native", "x32", "x64" }

  project "repl"
    files { "src/replmain.c", "src/repl.c" }
    includedirs { "src", "../nanovg/src" }
    kind "ConsoleApp"
    language "C"
    links { "nanovg" }
    targetdir("build")

    configuration { "linux" }
       defines { "NANOVG_GLEW" }
       linkoptions { "`pkg-config --libs glfw3`" }
       links { "GL", "GLEW", "GLU", "m" }

    configuration { "windows" }
       defines { "_CRT_SECURE_NO_WARNINGS", "NANOVG_GLEW" }
       links { "gdi32",
	       "GLEW",
	       "glfw3",
	       "glu32",
	       "kernel32",
	       "opengl32",
	       "user32",
	       "winmm" }

    configuration { "macosx" }
      linkoptions { "-framework Carbon",
		    "-framework Cocoa",
		    "-framework CoreVideo",
		    "-framework IOKit",
		    "-framework OpenGL" }
      links { "glfw" }

    configuration "Debug"
      defines { "DEBUG" }
      flags { "ExtraWarnings", "Symbols" }

    configuration "Release"
      defines { "NDEBUG" }
      flags { "ExtraWarnings", "Optimize" }