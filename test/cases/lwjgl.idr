module Main

import Data.Bits
import IdrisJava
import IdrisJava.System

%default partial

%include java "org.lwjgl.Sys"
%include java "org.lwjgl.glfw.*"
%include java "org.lwjgl.opengl.*"
 
%include java "java.nio.ByteBuffer"
 
%include java "static org.lwjgl.glfw.Callbacks.*"
%include java "static org.lwjgl.glfw.GLFW.*"
%include java "static org.lwjgl.opengl.GL11.*"
%include java "static org.lwjgl.system.MemoryUtil.*"

%lib java "org.lwjgl:lwjgl:3.0.0a"

-- we need to access ByteBuffer so we define a type for it

ByteBuffer : Type
ByteBuffer = refty "" "ByteBuffer"


-- First we defined a bunch of stuff for using lwjgl
-- in practice this will all go into its own idris package

null : Bits64
null = 0

GL_INT : Type
GL_INT   = Int

GL_FLOAT : Type
GL_FLOAT = Float

GL_TRUE : GL_INT
GL_TRUE = 1

GL_FALSE : GL_INT 
GL_FALSE = 0

GL_COLOR_BUFFER_BIT : Bits32
GL_COLOR_BUFFER_BIT = 16384

GL_DEPTH_BUFFER_BIT : Bits32
GL_DEPTH_BUFFER_BIT = 256

glInt2Bool : GL_INT -> Bool
glInt2Bool 0 = False
glInt2Bool _ = True

GLFW_VISIBLE : Int
GLFW_VISIBLE = 131076

GLFW_RESIZABLE : Int
GLFW_RESIZABLE = 131075

GlfwInitT : Type
GlfwInitT = JAVA_IO GL_INT

glfwInit : GlfwInitT
glfwInit = invoke "glfwInit" GlfwInitT

GlfwTerminateT : Type
GlfwTerminateT = JAVA_IO ()

glfwTerminate : GlfwTerminateT
glfwTerminate = invoke "glfwTerminate" GlfwTerminateT

GlfwCreateWindowT : Type
GlfwCreateWindowT = Int -> Int -> String -> Bits64 -> Bits64 -> JAVA_IO Bits64

glfwCreateWindow : GlfwCreateWindowT
glfwCreateWindow width height title monitor share =
  invoke "glfwCreateWindow" GlfwCreateWindowT width height title monitor share

GlfwDestroyWindowT : Type
GlfwDestroyWindowT = Bits64 -> JAVA_IO ()  

glfwDestroyWindow : GlfwDestroyWindowT
glfwDestroyWindow windowID = invoke "glfwDestroyWindow" GlfwDestroyWindowT windowID

GlfwDefaultWindowHintsT : Type
GlfwDefaultWindowHintsT = JAVA_IO ()

glfwDefaultWindowHints : GlfwDefaultWindowHintsT
glfwDefaultWindowHints = invoke "glfwDefaultWindowHints" GlfwDefaultWindowHintsT

GlfwGetPrimaryMonitorT : Type
GlfwGetPrimaryMonitorT = JAVA_IO Bits64

glfwGetPrimaryMonitor : GlfwGetPrimaryMonitorT
glfwGetPrimaryMonitor = 
  invoke "glfwGetPrimaryMonitor" GlfwGetPrimaryMonitorT
  
GlfwGetVideoModeT : Type
GlfwGetVideoModeT = Bits64 -> JAVA_IO ByteBuffer

glfwGetVideoMode : GlfwGetVideoModeT
glfwGetVideoMode monitor =
  invoke "glfwGetVideoMode" GlfwGetVideoModeT monitor

GlfwWindowHintT : Type 
GlfwWindowHintT = Int -> Int -> JAVA_IO ()

glfwWindowHint : GlfwWindowHintT
glfwWindowHint target hint = invoke "glfwWindowHint" GlfwWindowHintT target hint

GlfwMakeContextCurrentT : Type
GlfwMakeContextCurrentT = Bits64 -> JAVA_IO ()

glfwMakeContextCurrent : GlfwMakeContextCurrentT
glfwMakeContextCurrent windowID = 
  invoke "glfwMakeContextCurrent" GlfwMakeContextCurrentT windowID

GlfwSetWindowPosT : Type
GlfwSetWindowPosT = Bits64 -> Int -> Int -> JAVA_IO ()

glfwSetWindowPos : GlfwSetWindowPosT
glfwSetWindowPos window xpos ypos =
  invoke "glfwSetWindowPos" GlfwSetWindowPosT window xpos ypos

GlfwShowWindowT : Type
GlfwShowWindowT = Bits64 -> JAVA_IO ()

glfwShowWindow : GlfwShowWindowT
glfwShowWindow windowID = 
  invoke "glfwShowWindow" GlfwShowWindowT windowID

GlfwSwapIntervalT : Type
GlfwSwapIntervalT = Int -> JAVA_IO ()

glfwSwapInterval : GlfwSwapIntervalT
glfwSwapInterval interval = 
  invoke "glfwSwapInterval" GlfwSwapIntervalT interval
  
GLFWvidmodeWidthT : Type
GLFWvidmodeWidthT = ByteBuffer -> JAVA_IO Int 

GLFWvidmodeWidth : GLFWvidmodeWidthT
GLFWvidmodeWidth vidmode = 
  invoke "GLFWvidmode.width" GLFWvidmodeWidthT vidmode

GLFWvidmodeHeightT : Type
GLFWvidmodeHeightT = ByteBuffer -> JAVA_IO Int 

GLFWvidmodeHeight : GLFWvidmodeHeightT
GLFWvidmodeHeight vidmode = 
  invoke "GLFWvidmode.height" GLFWvidmodeHeightT vidmode

-- we won't need this once we update to tip for lwjgl
GLContextCreateFromCurrentT : Type
GLContextCreateFromCurrentT = JAVA_IO ()

GLContextCreateFromCurrent : GLContextCreateFromCurrentT
GLContextCreateFromCurrent = 
  invoke "GLContext.createFromCurrent" GLContextCreateFromCurrentT
 
GlClearColorT : Type
GlClearColorT = Float -> Float -> Float -> Float -> JAVA_IO ()

glClearColor : GlClearColorT
glClearColor red green blue alpha = 
  invoke "glClearColor" GlClearColorT red green blue alpha
    
GlClearT : Type
GlClearT = Bits32 -> JAVA_IO ()

glClear : GlClearT
glClear mask = 
  invoke "glClear" GlClearT mask

GlfwSwapBuffersT : Type 
GlfwSwapBuffersT = Bits64 -> JAVA_IO ()

glfwSwapBuffers : GlfwSwapBuffersT
glfwSwapBuffers windowID = 
  invoke "glfwSwapBuffers" GlfwSwapBuffersT windowID

GlfwPollEventsT : Type
GlfwPollEventsT = JAVA_IO ()

glfwPollEvents : GlfwPollEventsT
glfwPollEvents = 
  invoke "glfwPollEvents" GlfwPollEventsT

GlfwWindowShouldCloseT : Type
GlfwWindowShouldCloseT = Bits64 -> JAVA_IO GL_INT

glfwWindowShouldClose : GlfwWindowShouldCloseT
glfwWindowShouldClose windowID = 
  invoke "glfwWindowShouldClose" GlfwWindowShouldCloseT windowID

----------------------------------------

errorExit : String -> JAVA_IO ()
errorExit s = putStrLn s >>= \_ => exit 1

init : JAVA_IO Bits64
init = do
  i <- glfwInit >>= return . glInt2Bool
  when (not i) $ errorExit "Error initializing GLFW"
  
  let width  : Int = 640
  let height : Int = 480
                                                                            
  glfwDefaultWindowHints
  glfwWindowHint GLFW_VISIBLE GL_FALSE 
  glfwWindowHint GLFW_RESIZABLE GL_TRUE 
  windowID <- glfwCreateWindow width height "Hello from Idris" null null
  when (windowID == null) $ errorExit "Error creating a window"
  
  vidmode <- glfwGetPrimaryMonitor >>= glfwGetVideoMode
  vidw    <- GLFWvidmodeWidth vidmode  >>= \x => return $ (x - width) `div` 2
  vidh    <- GLFWvidmodeHeight vidmode >>= \x => return $ (x - height) `div` 2
  glfwSetWindowPos windowID vidw vidh
  
  glfwMakeContextCurrent windowID
  glfwSwapInterval 1
  
  glfwShowWindow windowID
                                                                                      
  return windowID 

loop : Bits64 -> JAVA_IO ()
loop windowID = do
     -- GL.createCapabilities() is what we need for latest build
     GLContextCreateFromCurrent
     glClearColor 1.0 0.0 0.0 0.0
     loop'
  where
    loop'        = do 
      close <- glfwWindowShouldClose windowID
      when (close == GL_FALSE) $ do 
        glClear $ (prim__orB32 GL_COLOR_BUFFER_BIT GL_DEPTH_BUFFER_BIT)
        glfwSwapBuffers windowID
        glfwPollEvents
        loop'
      return ()

shutdown : Bits64 -> JAVA_IO ()
shutdown window = do
  glfwDestroyWindow window
  glfwTerminate

main : JAVA_IO ()
main = do 
  windowID <- init 
  loop windowID
  shutdown windowID
  putStrLn "End"
