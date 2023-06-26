/- This file contains external declarations for interfacing with the system.
   See stdlib/sys.mc for higher-level utilities and wrappers.
   The semantics should follow https://v2.ocaml.org/api/Sys.html (and the
   documentation strings here are adapted from that page).
 -/

-- Tests if a file with the given name exists.
external sysFileExists ! : String -> Bool
let sysFileExists = lam f. sysFileExists f

-- Returns true if the given name is a directory, false if it is another kind of file,
-- or raises an error if no file exists with the given name.
external sysIsDirectory ! : String -> Bool
let sysIsDirectory = lam f. sysIsDirectory f

-- Removes the file with the given name from the system, or raises an error
-- if no file exists with the given name.
external sysRemove ! : String -> ()
let sysRemove = lam f. sysRemove f

-- 'sysRename old new' renames the file called 'old' to 'new', moving it between
-- directories if needed.  Replaces 'new' if it already exists, and raises an
-- error if 'old' does not exist.
external sysRename ! : String -> String -> ()
let sysRemove = lam f. sysRename f

-- Returns either the value associated with the given variable in the process
-- environment, or raises an error if the variable is unset.
external sysGetEnv ! : String -> String
let sysGetEnv = lam f. sysGetEnv f

-- Returns (val, true) where 'val' is the value associated with the given variable
-- in the process environment, or ("", false) if the variable is unset.
external sysGetEnvOpt ! : String -> (String, Bool)
let sysGetEnvOpt = lam f. sysGetEnvOpt f

-- Executes the given shell command, returning its exit code.
-- The semantics should follow Sys.command at https://v2.ocaml.org/api/Sys.html.
external sysCommand ! : String -> Int
let sysCommand = lam f. sysCommand f

-- Returns the processor time in seconds used by the program since the beginning of
-- execution.
external sysTime ! : () -> Float
let sysTime = lam u. sysTime u

-- Changes the current working directory of the process, or raises an error if
-- the directory does not exist.
external sysChDir ! : String -> ()
let sysChDir = lam f. sysChDir f

-- Creates a directory with the given permissions, or raises an error if the parent
-- directory does not exist.
external sysMkDir ! : String -> Int -> ()
let sysMkDir = lam f. lam p. sysMkDir f p

-- Removes an empty directory, or raises an error if the directory is not empty
-- or does not exist.
external sysRmDir ! : String -> ()
let sysRmDir = lam f. sysRmDir f

-- Returns the current working directory of the process.
external sysGetCwd ! : () -> String
let sysGetCwd = lam u. sysGetCwd u

-- Returns the names of all files present in the given directory.
-- The semantics should follow Sys.readdir at https://v2.ocaml.org/api/Sys.html.
external sysReadDir ! : String -> [String]
let sysReadDir = lam f. sysReadDir f

-- The operating system currently executing the program.
-- One of 'Unix', 'Win32' or 'Cygwin'.
external sysOsType : String

-- The size of one word on the machine currently executing the program: 32 or 64.
external sysWordSize : Int

-- Whether the machine currently executing the program is big-endian.
external sysBigEndian : Bool
