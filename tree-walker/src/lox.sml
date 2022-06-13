val runPrompt = "running prompt";
fun runFile (filename : string) = "running " ^ filename;

val args = CommandLine.arguments();

fun main [] = runPrompt
  | main [filename] = runFile filename
  | main _ = OS.Process.exit OS.Process.failure
