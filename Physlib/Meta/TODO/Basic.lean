/-
Copyright (c) 2024 Joseph Tooby-Smith. All rights reserved.
Released under Apache 2.0 license.
Authors: Joseph Tooby-Smith
-/
module

public meta import Lean.Elab.Command
import Lean.Exception
/-!

# Basic underlying structure for TODOs.

-/

@[expose] public section

namespace Physlib
open Lean

/-- The information from a `TODO ...` command. -/
structure todoInfo where
  /-- The content of the note. -/
  content : String
  /-- The file name where the note came from. -/
  fileName : Name
  /-- The line from where the note came from. -/
  line : Nat
  /-- The last line of the TODO command or its explicit target. -/
  endLine : Nat
  /-- The tag of the TODO item -/
  tag : String

/-- Environment extension to store `todo ...`. -/
meta initialize todoExtension : SimplePersistentEnvExtension todoInfo (Array todoInfo) ←
  registerSimplePersistentEnvExtension {
    name := `todoExtension
    addEntryFn := fun arr todoInfor => arr.push todoInfor
    addImportedFn := fun es => es.foldl (· ++ ·) #[]
  }

/-- Syntax for a TODO anchored to the command or to an explicit range in the same file. -/
syntax (name := todo_comment) "TODO " ("(" &"lines" " := " num ("-" num)? ") ")? str : command

/-- Elaborator for the `TODO ...` command. -/
@[command_elab todo_comment]
meta def elabTODO : Elab.Command.CommandElab := fun stx => do
  let (message, explicitRange) ← match stx with
    | `(TODO $s:str) => pure (s, none)
    | `(TODO (lines := $line:num) $s:str) => pure (s, some (line.getNat, line.getNat))
    | `(TODO (lines := $first:num-$last:num) $s:str) => pure (s, some (first.getNat, last.getNat))
    | _ => throwError "Invalid syntax for `TODO` command"
  let some pos := stx.getPos? | throwError "Invalid source position for `TODO` command"
  let env ← getEnv
  let fileMap ← getFileMap
  let commandStart := (fileMap.toPosition pos).line
  let commandEnd := (fileMap.toPosition (stx.getTailPos?.getD pos)).line
  let (line, endLine) := explicitRange.getD (commandStart, commandEnd)
  let lastSourceLine := fileMap.getLastLine - (if fileMap.source.endsWith "\n" then 1 else 0)
  unless 0 < line && line <= endLine && endLine <= lastSourceLine do
    throwError "TODO lines must be an ordered range within the current file"
  let content := message.getString
  let tag := toString (String.hash content)
  let info : todoInfo := {content, fileName := env.mainModule, line, endLine, tag}
  modifyEnv fun env => todoExtension.addEntry env info
  Elab.Command.liftTermElabM <| Lean.Elab.Term.addTermInfo' message
    (Lean.mkStrLit s!"TODO tag: {tag}") (expectedType? := none)

end Physlib
