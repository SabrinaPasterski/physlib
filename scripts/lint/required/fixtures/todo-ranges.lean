import Physlib.Meta.TODO.Basic

open Lean Elab Command Physlib

def rangeFixture : Nat :=
  1

TODO (lines := 5-6) "Point at the definition."
TODO "Keep the default start line."
TODO "Use the full span of a
multiline TODO command."

/-- error: TODO lines must be an ordered range within the current file -/
#guard_msgs in
TODO (lines := 6-5) "Reject a reversed range."

/-- error: TODO lines must be an ordered range within the current file -/
#guard_msgs in
TODO (lines := 0-6) "Reject line zero."

/-- error: TODO lines must be an ordered range within the current file -/
#guard_msgs in
TODO (lines := 5-99999) "Reject a range outside the file."

TODO (lines := 5) "Point at one line."

run_cmd do
  let env ← getEnv
  let entries := todoExtension.getState env
  unless entries.map (fun item => (item.line, item.endLine)) ==
      #[(5, 6), (9, 9), (10, 11), (5, 5)] do
    throwError "Unexpected TODO line ranges"
  unless entries.all (fun item =>
      item.fileName == env.mainModule && item.tag == toString (String.hash item.content)) do
    throwError "Unexpected TODO module or tag"
