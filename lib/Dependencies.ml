type action = string option
type files  = string list
type target =
  | Files of files
  | Default

type dependencies = (target * files * action) list
