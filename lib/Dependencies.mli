type action = string
type file   = string
type files  = file list

type target =
  | File of file
  | Default

type dependencies = (target * files * action option) list
