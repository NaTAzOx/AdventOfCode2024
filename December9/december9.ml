let read_file path =

let lines = ref [] in
let chan = open_in path in
try
    while true; do
        lines := input_line chan :: !lines
    done; !lines
with End_of_file ->
    close_in chan
    List.rev !lines ;;