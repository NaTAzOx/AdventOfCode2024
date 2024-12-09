let read_file path =
    let lines = ref [] in
    let chan = open_in path in
    try
        while true do
            lines := input_line chan :: !lines
        done; !lines
    with End_of_file ->
        close_in chan;
        List.rev !lines

(* Parse input into initial storage and keep track of max file index *)
let parse_input input_str =
    let rec aux idx acc max_index =
        if idx >= String.length input_str then (List.rev acc, max_index)
        else
            let size = int_of_char input_str.[idx] - int_of_char '0' in
            if size < 0 || size > 9 then
                failwith "Invalid character in input"
            else
                let content =
                    if size = 0 then "."
                    else String.concat "" (List.init size (fun _ -> string_of_int idx))
                in
                aux (idx + 1) (content :: acc) (max max_index idx)
    in
    aux 0 [] 0

(* Replace empty spaces and track file placement *)
let set_storage line =
    let (storage_list, max_index) = parse_input line in
    let storage = String.concat "" storage_list in
    let filled_storage = Bytes.of_string storage in
    let len = Bytes.length filled_storage in
    let file_map = Hashtbl.create max_index in

    (* Fill empty spaces *)
    let rec fill idx next_file_idx =
        if idx >= len then ()
        else if Bytes.get filled_storage idx = '.' then
            (* Find the next file to fill *)
            let rec find_next_file start_idx =
                if start_idx >= len then None
                else if Bytes.get filled_storage start_idx <> '.' then Some start_idx
                else find_next_file (start_idx + 1)
            in
            match find_next_file next_file_idx with
            | Some file_idx ->
                let file_char = Bytes.get filled_storage file_idx in
                Bytes.set filled_storage idx file_char;
                let file_index = int_of_string (String.make 1 file_char) in
                Hashtbl.replace file_map idx file_index;
                fill (idx + 1) (file_idx + 1)
            | None -> ()
        else
            fill (idx + 1) next_file_idx
    in
    fill 0 0;
    (Bytes.to_string filled_storage, file_map)

(* Calculate checksum using the file map *)
let checksum file_map =
    Hashtbl.fold (fun idx file_idx acc ->
        acc + (idx * file_idx)
    ) file_map 0

let () =
    let lines = read_file "input" in
    List.iter
        (fun line ->
            let (storage, file_map) = set_storage line in
            Printf.printf "Storage: %s\n" storage;
            let cs = checksum file_map in
            Printf.printf "Checksum: %d\n" cs)
        lines
